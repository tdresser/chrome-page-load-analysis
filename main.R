library(plotly)
library(gtools)
library(tidyverse)

source("breakdown_colors.R")

options(scipen=10000)

df <- read_csv('important_timestamps.csv', col_types=cols(
  site=col_character(), 
  cache_temperature=readr::col_factor(c("warm", "cold", "hot")), 
  .default=col_number()))

# Find cases where we're missing data for some sites.
num_occurrences_of_site <- df %>% 
  select(site) %>% 
  group_by(site) %>% 
  mutate(count=n())
sites_to_drop <- num_occurrences_of_site[num_occurrences_of_site$count < 3,]

df <- df %>% anti_join(sites_to_drop, by="site")

timestamps <- c('nav', 'firstPaint', 'firstContentfulPaint', 'firstMeaningfulPaint', 'firstInteractive', 'consistentlyInteractive')
friendly_timestamps <- c('Navigation', 'First Paint', 'First Contentful Paint', 'First Meaningful Paint', 'First Interactive', 'Consistently Interactive')

organized <- df %>% 
  gather(key, value, -cache_temperature, -site) %>% 
  tidyr::extract("key", c("start", "end", "is_cpu_time", "breakdown"), 
                 regex = "(.*)To(.*)Breakdown(CpuTime)?-(.*)", convert=TRUE, remove=TRUE) %>%
  mutate(start = factor(tolower(start), tolower(timestamps)),
         end = factor(tolower(end), tolower(timestamps)),
         is_cpu_time = as.factor(!is.na(is_cpu_time)),
         breakdown=as.factor(breakdown), 
         site=as.factor(site),
         value = value / 1000) %>%
  filter(!is.na(value), value != 0)


# Some sites have multiple entries - drop them.
# TODO - we should remove this once this input data doesn't have duplicate entries.
run_params <- organized %>% select(cache_temperature, site, start, end, is_cpu_time, breakdown)
organized <- organized[!duplicated(run_params),]

levels(organized$cache_temperature) <- c("Warm", "Cold", "Hot")
organized$cache_temperature <- factor(organized$cache_temperature, levels=c("Cold", "Warm", "Hot"))
levels(organized$start) <- friendly_timestamps
levels(organized$end) <- friendly_timestamps
levels(organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")

totals <- organized %>% filter(breakdown == "total", start=="Navigation") 

plot_totals_jitter_sampled <- totals %>% sample_frac(0.1) %>% ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) + 
  geom_jitter(alpha=0.1, size=0.3) + 
  facet_grid(is_cpu_time ~ end) +
  scale_y_log10() +
  labs(title="Totals per point in time", x="\n\nCache temperature", y="Seconds\n")

plot_totals_jitter <- totals %>% ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) + 
  geom_jitter(alpha=0.1, size=0.3) + 
  facet_grid(is_cpu_time ~ end) +
  scale_y_log10() +
  labs(title="Totals per point in time", x="\n\nCache temperature", y="Seconds\n")

ggplotly(plot_totals_jitter +
           theme(strip.text.x = element_text(size = 6)), tooltip="text")

# The spread operation below requires that we don't have duplicates in our input.
assert_that(!any(duplicated(select(organized, cache_temperature, site, start, end, is_cpu_time, breakdown))), msg="Duplicate rows")

breakdowns_together <- 
  organized %>%    
  spread(breakdown, value) 
breakdowns_together[is.na(breakdowns_together)] <- 0

get_endpoint_plots <- function(endpoint) {
  plots <- c()
  endpoint_df <- organized %>% filter(start=="Navigation", 
                             end==endpoint, 
                             breakdown != "total") 
  endpoint_means <- endpoint_df %>% 
    group_by(cache_temperature, breakdown, is_cpu_time) %>% 
    dplyr::summarize(value=mean(value))
  
  plots$warm_vs_cold <- endpoint_means %>% ggplot(aes(x=cache_temperature, y=value, fill=breakdown, 
                                                  text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) + 
    geom_bar(stat="identity") +
    ylim(0, NA) +
    scale_fill_manual(values=breakdown_colors) +
    labs(title=sprintf("Time To %s — Mean Contributors", endpoint), x="Cache Temperature", y="Seconds") +
    facet_grid(is_cpu_time ~ .)
  
  breakdowns_together_endpoint <- breakdowns_together %>% 
    filter(start == "Navigation", end == endpoint) %>%
    group_by(is_cpu_time, cache_temperature, start, end) %>%
    mutate(quantiles = ntile(total, 10) * 10)
  
  by_quantiles_endpoint <- breakdowns_together_endpoint %>% 
    group_by(quantiles, cache_temperature, start, end, is_cpu_time) %>% 
    dplyr::summarise_at(vars(-site), funs(mean(.)))
  
  by_quantiles_gathered_endpoint <- by_quantiles_endpoint %>% 
    gather(breakdown, value, -cache_temperature, -quantiles, -start, -end, -is_cpu_time) %>%
    filter(breakdown != "total")
  
  plots$contributors_by_quantile <- by_quantiles_gathered_endpoint %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                               text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(is_cpu_time ~ cache_temperature) +
    labs(title = sprintf("Time To %s— Contributors by Quantile", endpoint), x="Quantiles", y="Seconds")
  
  endpoint_normalized <- by_quantiles_gathered_endpoint %>% group_by(quantiles, cache_temperature, is_cpu_time) %>% mutate(value=value/sum(value))
  plots$contributors_by_quantile_normalized <- endpoint_normalized %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                                                     text=sprintf("breakdown: %s<br>value: %f%%", breakdown, value*100))) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(is_cpu_time ~ cache_temperature) +
    labs(title = sprintf("Time To %s — Normalized Contributors by Quantile", endpoint), x="Quantiles", y="Percent of time spent")
  
  return(plots)
}

## Broken down by important times.
important_times <- breakdowns_together %>% filter(
  start == 'Navigation' & end == 'First Paint' |
    start == 'First Paint' & end == 'First Contentful Paint' |
    start == 'First Contentful Paint' & end == 'First Meaningful Paint' |
    start == 'First Meaningful Paint' & end == 'First Interactive' |
    start == 'First Interactive' & end == 'Consistently Interactive') %>%
  group_by(start, end, cache_temperature, is_cpu_time) %>%
  dplyr::summarise_at(vars(-site), funs(mean(.))) %>%
  gather(breakdown, value, -cache_temperature, -start, -is_cpu_time, -end)

important_times <- important_times %>% filter(breakdown != "total")


plot_important_times <- important_times %>% ggplot(aes(x=end, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time + cache_temperature ~ .) +
  labs(title="Mean Contributors between Key Timestamps", x="End point", y="Seconds")

ggplotly(plot_important_times, tooltip="text")

# Startup vs Total for TTCI
if(FALSE) {
  ttci_breakdown_comparisons <- breakdowns_together %>% 
    filter(start=="nav", end=="ConsistentlyInteractiveBreakdown")
  
  plot_ttci_startup_vs_rest <- ttci_breakdown_comparisons %>% 
    ggplot(aes(x=startup, y=total-startup, label=site, color=script_execute)) +
    geom_point(alpha=1) +
    facet_wrap(~cache_temperature) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_continuous(trans="log10")
  ggplotly(plot_ttci_startup_vs_rest)
  
  # Total vs Script Execute for TTCI
  plot_ttci_script_execute_vs_rest <- ttci_breakdown_comparisons %>% 
    ggplot(aes(x=script_execute, y=total-startup, label=site)) +
    geom_point(alpha=0.2) +
    facet_wrap(~cache_temperature) +
    scale_x_log10() +
    scale_y_log10()
  ggplotly(plot_ttci_script_execute_vs_rest)
  
  
  p <- ttci_breakdown_comparisons %>% 
    ggplot(aes(x=total, y=v8_runtime, label=site)) +
    geom_point(alpha=0.2) +
    facet_wrap(~cache_temperature) #+
  #scale_x_log10() +
  #scale_y_log10()
  p
  ggplotly(p)
}

