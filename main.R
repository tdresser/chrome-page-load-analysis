library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gtools)

source("breakdown_colors.R")

df <- read_csv('important_timestamps.csv', col_types=cols(
  site=col_character(), 
  cache_temperature=readr::col_factor(c("pcv1-warm", "pcv1-cold")), 
  .default=col_number()))

#sort(colnames(df))

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
         site=as.factor(site)) %>%
  filter(!is.na(value))

levels(organized$cache_temperature) <- c("Warm", "Cold")
levels(organized$start) <- friendly_timestamps
levels(organized$end) <- friendly_timestamps
levels(organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")

totals <- organized %>% filter(breakdown == "total", start=="Navigation") 

plot_totals_violin <- totals %>% ggplot(aes(cache_temperature, value)) + 
  geom_violin() + 
  facet_grid(is_cpu_time ~ end) +
  scale_y_log10() +
  labs(title="Totals per point in time", x="Cache temperature", y="Milliseconds")
plot_totals_violin

plot_totals_jitter <- totals %>% ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) + 
  geom_jitter(alpha=0.1, size=0.3) + 
  facet_grid(is_cpu_time ~ end) +
  scale_y_log10() +
  labs(title="Totals per point in time", x="\n\nCache temperature", y="Milliseconds\n")

ggplotly(plot_totals_jitter +
           theme(strip.text.x = element_text(size = 6)), tooltip="text")

ci <- organized %>% filter(start=="Navigation", 
                           end=="Consistently Interactive", 
                           breakdown != "total", 
                           breakdown != "startup") 
ci_means <- ci %>% 
  group_by(cache_temperature, breakdown, is_cpu_time) %>% 
  dplyr::summarize(value=mean(value, na.rm=TRUE))

plot_ci_warm_vs_cold <- ci_means %>% ggplot(aes(x=cache_temperature, y=value, fill=breakdown, 
                                                text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) + 
  geom_bar(stat="identity") +
  ylim(0, NA) +
  scale_fill_manual(values=breakdown_colors) +
  labs(title="Time To Consistently Interactive — Mean Contributors", x="Cache Temperature", y="Milliseconds") +
  facet_grid(is_cpu_time ~ .)
ggplotly(plot_ci_warm_vs_cold, tooltip="text")


breakdowns_together <- organized %>% spread(breakdown, value)

breakdowns_together_ci <- breakdowns_together %>% filter(start == "Navigation", end == "Consistently Interactive")
quantiles <- seq(0,1,by=0.1)
breakdowns_together_ci$quantiles <- quantcut(breakdowns_together_ci$total, quantiles)
levels(breakdowns_together_ci$quantiles) <- quantiles

by_quantiles <- breakdowns_together_ci %>% 
  group_by(quantiles, cache_temperature, start, end, is_cpu_time) %>% 
  dplyr::summarise_at(vars(-site), funs(mean(., na.rm=TRUE)))

by_quantiles_gathered <- by_quantiles %>% 
  gather(breakdown, value, -cache_temperature, -quantiles, -start, -end, -is_cpu_time) %>%
  filter(breakdown != "total")

ci <- by_quantiles_gathered %>% filter(breakdown != "blocked_on_network")  #TODO - enable blocked_on_network

plot_ci <- ci %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                             text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time ~ cache_temperature) +
  labs(title = "Time To Consistently Interactive — Contributors by Quantile", x="Quantiles", y="Milliseconds")

ggplotly(plot_ci, tooltip="text")

ci_normalized <- ci %>% group_by(quantiles, cache_temperature, is_cpu_time) %>% mutate(value=value/sum(value, na.rm=TRUE))
plot_ci_normalized <- ci_normalized %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                                        text=sprintf("breakdown: %s<br>value: %f%%", breakdown, value*100))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time ~ cache_temperature) +
  labs(title = "Time To Consistently Interactive — Normalized Contributors by Quantile", x="Quantiles", y="Milliseconds")

ggplotly(plot_ci_normalized, tooltip="text")

## Broken down by important times.
important_times <- breakdowns_together %>% filter(
  start == 'Navigation' & end == 'First Paint' |
  start == 'First Paint' & end == 'First Contentful Paint' |
  start == 'First Contentful Paint' & end == 'First Meaningful Paint' |
  start == 'First Meaningful Paint' & end == 'First Interactive' |
  start == 'First Interactive' & end == 'Consistently Interactive') %>%
  group_by(start, end, cache_temperature, is_cpu_time) %>%
  dplyr::summarise_at(vars(-site), funs(mean(., na.rm=TRUE))) %>%
  gather(breakdown, value, -cache_temperature, -start, -is_cpu_time, -end)

important_times <- important_times %>% filter(breakdown != "total", breakdown != "startup")


plot_important_times <- important_times %>% ggplot(aes(x=end, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time + cache_temperature ~ .) +
  labs(title="Mean Contributors", x="End point", y="Milliseconds")

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

