library(plotly)
library(gtools)
library(tidyverse)
library(parallel)

source("breakdown_colors.R")

options(scipen=10000)
options(mc.cores = 45)

df_temperatures <- read_csv('per_second2.csv', col_types=cols(
  page_name=col_character(),
  traceUrls=col_character(),
  .default=col_number())) %>% mutate(subresource_filter = "Subresource Filter Off") %>%
  select(site=page_name, everything())

# Remove unneeded columns:
df <- df_temperatures %>% select(site, subresource_filter, matches("navToFirstPaint|navToFirstContentfulPaint|navToFirstMeaningfulPaint|navToFirstInteractive|navToConsistentlyInteractive")) %>%
  gather(key="m", value="v", dplyr::matches(".*nav.*")) %>%
  separate(m, c("cache_temperature", "m"), extra="merge") %>%
  spread(m, v)  %>%
  rename_all(. %>% gsub(" \\(ms\\)", "", .))

# For some reason we get some duplicate data. Remove it.
# TODO - we should remove this once this input data doesn't have duplicate entries.
df <- df %>% distinct(site, cache_temperature, subresource_filter, .keep_all = TRUE)

# Find cases where we're missing data for some sites, and completely remove those sites.

num_occurrences_of_site <- df %>%
   filter(!is.na(`navToConsistentlyInteractiveBreakdown-total`)) %>%
   select(site) %>%
   group_by(site) %>%
   summarize(count=n())
sites_to_keep <- num_occurrences_of_site[num_occurrences_of_site$count == max(num_occurrences_of_site$count),]
df <- semi_join(df, sites_to_keep, by="site")

# Now that we got rid of all the sites with missing data, we should be safe to replace NAs with 0.
df <- df %>% mutate_all(funs(replace(., is.na(.), 0)))

timestamps <- c('nav', 'firstPaint', 'firstContentfulPaint', 'firstMeaningfulPaint', 'firstInteractive', 'consistentlyInteractive')
friendly_timestamps <- c('Navigation', 'First Paint', 'First Contentful Paint', 'First Meaningful Paint', 'CPU Idle', 'Interactive')
abbreviated_timestamps <- c('Nav', 'FP', 'FCP', 'FMP', 'CPU Idle', 'TTI')

gathered <- organized <- df %>%
  gather(key, value, -cache_temperature, -site, -subresource_filter)

splitKey <- function(df) {
  # perl & useBytes are supposed to speed this up.
  return(df %>% tidyr::extract_("key", c("start", "end", "is_cpu_time", "breakdown"),
                                regex = "(.*)To(.*)Breakdown(CpuTime)?-(.*)", convert=TRUE, remove=TRUE, perl=TRUE, useBytes=TRUE))
}

# This is slow, but I haven't figured out how to make it faster, other than parallelizing it.
# Split into 20 groups, run the tidyr extract on each group as a separate thread, then merge.
start <- proc.time()
tidied <- bind_rows(mclapply(split(gathered, sample(1:20, nrow(gathered), replace=T)), splitKey))
end <- proc.time()
print("Time taken for column splitting")
print(end - start)

rm(gathered)

organized <- tidied %>% mutate(start = factor(tolower(start), tolower(timestamps)),
         end = factor(tolower(end), tolower(timestamps)),
         is_cpu_time = as.factor(!is.na(is_cpu_time)),
         subresource_filter = as.factor(subresource_filter),
         breakdown=as.factor(breakdown),
         site=as.factor(site),
         value = value / 1000) %>%
  filter(!is.na(value), value != 0)

organized$cache_temperature <- factor(organized$cache_temperature, levels=c("cold", "warm", "hot"))
levels(organized$cache_temperature) <- c("Cold", "Warm", "Hot")

levels(organized$start) <- friendly_timestamps
levels(organized$end) <- friendly_timestamps
levels(organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")
organized$breakdown <- organized$breakdown %>% fct_relevel("idle", after=Inf)

totals <- organized %>% filter(breakdown == "total", start=="Navigation")

plot_totals_jitter_sampled <- totals %>% sample_frac(0.1) %>%
  ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) +
  geom_jitter(alpha=0.1, size=0.3) +
  facet_grid(end ~ is_cpu_time + subresource_filter) +
  scale_y_log10() 

plot_totals_jitter <- totals %>%
  ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) +
  geom_jitter(alpha=0.1, size=0.3) +
  facet_grid(end ~ is_cpu_time + subresource_filter) +
  scale_y_log10()

# plot_totals_jitter_sampled

# The spread operation below requires that we don't have duplicates in our input.
assert_that(!any(duplicated(select(organized, site, cache_temperature, start, end, subresource_filter, is_cpu_time, breakdown))), msg="Duplicate rows")

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
    group_by(cache_temperature, breakdown, is_cpu_time, subresource_filter) %>%
    dplyr::summarize(value=mean(value))

  plots$warm_vs_cold <- endpoint_means %>% ggplot(aes(x=cache_temperature, y=value, fill=breakdown,
                                                  text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
    geom_bar(stat="identity") +
    ylim(0, NA) +
    scale_fill_manual(values=breakdown_colors) +
    labs(x="Cache Temperature", y="Seconds") +
    facet_grid(is_cpu_time + subresource_filter ~ .)

  breakdowns_together_endpoint <- breakdowns_together %>%
    filter(start == "Navigation", end == endpoint) %>%
    group_by(is_cpu_time, cache_temperature, start, end, subresource_filter) %>%
    mutate(quantiles = ntile(total, 10) * 10)

  by_quantiles_endpoint <- breakdowns_together_endpoint %>%
    group_by(quantiles, cache_temperature, start, end, is_cpu_time, subresource_filter) %>%
    dplyr::summarise_at(vars(-site), funs(mean(.)))

  by_quantiles_gathered_endpoint <- by_quantiles_endpoint %>%
    gather(breakdown, value, -cache_temperature, -quantiles, -start, -end, -is_cpu_time, -subresource_filter) %>%
    filter(breakdown != "total")

  by_quantiles_gathered_endpoint$breakdown <- by_quantiles_gathered_endpoint$breakdown %>% fct_relevel("idle", after=Inf)

  plots$contributors_by_quantile <- by_quantiles_gathered_endpoint %>% ggplot(aes(x=quantiles, y=value, fill=breakdown,
                               text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(is_cpu_time + subresource_filter ~ cache_temperature) +
    labs(x="Quantiles", y="Seconds")

  endpoint_normalized <- by_quantiles_gathered_endpoint %>% group_by(quantiles, cache_temperature, is_cpu_time, subresource_filter) %>% mutate(value=value/sum(value))
  plots$contributors_by_quantile_normalized <- endpoint_normalized %>% ggplot(aes(x=quantiles, y=value, fill=breakdown,
                                                     text=sprintf("breakdown: %s<br>value: %f%%", breakdown, value*100))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(is_cpu_time + subresource_filter ~ cache_temperature) +
    labs(x="Quantiles", y="Percent of time spent")

  return(plots)
}

## Broken down by important times.
important_times <- breakdowns_together %>%
  group_by(start, end, cache_temperature, is_cpu_time, subresource_filter) %>%
  dplyr::summarise_at(vars(-site), funs(mean(.))) %>%
  gather(breakdown, value, -cache_temperature, -start, -is_cpu_time, -end, -subresource_filter)

important_times <- important_times %>% filter(breakdown != "total")

levels(important_times$end) <- abbreviated_timestamps

plot_important_times <- important_times %>% ggplot(aes(x=end, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time + cache_temperature ~ subresource_filter) +
  labs(x="End point", y="Seconds")

ggplotly(plot_important_times, tooltip="text")

sites_ordered_by_tti <- organized %>% 
  filter(end == "Interactive", 
         breakdown == "total", 
         cache_temperature == "Cold", 
         subresource_filter == "Subresource Filter Off",
         is_cpu_time == "Wall Clock Time") %>% 
  select(site, value) %>% 
  arrange(-value)

save(sites_ordered_by_tti, file="sites_ordered_by_tti.RData")

# rm(df)
save.image(file = ".RData.main")
