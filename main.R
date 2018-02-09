library(plotly)
library(gtools)
library(tidyverse)
library(parallel)

source("breakdown_colors.R")

options(scipen=10000)
options(mc.cores = 45)

df_sf <- read_csv('important_timestamps_sf.csv', col_types=cols(
  site=col_character(),
  cache_temperature=readr::col_factor(c("warm", "cold", "hot")),
  .default=col_number())) %>% mutate(subresource_filter = "Subresource Filter On")

df_sf_disabled <- read_csv('important_timestamps_sf_disabled.csv', col_types=cols(
  site=col_character(),
  cache_temperature=readr::col_factor(c("warm", "cold", "hot")),
  .default=col_number())) %>% mutate(subresource_filter = "Subresource Filter Off")

df <- bind_rows(df_sf, df_sf_disabled)
rm(df_sf, df_sf_disabled)

# Remove unneeded columns:
df <- df %>% select(cache_temperature, site, subresource_filter, matches("navToFirstPaint|navToFirstContentfulPaint|navToFirstMeaningfulPaint|navToFirstInteractive|navToConsistentlyInteractive"))

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
friendly_timestamps <- c('Navigation', 'First Paint', 'First Contentful Paint', 'First Meaningful Paint', 'First Interactive', 'Consistently Interactive')

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

levels(organized$cache_temperature) <- c("Warm", "Cold", "Hot")
organized$cache_temperature <- factor(organized$cache_temperature, levels=c("Cold", "Warm", "Hot"))
levels(organized$start) <- friendly_timestamps
levels(organized$end) <- friendly_timestamps
levels(organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")
organized$breakdown <- organized$breakdown %>% fct_relevel("idle", after=Inf)

totals <- organized %>% filter(breakdown == "total", start=="Navigation")

plot_totals_jitter_sampled <- totals %>% sample_frac(0.1) %>%
  ggplot(aes(cache_temperature, value, text=sprintf("site: %s<br>value: %f", site, value))) +
  geom_jitter(alpha=0.1, size=0.3) +
  facet_grid(end ~ I(paste(as.character(is_cpu_time), as.character(subresource_filter), sep=" : "))) +
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
    labs(x="&nbsp;<br>Cache Temperature", y="Seconds<br>&nbsp;") +
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
    labs(x="&nbsp;<br>Quantiles", y="Seconds<br>&nbsp;")

  endpoint_normalized <- by_quantiles_gathered_endpoint %>% group_by(quantiles, cache_temperature, is_cpu_time, subresource_filter) %>% mutate(value=value/sum(value))
  plots$contributors_by_quantile_normalized <- endpoint_normalized %>% ggplot(aes(x=quantiles, y=value, fill=breakdown,
                                                     text=sprintf("breakdown: %s<br>value: %f%%", breakdown, value*100))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(is_cpu_time + subresource_filter ~ cache_temperature) +
    labs(x="&nbsp;<br>Quantiles", y="Percent of time spent<br>&nbsp;")

  return(plots)
}

## Broken down by important times.
important_times <- breakdowns_together %>%
  group_by(start, end, cache_temperature, is_cpu_time, subresource_filter) %>%
  dplyr::summarise_at(vars(-site), funs(mean(.))) %>%
  gather(breakdown, value, -cache_temperature, -start, -is_cpu_time, -end, -subresource_filter)

important_times <- important_times %>% filter(breakdown != "total")

plot_important_times <- important_times %>% ggplot(aes(x=end, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(is_cpu_time + cache_temperature ~ subresource_filter) +
  labs(x="&nbsp;<br>End point", y="Seconds<br>&nbsp;")

ggplotly(plot_important_times, tooltip="text")

sites_ordered_by_tti <- organized %>% 
  filter(end == "Consistently Interactive", 
         breakdown == "total", 
         cache_temperature == "Cold", 
         subresource_filter == "Subresource Filter Off",
         is_cpu_time == "Wall Clock Time") %>% 
  select(site, value) %>% 
  arrange(-value)

save(sites_ordered_by_tti, file="sites_ordered_by_tti.RData")

# rm(df)
save.image(file = ".RData.main")
