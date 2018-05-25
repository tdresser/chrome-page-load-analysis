# This is a big enough data set that you may need to set `ulimit -s 1280000`.
library(tidyverse)
library(plotly)
library(gtools)
library(stringr)

source("breakdown_colors.R")

options(scipen=10000)

per_second_df_temperatures <- read_csv('per_second2.csv', col_types=cols(
  page_name=col_character(),
  traceUrls=col_character(),
  .default=col_number())) %>% mutate(subresource_filter = "Subresource Filter Off") %>%
  select(site=page_name, everything())

#df_sf_disabled <- read_csv('per_second_sf_disabled.csv', col_types=cols(
#  site=col_character(),
#  cache_temperature=readr::col_factor(c("warm", "cold", "hot")),
#  .default=col_number())) %>% mutate(subresource_filter = "Subresource Filter Off")

#per_second_df <- bind_rows(df_sf, df_sf_disabled)
#rm(df_sf, df_sf_disabled)

# Only care about seconds from 1 to 30.
per_second_df <- per_second_df_temperatures %>% 
  dplyr::select(site, traceUrls, subresource_filter, dplyr::matches(".*nav[0-9]+secTo([0-9]|[1-2][0-9]|30)sec.*$")) %>%
  gather(key="m", value="v", dplyr::matches(".*nav.*")) %>%
  separate(m, c("cache_temperature", "m"), extra="merge") %>%
  spread(m, v)  %>%
  rename_all(. %>% gsub(" \\(ms\\)", "", .))
  

per_second_df$subresource_filter <- as.factor(per_second_df$subresource_filter)

# For some reason we get some duplicate data. Remove it.
# TODO - we should remove this once this input data doesn't have duplicate entries.
per_second_df <- per_second_df %>% distinct(site, cache_temperature, subresource_filter, .keep_all = TRUE)

# filter out cases where the total is NA for 30th second
num_occurrences_of_site <- per_second_df %>%
  filter(!is.na(`nav29secTo30secBreakdown-total`)) %>%
  select(site) %>%
  group_by(site) %>%
  summarize(count=n())
sites_to_keep <- num_occurrences_of_site[num_occurrences_of_site$count == max(num_occurrences_of_site$count),]
per_second_df <- semi_join(per_second_df, sites_to_keep, by="site")

# Now that we got rid of all the sites with missing data, we should be safe to replace NAs with 0.
per_second_df <- per_second_df %>% mutate_at(vars(matches('nav')), . %>% replace(., is.na(.), 0))

per_second_organized <- per_second_df %>%
  gather(key, value, -subresource_filter, -cache_temperature, -site, -traceUrls) %>%
  tidyr::extract("key", c("start", "end", "is_cpu_time", "breakdown"), 
                 regex = "nav([0-9]*)secTo([0-9]*)secBreakdown(CpuTime)?-(.*)", convert=TRUE, remove=TRUE, perl=TRUE, useBytes=TRUE) %>%
  mutate(breakdown=as.factor(breakdown), 
         site=as.factor(site),
         is_cpu_time = as.factor(!is.na(is_cpu_time)),
         start=as.numeric(start),
         end=as.numeric(end),
         value = value / 1000)

per_second_organized$cache_temperature <- factor(per_second_organized$cache_temperature, levels=c("cold", "warm", "hot"))
levels(per_second_organized$cache_temperature) <- c("Cold", "Warm", "Hot")
levels(per_second_organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")

per_second_breakdowns_together <- per_second_organized %>% 
  group_by(site, cache_temperature, subresource_filter, start, end, is_cpu_time, breakdown) %>% 
  #dplyr::summarize(value=mean(value))  %>% # Where are the duplicates coming from?
  spread(breakdown, value)

per_second_breakdowns <- function (per_second_breakdowns_together) {
  per_second <- per_second_breakdowns_together %>%
    group_by(start, end, subresource_filter, cache_temperature, is_cpu_time) %>%
    dplyr::summarise_at(vars(-site), funs(mean(.))) %>%
    gather(breakdown, value, -start, -subresource_filter, -cache_temperature, -end, -is_cpu_time) %>%
    filter(breakdown != 'total')
  
  per_second$breakdown <- per_second$breakdown %>% fct_relevel("idle", after=Inf)
  
  plot_per_second <- per_second %>% ggplot(aes(x=start, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=breakdown_colors) +
    facet_grid(~cache_temperature~is_cpu_time + subresource_filter) +
    labs(x="&nbsp;<br>Time in Seconds", y="Seconds<br>&nbsp;")
  return(plot_per_second)  
}

plot_per_second <- per_second_breakdowns(per_second_breakdowns_together)
ggplotly(plot_per_second, tooltip="text")

load('sites_ordered_by_tti.RData')
sites_ordered_by_tti <- sites_ordered_by_tti %>% semi_join(per_second_breakdowns_together, by="site")

per_second_breakdowns_together_worst100 <- per_second_breakdowns_together %>% 
  semi_join(sites_ordered_by_tti %>% head(n=100), by="site")
per_second_breakdowns_together_best100 <- per_second_breakdowns_together %>% 
  semi_join(sites_ordered_by_tti %>% tail(n=100), by="site")

plot_per_second_worst100 <- per_second_breakdowns(per_second_breakdowns_together_worst100)
plot_per_second_best100 <- per_second_breakdowns(per_second_breakdowns_together_best100)

save.image(file = ".RData.per_second")