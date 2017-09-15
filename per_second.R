# This is a big enough data set that you may need to set `ulimit -s 1280000`.
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gtools)

source("breakdown_colors.R")

options(scipen=10000)

per_second_df <- read_csv('per_second.csv', col_types=cols(
  site=col_character(), 
  cache_temperature=readr::col_factor(c("pcv1-warm", "pcv1-cold")), 
  .default=col_number()))

# sort(colnames(per_second_df))

per_second_organized <- per_second_df %>%
  gather(key, value, -cache_temperature, -site) %>%
  tidyr::extract("key", c("start", "end", "is_cpu_time", "breakdown"), 
                 regex = "nav([0-9]*)secTo([0-9]*)secBreakdown(CpuTime)?-(.*)", convert=TRUE, remove=TRUE) %>%
  mutate(breakdown=as.factor(breakdown), 
         site=as.factor(site),
         is_cpu_time = as.factor(!is.na(is_cpu_time)),
         start=as.numeric(start),
         end=as.numeric(end),
         value = value / 1000) %>%
  filter(!is.na(value), value != 0)

levels(per_second_organized$cache_temperature) <- c("Warm", "Cold")
per_second_organized$cache_temperature <- factor(per_second_organized$cache_temperature, levels=c("Cold", "Warm"))
levels(per_second_organized$is_cpu_time) <- c("Wall Clock Time", "CPU Time")

per_second_breakdowns_together <- per_second_organized %>% 
  filter(start < 30) %>% 
  group_by(site, cache_temperature, start, end, is_cpu_time, breakdown) %>% 
  dplyr::summarize(value=value)  %>% # Where are the duplicates coming from?
  spread(breakdown, value)
  
# Mean fails when NAs are removed.
per_second_breakdowns_together[is.na(per_second_breakdowns_together)] <- 0

per_second <- per_second_breakdowns_together %>%
  group_by(start, end, cache_temperature, is_cpu_time) %>%
  dplyr::summarise_at(vars(-site), funs(mean(.))) %>%
  gather(breakdown, value, -start, -cache_temperature, -end, -is_cpu_time) %>%
  filter(breakdown != 'total')

plot_important_times <- per_second %>% ggplot(aes(x=start, y=value, fill=breakdown, text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(~cache_temperature~is_cpu_time) +
  labs(title="Mean Contributors over time.", x="Time in Seconds", y="Seconds")
ggplotly(plot_important_times, tooltip="text")
