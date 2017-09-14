# This is a big enough data set that you may need to set `ulimit -s 1280000`.
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gtools)

source("breakdown_colors.R")

per_second_df <- read_csv('per_second.csv', col_types=cols(
  site=col_character(), 
  cache_temperature=readr::col_factor(c("pcv1-warm", "pcv1-cold")), 
  .default=col_number()))

sort(colnames(per_second_df))

per_second_organized <- per_second_df %>%
  gather(key, value, -cache_temperature, -site) %>%
  tidyr::extract("key", c("start", "end", "is_cpu_time", "breakdown"), 
                 regex = "nav([0-9]*)secTo([0-9]*)secBreakdown(CpuTime)?-(.*)", convert=TRUE, remove=TRUE) %>%
  mutate(breakdown=as.factor(breakdown), 
         site=as.factor(site),
         is_cpu_time = !is.na(is_cpu_time),
         start=as.numeric(start),
         end=as.numeric(end)) %>%
  filter(!is.na(value))

per_second_breakdowns_together <- per_second_organized %>% 
  filter(start < 30) %>% 
  group_by(site, cache_temperature, start, end, is_cpu_time, breakdown) %>% 
  dplyr::summarize(value=mean(value))  %>% # Where are the duplicates coming from?
  spread(breakdown, value)
  
per_second <- per_second_breakdowns_together %>%
  group_by(start, end, cache_temperature, is_cpu_time) %>%
  dplyr::summarise_at(vars(-site), funs(mean(., na.rm=TRUE))) %>%
  gather(key, value, -start, -cache_temperature, -end, -is_cpu_time) %>%
  filter(key != 'total')

plot_important_times <- per_second %>% ggplot(aes(x=start, y=value, fill=key)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_grid(~cache_temperature~is_cpu_time)
ggplotly(plot_important_times)