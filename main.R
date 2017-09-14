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

start_timestamps <- c('nav', 'firstPaint', 'firstContentfulPaint', 'firstMeaningfulPaint', 'firstInteractive', 'consistentlyInteractive')

organized <- df %>% 
  gather(key, value, -cache_temperature, -site) %>% 
  separate(key, into=c("start", "end"), sep="To") %>%
  separate(end, into=c("end", "breakdown"), sep="-") %>%
  mutate(start = factor(start, start_timestamps),
         end = as.factor(end),
         breakdown=as.factor(breakdown), 
         site=as.factor(site)) %>%
  filter(!is.na(value))

totals <- organized %>% filter(breakdown == "total", start=="nav") 

plot_totals_violin <- totals %>% ggplot(aes(cache_temperature, value)) + 
  geom_violin() + 
  facet_wrap(~end) +
  scale_y_log10() +
  labs(title="Test title", x="Cache temperature", y="Milliseconds")
plot_totals_violin

ci <- organized %>% filter(start=="nav", 
                           end=="ConsistentlyInteractiveBreakdown", 
                           breakdown != "total", 
                           breakdown != "startup") 
ci_means <- ci %>% 
  group_by(cache_temperature, breakdown) %>% 
  dplyr::summarize(value=mean(value, na.rm=TRUE))

plot_ci_warm_vs_cold <- ci_means %>% ggplot(aes(x=cache_temperature, y=value, fill=breakdown, 
                                                text=sprintf("breakdown: %s<br>value: %f", breakdown, value))) + 
  geom_bar(stat="identity") +
  ylim(0, NA) +
  scale_fill_manual(values=breakdown_colors)
ggplotly(plot_ci_warm_vs_cold, tooltip="text")


breakdowns_together <- organized %>% spread(breakdown, value)
quantiles <- seq(0,0.9,by=0.1)
breakdowns_together$quantiles <- quantcut(breakdowns_together$total, quantiles)
levels(breakdowns_together$quantiles) <- quantiles

by_quantiles <- breakdowns_together %>% 
  group_by(quantiles, cache_temperature, start, end) %>% 
  dplyr::summarise_at(vars(-site), funs(mean(., na.rm=TRUE)))

by_quantiles_gathered <- by_quantiles %>% 
  gather(breakdown, value, -cache_temperature, -quantiles, -start, -end) %>%
  filter(breakdown != "total")

ci <- by_quantiles_gathered %>% filter(start=="nav", 
                                       end=="ConsistentlyInteractiveBreakdown",
                                       cache_temperature=="pcv1-warm", 
                                       #breakdown != "startup",
                                       breakdown != "blocked_on_network")  #TODO - enable blocked_on_network

plot_ci <- ci %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                             text=sprintf("breakdown: %s<br> value: %f", breakdown, value))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors)

ggplotly(plot_ci)

ci_normalized <- ci %>% group_by(quantiles) %>% mutate(value=value/sum(value, omit.na=TRUE))
plot_ci_normalized <- ci_normalized %>% ggplot(aes(x=quantiles, y=value, fill=breakdown, 
                                        text=sprintf("breakdown: %s<br> value: %f%%", breakdown, value*100))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors)

# Startup vs Total for TTCI
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


## Broken down by important times.
important_times <- breakdowns_together %>% filter(
  start == 'nav' & end == 'FirstPaintBreakdown' |
  start == 'firstPaint' & end == 'FirstContentfulPaintBreakdown' |
  start == 'firstContentfulPaint' & end == 'FirstMeaningfulPaintBreakdown' |
  start == 'firstMeaningfulPaint' & end == 'FirstInteractiveBreakdown' |
  start == 'firstInteractive' & end == 'ConsistentlyInteractiveBreakdown') %>%
  select(-end) %>%
  group_by(start, cache_temperature) %>%
  dplyr::summarise_at(vars(-site), funs(mean(., na.rm=TRUE))) %>%
  gather(key, value, -cache_temperature, -start)


plot_important_times <- important_times %>% ggplot(aes(x=start, y=value, fill=key)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=breakdown_colors) +
  facet_wrap(~cache_temperature)

ggplotly(plot_important_times)
