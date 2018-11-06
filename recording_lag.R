library(tidyverse)
library(lubridate)

setwd('~/Dropbox/CurrentClasses/PublicSpaceResearchTutorial/Analysis/HalloweenFarmersMarket')

data <- read_csv('data/times.csv')

View(data)

time_differences <- data %>%
  mutate(TimeDifference = as.integer(difftime(TimeOfRecording, TimeOfObservation, units = 'secs')))

View(time_differences)

ggplot(time_differences,
       aes(x = TimeDifference)) +
  geom_histogram(binwidth = 60)
