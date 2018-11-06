library(ggridges)
library(tidyverse)

set.seed(1234)

setwd('~/Dropbox/CurrentClasses/PublicSpaceResearchTutorial/Analysis/HalloweenFarmersMarket')

data <- read_csv('data/bicyclist_cleaned.csv')

non_zero_var <- function(col) {
  var(col) != 0
}

data.up <- data %>%
  select(-c(LocationLatitude, LocationLongitude)) %>%
  select_if(non_zero_var)

print(setdiff(colnames(data), colnames(data.up)))

# View(data.up)

data.up.scaled <- as.tibble(scale(data.up))

hc.complete <- hclust(dist(data.up.scaled), method = 'complete')
hc.average <- hclust(dist(data.up.scaled), method = 'average')
dd <- as.dist(1-cor(t(data.up.scaled)))
hc.corr_complete <- hclust(dd, method = 'complete') 

par(mfrow=c(1,3))
plot(hc.complete, main = 'Complete Linkage', xlab = '', sub = '', cex = 0.9, hang = -1)
plot(hc.corr_complete, main = 'Complete Linkage with Correlation-Based Distance', xlab = '', sub = '', hang = -1)
plot(hc.average, main = 'Average Linkage', xlab = '', sub = '', cex = 0.9, hang = -1)

cuts <- cutree(hc.corr_complete, 4)

data.cuts <- data.up.scaled %>%
  mutate(Cut = factor(cuts))

pca <- prcomp(data.up.scaled)
scores <- as.tibble(pca$x) %>%
  select(PC1, PC2)

data.cuts_and_scores <- data.cuts %>%
  mutate(PC1 = scores$PC1,
         PC2 = scores$PC2)

top_sig_loadings <- function(loadings, top_n = 5) {
  loadings %>%
    mutate(distance = sqrt(PC1 ^ 2 + PC2 ^ 2)) %>%
    top_n(top_n, distance)
}

loadings <- as.tibble(pca$rotation, rownames = NA) %>%
  rownames_to_column('Var') %>%
  select(Var, PC1, PC2)

n <- dim(loadings)[1]
top_loadings <- top_sig_loadings(loadings, 10) # %>%
#  mutate(PC1 = jitter(PC1, factor = 3), PC2 = jitter(PC2, factor = 3))

# Used for labels + lines for loadings
e <- 0.03
top_loadings.shifted <- top_loadings %>%
  mutate(PC2 = ifelse(PC2 > 0, PC2 - e, PC2 + e))

top_loadings.with_center <- top_loadings %>%
  mutate(PC1 = 0, PC2 = 0) %>%
  bind_rows(top_loadings.shifted)

cyclist_labels <- c('Nervous cyclist', 'Mid-age, confident cyclist', 'Older age, confident cyclist', 'Group cyclist')
levels(data.cuts_and_scores$Cut) <- cyclist_labels

# par(mfrow=c(1, 1))
# biplot(pca)

ggplot() +
  geom_point(data = data.cuts_and_scores,
             aes(x = PC1,
                 y = PC2,
                 color = Cut),
             alpha = 0.25, size = 5) +
  scale_colour_brewer(palette = 'Set1') +
  geom_text(data = top_loadings,
            aes(x = PC1 * 5,
                y = PC2 * 5,
                label = Var),
            alpha = 0.75,
            size = 3,
            color = 'black') +
  geom_line(data = top_loadings.with_center,
            aes(x = PC1 * 5,
                y = PC2 * 5,
                group = Var),
            size = 0.25, alpha = 0.25) +
  ggtitle('First two principal component scores of clustered bicyclist observation data with most significanct feature directions/loadings') +
  labs(color = 'Cluster (HBU)')

get_summary <- function(table) {
  column_names <- colnames(table)
  num_rows <- dim(table)[1]
  summaries <- vector(length = num_rows)
  # Doesn't need to exclude 'PC1' and 'PC2' anymore
  skip_columns <- c('Cut', 'PC1', 'PC2')
  summary_columns <- setdiff(column_names, skip_columns)
  for (i in 1:num_rows) {
    current_summary <- c()
    for (column_name in summary_columns) {
      val <- table[i, column_name]
      include_in_summary <- val == 1
      if (include_in_summary) {
        current_summary <- c(column_name, current_summary) 
      }
    }
    current_summary <- paste(current_summary, collapse = ', ')
    summaries[i] <- current_summary
  }
  summaries
}

save_cut_summary <- function(table) {
  file_path <- table$file_path[1]
  write_csv(table %>% select(Summary), path = file_path)
  table
}

unlink('data/groups', recursive = TRUE)
dir.create('data/groups')

# Needs data.up for hot encoded categories
data.summary <- data.up %>%
  mutate(Cut = factor(cuts)) %>%
  mutate(Summary = get_summary(.)) %>%
  group_by(Cut) %>%
  mutate(file_path = str_c('data/groups/bicyclist_', Cut, '.csv')) %>%
  do(save_cut_summary(.))

# This could be cleaned up to work better with next components
# Just comment for now since below needs hot encodings
# data.summary <- data.cuts_and_components %>%
#   mutate(Summary = get_summary(.)) %>%
#   group_by(Cut) %>%
#   mutate(file_path = str_c('data/groups/bicyclist_', Cut, '.csv')) %>%
#   do(save_cut_summary(.))

# View(data.summary)

# Next: create histograms of summary
#       columns for each of the cuts
#       and save them somewhere

loading_significance <- loadings %>%
  mutate(Significance = sqrt(PC1 ^ 2 + PC2 ^ 2)) %>%
  rename(SummaryFeature = Var)

# View(loading_significance)

loading_significance.ordered <- loading_significance[order(loading_significance$Significance),]
loading_significance.levels <- rev(loading_significance.ordered$SummaryFeature)

loading_significance.ordered$SummaryFeature = factor(loading_significance.ordered$SummaryFeature, levels = loading_significance.levels)

cut_counts <- data.summary %>%
  group_by(Cut) %>%
  summarise(CutCount = n()) %>%
  ungroup()

# Don't use data.summary anymore - use data.cuts_and_scores (ideally)
data.summary_features <- data.summary %>%
  select(-c(Summary, file_path)) %>%
  gather('SummaryFeature', 'SummaryValue', BehaviorRiding:LocationNoSidewalk) %>%
  filter(SummaryValue == 1) %>%
  group_by(Cut, SummaryFeature) %>%
  summarise(SummaryFeatureCount = n()) %>%
  ungroup() %>%
  spread(SummaryFeature, SummaryFeatureCount) %>%
  replace(., is.na(.), 0) %>% # Replace NAs with 0
  gather('SummaryFeature', 'SummaryFeatureCount', BehaviorCrossing:RiderOutfitOther) %>%
  left_join(cut_counts, by = 'Cut') %>%
  mutate(SummaryFeatureFreq = SummaryFeatureCount / CutCount) %>%
  select(Cut, SummaryFeature, SummaryFeatureFreq, SummaryFeatureCount, CutCount) %>%
  mutate(SummaryFeature = factor(SummaryFeature, levels = loading_significance.levels)) %>%
  left_join(loading_significance.ordered %>% select(c(SummaryFeature, Significance)), by = 'SummaryFeature')

# View(data.summary_features)

cyclist_labels <- c('Nervous cyclist', 'Mid-age, confident cyclist', 'Older age, confident cyclist', 'Group cyclist')
levels(data.summary_features$Cut) <- cyclist_labels

ggplot() +
  geom_bar(data = data.summary_features,
           aes(x = SummaryFeature,
               y = SummaryFeatureFreq,
               fill = Significance),
           stat = 'identity') +
  scale_fill_gradient(high = '#3a005b', low = '#dfd5e5') +
  facet_grid(rows = vars(Cut)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle('Frequency of observation features between four types of cyclists in order of feature significance') +
  xlab('Observation Feature') +
  ylab('Frequency (per group)') +
  labs(fill = 'Feature Significance\n(L2 Norm of First Two Loadings)')

# How about one that is in one plot with overlapping bars/lines 

# https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points
spline_it <- function(table) {
  as.tibble(spline(table$SummaryFeature, table$SummaryFeatureFreq))
}

data.splined <- data.summary_features %>%
  group_by(Cut) %>%
  do(spline_it(.))

View(data.splined)

data.splined$Cut <- as.character(data.splined$Cut)
# https://stackoverflow.com/questions/48268022/ggplot2-shade-area-under-curve-by-group/48269825#48269825
ggplot(data = data.splined,
       aes(x = x,
           y = y,
          # color = Cut,
           group = Cut,
           fill = Cut)) +
  geom_density_line(stat = 'identity', size = 0.5, alpha = 0.2) +
  scale_fill_manual(name='', values=c("1" = "red", "2" = "green4", "3" = "blue", "4" = "yellow"))

