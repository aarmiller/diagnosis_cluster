
rm(list =ls())
library(tidyverse)
source("functions/cluster_funcs.R")

# load model fits
model_fits <- read_csv("data/sythetic_fits_tb_7day.csv")


### Find a single cluster ------------------------------------------------------

# find clusters
clust_data <- find_clusters(model_fits,
                            normalize = TRUE,
                            k = 15)

# search cluster for term
search_clust_terms(clust_data = clust_data,
                   search_term = c("cough"))

# return other conditions in cluster
search_clusters(clust_data = clust_data,
                search_code = c("7862"))


### Evaluate multiple values of k  ---------------------------------------------

# get clusters for different k=5:25
clust_res <- tibble(k=5:25) %>%
  mutate(clusters = map(k,~find_clusters(fit_data = model_fits,
                                         k =.x)))

# extract focal cluster
clust_res <- clust_res %>%
  mutate(focal_cluster=map(clusters,~search_clusters(clust_data = .,
                                                     search_code = "7862")))

# count how often each code appeared in the focal cluster
clust_res %>%
  select(k,focal_cluster) %>%
  unnest(focal_cluster) %>%
  count(code,description) %>%
  arrange(desc(n))




