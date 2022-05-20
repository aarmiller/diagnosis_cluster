euclidean <- function(a, b) sqrt(sum((a - b)^2))


find_clusters <- function(fit_data, code_type = "dx9", model = "lm",
                          normalize = TRUE, k = 5) {

  # prep data
  tmp <- fit_data[[code_type]][[model]] %>% na.omit

  if (normalize == TRUE){
    tmp[,4:ncol(tmp)] <- tmp[,4:ncol(tmp)] %>%
      mutate_all(~(.-mean(.))/sd(.))
  }

  km.res <- kmeans(tmp[,4:ncol(tmp)], k,  nstart = 50,iter.max = 50)

  tmp <- tmp %>%
    mutate(cluster=km.res$cluster)

  tmp_centers <-  km.res$centers %>%
    as_tibble() %>%
    mutate(cluster=row_number()) %>%
    select(cluster,everything()) %>%
    group_by(cluster) %>%
    nest() %>%
    mutate(center = map(data,~as.numeric(as.vector(.)))) %>%
    select(cluster,center)

  tmp_distances <- tmp[c(1,4:ncol(tmp))] %>%
    group_by(index,cluster) %>%
    nest() %>%
    mutate(v1 = map(data,~as.numeric(as.vector(.)))) %>%
    inner_join(tmp_centers, by = "cluster") %>%
    mutate(center_distance = map2_dbl(v1,center,euclidean)) %>%
    select(index,cluster,v1,center,center_distance)

  return(list(clusters = tmp,
              centers = tmp_centers,
              distances = tmp_distances))

}


search_clusters <- function(clust_data, search_code){

  clusters <- clust_data$clusters

  distances <- clust_data$distances

  # pull out code to focus on
  focus_code <- clusters %>%
    filter(code %in% search_code)

  # pull out center of focus code
  focus_center <- focus_code %>%
    group_by(cluster) %>%
    arrange(cluster,index) %>%
    filter(row_number()==1) %>%
    .[4:(ncol(focus_code))] %>%
    nest() %>%
    mutate(focus_center = map(data,~as.numeric(as.vector(.)))) %>%
    select(-data)


  cluster_data <- focus_code %>%
    distinct(cluster) %>%
    inner_join(clusters, by = "cluster")

  out_distances <- distances %>%
    inner_join(focus_center, by = "cluster") %>%
    mutate(focus_distance = map2_dbl(v1,focus_center,euclidean)) %>%
    select(cluster,index,center_distance,focus_distance) %>%
    arrange(cluster,focus_distance) %>%
    ungroup() %>%
    inner_join(select(cluster_data,index:description), by = "index")



  return(out_distances)
}

search_clust_terms <- function(clust_data,search_term){

  if (length(search_term)==1) {
    clust_data$clusters %>%
      filter(str_detect(tolower(description),tolower(search_term)))
  } else {
    out_list <- list()
    for (i in search_term){
      out_list[[i]] <-   clust_data$clusters %>%
        filter(str_detect(tolower(description),tolower(i)))
    }
    return(out_list)
  }

}

plot_cluster_code_group <- function(clust_data,search_code,size,code_type, span = 0.75,
                                    bin_by=1){

  tmp <- search_clusters(clust_data = clust_data,
                         search_code = search_code)$distances %>%
    arrange(focus_distance) %>%
    slice(1:size) %>%
    select(code,description,focus_distance)

  if (bin_by>1){
    plot_data <- code_counts[[code_type]] %>%
      mutate(days_since_index=(days_since_index %/% bin_by)*bin_by) %>%
      group_by(days_since_index,code) %>%
      summarise(frac=sum(n)/sum(tot_n)) %>%
      inner_join(tmp) %>%
      mutate(description = fct_relevel(description,tmp$description))
  } else {
    plot_data <- code_counts[[code_type]] %>%
      inner_join(tmp) %>%
      mutate(description = fct_relevel(description,tmp$description))
  }


  plot_data %>%
    ggplot(aes(days_since_index,frac)) +
    geom_point() +
    geom_smooth(span=span) +
    facet_wrap(~description, scales = "free_y") +
    theme_bw()
}
