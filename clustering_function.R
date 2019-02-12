# dependencies

library(lazyeval)
library(dplyr)

#' clustering_function.R
#' a simple function to apply kmeans to a dataframe
#'
#' @param df dataframe passed as character name
#' @param filter_var variable to filter for the purposes of determining clusters 
#' @param filter_val value for filter_var to be greater than 
#' @param id_var vector of unique key for dataframe rows
#' @param cluster_vars colnames of df columns to use for creation of kmeans clusters
#' @param num_clusters number of kmeans clusters
#' @param useScale ... kmeans() parameter
#'
#' @return dataframe returned with additional cluster column
#' @export
#'
#' @examples
assign_kmeans <- function(df,filter_var=NA,filter_val=NA,id_var,cluster_vars,num_clusters,useScale=TRUE){
  
  mydf <- get(df)
  # filter by chosen variable
  if(is.na(filter_var)){
    mydf_train <- mydf
  } else {
    filter_foo <- interp(~val>val2,val=as.name(filter_var),val2=filter_val)
    mydf_train <- mydf %>% filter_(.dots=filter_foo)
  }
  
  # create matrix for assignment of kmeans()
  names <- c(id_var,cluster_vars)
  subgroup <- mydf_train %>% select_(.dots=names) %>% as.matrix()
  rownames(subgroup) <- subgroup[,1]
  subgroup <- subgroup[,2:length(names)]
  class(subgroup) <- "numeric"
  set.seed(42)
  if(useScale==TRUE){
    subgroup <- subgroup %>% scale()
  }
  km <- kmeans(subgroup,centers=num_clusters,nstart=20)
  clusters <- as.data.frame(km$cluster,row.names = rownames(subgroup))
  centres <- as.data.frame(km$centers)
  clusters <- rownames_to_column(clusters,id_var)
  names(clusters) <- c(id_var,"Cluster")
  centres <- rownames_to_column(centres,"Cluster")
  names(centres) <- c("Cluster",paste0('Centre_',cluster_vars))
  class(centres$Cluster) <- "numeric"
  t3o <- mydf_train
  
  apply_name <- c(id_var)
  new_foo <- list(interp(~as.character(var),var=as.name(id_var)))
  t3o <- t3o %>% mutate_(.dots=setNames(new_foo,apply_name))
  
  Clustering_Result <- t3o %>% select_(.dots=names) %>% left_join(clusters,by=id_var) %>% left_join(centres,by="Cluster")

  choices <- c(id_var,'Cluster')
  mydf_train <- mydf_train %>% left_join(Clustering_Result %>% select_(.dots=choices),by=id_var) %>%
    arrange(Cluster)

  # apply clusters to remaining data
  if(is.na(filter_var)){
    mydf <- mydf_train
  } else {
    filter_foo <- interp(~val<=val2,val=as.name(filter_var),val2=filter_val)
    mydf_apply <- mydf %>% filter_(.dots=filter_foo)
  
    apply_clusters <- function(x, centers) {
      # compute squared euclidean distance from each sample to each cluster center
      temp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
      max.col(-t(temp))  # find index of min distance
    }

    subgroup <- mydf_apply %>% select_(.dots=names) %>% as.matrix()
    rownames(subgroup) <- subgroup[,1]
    subgroup <- subgroup[,2:length(names)]
    class(subgroup) <- "numeric"
    set.seed(42)
    if(useScale==TRUE){
      subgroup <- subgroup %>% scale()
    }
    new_clusters <- apply_clusters(subgroup, km[["centers"]])
    mydf_apply$Cluster <- new_clusters

    mydf <- bind_rows(mydf_train,mydf_apply)
  }
  selections <- c(id_var,'Cluster')
  mydf <- left_join(get(df),mydf %>% select_(.dots=selections),by=id_var)
  assign(df,mydf,envir = .GlobalEnv)
}
