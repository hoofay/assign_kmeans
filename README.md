This is a function for the application of kmeans() to a dataframe.

The function outputs the input dataframe with an additional column stating the assign Cluster. The function allows for filtering of the dataframe, such that a subset of the dataframe is used to assign clusters, and the remaining dataframe is then assigned a cluster based on the original calculations. 

  # df = dataframe (e.g. 'mydf')
  # filter_var = variable to filter for the purposes of determining clusters (e.g. 'Customers')
  # filter_val = value for filter_var to be greater than (e.g. 10 for 'Customers'>10)
  # id_var = a unique id column within the dataframe (e.g. 'Customer_Id')
  # cluster_vars = the variables used for clustering (e.g. c('Customer_Value','Number_of_Transactions'))
  # num_clusters = the number of centres argument in kmeans()
  # useScale = TRUE/FALSE, whether or not to scale the matrix prior to deploying kmeans(). 
  
 
