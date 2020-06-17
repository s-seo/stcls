#' KNN spatial measure
#'
#' Calculate d_ij and t_ij. d_ij = 1 when case j is a k nearest neighbor of case i in space, otherwise d_ij = 0. t_ij = 1 when case j is a k nearest neighbor of case i in time, otherwise t_ij = 0.
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param k the number of k nearest neightbors
#' @param loc A logical value. If true, return distance stastistic. Otherwise, return time statistic. (default TRUE)
#' @return Matrix d_ij or t_ij
#' @examples
#' library(surveillance)
#' data("imdepi")
#' imdepiB <- subset(imdepi, type == "B")
#' g = coordinates(imdepiB$events)
#' t = imdepiB$events$time
#' knn_d = knn_measure(g,t,k=5)
#' knn_t = knn_measure(g,t,k=5,loc=FALSE)
#' @export
#' @importFrom stats dist

knn_measure = function(geo, time, k, loc = T){
  loc_mat = as.matrix(stats::dist(geo))
  time_mat = as.matrix(stats::dist(time))

  knn_d = knn_t = matrix(0, nrow=nrow(loc_mat), ncol=ncol(loc_mat))
  for(i in 1:nrow(loc_mat)){
    knn_name = which(loc_mat[i,] %in% sort(loc_mat[i,-i])[1:k])[-1]
    knn_d[i,knn_name] = 1

    knn_name = which(time_mat[i,] %in% sort(time_mat[i,-i])[1:k])[-1]
    knn_t[i,knn_name] = 1
  }
  if(loc == TRUE){
    return(knn_d)
  }else{
    return(knn_t)
  }
}
