#' KNN test
#'
#' Spatiotemporal k Nearest Neighbors Test with Monte Carlo permutation test
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param B Number of permutations for Monte Carlo permutation test
#' @param K the number of k nearest neightbors
#' @return A B*2 dataframe composed of two test statistics D_k, lambda_D_k.
#' @examples
#' library(surveillance)
#' data("imdepi")
#' imdepiB <- subset(imdepi, type == "B")
#' g = coordinates(imdepiB$events)
#' t = imdepiB$events$time
#' res = knntest(g,t,B=999,K=5)
#' @export

knn_test = function(geo, time, B, K){
  res = matrix(0, ncol=2,nrow=B)
  for(i in 1:B){
    time_rpm = randperm(time, length(time))
    res[i,] = knn_test_stat(geo, time_rpm, K=K)
  }
  return(res)
}
