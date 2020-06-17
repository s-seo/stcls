#' KNN test
#'
#' Spatiotemporal k Nearest Neighbors Test with Monte Carlo permutation test
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param B Number of permutations for Monte Carlo permutation test
#' @param K the number of k nearest neightbors
#' @return A list composed of a B*2 matrix of two test statistics D_k, lambda_D_k, observed test statistics, p-value of cumulative test statistic and p-value of k-specific test statistic
#' @examples
#' geo <- matrix(rnorm(1000 * 2), 1000, 2)
#' time <- rexp(1000)
#' res = knntest(geo,time,B=999,K=5)
#' @export

knn_test = function(geo, time, B, K){
  res = list()
  res[[1]] = matrix(0, ncol=2,nrow=B)
  for(i in 1:B){
    time_rpm = randperm(time, length(time))
    res[[1]][i,] = knn_test_stat(geo, time_rpm, K=K)
  }

  res[[2]] = knn_test_stat(geo, time, K=K)
  res[[3]] = sum(res[[1]][,1] > res[[2]][1])/B
  res[[4]] = sum(res[[1]][,2] > res[[2]][2])/B
  return(res)
}
