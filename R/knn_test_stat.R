#' KNN test statistics
#'
#' Calculate cumulative test statistic and k– specific test statistic of spatiotemporal k Nearest Neighbors Test.
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param K the number of k nearest neightbors
#' @return A vector of test statistics c(D_k, lambda_D_k)
#' @examples
#' geo <- matrix(rnorm(1000 * 2), 1000, 2)
#' time <- rexp(1000)
#' res = knn_test_stat(geo,time,K=5)
#' @export

knn_test_stat = function(geo, time, K){
  knn_d = knn_measure(geo, time, k=K, loc=T)
  knn_t = knn_measure(geo, time, k=K, loc=F)
  D_k = sum((knn_d + knn_t) == 2)

  knn_d = knn_measure(geo, time, k=K-1, loc=T)
  knn_t = knn_measure(geo, time, k=K-1, loc=F)
  D_k1 = sum((knn_d + knn_t) == 2)
  LD_k = D_k - D_k1

  return(c(D_k, D_k1))
}

