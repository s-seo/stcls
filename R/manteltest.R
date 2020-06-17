#' Mantel test
#'
#' Mantel test with Monte Carlo permutation test
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param B Number of permutations for Monte Carlo permutation test
#' @return A list of randomized Mantel test statistic, observed Mantel text statistic, p value from Monte Carlo permutation test
#' @examples
#' geo <- matrix(rnorm(1000 * 2), 1000, 2)
#' time <- rexp(1000)
#' res1 = manteltest(geo,time,999)
#' @export
#' @importFrom pracma randperm
#' @importFrom stats dist
#' @importFrom stats sd

manteltest <- function(geo, time, B){
  N = nrow(geo)

  loc_dist = stats::dist(geo)
  loc_scale = (loc_dist - mean(loc_dist)) / stats::sd(loc_dist)

  time_dist = dist(time)
  time_scale = (time_dist - mean(time_dist)) / stats::sd(time_dist)
  T_mantel_obs = sum(loc_scale * time_scale) / (N^2 - N -1)

  T_mantel = c()
  for(i in 1:B){
    time_rpm = pracma::randperm(time, length(time))
    time_dist = stats::dist(time_rpm)
    time_scale = (time_dist - mean(time_dist)) / sd(time_dist)
    T_mantel[i] = sum(loc_scale * time_scale) / (N^2 - N -1)
  }
  pval_mantel = sum(T_mantel > T_mantel_obs)/B
  return(list(permutated_T_mantel = T_mantel, observed_T_mantel = T_mantel_obs,
              p_value = pval_mantel))
}
