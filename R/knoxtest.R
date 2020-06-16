#' Knox test
#'
#' Knox test with Monte Carlo method
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param delta A numeric value of critical space distances values
#' @param gamma A numeric value of critical time distances values
#' @param delta_ratio Optional, select delta as (1-a)th percentile of space distances where a is delta_ratio
#' @param gamma_ratio Optional, select delta as (1-b)th percentile of time distances where b is gamma_ratio
#' @param B Number of permutations for Monte Carlo permutation test
#' @return A list of randomized knox test statistic, observed knox text statistic, p value from Monte Carlo permutation test
#' @examples
#' library(surveillance)
#' data("imdepi")
#' imdepiB <- subset(imdepi, type == "B")
#' g = coordinates(imdepiB$events)
#' t = imdepiB$events$time
#' res1 = knoxtest(g,t,delta=50,gamma=30,B=999)
#' res1 = knoxtest(g,t,delta_ratio=0.1,gamma_ratio=0.02,B=999)
#' @export

knoxtest = function(geo, time, delta = NULL, gamma = NULL,
                    delta_ratio, gamma_ratio, B){

  loc_dist = dist(geo)
  if(is.null(delta)){
    delta = sort(loc_dist)[round(length(loc_dist)*delta_ratio)]
  }

  time_dist = dist(time)
  if(is.null(gamma)){
    gamma = sort(time_dist)[round(length(time_dist)*gamma_ratio)]
  }

  T_knox_obs = sum((loc_dist < delta) * (time_dist < gamma))/2

  T_know = c()
  for(i in 1:B){
    time_rpm = pracma::randperm(time, length(time))
    time_dist = dist(time_rpm)
    T_knox[i] = sum((loc_dist < delta) * (time_dist < gamma))/2
  }

  pval_knox = sum(T_knox > T_knox_obs)/B
  return(list(permutated_T_knox = T_knox, observed_T_knox = T_knox_obs, p_value = pval_knox))
}