#' Comparison between knox, mantel, knn test
#'
#' Compare the hypothesis testing-based clustering methods of the events spatio-temporal data type by histograms with p-value of Monte Carlo permutation test
#'
#' @param geo A dataframe with two columns containing geographic(coordinates) data.
#' @param time A vector containing time data
#' @param delta A numeric value of critical space distances values
#' @param gamma A numeric value of critical time distances values
#' @param delta_ratio Optional, select delta as (1-a)th percentile of space distances where a is delta_ratio
#' @param gamma_ratio Optional, select delta as (1-b)th percentile of time distances where b is gamma_ratio
#' @param B Number of permutations for Monte Carlo permutation test
#' @param K the number of k nearest neightbors
#' @return 4 histograms of know test, mantel test, knn test with cumulative test statistic, knn test with k-specific test statistic with p-value of Monte Carlo permutation test respectively
#' @examples
#' geo <- matrix(rnorm(1000 * 2), 1000, 2)
#' time <- rexp(1000)
#' stcls_hyp_plot(geo, time, delta=50, gamma=30, B=10,K=5)
#' stcls_hyp_plot(geo, time, delta_ratio = 0.1, gamma_ratio = 0.02, B=10,K=5)
#' @export
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_vline
#' @importFrom gridExtra grid.arrange

stcls_hyp_plot <- function(geo, time, delta = NULL, gamma = NULL, delta_ratio, gamma_ratio, B, K){

  res_knox = knoxtest(geo, time, delta=delta, gamma=gamma,
                      delta_ratio = delta_ratio, gamma_ratio = gamma_ratio, B = B)
  res_mantel = manteltest(geo, time, B)
  res_knn = knn_test(geo,time,B = B,K = K)

  g1 = data.frame(X = res_knox[[1]]) %>%
    ggplot(., aes(x=X)) +
    geom_histogram(binwidth = diff(range(res_knox[[1]]))/15, fill="lightblue", colour="black")+
    theme_minimal()+
    ggtitle('knox test')+
    annotate('text', x=-Inf, y=Inf, label=paste0('p-value : ',round(res_knox[[3]],3)),
             hjust=-.2, vjust=2)+
    geom_vline(xintercept = res_knox[[2]], size =2, col='red')+
    annotate('text', x=res_knox[[2]], y=0, label=res_knox[[2]], hjust=-.5, vjust=1.2)

  g2 = data.frame(X = res_mantel[[1]]) %>%
    ggplot(., aes(x=X)) +
    geom_histogram(binwidth = diff(range(res_mantel[[1]]))/15, fill="lightblue", colour="black")+
    theme_minimal()+
    ggtitle('Mantel test')+
    annotate('text', x=-Inf, y=Inf, label=paste0('p-value : ',round(res_mantel[[3]],3)),
             hjust=-.2, vjust=2)+
    geom_vline(xintercept = res_mantel[[2]], size =2, col='red')+
    annotate('text', x=res_mantel[[2]], y=0, label=round(res_mantel[[2]],3), hjust=-.5, vjust=1.2)

  g3 = data.frame(X = res_knn[[1]][,1]) %>%
    ggplot(., aes(x=X)) +
    geom_histogram(binwidth = diff(range(res_knn[[1]][,1]))/15, fill="lightblue", colour="black")+
    theme_minimal()+
    ggtitle('KNN test with cumulative test statistic')+
    annotate('text', x=-Inf, y=Inf, label=paste0('p-value : ',round(res_knn[[3]],3)),
             hjust=-.2, vjust=2)+
    geom_vline(xintercept = res_knn[[2]][1], size =2, col='red')+
    annotate('text', x=res_knn[[2]][1], y=0, label=res_knn[[2]][1], hjust=-.5, vjust=1.2)

  g4 = data.frame(X = res_knn[[1]][,2]) %>%
    ggplot(., aes(x=X)) +
    geom_histogram(binwidth = diff(range(res_knn[[1]][,2]))/15, fill="lightblue", colour="black")+
    theme_minimal()+
    ggtitle('KNN test with k– specific test statistic')+
    annotate('text', x=-Inf, y=Inf, label=paste0('p-value : ',round(res_knn[[4]],3)),
             hjust=-.2, vjust=2)+
    geom_vline(xintercept = res_knn[[2]][2], size =2, col='red')+
    annotate('text', x=res_knn[[2]][2], y=0, label=res_knn[[2]][2], hjust=-.5, vjust=1.2)

  grid.arrange(g1,g2,g3,g4)
}
