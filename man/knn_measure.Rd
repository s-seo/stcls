% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_measure.R
\name{knn_measure}
\alias{knn_measure}
\title{KNN spatial measure}
\usage{
knn_measure(geo, time, k, loc = T)
}
\arguments{
\item{geo}{A dataframe with two columns containing geographic(coordinates) data.}

\item{time}{A vector containing time data}

\item{k}{the number of k nearest neightbors}

\item{loc}{A logical value. If true, return distance stastistic. Otherwise, return time statistic. (default TRUE)}
}
\value{
Matrix d_ij or t_ij
}
\description{
Calculate d_ij and t_ij. d_ij = 1 when case j is a k nearest neighbor of case i in space, otherwise d_ij = 0. t_ij = 1 when case j is a k nearest neighbor of case i in time, otherwise t_ij = 0.
}
\examples{
geo <- matrix(rnorm(1000 * 2), 1000, 2)
time <- rexp(1000)
knn_d = knn_measure(geo,time,k=5)
knn_t = knn_measure(geo,time,k=5,loc=FALSE)
}
