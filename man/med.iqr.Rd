% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statar.R
\name{med.iqr}
\alias{med.iqr}
\title{Median (IQR)}
\usage{
med.iqr(my_data, sig)
}
\arguments{
\item{my_data}{the data for which you want the median (IQR)}

\item{sig}{the significant digits in the IQR}
}
\description{
This function returns the median (IQR)
}
\examples{
data(mtcars) # load the cars dataset
med.iqr(mtcars$mpg, sig=2)
}
\keyword{IQR}
\keyword{median}
