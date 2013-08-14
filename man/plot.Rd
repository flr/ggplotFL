\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,FLStock,missing-method}
\alias{plot,FLStocks,missing-method}
\title{ggplot versions of FLR class plot() methods}
\description{
  New basic plot for some FLR classes are defined in
  ggplotFL.

  New basic plot for some FLR classes are defined in
  ggplotFL.
}
\examples{
data(ple4)
  plot(ple4)
data(ple4)
  pls <- FLStocks(runA=ple4, runB=qapply(ple4, function(x) x*1.10))
  plot(pls)
}

