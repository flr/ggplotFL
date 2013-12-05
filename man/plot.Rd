\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,FLQuants,missing-method}
\alias{plot,FLStock,FLPar-method}
\alias{plot,FLStock,missing-method}
\alias{plot,FLStocks,missing-method}
\title{ggplot versions of FLR class plot() methods}
\description{
  New basic plot for some FLR classes are defined in
  ggplotFL.

  New basic plot for some FLR classes are defined in
  ggplotFL.

  New basic plot for some FLR classes are defined in
  ggplotFL.

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
data(ple4)
  rps <- FLPar(Harvest=0.14, Catch=1.29e5, Rec=9.38e5, SSB=1.25e6)
  plot(ple4, rps)
data(ple4)
  plot(FLQuants(SSB=ssb(ple4), rec=rec(ple4)))
data(ple4)
  plot(catch.n(ple4))
}

