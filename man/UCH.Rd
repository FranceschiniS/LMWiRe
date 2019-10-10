\name{UCH}
\alias{UCH}
\docType{data}
\title{ UCH Data : Urine Citrate-Hippurate database}

\description{This dataset comes from a H-NMR analysis of urine. There are 34 spectra with different experimental designs. Each spectrum is composed of 600 response variables. The aim is to explain the spectra with different variables: the concentration of Hippurate (Hippurate), the concentration of Citrate (Citrate) and the day of the analysis (Time).}
\usage{data("UCH")}
\format{
A list of 3 : the experimental design, the responses for every observations and the formula considered to analyze the data.
  \describe{
    \item{\code{responses}}{  A dataset with 34 observations and 600 response variables}
    \item{\code{formula}}{ The suggested formula to analyze the data}
    \item{\code{design}}{  The experimental design of 34 observations and 5 explanatory variables}
  }
}
\details{
\code{responses = Hippurate + Citrate + Time +
            H*C + T*H + T*C +
            H*C*T}
}
\source{
Rousseau, R. (2011). \emph{Statistical contribution to the analysis of metabonomics data in 1H NMR spectroscopy}
  (Doctoral dissertation, PhD thesis. Institut de statistique, biostatistique et sciences actuarielles, Université catholique de Louvain, Belgium).

Guisset, S.,Martin, M.,Govaerts, B. (2019). \emph{Comparison of PARAFASCA, AComDim and AMOPLS approaches in the multivariate GLM modelling of multi-factorial designs}, Chemometrics and Intelligent Laboratory Systems

}

\examples{
data("UCH")
LinePlot(UCH$outcomes[1:2,],main="H-NMR spectrum")

}
\keyword{datasets}
