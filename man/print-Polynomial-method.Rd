% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builder.R
\docType{methods}
\name{print,Polynomial-method}
\alias{print,Polynomial-method}
\title{Imprimer un Polynome}
\usage{
\S4method{print}{Polynomial}(x)
}
\arguments{
\item{x}{Polynôme à imprimer.}

\item{quote}{Avec ou sans (par défaut) guillemets.}
}
\description{
Imprimer un polynôme dans la console à l'aide de la fonction générique \code{\link[base]{print}}.
}
\details{
Sans guimets si \code{quote} est \code{FALSE} par défaut, avec sinon.
}
\examples{
poly1 <- Polynomial(c(1, -2, 3))
print(poly1)
print(poly1, quote = TRUE)

}
