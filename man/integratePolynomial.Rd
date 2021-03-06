% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builder.R
\name{integratePolynomial}
\alias{integratePolynomial}
\title{Integration d'une Fonction Polynomiale}
\usage{
integratePolynomial(p, xmin, xmax)
}
\arguments{
\item{p}{Un objet \code{Polynomial}, représentant la fonction à intégrer.}

\item{xmin}{Numérique atomique, borne inférieure de l'intégrale.}

\item{xmax}{Numérique atomique, borne supérieure de l'intégrale.}
}
\value{
Numérique atomique. Résultat de l'intégration.
}
\description{
Intégrer une fonction polynomiale sur un segment.
}
\details{
Calcul se fait par la recherche et l'évaluation de la primitive du polynôme.
}
\examples{
poly1 <- Polynomial(c(1, -2, 3))
integratePolynomial(poly1, 1, 2)
integratePolynomial(poly1, 2, 1)
integratePolynomial(poly1, 1, 1)
poly0 <- Polynomial(c(0,0,0))
integratePolynomial(poly0, 3, 10)

}
