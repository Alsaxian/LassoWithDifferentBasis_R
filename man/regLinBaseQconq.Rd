% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builder.R
\name{regLinBaseQconq}
\alias{regLinBaseQconq}
\title{Regression lineaire Utilisant une Base de Fonctions ou Polynomiale}
\usage{
regLinBaseQconq(X, Y, Base)
}
\arguments{
\item{X}{Numérique, le prédicteur univarié.}

\item{Y}{Numérique, la réponse de même longueur que \code{X}.}

\item{base}{Liste dont chaque membre contient une composante de la base, qui est soit un objet \code{Polynomial}, soit une fonction. Pour plus d'infos voir \code{\link{basePolynomial}}.}
}
\value{
Un objet de classe \code{"\link[stat]{lm}"} dont les coeffs sont nommés \code{X1}, \code{X2}...
}
\description{
Cette fonction fait la régression linaire entre un prédicteur univarié et la réponse en utilisant une base de fonction
quelconque, ou polynomiale.
}
\details{
La méthode des moindres carrés est employée. La fonction n'ajoute pas automatiquement une constante au modèle, c'est à
l'utilisateur de penser à l'inclure dans la base, en cas de besoin.
}
\examples{
X1 <- runif(n = 100, min = -1, max = 1)
epsilon <- rnorm(n = 100)
Y1 <- sin(4 * pi * X1) + epsilon
BasePoly4 <- basePolynomial(4, ortho=TRUE)
regLinBaseQconq(X1, Y1, BasePoly4)

}
