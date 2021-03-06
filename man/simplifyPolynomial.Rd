% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builder.R
\name{simplifyPolynomial}
\alias{simplifyPolynomial}
\title{Simplifier ou Augmenter le Degre d'un Polynome}
\usage{
simplifyPolynomial(x, degreVoulu = 0)
}
\arguments{
\item{x}{Un objet \code{Polynomial}.}

\item{degreVoulu}{Le degré que le polynôme devra avoir à la fin. Si c'est pour simplifier, on peut le laisser à 0.}
}
\value{
La fonction renvoie un objet de classe \code{Polynomial}.
}
\description{
Simplifier un polynôme pour que son terme dominant soit non nul ou y ajouter des termes nuls plus élevés pour forcer le polynôme d'avoir un certain degré.
}
\details{
Lorsque le paramètre \code{degreVoulu = 0}, la fonction va faire la simplification. Losqu'il est positif, la fonction va d'abord
vérifier si le degré saisi est plus haut que le degré du polynôme. Si c'est le cas, elle va le remplir avec les termes nuls
jusqu'au degré voulu. Sinon, elle va faire la simplification.
}
\examples{
poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
simplifyPolynomial(poly2)
simplifyPolynomial(poly2, degreVoulu = 5)
simplifyPolynomial(poly2, degreVoulu = 10)

## On s'assure également que le polynôme nul est compatible avec la fonction.
poly0 <- Polynomial(c(0, 0, 0))
simplifyPolynomial(poly0)

}
