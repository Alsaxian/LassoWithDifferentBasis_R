# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Classe Polynomiale
#'
#' Classe S4.
#'
#' @slot coeffs Objet de classe \code{"numeric"}. Il est le vecteur enregistrant les coeffs du polynôme dans l'ordre croissant des termes.
#' @slot racines Objet de classe \code{"numeric"}. Il est le vecteur enregistrant les racines du polynôme. Sans ordre. \code{NA} par défaut.
#'
#' @export
#' @note Accessible par l'utilisateur.
setClass("Polynomial", representation(coeffs = "numeric", racines = "numeric"), prototype(racines = NA_integer_))


setMethod(
  f = "show",
  signature = "Polynomial",
  definition = function(object) {
    cat("Un objet de classe \"Polynomial\" dont les coefficients dans l'ordre croissant sont : \n")
    print(object@coeffs)
    if (is.na(object@racines[1])) cat("Mais ses racines ne sont pas connues. \n") else {
      cat("Et ses racines sont : \n")
      print(object@racines)
    }
  }
)


#' Creer un Nouveau Polynome
#'
#' Création d'un nouveau polynôme soit par ses coefficients, soit par toutes ses racines.
#'
#' Fonction pour générer un polynôme. Si \code{root} est \code{TRUE}, le vecteur \code{num} est considéré comme les racines du polynôme à créer.
#' Au cas contraire, il est les coefficients du polynôme, d'où ceux des terms nuls doivent être representé par un 0 et non pas sautés.
#' Si \code{root == FALSE} et un \code{dominant} sont donnés en même temps, le dernier sera ignoré.
#' Dans ce cas-là le polynôme sera construit uniquement par ses coefficients, i.e. le vecteur \code{num}.
#'
#' @param num un vecteur numérique indiquant soit les coefficents soit les racines du polynôme à créer.
#' @param root logique, vaut \code{TRUE} si \code{nom} doit être les racines et \code{FALSE} si coeffs.
#' @param dominant numérique atomique, indiquant le coeff du plus haut terme. Ne fonctionne que quand \code{root == TRUE}.
#'
#'
#' @return
#' Un objet de classe \code{Polynomial}. Si le polynôme est construit par ses coeffs, le slot \code{racines} sera de valeur \code{NA}.
#'
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' poly1
#' poly3 <- Polynomial(c(1,2), roots = TRUE)
#' poly3
#' poly4 <- Polynomial(c(-1,-1), roots = TRUE, dominant = 5)
#' poly4
#' @export
#'
Polynomial <- function(num, roots = FALSE, dominant = 1) {
  if (roots) {
    coeffs <- sapply(length(num):1, function(x) sum(apply(combn(num, x), 2, prod)))
    new("Polynomial", coeffs = dominant * c(coeffs, 1), racines = sort(num))
  } else
    new("Polynomial", coeffs = num)
}


.is_def <- function(object, class2) methods::is(object, class2)
setGeneric("is", function(object, class2) standardGeneric("is"), useAsDefault = .is_def)
#' Verification de Classe \code{Polynomial}
#'
#' Vérifier si un objet est de classe \code{Polynomial}.
#'
#'
#' @param x Objet à vérifier si c'est de classe \code{Polynomial}.
#'
#' @return la fonction \code{is.Polynomial} ou \code{is(x, "Polynomial")} renvoie un booléen.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' is.Polynomial(poly1)
#' is(poly1, "Polynomial")
#' is.Polynomial(c(1, 2))
#' @export
setMethod(
  f = "is",
  signature = "Polynomial",
  definition = function(object) class(object) == "Polynomial"
)


#' Coercition pour la Classe \code{Polynomial}
#'
#' Convertir un objet en classe \code{Polynomial}.
#'
#' @usage as(x, "Polynomial")
#'
#' @param x objet à convertir. Il doit être un vecteur numérique ou une liste dont la première composante est un vecteur numérique.
#' Dans le cas ou la liste contient plusieurs composantes, les restes seront négligées.
#'
#' @return la fonction \code{as} renvoie un objet de classe \code{Polynomial} dont les coeffs sont représentés par le vecteur \code{num}.
#'
#' @examples as.Polynomial(list(x=c(1, 2)))
#' as.Polynomial(c(1, 2))
#' as(c(1, 2), 'Polynomial')
#' as(list(x=c(1, 2)), 'Polynomial')
#' ## La ligne suivante génère une erreur par la fonction "Polynomial" car le vecteur n'est pas numérique.
#' ## as.Polynomial((x=c('a', 'b')))
#' @export
setAs(
  from = "numeric",
  to = "Polynomial",
  def = function(from) Polynomial(if (is.list(from)) from[[1]] else from)
)
setAs(
  from = "list",
  to = "Polynomial",
  def = function(from) Polynomial(if (is.list(from)) from[[1]] else from)
)

#' Simplifier ou Augmenter le Degre d'un Polynome
#'
#' Simplifier un polynôme pour que son terme dominant soit non nul ou y ajouter des termes nuls plus élevés pour forcer le polynôme d'avoir un certain degré.
#'
#' Lorsque le paramètre \code{degreVoulu = 0}, la fonction va faire la simplification. Losqu'il est positif, la fonction va d'abord
#' vérifier si le degré saisi est plus haut que le degré du polynôme. Si c'est le cas, elle va le remplir avec les termes nuls
#' jusqu'au degré voulu. Sinon, elle va faire la simplification.
#'
#' @param x Un objet \code{Polynomial}.
#' @param degreVoulu Le degré que le polynôme devra avoir à la fin. Si c'est pour simplifier, on peut le laisser à 0.
#'
#' @return La fonction renvoie un objet de classe \code{Polynomial}.
#'
#' @examples poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' simplifyPolynomial(poly2)
#' simplifyPolynomial(poly2, degreVoulu = 5)
#' simplifyPolynomial(poly2, degreVoulu = 10)
#'
#' ## On s'assure également que le polynôme nul est compatible avec la fonction.
#' poly0 <- Polynomial(c(0, 0, 0))
#' simplifyPolynomial(poly0)
#'
#' @export
simplifyPolynomial <- function(x, degreVoulu = 0) {
  longueur <- length(x@coeffs)
  if (longueur - 1 < degreVoulu) {
    x@coeffs <- c(x@coeffs, rep(0, degreVoulu - longueur + 1))
    return (x)
  } else {
    if (longueur == 1) return(x)
    else if (tail(x@coeffs, 1) != 0 || longueur - 1 <= degreVoulu) return(x)
    else {
      x@coeffs <- x@coeffs[-longueur]
      return(simplifyPolynomial(x, degreVoulu))
    }
  }
}


#' Calcul du Degre d'un Polynome
#'
#' Calcul du Degré d'un Polynôme.
#'
#' @param p Un objet de classe \code{Polynomial}.
#'
#' @return Un entier positif. Pour le polynôme nul, elle renvoie -Inf.
#'
#' @export
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' degre(poly1)
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' degre(poly2)
#' poly0 <- Polynomial(c(0, 0, 0))
#' degre(poly0)
degre <- function(p) {
  sim_p <- simplifyPolynomial(p)@coeffs
  return(if ((longueur <- length(sim_p)) == 1 & sim_p[1] == 0) -Inf else longueur - 1)
}


#' Convertir un polynome en une chaine de caracteres ou en une expression.
#'
#' Convertir un objet de classe \code{Polynomial} soit en une chaîne de caractères ou en une expression, selon besoin.
#'
#' Quand \code{expressionStyle} est \code{FALSE}, la fonction renvoie un polynôme au format d'après l'habitude de l'humain,
#' c'est-à-dire sans signe de multiplication. Par contre quand  \code{expressionStyle} est \code{TRUE}, la fonction va retourner
#' une expression au format TEX comme si l'on utilisait la fonction \code{expression()}. La différence c'est que cette fois-ci
#' il y a des signes de multiplications entre les coeffs et les termes.
#'
#' @param x un objet \code{Polynomial}.
#' @param expressionStyle Si on veut un format humain (\code{FALSE}, par défaut) ou d'expression (\code{TRUE}).
#'
#' @return Selon le choix, une chaîne de caractères ou une expression.
#'
#' @examples as.character.Polynomial(Polynomial(c(0, 1)))
#' as.character.Polynomial(Polynomial(c(1, -1)))
#' as.character.Polynomial(Polynomial(c(0, 1, -1, 1, 0, -1, 2, 0)), expressionStyle = TRUE)
#'
#' @export
as.character.Polynomial <- function(x, expressionStyle = FALSE) {
  sim_x <- rev(simplifyPolynomial(x)@coeffs)
  Deg <- length(sim_x) - 1
  if (Deg == 0) return (as.character(sim_x[Deg+1])) else {
    coeffs_caracteres <- sim_x

    ## Ici le code pour insérer des '*' en cas de besoin.
    if (expressionStyle) {
      signeMulti <- rep("*", Deg+1)
      signeMulti[sim_x %in% c(-1, 1)] <- ""
      signeMulti[Deg+1] <- ""
    }

    coeffs_caracteres[-(Deg+1)][coeffs_caracteres[-(Deg+1)] == 1] <- ""
    coeffs_caracteres[-(Deg+1)][coeffs_caracteres[-(Deg+1)] == -1] <- "-"
    coeffs_caracteres[-1][coeffs_caracteres[-1] > 0 | coeffs_caracteres[-1] == ""] <-
      paste("+", coeffs_caracteres[-1][coeffs_caracteres[-1] > 0 | coeffs_caracteres[-1] == ""], sep="")
    return (
      paste(coeffs_caracteres[sim_x != 0],

            ## Ici le code pour insérer des '*' en cas de besoin.
            if (expressionStyle) signeMulti[sim_x != 0],

            c(rep("x^", Deg - 1), "x", "")[sim_x != 0],
            if (Deg == 1) c("", "")[(sim_x != 0)] else c(Deg:2, "", "")[sim_x != 0], sep = "", collapse = "")
    )
  }
}


#' S4 Methode pour convertir un polynome en une chaine de caracteres ou en une expression
#'
#' Méthode S4 de \code{as.character.Polynomial}. Seule différence c'est la forme d'appel.
#' Voir \code{\link{as.character.Polynomial}}.
#'
#' @usage as.character(x, "Polynomial")
#'
#' @export
setMethod(
  f = "as.character",
  signature = "Polynomial",
  definition = as.character.Polynomial
)


#' Imprimer un Polynome
#'
#' Imprimer un polynôme dans la console à l'aide de la fonction générique \code{\link[base]{print}}.
#'
#' Sans guimets si \code{quote} est \code{FALSE} par défaut, avec sinon.
#'
#' @param x Polynôme à imprimer.
#'
#' @param quote Avec ou sans (par défaut) guillemets.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' print(poly1)
#' print(poly1, quote = TRUE)
#'
#' @export
setMethod(
  f = "print",
  signature = "Polynomial",
  definition = function(x) print(as.character.Polynomial(x), quote = FALSE)
)

#' Calcul de la Derivee d'un Polynome
#'
#' Calcul de la dérivée d'un polynôme.
#'
#' @param x Un objet de classe \code{Polynomial}.
#'
#' @return Un objet de classe \code{Polynomial}.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' derivePolynomial(poly1)
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' derivePolynomial(poly2)
#' poly0 <- Polynomial(c(0,0,0))
#' derivePolynomial(poly0)
#'
#' @export
derivePolynomial <- function(x) {
  if ((Deg <- degre(x)) <= 0) return (Polynomial(0)) else {
    deriveCoeffs <- simplifyPolynomial(x)@coeffs[-1] * 1:Deg
    return (Polynomial(deriveCoeffs))
  }
}

#' Calcul de la Primitive d'un Polynome
#'
#' Calcul d'une primitive d'un polynôme.
#'
#' La primitive retournée c'est celle avec la contante nulle.
#'
#' @param x Un objet de classe \code{Polynomial}.
#'
#' @return Un objet de classe \code{Polynomial}.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' primitivePolynomial(poly1)
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' primitivePolynomial(poly2)
#' poly0 <- Polynomial(c(0,0,0))
#' primitivePolynomial(poly0)
#'
#' @export
primitivePolynomial <- function(p) {
  if ((Deg <- degre(p)) == -Inf) return (Polynomial(0)) else {
    primitiveCoeffs <- c(0, simplifyPolynomial(p)@coeffs / 1:(Deg + 1))
    return (Polynomial(primitiveCoeffs))
  }
}



#' Evaluation d'un Polynome
#'
#' Evaluation d'un polynôme en un ou plusieurs points.
#'
#' @param p Un objet \code{Polynomial}.
#'
#' @param x Un vecteur numérique. Les points en lesquels la fonction doit calculer les valeurs du polynôme.
#'
#' @return Un vecteur numériques correspondant aux valeurs cherchées.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' values(p=poly1, x=c(1, 2))
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' values(poly2, c(0, -1, -2))
#' polyà <- Polynomial(c(0,0,0))
#' values(poly0, c(0, 0))
#'
#' @export
values <- function(p, x) {
  sapply(x,
         function(poly, y) {
           sim_p <- simplifyPolynomial(poly)@coeffs
           if ((Deg <- degre(poly)) <= 0) return (sim_p[1]) else
             return ((sim_p)[1] + sum(sim_p[-1] * y^(1:Deg)))
         },
         poly = p
  )
}


#' Integration d'une Fonction Polynomiale
#'
#' Intégrer une fonction polynomiale sur un segment.
#'
#' Calcul se fait par la recherche et l'évaluation de la primitive du polynôme.
#'
#' @param p Un objet \code{Polynomial}, représentant la fonction à intégrer.
#' @param xmin Numérique atomique, borne inférieure de l'intégrale.
#' @param xmax Numérique atomique, borne supérieure de l'intégrale.
#'
#' @return Numérique atomique. Résultat de l'intégration.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' integratePolynomial(poly1, 1, 2)
#' integratePolynomial(poly1, 2, 1)
#' integratePolynomial(poly1, 1, 1)
#' poly0 <- Polynomial(c(0,0,0))
#' integratePolynomial(poly0, 3, 10)
#'
#' @export
integratePolynomial <- function(p, xmin, xmax) diff(values(primitivePolynomial(p), c(xmin, xmax)))


#' Resumer un Objet de Classe \code{Polynomial}
#'
#' Une méthode S4 de la fonction générique summary() afin de faire afficher les coefficients et les racines d'un polynôme,
#' informations qui pourront par la suite être extraites par le signe $.
#'
#' @param object Un objet \code{Polynomial}.
#'
#' @return Une liste à deux éléments, chacun comprend un vecteur représentant resp. les coeffs. et les racines.
#'
#' @examples poly3 <- Polynomial(c(1,2), roots = TRUE)
#' summary(poly3)
#'
#' @export
setMethod(
  f = "summary",
  signature = "Polynomial",
  definition = function(object) {
    return (list(coeffs = object@coeffs, racines = object@racines))
  }
)

#' Tracer un Polynome
#'
#' Une méthode S4 de la fonction générique \code{plot} pour tracer un polynôme.
#'
#' La méthode employée au fond c'est \code{plot.function()} et NON PAS \code{plot.xy()}.
#'
#' @param x Un objet de classe \code{Polynomial}.
#' @param xlim L'amplitude de l'axe de X.
#' @param main Titre principal.
#' @param xlab sous-titre pour l'axe de X. Par défaut, c'est l'expression du polynôme.
#' @param ylab sous-titre pour l'axe de Y.
#' @param col couleur du polynôme tracé.
#' @param lwd l'épaisseur de la ligne.
#' @param ... tout autre paramètre pouvant être acceptée par la fonction \code{print.function()}.
#'
#' @examples poly1 <- Polynomial(c(1,-2,3))
#' plot.Polynomial(poly1)
#' poly0 <- Polynomial(c(0,0,0))
#' plot(poly0, col='red', lwd=1.5)
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' plot(poly2, col="darkorange", lwd=3, xlim=c(-10, 10))
#'
#' @export
setMethod(
  f = "plot",
  signature = "Polynomial",
  definition = plot.Polynomial <- function(x, xlim=c(-1,1), main = "Trace polyn\U00F4miale",
                                           xlab = parse(text = as.character.Polynomial(x, expressionStyle = TRUE)),
                                           ylab = "valeur", col="darkblue", lwd=2, ...) {
    courbe <- function(y) values(x, y)
    plot(courbe, xlim = xlim, main = main, xlab = xlab, ylab = ylab, col=col, lwd=lwd, ...)
  }
)


#' Operations Arithmetiques entre Polynomes ou Polynome et Nombre
#'
#' Cette méthode S4 rendant possible les opérations arithmétiques entre polynômes ou polynôme et nombre, est une méthode
#' du groupe générique \code{\link[methods]{Arith}}.
#'
#' Les opérations suivantes sont implémentées :
#'
#'     \code{`+`}, \code{sum()} : addition polynomiale ou opératon unaire
#'
#'     \code{`-`} : soustraction polynomiale ou calcul de l'opposé d'un polynôme
#'
#'     \code{`*`} : multiplication de deux polynômes, un polynôme par un nombre ou encore un nombre par un polynôme
#'
#'     \code{`\%\%`} et \code{`\%\\\%`} : division euclidienne entre polynômes (diviseur non nul), le premier étant le quotient et le dernier le résidu
#'
#'     \code{`/`} : division d'un polynôme par un nombre non nul.
#'
#' @seealso Voir \code{\link[methods]{Arith}} pour plus d'informations.
#'
#' @examples poly1 <- Polynomial(c(1, -2, 3))
#' poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
#' poly0 <- Polynomial(c(0,0,0))
#'
#' ## Addition
#' poly1 + poly2
#' + poly2
#' + poly0
#' sum(list(poly1, poly1, poly1, poly1))
#'
#' ## Soustraction
#' poly1 - poly2
#' - poly2
#' - poly0
#' poly0 - poly0
#' poly0 + poly0
#' poly0 * poly1
#'
#' ## Multipliacation
#' poly1 * poly2
#' poly0 * poly0
#' poly0 * poly1
#' poly2 * 2
#' 2 * poly2
#'
#' ## Calcul du quotient
#' Polynomial(c(1, -1, 1), roots = TRUE) %% Polynomial(c(1, 1), roots = TRUE)
#' Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %% Polynomial(c(3, -5, -6, -8))
#' ## Cette commande renvoyera une erreur
#' #poly1 %% poly0
#'
#' ## Calcul du residu
#' Polynomial(c(1, 3, -1, 1), roots = TRUE) %/% Polynomial(c(5, 10, 5))
#' Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %/% Polynomial(c(3, -5, -6, -8))
#' ## Cette commande renvoyera une erreur
#' #poly1 %% poly0
#'
#' ## Division par un nombre
#' poly2 / 2
setMethod(
  f = "Arith",
  signature (e1 = "Polynomial", e2 = "Polynomial"),
  definition = function(e1, e2) {

    ## addition ##
    ## Rappelez-vous du paramètre "d'en trop" "degreVoulu" dans la fonction simplifyPolynomial(), dont le but est de
    ## compléter un polynôme avec des 0 à un degré donné plus élevé que celui du polynôme. Maintenant on va s'en servir
    ## pour éviter le mécanisme du recyclage des opérations vectorielles de R.
    if (as.character(match.call()[[1]]) == "+") {
      maxDeg <- max(degre(e1), degre(e2))
      e1 <- simplifyPolynomial(e1, degreVoulu = maxDeg)
      e2 <- simplifyPolynomial(e2, degreVoulu = maxDeg)
      resultat_coeffs <- callGeneric(e1@coeffs, e2@coeffs)
      return (simplifyPolynomial(Polynomial(resultat_coeffs)))
    } else

      ## Soustraction ##
      ## Exactement le même code sauf "-" au lieu de "+".
      if (as.character(match.call()[[1]]) == "-") {
        maxDeg <- max(degre(e1), degre(e2))
        e1 <- simplifyPolynomial(e1, degreVoulu = maxDeg)
        e2 <- simplifyPolynomial(e2, degreVoulu = maxDeg)
        resultat_coeffs <- callGeneric(e1@coeffs, e2@coeffs)
        return (simplifyPolynomial(Polynomial(resultat_coeffs)))
      } else

        ## Multiplication ##
        ## Il y a des algorithmes efficaces dédiés au calcul de la multiplication polynomiale en informatique,
        ## e.g. celui de "diviser pour régner". Ici on ne va pas le faire, puisque
        ## 1. R n'est de toute façon pas un logiciel très efficace, d'où de couper un polynôme en morceaux va prendre du temps
        ## 2. On ne va pas faire de la multiplication avec des polynômes gigantesques, le temps gagné sera pas remarquable.
        ## Donc ici, on le fait avec des boules.
        if (as.character(match.call()[[1]]) == "*") {
          Deg1 <- degre(e1)
          Deg2 <- degre(e2)
          if (Deg1 == -Inf || Deg2 == -Inf) return (Polynomial(0)) else {
            DegResultat <- Deg1 + Deg2
            coeffs1 <- simplifyPolynomial(e1)@coeffs
            coeffs2 <- simplifyPolynomial(e2)@coeffs
            resultat_coeffs <- numeric(DegResultat + 1)
            for (i in 1:(Deg1+1))
              for (j in 1:(Deg2+1))
                resultat_coeffs[i+j-1] <- resultat_coeffs[i+j-1] + coeffs1[i] * coeffs2[j]
            return (Polynomial(resultat_coeffs))
          }
        } else

          ## Résidu ##
          if (as.character(match.call()[[1]]) == "%/%")  {
            if (degre(e2) == -Inf) stop("Diviseur ne doit pas être nul !") else
              if (degre(e2) == 0) return (Polynomial(0)) else {
                coeffs1 <- simplifyPolynomial(e1)@coeffs
                coeffs2 <- simplifyPolynomial(e2)@coeffs
                if (length(coeffs1) < length(coeffs2) || length(coeffs1) == 0) {
                  e1@racines <- NA_integer_
                  return (simplifyPolynomial(e1))
                } else {
                  e1@coeffs <- coeffs1 - c(rep(0, length(coeffs1) - length(coeffs2)),
                                           tail(coeffs1, 1) / tail(coeffs2, 1) * coeffs2)
                  return (simplifyPolynomial(e1) %/% e2)
                }
              }
          } else

            ## Quotient ##
            if (as.character(match.call()[[1]]) == "%%") {
              if (degre(e2) == -Inf) stop("Diviseur ne doit pas être nul !") else {
                coeffs1 <- simplifyPolynomial(e1)@coeffs
                coeffs2 <- simplifyPolynomial(e2)@coeffs
                if (length(coeffs1) < length(coeffs2) || (length(coeffs1) == 1 && coeffs1[1] == 0)) return (Polynomial(0)) else {
                  Deg_diff <- length(coeffs1) - length(coeffs2)
                  Quotient_coeff_dominant <- tail(coeffs1, 1) / tail(coeffs2, 1)
                  e1@coeffs <- coeffs1 - c(rep(0, Deg_diff), Quotient_coeff_dominant * coeffs2)
                  return (Polynomial(c(rep(0, Deg_diff), Quotient_coeff_dominant)) + (simplifyPolynomial(e1) %% e2))
                }
              }
            } else

              ## Autre opération ##
              stop("Cette opération est non définie entre les polynômes !")
  }
)
setMethod(
  f = "Arith",
  signature (e1 = "Polynomial", e2 = "numeric"),
  definition = function(e1, e2) {

    ## Division par un monbre ##
    if (as.character(match.call()[[1]]) == "/") {
      if (e2 == 0) stop("Diviseur ne doit pas être nul !") else {
        e1@coeffs <- e1@coeffs / e2
        return (simplifyPolynomial(e1))
      }
    } else

      ## Multiplication par un monbre ##
      if (as.character(match.call()[[1]]) == "*") {
        e1@coeffs <- e1@coeffs * e2
        return (simplifyPolynomial(e1))
      } else

        ## Autre opération ##
        stop("Cette opération est non définie entre les polynômes !")
  }
)
setMethod(
  f = "Arith",
  signature (e1 = "numeric", e2 = "Polynomial"),
  definition = function(e1, e2) {

    ## Multiplication par un nombre ##
    if (as.character(match.call()[[1]]) == "*") {
      e2@coeffs <- e1 * e2@coeffs
      return (simplifyPolynomial(e2))
    } else

      ## Autre opération ##
      stop("Cette opération est non définie entre les polynômes !")
  }
)
.sum_def <- function(x, ..., na.rm = FALSE) base::sum(x, ..., na.rm = na.rm)
setGeneric("sum", function(x, ..., na.rm = FALSE) standardGeneric("sum"), useAsDefault = .sum_def, group="Summary")

setMethod(
  f = "sum",
  signature (x = "list", na.rm = "ANY"),
  definition = function(x, ..., na.rm) {
    if (length(x) == 0) return (Polynomial(0)) else
      if (length(x) == 1) return (simplifyPolynomial(x[[1]])) else
        return (simplifyPolynomial(x[[1]] + sum(x[-1])))
  }
)

#' Creation d'une Base Polynomiale Ortho-normee ou Non
#'
#' Créer une base polynomiale sur [-1, 1] d'une dimension quelconque. Ortho-normée ou non.
#'
#' Si \code{ortho} est \code{FALSE}, \code{1}, \code{x}, \code{x^2}... seront renoyés. Si \code{ortho} est \code{TRUE},
#' la fonction renvoie une base ortho-normée de dimension \code{d} en utilisant l'algorithme de Gram-Schmidt.
#'
#' @param d Nombre entier positif, précisant la dimension de la base.
#' @param ortho Logique, \code{FALSE} par défaut. Si on veut que la base polnomiale obtenue soit ortho-normée.
#'
#' @return Une liste dont chaque membre contient une composante de la base, qui est un objet \code{Polynomial}.
#'
#' @examples basePolynomial(3)
#' BasePoly4 <- basePolynomial(4, ortho=TRUE)
#' ## Est-cette base vraiment orthonormée ?
#' BasePoly4
#' sapply(BasePoly4, function(x) integratePolynomial(x*x, -1, 1)) # doit renvoyer : [1] 1 1 1 1
#' apply(combn(BasePoly4, 2), 2, function(x) integratePolynomial(x[[1]]*x[[2]], -1, 1)) # doit renvoyer : [1] 0 0 0 0 0 0
#'
#' @export
basePolynomial <- function(d, ortho=FALSE) {
  nouvelleComposante <- Polynomial(c(rep(0, d-1), 1))
  if (d <= 1) return (list(nouvelleComposante / if (!ortho) 1 else sqrt(2))) else {
    basePrecedente <- basePolynomial(d-1, ortho)
    if (ortho) {
      diminuteur <- sum(lapply(basePrecedente, function(p, p1) integratePolynomial(p * p1, -1, 1) * p,
                               p1 = nouvelleComposante))
      nouvelleComposante <- nouvelleComposante - diminuteur
      nouvelleComposante <- nouvelleComposante / sqrt (integratePolynomial(nouvelleComposante * nouvelleComposante, -1, 1))
    }
    return (c(basePrecedente, nouvelleComposante))
  }
}

#' Regression lineaire Utilisant une Base de Fonctions ou Polynomiale
#'
#' Cette fonction fait la régression linaire entre un prédicteur univarié et la réponse en utilisant une base de fonction
#' quelconque, ou polynomiale.
#'
#' La méthode des moindres carrés est employée. La fonction n'ajoute pas automatiquement une constante au modèle, c'est à
#' l'utilisateur de penser à l'inclure dans la base, en cas de besoin.
#'
#' @param X Numérique, le prédicteur univarié.
#'
#' @param Y Numérique, la réponse de même longueur que \code{X}.
#'
#' @param base Liste dont chaque membre contient une composante de la base, qui est soit un objet \code{Polynomial}, soit une fonction. Pour plus d'infos voir \code{\link{basePolynomial}}.
#'
#' @return Un objet de classe \code{"\link[stat]{lm}"} dont les coeffs sont nommés \code{X1}, \code{X2}...
#'
#' @examples X1 <- runif(n = 100, min = -1, max = 1)
#' epsilon <- rnorm(n = 100)
#' Y1 <- sin(4 * pi * X1) + epsilon
#' BasePoly4 <- basePolynomial(4, ortho=TRUE)
#' regLinBaseQconq(X1, Y1, BasePoly4)
#'
#' @export
regLinBaseQconq <- function(X, Y, Base) {
  if (!is.numeric(X) | !is.numeric(Y) | length(X) != length(Y)) stop("X et Y doivent être deux vecteurs numériques
    de même longueur !") else
      if (!is.list(Base)) stop("Base doit être soit une liste de polynômes soit une liste de functions !") else
        if (is.Polynomial(Base[[1]])) {
          Base <- lapply(Base, function(composante) return (function(x) values(composante, x)))
        }

  if (is.function(Base[[1]])) {
    lmFit <- lm(as.formula(paste("Y ~ 0", (paste("Base[[", 1:length(Base), "]](X)", sep="", collapse="+")), sep="+")))
  } else stop("La base entrée n'est ni de fonctions ni de polynômes !")
  ## Ici, je suis obsessif de fabriquer une formule plutôt que de créer une matrice avec une série de variables, pour
  ## que mon modèle à la fin reste un modèle entre X et Y. J'ai tout intérêt de faire ça comme ça car plus tard, quand
  ## je prédis pour n'importe quelle valeur de X entre [-1, 1], je pourrai prendre ce modèle calculé et lui donner
  ## directement la valeur du nouveau X. Sinon, je serais obligé de recalculer à chaque fois la matrice,
  ## si ce nouveau X n'appartient pas à l'ensemble des valeurs de X utilisées pour ajuster le modèle.

  names(lmFit$coefficients) <- paste("f_", 1:length(Base), "(X)", sep = "")
  return(lmFit)
}



