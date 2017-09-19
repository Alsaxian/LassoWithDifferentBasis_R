#' ---
#' title: Projet R 2017
#' author: Xian YANG <xian.yang@etu.unistra.fr>
#' date: 3 mai, 2017
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---


#' ## Avant propos
#' Ce document est écrit de façon qu'un rapport du style *Rmarkdown* peut être généré directement du scripte R, lorsqu'il
#' peut également être sourcé comme un document R normal. Pour le sourcer, il est important que votre système soit en
#' français, à cause des carartères spéciaux (bien que les lettres spéciales se trouvent seulement dans les comentaires
#' et que le codage soit mis en UTF-8, R ne les acceptent bizzarement pas lors de la source. Par contre runner
#' toutes les commandes même en une fois ne posera pas de problème). Si ce n'est pas le cas, veuillez exécuter la commande
#' suivante
#+ eval=FALSE
Sys.setlocale("LC_CTYPE", "fr_FR")
#'
#' L'encodage est mis en UTF-8 pour permettre l'affichage des lettres accentuées. Rouvrez le fichier avec le bon
#' codage si besoin.
#'
#' ## Une note importante
#' J'ai prévilégié dans l'ensemble du scripte les classes et méthodes S4 que celles S3, c'est notamment à cause du choix
#' personnel, bien que S4 dépasse S3 sous certains aspects (poo, vérification de classe, droit d'accès etc.) et devienne
#' plus compliqué sous certains autres. Cela étant dit, pour faciliter la lecture, j'ai toujours nommé les fonctions
#' d'enjeu au sein des méthodes S4 suivant la nomencloture S3 dès que possible, de telle sorte que une méthode S3 soit
#' également créée qui peut être appelée comme une fonction ordinaire sans forcément passant par la fonction générique.
#' Example :
#+ eval=FALSE
setMethod(
   f = "is",
   signature = "Polynomial",
   definition = is.Polynomial <- function(object) class(object) == "Polynomial"
)
#' `is.Polynomial <-` est ici redondant. Normalement, S4 ne soutient que d'appeler une méthode à travers sa fonction
#' générique `is(x, "Polynomial")` et non pas genre `is.Polynomial(x)`. Il convient de remarquer que
#'
#' * ça entraîne une redondance (deux méthodes pour faire la même chose) et que
#'
#' * ce n'est pas la bonne façon de travailler avec S4. Je fais comme ça seulement pour simplifier la vérification
#' du résultat.
#'
#'
#' ## Hightlights du scripte pour l'impatient
#'
#' * *Emploi de classes et méthodes S4.*
#'
#' * *Test complet de chaque fonction avec tout genre d'examples : polynôme 0, polynômes de même degré mais de coeffs
#' dominants de signes opposés qui s'annulent, etc.*
#'
#' * *Construction d'un polynôme par ses racines sans multiplier mononôme par mononôme.*
#'
#' * *Utilisation du méchanisme de trois points dans la définition de fonction.*
#'
#' * *Redéfinition des opérations arithmétiques en passant par leur groupe "Arith".*
#'
#' * *Utilisation des paquets "glmnet" et "plotmo" pour Lasso.*
#'
#' * *Création des fiches aides à l'aide du paquet "roxygen2".*
#'
#' ## Partie Base polynomiale
#' Comme le chargement du paquet `polynom` n'est pas souhaité d'après l'esprit de l'ensemble des exercices,
#' la classe `Polynomial` n'existe pas encore. Il convient d'abord de la créer. Ici, le nom de
#' la classe est capitalisé afin de ne pas le confondre avec celui défini dans le paquet `polynom`.
#+ results='markup'
setClass("Polynomial", representation(coeffs = "numeric"))
#'
#' On définit ici également le massage retourné par R à chqaue fois que le contenu d'un objet "Polynomial"
#' est demandé.
#+ results='markup'
setMethod(
  f = "show",
  signature = "Polynomial",
  definition = function(object) {
    cat("Un objet de classe \"Polynomial\" dont les coefficients dans l'ordre croissant sont : \n")
    print(object@coeffs)
  }
)
#'
#'
#' ### 1. Création de la fonction "Polynomial" qui crée un objet "Polynomial".
#+ results='markup'
Polynomial <- function(coeffs, roots = FALSE, dominant = 1) {
  if (roots) stop("La construction d'un polynôme à partir de ses racines n'est pas encore implémentée. ",
                  call. = FALSE) else
                    new("Polynomial", coeffs=coeffs)
}
#'
#' Essayons avec 1+2x+3x^2, puis `roots = TRUE`
poly1 <- Polynomial(c(1, -2, 3))
poly1
#' Ceci renvoyera un message d'erreur
#'
#' `Polynomial(c(1, 2), roots=TRUE)`
#'
#'
#' ### 2. Création de la méthode `is.Polynomial` vérifiant si l'objet est de classe `Polynomial`.
#'
#' Ici, une méthode pouvant être appelée par la fonction générique `is` est plus pertinente qu'une
#' fonction indépendante. Elle est créée en S3 et en S4.
#+ results='markup'
setMethod(
  f = "is",
  signature = "Polynomial",
  definition = is.Polynomial <- function(object) class(object) == "Polynomial"
)
#'
#' Essayons d'abord avec la méthode elle-même puis avec sa fonction générique "is".
is.Polynomial(poly1)
is(poly1, "Polynomial")
is.Polynomial(c(1, 2))
#'
#'
#' ### 3. Création de la méthode `as(x, "Polynomial")` convertissant un verteur numérique ou une liste dont le premier élément est un vecteur numérique en un objet `Polynomial`.
#'
#' *Attention*, car la fonction générique "as" est très particulière (les méthodes S4 se distinguent normalement
#' par leur entrée mais ici, l'enjeu c'est la valeur retournée par chaque méthode de la fonction générique "as"),
#' c'est compliqué de procéder avec la fonction setMethode. En revanche, il existe pour cela la fonction S4 "setAs",
#' qui permet d'appliquer la coercition sans peine.
#'
#' On ne vérifie pas si le vecteur ou le premier élément dé-listé est numérique. On laisse ce travail à la
#' fonction "Polynomial" qui renverra à son tour un massage d'erreur en cas d'incompatibilité.
#+ results='markup'
setAs(
  from = "numeric", # pour un vecteur numérique
  to = "Polynomial",
  def = as.Polynomial <- function(from) Polynomial(if (is.list(from)) from[[1]] else from)
)
#+ results='markup'
setAs(
  from = "list", # pour une liste
  to = "Polynomial",
  def = as.Polynomial <- function(from) Polynomial(if (is.list(from)) from[[1]] else from)
)
#'
#' On voit bien que grâce à la façon de construction, la méthode existe tant en S3 qu'en S4
as.Polynomial(list(x=c(1, 2)))
as.Polynomial(c(1, 2))
as(c(1, 2), 'Polynomial')
as(list(x=c(1, 2)), 'Polynomial')
#'
## La ligne suivante génère une erreur par la fonction "Polynomial" car le vecteur n'est pas numérique.
## as.Polynomial((x=c('a', 'b')))
#'
#'
#' ### 4. Création de la fonction `simplifyPolynomial` qui rend dominant le premier terme de coefficient non nul.
#'
#' Le paramètre degreVoulu sert à répérer le degré du polynôme renvoyé. Si On veut simplement le polynôme
#' simplyfié, pas besoin de préciser ce paramètre (0 par défaut). Par contre si le degré voulu est plus élevé que
#' celui du polynôme simplyfié, la fonction va le compléter avec des 0 jusqu'à ce degré-là. Cette conception verra
#' son utilité dans la question 11 (opérations arithmétiques).
#+ results='markup'
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
#'
#' Quelques examples
poly1
simplifyPolynomial(poly1)
poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
simplifyPolynomial(poly2)
simplifyPolynomial(poly2, degreVoulu = 5)
simplifyPolynomial(poly2, degreVoulu = 10)
#'
#' On s'assure également que le polynôme nul est compatible avec la fonction
poly0 <- Polynomial(c(0, 0, 0))
simplifyPolynomial(poly0)
#'
#'
#' ### 5. Création de la méthode `as.character.Polynomial` qui convertit un polynôme en une chaîne de caractères.
#'
#' D'abord une fonction `degre` qui renvoie le degré d'un objet `Polynomial`
#+ results='markup'
degre <- function(p) {
  sim_p <- simplifyPolynomial(p)@coeffs
  return(if ((longueur <- length(sim_p)) == 1 & sim_p[1] == 0) -Inf else longueur - 1)
}
#'
#' Essayons avec
poly1
degre(poly1)
poly2
degre(poly2)
poly0
degre(poly0)
#'
#' Maintenant création de la fonction `as.character.Polynomial`.
#' Les paramètres contiennt un booléen pour préciser si on veut des signes de multiplication entre les coefficients
#' et les `x` au résultat. S'il est faux, le format humain sera renvoyé et s'il est vrai, le format `expression`
#' sera renvoyé ce qui sert plus tard à mettre un libellé dans un plot plus facilement en pouvant faire appel à la
## fonction `expression`.
#+ results='markup'
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
#'
#' Maintenant on emballe la fonction dans une méthode
#+ results='markup'
setMethod(
  f = "as.character",
  signature = "Polynomial",
  definition = as.character.Polynomial
)
#'
#' Essayons avec quelques examples.
#' A noter que les 1 et les -1 apparaissent comme il faut selon s'ils sont termes constants ou non.
as.character.Polynomial(poly1)
as.character.Polynomial(poly2)
as.character.Polynomial(Polynomial(c(0, 1, -1, 1, 0, -1, 2, 0)), expressionStyle = TRUE)
as.character.Polynomial(poly0)
as.character.Polynomial(Polynomial(c(0, 1)))
as.character.Polynomial(Polynomial(c(1, -1)))
as.character.Polynomial(Polynomial(-1))
#'
#'
#' ### 6. Création d'une méthode de la fonction générique `print` pour afficher un polynôme à la console.
#'
#+ results='markup'
setMethod(
  f = "print",
  signature = "Polynomial",
  definition = print.Polynomial <- function(x) print(as.character.Polynomial(x), quote = FALSE)
)
#'
#' Essayons avec
print(poly1)
#'
#' ### 7. Création d'une fonction calculant la dérivée d'un polynôme.
#'
#+ results='markup'
derivePolynomial <- function(x) {
  if ((Deg <- degre(x)) <= 0) return (Polynomial(0)) else {
    deriveCoeffs <- simplifyPolynomial(x)@coeffs[-1] * 1:Deg
    return (Polynomial(deriveCoeffs))
  }
}
#'
#' Essayons avec
derivePolynomial(poly1)
derivePolynomial(poly2)
derivePolynomial(poly0)
#'
#' ### 8. Création d'une fonction calculant la primitive d'un polynôme.
#'
#' A savoir que la constante C ajoutée est 0.
#+ results='markup'
primitivePolynomial <- function(p) {
  if ((Deg <- degre(p)) == -Inf) return (Polynomial(0)) else {
    primitiveCoeffs <- c(0, simplifyPolynomial(p)@coeffs / 1:(Deg + 1))
    return (Polynomial(primitiveCoeffs))
  }
}
#'
#' Essayons avec
primitivePolynomial(poly1)
primitivePolynomial(poly2)
primitivePolynomial(poly0)
#'
#' ### 9. Création d'une fonction évaluant un polynôme en un ou plusieurs points donnés.
#'
#' A noter qu'ici le calcul vectoriel (x est un vecteur représentant plusieurs points) est soutenu.
#+ results='markup'
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
#'
#' Essayons avec
poly1
values(p=poly1, x=c(1, 2))
poly2
values(poly2, c(0, -1, -2))
values(poly0, c(0, 0))
#'
#'
#' ### 10. Création d'une fonction calculant l'intégrale d'un polynôme entre [-1, 1].
#'
#' A noter que le calcul se fait à travers la primitive du polynôme et que xmin peut être plus petit, égal, ou
#' plus grand que xmax.
#+ results='markup'
integratePolynomial <- function(p, xmin, xmax) diff(values(primitivePolynomial(p), c(xmin, xmax)))
#'
#' Essayons avec
poly1
primitivePolynomial(poly1)
integratePolynomial(poly1, 1, 2)
integratePolynomial(poly1, 2, 1)
integratePolynomial(poly1, 1, 1)
integratePolynomial(poly0, 3, 10)
#'
#' ### 11. Création des méthodes pour `+`, `-` et `*` permettant l'addition, la soustraction et la multiplication
#' entre polynômes.
#'
#' *Attention* : A la 1ère fois j'ai fait cet exercice en créant des méthodes S4 resp. pour `+`, `-` et `*`, dont j'ai mis
#' le code en commentaires à la fin du scripte. Ça a marché sans aucun souci en premier temps. Cependant, lorsque
#' j'utilise la fonction "cv.glmnet" du paquet "glmnet" dans la question 23, R retourne une erreur car cette fonction-là
#' surcharge également la fonction `+` pour sa nouvelle classe `Matrix` et la méthode rentre en conflict avec la mienne !
#' En outre, surcharger séparémént les fonctions génériquse `+`, `-` et `*` n'est en effet pas la bonne façon de travailler
#' avec les méthodes S4 car il s'agit d'un GROUPE de fonctions générique. La bonne façon pour S4 est de surcharger le
#' groupe générique "Arith", d'où il faudra un traitement intégral de toutes les opérations arithmétiques dont on a besoin
#' dans l'ensemble du scripte. Pour cela, j'ai regroupé les exercices 11 et 16 et invite le lecteur à trouver la réponse
#' de cet exercice à la place de l'exercice 16 (Division euclidienne entre polynômes).
#'
#'
#' ### 12. Redéfinition de la classe `Polynomial` et de la fonction de création `Polynomial`.
#'
#' On va d'abord redéfinir la classe `Polynomial` en y ajoutant un slot `racines`.
#+ results='markup'
setClass("Polynomial", representation(coeffs = "numeric", racines = "numeric"), prototype(racines = NA_integer_))
#'
#' Renouvellement de la fonction de création "Polynomial" qui crée maintenant un objet "Polynomial" à travers
#' ses coefficients ou ses racines.
#+ results='markup'
Polynomial <- function(num, roots = FALSE, dominant = 1) {
  if (roots) {
    coeffs <- sapply(length(num):1, function(x) sum(apply(combn(num, x), 2, prod)))
    new("Polynomial", coeffs = dominant * c(coeffs, 1), racines = sort(num))
  } else
    new("Polynomial", coeffs = num)
}
#'
#' Essayons avec quelques examples
poly3 <- Polynomial(c(1,2), roots = TRUE)
print(poly3)
poly4 <- Polynomial(c(-1,-1), roots = TRUE, dominant = 5)
print(poly4)
poly5 <- Polynomial(c(0,0), roots = TRUE)
print(poly5)
poly6 <- Polynomial(c(0,0,1), roots = TRUE)
print(poly6)
#'
#' Il convient également de redfinir la façon d'afficher un objet polynomial.
#+ results='markup'
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
#'
#' Il convient de réinitialiser les polynômes créés avec l'ancienne classe.
poly1 <- Polynomial(c(1, -2, 3))
poly2 <- Polynomial(c(1,-3,2,0,-5,0,0,0))
poly0 <- Polynomial(0)
poly1
poly2
#'
#' La fonction `simplifyPolynomial` est-elle toujours opérationnelle ? (Qui ne doit pas perdre les racines
#' en cas d'existence)
poly6
poly6@coeffs <- c(0, 0, 1, 1, 0, 0, 0)
simplifyPolynomial(poly6)
simplifyPolynomial(poly6, degreVoulu = 10) ## Réponse : oui.
#'
#' Il conviendrait aussi de redéfinir la fonction `primitivePolynomial`, Pour la garde des racines
#' si les opérandes en ont. Mais vu qu'on s'en servira pas par la suite, ici on ne va pas le faire.
#'
#'
#' ### 13. Création d'une méthode pour `summary` qui affiche les coeffs et les racines d'un polynôme.
#'
#+ results='markup'
setMethod(
  f = "summary",
  signature = "Polynomial",
  definition = function(object) {
    return (list(coeffs = object@coeffs, racines = object@racines))
  }
)
#'
#' Essayons avec
summary(poly6)
summary(poly1)$coeffs
#'
#'
#' ### 14. Creation d'une méthode pour `plot` qui trace un polynôme.
#'
#+ results='markup'
setMethod(
  f = "plot",
  signature = "Polynomial",
  definition = plot.Polynomial <- function(x, xlim=c(-1,1), main = "Trace polyn\U00F4miale",
                                           xlab = parse(text = as.character.Polynomial(x, expressionStyle = TRUE)),
                                           ylab = "", col="darkblue", lwd=2, ...) {
    courbe <- function(y) values(x, y)
    plot(courbe, xlim = xlim, main = main, xlab = xlab, ylab = ylab, col=col, lwd=lwd, ...)
  }
)
#'
#' Essayons avec
plot.Polynomial(poly1)
plot(poly0, col='red', lwd=1.5)
plot(poly2, col="darkorange", lwd=3, xlim=c(-10, 10))
#'
#'
#' ### 15. Surcharger la fonction `lines` pour ajouter d'autres traces polynomiales.
#'
#' *Attention* : On ne va pas créer la fonction `lines.Polynomial`, dont on ne servira pas de toute façon.
#' Pour ajouter une autre courbe à un plot existant, il suffirera d'utiliser `plot.Polynomial` avec le paramètre
#' add = TRUE. Pourquoi cela marche ?
#'
#' Quand on fait appel à la méthode `plot.Polynomial`, sa fonction générique va chercher la méthode
#' `plot.function` au lieu de `plot.default` qui va à son tour faire appel à la méthode `plot.xy`,
#' parce que notre méthode `plot.Polynomial` est construite de manière qu'elle prenne une fonction comme
#' entrée et non pas un couple de vecteurs numériques. Du coup `plot.function` accepte add=TRUE comme
#' paramètre alors que ce n'est pas le cas de `plot.xy` !
#'
#' Essayons avec
plot.Polynomial(poly1, xlab = NULL)
plot.Polynomial(poly3, xlab = NULL, add=TRUE, col="darkred")
#'
#'
#' ### 11. et 16. Opérations arithmétiques polynomiales et entre un polynôme et un nombre.
#'
#' Maintenant toutes les opérations arithmétiques polynomiales ensembles.
#'
#' i) Opérations polynomiales binaires
#+ results='markup'
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
#'
#' ii) Opérations polynomiales unaires
#+ results='markup'
setMethod(
  f = "Arith",
  signature (e1 = "Polynomial"),
  definition = function(e1) {
    e1@coeffs <- callGeneric(e1@coeffs)
    return (simplifyPolynomial(e1))
  }
)
#'
#' Vérifions maintenant les méthodes avec une série d'examples.
#'
#' * Addition
poly1 ; poly2
poly1 + poly2
+ poly2
+ poly0
poly0 + poly0
#'
#' * Soustraction
poly1 ; poly2
poly1 - poly2
- poly2
- poly0
poly0 - poly0
#'
#' * Multipliacation
poly1; poly2
poly1 * poly2
poly0 * poly0
poly0 * poly1
#'
#' * Calcul du residu
Polynomial(c(1, 3, -1, 1), roots = TRUE) %/% Polynomial(c(5, 10, 5))
Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %/% Polynomial(c(3, -5, -6, -8))
poly1
poly1 %/% Polynomial(2)
## Cette commande renvoyera une erreur
## poly1 %/% poly0
#'
#' * Calcul du quotient
Polynomial(c(1, -1, 1), roots = TRUE) %% Polynomial(c(1, 1), roots = TRUE)
Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %% Polynomial(c(3, -5, -6, -8))
poly1
poly1 %% Polynomial(2)
## Cette commande renvoyera une erreur
## poly1 %% poly0
#'
#' iii) Ensuite, au lieu de convertir à chaque fois le diviseur numérique en polynôme puis faire la division
#' polynomiale, il convient de définir la division et la multiplication d'un polynôme par un nombre, pour des
#' raisons d'efficacité de calcul.
#+ results='markup'
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
#'
poly1 / 0.5
poly2 * 2
#'
#+ results='markup'
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
#'
2 * poly2
#'
#'
#' ### 17. Création d'une fonction qui renvoie une base polynomiale, ortho-normée ou non.
#'
#' Ecrions d'abord une méthode pour `sum` qui prend comme paramètre une liste de listes chacune composée d'un seul
#' polynôme et calcule leur somme. Ce qui rendra le prochain morceaux du code beaucoup plus lisible.
#+ results='markup'
.sum_def <- function(x, ..., na.rm = FALSE) base::sum(x, ..., na.rm = na.rm)
setGeneric("sum", function(x, ..., na.rm = FALSE) standardGeneric("sum"), useAsDefault = .sum_def, group="Summary")
#+ results='markup'
setMethod(
  f = "sum",
  signature (x = "list", na.rm = "ANY"),
  definition = function(x, ..., na.rm) {
    if (length(x) == 0) return (Polynomial(0)) else
      if (length(x) == 1) return (simplifyPolynomial(x[[1]])) else
        return (simplifyPolynomial(x[[1]] + sum(x[-1])))
  }
)
#'
#' Essayons simplement avec
poly1
sum(list(poly1, poly1, poly1, poly1))
#'
#' Maintenant la fonction qui donne la base
#+ results='markup'
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
#'
#' Un example de base non orthogonale
basePolynomial(3)
#'
#' Puis un example de base ortho-normée
BasePoly4 <- basePolynomial(4, ortho=TRUE)
BasePoly4
#' Vérifions que la base est bien ortho-normée
sapply(BasePoly4, function(x) integratePolynomial(x*x, -1, 1)) # doit renvoyer : [1] 1 1 1 1
apply(combn(BasePoly4, 2), 2, function(x) integratePolynomial(x[[1]]*x[[2]], -1, 1)) # doit renvoyer : [1] 0 0 0 0 0 0
#'
#'
#' ### 18. Représentation des 10 premiers polynômes sur un graphique.
#'
#+ results='markup'
BasePoly10 <- basePolynomial(10, ortho=TRUE)
#+ results='markup'
set.seed(21007979)
#+ results='markup'
couleurs <- sample(colors(), 10)
#'
invisible(sapply(1:10, function(i, ...) plot(BasePoly10[[i]], col = couleurs[i], add = (i != 1), ...),
                 xlim = c(0, 1), ylim = c(-2, 2), xlab = "10 polyn\U00F4mes orthonorm\U00E9s"))
legend(0, -1.5, inset = 0.05, legend = paste("poly", 1:10, sep = ""), col = couleurs,
       lwd = 1, xpd = TRUE, ncol = 5, y.intersp = 0.7, cex = 0.7)
#'
#'
#' ### 19. Création d'une fonction qui fait la régression linéaire polynomiale à l'aide d'une base polynomiale donnée.
#'
#+ results='markup'
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
  ## Ici, je suis obsessif de fabriquer une formule plutôt que de créer une matrice avec une série de variables,
  ## pour que mon modèle à la fin reste un modèle entre X et Y. J'ai tout intérêt de faire ça comme ça car plus
  ## tard, quand je prédis pour n'importe quelle valeur de X entre [-1, 1], je pourrai prendre ce modèle calculé
  ## et lui donner directement la valeur du nouveau X. Sinon, je serais obligé de recalculer à chaque fois la
  ## matrice, si ce nouveau X n'appartient pas à l'ensemble des valeurs de X utilisées pour ajuster le modèle.

  names(lmFit$coefficients) <- paste("f_", 1:length(Base), "(X)", sep = "")
  return(lmFit)
}
#'
#'
#' ### 20. Simulations d'un jeu de données.
#'
X1 <- runif(n = 100, min = -1, max = 1)
epsilon <- rnorm(n = 100)
Y1 <- sin(4 * pi * X1) + epsilon
head(X1)
tail(Y1)
#'
#'
#' ### 21. Traces du nuage des points et les estimations des modèles avec des bases de 5, 10 et 15 composantes.
#'
#' En faveur de l'exercice 22, j'ai emballé les étapes de tracer dans une fonction, qui prend comme paramètres
#' entres autres un nombre quelconque de bases quelconques et le jeu de données.
#+ results='markup'
traceModelesAjustes <- function(newPar = c(2, 2), dimBases = c(5, 10, 15), FUN = basePolynomial,
                                Y = Y1, X = X1, ...) {
  FitListe <- lapply(dimBases, function(x) regLinBaseQconq(X, Y, FUN(x, ...)))
  XGrille = seq(-1, 1, by = 0.01)
  YPreditListe <- lapply(FitListe, function(Fit) predict(Fit, data.frame(X = XGrille)))
  oldPar <- par(mfrow = newPar)
  for (i in c(0, dimBases)) {
    plot(Y~X, pch = 16, col = "darkblue")
    plot(function(x) sin(4 * pi * x), xlim=(c(-1, 1)), add=TRUE, lwd=2, col="darkgreen")
    if (i > 0) {
      title(main = paste("Base", i))
      lines(XGrille, YPreditListe[[which(dimBases == i)]], lwd=2, col = "darkorange")
    } else
      title(main = "Jeu de donn\U{E9}es")
  }
  par(oldPar)
}
#'
#' Essayons maintenant avec les 2 types de bases polynomiales
traceModelesAjustes(ortho = TRUE)
traceModelesAjustes(ortho = FALSE)
## Aucune différence que la précédente ? Forcément, car pour les modèles complets,
## l'espace vectoriel engendré par les deux types de bases est le même.
#'
#'
#' ## Partie Bonus
#'
#'
#' ### 22. Base sinusoïdale.
#'
#' D'abord la fonction pour créer une base sinusoïdale de façon récursive.
baseSinusoidale <- function(d) {
  return (if (d == 1) list(function(x) 0*x + 1/2) else c(baseSinusoidale(d-1), function(x) sin((d - 1/2) * pi * x)))
}
#'
#' On essaie la fonction avec une base de 3 dimensions.
BaseSin3 <- baseSinusoidale(3)
BaseSin3[[3]](c(1,2,3))
BaseSin3[[2]](c(1,2,3))
BaseSin3[[1]](c(1,2,3))
#'
#' Maintenant on trace, grâce à la flexibilité de la fonction de plot créée à la question précédente, les modèles
#' ajustés avec une base sinusoïdale de 1 - 11 dimensions.
traceModelesAjustes(newPar = c(3,4), dimBases = c(1:11), FUN = baseSinusoidale)
#'
#'
#' ### 23. Sélection par Lasso.
#'
#' Pour faire Lasso, j'utilise le paquet "glmnet" au lieu de "lars", publié par Jérôme Friedman et al., dernière mise
#'  à jour le 30 avril 2017.
#########################################################################################
# install.packages("glmnet") # Enlever le 1er '#' pour l'installation du paquet #########
#########################################################################################
library(glmnet)
#'
#' **On traite d'abord la base polynomiale ortho-normée.**
#'
#' Création d'une base polynomiale orthonormée de 20 dimensions (Par observation à l'oeil des graphiques précédentes et
#' en prenant compte du petit nombre d'observations, on peut dire que 20 composantes suffirait pour faire la sélection
#' des features).
BasePolyOrtho20 <- basePolynomial(20, ortho = TRUE)
#' Création de la matrice à 20 variables (composantes de la base) et 100 lignes correspondant à chaque observation de X1.
X1OrthoMatrice20 <- sapply(BasePolyOrtho20, function(composante) sapply(X1, function(x) values(composante, x)))
#' Changement de nom des variables pour éviter de longues expressions.
colnames(X1OrthoMatrice20)  <- paste("Comp", 1:20, sep = "")
#'
#' Une série de valeurs de lambda avec qui R va devoir résoudre Lasso. Ces valeurs sont choisies ultérieurement, sachant
#' que lambda plus grand que 1 ne servant à rien dans ce cas.
grillePolyOrtho <- 10^seq(0, -2, length = 100)
#'
#' Maintenant la fonction glmnet va résoudre Lasso et renvoyer associée à chaque valeur de lambda une série de beta,
#' coefficients de chaque variables, stockée dans une matrice accessible par la function "coef".
#' Ici il convient de remarquer deux choses. L'une est que "glmnet" ajoute automatiquement le terme constant qui ne peut
#' pas être éteint en mettant "intercept = FALSE" ! Dans les anciennes versions de glmnet avant 2012 oui mais pas celles
#' d'après. Ceci pourrait affecter fort le résultat de Lasso car la première composante elle aussi est constante (grave
#' pour elle au moins). Mais vus les betas qu'on a obtenus au final, tant ceux associés au terme constant que ceux
#' associés à la première composante restent tout le temps 0, on est tranquille.
#' L'autre chose est que glmnet() fait par défaut la standardisation. Je ne vois pas de raison que celle-ci soit gênante
#' donc je l'ai laissée.
lassoPolyOrtho20 <- glmnet(X1OrthoMatrice20, Y1, intercept = FALSE, alpha = 1, lambda = grillePolyOrtho)
#'
summary(lassoPolyOrtho20)
coef(lassoPolyOrtho20)[, 30:35] # Sparse Matrix, . représente un 0.
#'
#' Maintenant pour tracer l'effet de la sélection contre les valeurs de log lambda, on charge le paquet `plotmo`.
#########################################################################################
# install.packages("plotmo") # Enlever le 1er '#' pour l'installation du paquet #########
#########################################################################################
library(plotmo)
plot_glmnet(lassoPolyOrtho20, label = 5) # libeller les 5 predicteurs les plus importants
#' Maintenant on met les autres variables en gris
randCol20 <- rep("lightgray", times = 20)
randCol20[c(1, 2, 3, 4, 18)] <- 1:5
plot_glmnet(lassoPolyOrtho20, label = 5, col = randCol20)
#'
#' Pour savoir quelle est la valeur optimale de lambda, on peut faire une validation croisée à 5 plis. Cela peut être
#' éffectué par la fonction `cv.glmnet` du paquet `glmnet`.
cvPolyOrtho = cv.glmnet(X1OrthoMatrice20, Y1, alpha = 1, nfolds = 5) # alpha = 1 pour Lasso
summary(cvPolyOrtho)
plot(cvPolyOrtho)
cvPolyOrtho$lambda.min # lambda optimal minimisant la MSE dans la vadilation croisée
dim(cvPolyOrtho$glmnet.fit$beta) # Combien de valeurs de lambda a-t-il testé en tout ?
cvPolyOrtho$glmnet.fit$beta[cvPolyOrtho$lambda == cvPolyOrtho$lambda.min] # les beta lorsque lambda prend sa valeur optimale
sum(cvPolyOrtho$glmnet.fit$beta[cvPolyOrtho$lambda == cvPolyOrtho$lambda.min] != 0) # nombre de beta non nuls dans ce cas
#' Ça veut dire qu'il faut aller jusqu'aux 12-14 dimensions pour qu'une base plynomiale ortho-normée représente
#' pour soi-même au mieux les données, d'après la validation croisée.
min(cvPolyOrtho$cvm) # la MSE minimum
#'
#' **Ensuite on traite la base plynomiale non-orthonormée, exactement de la même façon.**
#'
#' Création d'une base polynomiale NON-orthonormée de 20 dimensions.
BasePolyNONOrtho20 <- basePolynomial(20, ortho = FALSE)
#' Création de la matrice à 20 variables (composantes de la base) et 100 lignes correspondant à chaque observation de `X1`.
X1NONOrthoMatrice20 <- sapply(BasePolyNONOrtho20, function(composante) sapply(X1, function(x) values(composante, x)))
#' Changement de nom des variables pour éviter de longues expressions.
colnames(X1NONOrthoMatrice20)  <- paste("Comp", 1:20, sep = "")
#' Une série de valeurs de lambda avec qui R va devoir résoudre Lasso.
grillePolyNONOrtho <- 10^seq(-1, -3, length = 100)
lassoPolyNONOrtho20 <- glmnet(X1NONOrthoMatrice20, Y1, intercept = FALSE, alpha = 1, lambda = grillePolyNONOrtho)
plot_glmnet(lassoPolyNONOrtho20, label = 5)

#' Pour savoir quelle est la valeur optimale de lambda, on peut faire une validation croisée à 5 plis. Cela peut être
#' éffectué par la fonction `cv.glmnet`.
#+ warning=FALSE
cvPolyNONOrtho = cv.glmnet(X1NONOrthoMatrice20, Y1, alpha = 1, nfolds = 5)
summary(cvPolyNONOrtho)
plot(cvPolyNONOrtho)
cvPolyNONOrtho$lambda.min # lambda optimal minimisant la MSE dans la vadilation croisée
dim(cvPolyNONOrtho$glmnet.fit$beta) # Combien de valeurs de lambda a-t-il testé en tout ?
cvPolyNONOrtho$glmnet.fit$beta[, cvPolyNONOrtho$lambda == cvPolyNONOrtho$lambda.min] # les beta lorsque lambda prend sa valeur optimale
sum(cvPolyNONOrtho$glmnet.fit$beta[, "s56"] != 0) # nombre de beta non nuls dans ce cas
#' Au niveau du nombre de features sélectionnées, c'est pareil qu'avant. En effet, on doit s'attendre à à peu près
#' la même explicabilité pour les deux bases polynomiales.
min(cvPolyNONOrtho$cvm) # la MSE minimum
#'
#' **Enfin on traite la base sinusoïdale.**
#'
#' Création d'une base sinusoïdale de 20 dimensions.
BaseSin20 <- baseSinusoidale(20)
#' Création de la matrice à 20 variables (composantes de la base) et 100 lignes correspondant à chaque observation de X1.
X1SinMatrice20 <- sapply(BaseSin20, function(composante) sapply(X1, function(x) composante(x)))
#' Changement de nom des variables pour éviter de longues expressions.
colnames(X1SinMatrice20)  <- paste("Comp", 1:20, sep = "")
#' Une série de valeurs de lambda avec qui R va devoir résoudre Lasso.
grilleSin <- 10^seq(0, -4, length = 100)
lassoSin20 <- glmnet(X1SinMatrice20, Y1, intercept = FALSE, alpha = 1, lambda = grilleSin)
plot_glmnet(lassoSin20, label = 5)
#'
#' Pour savoir quelle est la valeur optimale de lambda, on peut faire une validation croisée à 5 plis. Cela peut être
#' éffectué par la fonction `cv.glmnet`.
cvSin = cv.glmnet(X1SinMatrice20, Y1, alpha = 1, nfolds = 5)
summary(cvSin)
plot(cvSin)
cvSin$lambda.min # lambda optimal minimisant la MSE dans la vadilation croisée
dim(cvSin$glmnet.fit$beta) # Combien de valeurs de lambda a-t-il testé en tout ?
cvSin$glmnet.fit$beta[, cvSin$lambda == cvSin$lambda.min] # les beta lorsque lambda prend sa valeur optimale
sum(cvSin$glmnet.fit$beta[, cvSin$lambda == cvSin$lambda.min] != 0) # nombre de beta non nuls dans ce cas
#' On voit que la base sinusoïdale n'a besoin que de la moitié de prédicteurs par rapport aux deux autres bases
#' pour expliquer les données,
min(cvSin$cvm) # et que sa MSE est mieux aussi !
#'
#' **On aimerais illustrer les MSEs estimées par la validation croisée de ces trois modèles contre log lambda
#' dans un plot.**
plot(1, 1, xlim = c(-8, 0), ylim = c(0, 4), xlab = "log lambda", ylab = "MSE de VC", type = 'n')
lines(log(cvPolyOrtho$lambda), cvPolyOrtho$cvm, col="purple", lwd = 2, lty = 1)
lines(log(cvPolyNONOrtho$lambda), cvPolyNONOrtho$cvm, col="blue", lwd = 2, lty = 2)
lines(log(cvSin$lambda), cvSin$cvm, col="red", lwd = 2, lty = 3)
abline(v=log(c(cvPolyOrtho$lambda.min, cvPolyNONOrtho$lambda.min, cvSin$lambda.min)), col = c('purple', 'blue', 'red'),
       lty = 1:3, lwd = 1) # positions optimales par validation croisée
legend("topleft", c("polyOrtho", "polyNonOrtho", "sinus"), lty = 1:3, col = c('purple', 'blue', 'red'), cex = 0.8)
#' Il semble que la base sinusoïdale est aussi bonne que la base polynomiale ortho-normée et de plus, elle est plus
#' explicable que les deux autres, car elle n'a besoin que de 6 variables pour arriver au meilleur lambda, alors que
#' ceci est 12-14 pour les bases polynomiales. *Est-ce vrai ?*
#'
#' **Maintenant, on mesurerera la performance des 3 méthodes sur un jeu de données de test.** On aurait dû couper le
#' jeu de données originel en 2 avant d'ajuster nos modèles ; mais comme les données sont simulées et rien ne nous
#' empêche d'en simuler beaucoup d'autres. On va donc créer un jeu de données de test de même taille de la même façon.
#'
#' Remarque : pour évaluer ces trois modèles, il suffit dans notre situation de comparer les valeurs prédites resp. par
#' chaque modèle avec la vraie fonction sous-jacente. Mais comme ceci n'est jamais possible en pratique, on prend
#' tout de même un jeu de données de test et on plotte les graphiques de la MSE de test.
#'
#' Génération d'un autre jeu de données contenant 100 observations, de la même façon qu'avant.
X2 <- runif(n = 100, min = -1, max = 1)
epsilon2 <- rnorm(n = 100)
Y2 <- sin(4 * pi * X2) + epsilon2
#' On l'illustre ensemble avec l'ancien jeu de données.
plot(Y1~X1, pch = 16, col = "darkblue", main = "100 données d'entraînement \n et 100 données de test",
     xlab = "X", ylab = "Y")
points(Y2~X2, pch = 16, col = "darkred")
plot(function(x) sin(4 * pi * x), xlim=(c(-1, 1)), add=TRUE, lwd=2, col="darkgreen")
legend(x = -1, y = 5, c("entraînement", "test"), pch = 16, col = c("darkblue", "darkred"), cex = 0.7, xpd = TRUE)
#'
#' Il vaut mieux avoir une série de valeurs de lambda commune significative pour toutes les trois bases
grilleCommune <- 10^seq(0, -4, length = 200)
#'
#' On prédit les réponses de test en prenant le modèle de Lasso ajusté à travers les données d'entraînement et calcule
#' la MSE de test pour la base polynomiale ortho-normée
X2OrthoMatrice20 <- sapply(BasePolyOrtho20, function(composante) sapply(X2, function(x) values(composante, x)))
predOrtho <- predict(lassoPolyOrtho20, s=grilleCommune, newx = X2OrthoMatrice20)
MSEOrtho <- apply(predOrtho, 2, function(x) mean((x - Y2)^2))
#'
#' On en fait du pareil avec la base polynomiale non-ortho-normée
X2NONOrthoMatrice20 <- sapply(BasePolyNONOrtho20, function(composante) sapply(X2, function(x) values(composante, x)))
predNONOrtho <- predict(lassoPolyNONOrtho20, s=grilleCommune, newx = X2NONOrthoMatrice20)
MSENONOrtho <- apply(predNONOrtho, 2, function(x) mean((x - Y2)^2))
#'
#' On en fait du pareil avec la base sinusoïdale
X2SinMatrice20 <- sapply(BaseSin20, function(composante) sapply(X1, function(x) composante(x)))
predSin <- predict(lassoSin20, s=grilleCommune, newx = X2SinMatrice20)
MSESin <- apply(predSin, 2, function(x) mean((x - Y2)^2))
#'
#' Et on plotte les courbes de MSE de test pour les trois bases ainsi que leurs positions optimales estimées
matplot(log(grilleCommune), cbind(MSEOrtho, MSENONOrtho, MSESin), col = c('purple', 'blue', 'red'), type = 'l', lwd = 2,
        xlab = "log lambda", ylab = "MSE de test", main = "Lasso")
abline(v = log(c(cvPolyOrtho$lambda.min, cvPolyNONOrtho$lambda.min, cvSin$lambda.min)), col = c('purple', 'blue', 'red'),
       lty = 1:3, lwd = 1)
abline(h = mean((Y2 - mean(Y1))^2)) ## droite sur laquelle se réunissent les 3 MSE lorque lambda devient grand,
## aussi la MSE qu'on aura quand on prédit pour n'importe quelle valeur de X entre [-1, 1] avec la moyenne de Y1.
legend("topleft", c("polyOrtho", "polyNonOrtho", "sinus"), lty = 1:3, col = c('purple', 'blue', 'red'), cex = 0.8)
#'
#' **De façon étonnante, la base sinusoïdale a joué le pire, même beaucoup pire que l'estimation banale par la moyenne
#' de `Y1`, contrairement au résultats qu'on a eus avec la validation croisée. La base polynomiale ortho-normée est la
#' meilleure, en plus son estimation de la valeur de lambda optimale est assez exacte. Quelle en est la raison ?**
#'
#' **On se rapelle que la fonction sous-jacente est une fonction trigonométrique de l'amplitude ±1, or l'écart-type du
#' terme de l'erreur tout seul est aussi 1. Ça veut dire que les réponses sont très instables. En plus, 100 observations
#' sont une taille assez petite par rapport à 20 prédicteurs. Tout ça entraîne un surapprentissage pour la base
#' sinusoïdale, qui a en fait la meilleure capacité d'ajuster le modèle, grâce à sa similarité avec la fonction
#' sous-jacente. Là c'est la robustesse de la base polynomiale qui a gagné. On peut par contre bien croire que, quand
#' il s'agit de 1000 voire 10,000 d'observations et que l'amplitude de la fonction sous-jacente est plus grande que
#' l'écart-type du terme de l'erreur, la base sinusoïdale va aisément battre les deux autres.**
#'
#'
#' ## Références
#'
#' **[1]** *Gareth James et al. (2013)*, An Introduction to Statistical Learning with Applications in R.
#'
#' **[2]** *Hadley Wickham (2014)*, Advanced R.
#'
#'
#'

## ANNEXE : Les opérations arithmétiques que j'ai faites séparément à la première fois
## Rappelez-vous du paramètre "d'en trop" "degreVoulu" dans la fonction simplifyPolynomial(), dont le but est de
## compléter un polynôme avec des 0 à un degré donné plus élevé que celui du polynôme. Maintenant on va s'en
## servir pour éviter le mécanisme du recyclage des opérations vectorielles de R.

# ## Addition.
# setMethod(
#   f = `+`,
#   signature (e1 = "Polynomial", e2 = "Polynomial"),
#   definition = `+.Polynomial` <- function(e1, e2) {
#     if (missing(e2)) resultat_coeffs <- callGeneric(e1@coeffs) else {
#       maxDeg <- max(degre(e1), degre(e2))
#       e1 <- simplifyPolynomial(e1, degreVoulu = maxDeg)
#       e2 <- simplifyPolynomial(e2, degreVoulu = maxDeg)
#       resultat_coeffs <- callGeneric(e1@coeffs, e2@coeffs)
#     }
#     return (simplifyPolynomial(Polynomial(resultat_coeffs)))
#   }
# )
#
# poly1 ; poly2
# poly1 + poly2
# + poly2
# + poly0
# poly0 + poly0
#
# ## Soustraction. Exactement le même code sauf "-" au lieu de "+".
# setMethod(
#   f = `-`,
#   signature (e1 = "Polynomial", e2 = "Polynomial"),
#   definition = `-.Polynomial` <- function(e1, e2) {
#     if (missing(e2)) resultat_coeffs <- callGeneric(e1@coeffs) else {
#       maxDeg <- max(degre(e1), degre(e2))
#       e1 <- simplifyPolynomial(e1, degreVoulu = maxDeg)
#       e2 <- simplifyPolynomial(e2, degreVoulu = maxDeg)
#       resultat_coeffs <- callGeneric(e1@coeffs, e2@coeffs)
#     }
#     return (simplifyPolynomial(Polynomial(resultat_coeffs)))
#   }
# )
#
# poly1 ; poly2
# poly1 - poly2
# - poly2
# - poly0
# poly0 - poly0
#
# ## Multiplication. Il y a des algorithmes efficaces dédiés au calcul de la multiplication polynomiale
# ## en informatique, e.g. celui de "diviser pour régner". Ici on ne va pas le faire, puisque
# ## 1. R n'est de toute façon pas un logiciel très efficace, d'où de couper un polynôme en morceaux va prendre du temps ;
# ## 2. On ne va pas faire de la multiplication avec des polynômes gigantesques, le temps gagné sera pas remarquable.
# ## Donc ici, on le fait avec des boules.
#
# setMethod(
#   f = `*`,
#   signature (e1 = "Polynomial", e2 = "Polynomial"),
#   definition = `-.Polynomial` <- function(e1, e2) {
#     Deg1 <- degre(e1)
#     Deg2 <- degre(e2)
#     if (Deg1 == -Inf || Deg2 == -Inf) return (Polynomial(0)) else {
#       DegResultat <- Deg1 + Deg2
#       coeffs1 <- simplifyPolynomial(e1)@coeffs
#       coeffs2 <- simplifyPolynomial(e2)@coeffs
#       resultat_coeffs <- numeric(DegResultat + 1)
#       for (i in 1:(Deg1+1))
#         for (j in 1:(Deg2+1))
#           resultat_coeffs[i+j-1] <- resultat_coeffs[i+j-1] + coeffs1[i] * coeffs2[j]
#       return (Polynomial(resultat_coeffs))
#     }
#   }
# )
# poly1; poly2
# poly1 * poly2
# poly0 * poly0
# poly0 * poly1
#
# ## Calcul du residu.
# setMethod(
#   f = `%/%`,
#   signature (e1 = "Polynomial", e2 = "Polynomial"),
#   definition = `%/%.Polynomial` <- function(e1, e2) {
#     if (degre(e2) == -Inf) stop("Diviseur ne doit pas être nul !") else
#       if (degre(e2) == 0) return (Polynomial(0)) else {
#         coeffs1 <- simplifyPolynomial(e1)@coeffs
#         coeffs2 <- simplifyPolynomial(e2)@coeffs
#         if (length(coeffs1) < length(coeffs2) || length(coeffs1) == 0) {
#           e1@racines <- NA_integer_
#           return (simplifyPolynomial(e1))
#         } else {
#           e1@coeffs <- coeffs1 - c(rep(0, length(coeffs1) - length(coeffs2)),
#                                    tail(coeffs1, 1) / tail(coeffs2, 1) * coeffs2)
#           return (`%/%.Polynomial`(simplifyPolynomial(e1), e2))
#         }
#       }
#   }
# )
#
# Polynomial(c(1, 3, -1, 1), roots = TRUE) %/% Polynomial(c(5, 10, 5))
# Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %/% Polynomial(c(3, -5, -6, -8))
# poly1
# poly1 %/% Polynomial(2)
# # poly1 %/% poly0
#
# ## Calcul du quotient.
# setMethod(
#   f = `%%`,
#   signature (e1 = "Polynomial", e2 = "Polynomial"),
#   definition = `%%.Polynomial` <- function(e1, e2) {
#     if (degre(e2) == -Inf) stop("Diviseur ne doit pas être nul !") else {
#       coeffs1 <- simplifyPolynomial(e1)@coeffs
#       coeffs2 <- simplifyPolynomial(e2)@coeffs
#       if (length(coeffs1) < length(coeffs2) || (length(coeffs1) == 1 && coeffs1[1] == 0)) return (Polynomial(0)) else {
#         Deg_diff <- length(coeffs1) - length(coeffs2)
#         Quotient_coeff_dominant <- tail(coeffs1, 1) / tail(coeffs2, 1)
#         e1@coeffs <- coeffs1 - c(rep(0, Deg_diff), Quotient_coeff_dominant * coeffs2)
#         return (Polynomial(c(rep(0, Deg_diff), Quotient_coeff_dominant)) +
#                   `%%.Polynomial`(simplifyPolynomial(e1), e2))
#       }
#     }
#   }
# )
#
# Polynomial(c(1, -1, 1), roots = TRUE) %% Polynomial(c(1, 1), roots = TRUE)
# Polynomial(c(1, 0, -2, 0, 0, 3, -5, -6, -8)) %% Polynomial(c(3, -5, -6, -8))
# poly1
# poly1 %% Polynomial(2)
# # poly1 %% poly0
#
# ## D'abord au lieu de convertir à chaque fois le diviseur numérique en polynôme puis faire la division polynomiale,
# ## il convient de définir la division d'un polynôme par un nombre, pour des raisons d'efficacité de calcul.
# setMethod(
#   f = `/`,
#   signature (e1 = "Polynomial", e2 = "numeric"),
#   definition = function(e1, e2) {
#     if (e2 == 0) stop("Diviseur ne doit pas être nul !") else {
#       e1@coeffs <- e1@coeffs / e2
#       return (simplifyPolynomial(e1))
#     }
#   }
# )
# poly1 / 0.5
#
# ## Il s'en va de même pour la multiplication par un nombre.
# setMethod(
#   f = `*`,
#   signature (e1 = "Polynomial", e2 = "numeric"),
#   definition = function(e1, e2) {
#     e1@coeffs <- e1@coeffs * e2
#     return (simplifyPolynomial(e1))
#   }
# )
# poly2 * 2
#
# setMethod(
#   f = "*",
#   signature (e1 = "numeric", e2 = "Polynomial"),
#   definition = function(e1, e2) {
#     e2@coeffs <- e1 * e2@coeffs
#     return (simplifyPolynomial(e2))
#   }
# )
# 2 * poly2

