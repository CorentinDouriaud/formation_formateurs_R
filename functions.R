# FONCTIONS ---------------------------------

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Calculer des statistiques agrégées pour un vecteur de données
#'
#' Cette fonction permet de calculer différentes statistiques agrégées, telles que la moyenne, l'écart-type
#' et la variance, pour un vecteur de données donné.
#'
#' @param donnees Un vecteur de données numériques pour lequel vous souhaitez calculer la statistique agrégée.
#' @param agregat Le type de statistique à calculer. Les options valides sont "moyenne" (par défaut),
#'                "ecart-type" ou "sd", et "variance".
#' @param ... Des paramètres supplémentaires qui seront passés à la fonction statistique choisie (par exemple,
#'            `na.rm` pour ignorer les valeurs manquantes).
#'
#' @return La statistique agrégée calculée en fonction de l'option spécifiée.
#'
#' @examples
#' # Calcul de la moyenne d'un vecteur de données
#' donnees <- c(1, 2, 3, 4, 5)
#' fonction_de_stat_agregee(donnees)  # Calcul de la moyenne par défaut
#' fonction_de_stat_agregee(donnees, "ecart-type")  # Calcul de l'écart-type
#' fonction_de_stat_agregee(donnees, "variance")  # Calcul de la variance
#'
#' @seealso
#' \code{\link{mean}}, \code{\link{sd}}, \code{\link{var}}
#'
#' @importFrom stats mean sd var
#'
#' @export
#' @rdname fonction_de_stat_agregee
#'
#' @author Votre nom
#'
#' @version 1.0
#'
#' @keywords statistique, agrégation
#'
#' @note Les éléments non numériques dans le vecteur de données seront ignorés lors du calcul.
#'
#' @references
#' Vous pouvez fournir ici des références ou des liens utiles.
#'
#' @examples
#' donnees <- c(1, 2, 3, 4, 5)
#' fonction_de_stat_agregee(donnees)
#'
#' @export

fonction_de_stat_agregee <- function(donnees, agregat = "moyenne", ...) {
  if (agregat == "moyenne") {
    resultat <- mean(donnees, na.rm = TRUE, ...)
  } else if (agregat == "ecart-type" || agregat == "sd") {
    resultat <- sd(donnees, na.rm = TRUE, ...)
  } else if (agregat == "variance") {
    resultat <- var(donnees, na.rm = TRUE, ...)
  }
  return(resultat)
}