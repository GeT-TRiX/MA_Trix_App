#' showHello est une fonction R permettant d'afficher un message de salutation sur le console.
#' @title La fonction « showHello »
#' @author Juhui Wang
#' @param s une chaîne de caractères formant la formule de salutation.
#' @return la fonction ne retourne rien du tout.
#' @references The package Roxygen (www.roxygen.org)
#' @note Si s est une variable numérique, la fonction affiche la valeur
#' numérique de s.
#' @examples
#' HelloWorld("Bonjour de la part de Roxygen")
#' @export

showHello <-function(s="Hello World"){
  cat(s,"!\n")
}