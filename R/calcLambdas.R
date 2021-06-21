#' Calculate a Holgate's Poisson parameters for a soccer match.
#'
#' @param coef a three column matrix resulting from calcCoefSD1 function.
#' @param home home team name.
#' @param visitor visitor team name.
#' @param local indicator for local factor to the match. Must be 0 or 1 
#'              (default).
#'
#' @return a vector of lenght three containing the Holgate's Poisson parameters
#'         for the match. 
#' 
#' @export
#'
# @examples
#' 
calcLambdas <- function(coef, home, visitor, local = 1) {
  
  teams = rownames(coef)
  n = dim(coef)[1]
  idxH = (teams == home)
  idxV = (teams == visitor)
  
  Esum <- coef[idxH, 1] + coef[idxV, 1] + local*coef[n, 1]
  Edif <- coef[idxH, 2] - coef[idxV, 2] + local*coef[n, 2]
  Esum2 <- coef[idxH, 3] + coef[idxV, 3] + local*coef[n, 3]
  
  lambda.M <- (Edif + 2*Esum - Esum2 + Esum^2)/2
  if(lambda.M <= 0)
    lambda.M <- 0.25
  lambda.V <- (2*Esum - Edif - Esum2 + Esum^2)/2
  if(lambda.V <= 0)
    lambda.V <- 0.25
  lambda.12 <- (Esum2 - Esum^2 - Esum)/2
  if(lambda.12 <= 0)
    lambda.12 <- 0.01
  
  lambdas <- cbind(lambda.M, lambda.V, lambda.12)

  return(lambdas)
 
}