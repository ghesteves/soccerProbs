#' Calculate coefficients for SD1 method.
#'
#' @param data a list resulting from crMatrixSD function.
#'
#' @return a three column matrix, with parameters alpha, beta and gamma.
#' 
#' @export
#'
#' @examples
#' 
calcCoefSD1 <- function(data) {
  
  ## Calculating beta coefficients
  tmp <- (t(data$matT) %*% data$matT)
  betas <- (MASS::ginv(tmp) %*% t(data$matT) %*% data$dif)   
  
  
  ## Solving the inverse of t(data$matS) %*% data$matS
  tmp <- (t(data$matS) %*% data$matS)
  if (round(det(tmp), 4) != 0)
    tmp2 <- solve(tmp) %*% t(data$matS)
  else
    tmp2 <- MASS::ginv(tmp) %*% t(data$matS)
  
  ## Calculating alpha coefficients
  alphas <- tmp2 %*% data$sum
  
  ## Calculating gammas coefficients
  gammas <- tmp2 %*% data$sum^2
  
  obj <- cbind(alphas, betas, gammas)
  colnames(obj) <- c("Alpha", "Beta", "Gamma")
  rownames(obj) <- colnames(data$matS)
  
  return(obj)

}