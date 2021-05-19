#' Calculate probabilities for a particular soccer match.
#'
#' @param lambdas a vector of lenght three containing the Holgate's Poisson
#'        parameters for the match (resulting from calcLambdas function).
#' @param dim.matrix matrix (square) dimension to use to calculate all probabilities.
#'
#' @return a list with two elements, Probs and Matrix. 
#' 
#' @export
#'
# @examples
#'
calcMatProb <- function(lambdas, dim.matrix = 11) {

  result <- matrix(nrow=dim.matrix, ncol=dim.matrix)

  for(i in 1:(dim.matrix-1))  {
    for (j in 1:(dim.matrix-1)) {
      result[i,j] <- extraDistr::dbvpois(j-1, i-1, lambdas[1], 
                                         lambdas[2], lambdas[3])
    }
  }
  
  ## Calculating the last column (last element apart)
  tmp <- stats::dpois(0:(dim.matrix-2), lambdas[2]+lambdas[3]) - 
    apply(result[-dim.matrix, -dim.matrix], 1, sum)
  
  result[1:(dim.matrix-1), dim.matrix] <- tmp
  
  ## Calculating the last line (last element apart)
  tmp <- stats::dpois(0:(dim.matrix-2), lambdas[1]+lambdas[3]) - 
    apply(result[-dim.matrix, -dim.matrix], 2, sum)
  
  result[dim.matrix, 1:(dim.matrix-1)] <- tmp
  
  ## Calculating the last element
  result[dim.matrix, dim.matrix] <- 1-sum(result, na.rm=T)
  
  
  
  ## Calculating probabilities
  idx <- upper.tri(result)
  homeWin <- sum(result[idx]) ## Prob (home win)
  
  idx <- lower.tri(result)
  visWin <- sum(result[idx]) ## Prob (visitor win)
  
  tie <- sum(diag(result)) ## Prob (tie)
  
  probs <- c(homeWin, tie, visWin)
  names(probs) <- c("Home", "Tie", "Visitor")

  ## Return a list with a vector of probabilities and a matrix with
  ## all bivariate Poisson probabilities
  return(list(Probs=probs, Matrix=result))  

}