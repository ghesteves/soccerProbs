#' Creat Data Matrices and Vectors for SD Method from a Dataframe Containing 
#' Data Results from Soccer Matches
#' 
#' This function creates data matrices and vectors (for sum and differences) 
#' from a dataframe containing results of soccer matches.
#'
#' @param data a data frame containing soccer matches database, where each line
#'             (observation) represent a soccer match, first variable must be 
#'             Home team, second variable must be number of goals achieved by 
#'             Home team, third variable must be number of gols achieved by 
#'             Visitor team, fourth must be the Visitor team, and the fifth 
#'             must indicate if there was a filed factor in the match (
#'             represented by 0 for 'no' or 1 for 'yes')
#'    
#' @return A list with two vectors: sum and dif; and two matrices: matS and matT.
#' 
#' @export
#'
# @examples
#' 
crMatrixSD <- function(data) {

  ## Number of matches
  nM <- dim(data)[1]
  
  ## Picking teams names in alphabetical order
  teams = sort(union(data[,1], data[,4]))
  nT <- length(teams) # Number of teams

  ## Calculating goals sum and diference
  sumG <- data[,2] + data[,3]
  difG <- data[,2] - data[,3]
  
  ## Creating S matrix
  matS <- matrix(0, nrow = nM, ncol = nT)
  for (i in 1:nM) {
    idx <- which(teams %in% data[i, c(1, 4)])
    matS[i, idx] <- 1
  }
  matS <- cbind(matS, data[,5])
  colnames(matS) <- c(teams, "Local")
  
  ## Creating T matrix
  matT <- matrix(0, nrow = nM, ncol = nT)
  for (i in 1:nM) {
    idxM <- which(teams %in% data[i, 1])
    idxV <- which(teams %in% data[i, 4])
    matT[i, idxM] <- 1
    matT[i, idxV] <- -1
  }
  matT <- cbind(matT, data[, 5])
  colnames(matT) <- c(teams, "Local")
  
  ## Returning a list
  res <- list(sum = sumG, dif = difG, matS = matS, matT = matT)
  return(res)

}