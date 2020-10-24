#' Creat a Data Matrix from a XLS File.
#' 
#' This function creates a data matrix from a spreadsheet file containing 
#' soccer matches results.
#'
#' @param xlsFile the spreadsheet file name to be loaded.
#' @param ... additional parameters for read.xls function (gdata package).
#'
#' @return A list with two vectors: sum and dif; and two matrices: matS and matT.
#' 
#' @export
#'
#' @examples
#' 
crMatrixSD <- function(xlsFile, ...) {
 
  ## Reading matches information
  matches <- gdata::read.xls(xlsFile, as.is = TRUE, ...)
  attach(matches, name = "DF_socceR")
  on.exit(detach("DF_socceR"))
  
  nM <- dim(matches)[1] # Number of matches
  
  ## Picking teams names in alphabetical order
  teams = sort(union(Home, Visitor))
  nT <- length(teams) # Number of teams

  ## Calculating goals sum and diference
  XpY <- X + Y
  XmY <- X - Y
  
  ## Creating S matrix
  matS <- matrix(0, nrow = nM, ncol = nT)
  colnames(matS) <- teams
  for (i in 1:nM) {
    idx <- which(teams %in% matches[i, c(1, 4)])
    matS[i, idx] <- 1
  }
  matS <- cbind(matS, Local)
  
  ## Creating T matrix
  matT <- matrix(0, nrow = nM, ncol = nT)
  colnames(matT) <- teams
  for (i in 1:nM) {
    idxM <- which(teams %in% Home[i])
    idxV <- which(teams %in% Visitor[i])
    matT[i, idxM] <- 1
    matT[i, idxV] <- -1
  }
  matT <- cbind(matT, Local)
  
  ## Returning a list
  res <- list(sum = XpY, dif = XmY, matS = matS, matT = matT)
  return(res)

}