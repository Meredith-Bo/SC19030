#' @title Finding the optimal numerber of Beta distribution mixture
#' @name mOpt.CN
#' @description A function to calculate the optimal number of weights
#' @param tdata A transformation original data (vector)
#' @param L the maximum of the estimation region (matrix)
#' @return number of optimal weights
#' @examples
#' \dontrun{
#' data(windspeed)
#' tdata<-windspeed$H10
#' n <- length(tdata)
#' Fn <- ecdf(tdata)
#' ep <- 3/(8*length(data))	
#' ecdf.vec <- Fn(tdata)	
#' L <- diag( n / ( ( ecdf.vec + ep)*(1 + ep - ecdf.vec ) ) )
#' mOpt.CN(tdata,L)
#' }
#' @export
mOpt.CN <- function(tData, L){
  
  #set starting point for m
  n <- length(tData)
  m <- floor(  n^(1/3) ) - 1  
  
  #set start value for while loop
  logratio <- 1
  
  while( logratio < sqrt(n) ){
    m <- m+1
    
    #construct D matrix using m value 
    B <- NULL
    for(k in 1:m){
      B <- cbind(B, pbeta(tData, shape1=k, shape2=m-k+1))
    }
    Dmat <- t(B) %*% L %*% B
    
    #take spectral decomposition to find eigenvalues
    spec <- eigen(Dmat, symmetric=TRUE)
    d <- spec$values
    min.eigenValue <- max( min(d), 0 )
    max.eigenValue <- max(d)
    
    logratio <- log10(max.eigenValue) - log10(min.eigenValue)		
  }	
  m-1 #return number of weights
}