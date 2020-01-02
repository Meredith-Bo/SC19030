#' @title Finding the weight vector
#' @name solveWeights
#' @description A function to solve for the weight vector
#' @param m the number of weights
#' @param Fn empirical cdf
#' @param lower the left boundary
#' @param upper the right boundary
#' @param Dmat some constraint matrix for calculating
#' @param dvec some constraint vector for calculating
#' @return a vector of weights
#' @examples
#' \dontrun{
#' data(windspeed)
#' tdata<-windspeed$H10
#' m<-7
#' ep <- 3/( 8*length(tdata) )	
#' Fn<-ecdf(tdata)
#' ecdf.vec<-Fn(tdata)
#' L <- diag( n / ( ( ecdf.vec + ep)*(1 + ep - ecdf.vec ) ) )
#' B <- NULL
#' for(k in 1:m){
#'  B <- cbind(B, pbeta(tdata, shape1=k, shape2=m-k+1))
#' }
#' Dmat <- t(B) %*% L %*% B
#' dvec <- t(B)%*% L %*% ecdf.vec
#' solveWeights(m,Fn,Dmat,dvec)
#'}
#' @export
solveWeights <- function(m, Fn, lower, upper, Dmat, dvec){	
  #find the location of the maximum weight
  max.place <- maxWeight(m, Fn, lower, upper)  ## return index
  #make the constraint matrix
  Amat <- constraintMat(m, max.place)	
  #make bvec vector of constraints
  bvec=c(1,rep(0,2*m-1))
  #find weights using solve.QP function
  w.hat = solve.QP(Dmat,dvec,Amat,bvec,meq=1)$solution   ## solve a quadratic programming problem
  #function to find max of an element and 0
  max0 <- function(x){max(x,0)}
  #make sure no weights are < 0
  w.hat <- sapply( w.hat, max0)
  #make sure the weights sum to 1
  wsum <- sum(w.hat)
  w.hat <- w.hat / wsum
  w.hat
}	