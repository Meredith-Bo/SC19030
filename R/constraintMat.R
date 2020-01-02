#' @title Generating the costraint matrix
#' @name constraintMat
#' @description A function to generate the constraint matrix
#' @param m number of the weights
#' @param maxplace the index of the maximum weight
#' @return the constraint matrix
#' @examples
#' \dontrun{
#' data(windspeed)
#' tdata<-windspeed$H10
#' m<-7
#' maxplace<-maxWeight(m,Fn)
#' constranintMat(m,maxplace)
#' }
#' @export
#Function to generate the constraint matrix 
constraintMat <- function( m, maxplace){
  A <- suppressWarnings(   
    rbind( rep(1,m), diag(rep(1,m)), 
           matrix( rep( c(-1,1, rep(0,m - 1)) , maxplace-1), maxplace-1, m, byrow=TRUE), 
           matrix( rep( c( rep (0,maxplace-1),1,-1,rep(0, m-maxplace)),m-maxplace),
                   m-maxplace,m,byrow=TRUE))
  )
  Amat <- t(A)
  Amat
}