#' @title Finding the index of the maximum weight
#' @name maxWeight
#' @description A function to determine the index of the maximum weight
#' @param m number of the weights
#' @param Fn emiprical cdf
#' @param lower the left boundary
#' @param upper the right boundary
#' @return the index of the maximum weight
#' @examples
#' \dontrun{
#' data(windspeed)
#' tdata<-windspeed$H10
#' Fn<-ecdf(tdata)
#' m<-7
#' maxWeight(m,Fn)
#' }
#' @export
maxWeight <- function(m, Fn, lower, upper){
  #find max of weights
  maxplace <- which.max( Fn( ((1:m)/m)) - 
                           Fn( ((0:(m-1))/m) )  )
}