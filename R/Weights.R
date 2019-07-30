#' Weights function
#'
#' This function calculates the weights for the proximity conditions
#' @param x is the dataframe matching the example using the appropriate column headers (see details)
#' @keywords nearest neighbour, proximity conditions
#' @export
#' @examples Weights (giraffe)

Weights <-function(x){

Total.Obs<-nrow(x)
Nearest.Obs<-sum(x$N.N !="0")
CP.Obs<-sum(x$C.P !="0")

NN.Weight <-1-(Nearest.Obs/Total.Obs)
CP.Weight <-1-(CP.Obs/Total.Obs)


cat ("Nearest Neighbour Weight: ",NN.Weight, "Interaction Distance Weight: ", CP.Weight)
W <-list ("NN" = NN.Weight, "IDist" = CP.Weight)
}

