#' CaptiveIndex Function
#'
#' This function calculates the sociality measure using the data created from the DataCreate Function.
#' @param x is the output dataframe from the /code{DataCreate} function
#' @param Weight.1 Input Nearest Neighbour Weight from /code{Weights} function
#' @param Weight.2 Input Interaction Distance Weight from /code{Weights} function 
#' @keywords CaptiveIndex, association index, weights
#' @export
#' @examples
#' data(giraffe)
#' W<-Weights (giraffe)
#' NN.ID <- DataCreate (giraffe)
#' head (NN.ID)
#' Results<-CaptiveIndex (NN.ID, W$NN, W$IDist)
#' head (Results)

CaptiveIndex<-function(x, Weight.1, Weight.2){

  pairs <- data.frame(pair1 = paste(substring(x$Subject,1,last=1000000),substring(x$Partner,1,last=1000000), sep="-"),
                      pair2 = paste(substring(x$Partner,1,last=1000000),substring(x$Subject,1,last=1000000), sep="-"))
  n1 <- vector()
  n2<- vector ()

  for(i in 1:nrow(pairs)){
    #      n1= (NN/N +NN/N)
    n1[i]<- (x$NN[x$Dyad==pairs[i,1]]/x$N[x$Dyad==pairs[i,1]]) + (x$NN[x$Dyad==pairs[i,2]]/x$N[x$Dyad==pairs[i,2]])
    n2[i]<- (x$IDist[x$Dyad==pairs[i,1]]/x$N[x$Dyad==pairs[i,1]]) + (x$IDist[x$Dyad==pairs[i,2]]/x$N[x$Dyad==pairs[i,2]])
  }
  x<- cbind(x,n1, n2)

  CaptiveIndex1 <- ((x$n1 * Weight.1) + (x$n2 * Weight.2)) / 2 ### Neareast neighbour and interaction distance
  CaptiveIndexResults<-as.data.frame(cbind(x, CaptiveIndex1))

  CaptiveIndexResults[is.na(CaptiveIndexResults)] <- 0
  return ((CaptiveIndexResults))
  }


