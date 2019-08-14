#' DataCreate function
#'
#' This function allows you to create a dataset to be used in the captive association index
#' @param x is the dataframe matching the example using the appropriate column headers (see details)
#' @keywords Captive Index, Association index
#' @export
#' @examples
#'NN.ID <- DataCreate (giraffe)
#'head (NN.ID)

DataCreate<-function(x, Measures=NULL){
  ### Identify subject names
  Subject.Names <- unique(x$Subject)
  res <- data.frame()
  res.all <- data.frame()
  ########
  ## Organising the data

  for(i in seq_along(Subject.Names)){
    # Make data frame for each subject in turn ie gir.names[i], when i == 1, then it's Gerty.
    dat1 <- subset(x, Subject == Subject.Names[i]) # Add THorn and Location... here in the subset function for additional criteria
    Partner <- vector()
    Subj <- vector()
    N <- vector()
    NN <- vector()
    CP <- vector()

    # loop two
    for(j in seq_along(Subject.Names)){

      # just to stop same name in dat2
      if(j == i) next

      # get data set when each giraffe is present with subject from dat1
      dat2 <- dat1[grepl(Subject.Names[j], dat1$Present),] #

      Subj[j] <- Subject.Names[i]
      Partner[j] <- Subject.Names[j]

      N[j] <- nrow(dat2)

      # Get nearest neighbour numbers
      NN[j] <- sum(dat2$N.N == Subject.Names[j]) # should be same as sum(dat1$N.N == gir.names[j])

      # Check that where named NN is also in present column
      #print(sum(dat2$N.N == gir.names[j]) == sum(dat1$N.N == gir.names[j]))
      if(!sum(dat2$N.N == Subject.Names[j]) == sum(dat1$N.N == Subject.Names[j])) {
        print(paste("Named NN is not in present column or vice versa for", Subject.Names[i], "and", Subject.Names[j]))}

      # Get CP
      CP[j] <- sum(dat2$C.P == Subject.Names[j])

    }

    tmp <- data.frame(Subject = Subj, Partner = Partner, N = N, NN = NN, IDist=CP)

    res <- rbind(res, tmp)
  }
  res.all<-na.omit(res) # check no NAs in other rows..
  res.all$Dyad <- paste(substring(res.all$Subject, 1,last=1000000),substring(res.all$Partner,1, last=1000000), sep="-")
  return(res.all)
}
