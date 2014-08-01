best <- function(state, outcome){
  outcomeFile <- read.csv("outcome-of-care-measures.csv",header = T, colClasses="character")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(outcomeFile[,7])
    
  if(!(outcome %in% outcomes)) stop("invalid outcome")
  
  if(!(state %in% states)) stop("invalid state")
  
  if (outcome == outcomes[1]){
    outcomeFile[,11] <- as.numeric(outcomeFile[,11])
    heartAttacks <- outcomeFile[,c(2,11)]
    heartAttacksOmit <- na.omit(heartAttacks)
    bestH <- lapply(heartAttacksOmit,min)
    
    return(unlist(bestH[1][1]))
  }
  
  if (outcome == outcomes[2]){
    outcomeFile[,17] <- as.numeric(outcomeFile[,17])
    heartFailures <- outcomeFile[,c(2,17)]
    heartFailuresOmit <- na.omit(heartFailures)
    bestH <- lapply(heartFailuresOmit,min)
    return(unlist(bestH[1][1]))
  }
  
  if (outcome == outcomes[3]){
    outcomeFile[,23] <- as.numeric(outcomeFile[,23])
    pneumonia <- outcomeFile[,c(2,23)]
    pneumoniaOmit <- na.omit(pneumonia)
    bestH <- lapply(pneumoniaOmit,min)
    
    return(unlist(bestH[1][1]))
  }

}
