best <- function(state, outcome){
  outcomeFile <- read.csv("outcome-of-care-measures.csv",header = T, colClasses="character")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(outcomeFile[,7])
  
  if(!(outcome %in% outcomes)) stop("invalid outcome")
  
  if(!(state %in% states)) stop("invalid state")
  
  if (outcome == outcomes[1]){
    outcomeFile[,11] <- as.numeric(outcomeFile[,11])
    heartAttacks <- outcomeFile[,c(2,7,11)]
    colnames(heartAttacks) <- c("Name", "State", "HeartAttack")
    heartAttacksOmit <- na.omit(heartAttacks)
    heartAttacksOmit <- heartAttacksOmit[ which(heartAttacksOmit$State == state),]
    rowNumber <- which.min(heartAttacksOmit$HeartAttack) 
    return(heartAttacksOmit[rowNumber,]$Name)
    
  }else if (outcome == outcomes[2]){
    outcomeFile[,17] <- as.numeric(outcomeFile[,17])
    heartFailures <- outcomeFile[,c(2,7,17)]
    colnames(heartFailures) <- c("Name", "State", "HeartFailure")
    heartFailuresOmit <- na.omit(heartFailures)
    heartFailuresOmit <- heartFailuresOmit[which(heartFailuresOmit$State == state),]
    rowNumber <- which.min(heartFailuresOmit$HeartFailure)
    return(heartFailuresOmit[rowNumber,]$Name)
    
  }else if (outcome == outcomes[3]){
    outcomeFile[,23] <- as.numeric(outcomeFile[,23])
    pneumonia <- outcomeFile[,c(2,7,23)]
    colnames(pneumonia) <- c("Name", "State", "Pneumonia")
    pneumoniaOmit <- na.omit(pneumonia)
    pneumoniaOmit <- pneumoniaOmit[which(pneumoniaOmit$State == state), ]
    rowNumber <- which.min(pneumoniaOmit$Pneumonia)
    return(pneumoniaOmit[rowNumber,]$Name)
  }
  
}
