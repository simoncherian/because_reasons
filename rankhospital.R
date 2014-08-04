rankhospital <- function(state, outcome, num="best"){
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% outcomes)) stop("invalid outcome")
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  states <- outcomeFile[,7]
  if(!(state %in% states)) stop("invalid state")
  if(num != "best" & num !="worst" & num < 1 & as.numeric(num)%%1 != 0) stop("invalid num")
  
  if(outcome == outcome[1]){
    outcomeFile[,11] <- as.numeric(outcomeFile[,11])
    heartAttacks <- outcomeFile[,c(2,7,11)]
    colnames(heartAttacks) <- c("Name", "State", "HeartAttack")
    heartAttacksOmit <- na.omit(heartAttacks)
    heartAttacksOmit <- heartAttacksOmit[ which(heartAttacksOmit$State == state),]
    heartAttacksFinal <- heartAttacksOmit[order(heartAttacksOmit$Name),]
    heartAttacksFinal <- heartAttacksFinal[order(heartAttacksFinal$HeartAttack),]
    if(num =="best" | num == "1") rowNumber <- which.min(heartAttacksFinal$HeartAttack)
    else if (num == "worst") rowNumber <- which.max(heartAttacksFinal$HeartAttack)
    else rowNumber <- num
    return(heartAttacksFinal[as.numeric(rowNumber),]$Name)
    
    }else if (outcome == outcomes[2]){
    outcomeFile[,17] <- as.numeric(outcomeFile[,17])
    heartFailures <- outcomeFile[,c(2,7,17)]
    colnames(heartFailures) <- c("Name", "State", "HeartFailure")
    heartFailuresOmit <- na.omit(heartFailures)
    heartFailuresOmit <- heartFailuresOmit[which(heartFailuresOmit$State == state),]
    heartFailuresFinal <- heartFailuresOmit[order(heartFailuresOmit$Name),]
    heartFailuresFinal <- heartFailuresFinal[order(heartFailuresFinal$HeartFailure),]
    if(num =="best" | num == "1") rowNumber <- which.min(heartFailuresFinal$HeartFailure)
    else if (num == "worst") rowNumber <- which.max(heartFailuresFinal$HeartFailure)
    else rowNumber <- num
    return(heartFailuresFinal[as.numeric(rowNumber),]$Name)
    
  }else if (outcome == outcomes[3]){
    outcomeFile[,23] <- as.numeric(outcomeFile[,23])
    pneumonia <- outcomeFile[,c(2,7,23)]
    colnames(pneumonia) <- c("Name", "State", "Pneumonia")
    pneumoniaOmit <- na.omit(pneumonia)
    pneumoniaOmit <- pneumoniaOmit[which(pneumoniaOmit$State == state), ]
    pneumoniaFinal <- pneumoniaOmit[order(pneumoniaOmit$Name),]
    pneumoniaFinal <- pneumoniaFinal[order(pneumoniaFinal$Pneumonia)]
    if(num =="best" | num == "1") rowNumber <- which.min(pneumoniaFinal$Pneumonia)
    else if (num == "worst") rowNumber <- which.max(pneumoniaFinal$Pneumonia)
    else rowNumber <- num
    return(pneumoniaFinal[as.numeric(rowNumber),]$Name)
    
  }
  
  
}
