pollutantmean <- function(directory, pollutant, id = 1:332){
        wd <- paste('~/', directory, sep="")
        setwd(wd)
        sumID = 0
        obs = 0
        meanID = 0
        if (pollutant == "sulfate") {
                for(i in id){
                        if(i<10){
                                csv <- paste("00",as.character(i),".csv",sep="")
                                readCSV <- read.csv(csv, header=T)
                                sumID = sumID + sum(readCSV$sulfate, na.rm=T)
                                obs = obs + length(readCSV$sulfate[!is.na(readCSV$sulfate)])
                        } else if (i < 100 && i > 9) {
                                csv <- paste("0", as.character(i), ".csv", sep="")
                                readCSV <- read.csv(csv, header=T)
                                sumID = sumID + sum(readCSV$sulfate, na.rm=T)
                                obs = obs + length(readCSV$sulfate[!is.na(readCSV$sulfate)])
                                
                        } else if(i> 99 ) {
                                csv <- paste(as.character(i),".csv", sep="")
                                readCSV <- read.csv(csv, header = T)
                                sumID = sumID + sum(readCSV$sulfate, na.rm=T)
                                obs=obs+length(readCSV$sulfate[!is.na(readCSV$sulfate)])
                        }
                }
        
                meanID <- sumID/obs
        } else if (pollutant == "nitrate") {
                for(i in id){
                        if(i<10){
                                csv <- paste("00",as.character(i),".csv",sep="")
                                readCSV <- read.csv(csv, header=T)
                                sumID = sumID + sum(readCSV$nitrate, na.rm=T)
                                obs = obs + length(readCSV$nitrate[!is.na(readCSV$nitrate)])
                        } else if (i < 100 && i > 9) {
                                csv <- paste("0", as.character(i), ".csv", sep="")
                                readCSV <- read.csv(csv, header=T)
                                sumID = sumID + sum(readCSV$nitrate, na.rm=T)
                                obs = obs + length(readCSV$nitrate[!is.na(readCSV$nitrate)])
                                
                        } else if(i> 99 ) {
                                csv <- paste(as.character(i),".csv", sep="")
                                readCSV <- read.csv(csv, header = T)
                                sumID = sumID + sum(readCSV$nitrate, na.rm=T)
                                obs=obs + length(readCSV$nitrate[!is.na(readCSV$nitrate)])
                        }
                }
                
                meanID <- sumID/obs
        }

        meanID
}
