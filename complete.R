complete <- function(directory, id=1:332){
        
        files_full <- list.files(directory, full.names = T)
        nobs = 0
        table <- data.frame(id=numeric(), nobs = numeric())
        for (i in id){
                id_data <- read.csv(files_full[i])
                id_nobs <- sum(complete.cases(id_data))
                table <- rbind (table, data.frame(i,id_nobs))
        }
        names(table)<-c("id","nobs")
        table
}
