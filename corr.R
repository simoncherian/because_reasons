corr <- function (directory, threshold = 0) {
        
        #x <- complete(directory)
        files_full <- list.files(directory, full.names = T)
        corNS <- c()
        for(i in seq_along(files_full)){
                id_data<- read.csv(files_full[i])
                nobs <- sum(complete.cases(id_data))
                if(nobs >= threshold){
                        corr <- cor(id_data$nitrate, id_data$sulfate, use = "pairwise.complete.obs")
                        corNS <- c(corNS, corr)
                }
        
        }
        corNS
}