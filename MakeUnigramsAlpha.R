# take ngram files and splits them alphabetically
function(data, version){
     require(data.table)
     alpha <- letters
     ngSplit <- list()
     
     #extracts unigrams starting with each letter and writes to file
     #returns list of ngrams
     for(i in 1:length(alpha)){
          tempAlpha <- data.table()
          
          #grab ngrams starting with the ith letter
          for(j in 1:length(data)){
               colnames(data[[j]]) <- c("ng", "Freq") 
               temp <- data[[j]][grep(paste("^", alpha[[i]], sep = ""), data[[j]]$ng),]
               tempAlpha <- rbind(tempAlpha, temp)
          }
          
          #place data table in a list
          ngSplit[[i]] <- tempAlpha
          ngSplit[[i]] <- aggregate(ngSplit[[i]]$Freq, by=list(Category = ngSplit[[i]]$ng), 
                                    FUN=sum, na.rm = TRUE)
          colnames(ngSplit[[i]]) <- c("ng", "Freq")
          
          #if given a version
          if(version !=0){
               write.csv(ngSplit[[i]], paste0(version, "/ngram/ngram1/", alpha[[i]], "_ngram.csv"),
                         row.names = FALSE)
          }
     }
     return(ngSplit)
}