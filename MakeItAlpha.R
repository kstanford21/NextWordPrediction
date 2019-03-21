# take ngram files and splits them alphabetically and writes into .csv files

function(data, version, n, goodWords, stats = TRUE){
     require(data.table)
     
     alpha <- letters              #list of letters (a-z)
     ngSplit <- data.table()       #temporary data.table to hold ngrams starting with each letter
     frequency <- list()           #store frequencies of ngrams starting with each lettter
     top01 <- list()               #store top 1% of ngrams starting with each letter
     
     #creates list of aphabetized ngrams
     for(i in 1:length(alpha)){ 
          ngSplit <- data.table()
          
          #grab ngrams starting with the ith letter
          for(j in 1:length(data)){     #loop for each ngram list in data
               #extract ngrams starting with alpsa[[i]
               rowlist <- grep(paste("^", alpha[[i]], sep = ""), data[[j]]$ng) 
               
               #if ngrams extracted
               if(length(rowlist >=1)){ 
                    temp <- data[[j]][rowlist,]
                    
                    #last word of ngram
                    B<- word(temp$ng, start = -1, end = -1, sep = " ")
                    #ngram without last word
                    A<- word(temp$ng, start = 1, end = -2, sep = " ")  
                    
                    #bind part A and B
                    temp <- cbind(A, B, temp) 
                    temp <- temp[,c("A","B", "Freq")]
                    
                    #bind to ngram data.table
                    ngSplit <- rbind(ngSplit, temp)   
                    colnames(ngSplit) <- c("A","B", "Freq")
               }
          }
          
          #aggregate data to get combined frequency if enough lines
          if(nrow(ngSplit >= 3)){
               ngSplit <- aggregate(ngSplit$Freq, by=list(ngSplit$A, ngSplit$B), FUN=sum, na.rm = TRUE)
               colnames(ngSplit) <- c("A","B", "Freq")
          }
          
          #if the list goodWords exists, only keep ngrams where B is in this list
          if(!is.null(goodWords)){ 
               ngSplit<- ngSplit[ngSplit$B %in% goodWords, ]
          }
          
          
          #if stats is TRUE collect data for summary
          if(stats == TRUE){
               #generate list of top 1% of ngrams starting with each letter
               cutoff <- quantile(ngSplit$Freq, probs = .99)
               top01[[i]]<- subset(ngSplit, Freq > cutoff)
               
               #frequency of ngrams starting with each letter
               frequency[[i]] <- ngSplit$Freq
          }
          #write alphabetized ngram file
          write.csv(ngSplit, paste0(version, "/ngram/ngram", n, "/", alpha[[i]], "_ngram.csv"), 
                    row.names = FALSE)  
     } 
     
     #prints statement to show progress
     print(paste0(n, "-grams done!"))   
     
     #if generating summary, return them, if not return string saying "complete"
     #if not generating summary, returns string if stats not being collected, shows progress
     if(stats == TRUE){
          ngData <- list(frequency, top01)
          return(ngData)
     }else{
          ngData<- "complete" 
          return(ngData)
     }
}