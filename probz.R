# calculate stupid backoff and Knesner-Ney smoothing probabilities of the last word in each ngram
function(file, d, version, nwords, db){
     require(data.table)
     require(dplyr)
     require(RSQLite)
     
     #list files in folder for specified ngram
     filelist <- list.files(path= paste0(version, "/ngram/ngram", file), pattern="*.csv")
     #special instructions for unigram probabilities
     if(file ==1){
          filelist <- list.files(path= paste0(version, "/ngram/ngram1"), pattern="*.csv")
          for(i in 1:length(filelist)){
               #read in ngram file
               temp <- fread(file = paste0(version, "/ngram/ngram1", "/", filelist[[i]]), 
                             stringsAsFactors = FALSE)
               temp$prob <- temp$Freq/nwords
               dbWriteTable(conn=db, name = paste0("ngram", file), temp, append=T, row.names=F)
          }
     }else{
          uniqueA <- list()   #unique instances of part A
          uniqueB <- list()   #unique instances of part B (word to be guessed)
          countA <- list()    #overall count of part A
          countB <- list()    #overall count of partB
          nNgram <- 0         #number of uniquengrams
          alpha <- letters    #list of letters
     
          #count unique values for part A and B
          #count overall frequency for part A and B
          #do loop for each file (ngrams separated by first letter)
          for(i in 1:length(alpha)){
               #read in ngram file
               temp <- fread(file = paste0(version, "/ngram/ngram", file, "/", filelist[[i]]), 
                        stringsAsFactors = FALSE)
               #count unique instances of part A and B
               uniqueA[[i]] <- data.table(table(temp$A))
               colnames(uniqueA[[i]]) <- c("A", "uniqueA")  
               uniqueB[[i]] <- data.table(table(temp$B))
               colnames(uniqueB[[i]]) <- c("B", "uniqueB")
          
               #count instances of A and B, place data.table in ordered list
               countA[[i]] <- data.table(cbind(temp$A, temp$Freq))
               colnames(countA[[i]]) <- c("A", "Freq")
               countA[[i]]$Freq <- as.numeric(countA[[i]]$Freq)
               countA[[i]] <- aggregate(countA[[i]]$Freq, by=list(Category = countA[[i]]$A), 
                                   FUN=sum, na.rm = TRUE) 
               colnames(countA[[i]]) <- c("A", "countA")
          
               #aggregated count of partB
               countB[[i]] <- data.table(cbind(temp$B, temp$Freq))
               colnames(countB[[i]]) <- c("B", "Freq")
               countB[[i]]$Freq <- as.numeric(countB[[i]]$Freq) 
               countB[[i]] <- aggregate(countB[[i]]$Freq, by=list(Category = countB[[i]]$B), 
                                   FUN=sum, na.rm = TRUE) 
               colnames(countB[[i]]) <- c("B", "countB") 
          
               #count of unique ngrams
               nNgram <- nNgram + nrow(temp)
          }  
     
          #aggregates count A by alphabetic file (make sure duplicates dealt with)
          #takes count A and unique A
          #aggregates and merges
          allA <- list() #all A data
          for(i in 1:length(alpha)){
               countAtemp <-aggregate(countA[[i]]$countA, by=list(Category = countA[[i]]$A), 
                                 FUN=sum, na.rm = TRUE)
               colnames(countAtemp) <- c("A", "countA")
          
               uniqueAtemp <-aggregate(uniqueA[[i]]$uniqueA, by=list(Category = uniqueA[[i]]$A), 
                                  FUN=sum, na.rm = TRUE)
               colnames(uniqueAtemp) <- c("A", "uniqueA")
          
               allA[[i]] <- data.table(merge(countAtemp, uniqueAtemp, by = "A"))
               colnames(allA[[i]])<- c("A", "countA", "uniqueA")
          }
     
          #list of alphabetic B lists by file
          countB <- makeUnigramsAlpha(countB, 0)
          uniqueB <- makeUnigramsAlpha(uniqueB, 0)
     
          #combine uniqe instances and counts of part B
          allB <- list()    
          for(i in 1:26){
               colnames(countB[[i]]) <- c("B", "countB")
               colnames(uniqueB[[i]]) <- c("B", "uniqueB")
               allB[[i]] <- merge(uniqueB[[i]], countB[[i]], by = "B")
               colnames(allB[[i]]) <- c("B", "countB", "uniqueB")
          }
     
          #for each ngram file calculates Kneser-Ney smoothing and stupid backoff probabilities
          #writes files with probabilities into "final" folder
          for(i in 1:length(alpha)){ 
               #read in ngrams
               temp <- fread(file = paste0(version, "/ngram/ngram", file, "/", filelist[[i]]), 
                        stringsAsFactors = FALSE)
               #get b values that are in the individual ngram file
               #with large datasets, using all B values results in memory issues
               bindB <- data.table()
               for(q in 1:length(allB)){
                    tempB <- allB[[q]]
                    tempB <- tempB[tempB$B %in% temp$B,]
                    bindB <- rbind(bindB, tempB)
               } 
          
               #join ngram file with counts for part B
               temp <- merge(temp, bindB, by = "B", all = FALSE)
               #join with A counts
               A <- allA[[i]]
               temp <- data.table(merge(temp, allA[[i]], by = "A", all.x = TRUE, all.y = FALSE))

               #Kneser-Ney smoothing and stupid backoff probabilities
               temp <- temp %>% 
                    rowwise() %>% 
                    mutate(prob= max((Freq - d), 0)/countA + (d * countA/countB) * (uniqueB/nNgram), 
                         stupid = Freq/countA)
          
               #this line returns top 5 most likely ngrams starting with each part A
               #can be used to reduce the size of files when generating models
               #temp <- setorder(setDT(temp), A, -prob)[, indx := seq_len(.N), A][indx <= 5L]

               #write sql database by ngram 
               dbWriteTable(conn=db, name=paste0("ngram", file), temp, append=T, row.names=F)
          }
     }

}