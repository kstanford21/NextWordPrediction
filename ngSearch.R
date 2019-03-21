
#search SQL database for most likely next words
#return possible next words as data frame
#given search string, list of tables to search, 
     #postion of start word(first word in ngram), 
     #position of last word and database to search
function(string, searchlist, start, end, db){
     require(RSQLite)
     require(stringr)
     require(dplyr)
     datalist <- NULL
     #if the string has more than 1 word
     if (length(string) > 1){
          #ngram files to search (omit larger ngram files if string is short)
          searchlist2 <- searchlist[2:(end-start +1)]
          #for each ngram file
          for(j in 1:length(searchlist2)){
               #get the search string
               tempStart <- max(end-j+1, 1)
               tempWords <- str_c(string[tempStart:end],collapse = " ")
               #get table to search
               file <- searchlist2[[j]]
               # Query database and get results
               query <- paste0("SELECT * FROM ", file, " WHERE A LIKE '%", tempWords, "%'")
               query <- paste0("SELECT * FROM ", file, " WHERE A = '", tempWords, "'")
               results <- dbGetQuery(db, query)
               datalist <- rbind(datalist, results)
          }
     }
     #order by probability
     #ties settled by count of the guessed word
    if(!is.null(datalist)){
     datalist <- arrange(datalist, desc(prob), desc(countB))
    }

     return(datalist)
}