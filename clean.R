#clean data
function(string, remove) {
     require(tm)
     require(textclean)
     #sometimes imports ' as <92>; changes to '
     temp <- gsub("<92>", "'", string)
     #expands contractions
     temp <- replace_contraction(temp)            
     #replace contractions misses "who're" and a couple others
     #also misses nouns followed by 's for is, but this is also posessive and "is" carries little info
     #fixes remaining contractions
     temp <- gsub("'re ", " are ", temp)
     #remove everything but letters  
     temp <- gsub("([^A-Za-z ])+", "", x = temp) 
     #convert to lower
     temp <- tolower(temp)   
     #remove words with letters repeated 3 or more times
     temp <- gsub("(\\w)\\1{2, }", " ", temp) 
     #remove repeated words
     temp <- gsub("(\\b\\w+\\b)(\\s+\\1)+", " ", temp)
     #remove single and letter words
     temp <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", temp) 
     #remove double letter words
     #temp <- gsub(" *\\b[[:alpha:]]{2}\\b *", " ", temp) 
     #remove words longer than 17 characters
     temp <- gsub("\\b[[:alpha:]]{17,}\\b", "", temp)      
     
     #remove words in list from corups
     for(w in 1:length(remove)){
          temp <- removeWords(temp, remove[[w]])  
     }
     
     #final cleaning of whitespace and punctuation
     temp <- removePunctuation(temp)
     temp <- stripWhitespace(temp)
     temp <- trimws(temp)
     
     return(temp)
}