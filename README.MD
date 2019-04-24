---
title: "Next Word Prediction Model Generation"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Purpose
The files here use a given text corpus to create a next word prediction model and tests the model.  Use of the code is well documented in both NgramGeneration.RMD and EvaluteModel.RMD.  To generate the next word prediction model, simply run the RMD files.  This process generated an next word prediction model with 18% accuracy on the testing data.  However, the options as is were not the ones used to generate this model.  The reason for this being that the data cleaning and model generation takes a considerable amount of time when run on a large corpus.  Settings as is use a smaller portion of the corpus to make model generation faster.  Some amount of tweaking of training data size and chunk size is necessary to avoid running out of memory.  Some tweaking (not in code shown here) was performed to make the ngram files smaller and improve accuracy.  

The key bennefit of using these RMD files in generating the next word model is that it is fully reproducable, easily modified, chunks data to alow for processing of large files, and saves models in an organized fashion in a folder for each version for the model.  The flexibility of the NgramGeneration.RMD is due to customizable options in the "options" chunk.  These options allow for selecting the files used, version, ngrams to make (ex: up to 4grams or up to 3grams), and words to remove from the corpus.  A second bennefit is that both NgramGeneration.RMD and EvaluateModel.RMD automatically generates some summary statistics and graphs when run.  Conbined, the flexibility, organized data production, and automated generation of summaries allows for identifying methods to improve the model including changing the words removed, how the corpus is cleaned and the method by which probabilities are calculated.

## Running Code  
To run the files here, first check your working directory and set it to the location where you would like your ngram model stored.  The .txt files used must also be placed in this directory (if not here, use a file path to specify their location).  The NgramGeneration.RMD contains all the code necessary to generate a next word prediction model and EvaluateModel.RMD contains all the code to test this model.  

It is important to remember that running these files to generate a model takes a considerable amount of time and memory (time to hit that knit button and go to bed).  Data is separated into chunks to allow for processing of these large files without running out of memory.  Additionally, to improve performance of the ngram generation code, data.tables were used instead of data.frames.  In testing this code, I recommend using small portions of the training data to generate the model and small portions of the test data to evaluate its performance.  While I have used this to test for ngrams of up to 6-grams, longer take longer to generate and reqire more memory often with little benefit to model performance.  

While some modifications to the code are built-in, others can be added.  These modifications include (but are not limited to) increasing the likelihood of words with certain frequencies (done in probz function), skipgrams, and using word stemming (cleaning function, or later in processing).  It is also possible to use a method for Ngram generation other than the RWeka tokenizer, but keep in mind that the outputs from other methods differ and will alter later processing steps.  

If deploying the model as an app, processing speed is of great importance.  Therfore additional modifications of the model are recommended.  These modifications include implementing feature hashing and retaining only the top probablities for each part "A" (each set of beginning word(s) can lead to many possible guessed words).  These modifications significantly improve processing speed while maintaning model accuracy.  

## Files
Below is a description of the RMD files used to run the code and the custom functions they call.  

Files used to run code:  
NgramGeneration.RMD:  Generates next word prediction model  
EvaluateModel.RMD:  Evaluates the next word preciction model  

Functions:
clean:  
-cleans the data 
  
makeUnigramsAlpha.R: 
-Generates alphabetized unigrams 
-Saves each unigram and its frequency as a .csv in "version/ngram/1gram"
-Files saved by the starting letter of the word (ex: "a_ngram.csv")
-Returns a list of 26 data.tables (alphebetized) which can be used for further analysis and pruning vocabulary

MakeItAlpha:  
-Generates alphabetized 2 through n grams (n specified through options as "ngrams" in NgramGeneration.Rmd)  
-Splits last word ("B") from the rest of ngram ("A"), and saves them in separate columns  
-Saves each ngram and its frequency as a .csv file in folder for the ngram (ex: "version/ngram/3gram")  
-Files saved by starting letter (ex:"a_ngram.csv") in a folder for each ngram  

Probz:
-Calculates the Knesner-Ney smoothing probability and stupid backoff probability for the last word of each ngram (2-n)  
-Writes ngram files and probabilities to SQL database  
-Stored in table for each ngram (ex: ngram1, ngram2) in "version/final"  
-Stupid backoff probability is stored in column named "stupid"  
-Knesner-Ney Smoothing Probability is stored in column named "prob"  

## Ngram Generation (NgramGeneration.RMD)
As prevously mentioned, the NgramGeneration.RMD contains all the code necessary to generate a nextword prediction model.  Below I Describe the actions of each code chunk.  

Options  
This code allows for customizing model generation.  Each variable is easily modified for use by later chunks. 
  
version:  
-string  
-version of ngram model, a folder will be created for each version in the working directory    
-subdirectories for testing data, training data, cleaned data raw ngams, and final ngram model will be created as   subrirectories    

ngrams:  
-integer
-generates ngrams up to this given number  
-larger the ngrams require longer the processing time and more memory  

files:  
-list  
-.txt files to use as the corpus 

size:  
-number; between 0 and 1 (but not including 0 and 1)  
-if set to .75; 75% of text will be placed in training corpus  

chunk.size:  
-Number of lines in each data chunk  
-if too large, JAVA memory will run out (consequence of using RWeka tokenizer to make ngrams)  
-Smaller chunks will take longer to process  

remove:  
-list of lists of words to remove  
-Here I removed a list of profanity words and stopwords  
-additional lists of words may be removed if desired  
-this list is passed to "clean" and removed from the cleaned training text  
```{r remove, eval = FALSE}     
cursing <- readLines("profanity.csv", encoding = "UTF-8", skipNul = TRUE) #list of profanity
stopwords <- c("oh", "lol", "and", "or", "the", "or", "by", "it", "an", "of", "its", "but", 
               "so", "be", "do", "to", "if", "in", "at", "as", "be", "am", "is", "me", "my", 
               "is", "id", "was","to", "with", "up", "too", "because", "at", "for", "since", 
               "been", "you", "on")
#put lists of words to remove in "remove"
remove <- list()
remove[[1]] <- cursing
remove[[2]] <- stopwords
```

stats:  
-boolean; TRUE or FALSE
-if set to TRUE, summary statistics and graphs are generated  
-if set to FALSE, these statistics and graphs are not generated  

d:  
-list of numbers of length ngram-1 (no discount for unigrams)  
-This is uused to calculate Knesner-Ney Smoothing probabilities (accomodate for unknown words and weights ngram models)  
-list order: from 2-gram to "ngrams" discount (lowest first)  
-These numbers are typically between 0 and 1  
-higher values are used for smaller order ngrams  
-read more about this here:  
[http://www.foldl.me/2014/kneser-ney-smoothing/]: Kneser-Ney smoothing explained  

Setup  
The Setup chunk sets up ngram generation by importing reqirued packages, custom functions, creates directory and subdirectories for the version, partions data into training and test sets.  
  
"train" and "test" folders:  
-raw training and test data  
  
"cleaned" folder:  
-cleaned training data  
  
"ngram" folder:  
-raw ngram files with ngrams and frequency  
-subfolders for each ngram (ngram1, ngram2...)  
  
"final" folder:  
-folder for ngram model with calculated probabilities for the last word in the ngram  
-After running code, this will contain "final.sqlite" (database with table for each ngram)  
-This folder will be searched to find the predicted next word in "EvaluateModel.RMD"  

cleanTrain 
Takes training corpus and chunks training, cleans (clean.R) then places it in the "cleaned" folder.  The size of the data chunks is selected in "Options".  

Unigrams  
Here, the cleaned training corpus is used to generate a list unigrams (single words) and their frequency.  
This is stored in alphabetized .csv files in "version/ngram/1gram". This code also generates a summary of word frequencies and top words (in graph).  Here, the a list of words (goodWords) is generated to decrease the size of higer order ngram files by only keeping ngrams where the last word is in the top 10% of words.  As this may effect the model, the corpus coverage using only these words is calculated.  Although this removes a large number of words, typically >90% of the corpus is comprised of these top 10% of words.
  
Ngrams
Ngrams generates ngrams from cleaned test text (2 through "ngrams" ngrams).  If "ngrams" is set to 4, the model will generate 2-, 3-, and 4-grams (1-grams generated in "unigrams" chunk).  As probability calculations calculate the probability of the last word, the last word ("B") is split from the beginning words ("A") and stored in separate columns.  The files with part "A", part "B" and the frequency are stored in .csv files corresponding with the first letter of the ngram inside folder for each ngram (ex: "version/ngram/ngram4").  
  
Here, if stats == TRUE (set in "Options" chunk), the frequency of all ngrams, top 1% of ngrams (starting with each letter) is returned.  This is not performed if stats = FALSE.  This data is used to generate graphs in the next code chunk.  

Graphs  
The Graphs chunk takes information about ngrams and plots the frequency of top ngrams and all ngrams.  These graphs are not generated if stats is set to FALSE in the options.  These graphs are printed in the final HTML document.  

Probabilities  
Probabilities reads raw ngram files from "version/ngram/*" and uses d (discount provided in "options") to calculate the final ngram probability using both the backoff and Knesner-Ney probabilities.  The stupid backoff probability is stored in column "stupid" whereas the Knesner-Ney probability is stored in column "prob".   

## Model Evaluation (EvaluateModel.RMD)
EvaluateModel.RMD contains all the code necessary to test the nextword prediction model generated in "NgramGeneration.RMD".  A summary of model performance and several graphs showing the comparison predicted next word frequency (as percent) and actual next word frequency (as percent) is printed in the final HTML summary. The test data is randomly split.  The last word of this substring is the word the model is trying to predict.  Words prior to the last word are used to query the SQL database for the most likely next word based on stupid backoff or Knesner-Ney smoothing probabilities.  Any ties are broken using the countB (count of word last word in corpus).  Although only the top probablity is returned in the analysis here, all possible next words are returned, allowing for the generation of word clouds if this code is modified to run in an app to supplement the next word prediction.  

With this code there are some caveats.  Here, when no word is found, the guessed word is calculated to be "FAIL".  In a final app, this can be prevented by searching for the most common unigrams.  As unigram searches rely only on word frequency and not prior words, this is not performed here. The number of times the model fails to guess the correct word based on 2-gram through n-gram results is an important metric in evaluating model performance. 

Below are the actions of each code chunk.  

###-Options 
These options are used to test the model.  These optoins point to the version of the ngram model (where it is stored), select the probability estimate method, and ensure cleaning of the data is the same as performed in the test set.  
  
version:  
-string; version of ngmodel to test (generated in NgramGeneration.RMD)  
-this must be the same as a model created in NgramGeneration.RMD  
-the search is directed to "final.sqlite" in "version/final"  

size:  
-number (0-1); cannot include 0
-indicates the poriton of the test corpus used in evaluating the model (.5 = test using 50% of the test data)  

method:  
-string; equal to "prob" or "stupid"
-"stupid" to test accuracy of stupid backoff model  
-"prob" for Knesner-Ney smoothing probability  

remove: 
-list of lists of words to remove from test data (done inside clean.R)  

```{r options, eval = FALSE}
# Options
#ngram version to test (files generated in NgramGeneration)
version <- "V1"
#portion of test file to test (number between 0 and 1)
size <- .00005
#ngram genration generates the stupid backoff probablility and Knesner-Ney smoothing probability
    #select "stupid" to test accuracy of stupid backoff model
    #select "prob" for Knesner-Ney smoothing probability
method <- "prob"

#create lists of words to remove during cleaning with clean()
cursing <- readLines("profanity_short.csv", encoding = "UTF-8", skipNul = TRUE) #list of profanity
stopwords <- c("oh", "lol", "and", "or", "the", "or", "by", "it", "an", "of", "its", "but", 
               "so", "be", "do", "to", "if", "in", "at", "as", "be", "am", "is", "me", "my", 
               "is", "id", "was","to", "with", "up", "too", "because", "at", "for", "since")
remove <- list()
remove[[1]] <- cursing
remove[[2]] <- stopwords
```

Setup 
This section loads required packages, loads custom functions (clean.R to clean test data and ngSearch.R to search model for the most likely next words), and cleans a subset test data ("test").  

Evaluate Model
Takes given options from "setup" then randomly cuts the string and predicts the next word and compares it to the next word using data in the SQL database generated in NgramGeneration.Rmd.  The search of the model is performed with the function "ngSearch.R"

Summary
Takes the predicted word, actual word and generates a summary to evaluate model performance.  Here, the percent of times the model guesses the correct word and fails to guess the correct word is calculated.  Additionally,graphs comparing the frequency a word in the actual next word and guessed word are generated.  Predicted words with frequencies much higher or lower frequencies than the actual next word may indicated that the probability for that word is under or over-estimated in the model. 

Calculations:
Word frequency is normalized to the number of words guessed and expressed as a percent (ex:  word frequency/total guesses *100).  Differences between the frequency of guessed words and frequency of actual next words (expressed as a percent).  These differences are plotted as a histogram.  Next the top 15 (under represented if >1) and bottom 15 differences (under represented if <0) are depicted as a bar chart.  Finally, the percent of guesses the model failed to generate and the percent of correct guesses is calculated and the table of actual and guessed word frequencies printed.  This detailed analysis allows for tweaking the model to improve accuracy.
