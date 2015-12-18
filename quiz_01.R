# ---------------------------------------------------------------------
# Coursera.org - Data Science Specialization - Capstone Project
# 
# >> Quiz Solution (01)
# author..: Sergio Vicente (@svicente99)
# data....: 15th Dec. 2015
# ---------------------------------------------------------------------

library(stringi)

DATASET_DIR = "../Capstone_Dataset/"
OUTPUT = "quiz_01.out.txt"
  
  
q01 <- function() {
  FILE = "en_US/en_US.blogs.txt"
  arq <- paste(DATASET_DIR,FILE,sep="")
  print(arq)
  en_blogs_size <- file.info(arq)$size / 1024 / 1024
  print("The en_US.blogs.txt file is how many megabytes?")
  print(en_blogs_size)
}

q02 <- function() {
  FILE = "en_US/en_US.twitter.txt"
  arq <- paste(DATASET_DIR,FILE,sep="")
  print(arq)
  print("The en_US.twitter.txt has how many lines of text?")
  len <- length(readLines(arq))
  format(len,big.mark=".",decimal.mark=",")  # Brazilian mode
}

q03 <- function() {
  FILE1 = "en_US/en_US.blogs.txt"
  FILE2 = "en_US/en_US.news.txt"
  FILE3 = "en_US/en_US.twitter.txt"
  print("What is the length of the longest line seen in any of the three en_US data sets? ")

  lstFiles <- c(FILE1, FILE2, FILE3)
  ##lstFiles <- c("quiz_01.R", "Quiz-1.html")
  
  fOut <- file("quiz_03-out.txt")
  for(f in lstFiles) 
    q03_do(f, fOut);
  close(fOut)
}  


q03_do <- function(nm_file, fOut) 
{
  file <- paste(DATASET_DIR,nm_file,sep="")
  #file <- nm_file
    
  ## Create connection
  con <- file(description=file, open="r")
  
  ## Hopefully you know the number of lines from some other source or
  com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep="")
  nLines <- system(command=com, intern=TRUE)
  
  ## Loop over a file connection
  nMaxLine <- 0
  maxLine <- ""
  for(i in 1:nLines) {
    oneLine <- readLines(con,1)
    ## determine the line with maximum length
    len <- stri_length(oneLine)
    if(len > nMaxLine) {
      maxLine <- oneLine; nMaxLine <- len 
    }
  }
  close(con)
  
  print("-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-")
  print(file); 
  print("________________________________________")
  print(toString(nMaxLine)); 
  print(maxLine)
}

#arq <- paste(DATASET_DIR,f,sep="")
#bash_cmd <- paste(paste("bash longest_line.sh",arq),OUTPUT)
#longline <- system(bash_cmd)

q04 <- function() 
{
  file <- paste(DATASET_DIR,"en_US/en_US.twitter.txt",sep="");

  ## Create connection
  con <- file(description=file, open="r")
  
  ## Hopefully you know the number of lines from some other source or
  com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep="")
  nLines <- system(command=com, intern=TRUE)
  
  ## Loop over a file connection
  nLove <- 0; nHate <- 0
  for(i in 1:nLines) {
    oneLine <- readLines(con,1)
    ## count "love" and "hate" occurrences
    if(grepl("love",oneLine))
      nLove <- nLove+1;
    if(grepl("hate",oneLine))
      nHate <- nHate+1;
  }
  close(con)
  
  print("-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-")
  print(paste("Number of LOVE occurrences", nLove)); 
  print(paste("Number of HATE occurrences", nHate)); 
  print("________________________________________")
  print('In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?")')
  print(paste("The answer is ",toString(nLove/nHate))); 
}

q05 <- function() 
{
  file <- paste(DATASET_DIR,"en_US/en_US.twitter.txt",sep="");
  WORD = "biostats"
  
  ## Create connection
  con <- file(description=file, open="r")
  
  ## Hopefully you know the number of lines from some other source or
  com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep="")
  nLines <- system(command=com, intern=TRUE)
  
  ## Loop over a file connection
  theLine <- ""
  for(i in 1:nLines) {
    oneLine <- readLines(con,1)
    ## locate the line if "biostats" word
    if(grepl(WORD,oneLine))
      theLine <- oneLine
  }
  close(con)
  
  print("-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-")
  print('The one tweet in the en_US twitter data set that matches the word "biostats" says what?')
  print(paste("This line answer is ",theLine)); 
  
  #ANSWER:
  # i know how you feel.. i have biostats on tuesday and i have yet to study =/
}


q06 <- function() 
{
  file <- paste(DATASET_DIR,"en_US/en_US.twitter.txt",sep="");
  PHRASE = "A computer once beat me at chess, but it was no match for me at kickboxing"
  
  ## Create connection
  con <- file(description=file, open="r")
  
  ## Hopefully you know the number of lines from some other source or
  com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep="")
  nLines <- system(command=com, intern=TRUE)
  
  ## Loop over a file connection
  nTweets <- 0
  for(i in 1:nLines) {
    oneLine <- readLines(con,1)
    ## locate the line for PHRASE
    if(grepl(PHRASE,oneLine))
      nTweets <- nTweets+1
  }
  close(con)
  
  print("-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-:-")
  print('How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)')
  print(paste("Number of tweets is ",nTweets)); 
}
