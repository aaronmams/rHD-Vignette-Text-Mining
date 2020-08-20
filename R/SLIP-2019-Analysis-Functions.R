######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
# This file contains functions to help parse the text of 
# Supreme Court Opinions

# I have downloaded a handful of pdf files of Supreme Court Opinions
# from here: https://www.supremecourt.gov/opinions/slipopinion/19

# Most of the heavy lifting is done by a single function called:

#  get.markers()


# The get.markers() functions reads in a Supreme Court Opinion and finds the 
# sections of text corresponding to the majority opinion and any dissenting opinions. 

# Each case opinion is organized similarly: there is a Syllabus, followed by the majority opion,
# followed by dissenting opinions. 

# Each case opinion is stored in a separate .pdf file. So if, for example, we want to get all of 
# Cheif Justice John Robert's dissenting opinions for the 2019 session, we need to read all the 
# case files in then parse them to find the dissenting opinions, then filter those dissenting opinions
# for just the ones written by John Roberts.


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
#Libraries and Packages
library(pdftools)
library(here)
library(tidyverse)
library(data.table)

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

#-------------------------------------------------------------------
# global search terms

# these are the terms that we are going to use to identify majority
# and dissenting opinions in the text from the case files

# for instance in the case of McGirt v. Oklahoma Justice Neil Gorsuch delivered
# the majority opinion while Cheif Justice Roberts wrote a dissenting opinion and
# Justice Clarence Thomas wrote another dissenting opinion. 

# our algorithm is going to search the text for all of the phrases,
# "JUSTICE ALITO delivered the opinion of the Court", "JUSTICE THOMAS delivered the opinion of the Court",
#   "JUSTICE KAGAN delivered the opinion of the Court", etc. 
# Once one of these terms is found, the algorithm will identify that page number and save it as an object.
#
# the algorithm will then search for all the dissenting terms, "ROBERTS, C. J., dissenting", 
# "GINSBURG, J., dissenting", "THOMAS, J., dissenting", etc. etc. the algorithm will collect all the page
# numbers where any of these search terms appear. 
#-------------------------------------------------------------------


majority.find <- data.frame(justice=c("Neil Gorsuch","Samuel Alito","Clarence Thomas","Elena Kagan","John Roberts",
                                      "Ruth Bader Ginsburg","Brett Kavanaugh"),
                            phrase=c("JUSTICE GORSUCH delivered the opinion of the Court",
                   "JUSTICE ALITO delivered the opinion of the Court",
                   "JUSTICE THOMAS delivered the opinion of the Court",
                   "JUSTICE KAGAN delivered the opinion of the Court",
                   "CHIEF JUSTICE ROBERTS delivered the opinion",
                   "JUSTICE GINSBURG delivered the opinion of the Court",
                   "JUSTICE KAVANAUGH delivered the opinion of the Court"))

dissent.find <- data.frame(justice=c("John Roberts","Samuel Alito","Clarence Thomas",
                                     "Elena Kagan","Sonya Sotomayor",
                                "Ruth Bader Ginsburg","Stephen Breyer"),
                           phrase=c("ROBERTS, C. J., dissenting",
                  "ALITO, J., dissenting",
                  "THOMAS, J., dissenting",
                  "KAGAN, J., dissenting",
                  "SOTOMAYOR, J., dissenting",
                  "GINSBURG, J., dissenting",
                  "BREYER, J., dissenting"))

#################################################################################################
#################################################################################################
#################################################################################################

###############################################################################################
###############################################################################################
###############################################################################################
# This function is the primary execution of our simplified algorithm. It takes in a file name,
#  then uses the pdf_text() method to read all the text from that pdf file into the workspace.
# the function then does an iterative regular expression search using all the search terms
# appearing above in the global search terms section.

# The function collects the page numbers where any of the search terms were found and organizes
# them in a data frame.

# The output from this function is a data frame containing the locations (page numbers) within the text
# of the majority and dissenting opinions and which justices authored them.

get.markers <- function(case){

doc <- pdf_text(here(paste('data/',case,sep="")))
  
# for each document find where the majority opinion starts and who wrote it
majority <- lapply(majority.find$phrase,function(x){which(str_detect(doc,x))})

majority.page.start <- unlist(majority) 
majority.author.idx <- which(unlist(lapply(majority,function(x){length(x)}))>0)
majority.author <- majority.find$justice[majority.author.idx]

# then find the dissenting opinions
#the dissenting opinion is a little funky because it has the phrase "so-and-so dissenting"
#    at the top of each page of the dissent...so I want to find the 1st occurance of that phrase
dissent.fn <- function(phrase,doc){
  res <- which(str_detect(doc,phrase))
  if(length(res)==0){
    return(data.frame(start=NA,end=NA,author=NA))
  }else{
    return(data.frame(start=min(res),end=max(res),author=dissent.find$justice[dissent.find$phrase==phrase]))     
  }
}

dissent <- data.frame(rbindlist(lapply(dissent.find$phrase,dissent.fn,doc=doc)))
# add the document and opinion type
dissent <- dissent %>% mutate(case=case,opinion="dissent")

#-----------------------------------------------------------------------------------
# at this point I have the location of the majority opinion and name of author + the 
# locations of all dissenting opinions and thier authors. I want to put this into a data frame

info <- rbind(dissent,data.frame(start=majority.page.start,
                                 end=min(dissent$start,na.rm=T)-1,
                                 author=majority.author,
                                 case=case,opinion="majority"))

# final step is to clean up the info data frame
info <- info %>% filter(is.na(author)==F)
#------------------------------------------------------------------------------------

return(info)
}

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
# 
#  This section contains some additional convenience functions
#
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################

# A simple function to get text
get.case.text <- function(case){
  path <- paste('data/',case,sep="")
  text <- pdf_text(here(path))
  return(text)
}

# A function to get majority and dissenting opinions for each/any Justice
get.justice.opinions <- function(justice,case.list){
  # use the get.markers() function to put the markers together
  markers <- data.frame(rbindlist(lapply(case.list,get.markers)))
  # filter the markers data frame for the justice of interest
  markers <- markers %>% filter(author==justice)
  
  justice.text <- lapply(markers$case,function(x){
    text <- get.case.text(case=x)
    text <- text[markers$start[markers$case==x]:markers$end[markers$case==x]]
  })
return(justice.text)
}

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
# A function to get a tokenized data frame for all of the opinions of a particular
# type (dissenting or majority) from a particular justice

filter.case.text <- function(case,page.range){
  text <- get.case.text(case=case)
  return(text[page.range[1]:page.range[2]])
}

tokenize.case.text <- function(case.text){
  case.text <- str_split(case.text,"[\n]")
  
  # now coerce this list object to a data frame where each row is a line from the original text
  df <- data.frame(rbindlist(lapply(case.text,function(x){data.frame(text=x)})))
  
  # now with each row as a line, I'm going to split each line into a "token"
  df <- df %>% 
    unnest_tokens(word, text) 
return(df)
}

#-------------------------------------------------------------------------------------
# this function can be used to get the text of all majority or dissenting opinions
# for a single justice.

# the function accepts the following inputs:
#
# 1. markers - this is the data frame output from the get.markers() function
# 2. justice - the name of a justice (i.e. "John Roberts", "Samuel Alito", etc.). The text
#               should match exactly to the names in the global search terms objects majority.find
#                 dissent.find
# 3. type - either "majority" or "dissent"
#
# the function uses the following convenience functions:

# 1. filter.case.text(): to get the text of opinions written by the justice
# 2. tokenize.case.text(): to parse the text of opinions and organize it as a data frame
#                           of words
#
# the function produces the following output:
# 1. a list of data frames. each data frame is a majority or dissenting opinion written by the 
#      justice of interest. each data frame is organized as a collection of words from the text

justice.opinions <- function(markers,justice,type){
  tmp <- markers %>% filter(opinion==type & author==justice)
  justice.opinion.words <- list()
  for(i in 1:nrow(tmp)){
    text <- filter.case.text(case=tmp$case[i],page.range=c(tmp$start[i],tmp$end[i]))
    df <- tokenize.case.text(case.text=text)
    justice.opinion.words[[i]] <- df %>% mutate(case=tmp$case[i],justice=tmp$author[i])
  }
return(justice.opinion.words)  
}
#------------------------------------------------------------------------------------



