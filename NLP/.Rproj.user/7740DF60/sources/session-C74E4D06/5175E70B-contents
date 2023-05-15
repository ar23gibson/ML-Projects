################################################################################
#
#   Title: Natural language processing Kaggle example
#   Author: A. Gibson
#   Date: 15/05/2023
#   Version: 1.0
#   GitRepo:
#   Description:
#
#   url:https://www.kaggle.com/code/rtatman/nlp-in-r-topic-modelling/notebook
#
#   To build an 
#       Unsupervised topic model using Latent Dirichlet Allocation (LDA)
#       Explore the impact of text pre-processing, including removing stop words
#       and stemming
#       Built a supervised topic model using term frequency–inverse document 
#       frequency (TF-IDF)
#
#       Topic modeling: The NLP task of identifying automatically identifying 
#       major themes in a text, usually by identifying informative words.
#
#       LDA outputs:
#       An estimate of how much each topic contributes to each document
#       An estimate of how much each word contributes to each topic
#
#       Data from:
#       M. Ott, C. Cardie, and J.T. Hancock. 2013. Negative Deceptive Opinion 
#       Spam. In Proceedings of the 2013 Conference of the North American 
#       Chapter of the Association for Computational Linguistics: Human Language
#       Technologies.
#
################################################################################


### Initialise libraries -------------------------------------------------------

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming


### Read in data ---------------------------------------------------------------

reviews <- read.csv("C:/Users/andrew.gibson/Documents/GitHub/ML-Projects/NLP/data/deceptive-opinion.csv")
# how to get this to split over two lines?


### Function that reads column; return plot of most informative words for given 
### number of topics ----------------------------------------------------------- 

#function takes column; returns plot; looks for 4 topics
top_terms_by_topic_LDA <- function(input_text, plot =T, number_of_topics =4)
{
  Corpus <- Corpus(VectorSource(input_text)) # defines corpus like object 
  DTM <- DocumentTermMatrix(Corpus) #get count of words/document 
  
  # perform LDA and store words in tidytext format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") 
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # find top terms for each topic
  top_terms <- topics %>% # take topics data frame (topics is a function - try)
    group_by(topic) %>% # treat each topic as a different group 
    top_n(10, beta) %>% # get the top 10 most informative words 
    ungroup() %>% # ungroup, why?
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if user asks for plot (true by default) plot top ten terms in order
  if(plot == T)
  {
    top_terms %>% #take top ten terms 
      mutate(term = reorder(term, beta)) %>% #sort terms by beta value 
      #mutate = creates columns of existing variables 
      ggplot(aes(term, beta, fill = factor(topic))) + 
      geom_col(show.legend = FALSE) + #barplot
      facet_wrap(~ topic, scales = "free") + #each topic in seperate plot
      labs(x=null, y= "Beta") + #no x label, change y
      coord_flip() # turns bars horizontal 
  }else{
    #if the plot == False, return a list
    return(top_terms)
  }
}

top_terms_by_topic_LDA(reviews$text, number_of_topics = 2)


