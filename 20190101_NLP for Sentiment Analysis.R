####################### Script Comments #######################
#**************************************************************
#*
#*
#*  Natural Language Processing for Sentiment Analysis
#*
#*  Objective: Explore some of the packages and techniques
#*  associated with sentiment analysis. Learn more about
#*  tokenization. Import some sample text blocks and score the 
#*  sentiment associated with the text. Utilize topic modeling
#*  to run unsuperivsed classification against more recent
#*  Republican vs Democrat State of the Union Addresses.
#*
#*  Initial Build - 1/1/2019 - Chris Castillo
#*
#*  Change Log
#*  4/12/2019
#*    - Switching dependencies to web hosted SOTU content
#*  8/25/2019
#*    - Switching dependencies to web hosted Presidents content
#*
#*  Notes:
#*    - Be sure to go back and verify that addresses are not
#*      duplicating due to written vs verbal address | Fixed
#*    - Go find additional data source to classify Party of the 
#*      President giving SOTU | Fixed
#**************************************************************
#**************************************************************



#* Clear workspace and load libraries
rm(list=ls())
gc()


library(XML)
library(rvest)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(glue)
library(stringr)
library(magrittr)
library(stringdist)

library(topicmodels)
library(tm)
library(wordcloud)


# Define State of the Union URL
SOTU_url = "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/annual-messages-congress-the-state-the-union"


#* Read SOTU website
SOTU_Website <- xml2::read_html(SOTU_url)


#* Retrieve all URL links from SOTU table
SOTU_Links <- SOTU_Website %>%
 rvest::html_nodes(".table-responsive") %>%
 rvest::html_nodes("tbody") %>%
 rvest::html_nodes("a") %>%
 rvest::html_attr("href")
  

#* Remove NAs 
SOTU_Links <- SOTU_Links[ !is.na(SOTU_Links) ]


#* Remove links that don't start with "https://www.presidency.ucsb.edu/"
SOTU_Links <- SOTU_Links[grep(
  pattern = "^https://www.presidency.ucsb.edu/"
  , x = SOTU_Links
)
]


#* Create function that iterates through SOTU_Links and determines President, Date, Address Content
SOTU_Data <- data.frame(
  President = character(length(SOTU_Links))
  , Address_Date = integer(length(SOTU_Links))
  , Address_Content = character(length(SOTU_Links))
  , stringsAsFactors = FALSE
)
class(SOTU_Data$Address_Date) <- "Date"
for (i in 1:length(SOTU_Links)){
  
  
  # Find JSON block that contains President, Date, Address Content
  Block <- read_html(SOTU_Links[i]) %>%
    html_nodes("div") %>%
    html_nodes("#block-system-main")
    
  
  #* Remove extra spaces
  President <- gsub(
    pattern = '[ ]+'
    , replacement = " "
    , x = Block %>% 
        html_nodes(".field-docs-person") %>%
        html_text()
    ) 
  
  
  #* Split by \n and strip extra spaces at ends
  President <- strsplit(
    President
    , split = "\n"
    )[[1]] %>%
    str_trim()
  
  
  #* Grab the first non "" line
  President <- President[ President != "" ][1]
  
  
  #* Grab the date of the address
  Address_Date <- as.Date(
    Block %>%
      html_nodes(".field-docs-start-date-time") %>%
      html_node("span") %>%
      html_attr("content")
  )
    
  
  #* Store the Address_Content
  Address_Content <- paste(
    Block %>%
      html_nodes(".field-docs-content") %>%
      html_nodes("p") %>%
      html_text()
    , collapse = ''
  )
  
  
  #* Remove \n
  Address_Content <- gsub(
    pattern = "(\n)"
    , replacement = " "
    , x = Address_Content
  )
  
  
  #* Append into SOTU_Data
  SOTU_Data$President[i] <- President
  SOTU_Data$Address_Date[i] <- Address_Date
  SOTU_Data$Address_Content[i] <- Address_Content
  
} # CLOSE LOOP
  

#* Remove duplicate/extraneous SOTU Addresses, Nixon had 11 in 1973... Damn...
SOTU_Data <- SOTU_Data %>%
  mutate(
    Address_Year = year(Address_Date)
  ) %>%
  group_by(
    President
    , Address_Year
  ) %>%
  mutate(
    SOTU_Rank = row_number(Address_Date)
  ) %>%
  ungroup() %>%
  filter(
    SOTU_Rank == 1
  ) %>%
  select(
    President
    , Address_Date
    , Address_Content
  ) %>%
  arrange(
    desc(Address_Date)
  ) %>%
  data.frame()


#* Set working directory
# setwd("C:/Users/Chris Castillo/Data Science/Projects/NLP Sentiment Build/Data/")


#* List the files in working directory/Data
# list.files()


#* Get a file list
# files <- list.files()


#* Define fileName for each file within Data
# fileName <- glue(files[1], sep = "")


#* Remove whitespace in fileName
# fileName <- trimws(fileName)


#* Read in the new file
# fileText <- glue(read_file(fileName))


#* Remove the $ signs, as they are a special character in R
fileText <- gsub("\\$", "", SOTU_Data$Address_Content[1])


#* Tokenize each file, define words/phrases as cells
tokens <- tibble(
  text = fileText
) %>%
  tidytext::unnest_tokens(
    output = word
    , input = text
    , to_lower = TRUE
    , drop = TRUE
  )


#* Retrieve sentiment from the text by:
tokens %>%
  inner_join(
    get_sentiments("bing")
    ) %>% # join on sentiment words only
  count(sentiment) %>% # count the number of positive/negative words
  tidyr::spread(
    sentiment
    , n
    , fill = 0
    ) %>% # made data wide than narrow
  mutate(sentiment = positive - negative) # positive words - negative words



#*************************************************************************
#*************************************************************************
#####   Create a Function for Retrieving Sentiment for a Text Object #####
#*************************************************************************
#*************************************************************************


Get_Sentiment <- function(Object){
  
  #* Retrieve file name
  # fileName <- glue(file)
  
  #* Trim white space from file name
  # fileName <- trimws(fileName)
  
  #* Read file
  # fileText <- glue(read_file(fileName))
  
  #* Remove $ signs
  fileText <- gsub("\\$", "", Object)
  
  #* Create tokens for each file
  tokens <- tibble(
    text = fileText
  ) %>%
    tidytext::unnest_tokens(
      output = word
      , input = text
      , to_lower = TRUE
      , drop = TRUE
    )
  
  # Retrieve sentiment by defined text and pass into dataframe
  sentiment <- suppressMessages(
    tokens %>%
      inner_join(
        get_sentiments("bing")
        ) %>% # join on sentiment words only
      count(sentiment) %>% # count the number of positive/negative words
      tidyr::spread(
        sentiment
        , n
        , fill = 0
        ) %>% # made data wide than narrow
      mutate(sentiment = positive - negative) %>% # positive words - negative words
      mutate(word_count = as.numeric(count(tokens))) # %>%
    # mutate(file = file) %>% # add the name of the file
    # mutate(year = as.numeric(
    #   str_match(
    #     file
    #     , "\\d{4}"
    #     )
    #   )) %>% # add year
    # mutate(president = str_match(file, "(.*?)_")[2])
  )
  
  # Retrieve sentiment dataframe
  return(sentiment)
    
}


#* Check Get_Sentiment on a test file
Get_Sentiment(SOTU_Data$Address_Content[1])



#****************************************************
#****************************************************
##### Apply Get_Sentiment function to all files #####
#****************************************************
#****************************************************


#* Create the object to put derived sentiments into
Sentiments <- tibble()

#* Run Get_Sentiment function on all files
for (i in 1:nrow(SOTU_Data)){
  
  Sentiments <- rbind(
    Sentiments
    , Get_Sentiment(SOTU_Data$Address_Content[i])
    )
  
}


#* Append on additional SOTU attributes
Sentiments$President <- SOTU_Data$President
Sentiments$Address_Date <- as.Date(SOTU_Data$Address_Date)


#* Create a couple of summary statistics
Sentiments <- Sentiments %>%
  mutate(
    Sentiment_Word_Count = as.numeric(sentiment / word_count)
    , Positive_Negative = as.numeric(positive / negative)
  ) %>%
  arrange(
    desc(Address_Date)
  )



#* Correct for Bush vs Bush Sr. problem | no longer needed
# BushSr <- Sentiments %>%
#   filter(
#     president == "Bush"
#     ) %>% # get rows where president is "Bush"
#   filter(
#     year < 2000
#     ) %>% # limit to the years prior to 2000
#   mutate(
#     president = "Bush Sr."
#     ) # rename those to "Bush Sr."


#* Remove incorrect rows | no longer needed
# Sentiments <- anti_join(
#   x = Sentiments
#   , y = Sentiments[ Sentiments$president == "Bush" &
#                       Sentiments$year < 2000
#                     , ]
#   )


#* Combine sentiments with BushSr fix | no longer needed
# Sentiments <- full_join(x = Sentiments, y = BushSr)


#************************************************
#************************************************
##### Run visual plots on sentiment dataset #####
#************************************************
#************************************************


#********************************
#* Evaluate pure sentiment count
#********************************

#* Plot sentiment over time
ggplot(
  data = Sentiments
  , aes(
    x = as.Date(Address_Date)
    , y = Positive_Negative
    )
  ) +
  geom_point(
    aes(
      color = President
      )
    ) +
  geom_smooth(method = "auto")


#*********************************************************
#*********************************************************
##### Retrieve and Append Presidents Political Party #####
#*********************************************************
#*********************************************************


#* Store the President URL
Presidents_url <- 'https://www.presidentsusa.net/partyofpresidents.html'


#* Read the Presidents website
Presidents_Website <- xml2::read_html(Presidents_url)


#* Store political parties
Political_Parties <- Presidents_Website %>%
  rvest::html_nodes("div + .row.list") %>%
  rvest::html_nodes("h4") %>%
  rvest::html_text()
Political_Parties <- Political_Parties[ Political_Parties != "" ]


#* Store President names
Presidents_Names <- Presidents_Website %>%
  rvest::html_nodes("div + .row.list") %>%
  rvest::html_nodes("ul") %>%
  rvest::html_text() 


#* Split out the different presidents names
President_Name_Party <- lapply(
  1:length(Presidents_Names)
  , function(x){
    strsplit(
      x = Presidents_Names[x]
      , split = "\r\n"
    )
  }
)


#* Assign the names of the political parties
names(President_Name_Party) <- Political_Parties


#* Remove whitespace
President_Name_Party <- lapply(
  President_Name_Party
  , function(x) 
    sapply(
      x
      , trimws
      )
  )


#* Drop the list with NA name
President_Name_Party[is.na(names(President_Name_Party))] <- NULL


#* Drop all elements that have nchar(x) == 0
President_Name_Party <- lapply(
  President_Name_Party
    , function(x)
      x[nchar(x)>0]
  )
        

#* Create tibble with Name_Party data
President_Name_Party <- tibble(
  Name = unlist(President_Name_Party)
  , Party = gsub(
    x = names(unlist(President_Name_Party))
    , pattern ='[0-9]'
    , replacement = ''
  )
)
  

#* Create vector of unique President_SOTU_Names
SOTU_Presidents <- tibble(President = Sentiments$President %>% unique())


#* Create vector of best approximate matches to SOTU_Presidents
Presidents_Match <- President_Name_Party$Name[ 
  amatch(
    x = SOTU_Presidents$President
    , table = President_Name_Party$Name
    , method = "jaccard"
    , maxDist = Inf
  )
  ]


#* Create data.frame with Political party affiliation
Party_Data <- SOTU_Presidents %>%
  mutate(
    President_Match = Presidents_Match
  ) %>%
  left_join(
    y = President_Name_Party
    , by = c("President_Match" = "Name")
  ) %>%
  select(
    President
    , President_Match
    , Party
  ) %>%
  distinct()


#* Hardcode "George H. W. Bush" fix | Not necessary
# Party_Data$President_Match[ Party_Data$President == "George Bush"] <- "George H. W. Bush"


#* Append on political party to Sentiments
Sentiments$Party <- Sentiments %>%
  left_join(
    y = Party_Data
    , by = c("President" = "President")
  ) %>%
  select(
    President
    , Party
  ) %>%
  pull()


#* Define arrays of Republican and Democrat sentiment (net)
Republican <- unlist((Sentiments[ Sentiments$Party == "Republican" 
                                  , "sentiment"]))
Democrat <- unlist(Sentiments[ Sentiments$Party == "Democrat"
                               , "sentiment"])


#* Determine if sentiment is different for Republicans vs Democrats
t.test(x = Democrat
       , y = Republican
       )


#* Plot sentiment comparison by "party"
ggplot(data = Sentiments
       , aes(x = Party
             , y = sentiment
             , color = Party)
       ) +
      geom_boxplot() +
      geom_point()



#*************************************
#*************************************
##### Evaluate percent_sentiment #####
#*************************************
#*************************************

#* Create percent_sentiment
Sentiments$percent_sentiment <- Sentiments %>%
  mutate(
    percent_sentiment = positive / (positive + negative)
  ) %>%
  select(
    percent_sentiment
  ) %>%
  pull() %>%
  as.numeric()


#* Plot percent_sentiment over time
ggplot(data = Sentiments
       , aes(x = Address_Date, y = percent_sentiment)) +
  geom_point(aes(color = President)) +
  geom_smooth(method = "auto")


#* Define arrays of Republican and Democrat sentiments
Republican <- unlist((Sentiments[ Sentiments$Party == "Republican", "percent_sentiment"]))
Democrat <- unlist(Sentiments[ Sentiments$Party == "Democrat", "percent_sentiment"])


#* Determine if sentiment is different for Republicans vs Democrats
t.test(x = Democrat
       , y = Republican
)


#* Plot sentiment comparison by Party
ggplot(data = Sentiments
       , aes(x = Party
             , y = percent_sentiment
             , color = Party)) +
  geom_boxplot() +
  geom_point()


#********************************************************
#********************************************************
##### Identify main subjects through Topic Modeling #####
#********************************************************
#********************************************************

#* Set Date_Var to be the filtering variable starting with Regan
Date_Var <- as.Date("1981-02-01")


#* Create storage tibble
Sentiments_Recent_Store <- tibble()


#* Convert recent Republican and Democrat SOTU addresses to Corpus
Documents <- Corpus(
  VectorSource(
    x = SOTU_Data[ SOTU_Data$Address_Date >= Date_Var, "Address_Content" ]
  )
)


#* Remove punctuation, numbers, stopwords, whitespace AND convert tolower
Documents <- Documents %>%
  tm_map(
    removePunctuation
  ) %>%
  tm_map(
    removeNumbers
  ) %>%
  tm_map(
    content_transformer(tolower)
  ) %>%
  tm_map(
    removeWords
    , c("i", stopwords("english"))
  ) %>%
  tm_map(
    stripWhitespace
  ) %>%
  tm_map(
    stemDocument # May need to rethink this concept
  )


#* Create document term matrix from corpus_test
dtm_Documents <- DocumentTermMatrix(Documents)


#* Remove empty rows from document term matrix, just in case
dtm_Documents <- dtm_Documents[ apply(
  dtm_Documents
  , 1
  , sum
) > 0 
, ]


#* Run LDA on dtm_Documents for two topics
lda_Documents <- LDA(
  x = dtm_Documents # Set target
  , k = 2 # Set number of topics
  , method = "Gibbs" # Set method of LDA
  , control = list(
    nstart = 5 # Set random starts
    , seed = list(1, 12, 123, 1234, 12345)
    , best = TRUE
    , burnin = 1000 # Set burnin?
    , iter = 2000 # Set number of iterations
    , thin = 500 # Thin the spaces between samples
  )
)


#* Find top 10 terms for the topics within lda_Documents
terms(
 x = lda_Documents
 , k = 10
)


#* Create matrix of lda_Documents top 50 terms
lda_Documents_terms <- as.matrix(
  terms(
    x = lda_Documents
    , k = 50
  )
)


#* Create colnames for lda_Documents_terms
colnames(lda_Documents_terms) <- paste0(
  "Topic "
  , c( 1:ncol(lda_Documents_terms) )
)


#* Create matrix of lda_Documents probabilities of topic classification
lda_Documents_prob <- as.matrix(
  lda_Documents@gamma
)


#* Create colnames for lda_Documents_prob
colnames(lda_Documents_prob) <- paste0(
  "Topic "
  , c( 1:ncol(lda_Documents_terms) )
)


#* Create Sentiments_Recent to evaluate derived Topics
Sentiments_Recent <- Sentiments[ Sentiments$Address_Date >= Date_Var , ]


#* Add highest probability topic column to Setiments
Sentiments_Recent$Topic <- as.factor(topics(lda_Documents))


#* See which Topics correspond to which Party 
Sentiments_Recent_Store <- Sentiments_Recent %>%
  group_by(
    Topic
  ) %>%
  mutate(
    Topic_Count = length(President)
  ) %>%
  group_by(
    Topic
    , Party
  ) %>%
  mutate(
    Party_Count = length(President)
  ) %>%
  ungroup() %>%
  mutate(
    Topic_Percent = Party_Count / Topic_Count
  ) %>%
  arrange(
    Topic
    , Party
  ) %>%
  distinct(
    Topic
    , Party
    , Topic_Count
    , Party_Count
    , Topic_Percent
  ) %>%
  mutate(
    Filter_Date = Date_Var
  )
  

#* Topic 1 = Democrat, Topic 2 = Republican
#* Topic 1 classified 16 / 19, Topic 2 classified 20 / 20
Sentiments_Recent_Store
  

#* Create a wordcloud for Topic 1 (Democrat)
dev.new()
par(mar=c(1,1,1,1))
wordcloud(
  words = lda_Documents@terms
  , freq = exp(lda_Documents@beta[1, ])
  , max.words = 50
  , random.order = FALSE
  , rot.per = 0.35
  , colors=brewer.pal(8, "Dark2")
  )


#* Create a wordcloud for Topic 2 (Republican)
dev.new()
par(mar=c(1,1,1,1))
wordcloud(
  words = lda_Documents@terms
  , freq = exp(lda_Documents@beta[2, ])
  , max.words = 50
  , random.order = FALSE
  , rot.per = 0.35
  , colors=brewer.pal(8, "Dark2")
)