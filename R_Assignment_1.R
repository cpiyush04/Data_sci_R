library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
library(utils)


#------------------------------------------------------------------------------#
# Ques -a
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")

#using html_table as element-wise scrapping did not give expected results
data<-html %>% html_table(fill=TRUE) 
data<-data[[1]]
data<- data.frame(data,row.names = NULL)
data_CMP<- data %>% select(-1,-14,-15) 
write.csv(data_CMP,"C:\\Users\\piyus\\Desktop\\nifty50.csv")



#------------------------------------------------------------------------------#
# Ques -b
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-passenger-cars/mahindra-mahindra/company-info")
infotab<-html %>% html_table(fill=TRUE,convert = FALSE,header = TRUE)
data1<-infotab[[1]]
data2<-infotab[[2]]
data3<-infotab[[3]]
data1<- data.frame(data1,row.names = NULL)
data1


#------------------------------------------------------------------------------#
tennis <- function(p){
  x <- 0
  win_sequence <- c()

  while (x<5) {
    
    #...code approach...
    # my approach is to keep updating the 
    # vector win_sequence with "A" or "B" by 
    # checking the appropriate condition and then
    # to check for whether the win_sequence contains
    # sequence "AAA" and "BBB". If yes the loop would
    # break here.
    
     x <- x+1
  }
  
  return(x)
}


#------------------------------------------------------------------------------#
# Ques -d
MontyHall <- function(){
  choice <- c("goat","car","goat")
  player_choose <- sample(choice)
  Monty_choose <- choice[-which(choice == "car" | choice == player_choose)]
  Monty_Opened <- sample(Monty_choose, 1)
  Switch <- choice[-which(choice == player_choose | choice== Monty_Opened)]
  if (Switch== "car") {
    return(1)  # Contestant wins
  } 
  else {
    return(0)  # Contestant loses
  }
}

wins<-0

for( i in 1:1000){
  output <- MontyHall()
  wins <- wins + output
}

prob_winning <- wins/1000
prob_winning
cat("Probability of winning if the contestant switches:", prob_winning)


#------------------------------------------------------------------------------#
# Ques -e
html2<-read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

# the names of the movies are in a tag of article_movie_title class
movies<- html_elements(html,".article_movie_title a")
movies_names <- html_text(movies)

# for extracting year as numeral we need to remove'()' character
movies_year <- html2 %>% html_elements(".subtle start-year") %>% html_text() %>%
               substr(2,5) %>% as.numeric()


movies<- html_elements(html,".tMeterScore")
movies_score<- html_text(movies)

movies<- html_elements(html,".countdown-index")
movies_rank<- html_text(movies)

movies_data<-data.frame("Name" = movies_names, "Year" = movies_year,
                        "Score" = movies_score, "Ranking" = movies_rank)
