# *in RStudio, first go under Session -> Set Working -> To Source File
# you'll need a few libraries...

install.packages(pkgs=c('urltools','pwr','lsr')) # run this once... 
install.packages(pkgs=c('ggplot2'))
install.packages(pkgs=c('magrittr'))


library(urltools) # to clean our headlines
library(tidyverse)
library(tidyr)
library(dplyr)

# load in CSV file, quick check, cleaning...

# reading in a comma-separate value (CSV) file; variable names in header (TRUE)
headlines = read.csv('lab_1_data.csv',header=TRUE)

# let's look at the first row... 
headlines[1,]

# we need to decode the title into a more readable format... so:
headlines$title = url_decode(headlines$title)

# how many of each source?
table(headlines$source)

# making subsets by source 
nyt <- subset(headlines, source == "New York Times")
biz <- subset(headlines, source == "Business Insider")

# specifying row in subsets
nyt.title <- nyt$title
biz.title <- biz$title

#counting and averaging characters by news source 
nyt.avg.char <- mean(nchar(nyt.title.char))
biz.avg.char <- mean(nchar(biz.title.char))

#just calling 
nyt.avg.char
biz.avg.char
########################################

# -------LAB 2-------- 


#identifying appearance of the string "she" by source
she.ment <- 1:nrow(headlines) %in% grep('she',tolower(headlines$title)) 
aggregate(she.ment~source, data = headlines, FUN = mean)

#adding column to df
headlines$she.ment <- 1:nrow(headlines) %in% grep('she',tolower(headlines$title)) 

#just messing around and making T/F to 0/1 
headlines$she.ment <- as.integer(as.logical(headlines$she.ment))

#counting "she" appearances in titles
nyt.she.ment.total <- filter(headlines, she.ment == "1" & headlines$source == "New York Times")
biz.she.ment.total <- filter(headlines, she.ment == "1" & headlines$source == "Business Insider")

table(she.ment)

# running t-test
t.test(she.ment)

# was attempting to calculate effect size by hand...needed standard deviations for both groups
aggregate(she.ment~source, data = headlines, FUN = sd)


##################
#these are just other measure I explored for this lab but did not use for submission
government.ment <- 1:nrow(headlines) %in% grep('government',tolower(headlines$title)) 
aggregate(government.ment~source, data = headlines, FUN = mean)
headlines$government.ment <- 1:nrow(headlines) %in% grep('government',tolower(headlines$title)) 


#identifying appearance of the string "you" by source
you.ment <- 1:nrow(headlines) %in% grep("you",tolower(headlines$title)) 
aggregate(you.ment~source, data = headlines, FUN = mean)

#adding column to df
headlines$you.ment <- 1:nrow(headlines) %in% grep("you",tolower(headlines$title)) 

table(you.ment)

#counting "you" appearances in titles
nyt.you.ment <- filter(headlines, you.ment == "TRUE" & headlines$source == "New York Times")
biz.you.ment <- filter(headlines, you.ment == "TRUE" & headlines$source == "Business Insider")

# removing variables/columns no longer being utilized in data frame becasue it was messy
headlines2 = select(headlines, -6, -7:-8)

