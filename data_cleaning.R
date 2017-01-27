#' ---
#' title: Inauguration Speech Data Cleaning
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' ---

#+ message = FALSE, warning = FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(rvest)
library(lubridate)
library(stringr)
library(stringi)

#' Even though we collected data from three different sources we still have
#' some cleaning to do before we can begin to analyze it. We'll comb through
#' the data and remove and additional oddities that we discover along the way.
#' 

#+ include=FALSE
# writeFiles = FALSE so that it doesn't rewrite our data
writeFiles <- FALSE

# read the data
data <- read_csv("Documents/code/inauguration_speeches/data/speech_data_dirty.csv")

#' First things first, let's clean up `president`, and formalize the date
#' so we can make `DATE` objects, and encode our text to 'ASCII'
data %<>% 
  mutate(
    president = str_trim(gsub("Address|of|;|", "", president)),
    speech_date = str_extract(speech_date, "January.*d*"),
    speech_date = gsub(",", "", speech_date),
    speech_date = as.Date(speech_date, format=c('%B %d %Y')),
    content = iconv(content, "UTF-8", "ASCII", sub = "")
  )

#' `president` and `speech_date` are now clean! Now comes the difficult part, 
#' trimming all the nonsense from the actual speech transcript. We'll have
#' to go through each transcript, print it to the screen, and see how can
#' extract the pieces we want. First though, we'll remove something I noticed
#' from Obama's second speech - '(Applause)' is littered all throughout
#' the text.

dataClean <- data %>%
  mutate(
    # strip all returns and new lines
    content = ifelse(
      year(speech_date) %in% c(2001, 2005, 2009),
      gsub("\r|\n", "", content), content
    ),
    # remove the opening part of the text
    content = ifelse(
      year(speech_date) %in% c(2001, 2005, 2009),
      gsub('^.*Address of.*?20\\d{2,}', "", content), content
    ),
    # remove trailing part of the text
    content = ifelse(
      year(speech_date) %in% c(2001, 2005, 2009),
      gsub('Inaugural\\sSpeeches\\sPage.*', '', content), content
    ),
    # clean up obama's 2013 speech
    content = ifelse(
      year(speech_date) == 2013,
        gsub('\\(Applause\\.\\)|\\(applause\\)', "", content), content
      ),
    content = ifelse(
      year(speech_date) == 2013,
      str_extract(content, "THE PRESIDENT:.*?END"), content
      ),
    content = ifelse(
      year(speech_date) == 2013,
      str_trim(gsub("THE\\sPRESIDENT:|END", "", content)), content
      ),
    # lastly, add a space after periods for Trump's speech
    content = ifelse(
    year(speech_date) == 2017,
    gsub("\\.", '. ', content), content
    )
    
  ) %>%
  arrange(speech_date)

if(writeFiles = TRUE) {
# write data
write.csv(dataClean, 'Documents/code/inauguration_speeches/data/speech_data_clean.csv', row.names = FALSE)
}