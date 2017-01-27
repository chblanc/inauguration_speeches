#' ---
#' title: Inauguration Speech Data Scraper
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' ---

#+ message = FALSE, warning = FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(stringi)

#+ include=FALSE
# writeFiles = FALSE so that it doesn't rewrite our data
writeFiles <- FALSE

#+ echo = FALSE
# Create object with today's date
dateToday <- Sys.Date()

#' Transcripts of inauguration speeches can be found at the following website:
#' http://avalon.law.yale.edu". We will build a scraper to go through
#' the site, download the text, and output it as a .csv.

#' ## Getting Starated
#' 
#' First, we create `baseLink` which is the foundational link that we'll use
#' to build out the URLs that lead directly to the content we desire. The
#' secondary object, `mainLink`, is where we'll pull the second part
#' of the URL (pointing us to each webpage)

# homepage link
baseLink <- "http://avalon.law.yale.edu"
mainLink <- "http://avalon.law.yale.edu/subject_menus/inaug.asp"

# Read the html 
homepage <- read_html(mainLink)

# pull out the individual links for each inaguration speech
speechLinks <-  homepage %>%
  html_nodes("tr+ tr a") %>%
  html_attr('href') %>%
  gsub("\\..", "", .) 

#' For some reason the original html didn't include Obama's speech so we'll
#' have to add it manually. It's also kind of a bummer that it only 
#' includes his first speech. At some point we could spend some time
#' finding a link to his second speech.

# add obama
speechLinks[(length(speechLinks) + 1)] <- "/21st_century/obamasp"

#' the links should look something like this:
#' http://avalon.law.yale.edu/18th_century/wash1.asp, and it looks like
#' `speechLinks` is missing the '.a' in '.as'. Fix it!

# fix urls
speechLinks <- gsub('(sp)$', '.asp', speechLinks)

# Now create the link leading to the text
speechUrls <- paste0(baseLink, speechLinks)

# subset links to go back to 20th century
speechUrlsSub <- speechUrls[grepl('21st', speechLinks)]


# write a loop to pull in all the content from all the pages:
for(i in 1:length(speechUrlsSub)) {
  
  if(i == 1) { speechContent <- list() }
  
  # read in url
  speechHtml <- read_html(speechUrlsSub[[i]])
  
  # save content to a list
  speechContent[[i]] <- speechHtml %>%
    html_nodes('.text-properties') %>%
    html_text()
  
  # chill for a second, bruh
  Sys.sleep(floor(runif(1, 10, 20)))
  
}

#' Next we convert our list into a dataframe so that we can prepare it to be
#' saved as a `csv`.

# Convert to dataframe
data <- data.frame(
  content = do.call('rbind', speechContent)
)

#' ## Grab the second Obama speech
#' 
#' As noted earlier, Obama's second inauguration speech was missing from
#' the website so pull the version from *obamawhitehouse*.

obamaLink <- "https://obamawhitehouse.archives.gov/the-press-office/2013/01/21/inaugural-address-president-barack-obama"
obamaHtml <- read_html(obamaLink)

obamaContent <- obamaHtml %>%
  html_nodes('div') %>%
  html_text()

obamaData <- data.frame(content = obamaContent) %>%
  summarise(content = paste(content, collapse = '')) %>%
  mutate(
         content = gsub("\n|\r", '', content),
         content = str_extract(content, 'THE PRESIDENT.*END'),
         president = 'Barack Obama',
         speech_date = 'January 21, 2013')

#' ## Now, for the Trump speech

#' We can pull a transcript from *vice.com*:

# Create an object with link
trumpLink <- "https://news.vice.com/story/full-transcript-of-donald-trumps-inauguration-speech"

# read in the html
trumpHtml <- read_html(trumpLink)

# extract the relevant text
trumpContent <- trumpHtml %>%
  html_nodes('em') %>%
  html_text()

#' the text came in on a per-line basis, so we'll have to flatten it into
#' a single string.

# flatten 
trumpContent <- data.frame(content=trumpContent) %>%
  summarise(content=paste(content, collapse=''),
            president = 'Donald J. Trump',
            speech_date = 'January 20, 2017')

#' ## Putting it all together
#' 
#' before finishing up, let's just extract the president name, and
#' speech date so that it matches up with the info we pulled for Trump.

data <- data %>%
  mutate(
    president = str_extract(content, 'Address of.*;'),
    speech_date = str_extract(content, paste0(president, '(.*)\r'))
  )

#' Finally, we can combine both datasets and output to a .csv
dataOut <- rbind(data, trumpContent, obamaData)

if(writeFiles = TRUE) {
write.csv(dataOut, 'Documents/code/inauguration_speeches/data/speech_data_dirty.csv', row.names = FALSE)
}

#+ include=FALSE
# # header info: pull out president name and date
# header <- speechHtml %>%
#   html_nodes(".header") %>%
#   html_text()
# 
# # pull out the text
# speechContent <- speechHtml %>%
#   html_nodes('.text-properties') %>%
#   html_text()
# 
# # remove the footer
# speechContent <- gsub('Inaugural Speeches Page.*', '', speechContent)
# 
# # remove header
# speechContent <- gsub('^.*Inaugural Address', '', speechContent)
# 
# # extract president name:
# presName <- str_extract(speechContent, '^(.*);') %>%
#   gsub('of|;', '', .) %>%
#   str_trim()
# 
# # extract date:
# speechDate <- str_extract(speechContent, ';(.*)\r\n') %>%
#   gsub(";|,|\r|\n", '', .) %>%
#   str_trim()
# 
# # final clean up of speech content
# speechContent  <- gsub(paste0('^.*', word(speechDate, -1)), '', speechContent) %>%
#   str_trim()