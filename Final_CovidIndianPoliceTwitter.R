library(httr)
library(tidyverse)
library(jsonlite)
library(rtweet)
library(tidyverse)
library(tidytext)
library(dplyr)
library(lubridate)
library(SnowballC)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(stringr)
library(ggsci) # for ggplot palettes

#### The first part of the code has been copied from Christopher Barrie's open source code
#### to collect tweets using V2 Academic Research Product Track. 
### The Github for reference is https://github.com/cjbarrie/academictwitteR

## Academic Track twitter scraping
#create folders for storage
ifelse(!dir.exists(file.path("data/")),
       dir.create(file.path("data/"), showWarnings = FALSE),
       FALSE)
ifelse(!dir.exists(file.path("includes/")),
       dir.create(file.path("includes/"), showWarnings = FALSE),
       FALSE)

#store bearer token for Academic product track v2
bearer_token = "copy your academic track bearer toeken"
source("funs/get_tweets.R")

##get India police agencies twitter handles
users <- c("MumbaiPolice","AhmedabadPolice",
           "BlrCityPolice","chennaipolice_",
           "CP_SuratCity","DelhiPolice",
           "hydcitypolice","jaipur_police",
           "KolkataPolice","PuneCityPolice")

#get tweets for the specific timelinen
start_tweets = "2020-03-25T00:00:00Z"
end_tweets = "2020-06-01T00:00:00Z"
nextoken <- ""
i <- 1

while (!is.null(nextoken)) {
  query <- paste0('from:', users[[i]])
  userhandle <- users[[i]]
  df <-
    get_tweets(
      q = query ,
      n = 500,
      start_time = start_tweets,
      end_time = end_tweets,
      token = bearer_token,
      next_token = nextoken
    )
  jsonlite::write_json(df$data,
                       paste0("data/", "data_", userhandle, df$data$id[nrow(df$data)], ".json"))
  jsonlite::write_json(df$includes,
                       paste0("includes/", "includes_", userhandle, df$data$id[nrow(df$data)], ".json"))
  nextoken <-
    df$meta$next_token #this is NULL if there are no pages left
  cat(query, ": ", "(", nrow(df$data), ") ", "\n", sep = "")
  Sys.sleep(3.1) #sleep between calls to avoid rate limiting
  if (is.null(nextoken)) {
    cat("next_token is now NULL for",
        userhandle,
        " moving to next account \n")
    nextoken <- ""
    i = i + 1
    if (i > length(users)) {
      cat("No more accounts to capture")
      break
    }
  }
}

#parse and bind
files <-
  list.files(
    path = file.path("data/"),
    recursive = T,
    include.dirs = T
  )
files <- paste("data/", files, sep = "")

pb = txtProgressBar(min = 0,
                    max = length(files),
                    initial = 0)

df.all <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  df <- read_json(filename, simplifyVector = TRUE)
  df.all <- bind_rows(df.all, df)
  setTxtProgressBar(pb, i)
}

saveRDS(df.all, file = "meril_police_tweets_feb24.rds")
## combining the public metrics
public_metrics <- df.all$public_metrics

df_final <- df.all %>% select(lang,conversation_id,author_id,
                              id, text, created_at,in_reply_to_user_id) %>% 
  cbind(public_metrics)
## save the raw data file
write_csv(df_final, "cleaned_full_tweets_police.csv")

##### Next Step 


#Read the csv file
final.data <- read_csv("Data/cleaned_full_tweets_police.csv")

## seperating the date and hour
final.data$created_at_new <- lubridate::ymd_hms(final.data$created_at)
final.data$created_at_date <- ymd(paste0(year(final.data$created_at_new),"-",
                                         month(final.data$created_at_new),"-",
                                         day(final.data$created_at_new)))
final.data$created_at_time <-  strftime(final.data$created_at_new, format="%H:%M:%S")

#only english tweets 
final.data <- final.data %>% filter(final.data$lang == 'en')

#only original tweets without the reply to userid
final.data.original <- final.data %>% filter(is.na(final.data$in_reply_to_user_id))
write.csv(final.data.original, "Data/final_data_originaltweets.csv")

##To convert author Id to twitter handles, we used www.tweeterid.com to find the handle. 
data <- read_csv("Data/final_data_originaltweets.csv")
##original data = 4729 observations 
data.eng <- data %>% filter(data$lang == 'en')
##only english == 3227 observations 

unique(data.eng$author_id)
#775296019 => @BlrCityPolice
#1406885239 => @chennaipolice_
#1850705408 => @DelhiPolice
#2842306788 => @hydcitypolice
#3412395613 => @KolkataPolice
#4573405572 => @MumbaiPolice
#4707327216 => @PuneCityPolice
#959681983755546000 ->  @AhmedabadPolice
#713285748393259000 >- @CP_SuratCity
#778576411653124000 >- jaipur_police

data.eng <- data.eng %>% add_column(Police = NA)
##All states added
data.eng$Police <- ifelse(data.eng$author_id == 775296019 , 'Bangalore Police', 
                          ifelse(data.eng$author_id == 1406885239 , 'Chennai Police', 
                                 ifelse(data.eng$author_id == 1850705408 , 'Delhi Police',
                                        ifelse(data.eng$author_id == 2842306788, 'Hydrabad Police',
                                               ifelse(data.eng$author_id == 3412395613, 'Kolkata Police',
                                                      ifelse(data.eng$author_id == 4573405572, 'Mumbai Police',
                                                             ifelse(data.eng$author_id == 4707327216, 'Pune Police',
                                                                    ifelse(data.eng$author_id == 9.59682E+17,'Ahmedabad Police',
                                                                           ifelse(data.eng$author_id == 7.13286E+17, 'Surat Police',
                                                                                  ifelse(data.eng$author_id == 7.78576E+17, 'Jaipur Police', 'NA'))))))))))

unique(data.eng$Police)
##Added manually to change the author id to twitter handle

### Need to create date breaks in the form of the column date_period
#### date_period = 1 for March 25 - April 14
#### date_period = 2 for April 15 - May 3 
#### date_period = 3 for May 4 - May 17
#### date_period = 4 for May 18 - May 31

final.data <- final.data %>% 
  mutate(date_period=case_when(
    (created_at_date>=as.Date("2020-03-25") & 
       final.data$created_at_date<=as.Date("2020-04-14"))~"1",
    (created_at_date>=as.Date("2020-04-15") & 
       final.data$created_at_date<=as.Date("2020-05-03"))~"2",
    (created_at_date>=as.Date("2020-05-04") & 
       final.data$created_at_date<=as.Date("2020-05-17"))~"3",
    (created_at_date>=as.Date("2020-05-18") & 
       created_at_date<=as.Date("2020-05-31"))~"4",
    TRUE~"0"
  ))


# now creating the dictionaries for analysis
patrol_dictionary <- c("police", "control", "main", "branch", "report", "official", "exact", "locate",
                       "lockdown", "action")

information_dictionary <- c("please", "share", "traffic", "information", "details", "police", 
                            "whatsapp", "help", "matter", "concern", "travel", "guidelines", 
                            "local", "forward")

covid_dictionary <- c("covid", "covid-19", "corona", "coronavirus", "epidemic", "pandemic", 
                      "virus", "lockdown", "shelter", "stayathome", "shelterinplace", "test", 
                      "testing", "hospital","spread", "sanitizer", "stay", "home", "health", 
                      "cases", "public", "protect", "reopen" ,"reopening", "quarantine",
                      "socialdistance", "social", "distance", "mask", "covid19","safe","recovery", 
                      "vaccine", "cdc", "essential", "workers", "crisis", "briefing","wear","daily",
                      "screening", "symptomatic", "asymptomatic", "ventilator", "incubation", 
                      "schools", "closing", "newnormal", "fatalities", "death", "infection", "infected",
                      "mers", "china", "wuhan", "respiratory","cough", "fever", "breath", "breathing",
                      "novel", "frontline", "distancing", "fauci", "isolation", "outbreak", "mandatory",
                      "cluster", "curfew", "transmission", "order", "curve", "flatten", "flattening", 
                      "hygiene", "ppe", "n95", "n-95", "federal", "funding", "relief", "package", 
                      "unemployment", "claims", "doctor", "dhs", "department", "contagious")

## now creating the dictionaries from the list of words

dic_patrol <- paste(patrol_dictionary, collapse ="|")

dic_info <- paste(information_dictionary, collapse = "|")

dic_covid <- paste(covid_dictionary, collapse = "|")

## now filtering the tweets based on the dictionaries we have created so far

city_dic_covid <- final.data %>% 
  filter(str_detect(text,dic_covid))

city_dic_covid_info <- final.data %>% 
  filter(str_detect(text,dic_info)&str_detect(text,dic_covid))

city_dic_covid_patrol <- final.data %>% 
  filter(str_detect(text,dic_patrol)&str_detect(text,dic_covid))


## Summaries Data only by date period and not screen name
covid.summary <- city_dic_covid %>% group_by(date_period) %>% 
  summarize(no.tweets=n(),
            total.retweets=sum(retweet_count),
            avg.retweet=round(sum(retweet_count)/n(),0),
            total.likes = sum(like_count),
            avg.like=round(sum(like_count)/n(),0)) 
covid.summary$dic.type <- "covid"
write_csv(covid.summary, path = "Output/covid_summary.csv")



covid.patrol.summary <- city_dic_covid_patrol %>% group_by(date_period) %>% 
  summarize(no.tweets=n(),
            total.retweets=sum(retweet_count),
            avg.retweet=round(sum(retweet_count)/n(),0),
            total.likes = sum(like_count),
            avg.like=round(sum(like_count)/n(),0)) 

covid.patrol.summary$dic.type <- "covid-patrol"
write_csv(covid.patrol.summary, path = "Output/covid_patrol_summary.csv")


covid.info.summary <- city_dic_covid_info %>% group_by(date_period) %>% 
  summarize(no.tweets=n(),
            total.retweets=sum(retweet_count),
            avg.retweet=round(sum(retweet_count)/n(),0),
            total.likes = sum(like_count),
            avg.like=round(sum(like_count)/n(),0)) 

covid.info.summary$dic.type <- "covid-info"
write_csv(covid.info.summary, path = "Output/covid_info_summary.csv")


all.dic.summaries <- data.frame(rbind(covid.summary, covid.patrol.summary, covid.info.summary))
write_csv(all.dic.summaries, path = "Output/totalsummary.csv" )


##LineGraphs
##First: Covid related tweets 
covid.summary$date_period <- as.numeric(as.character(covid.summary$date_period))
ggplot(covid.summary, aes (date_period,no.tweets)) +
  geom_line() + 
  ggtitle("Number of COVID related Tweets")

#plot
ggplot(covid.summary, aes(x=date_period, y=no.tweets, fill=factor(dic.type))) +
  geom_bar(stat="identity")

##Second: No of tweets for all three types
#Step 1: convert date period to numeric
all.dic.summaries$date_period <- as.numeric(as.character(all.dic.summaries$date_period))
ggplot(all.dic.summaries, aes (date_period, no.tweets, fill=factor(dic.type))) +
  geom_line(aes(linetype=dic.type, color=dic.type)) + 
  ggtitle("Number of Tweets based on Dictionary Filter")

## Third: No of retweets based for all three types
ggplot(all.dic.summaries, aes (date_period, total.retweets, fill=factor(dic.type))) +
  geom_line(aes(linetype=dic.type, color=dic.type)) + 
  ggtitle("No. of Re-tweets based on Dictionary Filter")


## Fourth: No of likes based on all three types
ggplot(all.dic.summaries, aes (date_period, total.likes, fill=factor(dic.type))) +
  geom_line(aes(linetype=dic.type, color=dic.type)) + 
  ggtitle("No. of likes based on Dictionary Filter")

##plots
covid.summary$date_period <- as.numeric(as.character(covid.summary$date_period))

#plot
ggplot(covid.summary, aes(x=date_period, y=no.tweets, fill=factor(Police))) +
  geom_line(aes(linetype = Police, color = Police))+
  ggtitle("Number of COVID related Tweets by State")
#plot
covid.patrol.summary$date_period <- as.numeric(as.character(covid.patrol.summary$date_period))
ggplot(covid.patrol.summary, aes(x=date_period, y=no.tweets, fill=factor(Police))) +
  geom_line(aes(linetype = Police, color = Police))+
  ggtitle("Number of PATROL related Tweets by State")
#plot
covid.info.summary$date_period <- as.numeric(as.character(covid.info.summary$date_period))
ggplot(covid.info.summary, aes(x=date_period, y=no.tweets, fill=factor(Police))) +
  geom_line(aes(linetype = Police, color = Police))+
  ggtitle("Number of INFORMATION related Tweets by State")
