Words = read.csv(file="~/Documents/Kaggle competition/Problem Analysis/query_result (1).csv",head=TRUE)
library(RColorBrewer)
library(NLP)
library(wordcloud)
library(tm)
lords <- Corpus(VectorSource(Words))
inspect(lords)
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords,PlainTextDocument)
mystopswords <- c(stopwords('english'),"available","via")
lords <- tm_map(lords, removeWords,mystopswords)
lords <- tm_map(lords, removeWords,RMlist)
lords <- tm_map(lords, stemDocument)
lords <- tm_map(lords, removeNumbers)
lords <- tm_map(lords, removePunctuation)
lords <- tm_map(lords,tolower)
ap.tdm <- TermDocumentMatrix(lords)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(Word=names(ap.v),freq=ap.v)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(ap.d$Word,ap.d$freq,scale=c(5,0.2),min.freq=10,max.words=500,random.order=FALSE,rot.per=0.15,use.r.layout=FALSE,colors=pal2)

#Remove words from RMlist
lords <- tm_map(lords, removeWords,RMlist)
#tweeter API

#API packages
install.packages(c("devtools", "rjson", "bit64", "httr"))
install_github("twitteR", username="geoffjentry")

library(devtools)
library(twitteR)
api_key <- "OzyvKENmPGtZLyCyawI0PLijE"
api_secret <- "EaRLE57E9Ys3tMp29saJuKvUpP83GL48EcgrcdCrDvCMeJQUGs"
access_token <- "2800419542-eBKK1TTb8zvjzVCXtChpKp5RY8Bpa6cfpsL57VC"
access_token_secret <- "lX8YZfHt1z8LxDOGMCYIz2MtRiM0O5AAc1Zn7vVN3lUwm"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

un.tweets = searchTwitter('@DataDataDataRob',n=1000)
un.text = lapply(un.tweets, function(t)t$getText())
write.csv(un.text,file="~/Documents/Temp/un.text.csv",row.names=FALSE)

#User Timeline Tweets
un.tweets <- {userTimeline('@DataDataDataRob', n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE, 
             excludeReplies=FALSE)
            homeTimeline(n=25, maxID=NULL, sinceID=NULL)}
df <- do.call("rbind", lapply(un.tweets, as.data.frame))
Tweets <- data.frame(df$text)
Tweets <- Corpus(VectorSource(Tweets))
inspect(Tweets)
Tweets <- tm_map(Tweets, stripWhitespace)
Tweets <- tm_map(Tweets,PlainTextDocument)
Tweets <- tm_map(Tweets, stemDocument)
Tweets <- tm_map(Tweets, removeNumbers)
Tweets <- tm_map(Tweets, removePunctuation)
Tweets <- tm_map(Tweets, tolower)

wordcloud(Tweets,scale=c(5,0.5),max.words=200,random.order=FALSE,rot.per=0.35,use.r.layout=FALSE,colors="black")



# Maps

library(maptools)
library(ggplot2)
library(ggmap)
# Plain mapping with text plots
library("maps")
map.text("state", regions=c("alabama",
                            "arizona",
                            "arkansas",
                            "california",
                            "colorado",
                            "connecticut",
                            "delaware",
                            "district of columbia",
                            "florida",
                            "georgia",
                            "idaho",
                            "illinois",
                            "indiana",
                            "iowa",
                            "kansas",
                            "kentucky",
                            "louisiana",
                            "maine",
                            "maryland",
                            "massachusetts:main",
                            "michigan:north",
                            "minnesota",
                            "mississippi",
                            "missouri",
                            "montana",
                            "nebraska",
                            "nevada",
                            "new hampshire",
                            "new jersey",
                            "new mexico",
                            "new york:main",
                            "north carolina:main",
                            "north dakota",
                            "ohio",
                            "oklahoma",
                            "oregon",
                            "pennsylvania",
                            "rhode island",
                            "south carolina",
                            "south dakota",
                            "tennessee",
                            "texas",
                            "utah",
                            "vermont",
                            "virginia:main",
                            "washington:main",
                            "west virginia",
                            "wisconsin",
                            "wyoming"), labels=as.character(c(29,
                                                              38,
                                                              13,
                                                              173,
                                                              21,
                                                              12,
                                                              5,
                                                              1,
                                                              108,
                                                              59,
                                                              7,
                                                              40,
                                                              46,
                                                              3,
                                                              9,
                                                              40,
                                                              23,
                                                              8,
                                                              24,
                                                              20,
                                                              49,
                                                              9,
                                                              14,
                                                              27,
                                                              2,
                                                              4,
                                                              12,
                                                              7,
                                                              21,
                                                              12,
                                                              60,
                                                              73,
                                                              2,
                                                              108,
                                                              18,
                                                              14,
                                                              71,
                                                              1,
                                                              35,
                                                              1,
                                                              49,
                                                              117,
                                                              10,
                                                              4,
                                                              39,
                                                              27,
                                                              18,
                                                              10,
                                                              2)))




# Sample to subset data and count 

sentence <- c('Hello World', 'Red ball and blue river', 'My laptop')
df <- data.frame(date=c(1, 2, 4), sentence=sentence)
df$sentence <- as.character(df$sentence)
df$words <- strsplit(df$sentence, ' ')

test_membership_in_list <- function(word, words) {
  return(word %in% words)
}

J <- mapply(test_membership_in_list,
       rep("blue", nrow(df)),
       df$words)

df$J <- J

str_count(df$sentence,"Hello")

# Subsetting database
words <- Words[,c(1,2)]
words$problem <- as.character(words$problem)
words$words <- strsplit(words$problem,' ')
Time <- mapply(test_membership_in_list,
            rep("time", nrow(words)),
            words$words)
words$Time <- Time
write.csv(words1,file="~/Documents/words3.csv",row.names=FALSE)

Time <- subset(words,words$words=="time")
CountTime <- str_count(words$problem,"time")
words$CountTime <- CountTime

# Subsetting database

J <- grep("mani",Words$problem)
J <- regexpr("<dd>water</dd>",Words$problem)
test <- Words
test$flag
for(i in 1:dim(test)[1]){
  for(j in 1:dim(keyword)[1]){
    test$flag[i] <- ifelse(grep(keyword[j], test$problem[i], ignore.case = TRUE), 1, 0)    
  }
}

fun <- function(x,y,pattern) y[agrep(pattern,y)[which(agrep(pattern,y) %in% sapply(x, agrep, x=y))]]
# x is a vector containing your data for filter
# y is a vector containing the data to filter on
# pattern is the quoted pattern you're filtering on
fun(temp, city, "Jan")

# readfile to be removed from Lords
RMList = read.csv(file="~/Documents/Kaggle competition/Problem Analysis/Word Suppression List.csv",head=TRUE)
RMlist <- as.list(RMList)
RMlist <- as.character(RMlist$LIST)


#extract all the states
States <- Words[,c(1)]
States1 <- as.character(States)
# remove duplicates
States1 <- unique(States1)
states2 <- data.frame(states=States1)
states2$states <- as.character(states2$states)
# create a column with each string split
states2$words <- strsplit(states2$states," ")
# create a function to extract only the last two characters
substrRight <- function(x,n=2){
  substr(x,nchar(x)-n+1,nchar(x))
  }
states3 <- substrRight(states2$words)
states3 <- unique(states3)
states4 <- data.frame(states=states3)
states <- write.csv(states4,file="~/Documents/Kaggle competition/Problem Analysis/statessubset.csv")

#old codes to be descarded
#Convert Problem column as Character
Words$problem <- as.character(Words$problem)
# Split the Setence into Word and create a list
List <- strsplit(Words$problem, " ")
Df <- data.frame(ID=rep(Words$created_at,sapply(List,length)),Words=unlist(List))
Df$Words <- gsub('[,.!]',"",Df$Words)
Df$Words <- tolower(Df$Words)
Df$Count <- rep(1,length(369864))
Plot <- data.frame(aggregate(Count~Words,data=Df,sum))
write.csv(Plot,file="~/Documents/Count.csv",row.names=FALSE)

#write a function to subset data and run the analysis
# Read Problem file
Words = read.csv(file="~/Documents/Kaggle competition/Problem Analysis/query_result.csv",head=TRUE)

# Read the list of common words to be removed
RMList = read.csv(file="~/Documents/Kaggle competition/Problem Analysis/Word Suppression List.csv",head=TRUE)
RMlist <- as.list(RMList)
RMlist <- as.character(RMlist$LIST)

#Install all the library
library(RColorBrewer)
library(NLP)
library(wordcloud)
library(tm)

#write a function to subset data and run the analysis
PA <- subset(Words,Words$state=="PA")
PA <- Corpus(VectorSource(PA))
inspect(PA)
PA <- tm_map(PA, stripWhitespace)
PA <- tm_map(PA,PlainTextDocument)
PA <- tm_map(PA, stemDocument)
PA <- tm_map(PA,tolower)
PA <- tm_map(PA, removeWords,RMlist)
PA <- tm_map(PA, removeNumbers)
PA <- tm_map(PA, removePunctuation)
PA <- tm_map(PA,PlainTextDocument)
PA.tdm <- TermDocumentMatrix(PA)
PA.m <- as.matrix(PA.tdm)
PA.v <- sort(rowSums(PA.m),decreasing=TRUE)
PA.d <- data.frame(Word=names(PA.v),freq=PA.v)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(PA.d$Word,PA.d$freq,scale=c(5,0.2),min.freq=10,max.words=500,random.order=FALSE,rot.per=0.15,use.r.layout=FALSE,colors=pal2)

#write a function to subset data based on words
State ='NM'
Word = 'juice'
PA <- subset(Words,Words$state==State)
PA$problem <- tolower(PA$problem)
PA$problem <- as.character(PA$problem)
PA$words <- strsplit(PA$problem,' ')
PA.2 <- as.matrix(PA$problem [grep(Word,PA$words,ignore.case=TRUE)])
PA.2 <- data.frame(PA.2)
colnames(PA.2)[1]<- "Problem_description"
write.csv(PA.2,file="~/Documents/Kaggle competition/Problem Analysis/NM.csv",row.names=FALSE)
