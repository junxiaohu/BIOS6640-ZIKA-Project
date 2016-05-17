install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc") 
library("twitteR")
library("ROAuth")
library("base64enc")

library(RCurl)
library(stringr)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(SnowballC)

library(maptools)
library(ggplot2)
setwd
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

download.file(url="https://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions

setup_twitter_oauth(consumer_key='AmpoqFIoxfdosgi3lAHH8LC6e', 
                          consumer_secret='hOUYEiBXNpprEPei28dWjZpzVSBAOFSyd9RPk09oJbKnHh8SPN', 
                          access_token='2503525646-POCXZkyasjN5Zg7U3AmSDpGTNHh7Px1wYpjPxd9', 
                          access_secret='QYrweE4h4g8FoR3vcwoMt1Zfo47gPiHZut5okkQxtkJeE')

N=2000  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

cities=c("DC","New York","San Fransisco","Colorado","Mountainview","Tampa","Austin","Boston",
       "Seatle","Vegas","Montgomery","Phoenix","Little Rock","Atlanta","Springfield",
       "Cheyenne","Bisruk","Helena","Springfield","Madison","Lansing","Salt Lake City","Nashville",
       "Jefferson City","Raleigh","Harrisburg","Boise","Lincoln","Salem","St. Paul")

citylist <- cbind(cities,lats,lons)
write.csv(citylist, file="Supplement1.csv")

zika=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Zika',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))
setwd('/Users/Junxiao/Documents/BIOS6640/zika')
save(zika, file='zikadata')
load('zikadata')
zikalat=sapply(zika, function(x) as.numeric(x$getLatitude()))
zikalat=sapply(zikalat, function(z) ifelse(length(z)==0,NA,z))  

zikalon=sapply(zika, function(x) as.numeric(x$getLongitude()))
zikalon=sapply(zikalon, function(z) ifelse(length(z)==0,NA,z))  

zikadate=lapply(zika, function(x) x$getCreated())
zikadate=sapply(zikadate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

zikatext=sapply(zika, function(x) x$getText())
zikatext=unlist(zikatext)

isretweet=sapply(zika, function(x) x$getIsRetweet())
retweetcount=sapply(zika, function(x) x$getRetweetCount())

favoritecount=sapply(zika, function(x) x$getFavoriteCount())

data=as.data.frame(cbind(tweet=zikatext,date=zikadate,lat=zikalat,lon=zikalon,
                         isretweet=isretweet, retweetcount=retweetcount,favoritecount=favoritecount))

#Clean text
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data$tweet=tweet

#Create word cloud
# Create corpus
corpus=Corpus(VectorSource(data$tweet))

#Get rid of non standard character
corpus=tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

# Convert to lower-case
corpus=tm_map(corpus,tolower, mc.cores=1)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(4,1.4),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)


#lexicon based sentiment analysis. Use a list of positive and negative opinion words or sentiment words in English
#to aquire a sentiment score

positives= readLines("positivewords.txt")
negatives = readLines("negativewords.txt")
#calculates sentiment scores.

scorefun = function(tweet, positive_words=positives, negative_words=negatives){
  tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
  tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
  tweet = gsub('\\d+', '', tweet)          # remove digits
  
  # Let's have error handling function when trying tolower
  tryTolower = function(x){
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # use tryTolower with sapply
  tweet = sapply(tweet, tryTolower)
  # split sentence into words with str_split function from stringr package
  word_list = str_split(tweet, "\\s+")
  words = unlist(word_list)
  # compare words to the dictionaries of positive & negative terms
  positive.matches = match(words, positive_words)
  negative.matches = match(words, negative_words)
  # get the position of the matched term or NA                  
  positive_matches = !is.na(positive.matches)
  negative_matches = !is.na(negative.matches)
  # final score
  score = sum(positive_matches) - sum(negative_matches)
  return(score)
}
scorefun(data$tweet[1])
scores = lapply(data$tweet, scorefun)
date <- strptime(data$date, "%Y-%m-%d %H:%M:%S")
data$score=unlist(scores) 
data$time=date
byhour = format(date, "%H")
byday = format(date, "%d")
data$byhour=byhour
data$byday=byday
save(data, file="finaldata")
load("finaldata")

library(plyr)
#present data
#time series count of tweets by day abd by hour
tbyday <- ddply(data, ~ byday, nrow)
colnames(tbyday) <- c("day", 'count')
tweetsday <- ts(tbyday$count, start=18, end=27)
plot.ts(tweetsday)

colnames(data)

sumdat <- data[,c("isretweet", "retweetcount", "favoritecount", "score"   )]
summary(sumdat)

#plot data
#score
hist(sumdat$score, main = "sentiment score histogram",
     xlab = "sentiment score", ylab = "Frequency")
#retweetcount
hist(as.numeric(sumdat$retweetcount), main = "Count of retweet histogram",
     xlab = "count of retweet", ylab = "Frequency")
#favoritecount
hist(as.numeric(sumdat$favoritecount), main = "Count of favorite histogram",
     xlab = "count of favorite", ylab = "Frequency")

#create map
library(ggplot2)
library(maps)
library(mapproj)
library(rworldmap)
newmap <- getMap(resolution = "li")
oldpar <- par(mar=rep(0,4))
plot(newmap, asp=1)
points(data$lon, data$lat, col = "red", cex = .6)
par(oldpar)

#get Address
mapdata=filter(data[,1:4], !is.na(lat),!is.na(lon))
lonlat=select(mapdata,lon,lat)
write.csv(lonlat, file="supplement2.csv")
result <- do.call(rbind, lapply(1:nrow(lonlat),
                                function(i) revgeocode(as.numeric(lonlat[i,1:2]))))

write.csv(result, file="supplement3.csv")
