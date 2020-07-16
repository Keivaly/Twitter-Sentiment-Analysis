install.packages("httpuv")
## load rtweet
install.packages("rtweet")
install.packages("httr")
library(httr)
library(rtweet)
library(httpuv)
library(dplyr)
library(tidyr)
install.packages("tidytext")
library(tidytext)
library(ggplot2)
install.packages("twitteR")
library(twitteR)
library(purrr)
## store api keys
api_key <-"BACwDEh4S3AUM87t8qUEu9j6e"
api_secret_key<-"55fRoW5mZ6D9jdA5yNEGn2eOcD7ptV05AXl2CDhhgMeWZHopW9"
access_token <-"345842417-zZQY2MdetjtBeESPQqVUjjqeGXWhXbAfXUddqUfn"
access_token_secret <-"LNUg0Llvr1wMOKziSBrxJkskLOf6eUkugy1QLrpUn5FUe"
## authenticate via web browser
token <- create_token(
  app = "antibodyresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## authenticate via web browser
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

get_token()

dat1<-search_tweets("antibody test", n=100, include_rts = FALSE)
dat1
tweets.dat1=dat1%>%select(screen_name,text)

head(tweets.dat1$text)
#remove http elements manually
tweets.dat1$stripped_text1<-gsub("http|t.co|19|positive|negative|virus","",tweets.dat1$text)
# remove punctuation and add id for each tweet also use unnest_tokens() function to convert into lower case
tweets.dat1_stem<-tweets.dat1%>%select(stripped_text1)%>%unnest_tokens(word,stripped_text1)

head(tweets.dat1_stem)
#remove stop words from your list of words
cleaned_tweets.dat1<-tweets.dat1_stem%>%anti_join(stop_words)
head(cleaned_tweets.dat1)

head(tweets.dat1$text)

#Top 20 words in #antobodytest
cleaned_tweets.dat1%>%
  count(word,sort = TRUE)%>%
  top_n(20)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(x=word,y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x="Count",
       y="Unique words",
       title="Unique words found in #antibodytest")

#bing sentiment analysis 
bing_dat1<-cleaned_tweets.dat1%>%inner_join(get_sentiments("bing"))%>%count(word,sentiment,sort=TRUE)%>%ungroup()
bing_dat1

bing_dat1%>%group_by(sentiment)%>%top_n(20)%>%ungroup()%>%mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y")+
  labs(title = "Tweets containing 'antibody test'",
       y="Contribution to sentiment",
       x=NULL)+
  coord_flip()+theme_bw()

#function for assigning score(snt) to each tweet

sentiment_bing=function(twt){
  
 #step 1: perform basic text cleaning (on the tweet)
  twt_tbl=tibble(text=twt)%>%
    mutate(
      #remove http elements manually
      stripped_text = gsub("http|t.co|19|positive|negative|virus","",text)
    )%>%
    unnest_tokens(word,stripped_text)%>%
    anti_join(stop_words)%>%
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment,sort=TRUE)%>%
    ungroup()%>%
    #create a column "snt" that assigns a -1 to all negative words and 1 to all positive words.
    mutate (
      snt = case_when(
        sentiment=='negative'~n*(-1),
        sentiment=='positive'~n*1)
      )
  #calculate total snt
  sent.snt=case_when(
    nrow(twt_tbl)==0~0, #if there are no words, snt is 0
    nrow(twt_tbl)>0~sum(twt_tbl$snt)#otherwise sum the negative and positives
  )
  #this is to keep track of which tweets contained no words at all from this bing list
  zero.type=case_when(
    nrow(twt_tbl)==0~"Not Classified",
    nrow(twt_tbl)>0~"Classified"
  )
  list(snt = sent.snt, type=zero.type, twt_tbl=twt_tbl)
}

#Applying the function on to assign score to each tweet in dat1
dat1_sent=lapply(dat1$text,function(x){sentiment_bing(x)})

dat1_sent[2566]
length(dat1_sent)

#unlisting dat1_sent into a tibble for plotting
dat1_sentiment=tibble(keyword="antibody test",
  snt=unlist(map(dat1_sent,'snt')),
  type=unlist(map(dat1_sent,'type'))
  )


#Final histogram for taking a call on which way the sentiments is heavier
ggplot(dat1_sentiment,aes(x=snt))+geom_histogram(bins=15,alpha=0.6,fill="orange")
