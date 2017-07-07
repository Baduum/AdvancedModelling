library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

print("Reading in data")
bbl <- read_csv("C:/Users/Baduum/Desktop/R Project/billboard_lyrics_1964-2015.csv")
head(bbl)

print("Picking up the Lyrics column")
k<-as.data.frame(bbl[,5])

print("dropping the lyrics column")
bbl<-bbl[,c(1:4,6)]

class(k$Lyrics)
k$length<-1
head(k)


# Count length of each song and putting it in the column
for(i in 1: nrow(k)){
  l <- sapply(strsplit(k[i,1], " "), length)
  k[i,2]<-l
}

head(k)
bbl<-cbind.data.frame(bbl,k)
head(bbl)
str(bbl)


bbl%>%
  select(Year,length)%>%
  group_by(Year)%>%
  summarise(value=mean(length))-> bbl_avgs


print(qplot(bbl_avgs$Year,bbl_avgs$value, main = "Average words per song 1965-2015", xlab = "Year", ylab = "Number of Words")+geom_line())

t<-as.data.frame(table(bbl$Year))

head(t)



library(wordcloud)
library(tm)
library(animation)

saveGIF({
  
  for(i in 1965:2015)
  { 
    t<-bbl[which(bbl$Year==i),]
    
    clean_tw<-str_replace_all(t$Lyrics,"[^[:graph:]]", " ") 
    
    vectored_data<-Corpus(VectorSource(clean_tw))
    
    clean_tw<-tm_map(vectored_data, removePunctuation)
    clean_tw<-tm_map(clean_tw,removeWords, stopwords("english"))
    clean_tw<-tm_map(clean_tw, (tolower))
    
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, paste("Year",i))
    
    wordcloud(clean_tw, random.order=F,col=rainbow(40), max.words = 100)
  }
}, interval = 0.6, movie.name = "songs_blbrd.gif", ani.width = 980, ani.height = 720)
