data<-read.csv("/Users/lkm/Desktop/IDVA/final/consumer_complaints.csv",header = TRUE)

#choose sample from the data
text<-data$Consumer.complaint.narrative[sample(1:nrow(data),50000)]
library(tm)
dtm<-data.frame(text)
mycorpus<-Corpus(DataframeSource(dtm))
tdm<-TermDocumentMatrix(mycorpus, control = list(removePunctuation = TRUE, stopwords = TRUE))
inspect(tdm)

#word frequence
freq <- rowSums(as.matrix(tdm,stringsAsFactors=FALSE))
a<-as.matrix(tdm,stringsAsFactors=FALSE)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)
#get the frequency rank
library(ggplot2)
wfr<-wf[order(-freq),]
p<-ggplot(subset(wfr, freq>5000),aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity",fill = "steelblue") + 
  theme(axis.text.x=element_text(angle=45, hjust=1,size=15),axis.text.y=element_text(size=15),plot.title = element_text(lineheight=3, face="bold", color="black", size=25),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18))+
  labs(x='Words',y='Frequency')+
  ggtitle("The words with Freq>50")
p


#word clouds occurs at least 20 times
library(wordcloud)
library(RColorBrewer)
wordcloud(names(freq), freq, min.freq=20,scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 


bp.cols<- c("light blue","cornflowerblue", "coral2", brewer.pal(8,"Dark2"))
wordcloud(names(freq), freq,
          scale=c(5.5,0.5), 
          max.words=100, 
          min.freq=20, 
          random.order=FALSE, 
          rot.per=0.40, 
          use.r.layout=FALSE, 
          random.color=TRUE, 
          colors=bp.cols)

#plot the piechart
tags<-data$Tags[data$Tags!=""]
k<-data.frame(summary(tags))
slices<-k[-1,1]
lbls<-rownames(k)[-1]
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels=lbls,main="Pie chart of Tags",col=terrain.colors(5))


#plot the location 
statedata<-data.frame(summary(data$State),stringsAsFactors = FALSE)
inputs<-data.frame(cbind(rownames(statedata),statedata[,1])[-1,],stringsAsFactors = FALSE)
inputs$X1<-as.character(inputs$X1)
inputs$X2<-as.numeric(inputs$X2)
k<-data.frame(state.abb)
common<-inputs[match(k[,1],inputs[,1]),]
common<-na.omit(common)
colnames(common)<-c("region","records_count")
library(ggplot2)
library(openintro)
library(lettercase)
states<-map_data("state")
common[,1]<-abbr2state(common[,1])
common[,1]<-str_lowercase(common[,1])
common[,2]<-common[,2]
# Then we merge our dataset with the geospatial data:
sim_data_geo <- merge(states, common, by="region")
# The following should give us the plot without the numbers: 
snames <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
# Then again, we need to join our original dataset 
# to get the value that should be printed at the center.
snames <- merge(snames, common, by="region")
snames$region<-state2abbr(snames$region)
p<-ggplot(sim_data_geo, aes(long, lat)) + geom_polygon(aes(group=group, fill=records_count)) + geom_text(data=snames, aes(long, lat, label=region),col="white")+ggtitle("Number of Records from Different State")
p + theme(
  plot.title = element_text(color="red", size=14, face="bold.italic",hjust = 0.5),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="blue", size=14, face="bold")
)
