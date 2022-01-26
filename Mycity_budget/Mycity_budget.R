#load the data
data<-read.csv(file="/Users/lkm/Desktop/IDVA/mycity/pro2/Budget_Facilities_FY2017.csv",header=TRUE)
budget<-data[,-c(1,2)]
a<-factor(budget$Investment_Categorization,levels=c(80,72,69,79,65,83,70))
budget$Total_Project_Budget<-as.numeric(budget$Total_Project_Budget)

#draw the budget location
library('rgdal')
library('ggmap')
#install.packages("ggmap", type = "source")
library("ggplot2")
library('scales')
boston<-get_map('boston', zoom = 12,maptype="roadmap")
bostonmap<-ggmap('boston',extent = 'device',legend = 'topleft')
bostonmap+stat_density2d(aes(x=Longitude,y=Latitude,fill=Investment_Categorization,alpha=0.3),size=2,bins=4,data=budget,geom='polygon')
bostonmap+stat_density2d(aes(x=Longitude,y=Latitude,fill=Total_Project_Budget,alpha=0.3),size=2,bins=10,data=budget,geom='polygon')+scale_fill_gradient(low = "black", high = "red")


bostonmap<-get_map(location = c(lon = mean(budget$Longitude), lat =mean(budget$Latitude))-0.015 ,zoom = 12,maptype="roadmap")
bostonmap<-ggmap(bostonmap)
ggmap(bostonmap)+geom_point(data=budget,aes(x=Longitude,y=Latitude,size=(budget$Total_Project_Budget)/3000000,fill=budget$Investment_Categorization),shape=21,alpha=0.7)+ scale_size_continuous(range = c(0,42), breaks=pretty_breaks(5))+guides(col = guide_legend(nrow = 4))

#draw the average Total_Project_Budget for different kind of groups
par(mfrow=c(1,2))

p1<-ggplot(data=budget,aes(x=Investment_Categorization,y=Total_Project_Budget/100000000,fill=budget$Investment_Categorization))+stat_summary(fun.y="mean",geom="bar")+ coord_flip()+labs(fill='Investment Organization',y='Average Project Budget(million)')+scale_fill_brewer()+
  theme(axis.text=element_text(size=10),
         axis.title=element_text(size=14,face="bold"))

#draw the average Total_Project_Budget for different kind of groups
p2<-ggplot(data=budget,aes(x=Investment_Categorization,y=Total_Project_Budget/100000000,fill=budget$Investment_Categorization))+stat_summary(fun.y="sum",geom="bar")+ coord_flip()+labs(fill='Investment Organization',y='Total Project Budget(million)')+scale_fill_brewer(palette='green')+
 theme(axis.text=element_text(size=10),
         axis.title=element_text(size=14,face="bold"))


#draw the pie chart for the department and the organization
a<-summary(budget$City_Department)
a1<-data.frame(City_Department=names(a),nums=a)
b<-summary(budget$Investment_Categorization)
b1<-data.frame(Investment_Categorization=names(b),nums=b)
par(mfrow=c(1,2))
pie(a1[,2],labels=paste(paste(a1[,1],"\n",round(a1[,2]/nrow(budget)*100,2)),"%",sep=""),main="pie chart of City Department",col=rainbow(nrow(a1)),cex=0.7)
pie(b1[,2],labels=paste(paste(b1[,1],round(b1[,2]/nrow(budget)*100,2)),"%",sep=""),main="pie chart of Investment Categorization",col=rainbow(nrow(b1)),cex=0.7)
par(mfrow=c(1,1))


#bar chart for the neighborhood
g1<-ggplot(budget,aes(Neighborhood))+geom_bar(aes(fill=Investment_Categorization))+ coord_flip()+theme(axis.text=element_text(size=12),
                                                                                                       axis.title=element_text(size=14,face="bold"))+ scale_fill_brewer()
g1





