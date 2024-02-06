
require(ggplot2)

#load data and merge to one data table
hay.2022 <- read.table("Data\\2022_HAY2201.csv", header=FALSE, sep=",")
hay.2021 <- read.table("Data\\2021_HAY2201.csv", header=FALSE, sep=",")
hay.2020 <- read.table("Data\\2020_HAY2201.csv", header=FALSE, sep=",")
hay.2019 <- read.table("Data\\2019_HAY2201.csv", header=FALSE, sep=",")
hay.2018 <- read.table("Data\\2018_HAY2201.csv", header=FALSE, sep=",")
hay.2017 <- read.table("Data\\2017_HAY2201.csv", header=FALSE, sep=",")
hay.2016 <- read.table("Data\\2016_HAY2201.csv", header=FALSE, sep=",")
hay.2015 <- read.table("Data\\2015_HAY2201.csv", header=FALSE, sep=",")
hay.2014 <- read.table("Data\\2014_HAY2201.csv", header=FALSE, sep=",")

hay.all <-rbind(hay.2022[,1:4],hay.2021[,1:4],hay.2020[,1:4],hay.2019[,1:4],hay.2018[,1:4],hay.2017[,1:4],hay.2016[,1:4],hay.2015[,1:4],hay.2014[,1:4])

#add headings and extra columns
hay.all[,2]<-as.Date(hay.all[,2],"%d/%m/%Y")
hay.all<-cbind(hay.all,format(hay.all[,2],"%d"),
  format(hay.all[,2],"%m"),format(hay.all[,2],"%Y"))

colnames(hay.all)<-c("GPX","Date","Time","Price","Day","Month","Year")
hay.all$Date<-as.Date(hay.all$Date,"%d/%m/%Y")
hay.all$Time <- hay.all$Time/2


#Daily usage profile

hay.usage<-cbind(c(1:48)/2,
  c(0.51,0.51,0.20,0.20,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
    0.10,0.10,1.65,1.65,1.02,1.02,0.00,0.00,0.00,0.00,0.00,0.00,
    0.00,0.00,0.36,0.36,0.00,0.00,0.00,0.00,0.00,0.00,0.50,0.50,
    0.24,0.24,0.36,0.36,0.06,0.06,0.00,0.00,0.00,0.00,0.00,0.00)/2,
  c(0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.01,0.03,0.03,0.01,0.01,
    0.09,0.09,0.20,0.20,0.10,0.10,0.04,0.04,0.03,0.03,0.01,0.01,
    0.01,0.01,0.20,0.20,0.18,0.18,0.01,0.01,0.02,0.02,0.20,0.20,
    0.20,0.20,0.20,0.20,0.20,0.20,0.07,0.07,0.04,0.04,0.02,0.02)/2)

#Calculate proportions and add headers

hay.usage.prop <-hay.usage[,2]+hay.usage[,3]
prop.table(hay.usage.prop)
hay.usage<-cbind(hay.usage,prop.table(hay.usage.prop))
colnames(hay.usage)<-c("Time","Controlled","Uncontrolled","Proportion")
rm(hay.usage.prop)

#Cost functions A wholesale

whs_avg_rate<-function(whs_rate,prop){
   prop*whs_rate/1000} 

kw_cost<-function(whs_rate,cont_used,uncont_used) {
   cont_used*(0.0558+0.0013+0.03+(whs_rate/1000))+
   uncont_used*(0.1158+0.0013+0.03+(whs_rate/1000))}

day_cost<-function(whs_cost,days) {
   (days*(0.15+0.15)+whs_cost)*1.15} 

#cost functions fixed rate
kw_cost_B<-function(cont_used,uncont_used) {
  (cont_used*(.175+.0013)+uncont_used*(.223+.0013))
}

day_cost_B<-function(unit_cost,days){
  (days*0.30+unit_cost)*1.15
}

#Calculate daily cost

hay.all<-merge(hay.all,hay.usage, by.x="Time", by.y="Time")
hay.all<-cbind(hay.all, kw_cost(hay.all$Price,hay.all$Controlled,
  hay.all$Uncontrolled),whs_avg_rate(hay.all$Price,hay.all$Proportion))
hay.all<-cbind(hay.all, kw_cost_B(hay.all$Controlled,
  hay.all$Uncontrolled))

colnames(hay.all)<-c(colnames(hay.all[,1:10]),"Cost","Day_Avg_Rate","c_unit_cost")

hay.day<-merge(
  aggregate(hay.all$Cost, by=list(Category=hay.all$Date), FUN=sum),
  aggregate(hay.all$Day_Avg_Rate, by=list(Category=hay.all$Date), FUN=sum),
  by.x="Category", by.y="Category")

hay.day<-merge(
   hay.day,
    aggregate(hay.all$c_unit_cost, by=list(Category=hay.all$Date), FUN=sum),
  by.x="Category", by.y="Category")

hay.day<-cbind(hay.day, day_cost(hay.day[,2],1))
hay.day<-cbind(hay.day, day_cost_B(hay.day[,4],1))

colnames(hay.day)<-c("Date","Cost","Day_Avg_Rate","B_unit_Cost","A_Total","B_Total")

#Analyis
hay.ytd<-(subset(hay.day, Date >="2017-01-01"))
aggregate(hay.ytd$A_Total, by=list(Category=format(hay.ytd$Date,"%Y%m")), FUN=sum)
aggregate(hay.ytd$Day_Avg_Rate, by=list(Category=format(hay.ytd$Date,"%Y%m")), FUN=mean)
aggregate(hay.ytd$B_Total, by=list(Category=format(hay.ytd$Date,"%Y%m")), FUN=sum)


hay.cpd<-aggregate(hay.ytd$A_Total, by=list(Category=format(hay.ytd$Date,"%Y%m")), FUN=sum)
hay.cpd<-cbind(hay.cpd,aggregate(hay.ytd$B_Total, by=list(Category=format(hay.ytd$Date,"%Y%m")), FUN=sum))
colnames(hay.cpd)<-c("Date","A_Cost","Date","B_Cost")
hay.cpd<-cbind(hay.cpd,(hay.cpd[,3] - hay.cpd[,6])
               
               
#Charts 
#Histogram of Daily Cost
  ggplot(hay.ytd, aes(A_Total)) +
 # + facet_wrap(~variable, scales = 'free_x')
  geom_histogram(binwidth = 0.1)

#line chart of Daily Cost
ggplot(data=hay.day, aes(x=Date,y=A_Total)) +
geom_line()

ggplot(data=hay.day, aes(x=Date,y=Day_Avg_Rate)) +
geom_line()

#box plot by month
qplot(Month, Price, data=hay.all[,c(4:7)],facets=~Year,ylim=c(-100,1000))



