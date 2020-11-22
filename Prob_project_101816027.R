X <- read.csv("Test Batting Figures.csv", header = TRUE )
X$Inn <-as.numeric(X$Inn)
X$NO <-as.numeric(X$NO)
X$Runs <-as.numeric(X$Runs)
X$HS <-as.numeric(X$HS)
X$Avg <-as.numeric(X$Avg)
X$X100 <-as.numeric(X$X100)
X$X50 <- as.numeric(X$X50)
mean(X$Runs)
sum(X$Runs)
sum

#Histogram for highest scores, average of players,No. of players remained not out.
hist(X$HS,main = "Highest Score of the Players",ylim = c(0,500),col = "blue",xlab = "Highest Score")
hist(X$Avg[X$Mat>50],main = "Average of Players",col = "blue",xlab = "Average",ylab = "NO. of players",xlim = c(0,110),ylim = c(0,100))
hist(X$NO,main = "No of players reamined not out",xlab = "No. of times player remained not out",ylab = "NO. of players",col = "red",ylim = c(0,1000))
hist(X$NO[X$Mat>50],main = "No of players reamined not out",xlab = "No. of times player remained not out",ylab = "NO. of players",col = "red",ylim = c(0,200))

#plot between No. of centuries and No. of Half-Centuries
plot(X$X100,X$X50,xlab = "No. of centuries ",ylab = "No. of Half-Centuries")
#plot between No. of Matches played and No. of innings played
plot(X$Mat,xlab = "No. of Matches played",ylab = "No. of innings played")


#Pie chart of Player Played more than 50 tests
a<-table(X$Mat>50)
piepercent<- round(100*a/sum(a),1)
pie(a,labels = piepercent,main = "Player Played more than 50 tests",col = rainbow(length(a)))
legend("topright", c("<50",">50"), cex = 0.8,
       fill = rainbow(length(x)))

#pie chart- Percentage of player with Average more than 50
a<-table(X$Avg>50[X$Mat>50])
piepercent<- round(100*a/sum(a),1)
pie(a,labels = piepercent,main = "Percentage of player with Average more than 50",col = rainbow(length(a)))
legend("topright", c("Avg<50","Avg>50"), cex = 0.8,
       fill = rainbow(length(x)))

#Pie chart-Out VS Not Out
outs <- (sum(X$Inn) - sum(X$NO))
notouts <-(sum(X$Inn) -outs)
x<-(c (outs,notouts))
pie(x, main = "Out VS Not Out",col = rainbow(length(x)),radius = 1)
legend("topright", c("Out","Not Out"), cex = 0.8,
       fill = rainbow(length(x)))

#Splitting the Counries from names and finding the no. of players played for each country

  cln_data<-na.omit(X) 
  sum(is.na(cln_data)) 
  
  cnt <-str_extract(cln_data$Player,"\\([^()]+\\)")
  country<-as.data.frame( cnt ) #country names
  plr <-word(cln_data$Player,1,sep = "\\(")
  plr<-as.data.frame( plr ) #dataframe with players and countires
  sub <- data.frame(plr,country) #final table of player and the country 
  sub$cnt<-gsub("[()]", "", sub$cnt) #cleaning the country columns from ()
  sub$cnt<-gsub("ICC/", "", sub$cnt)
  sub$cnt<-gsub("/ICC", "", sub$cnt)
  cln_data$Player <- sub$plr
  cln_data$country <- sub$cnt
  head(cln_data)

  contstats <-cln_data %>% group_by(country)%>% summarise(Players = n_distinct(Player))
  contstats%>%arrange(desc(Players))
  view(contstats)


g<-mean(X$Runs)
g
#probability Distributons

      x<-X$X100
      mean(X$X100)
      y<-dnorm(X$X100,mean = 1.4,1)
      plot(x,y,xlab = "Centuries")
      
      mean(X$Mat)
      sd(X$Mat)
      
      x<-X$Mat
      y<-pnorm(x,mean = 17,25)
      plot(x,y,xlab = "No. of Matches")
      
      
      
      y<-rnorm(X$Avg)
      hist(y,main = "Normal Distribution",xlab = "Average")
      
      
      e<-X$Inn
      q<-dbinom(e,200,.5)
      plot(e,q,xlab = "200 Innings")


 

library(dplyr)
library(ggplot2)
#gg barplot for getting TOP 5 players with
#Most matches
#most runs
#most average
#most 100s and 50s
#most ducks
  mostmatch <-X %>% group_by(Player)%>% summarise(maxmatch = sum(Mat))%>% top_n(n=5)
  ggplot(mostmatch, aes(x=Player, y=maxmatch))+geom_bar(stat="identity",fill="#FF9339", colour="black")+guides(fill=FALSE)
  
  
  mostruns <-X %>% group_by(Player)%>% summarise(maxruns = sum(Runs))%>% top_n(n=5)%>%arrange(desc(maxruns))
  ggplot(mostruns, aes(x=Player, y=maxruns))+geom_bar(stat="identity",fill="#FF4339", colour="black")+guides(fill=FALSE) #Top 5 players with most runs socred
  
  which(grepl("A Bacher",X$Player))
  
  mostavg <-X %>% group_by(Player)%>% summarise(maxavg = sum(Avg[Mat>50]))%>% 
    top_n(n=5)%>%arrange(desc(maxavg))
  ggplot(mostavg, aes(x=Player, y=maxavg))+geom_bar(stat="identity",fill="#FF5293", colour="black")+guides(fill=FALSE)
  
  
  most100 <-  X %>% group_by(Player)%>% summarise(max = sum(X100))%>% top_n(n=5)%>%arrange(desc(max))
  ggplot(most100, aes(x=Player, y=max))+geom_bar(stat="identity",fill="#FF7399", colour="black")+guides(fill=FALSE)
  
  most50 <-X %>% group_by(Player)%>% summarise(max = sum(X50))%>% top_n(n=5)%>%arrange(desc(max))
  ggplot(most50, aes(x=Player, y=max))+geom_bar(stat="identity",fill="#FF4339", colour="black")+guides(fill=FALSE) 
  
  most0 <-X %>% group_by(Player)%>% summarise(max = sum(X0))%>% top_n(n=5)%>%arrange(desc(max))
  ggplot(most0, aes(x=Player, y=max))+geom_bar(stat="identity",fill="#FF4339", colour="black")+guides(fill=FALSE) #Top 5 players with most 0's socred

  
g<-mean(X$Runs)
g
#probability Distributons
  
  x<-X$X100
  mean(X$X100)
  y<-dnorm(X$X100,mean = 1.4,1)
  plot(x,y,xlab = "Centuries")
  
  mean(X$Mat)
  sd(X$Mat)
  
  x<-X$Mat
  y<-pnorm(x,mean = 17,25)
  plot(x,y,xlab = "No. of Matches")
  
  
  
  y<-rnorm(X$Avg)
  hist(y,main = "Normal Distribution",xlab = "Average")
  
  
  e<-X$Inn
  q<-dbinom(e,200,.5)
  plot(e,q,xlab = "200 Innings")
  
  
  
