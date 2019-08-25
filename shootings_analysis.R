#####################################################
# Analysis of school shootings from 1990 to present #
#####################################################

#load packages
library(ggplot2)
library(scales)
library(maps)
library(tm)
library(plyr)

#import data
df2 <- read.csv("C:/workspaceR/school_shootings/shootings.csv", header=FALSE, stringsAsFactors=FALSE)

#clean header
df2[1,1] <- "Date"
names(df2) <- df2[1,]
df2 <- df2[-1,]

#set row number
row.names(df2) <- 1:nrow(df2)

#reformat date to YYYY-MM-DD
df2$Date <- as.Date(df2$Date, format='%m/%d/%y')


# 1. Descriptives of df2$school
df2$School <- as.factor(df2$School)
levels(df2$School)
levels(df2$School)[match("-", levels(df2$School))] <- NA
levels(df2$School)[match("", levels(df2$School))] <- NA

df2$School <- factor(df2$School, levels = c("ES", "MS", "HS", "C"))
levels(df2$School)[levels(df2$School) == "ES"] <- "Elementary"
levels(df2$School)[levels(df2$School) == "MS"] <- "Middle"
levels(df2$School)[levels(df2$School) == "HS"] <- "High"
levels(df2$School)[levels(df2$School) == "C"] <- "College"

table(df2$School)

par(mar=c(5,5,5,3))
plot(df2$School, main = "Type of school where shooting occured", ylim=c(0,500), ylab="Incidences")


# 2. Plot Date by Number_shot
df2$Fatalities <- as.numeric(df2$Fatalities)
df2$Wounded <- as.numeric(df2$Wounded)
df2$Number_shot <- rowSums(df2[,c("Fatalities", "Wounded")], na.rm = TRUE)

p2 <- ggplot(data = df2, aes(Date, Number_shot)) + geom_line() +ylim(0,60)
p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_date(labels = date_format("%Y"), breaks='1 years') + 
  ggtitle("Time series: Number shot") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Number shot")


hist(df2$Number_shot, 
     main = "Histogram: Number of people shot",
     xlab = "Number shot",
     xlim = c(0,60),
     ylim = c(0,350),
     las = 1,
     breaks = 80)

table(df2$Number_shot)


# 3. Map shooting by state
df3 <- ddply(df2, .(State), function(df2) c(Count=nrow(df2)))

df3$region <- tolower(df3$State)
states <- map_data("state")
map.df <- merge(states,df3, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Count))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()+ggtitle("Shooting occurances by state")+theme(plot.title = element_text(hjust = 0.5))


# 4. Text analysis of shooting description
text_desc <- paste(df2$Desc, collapse = " ")
corpus_desc <- Corpus(VectorSource(text_desc))
corpus_desc <- tm_map(corpus_desc, content_transformer(tolower))
corpus_desc <- tm_map(corpus_desc, removePunctuation)
corpus_desc <- tm_map(corpus_desc, stripWhitespace)
corpus_desc <- tm_map(corpus_desc, removeWords, stopwords("en"))

dtm_desc <- TermDocumentMatrix(corpus_desc)
m_desc <- as.matrix(dtm_desc)
v_desc <- sort(rowSums(m_desc),decreasing=TRUE)
d_desc <- data.frame(word = names(v_desc),freq=v_desc)
row.names(d_desc) <- 1:nrow(d_desc)
d_desc <- d_desc[-c(10,19,31), ]
head(d_desc, 15)
par(mar=c(5,5,5,5))
barplot(d_desc[1:15,]$freq, las = 2, names.arg = d_desc[1:15,]$word,
        col ="gray", main ="Most common words in shooting description",
        ylab = "Word frequencies", ylim=c(0,400))


df_desc_1 <- df2[grep("arrested",df2$Desc),] #examine events associated arrested



