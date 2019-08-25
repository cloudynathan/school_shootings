#############################
# Analysis of school census #
#############################

#load packages
library(dplyr)

#import data
df1 <- read.csv("C:/workspaceR/school_shootings/census.csv")

#Select years greater than 1989
df1 <- dplyr::filter(df1, Year > 1989)

#Plot Year by Total enrollment
p <- ggplot(data = df1, aes(Year, Total.enrolled)) + geom_line() +ylim(50000,90000)
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Yearly enrollment") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Total enrollment")

