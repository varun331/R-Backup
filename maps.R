library(sqldf)
london <- read.csv("~/Documents/london.csv")
invention <- read.csv("~/Documents/Countries_Submissions copy2.csv")
invention$partition <- as.character(invention$partition)
mod_player_data <- sqldf("select Count, Country, count(*) as num_of_players 
                         from invention
                         group by 1, 2")
library(maps)
library(mapdata)
library(RColorBrewer)
library(plyr)
cluster1 <- subset(mod_player_data, num_of_players == 1)
plot_data1 <- cbind(cluster1, partition = cut(as.numeric(cluster1$num_of_players),c(0, max(cluster1$num_of_players)),labels = FALSE, right = TRUE))

gama1 <- brewer.pal(9,"Oranges")
col1 <- character()
for (i in 1:nrow(invention))
{
  col1 <- append(col1,gama1[as.numeric(invention[i,2])])
}

map('worldHires',as.character(invention[,1]),fill=TRUE,col=col1,plot=TRUE, cex = 15, exact=F)
title("User Idea Submission Heatmap")
legend("bottomleft", c ("1-15", "16 -100", "101 - 500", "500 - 100", "10K - 12K", "more than 50"),border=gama1, fill = gama1, cex = 0.5, box.col = "white")

write.csv(nams1,file="~/Documents/nams1.csv",row.names=FALSE)
