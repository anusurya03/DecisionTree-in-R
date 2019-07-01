library(rpart)
library(party)
library(rpart.plot)
library(rattle)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(partykit)
library(caret)
f<-file.choose()
k<-read.csv(f)
k
#create the input data frame
input.dat=as.data.frame(k)
output.tree=rpart(play~day+outlook+temp+humidity+wind,method = "class",data=input.dat)
output.tree
#plot the tree
rpart.plot(output.tree)
#plot model
fancyRpartPlot(output.tree)
filter(k,k$outlook=="overcast")
b=filter(k,k$outlook=="overcast")
c=b%%count(play=="y")
c
filter(b,humidity=="high")
