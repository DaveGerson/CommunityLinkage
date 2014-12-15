#trans = read.transactions("some_data.csv", format = "single", sep = ",", cols = c("transactionID", "productID"))
library("arules")
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.00005, confidence=0.3 , maxlen = 2))
rules
rulesDF<-as(rules, "data.frame")

rulesDF$lhs <- unlist(lapply(strsplit(gsub("\\}", "",gsub("\\{", "",(as.character(rulesDF$rules)))), "=>"), "[", 1))
rulesDF$rhs <- unlist(lapply(strsplit(gsub("\\}", "",gsub("\\{", "",(as.character(rulesDF$rules)))), "=>"), "[", 2))
rulesDF$lhs <- as.factor(rulesDF$lhs)
rulesDF$rhs <- as.factor(rulesDF$rhs)

#deprecated in testing at single item rules
#strsplit(as.character(rulesDF$lhs),",")

#percentileParam<-quantile(rulesDF$lift, c(.75))
#rulesDF_2 <-rulesDF[ which( rulesDF$lift > percentileParam), ]	
rulesDF_2<-rulesDF

library("igraph")
df.g <- graph.data.frame(d = data.frame(rulesDF_2$lhs,rulesDF_2$rhs), directed = TRUE)

#Graph for the graph
plot(df.g, vertex.label = V(df.g)$name)



library("fastcluster")
library("linkcomm")

#One Large Cluster with many smaller
lc <- getLinkCommunities(data.frame(rulesDF_2$lhs,rulesDF_2$rhs) , hcmethod = "single" , directed = TRUE)
print(lc)
plot(lc, type = "graph", layout = layout.fruchterman.reingold)
plot(lc, type = "graph", layout = "spencer.circle")

#Average Produces a large number of small clusters with a few large ones
lc <- getLinkCommunities(data.frame(rulesDF_2$lhs,rulesDF_2$rhs) , hcmethod = "average" , , directed = TRUE)
print(lc)
plot(lc, type = "graph", layout = layout.fruchterman.reingold)
plot(lc, type = "graph", layout = "spencer.circle")

#McQuitty Produces a large number of small clusters with a few large ones
lc <- getLinkCommunities(data.frame(rulesDF_2$lhs,rulesDF_2$rhs) , hcmethod = "mcquitty" , , directed = TRUE)
print(lc)
plot(lc, type = "graph", layout = layout.fruchterman.reingold)
plot(lc, type = "graph", layout = "spencer.circle")

#Ward Produces a large number of small clusters
lc <- getLinkCommunities(data.frame(rulesDF_2$lhs,rulesDF_2$rhs) , hcmethod = "ward" , , directed = TRUE)
print(lc)
plot(lc, type = "graph", layout = layout.fruchterman.reingold)
plot(lc, type = "graph", layout = "spencer.circle")
