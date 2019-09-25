#Solving for nutrient data


data = read.csv(file.choose())

cor(data[2:5])
library(corrplot)
corrplot(cor(data[2:5]))

pca = princomp(data[,2:5],scores=TRUE)
loadings(pca)
screeplot(pca)
summary(pca)
dimdata = pca$scores[,1:2]

dimdata

op = kmeans(dimdata,3)
data$cluster = op$cluster
head(data)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dimdata, nc=15)
setwd("/Users/gurumoorthypattabiraman/Downloads/Workarea.Gurumoorthy1/Guru/")
getwd()

library(cluster)

silscore <- silhouette(op$cluster,dist(dimdata))

data$ss <- silscore[,3]

data$neighbor <- silscore[,2]


write.csv(data,file="myfile4.csv")
