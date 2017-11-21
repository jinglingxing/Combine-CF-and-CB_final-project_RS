#import libraries
library(RCurl)
library(readr)
library(dplyr)
library(tidyr)
library(Matrix)
library(text2vec)
library(ggplot2)

######################import data from TP1###########################


u.data <- read.csv(text = getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd = '20113:20113'), sep='|', header=T)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
u.item <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.item.csv', userpwd = '20113:20113'), sep='|', header=T)
u.user <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.user.csv', userpwd = '20113:20113'), sep='|', header=T)


########################Preprocess data##############################


uitem<- u.item[,c('movie.id','movie.title', 'release.date', 'Action', 'Adventure', 'Animation', 'Children.s', 'Comedy','Crime', 'Documentary','Drama','Fantasy', 'Film.Noir','Horror','Musical','Mystery','Romance','Sci.Fi','Thriller','War','Western','video.release.date','IMDb.URL','unknown')]
uudata<-uitem[,1:21]  #the data of movies

#separate the year-month-date
moviesData <- uudata %>% separate(col = release.date, 
                                      into = c('Day', 'Month','Year'), 
                                      sep = "-")  
##delete the day and month
moviesData$Day <- NULL  
moviesData$Month <- NULL
moviesData$Year <- as.numeric(moviesData$Year)

#Match moviesData$Year with ratingsData
mD <- moviesData %>%  select(movie.id, Year)  #select movie.id and Year columns 
#Match moviesData$Year with ratingsData:
ratingsData <- merge(u.data, mD, by.x = 'item.id', by.y = 'movie.id' ) 



##################################################################
#########################collabrative filtering method############
##################################################################



#Pearson Correlations: User-User Sim Matrix
ratingsData$timestamp <- NULL
ratingsData$Year<- NULL

#every user votes every movie
voteitem <- ratingsData %>% spread(key =item.id,
                                   value = rating, 
                                   sep = "_") %>% arrange(user.id)


###compute the similarity based on CF(pearson correlations)


#delete the user.id column, the leaving part is only the item.id
voteitem <- voteitem %>% select(starts_with("item.id"))
itemvote <- t(voteitem)

#the userusersim matrix is the pearson cor based on the users vote for each item
UserUserSim <- cor(itemvote,  use = 'pairwise.complete.obs')
UserUserSim <- as.data.frame(UserUserSim) 



##################################################################
###Pearson Correlations: Item-Item Sim Matrix

#the itemitemsim matrix is the pearson cor base on the voteitems
#the itemitemsim matrix is a diagonal symmetric matrix
ItemItemSim <- cor(voteitem, use = 'pairwise.complete.obs')  
ItemItemSim <- as.data.frame(as.matrix(ItemItemSim))


##################################################################
###################content based method###########################
##################################################################

###compute user distance 

#based on the jaccard function which is used for sparse matrix
usersData<-u.data
m <- sparseMatrix(usersData[,1],usersData[,2],x=usersData[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
usersData<-m
userDistance<- dist2(usersData, method = "jaccard")
userDistance<-as.matrix(userDistance)

#after we traverse to matrix, then we can traverse to dataframe
userDistance<-as.data.frame(userDistance) 

###compute movie distance

#jaccard method
#select the columns from the 4th column
moviesDistance<- moviesData[, 4:ncol(moviesData)]  
w <- which(moviesDistance > 0, arr.ind = T)
moviesDistance[w] <- 1
moviesDistance<-dist2(Matrix(as.matrix(moviesData[, 4:ncol(moviesData)])), 
                      method = "jaccard")

moviesDistance <- as.matrix(moviesDistance)
moviesDistance<-as.data.frame(moviesDistance)

#############################gabage return##########################

#there are too much data in the data environment
#so i use gabage return and left the data we really need
rm(itemvote); gc()
rm(voteitem); gc()
rm(mD); gc()
rm(moviesData); gc()
rm(ratingsData); gc()
rm(u.data); gc()
rm(u.item); gc()
rm(u.user); gc()
rm(uitem); gc()
rm(userrating); gc()
rm(uudata); gc()
rm(w); gc()
rm(ukdata); gc()

####################################################################
#####################A hybird method(user-user)#####################
####################################################################

# A hybrid method which combining the Collaborative filtering and contend-based methods together
# Using a linearing regression method
# First I choose the weight of CF and CB by myself take the weight half and half
WeightedUser=1.0*userDistance+0*UserUserSim

#######################cross-validation#####################

WeightedUser<-as.matrix(WeightedUser)
observed <- which(WeightedUser > 0)           #Non-zero values of weightedUser
hasard <- sample(observed, length(observed))  # sampling indices of non-zero values of R
fold.size <- round(length(hasard) / 10)       # 10 folds ==> test on 10% of items and train on 90%
w.false <- rep(FALSE, length(WeightedUser))
fold.number <- 1                              #The index of the test set

w.test.b <- w.false               # Boolean Indices for Test and Training Cells
#The indexed cells of the corresponding fold are set to TRUE for the testing
w.test.b[ hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
w.train.b <-  !w.test.b             #and FALSE for training
w.na.train <- WeightedUser
w<-WeightedUser
w.na.train[w.test.b] <- NA       #I remove test data for training

### A hybrid method on training data (user-user)
cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m)/(n * sqrt (sum (v ^ 2)))} 

cosinematrix = matrix(nrow=943,ncol=943);
for( i in 1:943){
  cosinematrix[i,]<-cosinus.vm(t(w.na.train)[,i],t(w.na.train))  #calculus of the cosine between the rows of the matrix w.na.train
}
cosinematrix[is.na(cosinematrix)]<-0
w.prediction <- rowMeans(w.na.train,na.rm=T)+(cosinematrix%*% (w.na.train-rowMeans(w.na.train,na.rm=T)))/rowSums(abs(cosinematrix))
w.prediction[is.na(w.prediction)]<-0

mae <- function(m1, m2) mean(abs(m1 - m2), na.rm=T)
mae(w.prediction[w.test.b],w[w.test.b])
sqrt(mean((w.prediction[w.test.b] - w[w.test.b])^2, na.rm=T))

####################################################################
#####################A hybird method(item-item)#####################
####################################################################

#A hybrid method which combining the Collaborative filtering 
# and contend-based methods together

#I choose the weight of CF and CB 
#take the weight half and half
WeightedItem=0*moviesDistance+1*ItemItemSim

#######################cross-validation#####################

WeightedItem<-as.matrix(WeightedItem)
wi.observed <- which(WeightedItem > 0)           #Non-zero values of weightedUser
wi.hasard <- sample(wi.observed, length(wi.observed))  # sampling indices of non-zero values of R
wi.fold.size <- round(length(wi.hasard) / 10)       # 10 folds ==> test on 10% of items and train on 90%
wi.false <- rep(FALSE, length(WeightedItem))
wi.fold.number <- 1                              #The index of the test set

wi.test.b <- wi.false               # Boolean Indices for Test and Training Cells
#The indexed cells of the corresponding fold are set to TRUE for the testing
wi.test.b[ hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
wi.train.b <-  !wi.test.b             #and FALSE for training
wi.na.train <- WeightedItem
wi<-WeightedItem
wi.na.train[wi.test.b] <- NA       #I remove test data for training

### A hybrid method on training data (item-item)

for( i in 1:1682){
  cosineItem[i,]<-cosinus.vm(t(wi.na.train)[,i],t(wi.na.train))  #calculus of the cosine between the rows of the matrix w.na.train
}
cosineItem[is.na(cosineItem)]<-0
wi.prediction <- rowMeans(wi.na.train,na.rm=T)+(cosineItem%*% (wi.na.train-rowMeans(wi.na.train,na.rm=T)))/rowSums(abs(cosineItem))
wi.prediction[is.na(wi.prediction)]<-0

mae(wi.prediction[wi.test.b],wi[wi.test.b])
sqrt(mean((wi.prediction[wi.test.b] - wi[wi.test.b])^2, na.rm=T))