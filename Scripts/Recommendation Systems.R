install.packages("reshape2")
install.packages("OneR")
install.packages("maditr")
install.packages("Hmisc")
library(reshape2)
library(dplyr)
library(tidyr)
library(OneR)
library(maditr)
library(Hmisc)
library(proxy)

path <- "C:/Users/vernoult/Desktop/Recommendation Tools/Group Assignment/last.fm/"

#reading the data
artists <- read.csv(paste0(path,'artists.dat'), sep="\t")
user_tagged_artists <- read.csv(paste0(path,'user_taggedartists.dat'), sep="\t")
tags <- read.csv(paste0(path,'tags.dat'), sep="\t")
user_artists <- read.csv(paste0(path,'user_artists.dat'), sep="\t")

# merging user_taggedartists and tags
combined_usertaggedartists_tags <- left_join(user_tagged_artists,tags, by= "tagID")


# creating the final data for content based recommendations
# artists in rows, tagValues in columns 
# instead of the number of counts there are only two options 1=tag assigned / 0= tag not assigned
final_cb <- (dcast(combined_usertaggedartists_tags, artistID ~ tagValue, fun.aggregate = length))
rownames(final_cb) <- final_cb$artistID
final_cb$artistID <- NULL
final_cb_matrix <- as.matrix(final_cb)
final_cb_matrix[, 2:ncol(final_cb)] <- ifelse(final_cb[, 2:ncol(final_cb)] == 0, 0, 1)


# subset to inspect final data
sub <- final_cb[1:10,1:10]



# inspecting the weight variable of the user_artist dataset
hist(user_artists$weight)
max(user_artists$weight)
min(user_artists$weight)

# create 10 equal sized groups for the weight variable (categorization)
user_artists$group <- as.numeric(cut_number(user_artists$weight,10))
user_artists$weight <- NULL

# creating the final data for the colaborative filtering
final_cf <- spread(user_artists, artistID, group)
rownames(final_cf) <- final_cf$userID
final_cf$userID <- NULL
final_cf_matrix <- as(final_cf,"matrix")

# subset to observe the data
final_cf[1:10,1:10]

# test train split
train <- final_cf_matrix[1:1200,]
test <- final_cf_matrix[1201:1892,]



############################################################################
### Cluster based CF as a function (non-hirachical)#########################
### taken from the skript###################################################
############################################################################


ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(data[u, is.na(data[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), N))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 

# run the function
resnhc <- ClusterBasedCF(final_cf_matrix, N=3, centers = 200, iter = 10, onlyNew = TRUE )
# get the results
predictionsnhc <- resnhc$prediction
topNnhc <- resnhc$topN



#############################################################################
### Cluster based CF as a function (hirachical) #############################
#############################################################################

hcfunction <- function(data, k, N){
  
  # changing na's to 0 otherwise errors
  data[is.na(data)] <- 0 
  d <- dist(as.matrix(data), method = "cosine")
  
  hc <- hclust(d)
  
  # Create groups
  grouping <- cutree(hc, k=k)
  
  # Statistics of the groups
  tab <- table(grouping)
  
  min(tab)
  max(tab)
  mean(tab)
  
  # Assign users to groups
  JM <- cbind(data, as.data.frame(grouping))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(JM, list(JM$grouping), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(JM$grouping)
  users <- cbind(users, rownames(JM))
  colnames(users) <- c("grouping", 'user')
  
  predictionDist = merge(users, aggregation, by="grouping")
  rownames(predictionDist) <- predictionDist$user
  
  predictionDist  <- predictionDist[, -1:-2]
  
  # TopN
  TopNDist <- t(apply(predictionDist, 1, function(x) names(head(sort(x, decreasing=TRUE), N))))
  
  res <- list(predictionDist, TopNDist)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 

# run function
reshc <- hcfunction(final_cf_matrix, 300, 3)
# get the results
predictionshc <- reshc$prediction
topNhc <- reshc$topN




######################################
### Content Based Fitering (CB) ###
######################################

# what is product data ?

##TESTDOM
N <- 3
NN <- 10
similarity_matrix <- as.matrix(simil(train, method="cosine"))

print("Similarity calculation done")

# Set Nearest neighbors (stolen from user-based CF)
similarity_matrix_NN <- similarity_matrix

for (k in 1:nrow(similarity_matrix_NN)){
  crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
  similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
}

similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
test_data2 <- test
test_data2[is.na(test_data2)] <- 0

print("Nearest neighbor selection done")

Num <-  test_data2[1, ] %*% similarity_matrix_NN[,1]
## TESTDOM

ContentBased <- function(product_data, test_data, N, NN, onlyNew=TRUE){
  
  # Similarity calculation (stolen from user-based CF)
  similarity_matrix <- as.matrix(simil(product_data, method="cosine"))
  
  print("Similarity calculation done")
  
  # Set Nearest neighbors (stolen from user-based CF)
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
  }
  
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction (stolen from item-based CF) ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


#run the function
rescb <- ContentBased(train, as(test, "matrix"), 3, 10, onlyNew=T)
#get the results
predictioncb <- rescb$prediction
topNcb <- rescb$topN



#############################################
### User Based Colaborative Fitering (CB) ###
#############################################

# only method is changed not the formula

UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_matrix <- as.matrix(simil(train_data, method="correlation"))
  
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  
  TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  ### Numerator ###
  for (u in rownames(test_data)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
    
    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
    Num = similarity_vector %*% NN_norm
    
    #Prediction
    prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


#run the function
resubcf <- UserBasedCF(train, test, 3, 10, onlyNew=T)
#get the results
predictionubcf <- resubcf$prediction
topNubcf <- resubcf$topN
write.csv(resibcf, "user-based predictions.csv")



#############################################
### Item based Colaborative Fitering (CB) ###
#############################################

# only changed the method

## Item-based (IBCF) ##
ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity
  
  similarity_matrix <- as.matrix(simil(train_data, method="correlation"))
  
  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

#run the function
resibcf <- ItemBasedCF(train, test, 3, 10, onlyNew=T)
#get the results
predictionibcf <- resibcf$prediction
topNibcf <- resibcf$topN

write.csv(resibcf, "item-based predictions.csv")

