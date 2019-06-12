# Pearson correlation
rxmean <- mean(JesterMatrix[i, ], na.rm=T)
rymean <- mean(JesterMatrix[j, ], na.rm=T)
sim <- sum((JesterMatrix[i, ]-rxmean)*(JesterMatrix[j,]-rymean), na.rm=TRUE)/sqrt(sum((JesterMatrix[i, ]-rxmean)^2, na.rm=TRUE) * sum((JesterMatrix[j, ]-rymean)^2, na.rm=TRUE))

data_path <- "C:/Users/vernoult/Desktop/Recommendation Tools/Group Assignment/"

#### Function ####
MAE <- function(prediction, real){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    mae = sum(abs(real-prediction), na.rm=T)/sum(!is.na(real-prediction))
    return(mae)
    
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

#### Testing ####
predictions <- read.csv(paste0(data_path, "predictions.csv"))
preds <- predictions$prediction
real <- predictions$rating
MAE(preds, real)


#### Function ####
Classification_wF1 <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1 = 2 * ( (Precision*Recall) / (Precision+Recall) )
      Class_Thres = list(Recall, Precision, F1)
      names(Class_Thres) = c("Recall", "Precision", "F1")
    }
    if (!is.na(TopN)){
      TP = vector(length = nrow(prediction))
      FP = vector(length = nrow(prediction))
      FN = vector(length = nrow(prediction))
      
      for (u in nrow(prediction)){
        threshold_pred = -sort(-prediction[u, ])[TopN]
        threshold_real = -sort(-real[u, ])[TopN]

        TP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
        FP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] < threshold_real, 1, 0), na.rm=T)
        FN[u] = sum(ifelse(prediction[u, ] < threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[u])
      FP = sum(FP[u])
      FN = sum(FN[u])
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1 = 2 * ( (Precision*Recall) / (Precision+Recall) )
      Class_TopN = list(Recall, Precision, F1)
      names(Class_TopN) = c("Recall", "Precision", "F1")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}


#### Testing ####
library(proxy)
library(recommenderlab)
ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity
  
  similarity_matrix <- as.matrix(simil(t(train_data), method="cosine"))
  
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
data("Jester5k")
JesterMatrix <- as(Jester5k,"matrix")
train <- JesterMatrix[1:4000,]
test <- JesterMatrix[4001:5000,]

ResultsIBCFUser <- ItemBasedCF(train, test, 3, NN=10, onlyNew=TRUE)
predictionUser <- as.data.frame(ResultsIBCFUser$prediction)
TopNUser <- as.data.frame(ResultsIBCFUser$topN)

Classification_wF1(predictionUser, test, TopN=10)


Classification_wF1(XY2$prediction, test, threshold = 0.5)

write.csv(shivam_results, "predictions.csv")
