# 1st Paper version
# SET-UP
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

papply <- function(X, MARGIN, FUN, ...){
  if(MARGIN == 1){
    return(foreach(i = 1:nrow(X)) %dopar% return(FUN(X[i,], ...)))
  }else if(MARGIN == 2){
    return(foreach(i = 1:ncol(X)) %dopar% return(FUN(X[,i], ...)))
  }
  return(NULL)
}

plapply <- function(X, FUN, ...){
  return(foreach(i = 1:length(X)) %dopar% return(FUN(X[[i]], ...)))
}

# INCLUDES
pkgTest("doParallel")
pkgTest("foreach")
pkgTest("glmnet")
pkgTest("koRpus")
pkgTest("text2vec")
pkgTest("e1071")
pkgTest("randomForest")
pkgTest("pROC")
pkgTest("caret")

registerDoParallel(cores = detectCores() - 1)


dataMat <- rbind(
  read.csv("conteoPosOtt14Pos.csv", header = T),
  read.csv("conteoNegOtt14pos.csv", header = T))

wordMat <- read.csv("positiveTOtt14PaperWords.csv", header = T)
labels <- 1 - wordMat[,ncol(wordMat)]
wordMat <- wordMat[,-ncol(wordMat)]

#REMOVING UNIGRAMS HAVING LOW ACTIVATION RATE
# OR HIGH ACTIVATION RATE BUT LOW VARIABILITY
dummy_ugrams <- wordMat
dummy_ugrams[dummy_ugrams>1] <- 1
sumMat <- colSums(dummy_ugrams)
CV <- function(x){sd(x)/abs(mean(x))}
cvMat <- unlist(papply(wordMat, 2, CV))
slack <- round(0.02 * nrow(wordMat))

freqs_ugrams <- wordMat[, (sumMat > slack &
                        sumMat < nrow(wordMat) - slack) |
                     (sumMat >= nrow(wordMat) - slack & 
                        cvMat > CV(c(rep(0,slack), round(rnorm(n = nrow(wordMat)-slack, mean = 2, sd = 1/6)))))] #CV(c(rep(0,slack), rep(1,ncol(wordMat)-slack)))
freqs_data <- dataMat
logs_ugrams <- log(freqs_ugrams+1)
num_ugrams <- rowSums(freqs_ugrams)
num_ugrams[num_ugrams<=0] <- 1
ratio_ugrams <- freqs_ugrams/num_ugrams
dummy_ugrams <- freqs_ugrams
dummy_ugrams[dummy_ugrams>1] <- 1
num_concepts <- rowSums(dummy_ugrams)
freqs_data <- cbind(freqs_data, num_ugrams, num_concepts)
dummy_data <- freqs_data
dummy_data[dummy_data>1] <- 1
logs_data <- log(freqs_data+1)
ratio_data <- freqs_data/freqs_data[,1]
#CONSIDERAR TRANSFORMACI?N OBTENIDA POR BOX-COX

colnames(freqs_ugrams) <- paste("freqs",colnames(freqs_ugrams), sep = "_")
colnames(logs_ugrams) <- paste("logs",colnames(logs_ugrams), sep = "_")
colnames(ratio_ugrams) <- paste("ratio",colnames(ratio_ugrams), sep = "_")
colnames(dummy_ugrams) <- paste("dummy",colnames(dummy_ugrams), sep = "_")
colnames(freqs_data) <- paste("freqs",colnames(freqs_data), sep = "_")
colnames(logs_data) <- paste("logs",colnames(logs_data), sep = "_")
colnames(dummy_data) <- paste("dummy",colnames(dummy_data), sep = "_")
colnames(ratio_data) <- paste("ratio",colnames(ratio_data), sep = "_")

x <- as.matrix(cbind(dummy_data, freqs_data, logs_data, ratio_data,
                     dummy_ugrams, freqs_ugrams, logs_ugrams, ratio_ugrams))
y <- labels

# FEATURE SELECTION W/ LASSO
mod.lasso0 <- cv.glmnet(x = x, y = y, alpha = 1, type.measure = "class", family = "binomial", nfolds = length(y), parallel = TRUE, grouped = FALSE)
coef.min <- coef(mod.lasso0, "lambda.min")
varPos <- which(coef.min != 0)[-1]-1;

# FEATURE SELECTION IMPROVEMENT
# Variable simplification:
# BIN < FREQ < LOG < RATIO
num_data <- ncol(dummy_data); tot_data <- num_data * 4;
num_ugrams <- ncol(dummy_ugrams);
for(i in 1:length(varPos)){
  aux <- varPos[i]
  if(aux > num_data & aux <= tot_data){
    cVar <- cor(x[,seq(aux,1,-num_data)])[1,-1]
    cVar[is.na(cVar)] <- 0
    pot <- which(cVar >= (1-1e-5))
    if(length(pot) > 0){
      #print(i)
      jump <- max(pot)
      varPos[i] <- varPos[i] - jump * num_data
    }
  }
  if(aux > tot_data + num_ugrams){
    cVar <- cor(x[,seq(aux,tot_data+1,-num_ugrams)])[1,-1]
    cVar[is.na(cVar)] <- 0
    pot <- which(cVar >= (1-1e-5))
    if(length(pot) > 0){
      #print(i)
      jump <- max(pot)
      varPos[i] <- varPos[i] - jump * num_ugrams
    }
  }
}

# Identical variables removal
for(i in 1:(length(varPos)-1)){
  auxi <- varPos[i]
  if(!is.na(auxi)){
    for(j in (i+1):length(varPos)){
      auxj <- varPos[j]
      if(!is.na(varPos[j])){
        if(cor(x[,auxi], x[,auxj])>= (1-1e-5)){
        #if(sum(x[,auxi] == x[,auxj]) == nrow(x)){
          varPos[j] <- NA
        }
      }
    }
  }
}
varPos <- varPos[which(!is.na(varPos))]

# Final feature selection
xx <- x[,varPos]

#LASSO
#training error
#mod.lasso <- glmnet(x = x, y = y, alpha = 1, type.measure = "class", family = "binomial")
#trn.lasso <- mean(round(predict(mod.lasso, xx)) == y)
#rm(mod.lasso); gc();
#validation error
mod.lasso <- cv.glmnet(x = xx, y = y, alpha = 0, type.measure = "class", family = "binomial", nfolds = length(y), keep = TRUE, parallel = TRUE, grouped = FALSE)
pred.lasso <- mod.lasso$fit.preval[,which(mod.lasso$lambda==mod.lasso$lambda.min)]
gc()


#SUPPORT VECTOR MACHINE
#training error
#mod.svm <- svm(x=xx, y=y)
#trn.svm <- mean(round(predict(mod.svm, xx)) == y)
#rm(mod.svm); gc();
#validation error
pred.svm <- foreach(i = 1:length(y), .packages = c("e1071"), .combine = c) %dopar% {
  mod.svm <- svm(x=xx[-i,], y=y[-i])
  return(predict(mod.svm, t(xx[i,])))
}
gc()

#NAIVE BAYES
#training error
#mod.nb <- naiveBayes(x=xx, y=as.factor(y))
#trn.nb <- mean(round(predict(mod.nb, xx, "raw")[,2]) == y)
#rm(mod.nb); gc();
#validation error
pred.nb <- foreach(i = 1:length(y), .packages = c("e1071"), .combine = c) %dopar% {
  mod.nb <- naiveBayes(x = xx[-i,], y = as.factor(y[-i]))
  return(predict(mod.nb, t(xx[i,]), "raw")[,2])
}
gc()

#RANDOM FORESTS
#training error
#mod.rf <- randomForest(x=xx, y=y)
#trn.rf <- mean(round(predict(mod.rf, xx)) == y)
#rm(mod.rf); gc();
#validation error
pred.rf <- foreach(i = 1:length(y), .packages = c("randomForest"), .combine = c) %dopar% {
  mod.rf <- randomForest(xx[-i,], y[-i])
  return(predict(mod.rf, xx[i,]))
}
gc();

#MODELS EVALUATION
#CONFUSION MATRICES
confMat.lasso <- confusionMatrix(as.factor(round(pred.lasso)), as.factor(y), positive = "1", mode = "everything")
confMat.svm <- confusionMatrix(as.factor(round(pred.svm)), as.factor(y), positive = "1", mode = "everything")
confMat.nb <- confusionMatrix(as.factor(round(pred.nb)), as.factor(y), positive = "1", mode = "everything")
confMat.rf <- confusionMatrix(as.factor(round(pred.rf)), as.factor(y), positive = "1", mode = "everything")
#ROC
roc.lasso <- roc(response = y, predictor = pred.lasso, algorithm = 3)
roc.svm <- roc(response = y, predictor = pred.svm, algorithm = 3)
roc.nb <- roc(response = y, predictor = pred.nb, algorithm = 3)
roc.rf <- roc(response = y, predictor = pred.rf, algorithm = 3)
#ROC PLOT
plot.roc(roc.lasso, col=rainbow(4)[1], cex.lab = 1.5, cex.axis = 1.5, main = "")
plot.roc(roc.svm, pred.svm, col=rainbow(4)[2], add = TRUE)
plot.roc(roc.rf, pred.rf, col=rainbow(4)[3], add = TRUE)
plot.roc(roc.nb, pred.nb, col=rainbow(4)[4], add = TRUE)
legend("bottomright", legend=c("Ridge Regression", "SVM", "Random Forests", "Naive Bayes"), col=rainbow(4), lwd=2, cex=1.5)

dt.comp <- data.table(name = c("RLR","SVM","Random Forests", "Naive Bayer"))
dt.comp$accuracy <- c(confMat.lasso$overall[1],
                      confMat.svm$overall[1],
                      confMat.rf$overall[1],
                      confMat.nb$overall[1])
dt.comp$accuracyLo <- c(confMat.lasso$overall[3],
                        confMat.svm$overall[3],
                        confMat.rf$overall[3],
                        confMat.nb$overall[3])
dt.comp$accuracyUp <- c(confMat.lasso$overall[4],
                        confMat.svm$overall[4],
                        confMat.rf$overall[4],
                        confMat.nb$overall[4])
dt.comp$sensitivity <- c(confMat.lasso$byClass[1],
                        confMat.svm$byClass[1],
                        confMat.rf$byClass[1],
                        confMat.nb$byClass[1])
dt.comp$specificity <- c(confMat.lasso$byClass[2],
                         confMat.svm$byClass[2],
                         confMat.rf$byClass[2],
                         confMat.nb$byClass[2])
dt.comp$precision <- c(confMat.lasso$byClass[5],
                         confMat.svm$byClass[5],
                         confMat.rf$byClass[5],
                         confMat.nb$byClass[5])
dt.comp$F1 <- c(confMat.lasso$byClass[7],
                         confMat.svm$byClass[7],
                         confMat.rf$byClass[7],
                         confMat.nb$byClass[7])
dt.comp$auc <- c(auc(roc.lasso), auc(roc.svm), auc(roc.rf), auc(roc.nb))
pkgTest("xtable")
xtable(dt.comp)




logit <- function(x) 1/(1+exp(-x))
coef.min <- coef(mod.lasso, "lambda.1se")
coefs <- coef.min[which(coef.min != 0)]
names(coefs) <- rownames(coef.min)[which(coef.min != 0)]
print(coefs[order(abs(logit(coefs)-0.5), decreasing = T)])
print(paste("# of variables selected: ", length(coefs)-1))
