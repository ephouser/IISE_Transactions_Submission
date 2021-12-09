##########################################
################ Modeling ################
##########################################

Start.Time <- Sys.time()

############
## Load the libraries and data
############
# install.packages("dplyr")
# install.packages("nnet")
# install.packages("GA")
# install.packages("doParallel")
# install.packages("parallel")
# install.packages("caret")
# install.packages("gam")
# install.packages("pROC")
# install.packages("glmnet")
# install.packages("MASS")
# install.packages("leaps")
# install.packages("cutpointr")
# install.packages("stringr")
# install.packages("doRNG")

library(dplyr)
library(nnet)
library(GA)
library(doParallel)
library(parallel)
library(caret)
library(gam)
library(pROC)
library(glmnet)
library(MASS)
library(leaps)
library(cutpointr)
library(stringr)

AllData <- read.csv("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/Modeling_Dataset_2.csv", header = T)
AllData <- as.data.frame(AllData)
head(AllData)
nrow(AllData[AllData$Response>0,])

## Moving the response to the last column and removing the first column 
#AllData=AllData[,-1]
AllData=cbind(AllData[,-2],AllData$Response)
head(AllData)
colnames(AllData)[ncol(AllData)]="y"

# Create subsets of AllData for defects and for non-defects
Positives <- c(which(AllData$y == 1))
Negatives <- c(which(AllData$y == 0))

############
## Set parameters and seeds
############
# folds
K <- 10 
# count of undersamples 
n <- 100
# ratio of undersamples
p <- 2
# Percent of defects sampled
c <- 1
#F Measure
beta.val = 3
# LASSO parameters
alpha.par = 0.9
var.sel = 30
# GA parameters
popSize_param <- 50
pCrossover_param <- 0.8
pMutation_param <- 0.2
maxIter_param <- 1000
run_param <- 150
optim_param <- T
# Seeds
ga.initial.seed <- 2500
ga.seed<- 900
s.folds<-1234
s.negativesamples<-2
s.resample <- seq(919, 1117, by=2)
s.optimizelambda <- 919
# New seeds
s.folds.new<-4321
s.positivesamples.new<-20
s.negativesamples.new<-21
SF <- 1 

############
## Step 1 ##: Divide the dataset into K' Bootstraps, each with an equal number of defective layers
############
# Assigning folds to Non-Defective and Defective Layers
Negative_folds <- vector(mode="list",length=K)
Positive_folds <- vector(mode="list",length=K)

set.seed(s.folds) # this is to ensure we get the same folds as each other.
for (k in 1:K) {
  Negative_folds[[k]] <- sample(Negatives, floor(length(Negatives)/K), replace = TRUE)
  Positive_folds[[k]] <- sample(Positives, length(Positives), replace = TRUE)
}

#### parallel processing
clusters<-40
cl<-makeCluster(clusters)
registerDoParallel(cl)
############################

## Create vectors to store the F_Beta Measure and best features for LASSO and SOFS for each fold
f_measure.bm=vector(mode="list",length=K)
feats.bm=vector(mode="list",length=K)

f_measure=vector(mode="list",length=K)
feats=vector(mode="list",length=K)

i=1

# Iterations
for(i in 1:K) {
  ############
  ## Step 2 ##: Use the ith fold as the testing set and the rest in the training dataset
  ############
  i.train_data_N <- unlist(Negative_folds[-i])
  i.test_data_N <- Negative_folds[[i]]
  
  i.train_data_P <- Positive_folds[[i]]
  i.test_data_P <- Positive_folds[[i]]
  
  # All train and all test data
  i.train_data <- slice(AllData, sort(append(i.train_data_N,i.train_data_P)))
  i.train_data$y <- as.factor(i.train_data$y)
  
  i.test_data <- slice(AllData, sort(append(i.test_data_N,i.test_data_P)))
  i.test_data <- i.test_data[,-1]
  i.test_data$y <- as.factor(i.test_data$y)
  
  # ### checkpoint
  # nrow(i.train_data[i.train_data$y==1,])
  # nrow(i.test_data[i.test_data$y==1,])
  
  ############
  ## Step 3 ##: Randomly select n resamples of the training dataset keeping a 1:p ratio for 1's and 0's
  ############
  Sampled_N <- vector(mode = "list", length = n)
  Sampled_P <- vector(mode = "list", length = n)
  Resampled_Data <- vector(mode = "list", length = n)
  
  for(j in 1:n){
    set.seed(s.resample[j])
    # Sampled_N are the indices of randomly selected 0's ensure the 1:p ratio
    Sampled_N[[j]] <- sample(i.train_data_N, length(Positives)*c*p, replace = FALSE)
    Sampled_P[[j]] <- sample(i.train_data_P, length(Positives)*c, replace = TRUE)
    # Combine the data for the randomly sampled 0's and the 1's for the jth resampled dataset
    j.sampled_data <- append(Sampled_N[[j]], Sampled_P[[j]])
    
    # Randomly order the randomly sampled data
    Resampled_Data[[j]] <- sample(j.sampled_data, length(j.sampled_data), replace = FALSE)
  }
  
  # ### checkpoint
  # ds <- slice(AllData, Resampled_Data[[1]])
  # nrow(ds[ds$y==1,])
  # nrow(ds[ds$y==0,])
  
  ############
  ## Step 4.1 Use LASSO to build the best classifier for each resampled dataset j
  ############
  
  # Side Step: Choose features that are in most of the 100 classifiers as the important ones
  # Create a vector to store coefficients from each trial run 
  Coef_All.Trials <- vector(mode = "list", length = n)
  for(j in 1:n) {
    # Subset the data for resampled set j
    Sample_Dataset <- slice(AllData[-1], Resampled_Data[[j]])
    
    # Create predictor matrix and vector for response
    x = model.matrix(y ~ . , Sample_Dataset)[,-1]
    y.data = Sample_Dataset$y
    
    # Create grid for lambda, fit model using all lambdas
    grid = 10^seq(0,-6,length=100) # lambda ranges from 0.1 to 0.000001 
    Lasso.Mod = glmnet(x,y.data,alpha=alpha.par,lambda=grid, family = "binomial")  
    
    #Check coefficent values for each value of lambda
    # plot(Lasso.Mod)  # x-axis is in terms of sum(beta^2)
    # abline(h=0,lty=3)
    
    # Optimize lambda using cross-validation
    set.seed(s.optimizelambda)
    CV.Lasso  = cv.glmnet(x,y.data,alpha=alpha.par,lambda=grid, family = "binomial")
    # plot(CV.Lasso)
    BestLamda.l = CV.Lasso$lambda.min
    MSE.l = min(CV.Lasso$cvm)
    BestLamda.l
    MSE.l
    
    # Record the coefficients for the best model for sub-dataset j
    Coef_All.Trials[[j]] = predict(Lasso.Mod, type="coefficients", s = BestLamda.l)
    Coef_All.Trials[[j]]
    
    # Plot fitted values for LASSO, compare with actual
    Fit.Lasso = predict(Lasso.Mod, s = BestLamda.l, x)
    # points(Fit.Lasso, Sample_Dataset$y, col = "blue", lwd = 2)
    # abline(a = 0, b = 1)
    
    # Look at R2 for the LASSO
    y.R2 <- as.numeric(Sample_Dataset$y)
    R2.lasso = cor(Fit.Lasso,y.R2)^2
    R2.lasso
  }
  
  # Count how many times each feature was selected
  names <- NULL
  for(j in 1:n){
    BestLasso.ModelCoef <- as.data.frame(as.matrix(Coef_All.Trials[[j]]))
    colnames(BestLasso.ModelCoef) <- list("Coefficient")
    filtered_ModelCoef <- rownames(BestLasso.ModelCoef)[which(BestLasso.ModelCoef$Coefficient!=0)] #filter(BestLasso.ModelCoef, Coefficient != 0)
    names <- append(names, filtered_ModelCoef)
  } 
  
  names <- as.data.frame(names)
  # Create a list of all possible features
  All.Features <- rownames(BestLasso.ModelCoef)
  All.Features <- as.data.frame(All.Features)
  All.Features$All.Features=as.character(All.Features$All.Features)
  names$names=as.character(names$names)
  for(k in 1:length(All.Features$All.Features)){
    filter_All <- filter(names, names == All.Features$All.Features[k])
    All.Features$Frequency[k] <- length(filter_All$names)
  }
  
  # Order the features based on frequency of occurrence to identify which features are chosen most often
  All.Features <- All.Features[order(-All.Features$Frequency),]
  All.Features <- filter(All.Features, All.Features != "(Intercept)" & All.Features != "y")
  
  ############
  ## Step 4.2 Evaluate the prediction by taking the majority vote from the 100 classifiers on each data point in the test set
  ############
  
  # Create a list of features which were selected in the majority of datasets
  Best.Features <- filter(All.Features, Frequency >= var.sel)
  Best.Features <- Best.Features[,1]
  
  # Adjust errors that occurred with variable names (?not sure why this happened?)
  Best.Features <- str_replace(Best.Features, "21", "2")
  Best.Features <- str_replace(Best.Features, "Last1", "Last")
  Best.Features
  
  # Store the best features of this fold
  feats.bm[[i]] <- Best.Features
  
  # Creata a set of columns to filter the dataset beginning with the Response
  Selected.Columns.train <- which(names(i.train_data)=="y")
  Selected.Columns.train <- append(Selected.Columns.train, as.numeric(which(names(i.train_data) %in% Best.Features)))
  
  Selected.Columns.test <- which(names(i.test_data)=="y")
  Selected.Columns.test <- append(Selected.Columns.test, as.numeric(which(names(i.test_data) %in% Best.Features)))
  
  # Filter the dataset so only selected variables will be trained and tested (easier to replicate with different sets of variables)
  i.train_data.best <- i.train_data[,Selected.Columns.train]
  i.test_data.best <- i.test_data[,Selected.Columns.test]
  
  # Creata a dataset w/ the selected features
  Log.Mod <- glm(y ~., family = "binomial", data = i.train_data.best )
  
  # Prepare the test dataset
  X_test <- as.data.frame(i.test_data[as.numeric(which(names(i.test_data) %in% Best.Features))])
  
  # Predict whether a defect will happen or not
  Probability.bm.is <- predict(Log.Mod, type = "response")
  MyRoc=pROC::roc(i.train_data$y~Probability.bm.is)
  coord=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
  threshold.bm =coord
  Probability.bm <- predict(Log.Mod, newdata = X_test, type = "response")
  
  #translate probabilities to predictions
  Prediction.bm <- ifelse(Probability.bm > as.numeric(threshold.bm),1,0)
  Prediction.bm <- factor(Prediction.bm, levels = c(0,1))
  
  ############
  ## Step 5 SOFS with the FNR objective function; no side-steps for feature selection here
  ############
  # fitness function
  fitness_lr <- function(string) {
    f_measure_ga <- array(0, dim = n)
    inc <- which(string == 1)+1
    cc <- colnames(AllData)[c(inc,ncol(AllData))]
    for (j in 1:n) {
      # Subset the data for resampled set j to build model and and all but j to evaluate model
      s.dataset <- slice(AllData, Resampled_Data[[j]])
      s.dataset.complement <- i.train_data[!(i.train_data$Layer_ID %in% s.dataset$Layer_ID),]
      
      # build the model and predict for all but j resamples
      mod <- glm(y ~ ., data = s.dataset[,cc], family = "binomial")
      prob_is<-predict(mod, type="response")
      s.dataset=as.data.frame(s.dataset)
      MyRoc=pROC::roc(s.dataset$y~prob_is)
      coord=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
      threshold=coord
      prob<-predict(mod, s.dataset.complement[,cc], type="response")
      
      predicted<- ifelse(prob>as.numeric(threshold), 1, 0)
      predicted<- factor(predicted, levels = c(0,1))
      
      res.ga <- table(s.dataset.complement$y, predicted)
      
      true.pos <- res.ga[2,2]
      false.neg <- res.ga[2,1]
      false.pos <- res.ga[1,2]
      f_measure_ga[j] = ((1+beta.val^2)*(true.pos))/((1+beta.val^2)*(true.pos)+(beta.val^2)*(false.neg)+false.pos)
    }
    # return the average error measure with negative sign to minimize it (GA's default is maximization)
    mean(f_measure_ga)
  }
  
  # GA with logistic regression (lr)
  set.seed(ga.initial.seed)
  galr <- ga(type = "binary", fitness = fitness_lr, nBits = ncol(AllData)-2,
             names = colnames(AllData)[2:(ncol(AllData)-1)],
             popSize = popSize_param, maxiter = maxIter_param, optim = optim_param,
             pcrossover = pCrossover_param, pmutation = pMutation_param,
             parallel = T, run = run_param, seed = ga.seed
  )
  
  # test convergence of GA
  plot(galr, main= paste("Iteration", i))
  
  # save the results in selected.cols vector
  selected.cols <- na.omit(colnames(AllData[,-1])[galr@solution[1,] == 1])
  feats[[i]]=selected.cols
  # ensure variable "y" is not in the selected variables
  if(any(selected.cols=="y")){
    idx<-which(selected.cols=="y")
    selected.cols<-selected.cols[-idx]
  }
  
  # build the model and predict for i.test_data
  model <- glm(y~., data = i.train_data[,c(selected.cols,"y")], family=binomial)
  prob_is<-predict(model, type="response")
  MyRoc=pROC::roc(i.train_data$y~prob_is)
  coord=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
  threshold=coord
  prob<-predict(model, i.test_data, type="response")
  
  predicted<- ifelse(prob>as.numeric(threshold), 1, 0)
  predicted<- factor(predicted, levels = c(0,1))
  
  ############
  ## Step 6.1 Calculate the F Beta measures for fold i of the benchmark
  ############
  
  # Confusion matrix
  res.bm <- table(i.test_data$y, Prediction.bm);res.bm
  
  true.pos <- res.bm[2,2]
  false.neg <- res.bm[2,1]
  false.pos <- res.bm[1,2]
  f_measure.bm[[i]] = ((1+beta.val^2)*(true.pos))/((1+beta.val^2)*(true.pos)+(beta.val^2)*(false.neg)+false.pos)
  print(f_measure.bm[[i]])
  
  ############
  ## Step 6.2 Calculate the F Beta measures for fold i
  ############
  
  # Confusion matrix
  res <- table(i.test_data$y, predicted);res
  
  true.pos <- res[2,2]
  false.neg <- res[2,1]
  false.pos <- res[1,2]
  f_measure[[i]] = ((1+beta.val^2)*(true.pos))/((1+beta.val^2)*(true.pos)+(beta.val^2)*(false.neg)+false.pos)
  print(f_measure[[i]])
  
}

############
## Step 7 ##: Calculate the average and variance of the evaluated error rates for all K Bootstraps
############

#### LASSO ####
# Calculate the average and variance for the f measure
f_measure.bm.Average <- mean(unlist(f_measure.bm))
f_measure.bm.Var <- var(unlist(f_measure.bm))

#### SOFS ####
# Calculate the average and variance for the f measure
f_measure.Average <- mean(unlist(f_measure))
f_measure.Var <- var(unlist(f_measure))

############
## Step 8.0: Create the list of optimal features
############

K = 10 
SF.List <- c(0.1, 0.5, 0.6, 0.7, 0.8)
feats.bm.intersect.b3 <- vector(mode="list",length=length(SF.List))
feats.intersect.b3 <- vector(mode="list",length=length(SF.List))

for(i in 1:length(SF.List)){
  # Feature Selection from LASSO
  featList <- unique(unlist(feats.bm))
  featSummary <- as.data.frame(matrix(numeric(),nrow = 0, ncol = 2))
  for (var in featList) {
    count = 0
    for (feat in unlist(feats.bm)) {if (var == feat) {count = count +1}}
    featSummary <- rbind(featSummary, c(var, count))
  }
  colnames(featSummary) <- c("Feature", "Frequency")
  featSummary$Frequency <- as.numeric(featSummary$Frequency)
  
  feats.bm.intersect.b3[[i]] <- c(featSummary$Feature[which(featSummary$Frequency >= K*SF.List[i])])
  
  # Feature Selection from SOFS
  featList <- unique(unlist(feats))
  featSummary <- as.data.frame(matrix(numeric(),nrow = 0, ncol = 2))
  for (var in featList) {
    count = 0
    for (feat in unlist(feats)) {if (var == feat) {count = count +1}}
    featSummary <- rbind(featSummary, c(var, count))
  }
  colnames(featSummary) <- c("Feature", "Frequency")
  featSummary$Frequency <- as.numeric(featSummary$Frequency)
  
  feats.int <- featSummary$Feature[which(featSummary$Frequency >= K*SF.List[i])]
  feats.intersect.b3[[i]] <- c(feats.int[feats.int != "y"])
}

Stop.Time <- Sys.time()
Total.P1.B3.Time <- difftime(Stop.Time, Start.Time)

save(feats.bm.intersect.b1,file="P1B3_LASSOI_Feats.RData")
save(feats.intersect.b1,file="P1B3_SOFSI_Feats.RData")

