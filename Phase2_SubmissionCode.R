install.packages("dplyr")
install.packages("nnet")
install.packages("GA")
install.packages("doParallel")
install.packages("parallel")
install.packages("caret")
install.packages("gam")
install.packages("pROC")
install.packages("glmnet")
install.packages("MASS")
install.packages("leaps")
install.packages("cutpointr")
install.packages("stringr")
install.packages("tibble")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("directlabels")
install.packages("ggrepel")
install.packages("ggtext")

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
library(tibble)
library(tidyr)
library(ggplot2)
library(directlabels)
library(ggrepel)
library(ggtext)

Start.Time <- Sys.time()

AllData <- read.csv("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/Modeling_Dataset_2.csv", header = T)
AllData <- as.data.frame(AllData)
head(AllData)
nrow(AllData[AllData$Response>0,])

## Moving the response to the last column and removing the first column
#AllData=AllData[,-1]
AllData=cbind(AllData[,-1])
head(AllData)
colnames(AllData)[ncol(AllData)]="y"

# Create subsets of AllData for defects and for non-defects
Positives <- c(which(AllData$y == 1))
Negatives <- c(which(AllData$y == 0))

#######################################
#######################################
### Importing Optimal Feature Lists ###
#######################################
#######################################

load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B1_LASSOI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B2_LASSOI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B3_LASSOI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B4_LASSOI_Feats.RData")

load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B1_SOFSI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B2_SOFSI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B3_SOFSI_Feats.RData")
load("G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/P1B4_SOFSI_Feats.RData")

Beta.List <- c(1,2,3,4)
SF.List <- c(0.1,0.5, 0.6, 0.7, 0.8)

agg.feats.bm <- vector(mode="list", length = length(Beta.List)*length(SF.List))
agg.feats <- vector(mode="list", length = length(Beta.List)*length(SF.List))

agg.feats.bm <- c(feats.bm.intersect.b1,
                  feats.bm.intersect.b2,
                  feats.bm.intersect.b3,
                  feats.bm.intersect.b4)

agg.feats<- c(feats.intersect.b1,
              feats.intersect.b2,
              feats.intersect.b3,
              feats.intersect.b4)


################################

############
## Crude Lasso Step 1
############
# Create predictor matrix and vector for response
x = model.matrix(y ~ . , AllData)[,-1]
y.data = AllData$y

# Create grid for lambda, fit model using all lambdas
grid = 10^seq(0,-6,length=100) # lambda ranges from 0.1 to 0.000001
Lasso.Mod = glmnet(x,y.data,alpha=0.9, family = "binomial")

#Check coefficent values for each value of lambda
plot(Lasso.Mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# Optimize lambda using cross-validation
set.seed(919)
CV.Lasso  = cv.glmnet(x,y.data,alpha=0.9, family = "binomial")
plot(CV.Lasso)
BestLamda.l = CV.Lasso$lambda.min
MSE.l = min(CV.Lasso$cvm)
BestLamda.l
MSE.l

# Record the coefficients for the best model for sub-dataset j
Sel.Coef = predict(Lasso.Mod, type="coefficients", s = BestLamda.l)

# Plot fitted values for LASSO, compare with actual
Fit.Lasso = predict(Lasso.Mod, s = BestLamda.l, x)
points(Fit.Lasso, AllData$y, col = "blue", lwd = 2)
abline(a = 0, b = 1)

# Look at R2 for the LASSO
y.R2 <- as.numeric(AllData$y)
R2.lasso = cor(Fit.Lasso,y.R2)^2
R2.lasso


# Count how many times each feature was selected
names <- NULL
for(j in 1:100){
  BestLasso.ModelCoef <- as.data.frame(as.matrix(Sel.Coef))
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
## Crude Lasso Step 2
############

# Create a list of features which were selected in the majority of datasets
Best.Features <- filter(All.Features, Frequency >= 1)
Best.Features <- Best.Features[,1]

# Adjust errors that occurred with variable names (?not sure why this happened?)
Best.Features <- str_replace(Best.Features, "21", "2")
Best.Features <- str_replace(Best.Features, "Last1", "Last")
Best.Features

# Store the best features of this fold
feats.crude <- Best.Features

####################################

# ############
# ## Step 8.1 ##: Determining the Best K's
# ############
# 
# Create subsets of AllData for defects and for non-defects
# Positives <- c(which(AllData$y == 1))
# Negatives <- c(which(AllData$y == 0))
# 
# 
# ## Setting Some Parameters ##
# # New Seeds
# s.folds.new<-4321
# 
# # Predefined Variables
# Beta.List <- c(1,2,3,4)
# SF.List <- c(0.5, 0.6, 0.7, 0.8)
# 
# 
# # Determining Best K
# best.k.FM <- data.frame(row.names = c("Beta = 1", "Beta = 2", "Beta = 3", "Beta = 4"))
# best.k.VR <- data.frame(row.names = c("Beta = 1", "Beta = 2", "Beta = 3", "Beta = 4"))
# best.k.FNR <- data.frame(row.names = c("Beta = 1", "Beta = 2", "Beta = 3", "Beta = 4"))
# best.k.FPR <- data.frame(row.names = c("Beta = 1", "Beta = 2", "Beta = 3", "Beta = 4"))
# 
# loop.tracker <- data.frame(SF = character(), Best.K=numeric())
# # loop.tracker[1,] <- c("SF = 10%", 0)
# loop.tracker[1,] <- c("SF = 50%", 0)
# loop.tracker[2,] <- c("SF = 60%", 0)
# loop.tracker[3,] <- c("SF = 70%", 0)
# loop.tracker[4,] <- c("SF = 80%", 0)
# 
# k.history.FPR <- data.frame()
# k.history.VR <- data.frame()
# 
# best.k.val <- vector(mode="list", length = length(SF.List))
# 
# i.int = 1
# for (h in 1:length(SF.List)){
#   best.k.found <- FALSE
#   K.val <- 10
#   
#   while (best.k.found == FALSE) {
#     i = i.int + (h-1)
#     
#     loop.tracker[h,2] <- K.val
#     loop.plot <- ggplot(loop.tracker, aes(x = SF , y = Best.K)) +
#       geom_bar(fill = "#0073C2FF", stat = "identity") +
#       geom_text(aes(label = Best.K), vjust = -0.3) + 
#       ggtitle(SF.List[h])
#     print(loop.plot)
#     
#     for (j in 1:length(Beta.List)) {
#       #Create new samples for training
#       train.negative_folds <- vector(mode="list",length=K.val)
#       train.positive_folds <- vector(mode="list",length=K.val)
#       test.negative_folds <- vector(mode="list",length=K.val)
#       test.positive_folds <- vector(mode="list",length=K.val)
#       
#       unique.p <- vector(mode="list",length=K.val)
#       Positives.p2 <- vector(mode="list",length=K.val)
#       
#       unique.n <- vector(mode="list",length=K.val)
#       Negatives.p2 <- vector(mode="list",length=K.val)
#       
#       num.n.p2 <- vector(mode = "list", length = K.val)
#       ratio.p1 <- round(length(Negatives)/length(Positives))
#       
#       set.seed(s.folds.new) # this is to ensure we get the same folds as each other.
#       for (m in 1:K.val) {
#         
#         train.negative_folds[[m]] <- sample(Negatives, floor(length(Positives)*2), replace = TRUE)
#         train.positive_folds[[m]] <- sample(Positives, length(Positives), replace = TRUE)
#         
#         unique.p[[m]] <- unique(train.positive_folds[[m]])
#         Positives.p2[[m]] <- Positives[!(Positives %in% unique.p[[m]])]
#         
#         unique.n[[m]] <- unique(train.negative_folds[[m]])
#         Negatives.p2[[m]] <- Negatives[!(Negatives %in% unique.n[[m]])]
#         
#         num.n.p2[[m]] <- length(Positives.p2[[m]])*ratio.p1
#         
#         test.negative_folds[[m]] <- sample(Negatives, num.n.p2[[m]], replace = FALSE)
#         test.positive_folds[[m]] <- Positives.p2[[m]]
#       }
#       
#       ############
#       ## Step 8.2 ##: Build a model using the agg of SOFS-I selected features
#       ############
#       
#       agg.f_measure <- vector(mode="list",length=K.val)
#       agg.FNR <- vector(mode="list",length=K.val)
#       agg.FPR <- vector(mode="list",length=K.val)
#       agg.v.ratio <- vector(mode="list",length=K.val)
#       
#       for(n in 1:K.val) {
#         
#         i.train_data_N <- unlist(train.negative_folds[-n])
#         i.test_data_N <- train.negative_folds[[n]]
#         
#         i.train_data_P <- unlist(train.positive_folds[-n])
#         i.test_data_P <- train.positive_folds[[n]]
#         
#         # All train and all test data
#         i.train_data <- slice(AllData, sort(append(i.train_data_N,i.train_data_P)))
#         i.train_data <- i.train_data[,-1]
#         i.train_data$y <- as.factor(i.train_data$y)
#         
#         i.test_data <- slice(AllData, sort(append(i.test_data_N,i.test_data_P)))
#         i.test_data <- i.test_data[,-1]
#         i.test_data$y <- as.factor(i.test_data$y)
#         
#         model <- glm(y~., data = i.train_data[,c(unlist(agg.feats[i]),"y")], family=binomial)
#         prob_is<-predict(model, type="response")
#         MyRoc=pROC::roc(i.train_data$y~prob_is)
#         # plot.roc(MyRoc, main = paste("Fold", n), identity.col = "red")
#         threshold=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
#         prob<-predict(model, i.test_data, type="response")
#         
#         predicted <- ifelse(prob > as.numeric(threshold),1,0)
#         predicted <- factor(predicted, levels = c(0, 1))
#         
#         # Confusion matrix
#         res <- table(i.test_data$y, predicted);res
#         
#         tn <- res[1,1]
#         tp <- res[2,2]
#         fn <- res[2,1]
#         fp <- res[1,2]
#         
#         # Calculate FNR and FPR
#         agg.FNR[[n]] = fn/(fn+tp)
#         agg.FPR[[n]] = fp/(fp+tn)
#         
#         # Calculate the F Measure
#         agg.f_measure[[n]] = ((1+Beta.List[j]^2)*(tp))/((1+Beta.List[j]^2)*(tp)+(Beta.List[j]^2)*(fn)+fp)
#         print(agg.f_measure[[n]])  
#         
#         # Calculate the Verification Ratio
#         agg.v.ratio[[n]] = (tp+fp)/(tp+fn)
#         
#       }
#       
#       agg.v.ratio
#       # Calculate the mean, std dev, and half-width for FNR
#       vr.mean <- round(mean(unlist(agg.v.ratio)), 2);vr.mean
#       vr.sd <- round(sd(unlist(agg.v.ratio)), 2);vr.sd
#       
#       z.stat <- round(qnorm(0.975), 2)
#       vr.hw <- round(z.stat*vr.sd/sqrt(K.val), 2);vr.hw
#       
#       # VR.df.sd[j,1] <- paste(vr.mean, "±", vr.sd)
#       best.k.VR[j,1] <- vr.mean
#       
#       agg.FNR
#       # Calculate the mean, std dev, and half-width for FNR
#       fnr.mean <- round(mean(unlist(agg.FNR)), 2);fnr.mean
#       fnr.sd <- round(sd(unlist(agg.FNR)), 2);fnr.sd
#       # std_dev.df$FNR.sd[i] <- fnr.sd
#       
#       z.stat <- round(qnorm(0.975), 2)
#       fnr.hw <- round(z.stat*fnr.sd/sqrt(K.val), 2);fnr.hw
#       
#       # FNR.df.sd[j,1] <- paste(fnr.mean, "±", fnr.sd)
#       best.k.FNR[j,1] <- fnr.mean
#       
#       agg.FPR
#       # Calculate the mean, std dev, and half-width for FPR
#       fpr.mean <- round(mean(unlist(agg.FPR)), 2);fpr.mean
#       fpr.sd <- round(sd(unlist(agg.FPR)), 2);fpr.sd
#       # std_dev.df$FPR.sd[i] <- fpr.sd
#       
#       z.stat <- round(qnorm(0.975), 2)
#       fpr.hw <- round(z.stat*fpr.sd/sqrt(K.val), 2);fpr.hw
#       
#       # FPR.df.sd[j,1] <- paste(fpr.mean, "±", fpr.sd)
#       best.k.FPR[j,1] <- fpr.mean
#       
#       agg.f_measure
#       # Calculate the mean, std dev, and half-width for F Measure
#       fmeasure.mean <- round(mean(unlist(agg.f_measure)), 2);fmeasure.mean
#       fmeasure.sd <- round(sd(unlist(agg.f_measure)), 2)
#       # std_dev.df$FM.sd[i] <- fmeasure.sd
#       
#       z.stat <- round(qnorm(0.975), 2)
#       fmeasure.hw <- round(z.stat*fmeasure.sd/sqrt(K.val), 2);fmeasure.hw
#       
#       # f_measure.df.sd[j,1] <- paste(fmeasure.mean, "±", fmeasure.sd)
#       best.k.FM[j,1] <- fmeasure.mean
#       
#       i = i + 4
#     }
#     print(K.val)
#     
#     best.k.VR
#     print(best.k.VR)
#     
#     if (best.k.VR[1,1] < best.k.VR[2,1]) {
#       if(best.k.VR[2,1] < best.k.VR[3,1]) {
#         if(best.k.VR[3,1] < best.k.VR[4,1]) {
#           if(best.k.VR[1,1] < best.k.VR[4,1]){
#             best.k.val[h] <- K.val
#             best.k.found <- TRUE
#           }
#           else if ((best.k.VR[1,1] == 0) & (best.k.VR[4,1] == 0)) {
#             best.k.val[h] <- K.val
#             best.k.found <- TRUE
#           }
#         }
#       }
#     }
#     
#     i.VR.val <- rownames_to_column(best.k.VR, var = "Beta")
#     k.history.VR <- rbind(k.history.VR, cbind(i.VR.val, rep(K.val,4), rep(SF.List[h], 4)))
#     
#     K.val = K.val + 10
#     
#     if (K.val > 500) {best.k.found <- TRUE}
#   }
# }
# 
# 
# colnames(k.history.VR) <- c("Beta", "VR", "K", "SF")
# 
# for (i.SF in SF.List) {
# 
#   temp.k.sf <- k.history.VR %>% filter(SF == i.SF)
#   max.k <- max (temp.k.sf$K)
# 
# 
#   temp.k.best.history.VR <- temp.k.sf %>% filter(K == max.k)
#   temp.k.comp.history.VR <- temp.k.sf %>% filter(K != max.k)
# 
#   fpr.plot <- (ggplot(data = temp.k.comp.history.VR , aes(x = Beta, y = FPR, color = "springgreen", group = K))
#    + geom_point()
#    + geom_line()
#    + geom_point(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, color = "red", group = K))
#    + geom_line(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, color = "red", group = K))
#    + geom_dl(data = temp.k.best.history.VR, aes(label = paste("K =",max.k), color = "red"), method = list(dl.trans(x = x + .2), "last.points"))
#    + ylab("FPR")
#    + ggtitle(paste("FPR when SF=", i.SF))
#    + theme(plot.title = element_text(hjust=0.5)))
# 
#   plot(fpr.plot)
# }
# 
# 
# i.SF = 0.5
# temp.k.sf <- k.history.VR %>% filter(SF == i.SF)
# max.k <- 30
# 
# temp.k.best.history.VR <- temp.k.sf %>% filter(K == max.k)
# temp.k.comp.history.VR <- temp.k.sf %>% filter(K != max.k)
# 
# fpr.plot <- (ggplot(data = temp.k.comp.history.VR , aes(x = Beta, y = FPR, group = K))
#              + geom_point(color = "blue")
#              + geom_line(color = "blue")
#              + geom_point(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_line(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_dl(data = temp.k.best.history.VR, aes(label = paste("K =",max.k), fontface = "bold"), color = "red", method = list(cex = 1.65, dl.trans(x = x + .2), "last.points"))
#              + ylab(expression(paste("FPR (",italic("s"),"=0.5)")))
#              + ylim(c(0, 0.41))
#              + theme(plot.title = element_text(hjust=0.5, size = 18),
#                      axis.text.x = element_blank(),
#                      axis.text.y = ggtext::element_markdown(size = 24, face = "plain"),
#                      axis.title.x = element_blank(),
#                      axis.title.y = element_text(size = 26, face = "plain"),
#                      axis.ticks.x = element_blank()))
# 
# plot(fpr.plot)
# 
# 
# i.SF = 0.6
# temp.k.sf <- k.history.VR %>% filter(SF == i.SF)
# max.k <- 210
# 
# temp.k.best.history.VR <- temp.k.sf %>% filter(K == max.k)
# temp.k.comp.history.VR <- temp.k.sf %>% filter(K != max.k)
# 
# fpr.plot <- (ggplot(data = temp.k.comp.history.VR , aes(x = Beta, y = FPR, group = K))
#              + geom_point(color = "blue")
#              + geom_line(color = "blue")
#              + geom_point(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_line(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2, color = "red")
#              + geom_dl(data = temp.k.best.history.VR, aes(label = paste("K =",max.k), fontface = "bold"), color = "red", method = list(cex = 1.6, dl.trans(x = x + .2), "last.points"))
#              + ylab(expression(paste("FPR (",italic("s"),"=0.6)")))
#              + ylim(c(0, 0.41))
#              + theme(plot.title = element_text(hjust=0.5, size = 18),
#                      axis.text.x = element_text(size = 24, face = "plain"),
#                      axis.text.y = element_text(size = 24, face = "plain"),
#                      axis.title.x = element_blank(),
#                      axis.title.y = element_text(size = 26, face = "plain")))
# 
# plot(fpr.plot)
# 
# i.SF = 0.7
# temp.k.sf <- k.history.VR %>% filter(SF == i.SF)
# max.k <- 110
# 
# temp.k.best.history.VR <- temp.k.sf %>% filter(K == max.k)
# temp.k.comp.history.VR <- temp.k.sf %>% filter(K != max.k)
# 
# fpr.plot <- (ggplot(data = temp.k.comp.history.VR , aes(x = Beta, y = FPR, group = K))
#              + geom_point(color = "blue")
#              + geom_line(color = "blue")
#              + geom_point(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_line(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_dl(data = temp.k.best.history.VR, aes(label = paste("K =",max.k), fontface = "bold"), color = "red", method = list(cex = 1.65, dl.trans(x = x + .2), "last.points"))
#              + ylab(expression(paste("FPR (",italic("s"),"=0.7)")))
#              + ylim(c(0, 0.41))
#              + theme(plot.title = element_text(hjust=0.5, size = 18),
#                      axis.text.x = element_blank(),
#                      axis.text.y = ggtext::element_markdown(size = 24, face = "plain"),
#                      axis.title.x = element_blank(),
#                      axis.title.y = element_text(size = 26, face = "plain"),
#                      axis.ticks.x = element_blank()))
# 
# plot(fpr.plot)
# 
# 
# i.SF = 0.8
# temp.k.sf <- k.history.VR %>% filter(SF == i.SF)
# max.k <- 180
# 
# temp.k.best.history.VR <- temp.k.sf %>% filter(K == max.k)
# temp.k.comp.history.VR <- temp.k.sf %>% filter(K != max.k)
# 
# fpr.plot <- (ggplot(data = temp.k.comp.history.VR , aes(x = Beta, y = FPR, group = K))
#              + geom_point(color = "blue")
#              + geom_line(color = "blue")
#              + geom_point(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2.5, color = "red")
#              + geom_line(data = temp.k.best.history.VR , aes(x = Beta, y = FPR, group = K), size = 2, color = "red")
#              + geom_dl(data = temp.k.best.history.VR, aes(label = paste("K =",max.k), fontface = "bold"), color = "red", method = list(cex = 1.6, dl.trans(x = x + .2), "last.points"))
#              + ylab(expression(paste("FPR (",italic("s"),"=0.8)")))
#              + ylim(c(0, 0.41))
#              + theme(plot.title = element_text(hjust=0.5, size = 18),
#                      axis.text.x = element_text(size = 24, face = "plain"),
#                      axis.text.y = element_text(size = 24, face = "plain"),
#                      axis.title.x = element_blank(),
#                      axis.title.y = element_text(size = 26, face = "plain")))
# 
# plot(fpr.plot)


############
## Set parameters and seeds
############

# Folds
K <- c(70, 30, 210, 110, 180)

# New Seeds
s.folds.new<-4321

# Predefined Variables
i = 1
Beta.List <- c(1,2,3,4)
SF.List <- c(0.1, 0.5, 0.6, 0.7, 0.8)

########################################
# Create data frames to store averages #
########################################

# Crude LASSO
f_measure.crude.df <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.crude.df <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.crude.df <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.crude.df <- data.frame(row.names = c("1", "2", "3", "4"))

# LASSO-I
f_measure.bm.df <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.bm.df <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.bm.df <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.bm.df <- data.frame(row.names = c("1", "2", "3", "4"))

# SOFS-I
f_measure.df <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.df <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.df <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.df <- data.frame(row.names = c("1", "2", "3", "4"))

##############################################
# Create data frames to store averages w/ sd #
##############################################

# Crude LASSO
f_measure.crude.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.crude.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.crude.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.crude.df <- data.frame(row.names = c("1", "2", "3", "4"))

# LASSO-I
f_measure.bm.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.bm.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.bm.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.bm.df <- data.frame(row.names = c("1", "2", "3", "4"))

# SOFS-I
f_measure.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FNR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
FPR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
var_sel.df <- data.frame(row.names = c("1", "2", "3", "4"))

std_dev.df.bm <- data.frame(matrix(ncol = 3, nrow = 20))
std_dev.df <- data.frame(matrix(ncol = 3, nrow = 20))

var.names <- c("FM.se", "FNR.se", "FPR.se")
colnames(std_dev.df.bm) <- var.names
colnames(std_dev.df) <- var.names

#######################################################
# Create data frames to store half widths of averages #
#######################################################

# Crude LASSO
Crude.FM.HW <- data.frame(row.names = c("1", "2", "3", "4"))
Crude.FNR.HW <- data.frame(row.names = c("1", "2", "3", "4"))
Crude.FPR.HW <- data.frame(row.names = c("1", "2", "3", "4"))

# LASSO-I
LASSO.FM.HW <- data.frame(row.names = c("1", "2", "3", "4"))
LASSO.FNR.HW <- data.frame(row.names = c("1", "2", "3", "4"))
LASSO.FPR.HW <- data.frame(row.names = c("1", "2", "3", "4"))

# SOFS-I
SOFSI.FM.HW <- data.frame(row.names = c("1", "2", "3", "4"))
SOFSI.FNR.HW <- data.frame(row.names = c("1", "2", "3", "4"))
SOFSI.FPR.HW <- data.frame(row.names = c("1", "2", "3", "4"))


###########################################
# Create data frames to store differences #
###########################################

# (LASSO-I) - (Crude LASSO)
diff.LC.FM.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FNR.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FPR.df <- data.frame(row.names = c("1", "2", "3", "4"))

# (SOFS-I) - (Crude LASSO)
diff.SC.FM.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FNR.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FPR.df <- data.frame(row.names = c("1", "2", "3", "4"))

# (SOFS-I) - (LASSO-I)
diff.SL.FM.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FNR.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FPR.df <- data.frame(row.names = c("1", "2", "3", "4"))

#################################################
# Create data frames to store differences w/ sd #
#################################################

# (LASSO-I) - (Crude LASSO)
diff.LC.FM.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FNR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FPR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))

# (SOFS-I) - (Crude LASSO)
diff.SC.FM.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FNR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FPR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))

# (SOFS-I) - (LASSO-I)
diff.SL.FM.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FNR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FPR.df.se <- data.frame(row.names = c("1", "2", "3", "4"))

######################################################
# Create data frames to store difference half widths #
######################################################

# (SOFS-I) - (Crude LASSO)
diff.SC.FM.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FNR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SC.FPR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))

# (LASSO-I) - (Crude LASSO)
diff.LC.FM.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FNR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.LC.FPR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))

# (SOFS-I) - (LASSO-I)
diff.SL.FM.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FNR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))
diff.SL.FPR.HW.df <- data.frame(row.names = c("1", "2", "3", "4"))

for (j in 1:length(Beta.List)) {
  for(k in 1:length(SF.List)) {
    
    #Create new samples for training
    train.negative_folds <- vector(mode="list",length=K[k])
    train.positive_folds <- vector(mode="list",length=K[k])
    test.negative_folds <- vector(mode="list",length=K[k])
    test.positive_folds <- vector(mode="list",length=K[k])
    
    unique.p <- vector(mode="list",length=K[k])
    Positives.p2 <- vector(mode="list",length=K[k])
    
    unique.n <- vector(mode="list",length=K[k])
    Negatives.p2 <- vector(mode="list",length=K[k])
    
    num.n.p2 <- vector(mode = "list", length = K[k])
    ratio.p1 <- round(length(Negatives)/length(Positives))
    
    set.seed(s.folds.new) # this is to ensure we get the same folds as each other.
    for (m in 1:K[k]) {
      train.negative_folds[[m]] <- sample(Negatives, floor(length(Positives)*2), replace = TRUE)
      train.positive_folds[[m]] <- sample(Positives, length(Positives), replace = TRUE)
      
      unique.p[[m]] <- unique(train.positive_folds[[m]])
      Positives.p2[[m]] <- Positives[!(Positives %in% unique.p[[m]])]
      
      unique.n[[m]] <- unique(train.negative_folds[[m]])
      Negatives.p2[[m]] <- Negatives[!(Negatives %in% unique.n[[m]])]
      
      num.n.p2[[m]] <- length(Positives.p2[[m]])*ratio.p1
      
      test.negative_folds[[m]] <- sample(Negatives, num.n.p2[[m]], replace = FALSE)
      test.positive_folds[[m]] <- Positives.p2[[m]]
    }
    
    ############
    ## Step 8.2 ##: Build a model using the intersection of Crude LASSO selected features
    ############
    
    intersect.f_measure.crude <- vector(mode="list",length=K[k])
    intersect.FNR.crude <- vector(mode="list",length=K[k])
    intersect.FPR.crude <- vector(mode="list",length=K[k])
    n = 1
    for(n in 1:K[k]) {
      i.train_data_N <- unlist(train.negative_folds[-n])
      i.test_data_N <- test.negative_folds[[n]]
      
      i.train_data_P <- unlist(train.positive_folds[-n])
      i.test_data_P <- test.positive_folds[[n]]
      
      # All train and all test data
      i.train_data <- slice(AllData, sort(append(i.train_data_N,i.train_data_P)))
      i.train_data$y <- as.factor(i.train_data$y)
      
      i.test_data <- slice(AllData, sort(append(i.test_data_N,i.test_data_P)))
      i.test_data$y <- as.factor(i.test_data$y)
      
      model <- glm(y~., data = i.train_data[,c(feats.crude,"y")], family=binomial)
      prob_is<-predict(model, type="response")
      MyRoc=pROC::roc(i.train_data$y~prob_is)
      plot.roc(MyRoc, main = paste("Fold", n), identity.col = "red")
      threshold=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
      prob<-predict(model, i.test_data, type="response")
      
      predicted <- ifelse(prob > as.numeric(threshold),1,0)
      predicted <- factor(predicted, levels = c(0, 1))
      
      # Confusion matrix
      res <- table(i.test_data$y, predicted);res
      
      tn <- res[1,1]
      tp <- res[2,2]
      fn <- res[2,1]
      fp <- res[1,2]
      
      # Calculate FNR and FPR
      intersect.FNR.crude[[n]] = fn/(fn+tp)
      intersect.FPR.crude[[n]] = fp/(fp+tn)
      
      # Calculate the F Measure
      intersect.f_measure.crude[[n]] = ((1+Beta.List[j]^2)*(tp))/((1+Beta.List[j]^2)*(tp)+(Beta.List[j]^2)*(fn)+fp)
      print(intersect.f_measure.crude[[n]])  
      
    }
    
    if (k == 1) {
      intersect.FNR.crude
      # Calculate the mean, std dev, and half-width for FNR
      fnr.crude.mean <- round(mean(unlist(intersect.FNR.crude)),2);fnr.crude.mean
      fnr.crude.se <- round((sd(unlist(intersect.FNR.crude))/sqrt(K[k])),3);fnr.crude.se
      t_stat <- qt(0.975, K[k]-1)
      fnr.crude.hw <- round(t_stat*fnr.crude.se,2);fnr.crude.hw
      
      FNR.crude.df.se[j,1] <- paste(fnr.crude.mean, "±", fnr.crude.se)
      FNR.crude.df[j,1] <- fnr.crude.mean
      FNR.crude.df[j,2] <- fnr.crude.se
      Crude.FNR.HW[j,1] <- fnr.crude.hw
      
      
      intersect.FPR.crude
      # Calculate the mean, std dev, and half-width for FPR
      fpr.crude.mean <- round(mean(unlist(intersect.FPR.crude)),2);fpr.crude.mean
      fpr.crude.se <- round((sd(unlist(intersect.FPR.crude))/sqrt(K[k])),3);fpr.crude.se
      t_stat <- qt(0.975, K[k]-1)
      fpr.crude.hw <- round(t_stat*fpr.crude.se,2);fpr.crude.hw
      
      FPR.crude.df.se[j,1] <- paste(fpr.crude.mean, "±", fpr.crude.se)    
      FPR.crude.df[j,1] <- fpr.crude.mean    
      FPR.crude.df[j,2] <- fpr.crude.se
      Crude.FPR.HW[j,1] <- fpr.crude.hw
      
      
      intersect.f_measure.crude
      # Calculate the mean, std dev, and half-width for F Measure
      fmeasure.crude.mean <- round(mean(unlist(intersect.f_measure.crude)),2);fmeasure.crude.mean
      fmeasure.crude.se <- round((sd(unlist(intersect.f_measure.crude))/sqrt(K[k])),3)
      t_stat <- qt(0.975, K[k]-1)
      fmeasure.crude.hw <- round(t_stat*fmeasure.crude.se,3);fmeasure.crude.hw
      
      f_measure.crude.df.se[j,1] <- paste(fmeasure.crude.mean, "±", fmeasure.crude.se)
      f_measure.crude.df[j,1] <- fmeasure.crude.mean
      f_measure.crude.df[j,2] <- fmeasure.crude.se
      Crude.FM.HW[j,1] <- fmeasure.crude.hw
      
      var_sel.crude.df[j,1] <- length(feats.crude)
      
    }
    
    
    ############
    ## Step 8.3 ##: Build a model using the intersection of LASSO selected features
    ############
    
    intersect.f_measure.bm <- vector(mode="list",length=K[k])
    intersect.FNR.bm <- vector(mode="list",length=K[k])
    intersect.FPR.bm <- vector(mode="list",length=K[k])
    
    for(n in 1:K[k]) {
      i.train_data_N <- unlist(train.negative_folds[-n])
      i.test_data_N <- train.negative_folds[[n]]
      
      i.train_data_P <- unlist(train.positive_folds[-n])
      i.test_data_P <- train.positive_folds[[n]]
      
      # All train and all test data
      i.train_data <- slice(AllData, sort(append(i.train_data_N,i.train_data_P)))
      i.train_data$y <- as.factor(i.train_data$y)
      
      i.test_data <- slice(AllData, sort(append(i.test_data_N,i.test_data_P)))
      i.test_data$y <- as.factor(i.test_data$y)
      
      model <- glm(y~., data = i.train_data[,c(unlist(agg.feats.bm[i]),"y")], family=binomial)
      prob_is<-predict(model, type="response")
      MyRoc=pROC::roc(i.train_data$y~prob_is)
      plot.roc(MyRoc, main = paste("Fold", n), identity.col = "red")
      threshold=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
      prob<-predict(model, i.test_data, type="response")
      
      predicted <- ifelse(prob > as.numeric(threshold),1,0)
      predicted <- factor(predicted, levels = c(0, 1))
      
      # Confusion matrix
      res <- table(i.test_data$y, predicted);res
      
      tn <- res[1,1]
      tp <- res[2,2]
      fn <- res[2,1]
      fp <- res[1,2]
      
      # Calculate FNR and FPR
      intersect.FNR.bm[[n]] = fn/(fn+tp)
      intersect.FPR.bm[[n]] = fp/(fp+tn)
      
      # Calculate the F Measure
      intersect.f_measure.bm[[n]] = ((1+Beta.List[j]^2)*(tp))/((1+Beta.List[j]^2)*(tp)+(Beta.List[j]^2)*(fn)+fp)
      print(intersect.f_measure.bm[[n]])  
      
    }
    
    intersect.FNR.bm
    # Calculate the mean, std dev, and half-width for FNR
    fnr.bm.mean <- round(mean(unlist(intersect.FNR.bm)),2);fnr.bm.mean
    fnr.bm.se <- round((sd(unlist(intersect.FNR.bm))/sqrt(K[k])),3);fnr.bm.se
    std_dev.df.bm$FNR.se[i] <- fnr.bm.se
    
    t_stat <- qt(0.975, K[k]-1)
    fnr.bm.hw <- round(t_stat*fnr.bm.se,2);fnr.bm.hw
    
    FNR.bm.df.se[j,k] <- paste(fnr.bm.mean, "±", fnr.bm.se)
    FNR.bm.df[j,k] <- fnr.bm.mean
    LASSO.FNR.HW[j,k] <- fnr.bm.hw
    
    
    intersect.FPR.bm
    # Calculate the mean, std dev, and half-width for FPR
    fpr.bm.mean <- round(mean(unlist(intersect.FPR.bm)),2);fpr.bm.mean
    fpr.bm.se <- round((sd(unlist(intersect.FPR.bm))/sqrt(K[k])),3);fpr.bm.se
    std_dev.df.bm$FPR.se[i] <- fpr.bm.se
    
    t_stat <- qt(0.975, K[k]-1)
    fpr.bm.hw <- round(t_stat*fpr.bm.se,2);fpr.bm.hw
    
    FPR.bm.df.se[j,k] <- paste(fpr.bm.mean, "±", fpr.bm.se)    
    FPR.bm.df[j,k] <- fpr.bm.mean    
    LASSO.FPR.HW[j,k] <- fpr.bm.hw
    
    
    intersect.f_measure.bm
    # Calculate the mean, std dev, and half-width for F Measure
    fmeasure.bm.mean <- round(mean(unlist(intersect.f_measure.bm)),2);fmeasure.bm.mean
    fmeasure.bm.se <- round((sd(unlist(intersect.f_measure.bm))/sqrt(K[k])),3)
    std_dev.df.bm$FM.se[i] <- fmeasure.bm.se
    
    t_stat <- qt(0.975, K[k]-1)
    fmeasure.bm.hw <- round(t_stat*fmeasure.bm.se,2);fmeasure.bm.hw
    
    f_measure.bm.df.se[j,k] <- paste(fmeasure.bm.mean, "±", fmeasure.bm.se)
    f_measure.bm.df[j,k] <- fmeasure.bm.mean
    LASSO.FM.HW[j,k] <- fmeasure.bm.hw
    
    
    var_sel.bm.df[j,k] <- length(agg.feats.bm[[i]])
    
    ############
    ## Step 8.4 ##: Build a model using the intersection of SOFS-I selected features
    ############
    
    intersect.f_measure <- vector(mode="list",length=K[k])
    intersect.FNR <- vector(mode="list",length=K[k])
    intersect.FPR <- vector(mode="list",length=K[k])
    
    for(n in 1:K[k]) {
      i.train_data_N <- unlist(train.negative_folds[-n])
      i.test_data_N <- train.negative_folds[[n]]
      
      i.train_data_P <- unlist(train.positive_folds[-n])
      i.test_data_P <- train.positive_folds[[n]]
      
      # All train and all test data
      i.train_data <- slice(AllData, sort(append(i.train_data_N,i.train_data_P)))
      i.train_data$y <- as.factor(i.train_data$y)
      
      i.test_data <- slice(AllData, sort(append(i.test_data_N,i.test_data_P)))
      i.test_data$y <- as.factor(i.test_data$y)
      
      model <- glm(y~., data = i.train_data[,c(unlist(agg.feats[i]),"y")], family=binomial)
      prob_is<-predict(model, type="response")
      MyRoc=pROC::roc(i.train_data$y~prob_is)
      plot.roc(MyRoc, main = paste("Fold", n), identity.col = "red")
      threshold=coords(MyRoc, "best", ret=c("threshold"), transpose = TRUE)
      prob<-predict(model, i.test_data, type="response")
      
      predicted <- ifelse(prob > as.numeric(threshold),1,0)
      predicted <- factor(predicted, levels = c(0, 1))
      
      # Confusion matrix
      res <- table(i.test_data$y, predicted);res
      
      tn <- res[1,1]
      tp <- res[2,2]
      fn <- res[2,1]
      fp <- res[1,2]
      
      # Calculate FNR and FPR
      intersect.FNR[[n]] = fn/(fn+tp)
      intersect.FPR[[n]] = fp/(fp+tn)
      
      # Calculate the F Measure
      intersect.f_measure[[n]] = ((1+Beta.List[j]^2)*(tp))/((1+Beta.List[j]^2)*(tp)+(Beta.List[j]^2)*(fn)+fp)
      print(intersect.f_measure[[n]])  
      
      
    }
    
    intersect.FNR
    # Calculate the mean, std dev, and half-width for FNR
    fnr.mean <- round(mean(unlist(intersect.FNR)), 2);fnr.mean
    fnr.se <- round((sd(unlist(intersect.FNR))/sqrt(K[k])), 3);fnr.se
    std_dev.df$FNR.se[i] <- fnr.se
    
    t_stat <- qt(0.975, K[k]-1)
    fnr.hw <- round(t_stat*fnr.se, 2);fnr.hw

    FNR.df.se[j,k] <- paste(fnr.mean, "±", fnr.se)
    FNR.df[j,k] <- fnr.mean
    SOFSI.FNR.HW[j,k] <- fnr.hw
    
    intersect.FPR
    # Calculate the mean, std dev, and half-width for FPR
    fpr.mean <- round(mean(unlist(intersect.FPR)), 2);fpr.mean
    fpr.se <- round((sd(unlist(intersect.FPR))/sqrt(K[k])), 3);fpr.se
    std_dev.df$FPR.se[i] <- fpr.se
    
    t_stat <- qt(0.975, K[k]-1)
    fpr.hw <- round(t_stat*fpr.se, 2);fpr.hw

    FPR.df.se[j,k] <- paste(fpr.mean, "±", fpr.se)
    FPR.df[j,k] <- fpr.mean
    SOFSI.FPR.HW[j,k] <- fpr.hw
    
    intersect.f_measure
    # Calculate the mean, std dev, and half-width for F Measure
    fmeasure.mean <- round(mean(unlist(intersect.f_measure)), 2);fmeasure.mean
    fmeasure.se <- round((sd(unlist(intersect.f_measure))/sqrt(K[k])), 3)
    std_dev.df$FM.se[i] <- fmeasure.se
    
    t_stat <- qt(0.975, K[k]-1)
    fmeasure.hw <- round(t_stat*fmeasure.se, 2);fmeasure.hw
    
    f_measure.df.se[j,k] <- paste(fmeasure.mean, "±", fmeasure.se)
    f_measure.df[j,k] <- fmeasure.mean
    SOFSI.FM.HW[j,k] <- fmeasure.hw
    
    var_sel.df[j,k] <- length(agg.feats[[i]])
    
    ### Difference Between SOFS-I and Crude LASSO
    #Mean
    diff.SC.FM.mean <- round(mean(unlist(intersect.f_measure) - unlist(intersect.f_measure.crude)),2)
    diff.SC.FNR.mean <- round(mean(unlist(intersect.FNR) - unlist(intersect.FNR.crude)),2)
    diff.SC.FPR.mean <- round(mean(unlist(intersect.FPR) - unlist(intersect.FPR.crude)),2)
    #Std Dev
    diff.SC.FM.se <- round(sd(unlist(intersect.f_measure) - unlist(intersect.f_measure.crude)),2)
    diff.SC.FNR.se <- round(sd(unlist(intersect.FNR) - unlist(intersect.FNR.crude)),2)
    diff.SC.FPR.se <- round(sd(unlist(intersect.FPR) - unlist(intersect.FPR.crude)),2)
    
    ### Difference Between LASSO-I and Crude LASSO
    #Mean
    diff.LC.FM.mean <- round(mean(unlist(intersect.f_measure.bm) - unlist(intersect.f_measure.crude)),2)
    diff.LC.FNR.mean <- round(mean(unlist(intersect.FNR.bm) - unlist(intersect.FNR.crude)),2)
    diff.LC.FPR.mean <- round(mean(unlist(intersect.FPR.bm) - unlist(intersect.FPR.crude)),2)
    #Std Dev
    diff.LC.FM.se <- round(sd(unlist(intersect.f_measure.bm) - unlist(intersect.f_measure.crude)),2)
    diff.LC.FNR.se <- round(sd(unlist(intersect.FNR.bm) - unlist(intersect.FNR.crude)),2)
    diff.LC.FPR.se <- round(sd(unlist(intersect.FPR.bm) - unlist(intersect.FPR.crude)),2)
    
    ### Differences Between SOFS-I, LASSO-I, and Crude LASSO
    #Mean
    diff.SL.FM.mean <- round(mean(unlist(intersect.f_measure) - unlist(intersect.f_measure.bm)),2)
    diff.SL.FNR.mean <- round(mean(unlist(intersect.FNR) - unlist(intersect.FNR.bm)),2)
    diff.SL.FPR.mean <- round(mean(unlist(intersect.FPR) - unlist(intersect.FPR.bm)),2)
    #Std Dev
    diff.SL.FM.se <- round(sd(unlist(intersect.f_measure) - unlist(intersect.f_measure.bm)),2)
    diff.SL.FNR.se <- round(sd(unlist(intersect.FNR) - unlist(intersect.FNR.bm)),2)
    diff.SL.FPR.se <- round(sd(unlist(intersect.FPR) - unlist(intersect.FPR.bm)),2)
    
    # (SOFS-I) - (Crude LASSO)
    diff.SC.FM.df[j,k] <- diff.SC.FM.mean
    diff.SC.FNR.df[j,k] <- diff.SC.FNR.mean
    diff.SC.FPR.df[j,k] <- diff.SC.FPR.mean
    
    # (LASSO-I) - (Crude LASSO)
    diff.LC.FM.df[j,k] <- diff.LC.FM.mean
    diff.LC.FNR.df[j,k] <- diff.LC.FNR.mean
    diff.LC.FPR.df[j,k] <- diff.LC.FPR.mean
    
    # (SOFS-I) - (LASSO-I)
    diff.SL.FM.df[j,k] <- diff.SL.FM.mean
    diff.SL.FNR.df[j,k] <- diff.SL.FNR.mean
    diff.SL.FPR.df[j,k] <- diff.SL.FPR.mean
    
    # SC w/ sd
    diff.SC.FM.df.se[j,k] <- paste(diff.SC.FM.mean, "±", diff.SC.FM.se)
    diff.SC.FNR.df.se[j,k] <- paste(diff.SC.FNR.mean, "±", diff.SC.FNR.se)
    diff.SC.FPR.df.se[j,k] <- paste(diff.SC.FPR.mean, "±", diff.SC.FPR.se)
    
    # LC w/ sd
    diff.LC.FM.df.se[j,k] <- paste(diff.LC.FM.mean, "±", diff.LC.FM.se)
    diff.LC.FNR.df.se[j,k] <- paste(diff.LC.FNR.mean, "±", diff.LC.FNR.se)
    diff.LC.FPR.df.se[j,k] <- paste(diff.LC.FPR.mean, "±", diff.LC.FPR.se)
    
    # SL w/ sd
    diff.SL.FM.df.se[j,k] <- paste(diff.SL.FM.mean, "±", diff.SL.FM.se)
    diff.SL.FNR.df.se[j,k] <- paste(diff.SL.FNR.mean, "±", diff.SL.FNR.se)
    diff.SL.FPR.df.se[j,k] <- paste(diff.SL.FPR.mean, "±", diff.SL.FPR.se)

    
    
    ### Difference Halfwidths
    t_stat <- qt(0.975, K[k]-1)
    
    # SC
    diff.SC.FM.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.f_measure))^2/K[k] + (sd(unlist(intersect.f_measure.crude)))^2/K[k])),3)
    diff.SC.FNR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FNR))^2/K[k] + (sd(unlist(intersect.FNR.crude)))^2/K[k])),3)
    diff.SC.FPR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FPR))^2/K[k] + (sd(unlist(intersect.FPR.crude)))^2/K[k])),3)
    
    # LC
    diff.LC.FM.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.f_measure.bm))^2/K[k] + (sd(unlist(intersect.f_measure.crude)))^2/K[k])),3)
    diff.LC.FNR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FNR.bm))^2/K[k] + (sd(unlist(intersect.FNR.crude)))^2/K[k])),3)
    diff.LC.FPR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FPR.bm))^2/K[k] + (sd(unlist(intersect.FPR.crude)))^2/K[k])),3)
    
    # SL
    diff.SL.FM.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.f_measure))^2/K[k] + (sd(unlist(intersect.f_measure.bm)))^2/K[k])),3)
    diff.SL.FNR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FNR))^2/K[k] + (sd(unlist(intersect.FNR.bm)))^2/K[k])),3)
    diff.SL.FPR.HW.df[j,k] <- round(t_stat*sqrt((sd(unlist(intersect.FPR))^2/K[k] + (sd(unlist(intersect.FPR.bm)))^2/K[k])),3)
    
    i = i + 1
  }
}

# Averages
colnames(FNR.crude.df) <- c("Mean", "Std_Dev")  
colnames(FPR.crude.df) <- c("Mean", "Std_Dev")
colnames(f_measure.crude.df) <- c("Mean", "Std_Dev")

colnames(FNR.bm.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(FPR.bm.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(f_measure.bm.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(var_sel.bm.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(FNR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(FPR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(f_measure.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(var_sel.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

# Averages w/ SD
colnames(FNR.bm.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(FPR.bm.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(f_measure.bm.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(var_sel.bm.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(FNR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(FPR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(f_measure.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(var_sel.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

# Half widths of Averages
colnames(Crude.FM.HW) <- c("Halfwidth")
colnames(Crude.FNR.HW) <- c("Halfwidth")
colnames(Crude.FPR.HW) <- c("Halfwidth")

colnames(LASSO.FM.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(LASSO.FNR.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(LASSO.FPR.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(SOFSI.FM.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(SOFSI.FNR.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")
colnames(SOFSI.FPR.HW) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

### Differences
colnames(diff.SC.FM.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SC.FNR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SC.FPR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.LC.FM.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.LC.FNR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.LC.FPR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.SL.FM.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SL.FNR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SL.FPR.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

### Differences w/ sd
colnames(diff.SC.FM.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SC.FNR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SC.FPR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.LC.FM.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.LC.FNR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.LC.FPR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.SL.FM.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SL.FNR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SL.FPR.df.se) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")


### Halfwidth of Differences
colnames(diff.SC.FM.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SC.FNR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SC.FPR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.LC.FM.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.LC.FNR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.LC.FPR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

colnames(diff.SL.FM.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")  
colnames(diff.SL.FNR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)") 
colnames(diff.SL.FPR.HW.df) <- c("10% (70)", "50% (30)", "60% (210)", "70% (110)", "80% (180)")

##############################
##############################
##############################
######  Creating Plots  ######
##############################
##############################
##############################

#####

##############################
##############################
###   Plotting F Measure   ###
##############################
##############################

# Balance Controlled LASSO
f_measure.bm <- rownames_to_column(f_measure.bm.df, var = "Beta")
f_measure.bm <- f_measure.bm %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "F_Measure")
f_measure.bm

(ggplot(data = f_measure.bm, aes(x = Beta, y = F_Measure, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("F Measure")
  + ggtitle("F Measure \n (LASSO)" )
  + theme(plot.title = element_text(hjust=0.5)))

# SOFS-I
f_measure <- rownames_to_column(f_measure.df, var = "Beta")
f_measure <- f_measure %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "F_Measure")
f_measure

(ggplot(data = f_measure, aes(x = Beta, y = F_Measure, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("F Measure")
  + ggtitle("F Measure \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

##############################
##############################
###      Plotting FNR      ###
##############################
##############################

# Balance Controlled LASSO
fnr.bm.vals <- rownames_to_column(FNR.bm.df, var = "Beta")
fnr.bm.vals <- fnr.bm.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FNR")
fnr.bm.vals

(ggplot(data = fnr.bm.vals, aes(x = Beta, y = FNR, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FNR")
  + ggtitle("FNR \n (LASSO)")
  + theme(plot.title = element_text(hjust=0.5)))

# SOFS-I
fnr.vals <- rownames_to_column(FNR.df, var = "Beta")
fnr.vals <- fnr.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FNR")
fnr.vals

(ggplot(data = fnr.vals, aes(x = Beta, y = FNR, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FNR")
  + ggtitle("FNR \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

##############################
##############################
###      Plotting FPR      ###
##############################
##############################

# Balance Controlled LASSO
fpr.bm.vals <- rownames_to_column(FPR.bm.df, var = "Beta")
fpr.bm.vals <- fpr.bm.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FPR")
fpr.bm.vals

(ggplot(data = fpr.bm.vals, aes(x = Beta, y = FPR, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FPR")
  + ggtitle("FPR \n (LASSO)")
  + theme(plot.title = element_text(hjust=0.5)))

# SOFS-I
fpr.vals <- rownames_to_column(FPR.df, var = "Beta")
fpr.vals <- fpr.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FPR")
fpr.vals

(ggplot(data = fpr.vals, aes(x = Beta, y = FPR, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FPR")
  + ggtitle("FPR \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

##############
# Halfwidths #
##############

# Balance Controlled LASSO
LASSO.FM.HW.vals <- rownames_to_column(LASSO.FM.HW, var = "Beta")
LASSO.FM.HW.vals <- LASSO.FM.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
LASSO.FM.HW.vals

LASSO.FNR.HW.vals <- rownames_to_column(LASSO.FNR.HW, var = "Beta")
LASSO.FNR.HW.vals <- LASSO.FNR.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
LASSO.FNR.HW.vals

LASSO.FPR.HW.vals <- rownames_to_column(LASSO.FPR.HW, var = "Beta")
LASSO.FPR.HW.vals <- LASSO.FPR.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
LASSO.FPR.HW.vals

# Balance Controlled SOFSI
SOFSI.FM.HW.vals <- rownames_to_column(SOFSI.FM.HW, var = "Beta")
SOFSI.FM.HW.vals <- SOFSI.FM.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
SOFSI.FM.HW.vals

SOFSI.FNR.HW.vals <- rownames_to_column(SOFSI.FNR.HW, var = "Beta")
SOFSI.FNR.HW.vals <- SOFSI.FNR.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
SOFSI.FNR.HW.vals

SOFSI.FPR.HW.vals <- rownames_to_column(SOFSI.FPR.HW, var = "Beta")
SOFSI.FPR.HW.vals <- SOFSI.FPR.HW.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")
SOFSI.FPR.HW.vals

################################################
# Create Dataframes with both LASSO and SOFS-I #
################################################

Combined.FM <- cbind(f_measure.bm, std_dev.df.bm$FM.se, LASSO.FM.HW.vals$Halfwidth, 
                      f_measure$F_Measure, std_dev.df$FM.se, SOFSI.FM.HW.vals$Halfwidth)

Combined.FNR <- cbind(fnr.bm.vals, std_dev.df.bm$FNR.se, LASSO.FNR.HW.vals$Halfwidth,  
                      fnr.vals$FNR, std_dev.df$FNR.se, SOFSI.FNR.HW.vals$Halfwidth)

Combined.FPR <- cbind(fpr.bm.vals, std_dev.df.bm$FPR.se, LASSO.FPR.HW.vals$Halfwidth, 
                      fpr.vals$FPR, std_dev.df$FPR.se, SOFSI.FPR.HW.vals$Halfwidth)

colnames(Combined.FM) <- c("Beta", "SF", "FM_LASSO", "FM_LASSO_SD", "FM_LASSO_HW", "FM_SOFSI", "FM_SOFSI_SD", "FM_SOFSI_HW")
colnames(Combined.FNR) <- c("Beta", "SF", "FNR_LASSO", "FNR_LASSO_SD", "FNR_LASSO_HW", "FNR_SOFSI", "FNR_SOFSI_SD", "FNR_SOFSI_HW")
colnames(Combined.FPR) <- c("Beta", "SF", "FPR_LASSO", "FPR_LASSO_SD", "FPR_LASSO_HW", "FPR_SOFSI", "FPR_SOFSI_SD", "FPR_SOFSI_HW")


####################################
####################################
###   Plotting F Measure w/ HW   ###
####################################
####################################

# Balance Controlled LASSO
(ggplot(data = Combined.FM, aes(x = Beta, y = FM_LASSO, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("F Measure")
  + ggtitle("F Measure \n (LASSO)" )
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FM_LASSO-FM_LASSO_HW, ymax = FM_LASSO+FM_LASSO_HW, fill = SF), alpha = 0.2)
  + geom_line(aes(group = SF)))

# SOFS-I
(ggplot(data = Combined.FM, aes(x = Beta, y = FM_SOFSI, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("F Measure")
  + ggtitle("F Measure \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FM_SOFSI-FM_SOFSI_HW, ymax = FM_SOFSI+FM_SOFSI_HW, fill = SF), alpha = 0.2)
  + geom_line(aes(group = SF)))

####################################
####################################
###      Plotting FNR w/ HWs     ###
####################################
####################################

# Balance Controlled LASSO
(ggplot(data = Combined.FNR, aes(x = Beta, y = FNR_LASSO, color = SF, group = SF)) 
 + geom_point() 
 + geom_line() 
 + ylab("FNR")
 + ggtitle("FNR \n (LASSO)" )
 + theme(plot.title = element_text(hjust=0.5))
 + geom_ribbon(aes(ymin = FNR_LASSO-FNR_LASSO_HW, ymax = FNR_LASSO+FNR_LASSO_HW, fill = SF), alpha = 0.2)
 + geom_line(aes(group = SF)))

# SOFS-I
(ggplot(data = Combined.FNR, aes(x = Beta, y = FNR_SOFSI, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FNR")
  + ggtitle("FNR \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FNR_SOFSI-FNR_SOFSI_HW, ymax = FNR_SOFSI+FNR_SOFSI_HW, fill = SF), alpha = 0.2)
  + geom_line(aes(group = SF)))

####################################
####################################
###      Plotting FPR w/ HWs     ###
####################################
####################################

# Balance Controlled LASSO
(ggplot(data = Combined.FPR, aes(x = Beta, y = FPR_LASSO, color = SF, group = SF)) 
 + geom_point() 
 + geom_line() 
 + ylab("FPR")
 + ggtitle("FPR \n (LASSO)" )
 + theme(plot.title = element_text(hjust=0.5))
 + geom_ribbon(aes(ymin = FPR_LASSO-FPR_LASSO_HW, ymax = FPR_LASSO+FPR_LASSO_HW, fill = SF), alpha = 0.2)
 + geom_line(aes(group = SF)))

# SOFS-I
(ggplot(data = Combined.FPR, aes(x = Beta, y = FPR_SOFSI, color = SF, group = SF)) 
  + geom_point() 
  + geom_line() 
  + ylab("FPR")
  + ggtitle("FPR \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FPR_SOFSI-FPR_SOFSI_HW, ymax = FPR_SOFSI+FPR_SOFSI_HW, fill = SF), alpha = 0.2)
  + geom_line(aes(group = SF)))

###############################
# Scatter Plot of FNR vs FPR  #
###############################

lasso.rates <- cbind(Combined.FM[c(1,2,3,4,5)],Combined.FNR[c(3,4,5)],Combined.FPR[c(3,4,5)])
sofs.rates <- cbind(Combined.FM[c(1,2,6,7,8)],Combined.FNR[c(6,7,8)],Combined.FPR[c(6,7,8)])

test.lasso.rates <- lasso.rates
test.sofs.rates <- sofs.rates

colnames(test.lasso.rates) <- c("Beta", "SF", "FM", "FM_SD", "FM_HW", "FNR", "FNR_SD", "FNR_HW", "FPR", "FPR_SD", "FPR_HW")
colnames(test.sofs.rates) <- c("Beta", "SF", "FM", "FM_SD", "FM_HW", "FNR", "FNR_SD", "FNR_HW", "FPR", "FPR_SD", "FPR_HW")

test.lasso.rates$Method <- "LASSO"
test.sofs.rates$Method <- "SOFSI"

merged.rates <- rbind(test.lasso.rates,test.sofs.rates)

### Sample 1
(ggplot(data = sofs.rates, aes(x = FNR_SOFSI , y = FPR_SOFSI, group = SF)) 
  + geom_point(aes(shape=SF), color = "blue", size = 4)
  + ggrepel::geom_text_repel(data = sofs.rates, aes(x = FNR_SOFSI, y = FPR_SOFSI,label = Beta), point.padding = 5)
  
  + geom_point(data = lasso.rates,mapping = aes(x = FNR_LASSO, y = FPR_LASSO, shape = SF), size = 4, color = "red")
  + ggrepel::geom_text_repel(data = lasso.rates, aes(x = FNR_LASSO, y = FPR_LASSO, label = Beta), point.padding = 5)
  
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + ylab("FPR")
  + xlab("FNR")
  + ggtitle(paste("FNR vs. FPR \n (SOFS-I)"))
  + theme(plot.title = element_text(hjust=0.5)))

for(B in 1:4) {
  
  temp.lasso <- lasso.rates %>% filter(Beta == paste("Beta =", B))
  temp.sofs <- sofs.rates %>% filter(Beta == paste("Beta =", B))
  
  plot.values = (ggplot(data = temp.sofs, aes(x = FNR_SOFSI , y = FPR_SOFSI, group = SF)) 
   + geom_point(aes(shape=SF), size = 4, color = "blue")
   + ggrepel::geom_text_repel(data = temp.sofs, aes(x = FNR_SOFSI, y = FPR_SOFSI,label = Beta), point.padding = 5)
   
   + geom_point(data = temp.lasso,mapping = aes(x = FNR_LASSO, y = FPR_LASSO, shape = SF), size = 4, color = "red")
   + ggrepel::geom_text_repel(data = temp.lasso, aes(x = FNR_LASSO, y = FPR_LASSO, label = Beta), point.padding = 5)
   
   + scale_shape_manual(values = c(15, 19, 17, 8, 18))
   + scale_color_manual(values = c("red", "blue"))
   + ylab("FPR")
   + ylim(0, 0.41)
   + xlab("FNR")
   + xlim(0,0.21)
   + ggtitle(paste("FNR vs. FPR \n (SOFS-I, Beta=", B,")"))
   + theme(plot.title = element_text(hjust=0.5)))
  
  plot(plot.values)
  
}

### Sample 2
(ggplot(data = merged.rates, aes(x = FNR , y = FPR, group = SF)) 
  + geom_point(aes(shape=SF, color = Method), size = 4)
  + ggrepel::geom_text_repel(aes(x = FNR, y = FPR,label = Beta), point.padding = 8)

  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + scale_color_manual(values = c("red", "blue"))
  + ylab("FPR")
  + xlab("FNR")
  + ggtitle(paste("FNR vs. FPR"))
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))

### Sample 3
temp.merged.rates <- merged.rates %>% filter(Beta == "2" | Beta == "4")

x_mid <- (max(merged.rates$FNR)+min(merged.rates$FNR))/2
y_mid <- (max(merged.rates$FPR)+min(merged.rates$FPR))/2

(ggplot(data = merged.rates, aes(x = FNR , y = FPR, group = SF)) 
  + geom_point(aes(shape=SF, color = Method), size = 12)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18), name = expression(paste(italic("s"),"(K')")))
  + scale_color_manual(values = c("red", "blue"))
  + ylab("FPR")
  + geom_vline(xintercept = x_mid, size = 1)
  + xlab("FNR")
  + xlim(c(0,0.4))
  + geom_hline(yintercept = y_mid, size = 1)
  # + ggtitle(paste("FNR vs. FPR"))
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 22, face = "plain"),
          axis.text.y = element_text(size = 24, face = "plain"),  
          axis.title.x = element_text(size = 22, face = "plain"),
          axis.title.y = element_text(size = 24, face = "plain"),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20)))

#######################################
# Scatter Plot of Average vs Std Dev  #
#######################################

temp.sofs <- merged.rates %>% filter(Method == "SOFSI")

# SOFS-I FM
x_mid <- (max(temp.sofs$FM)+min(temp.sofs$FM))/2
y_mid <- (max(temp.sofs$FM_SD)+min(temp.sofs$FM_SD))/2

# With Point Labels
(ggplot(data = temp.sofs, aes(x = FM, y = FM_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + ggrepel::geom_text_repel(data = temp.sofs, aes(label = Beta), point.padding = 7 )
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + ylab("Std Dev")
  + xlab("F Measure")
  + scale_x_continuous(breaks=c(0.75, 0.8, 0.85, 0.9, 0.95, 1))
  + scale_y_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125))
  + ggtitle("F Measure vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
        axis.text.x = element_text(size = 15, face = "plain"),
        axis.text.y = element_text(size = 15, face = "plain"),  
        axis.title.x = element_text(size = 17, face = "plain"),
        axis.title.y = element_text(size = 17, face = "plain"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)))

# Without Point Labels
(ggplot(data = temp.sofs, aes(x = FM, y = FM_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + ylab("Std Dev")
  + xlab("F Measure")
  + scale_x_continuous(breaks=c(0.75, 0.8, 0.85, 0.9, 0.95, 1))
  + scale_y_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125))
  + ggtitle("F Measure vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))


# SOFS-I FNR
x_mid <- (max(temp.sofs$FNR)+min(temp.sofs$FNR))/2
y_mid <- (max(temp.sofs$FNR_SD)+min(temp.sofs$FNR_SD))/2

# With Point Labels
(ggplot(data = temp.sofs, aes(x = FNR, y = FNR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + ggrepel::geom_text_repel(data = temp.sofs, aes(label = Beta), point.padding = 10 )
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + ylab("Std Dev")
  + xlab("FNR")
  + ggtitle("FNR vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))

# Without Point Labels
(ggplot(data = temp.sofs, aes(x = FNR, y = FNR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + ylab("Std Dev")
  + xlab("FNR")
  + ggtitle("FNR vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))



# SOFS-I FPR
x_mid <- (max(temp.sofs$FPR)+min(temp.sofs$FPR))/2
y_mid <- (max(temp.sofs$FPR_SD)+min(temp.sofs$FPR_SD))/2

# With Point Labels
(ggplot(data = temp.sofs, aes(x = FPR, y = FPR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + ggrepel::geom_text_repel(data = temp.sofs, aes(label = Beta), point.padding = 10 )
  + scale_shape_manual(values = c(15, 19, 17, 8, 18)) 
  + ylab("Std Dev")
  + xlab("FPR")
  + ggtitle("FPR vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)  
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))

# Without Point Labels
(ggplot(data = temp.sofs, aes(x = FPR, y = FPR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 7)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18)) 
  + ylab("Std Dev")
  + xlab("FPR")
  + ggtitle("FPR vs. Std Dev \n (SOFS-I)")
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)  
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 15, face = "plain"),
          axis.text.y = element_text(size = 15, face = "plain"),  
          axis.title.x = element_text(size = 17, face = "plain"),
          axis.title.y = element_text(size = 17, face = "plain"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)))


###############
###############
# Same Y Axis #

x_mid <- (max(temp.sofs$FM)+min(temp.sofs$FM))/2
y_mid <- (max(temp.sofs$FM_SD)+min(temp.sofs$FM_SD))/2

(ggplot(data = temp.sofs, aes(x = FM, y = FM_SD, group = SF)) 
 + geom_point(aes(shape=SF, color = SF), size = 10)
 + scale_shape_manual(values = c(15, 19, 17, 8, 18))
 + ylab("Std Dev")
 + ylim(0, 0.15)
 + xlab("F Measure")
 + scale_x_continuous(breaks=c(0.75, 0.8, 0.85, 0.9, 0.95, 1))
 # + scale_y_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
 + geom_vline(xintercept = x_mid, size = 1)
 + geom_hline(yintercept = y_mid, size = 1)
 + theme(plot.title = element_text(hjust=0.5, size = 18),
         axis.text.x = element_text(size = 24, face = "plain"),
         axis.text.y = element_text(size = 24, face = "plain"),  
         axis.title.x = element_text(size = 26, face = "plain"),
         axis.title.y = element_text(size = 26, face = "plain"),
         legend.position = "none"))

x_mid <- (max(temp.sofs$FNR)+min(temp.sofs$FNR))/2
y_mid <- (max(temp.sofs$FNR_SD)+min(temp.sofs$FNR_SD))/2

(ggplot(data = temp.sofs, aes(x = FNR, y = FNR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 10)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  # + ylab("Std Dev")
  + xlab("FNR")
  + ylim(0, 0.15)
  # + scale_y_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 24, face = "plain"),
          axis.title.x = element_text(size = 26, face = "plain"),
          axis.text.y = element_blank(),  
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 22),
          legend.position = c(0.75, 0.25)))

x_mid <- (max(temp.sofs$FPR)+min(temp.sofs$FPR))/2
y_mid <- (max(temp.sofs$FPR_SD)+min(temp.sofs$FPR_SD))/2

(ggplot(data = temp.sofs, aes(x = FPR, y = FPR_SD, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 10)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18)) 
  # + ylab("Std Dev")
  + xlab("FPR")
  + ylim(0,0.15)
  # + scale_y_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 18),
          axis.text.x = element_text(size = 24, face = "plain"),
          axis.title.x = element_text(size = 26, face = "plain"),
          axis.text.y = element_blank(),  
          axis.title.y = element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none")
)

###############
###############
# Same X Axis #

y_mid <- (max(temp.sofs$FM)+min(temp.sofs$FM))/2
x_mid <- (max(temp.sofs$FM_SD)+min(temp.sofs$FM_SD))/2

(ggplot(data = temp.sofs, aes(x = FM_SD, y = FM, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 10)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  + xlab("Std Dev")
  + xlim(0, 0.15)
  + ylab("F Measure")
  + scale_y_continuous(breaks=c(0.75, 0.8, 0.85, 0.9, 0.95, 1))
  # + scale_x_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 22),
          axis.text.y = element_text(size = 22, face = "plain"),
          axis.title.y = element_text(size = 24, face = "plain"),
          axis.text.x = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = c(.99, .99),
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          legend.text = element_text(size = 18)))

y_mid <- (max(temp.sofs$FNR)+min(temp.sofs$FNR))/2
x_mid <- (max(temp.sofs$FNR_SD)+min(temp.sofs$FNR_SD))/2

(ggplot(data = temp.sofs, aes(x = FNR_SD, y = FNR, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 10)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18))
  # + xlab("Std Dev")
  + ylab("FNR")
  + xlim(0, 0.15)
  # + scale_x_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)  
  + theme(plot.title = element_text(hjust=0.5, size = 22),
          axis.text.y = element_text(size = 22, face = "plain"),
          axis.title.y = element_text(size = 24, face = "plain"),
          axis.text.x = element_blank(),  
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none"))

y_mid <- (max(temp.sofs$FPR)+min(temp.sofs$FPR))/2
x_mid <- (max(temp.sofs$FPR_SD)+min(temp.sofs$FPR_SD))/2

(ggplot(data = temp.sofs, aes(x = FPR_SD, y = FPR, group = SF)) 
  + geom_point(aes(shape=SF, color = SF), size = 10)
  + scale_shape_manual(values = c(15, 19, 17, 8, 18)) 
  # + xlab("Std Dev")
  + ylab("FPR")
  + xlim(0,0.15)
  # + scale_x_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  + geom_vline(xintercept = x_mid, size = 1)
  + geom_hline(yintercept = y_mid, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 22),
          axis.text.y = element_text(size = 22, face = "plain"),
          axis.text.x = element_text(size = 22, face = "plain"),  
          axis.title.y = element_text(size = 24, face = "plain"),
          axis.title.x = element_text(size = 24, face = "plain"),
          legend.position = "none"))

################################
################################
###      Plotting Betas      ###
################################
################################

#F Measure
(ggplot(data = Combined.FM, aes(x = SF, y = FM_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("F Measure")
  + ggtitle("F Measure by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

#FNR
(ggplot(data = Combined.FNR, aes(x = SF, y = FNR_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("FNR")
  + ggtitle("FNR by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

#FPR
(ggplot(data = Combined.FPR, aes(x = SF, y = FPR_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("FPR")
  + ggtitle("FPR by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5)))

##### With Halfwidths
#F Measure
(ggplot(data = Combined.FM, aes(x = SF, y = FM_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("F Measure")
  + ggtitle("F Measure by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FM_SOFSI-FM_SOFSI_HW, ymax = FM_SOFSI+FM_SOFSI_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

#FNR
(ggplot(data = Combined.FNR, aes(x = SF, y = FNR_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("FNR")
  + ggtitle("FNR by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FNR_SOFSI-FNR_SOFSI_HW, ymax = FNR_SOFSI+FNR_SOFSI_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

#FPR
(ggplot(data = Combined.FPR, aes(x = SF, y = FPR_SOFSI, color = Beta, group = Beta)) 
  + geom_point(size = 2) 
  + geom_line(size = 1) 
  + ylab("FPR")
  + ggtitle("FPR by Beta \n (SOFS-I)")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin = FPR_SOFSI-FPR_SOFSI_HW, ymax = FPR_SOFSI+FPR_SOFSI_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))



###########################################################################
###########################################################################
################     Difference In Performance Metrics     ################
###########################################################################
###########################################################################

### Differences
# SOFSI - Crude LASSO
Diff.FM.SC.vals <- rownames_to_column(diff.SC.FM.df, var = "Beta")
Diff.FM.SC.vals <- Diff.FM.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FM")

Diff.FNR.SC.vals <- rownames_to_column(diff.SC.FNR.df, var = "Beta")
Diff.FNR.SC.vals <- Diff.FNR.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FNR")

Diff.FPR.SC.vals <- rownames_to_column(diff.SC.FPR.df, var = "Beta")
Diff.FPR.SC.vals <- Diff.FPR.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FPR")

# LASSOI - Crude LASSO
Diff.FM.LC.vals <- rownames_to_column(diff.LC.FM.df, var = "Beta")
Diff.FM.LC.vals <- Diff.FM.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FM")

Diff.FNR.LC.vals <- rownames_to_column(diff.LC.FNR.df, var = "Beta")
Diff.FNR.LC.vals <- Diff.FNR.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FNR")

Diff.FPR.LC.vals <- rownames_to_column(diff.LC.FPR.df, var = "Beta")
Diff.FPR.LC.vals <- Diff.FPR.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FPR")

# SOFSI - LASSOI
Diff.FM.SL.vals <- rownames_to_column(diff.SL.FM.df, var = "Beta")
Diff.FM.SL.vals <- Diff.FM.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FM")

Diff.FNR.SL.vals <- rownames_to_column(diff.SL.FNR.df, var = "Beta")
Diff.FNR.SL.vals <- Diff.FNR.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FNR")

Diff.FPR.SL.vals <- rownames_to_column(diff.SL.FPR.df, var = "Beta")
Diff.FPR.SL.vals <- Diff.FPR.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "FPR")

### Halfwidth of Differences
# SOFSI - Crude LASSO
Diff.FM.HW.SC.vals <- rownames_to_column(diff.SC.FM.HW.df, var = "Beta")
Diff.FM.HW.SC.vals <- Diff.FM.HW.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FNR.HW.SC.vals <- rownames_to_column(diff.SC.FNR.HW.df, var = "Beta")
Diff.FNR.HW.SC.vals <- Diff.FNR.HW.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FPR.HW.SC.vals <- rownames_to_column(diff.SC.FPR.HW.df, var = "Beta")
Diff.FPR.HW.SC.vals <- Diff.FPR.HW.SC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

# LASSOI - Crude LASSO
Diff.FM.HW.LC.vals <- rownames_to_column(diff.LC.FM.HW.df, var = "Beta")
Diff.FM.HW.LC.vals <- Diff.FM.HW.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FNR.HW.LC.vals <- rownames_to_column(diff.LC.FNR.HW.df, var = "Beta")
Diff.FNR.HW.LC.vals <- Diff.FNR.HW.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FPR.HW.LC.vals <- rownames_to_column(diff.LC.FPR.HW.df, var = "Beta")
Diff.FPR.HW.LC.vals <- Diff.FPR.HW.LC.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

# SOFSI - LASSOI
Diff.FM.HW.SL.vals <- rownames_to_column(diff.SL.FM.HW.df, var = "Beta")
Diff.FM.HW.SL.vals <- Diff.FM.HW.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FNR.HW.SL.vals <- rownames_to_column(diff.SL.FNR.HW.df, var = "Beta")
Diff.FNR.HW.SL.vals <- Diff.FNR.HW.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")

Diff.FPR.HW.SL.vals <- rownames_to_column(diff.SL.FPR.HW.df, var = "Beta")
Diff.FPR.HW.SL.vals <- Diff.FPR.HW.SL.vals %>% pivot_longer(cols = '10% (70)':'80% (180)', names_to = "SF", values_to = "Halfwidth")


Diff.SC <- cbind(Diff.FM.SC.vals[c(1,2,3)], Diff.FM.HW.SC.vals[3],
                 Diff.FNR.SC.vals[c(3)], Diff.FNR.HW.SC.vals[3],
                 Diff.FPR.SC.vals[c(3)], Diff.FPR.HW.SC.vals[3])

Diff.LC <- cbind(Diff.FM.LC.vals[c(1,2,3)], Diff.FM.HW.LC.vals[3],
                 Diff.FNR.LC.vals[c(3)], Diff.FNR.HW.LC.vals[3],
                 Diff.FPR.LC.vals[c(3)], Diff.FPR.HW.LC.vals[3])

Diff.SL <- cbind(Diff.FM.SL.vals[c(1,2,3)], Diff.FM.HW.SL.vals[3],
                 Diff.FNR.SL.vals[c(3)], Diff.FNR.HW.SL.vals[3],
                 Diff.FPR.SL.vals[c(3)], Diff.FPR.HW.SL.vals[3])

colnames(Diff.SC) <- c("Beta", "SF", "FM_Diff", "FM_Diff_HW", "FNR_Diff", "FNR_Diff_HW", "FPR_Diff", "FPR_Diff_HW")
colnames(Diff.LC) <- c("Beta", "SF", "FM_Diff", "FM_Diff_HW", "FNR_Diff", "FNR_Diff_HW", "FPR_Diff", "FPR_Diff_HW")
colnames(Diff.SL) <- c("Beta", "SF", "FM_Diff", "FM_Diff_HW", "FNR_Diff", "FNR_Diff_HW", "FPR_Diff", "FPR_Diff_HW")

########################
#   Plot Differences   #
# SOFS-I - Crude LASSO #
########################

###### Grouped by Beta
# Create a plot for F Measure
(ggplot(data = Diff.SC, aes(x = SF, y = FM_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  # + ggtitle("Performance Difference Between \n SOFS-I and Crude LASSO")
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_text(size = 27, face = "plain"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.position = "none")
  + geom_ribbon(aes(ymin =FM_Diff-FM_Diff_HW, ymax =FM_Diff+FM_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FNR
(ggplot(data = Diff.SC, aes(x = SF, y = FNR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("FNR Difference")
  + ylim(-0.5, 0.3)
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  + geom_hline(yintercept = 0, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_text(size = 27, face = "plain"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.position = c(.75, .5),
          legend.justification = c("right", "top"),
          legend.title = element_text(expression(beta), size = 26),
          legend.text = element_text(size = 26))
  + geom_ribbon(aes(ymin =FNR_Diff-FNR_Diff_HW, ymax =FNR_Diff+FNR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FPR
(ggplot(data = Diff.SC, aes(x = SF, y = FPR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("FPR Difference")
  + ylim(-0.5, 0.3)
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  + geom_hline(yintercept = 0, size = 1)
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.position = "none",)
  + geom_ribbon(aes(ymin =FPR_Diff-FPR_Diff_HW, ymax =FPR_Diff+FPR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))
  
########################
#   Plot Differences   #
# LASSO - Crude LASSO #
########################

###### Grouped by Beta
# Create a plot for F Measure
(ggplot(data = Diff.LC, aes(x = SF, y = FM_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ggtitle("F Measure Difference Between \n LASSO-I and Crude LASSO")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FM_Diff-FM_Diff_HW, ymax =FM_Diff+FM_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FNR
(ggplot(data = Diff.LC, aes(x = SF, y = FNR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ggtitle("FNR Difference Between \n LASSO-I and Crude LASSO")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FNR_Diff-FNR_Diff_HW, ymax =FNR_Diff+FNR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FPR
(ggplot(data = Diff.LC, aes(x = SF, y = FPR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ggtitle("FPR Difference Between \n LASSO-I and Crude LASSO")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FPR_Diff-FPR_Diff_HW, ymax =FPR_Diff+FPR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

#####################################################

########################
#   Plot Differences   #
# (SOFS-I) - (LASSO-I) #
########################

###### Grouped by Beta
# Create a plot for F Measure
(ggplot(data = Diff.SL, aes(x = SF, y = FM_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  + geom_hline(yintercept = 0, size = 1)
  # + ggtitle("F Measure Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_text(size = 27, face = "plain"),
          axis.title.y = element_text(size = 27, face = "plain"),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.position = "none")
  + geom_ribbon(aes(ymin =FM_Diff-FM_Diff_HW, ymax =FM_Diff+FM_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FNR
(ggplot(data = Diff.SL, aes(x = SF, y = FNR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  + geom_hline(yintercept = 0, size = 1)
  # + ggtitle("FNR Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.title = element_text(expression(beta), size = 34),
          legend.text = element_text(size = 34),
          legend.position = c(0.5,0.25))
  + geom_ribbon(aes(ymin =FNR_Diff-FNR_Diff_HW, ymax =FNR_Diff+FNR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

# Create a plot for FPR
(ggplot(data = Diff.SL, aes(x = SF, y = FPR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + scale_x_discrete(guide = guide_axis(n.dodge=2))
  + geom_hline(yintercept = 0, size = 1)
  # + ggtitle("FPR Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5, size = 27),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 27, face = "plain"),
          axis.title.x = element_blank(),
          legend.position = "none")
  + geom_ribbon(aes(ymin =FPR_Diff-FPR_Diff_HW, ymax =FPR_Diff+FPR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))


################################################################
################################################################
################################################################

# F Measure
temp.FM.b1 <- Combined.FM %>% filter(Beta == "1")
temp.FM.b2 <- Combined.FM %>% filter(Beta == "2")
temp.FM.b3 <- Combined.FM %>% filter(Beta == "3")
temp.FM.b4 <- Combined.FM %>% filter(Beta == "4")

temp.diff.FM.b12 <- temp.FM.b1$FM_SOFSI - temp.FM.b2$FM_SOFSI

# FNR
temp.FNR.b1 <- Combined.FNR %>% filter(Beta == "1")
temp.FNR.b2 <- Combined.FNR %>% filter(Beta == "2")
temp.FNR.b3 <- Combined.FNR %>% filter(Beta == "3")
temp.FNR.b4 <- Combined.FNR %>% filter(Beta == "4")

# FPR
temp.FPR.b1 <- Combined.FPR %>% filter(Beta == "1")
temp.FPR.b2 <- Combined.FPR %>% filter(Beta == "2")
temp.FPR.b3 <- Combined.FPR %>% filter(Beta == "3")
temp.FPR.b4 <- Combined.FPR %>% filter(Beta == "4")

######################################################################
### Determining the Most Reliable Beta Value for 70% and 80% cases ###
######################################################################

####################################
###   Plotting F Measure w/ HW   ###
####################################

temp.combined.FM <- Combined.FM %>% filter(SF == "70% (110)" | SF == "80% (180)")
temp.combined.FNR <- Combined.FNR %>% filter(SF == "70% (110)" | SF == "80% (180)")
temp.combined.FPR <- Combined.FPR %>% filter(SF == "70% (110)" | SF == "80% (180)")

# SOFS-I
(ggplot(data = temp.combined.FM, aes(x = SF, y = FM_SOFSI, color = Beta, group = Beta)) 
 + geom_point() 
 + geom_line() 
 + ylab("F Measure")
 + ggtitle("F Measure \n (SOFS-I)")
 + theme(plot.title = element_text(hjust=0.5))
 + geom_ribbon(aes(ymin = FM_SOFSI-FM_SOFSI_HW, ymax = FM_SOFSI+FM_SOFSI_HW, fill = Beta), alpha = 0.2)
 + geom_line(aes(group = Beta)))


####################################
###      Plotting FNR w/ HWs     ###
####################################

# SOFS-I
(ggplot(data = temp.combined.FNR, aes(x = SF, y = FNR_SOFSI, color = Beta, group = Beta)) 
 + geom_point() 
 + geom_line() 
 + ylab("FNR")
 + ggtitle("FNR \n (SOFS-I)")
 + theme(plot.title = element_text(hjust=0.5))
 + geom_ribbon(aes(ymin = FNR_SOFSI-FNR_SOFSI_HW, ymax = FNR_SOFSI+FNR_SOFSI_HW, fill = Beta), alpha = 0.2)
 + geom_line(aes(group = Beta)))


####################################
###      Plotting FPR w/ HWs     ###
####################################

# SOFS-I
(ggplot(data = temp.combined.FPR, aes(x = SF, y = FPR_SOFSI, color = Beta, group = Beta)) 
 + geom_point() 
 + geom_line() 
 + ylab("FPR")
 + ggtitle("FPR \n (SOFS-I)")
 + theme(plot.title = element_text(hjust=0.5))
 + geom_ribbon(aes(ymin = FPR_SOFSI-FPR_SOFSI_HW, ymax = FPR_SOFSI+FPR_SOFSI_HW, fill = Beta), alpha = 0.2)
 + geom_line(aes(group = Beta)))


######################################################
###      Plotting F Measure Differences w/ HWs     ###
######################################################

temp.diff.SL <- Diff.SL %>% filter(SF == "70% (110)" | SF == "80% (180)")

# Create a plot for F Measure
(ggplot(data = temp.diff.SL, aes(x = SF, y = FM_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + geom_hline(yintercept = 0, size = 1)
  + ggtitle("F Measure Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FM_Diff-FM_Diff_HW, ymax =FM_Diff+FM_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

################################################
###      Plotting FNR Differences w/ HWs     ###
################################################

# Create a plot for FNR
(ggplot(data = temp.diff.SL, aes(x = SF, y = FNR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + geom_hline(yintercept = 0, size = 1)
  + ggtitle("FNR Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FNR_Diff-FNR_Diff_HW, ymax =FNR_Diff+FNR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

################################################
###      Plotting FPR Differences w/ HWs     ###
################################################

# Create a plot for FPR
(ggplot(data = temp.diff.SL, aes(x = SF, y = FPR_Diff, color = Beta, group = Beta))
  + geom_point()
  + geom_line()
  + ylab("Difference")
  + ylim(c(-0.32, 0.25))
  + geom_hline(yintercept = 0, size = 1)
  + ggtitle("FPR Difference Between \n SOFS-I and LASSO-I")
  + theme(plot.title = element_text(hjust=0.5))
  + geom_ribbon(aes(ymin =FPR_Diff-FPR_Diff_HW, ymax =FPR_Diff+FPR_Diff_HW, fill = Beta), alpha = 0.2)
  + geom_line(aes(group = Beta)))

Stop.Time <- Sys.time()
Total.P2.Time <- difftime(Stop.Time, Start.Time)

# Save the Workspace
save.image(file='G:/My Drive/SOFS-I/FinalExperimentation/ExperimentalRuns_Code/Results/Phase2_Results.RData')

