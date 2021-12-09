#########################################
#########################################
############### SET UP ################## 
#########################################
#########################################

#Load all necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(modeest)
library(png)
library(MASS)
library(leaps)

Start.Time <- Sys.time()

Build629 <- read_csv("6-29 Build 2 All Data.txt", col_names = FALSE, col_types = cols(X1 = col_character()))
Build703 <- read_csv("7-3 Build 3 All Data.txt",  col_names = FALSE, col_types = cols(X1 = col_character()))
Build710 <- read_csv("7-10 Build 4 All Data.txt", col_names = FALSE, col_types = cols(X1 = col_character()))
Build718 <- read_csv("7-18 Build 4 All Data.txt", col_names = FALSE, col_types = cols(X1 = col_character()))
Build723 <- read_csv("7-23 Build 5 All Data.txt", col_names = FALSE, col_types = cols(X1 = col_character()))

####################################################################################################################

##################
#### Build629 ####
##################

#Separate date and time from each other
Build629 <- separate(Build629, X1, into = c("Date", "Time"), sep = "\\s")
head(Build629)

#Delete all NA entries and CORE Actions
Build629 <- na.omit(Build629)
Build629 <- filter(Build629, X3!="Core")
Build629 <- Build629[order(Build629$Time),]
head(Build629)

#Subset dataframe to obtain HEIGHT and attach LAYER using 
#Subset dataframe for Height factor
Defects629 <- subset(Build629 [c(2,3,6)], X5=="LowerTable")

#Add layer number based on the lower table action
Defects629$Layer <- seq.int(nrow(Defects629))

#Create table for height
Height629 <- subset(Build629 [c(2,3,6)], X2=="Builds.State.CurrentBuild.CurrentHeight")
Height629$X5 <- as.numeric(as.character(Height629$X5))

#Assign height for each layer
Defects629Time <- Defects629$Time
Height629Time <- Height629$Time
Height629Position <- Height629$X5
Defects629Height <- rep(0, length(Defects629))

for(i in 1:length(Defects629Time)) {
  for(j in 1:length(Height629Time)) {
    if (j == 1) { 
      if (Defects629Time[i] < Height629Time[j]) { Defects629Height[i]=Height629Position[j]-.05 }
      else if (Defects629Time[i] <= Height629Time[j+1]) { Defects629Height[i]=Height629Position[j]}}
    else if (j == length(Height629Time)) { 
      if (Defects629Time[i] > Height629Time[j]) { Defects629Height[i]=Height629Position[length(Height629Position)] }}
    else {
      if (Defects629Time[i] >= Height629Time[j] && Defects629Time[i] < Height629Time[j+1]) {
        Defects629Height[i] = Height629Position[j]}
    }
  }
}

Defects629$Height <- Defects629Height

#To change the order as in the above question do df2[,c(1,3,2,4)]
Defects629 <- Defects629[,c(1,2,3,5,4)]
head(Defects629)

#Create a table for OutputDescriptions
#Subset the 3 cateogires which contain process state information
OutputDescription629 <- subset(Build629 [c(2,3,6)], X2=="Process.ProcessManager.Action" | X2=="Process.ProcessManager.CurrentTask" | X2=="Builds.State.CurrentBuild.OutputDescription")

#Add layer to categorize
Output629Time <- OutputDescription629$Time
Output629Layer <- rep(0, length(Output629Time))
Defects629Layer <- Defects629$Layer

for(i in 1:length(Output629Time)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Output629Time[i] < Defects629Time[j]) { Output629Layer[i]=Defects629Layer[j]-1 }
      else if (Output629Time[i] <= Defects629Time[j+1]) { Output629Layer[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Output629Time[i] > Defects629Time[j]) { Output629Layer[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Output629Time[i] >= Defects629Time[j] && Output629Time[i] < Defects629Time[j+1]) {
        Output629Layer[i] = Defects629Layer[j]}
    }
  }
}

OutputDescription629$Layer <- Output629Layer
head(OutputDescription629)

#Only look at layers >0
OutputDescription629 <- subset(OutputDescription629, Layer != 0)
colnames(OutputDescription629) <- list("Time", "Variable", "Process", "Layer") 

#Look for BuildStopped or BuildCrashed to identify when a "defect" may have occured
head(filter(OutputDescription629, Process=="BuildStopped" | Process=="BuildCrashed"))

#############################
### LAYERS ARE: 20 AND 51 ###
#############################

## Identify when the support ends
Support629 <- filter(OutputDescription629, Process == "[2]: Wafer Support")
tail(Support629)

###############################
### Last Support Layer = 85 ###
###############################

CheckRakes <- as.data.frame(unique(Build629$X2))

CheckRakes9 <- subset(Build629 [c(2,3,6)], X2=="Process.RakeControl.MoveToOtherSideAgain")

#### Move Rake ####
#See how often a Rake Process occurred
MoveRake629 <- subset(Build629 [c(2,3,6)], X2=="OPC.Rake.MoveDone")

#Add layer to categorize
Rake629Time <- MoveRake629$Time
Rake629Layer <- rep(0, length(Rake629Time))
Defects629Time <- Defects629$Time
Defects629Layer <- Defects629$Layer

for(i in 1:length(Rake629Time)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Rake629Time[i] < Defects629Time[j]) { Rake629Layer[i]=Defects629Layer[j]-1 }
      else if (Rake629Time[i] <= Defects629Time[j+1]) { Rake629Layer[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Rake629Time[i] > Defects629Time[j]) { Rake629Layer[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Rake629Time[i] >= Defects629Time[j] && Rake629Time[i] < Defects629Time[j+1]) {
        Rake629Layer[i] = Defects629Layer[j]}
    }
  }
}

MoveRake629$Layer <- Rake629Layer
colnames(MoveRake629) <- list("Time", "Variable", "Raking", "Layer")
head(MoveRake629)

#Only look at layers > 0
MoveRake629 <- subset(MoveRake629, Layer!="0")
head(MoveRake629)

for(i in 1:length(MoveRake629$Raking)) {
  if(MoveRake629$Raking[i] == "True") {MoveRake629$Value[i]=1}
  else {MoveRake629$Value[i]=0}
}

TrueRake629 <- filter(MoveRake629, Raking == "True")
NumLayers <- Defects629$Layer
N <- rep(0, length(Defects629Layer))

for(i in 1:length(NumLayers)) {
  filteredLayers <- filter(TrueRake629, Layer == NumLayers[i])
  N[i] = length(filteredLayers$Layer)
}

RakingPerLayer629 <- cbind.data.frame(NumLayers, N)
colnames(RakingPerLayer629) <- list("Layer", "Rakes")
RakingPerLayer629 <- filter(RakingPerLayer629, Layer > 85 & Layer < max(TrueRake629$Layer))

#Calculate how long into the layer it was before each Rake occurred
TrueRake629 <- filter(TrueRake629, Layer > 85 & Layer < max(TrueRake629$Layer))
TrueRake629$Value <- as.numeric(TrueRake629$Value)
Groups629 <- ceiling(length(unique(TrueRake629$Layer))/10)

TrueRake629$Time <- as.POSIXct(TrueRake629$Time,format="%H:%M:%OS")
Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")

for(i in 1:length(TrueRake629$Layer)) {
  for(j in 1:length(Defects629$Layer)) {
    if(TrueRake629$Layer[i]==j) {TrueRake629$TimeOfRake[i] = TrueRake629$Time[i]-Defects629$Time[j]}
  }
}

####################################################################################################################

##################
#### Build703 ####
##################

#Separate date and time from each other
Build703 <- separate(Build703, X1, into = c("Date", "Time"), sep = "\\s")
head(Build703)

#Delete all NA entries and CORE Actions
Build703 <- na.omit(Build703)
Build703 <- filter(Build703, X3!="Core")
Build703<- subset(Build703, Date=="2018-07-05")
Build703 <- Build703[order(Build703$Time),]
head(Build703)

#Subset dataframe to obtain HEIGHT and attach LAYER using 
#Subset dataframe for Height factor
Defects703 <- subset(Build703 [c(2,3,6)], X5=="LowerTable")

#Add layer number based on the lower table action
Defects703$Layer <- seq.int(nrow(Defects703))

#Create table for height
Height703 <- subset(Build703 [c(2,3,6)], X2=="Builds.State.CurrentBuild.CurrentHeight")
Height703$X5 <- as.numeric(as.character(Height703$X5))

#Assign height for each layer
Defects703Time <- Defects703$Time
Height703Time <- Height703$Time
Height703Position <- Height703$X5
Defects703Height <- rep(0, length(Defects703))

for(i in 1:length(Defects703Time)) {
  for(j in 1:length(Height703Time)) {
    if (j == 1) { 
      if (Defects703Time[i] < Height703Time[j]) { Defects703Height[i]=Height703Position[j]-.05 }
      else if (Defects703Time[i] <= Height703Time[j+1]) { Defects703Height[i]=Height703Position[j]}}
    else if (j == length(Height703Time)) { 
      if (Defects703Time[i] > Height703Time[j]) { Defects703Height[i]=Height703Position[length(Height703Position)] }}
    else {
      if (Defects703Time[i] >= Height703Time[j] && Defects703Time[i] < Height703Time[j+1]) {
        Defects703Height[i] = Height703Position[j]}
    }
  }
}

Defects703$Height <- Defects703Height

#To change the order as in the above question do df2[,c(1,3,2,4)]
Defects703 <- Defects703[,c(1,2,3,5,4)]
head(Defects703)

#Create a table for OutputDescriptions
#Subset the 3 cateogires which contain process state information
OutputDescription703 <- subset(Build703 [c(2,3,6)], X2=="Process.ProcessManager.Action" | X2=="Process.ProcessManager.CurrentTask" | X2=="Builds.State.CurrentBuild.OutputDescription")

#Add layer to categorize
Output703Time <- OutputDescription703$Time
Output703Layer <- rep(0, length(Output703Time))
Defects703Layer <- Defects703$Layer

for(i in 1:length(Output703Time)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Output703Time[i] < Defects703Time[j]) { Output703Layer[i]=Defects703Layer[j]-1 }
      else if (Output703Time[i] <= Defects703Time[j+1]) { Output703Layer[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Output703Time[i] > Defects703Time[j]) { Output703Layer[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Output703Time[i] >= Defects703Time[j] && Output703Time[i] < Defects703Time[j+1]) {
        Output703Layer[i] = Defects703Layer[j]}
    }
  }
}

OutputDescription703$Layer <- Output703Layer
head(OutputDescription703)

#Only look at layers >0
OutputDescription703 <- subset(OutputDescription703, Layer != 0)
colnames(OutputDescription703) <- list("Time", "Variable", "Process", "Layer") 

#Look for BuildStopped or BuildCrashed to identify when a "defect" may have occured
head(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed"))

##############################
##### Layers 223 and 231 #####
##############################

## Identify when the support ends
Support703 <- filter(OutputDescription703, Process == "[2]: Wafer Support")
tail(Support703)

###############################
### Last Support Layer = 85 ###
###############################

#### Move Rake ####
#See how often a Rake Process occurred
MoveRake703 <- subset(Build703 [c(2,3,6)], X2=="OPC.Rake.MoveDone")

#Add layer to categorize
Rake703Time <- MoveRake703$Time
Rake703Layer <- rep(0, length(Rake703Time))
Defects703Time <- Defects703$Time
Defects703Layer <- Defects703$Layer

for(i in 1:length(Rake703Time)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Rake703Time[i] < Defects703Time[j]) { Rake703Layer[i]=Defects703Layer[j]-1 }
      else if (Rake703Time[i] <= Defects703Time[j+1]) { Rake703Layer[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Rake703Time[i] > Defects703Time[j]) { Rake703Layer[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Rake703Time[i] >= Defects703Time[j] && Rake703Time[i] < Defects703Time[j+1]) {
        Rake703Layer[i] = Defects703Layer[j]}
    }
  }
}

MoveRake703$Layer <- Rake703Layer
colnames(MoveRake703) <- list("Time", "Variable", "Raking", "Layer")
head(MoveRake703)

#Only look at layers > 0
MoveRake703 <- subset(MoveRake703, Layer!="0")
head(MoveRake703)

for(i in 1:length(MoveRake703$Raking)) {
  if(MoveRake703$Raking[i] == "True") {MoveRake703$Value[i]=1}
  else {MoveRake703$Value[i]=0}
}

TrueRake703 <- filter(MoveRake703, Raking == "True")
NumLayers <- Defects703$Layer
N <- rep(0, length(Defects703Layer))

for(i in 1:length(NumLayers)) {
  filteredLayers <- filter(TrueRake703, Layer == NumLayers[i])
  N[i] = length(filteredLayers$Layer)
}

RakingPerLayer703 <- cbind.data.frame(NumLayers, N)
colnames(RakingPerLayer703) <- list("Layer", "Rakes")
RakingPerLayer703 <- filter(RakingPerLayer703, Layer > 85 & Layer <= max(TrueRake703$Layer))

#Calculate how long into the layer it was before each Rake occurred
TrueRake703 <- filter(TrueRake703, Layer > 85 & Layer <= max(TrueRake703$Layer))
TrueRake703$Value <- as.numeric(TrueRake703$Value)
Groups703 <- ceiling(length(unique(TrueRake703$Layer))/10)

TrueRake703$Time <- as.POSIXct(TrueRake703$Time,format="%H:%M:%OS")
Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")

for(i in 1:length(TrueRake703$Layer)) {
  for(j in 1:length(Defects703$Layer)) {
    if(TrueRake703$Layer[i]==j) {TrueRake703$TimeOfRake[i] = TrueRake703$Time[i]-Defects703$Time[j]}
  }
}


####################################################################################################################

##################
#### Build710 ####
##################

#Separate date and time from each other
Build710 <- separate(Build710, X1, into = c("Date", "Time"), sep = "\\s")
head(Build710)

#Delete all NA entries and CORE Actions
Build710 <- na.omit(Build710)
Build710 <- filter(Build710, X3!="Core")
Build710 <- Build710[order(Build710$Time),]
head(Build710)

#Subset dataframe to obtain HEIGHT and attach LAYER using 
#Subset dataframe for Height factor
Defects710 <- subset(Build710 [c(2,3,6)], X5=="LowerTable")

#Add layer number based on the lower table action
Defects710$Layer <- seq.int(nrow(Defects710))

#Create table for height
Height710 <- subset(Build710 [c(2,3,6)], X2=="Builds.State.CurrentBuild.CurrentHeight")
Height710$X5 <- as.numeric(as.character(Height710$X5))

#Assign height for each layer
Defects710Time <- Defects710$Time
Height710Time <- Height710$Time
Height710Position <- Height710$X5
Defects710Height <- rep(0, length(Defects710))

for(i in 1:length(Defects710Time)) {
  for(j in 1:length(Height710Time)) {
    if (j == 1) { 
      if (Defects710Time[i] < Height710Time[j]) { Defects710Height[i]=Height710Position[j]-.05 }
      else if (Defects710Time[i] <= Height710Time[j+1]) { Defects710Height[i]=Height710Position[j]}}
    else if (j == length(Height710Time)) { 
      if (Defects710Time[i] > Height710Time[j]) { Defects710Height[i]=Height710Position[length(Height710Position)] }}
    else {
      if (Defects710Time[i] >= Height710Time[j] && Defects710Time[i] < Height710Time[j+1]) {
        Defects710Height[i] = Height710Position[j]}
    }
  }
}

Defects710$Height <- Defects710Height

#To change the order as in the above question do df2[,c(1,3,2,4)]
Defects710 <- Defects710[,c(1,2,3,5,4)]
head(Defects710)

#Create a table for OutputDescriptions
#Subset the 3 cateogires which contain process state information
OutputDescription710 <- subset(Build710 [c(2,3,6)], X2=="Process.ProcessManager.Action" | X2=="Process.ProcessManager.CurrentTask" | X2=="Builds.State.CurrentBuild.OutputDescription")

#Add layer to categorize
Output710Time <- OutputDescription710$Time
Output710Layer <- rep(0, length(Output710Time))
Defects710Layer <- Defects710$Layer

for(i in 1:length(Output710Time)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Output710Time[i] < Defects710Time[j]) { Output710Layer[i]=Defects710Layer[j]-1 }
      else if (Output710Time[i] <= Defects710Time[j+1]) { Output710Layer[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Output710Time[i] > Defects710Time[j]) { Output710Layer[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Output710Time[i] >= Defects710Time[j] && Output710Time[i] < Defects710Time[j+1]) {
        Output710Layer[i] = Defects710Layer[j]}
    }
  }
}

OutputDescription710$Layer <- Output710Layer
head(OutputDescription710)

#Only look at layers >0
OutputDescription710 <- subset(OutputDescription710, Layer != 0)
colnames(OutputDescription710) <- list("Time", "Variable", "Process", "Layer") 

#Look for BuildStopped or BuildCrashed to identify when a "defect" may have occured
head(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed"))

##################################################
############# According to Output: 254 ###########
####### According to Carter's Notes: 10,14 #######
#### Defect Layers to Observe: 10,14, and 254 ####
##################################################

## Identify when the support ends
Support710 <- filter(OutputDescription710, Process == "[2]: Wafer Support")
tail(Support710)

###############################
### Last Support Layer = 85 ###
###############################

#### Move Rake ####
#See how often a Rake Process occurred
MoveRake710 <- subset(Build710 [c(2,3,6)], X2=="OPC.Rake.MoveDone")

#Add layer to categorize
Rake710Time <- MoveRake710$Time
Rake710Layer <- rep(0, length(Rake710Time))
Defects710Time <- Defects710$Time
Defects710Layer <- Defects710$Layer

for(i in 1:length(Rake710Time)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Rake710Time[i] < Defects710Time[j]) { Rake710Layer[i]=Defects710Layer[j]-1 }
      else if (Rake710Time[i] <= Defects710Time[j+1]) { Rake710Layer[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Rake710Time[i] > Defects710Time[j]) { Rake710Layer[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Rake710Time[i] >= Defects710Time[j] && Rake710Time[i] < Defects710Time[j+1]) {
        Rake710Layer[i] = Defects710Layer[j]}
    }
  }
}

MoveRake710$Layer <- Rake710Layer
colnames(MoveRake710) <- list("Time", "Variable", "Raking", "Layer")
head(MoveRake710)

#Only look at layers > 0
MoveRake710 <- subset(MoveRake710, Layer!="0")
head(MoveRake710)

for(i in 1:length(MoveRake710$Raking)) {
  if(MoveRake710$Raking[i] == "True") {MoveRake710$Value[i]=1}
  else {MoveRake710$Value[i]=0}
}

TrueRake710 <- filter(MoveRake710, Raking == "True")
NumLayers <- Defects710$Layer
N <- rep(0, length(Defects710Layer))

for(i in 1:length(NumLayers)) {
  filteredLayers <- filter(TrueRake710, Layer == NumLayers[i])
  N[i] = length(filteredLayers$Layer)
}

RakingPerLayer710 <- cbind.data.frame(NumLayers, N)
colnames(RakingPerLayer710) <- list("Layer", "Rakes")
RakingPerLayer710 <- filter(RakingPerLayer710, Layer > 85 & Layer <= max(TrueRake710$Layer))

#Calculate how long into the layer it was before each Rake occurred
TrueRake710 <- filter(TrueRake710, Layer > 85 & Layer <= max(TrueRake710$Layer))
TrueRake710$Value <- as.numeric(TrueRake710$Value)
Groups710 <- ceiling(length(unique(TrueRake710$Layer))/10)

TrueRake710$Time <- as.POSIXct(TrueRake710$Time,format="%H:%M:%OS")
Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")

for(i in 1:length(TrueRake710$Layer)) {
  for(j in 1:length(Defects710$Layer)) {
    if(TrueRake710$Layer[i]==j) {TrueRake710$TimeOfRake[i] = TrueRake710$Time[i]-Defects710$Time[j]}
  }
}


####################################################################################################################

##################
#### Build718 ####
##################

#Separate date and time from each other
Build718 <- separate(Build718, X1, into = c("Date", "Time"), sep = "\\s")
head(Build718)

#Delete all NA entries and CORE Actions
Build718 <- na.omit(Build718)
Build718 <- filter(Build718, X3!="Core")
Build718 <- Build718[order(Build718$Time),]
head(Build718)

#Subset dataframe to obtain HEIGHT and attach LAYER using 
#Subset dataframe for Height factor
Defects718 <- subset(Build718 [c(2,3,6)], X5=="LowerTable")

#Add layer number based on the lower table action
Defects718$Layer <- seq.int(nrow(Defects718))

#Create table for height
Height718 <- subset(Build718 [c(2,3,6)], X2=="Builds.State.CurrentBuild.CurrentHeight")
Height718$X5 <- as.numeric(as.character(Height718$X5))

#Assign height for each layer
Defects718Time <- Defects718$Time
Height718Time <- Height718$Time
Height718Position <- Height718$X5
Defects718Height <- rep(0, length(Defects718))

for(i in 1:length(Defects718Time)) {
  for(j in 1:length(Height718Time)) {
    if (j == 1) { 
      if (Defects718Time[i] < Height718Time[j]) { Defects718Height[i]=Height718Position[j]-.05 }
      else if (Defects718Time[i] <= Height718Time[j+1]) { Defects718Height[i]=Height718Position[j]}}
    else if (j == length(Height718Time)) { 
      if (Defects718Time[i] > Height718Time[j]) { Defects718Height[i]=Height718Position[length(Height718Position)] }}
    else {
      if (Defects718Time[i] >= Height718Time[j] && Defects718Time[i] < Height718Time[j+1]) {
        Defects718Height[i] = Height718Position[j]}
    }
  }
}

Defects718$Height <- Defects718Height

#To change the order as in the above question do df2[,c(1,3,2,4)]
Defects718 <- Defects718[,c(1,2,3,5,4)]
head(Defects718)

#Create a table for OutputDescriptions
#Subset the 3 cateogires which contain process state information
OutputDescription718 <- subset(Build718 [c(2,3,6)], X2=="Process.ProcessManager.Action" | X2=="Process.ProcessManager.CurrentTask" | X2=="Builds.State.CurrentBuild.OutputDescription")

#Add layer to categorize
Output718Time <- OutputDescription718$Time
Output718Layer <- rep(0, length(Output718Time))
Defects718Layer <- Defects718$Layer

for(i in 1:length(Output718Time)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Output718Time[i] < Defects718Time[j]) { Output718Layer[i]=Defects718Layer[j]-1 }
      else if (Output718Time[i] <= Defects718Time[j+1]) { Output718Layer[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Output718Time[i] > Defects718Time[j]) { Output718Layer[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Output718Time[i] >= Defects718Time[j] && Output718Time[i] < Defects718Time[j+1]) {
        Output718Layer[i] = Defects718Layer[j]}
    }
  }
}

OutputDescription718$Layer <- Output718Layer
head(OutputDescription718)

#Only look at layers >0
OutputDescription718 <- subset(OutputDescription718, Layer != 0)
colnames(OutputDescription718) <- list("Time", "Variable", "Process", "Layer") 

#Look for BuildStopped or BuildCrashed to identify when a "defect" may have occured
head(filter(OutputDescription718, Process=="BuildStopped" | Process=="BuildCrashed"))

##############################
###### Successful Build ######
##############################

## Identify when the support ends
Support718 <- filter(OutputDescription718, Process == "[2]: Wafer Support")
tail(Support718)

###############################
### Last Support Layer = 85 ###
###############################

#### Move Rake ####
#See how often a Rake Process occurred
MoveRake718 <- subset(Build718 [c(2,3,6)], X2=="OPC.Rake.MoveDone")

#Add layer to categorize
Rake718Time <- MoveRake718$Time
Rake718Layer <- rep(0, length(Rake718Time))
Defects718Time <- Defects718$Time
Defects718Layer <- Defects718$Layer

for(i in 1:length(Rake718Time)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Rake718Time[i] < Defects718Time[j]) { Rake718Layer[i]=Defects718Layer[j]-1 }
      else if (Rake718Time[i] <= Defects718Time[j+1]) { Rake718Layer[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Rake718Time[i] > Defects718Time[j]) { Rake718Layer[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Rake718Time[i] >= Defects718Time[j] && Rake718Time[i] < Defects718Time[j+1]) {
        Rake718Layer[i] = Defects718Layer[j]}
    }
  }
}

MoveRake718$Layer <- Rake718Layer
colnames(MoveRake718) <- list("Time", "Variable", "Raking", "Layer")
head(MoveRake718)

#Only look at layers > 0
MoveRake718 <- subset(MoveRake718, Layer!="0")
head(MoveRake718)

for(i in 1:length(MoveRake718$Raking)) {
  if(MoveRake718$Raking[i] == "True") {MoveRake718$Value[i]=1}
  else {MoveRake718$Value[i]=0}
}

TrueRake718 <- filter(MoveRake718, Raking == "True")
NumLayers <- Defects718$Layer
N <- rep(0, length(Defects718Layer))

for(i in 1:length(NumLayers)) {
  filteredLayers <- filter(TrueRake718, Layer == NumLayers[i])
  N[i] = length(filteredLayers$Layer)
}

RakingPerLayer718 <- cbind.data.frame(NumLayers, N)
colnames(RakingPerLayer718) <- list("Layer", "Rakes")
RakingPerLayer718 <- filter(RakingPerLayer718, Layer > 85 & Layer < max(TrueRake718$Layer))

#Calculate how long into the layer it was before each Rake occurred
TrueRake718 <- filter(TrueRake718, Layer > 85 & Layer < max(TrueRake718$Layer))
TrueRake718$Value <- as.numeric(TrueRake718$Value)
Groups718 <- ceiling(length(unique(TrueRake718$Layer))/10)

TrueRake718$Time <- as.POSIXct(TrueRake718$Time,format="%H:%M:%OS")
Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")

for(i in 1:length(TrueRake718$Layer)) {
  for(j in 1:length(Defects718$Layer)) {
    if(TrueRake718$Layer[i]==j) {TrueRake718$TimeOfRake[i] = TrueRake718$Time[i]-Defects718$Time[j]}
  }
}


####################################################################################################################

##################
#### Build723 ####
##################

#Separate date and time from each other
Build723 <- separate(Build723, X1, into = c("Date", "Time"), sep = "\\s")
head(Build723)

#Delete all NA entries and CORE Actions
Build723 <- na.omit(Build723)
Build723 <- filter(Build723, X3!="Core")
Build723 <- Build723[order(Build723$Time),]
head(Build723)

#Subset dataframe to obtain HEIGHT and attach LAYER using 
#Subset dataframe for Height factor
Defects723 <- subset(Build723 [c(2,3,6)], X5=="LowerTable")

#Add layer number based on the lower table action
Defects723$Layer <- seq.int(nrow(Defects723))

#Create table for height
Height723 <- subset(Build723 [c(2,3,6)], X2=="Builds.State.CurrentBuild.CurrentHeight")
Height723$X5 <- as.numeric(as.character(Height723$X5))

#Assign height for each layer
Defects723Time <- Defects723$Time
Height723Time <- Height723$Time
Height723Position <- Height723$X5
Defects723Height <- rep(0, length(Defects723))

for(i in 1:length(Defects723Time)) {
  for(j in 1:length(Height723Time)) {
    if (j == 1) { 
      if (Defects723Time[i] < Height723Time[j]) { Defects723Height[i]=Height723Position[j]-.05 }
      else if (Defects723Time[i] <= Height723Time[j+1]) { Defects723Height[i]=Height723Position[j]}}
    else if (j == length(Height723Time)) { 
      if (Defects723Time[i] > Height723Time[j]) { Defects723Height[i]=Height723Position[length(Height723Position)] }}
    else {
      if (Defects723Time[i] >= Height723Time[j] && Defects723Time[i] < Height723Time[j+1]) {
        Defects723Height[i] = Height723Position[j]}
    }
  }
}

Defects723$Height <- Defects723Height

#To change the order as in the above question do df2[,c(1,3,2,4)]
Defects723 <- Defects723[,c(1,2,3,5,4)]
head(Defects723)

#Create a table for OutputDescriptions
#Subset the 3 cateogires which contain process state information
OutputDescription723 <- subset(Build723 [c(2,3,6)], X2=="Process.ProcessManager.Action" | X2=="Process.ProcessManager.CurrentTask" | X2=="Builds.State.CurrentBuild.OutputDescription")

#Add layer to categorize
Output723Time <- OutputDescription723$Time
Output723Layer <- rep(0, length(Output723Time))
Defects723Layer <- Defects723$Layer

for(i in 1:length(Output723Time)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Output723Time[i] < Defects723Time[j]) { Output723Layer[i]=Defects723Layer[j]-1 }
      else if (Output723Time[i] <= Defects723Time[j+1]) { Output723Layer[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Output723Time[i] > Defects723Time[j]) { Output723Layer[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Output723Time[i] >= Defects723Time[j] && Output723Time[i] < Defects723Time[j+1]) {
        Output723Layer[i] = Defects723Layer[j]}
    }
  }
}

OutputDescription723$Layer <- Output723Layer
head(OutputDescription723)

#Only look at layers >0
OutputDescription723 <- subset(OutputDescription723, Layer != 0)
colnames(OutputDescription723) <- list("Time", "Variable", "Process", "Layer") 

#Look for BuildStopped or BuildCrashed to identify when a "defect" may have occured
head(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed"))

######################################
#### LAYERS ARE: 1, 2, 5, and 104 ####
######################################

## Identify when the support ends
Support723 <- filter(OutputDescription723, Process == "[2]: Wafer Support")
tail(Support723)

###############################
### Last Support Layer = 85 ###
###############################
#### Move Rake ####
#See how often a Rake Process occurred
MoveRake723 <- subset(Build723 [c(2,3,6)], X2=="OPC.Rake.MoveDone")

#Add layer to categorize
Rake723Time <- MoveRake723$Time
Rake723Layer <- rep(0, length(Rake723Time))
Defects723Time <- Defects723$Time
Defects723Layer <- Defects723$Layer

for(i in 1:length(Rake723Time)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Rake723Time[i] < Defects723Time[j]) { Rake723Layer[i]=Defects723Layer[j]-1 }
      else if (Rake723Time[i] <= Defects723Time[j+1]) { Rake723Layer[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Rake723Time[i] > Defects723Time[j]) { Rake723Layer[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Rake723Time[i] >= Defects723Time[j] && Rake723Time[i] < Defects723Time[j+1]) {
        Rake723Layer[i] = Defects723Layer[j]}
    }
  }
}

MoveRake723$Layer <- Rake723Layer
colnames(MoveRake723) <- list("Time", "Variable", "Raking", "Layer")
head(MoveRake723)

#Only look at layers > 0
MoveRake723 <- subset(MoveRake723, Layer!="0")
head(MoveRake723)

for(i in 1:length(MoveRake723$Raking)) {
  if(MoveRake723$Raking[i] == "True") {MoveRake723$Value[i]=1}
  else {MoveRake723$Value[i]=0}
}

TrueRake723 <- filter(MoveRake723, Raking == "True")
NumLayers <- Defects723$Layer
N <- rep(0, length(Defects723Layer))

for(i in 1:length(NumLayers)) {
  filteredLayers <- filter(TrueRake723, Layer == NumLayers[i])
  N[i] = length(filteredLayers$Layer)
}

RakingPerLayer723 <- cbind.data.frame(NumLayers, N)
colnames(RakingPerLayer723) <- list("Layer", "Rakes")
RakingPerLayer723 <- filter(RakingPerLayer723, Layer > 85 & Layer <= max(TrueRake723$Layer))

#Calculate how long into the layer it was before each Rake occurred
TrueRake723 <- filter(TrueRake723, Layer > 85 & Layer <= max(TrueRake723$Layer))
TrueRake723$Value <- as.numeric(TrueRake723$Value)
Groups723 <- ceiling(length(unique(TrueRake723$Layer))/10)

TrueRake723$Time <- as.POSIXct(TrueRake723$Time,format="%H:%M:%OS")
Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")

for(i in 1:length(TrueRake723$Layer)) {
  for(j in 1:length(Defects723$Layer)) {
    if(TrueRake723$Layer[i]==j) {TrueRake723$TimeOfRake[i] = TrueRake723$Time[i]-Defects723$Time[j]}
  }
}


testing <- as.data.frame(unique(Build629$X2))
####################################################################################################################

#########################################
#########################################
############ DATA ANALYSIS ##############
#########################################
#########################################
########################### Defect: Smoke Event ####################################

##############################################
############ Smoke Detector Count ############
##############################################

##################
#### Build629 ####
##################

SmokeDetectorCount629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.SmokeDetector.Counts")

#Add layer to SmokeDetectorCount and make plot
#Add layer to categorize
Time_new <- SmokeDetectorCount629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

SmokeDetectorCount629$Layer <- Layer_new
colnames(SmokeDetectorCount629) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
SmokeDetectorCount629 <- subset(SmokeDetectorCount629, Layer > 80 & Layer < max(Defects629Layer))

### Assign processes within each layer
SmokeDetectorCount629Time <- SmokeDetectorCount629$Time
tempOutputDescription629 <- OutputDescription629

Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(SmokeDetectorCount629$Value))

for(i in 1:length(SmokeDetectorCount629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (SmokeDetectorCount629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (SmokeDetectorCount629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (SmokeDetectorCount629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (SmokeDetectorCount629Time[i] >= Process629Time[j] && SmokeDetectorCount629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
SmokeDetectorCount629$Process <- Process629
SmokeDetectorCount629$Build <- "629"

#Create a table to count the number of times an action is performed in each layer
tempSmokeDetectorFreq629 <- subset(SmokeDetectorCount629 [c(4,5)], Layer >85)

Process629 <- rep(c(unique(OutputDescription629$Process)), length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempSmokeDetectorFreq629 <- subset(tempSmokeDetectorFreq629, tempSmokeDetectorFreq629$Layer == j)
      N[i] = length(which(temptempSmokeDetectorFreq629 ==Process629[i]))
    }
  }
}

ProcessFreqSmoke <- cbind.data.frame(Layer, Process629 , N)
SummarySmokeFreq <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process629))), FUN=sum)
colnames(SummarySmokeFreq) <- c("Process", "Frequency")

SummarySmokeMean <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process629))), FUN=mean)
colnames(SummarySmokeMean) <- c("Process", "Average")

SummarySmoke629 <- cbind.data.frame(SummarySmokeFreq, SummarySmokeMean [c(2)])
SummarySmoke629 <- SummarySmoke629[order(-SummarySmoke629$Frequency),]

# #Plotting around different layers
# SmokeDetectorCount629$Time <- as.POSIXct(SmokeDetectorCount629$Time,format="%H:%M:%OS")
# SmokeDetectorCount629$Value <- as.numeric(SmokeDetectorCount629$Value)
# 
# SmokeProcessPlotData629 <- subset(SmokeDetectorCount629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(SmokeProcessPlotData629$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(SmokeProcessPlotData629$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke629Count <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount629New <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Smoke Detector Count/","Smoke629ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke629ProcessPlot <- ggplot(data = SmokeCount629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke629Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount629New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(SmokeCount629New$Layer)) {
#         if(SmokeCount629New$Layer[j]==Defects629$Layer[i]) {Smoke629ProcessPlot = Smoke629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(SmokeProcessPlotData629$Layer)
#     tempSmoke629Count <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount629New <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Smoke Detector Count/","Smoke629ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke629ProcessPlot <- ggplot(data = tempSmoke629Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount629New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(SmokeCount629New$Layer)) {
#         if(SmokeCount629New$Layer[j]==Defects629$Layer[i]) {Smoke629ProcessPlot = Smoke629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke629Count <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount629New <- filter(SmokeProcessPlotData629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Smoke Detector Count/","Smoke629ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke629ProcessPlot <- ggplot(data = tempSmoke629Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount629New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(SmokeCount629New$Layer)) {
#         if(SmokeCount629New$Layer[j]==Defects629$Layer[i]) {Smoke629ProcessPlot = Smoke629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCount629$Value <- as.numeric(SmokeDetectorCount629$Value)
SmokeCountThreshold629 <- filter(SmokeDetectorCount629, Value >= 200)

##################
#### Build703 ####
##################

SmokeDetectorCount703 <- subset(Build703 [c(2,3,6)], X2=="OPC.PowerSupply.SmokeDetector.Counts")

#Add layer to SmokeDetectorCount and make plot
#Add layer to categorize
Time_new <- SmokeDetectorCount703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

SmokeDetectorCount703$Layer <- Layer_new
colnames(SmokeDetectorCount703) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
SmokeDetectorCount703 <- subset(SmokeDetectorCount703, Layer > 80 & Layer <= max(Defects703$Layer))

### Assign processes within each layer
SmokeDetectorCount703Time <- SmokeDetectorCount703$Time
tempOutputDescription703 <- OutputDescription703
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(SmokeDetectorCount703$Value))

for(i in 1:length(SmokeDetectorCount703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (SmokeDetectorCount703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (SmokeDetectorCount703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (SmokeDetectorCount703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (SmokeDetectorCount703Time[i] >= Process703Time[j] && SmokeDetectorCount703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
SmokeDetectorCount703$Process <- Process703
SmokeDetectorCount703$Build <- "703"

#Create a table to count the number of times an action is performed in each layer
tempSmokeDetectorFreq703 <- subset(SmokeDetectorCount703 [c(4,5)],)

Process703 <- rep(c(unique(OutputDescription703$Process)), length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempSmokeDetectorFreq703 <- subset(tempSmokeDetectorFreq703, tempSmokeDetectorFreq703$Layer == j)
      N[i] = length(which(temptempSmokeDetectorFreq703 ==Process703[i]))
    }
  }
}

ProcessFreqSmoke <- cbind.data.frame(Layer, Process703 , N)
SummarySmokeFreq <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process703))), FUN=sum)
colnames(SummarySmokeFreq) <- c("Process", "Frequency")

SummarySmokeMean <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process703))), FUN=mean)
colnames(SummarySmokeMean) <- c("Process", "Average")

SummarySmoke703 <- cbind.data.frame(SummarySmokeFreq, SummarySmokeMean [c(2)])
SummarySmoke703 <- SummarySmoke703[order(-SummarySmoke703$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList703 <- unique(DefectList703)
# 
# SmokeDetectorCount703$Time <- as.POSIXct(SmokeDetectorCount703$Time,format="%H:%M:%OS")
# SmokeDetectorCount703$Value <- as.numeric(SmokeDetectorCount703$Value)
# 
# Smoke20BeforeDefect703 <- subset(SmokeDetectorCount703, Layer >= DefectList703$Layer[1]-20 & Layer <= DefectList703$Layer[1]-1)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(Smoke20BeforeDefect703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect703$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke703Count <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Smoke Detector Count/","Smoke20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = SmokeCount703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke703Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#     }
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect703$Layer)
#     tempSmoke703Count <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Smoke Detector Count/","Smoke20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
# 
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = tempSmoke703Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#     }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke703Count <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(Smoke20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Smoke Detector Count/","Smoke20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = tempSmoke703Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempSmoke703Count <- filter(SmokeDetectorCount703, Layer == DefectList703$Layer[1])
# SmokeCount703New <- filter(SmokeDetectorCount703, Layer == DefectList703$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Smoke Detector Count/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Smoke703ProcessPlot <- ggplot(data = tempSmoke703Count, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#   ylab("Smoke Detector Count") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Smoke Detector Count \n Layer", DefectList703$Layer[1]))
# 
# plot(Smoke703ProcessPlot)
# 
# dev.off()

# #### Plotting around different layers
# SmokeDetectorCount703$Time <- as.POSIXct(SmokeDetectorCount703$Time,format="%H:%M:%OS")
# SmokeDetectorCount703$Value <- as.numeric(SmokeDetectorCount703$Value)
# 
# SmokeProcessPlotData703 <- subset(SmokeDetectorCount703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(SmokeProcessPlotData703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(SmokeProcessPlotData703$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke703Count <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Smoke Detector Count/","Smoke703ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = SmokeCount703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke703Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(SmokeProcessPlotData703$Layer)
#     tempSmoke703Count <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Smoke Detector Count/","Smoke703ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = tempSmoke703Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke703Count <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount703New <- filter(SmokeProcessPlotData703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Smoke Detector Count/","Smoke703ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke703ProcessPlot <- ggplot(data = tempSmoke703Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount703New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(SmokeCount703New$Layer)) {
#         if(SmokeCount703New$Layer[j]==Defects703$Layer[i]) {Smoke703ProcessPlot = Smoke703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCount703$Value <- as.numeric(SmokeDetectorCount703$Value)
SmokeCountThreshold703 <- filter(SmokeDetectorCount703, Value >= 200)

##################
#### Build710 ####
##################

SmokeDetectorCount710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.SmokeDetector.Counts")

#Add layer to SmokeDetectorCount and make plot
#Add layer to categorize
Time_new <- SmokeDetectorCount710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

SmokeDetectorCount710$Layer <- Layer_new
colnames(SmokeDetectorCount710) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
SmokeDetectorCount710 <- subset(SmokeDetectorCount710, Layer > 80 & Layer <= max(Defects710$Layer))

### Assign processes within each layer
SmokeDetectorCount710Time <- SmokeDetectorCount710$Time
tempOutputDescription710 <- OutputDescription710
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(SmokeDetectorCount710$Value))

for(i in 1:length(SmokeDetectorCount710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (SmokeDetectorCount710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (SmokeDetectorCount710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (SmokeDetectorCount710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (SmokeDetectorCount710Time[i] >= Process710Time[j] && SmokeDetectorCount710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
SmokeDetectorCount710$Process <- Process710
SmokeDetectorCount710$Build <- "710"

#Create a table to count the number of times an action is performed in each layer
tempSmokeDetectorFreq710 <- subset(SmokeDetectorCount710 [c(4,5)],)

Process710 <- rep(c(unique(OutputDescription710$Process)), length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempSmokeDetectorFreq710 <- subset(tempSmokeDetectorFreq710, tempSmokeDetectorFreq710$Layer == j)
      N[i] = length(which(temptempSmokeDetectorFreq710 ==Process710[i]))
    }
  }
}

ProcessFreqSmoke <- cbind.data.frame(Layer, Process710 , N)
SummarySmokeFreq <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process710))), FUN=sum)
colnames(SummarySmokeFreq) <- c("Process", "Frequency")

SummarySmokeMean <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process710))), FUN=mean)
colnames(SummarySmokeMean) <- c("Process", "Average")

SummarySmoke710 <- cbind.data.frame(SummarySmokeFreq, SummarySmokeMean [c(2)])
SummarySmoke710 <- SummarySmoke710[order(-SummarySmoke710$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList710 <- unique(DefectList710)
# 
# SmokeDetectorCount710$Time <- as.POSIXct(SmokeDetectorCount710$Time,format="%H:%M:%OS")
# SmokeDetectorCount710$Value <- as.numeric(SmokeDetectorCount710$Value)
# 
# Smoke20BeforeDefect710 <- subset(SmokeDetectorCount710, Layer >= DefectList710$Layer[1]-20 & Layer <= DefectList710$Layer[1]-1)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(Smoke20BeforeDefect710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect710$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke710Count <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Smoke Detector Count/","Smoke20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = SmokeCount710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke710Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect710$Layer)
#     tempSmoke710Count <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Smoke Detector Count/","Smoke20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = tempSmoke710Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke710Count <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(Smoke20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Smoke Detector Count/","Smoke20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = tempSmoke710Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempSmoke710Count <- filter(SmokeDetectorCount710, Layer == DefectList710$Layer[1])
# SmokeCount710New <- filter(SmokeDetectorCount710, Layer == DefectList710$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Smoke Detector Count/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Smoke710ProcessPlot <- ggplot(data = tempSmoke710Count, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#   ylab("Smoke Detector Count") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Smoke Detector Count \n Layer", DefectList710$Layer[1]))
# 
# plot(Smoke710ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# SmokeDetectorCount710$Time <- as.POSIXct(SmokeDetectorCount710$Time,format="%H:%M:%OS")
# SmokeDetectorCount710$Value <- as.numeric(SmokeDetectorCount710$Value)
# 
# SmokeProcessPlotData710 <- subset(SmokeDetectorCount710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(SmokeProcessPlotData710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(SmokeProcessPlotData710$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke710Count <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Smoke Detector Count/","Smoke710ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = SmokeCount710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke710Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(SmokeProcessPlotData710$Layer)
#     tempSmoke710Count <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Smoke Detector Count/","Smoke710ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = tempSmoke710Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke710Count <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount710New <- filter(SmokeProcessPlotData710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Smoke Detector Count/","Smoke710ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke710ProcessPlot <- ggplot(data = tempSmoke710Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount710New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(SmokeCount710New$Layer)) {
#         if(SmokeCount710New$Layer[j]==Defects710$Layer[i]) {Smoke710ProcessPlot = Smoke710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCount710$Value <- as.numeric(SmokeDetectorCount710$Value)
SmokeCountThreshold710 <- filter(SmokeDetectorCount710, Value >= 200)

##################
#### Build718 ####
##################

SmokeDetectorCount718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.SmokeDetector.Counts")

#Add layer to SmokeDetectorCount and make plot
#Add layer to categorize
Time_new <- SmokeDetectorCount718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

SmokeDetectorCount718$Layer <- Layer_new
colnames(SmokeDetectorCount718) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
SmokeDetectorCount718 <- subset(SmokeDetectorCount718, Layer > 80 & Layer < max(Defects718$Layer))

### Assign processes within each layer
SmokeDetectorCount718Time <- SmokeDetectorCount718$Time
tempOutputDescription718 <- OutputDescription718
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(SmokeDetectorCount718$Value))

for(i in 1:length(SmokeDetectorCount718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (SmokeDetectorCount718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (SmokeDetectorCount718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (SmokeDetectorCount718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (SmokeDetectorCount718Time[i] >= Process718Time[j] && SmokeDetectorCount718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
SmokeDetectorCount718$Process <- Process718
SmokeDetectorCount718$Build <- "718"

#Create a table to count the number of times an action is performed in each layer
tempSmokeDetectorFreq718 <- subset(SmokeDetectorCount718 [c(4,5)],)

Process718 <- rep(c(unique(OutputDescription718$Process)), length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempSmokeDetectorFreq718 <- subset(tempSmokeDetectorFreq718, tempSmokeDetectorFreq718$Layer == j)
      N[i] = length(which(temptempSmokeDetectorFreq718 ==Process718[i]))
    }
  }
}

ProcessFreqSmoke <- cbind.data.frame(Layer, Process718 , N)
SummarySmokeFreq <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process718))), FUN=sum)
colnames(SummarySmokeFreq) <- c("Process", "Frequency")

SummarySmokeMean <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process718))), FUN=mean)
colnames(SummarySmokeMean) <- c("Process", "Average")

SummarySmoke718 <- cbind.data.frame(SummarySmokeFreq, SummarySmokeMean [c(2)])
SummarySmoke718 <- SummarySmoke718[order(-SummarySmoke718$Frequency),]

quantile(SmokeDetectorCount718$Value, c(.75, .8, .85, .9, .95, .975, .995, 1))

# #### Plotting 20 Layers Before the Defect
# Defects <- c(104,223,254)
# 
# SmokeDetectorCount718$Time <- as.POSIXct(SmokeDetectorCount718$Time,format="%H:%M:%OS")
# SmokeDetectorCount718$Value <- as.numeric(SmokeDetectorCount718$Value)
# 
# Smoke20BeforeDefect718_1 <- subset(SmokeDetectorCount718, Layer >= Defects[1]-20 & Layer <=Defects[1]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Smoke20BeforeDefect718_1$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect718_1$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = SmokeCount718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke718Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect718_1$Layer)
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# Smoke20BeforeDefect718_2 <- subset(SmokeDetectorCount718, Layer >= Defects[2]-20 & Layer <=Defects[2]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Smoke20BeforeDefect718_2$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect718_2$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = SmokeCount718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke718Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect718_2$Layer)
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# Smoke20BeforeDefect718_3 <- subset(SmokeDetectorCount718, Layer >= Defects[3]-20 & Layer <=Defects[3]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Smoke20BeforeDefect718_3$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect718_3$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = SmokeCount718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke718Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect718_3$Layer)
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(Smoke20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","Smoke20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# for(i in 1:length(Defects)) {
#   tempSmoke718Count <- filter(SmokeDetectorCount718, Layer == Defects[i])
#   SmokeCount718New <- filter(SmokeDetectorCount718, Layer == Defects[i])
#   
#   #Set Plot location and dimensions
#   png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Smoke Detector Count/","DefectPlot",i,".png" ,sep = ""),
#       width = 999,
#       height = 333)
#   
#   #Create Plot
#   theme_update(plot.title = element_text(hjust = 0.5))
#   Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#     geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#     geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#     ylab("Smoke Detector Count") +
#     xlab("Time") +
#     ggtitle(paste("Processes of Smoke Detector Count \n Layer", Defects[i]))
#   
#   plot(Smoke718ProcessPlot)
#   
#   dev.off()
#   
# }

# #Plotting around different layers
# SmokeDetectorCount718$Time <- as.POSIXct(SmokeDetectorCount718$Time,format="%H:%M:%OS")
# SmokeDetectorCount718$Value <- as.numeric(SmokeDetectorCount718$Value)
# 
# SmokeProcessPlotData718 <- subset(SmokeDetectorCount718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(SmokeProcessPlotData718$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(SmokeProcessPlotData718$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Smoke Detector Count/","Smoke718ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = SmokeCount718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke718Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(SmokeProcessPlotData718$Layer)
#     tempSmoke718Count <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Smoke Detector Count/","Smoke718ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke718Count <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount718New <- filter(SmokeProcessPlotData718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Smoke Detector Count/","Smoke718ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke718ProcessPlot <- ggplot(data = tempSmoke718Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount718New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(SmokeCount718New$Layer)) {
#         if(SmokeCount718New$Layer[j]==Defects718$Layer[i]) {Smoke718ProcessPlot = Smoke718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCount718$Value <- as.numeric(SmokeDetectorCount718$Value)
SmokeCountThreshold718 <- filter(SmokeDetectorCount718, Value >= 200)

##################
#### Build723 ####
##################

SmokeDetectorCount723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.SmokeDetector.Counts")

#Add layer to SmokeDetectorCount and make plot
#Add layer to categorize
Time_new <- SmokeDetectorCount723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

SmokeDetectorCount723$Layer <- Layer_new
colnames(SmokeDetectorCount723) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
SmokeDetectorCount723 <- subset(SmokeDetectorCount723, Layer > 80 & Layer <= max(Defects723$Layer))

### Assign processes within each layer
SmokeDetectorCount723Time <- SmokeDetectorCount723$Time
tempOutputDescription723 <- OutputDescription723
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(SmokeDetectorCount723$Value))

for(i in 1:length(SmokeDetectorCount723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (SmokeDetectorCount723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (SmokeDetectorCount723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (SmokeDetectorCount723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (SmokeDetectorCount723Time[i] >= Process723Time[j] && SmokeDetectorCount723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
SmokeDetectorCount723$Process <- Process723
SmokeDetectorCount723$Build <- "723"

#Create a table to count the number of times an action is performed in each layer
tempSmokeDetectorFreq723 <- subset(SmokeDetectorCount723 [c(4,5)],)

Process723 <- rep(c(unique(OutputDescription723$Process)), length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempSmokeDetectorFreq723 <- subset(tempSmokeDetectorFreq723, tempSmokeDetectorFreq723$Layer == j)
      N[i] = length(which(temptempSmokeDetectorFreq723 ==Process723[i]))
    }
  }
}

ProcessFreqSmoke <- cbind.data.frame(Layer, Process723 , N)
SummarySmokeFreq <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process723))), FUN=sum)
colnames(SummarySmokeFreq) <- c("Process", "Frequency")

SummarySmokeMean <- aggregate(ProcessFreqSmoke$N, by=(list((ProcessFreqSmoke$Process723))), FUN=mean)
colnames(SummarySmokeMean) <- c("Process", "Average")

SummarySmoke723 <- cbind.data.frame(SummarySmokeFreq, SummarySmokeMean [c(2)])
SummarySmoke723 <- SummarySmoke723[order(-SummarySmoke723$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList723 <- unique(DefectList723)
# 
# SmokeDetectorCount723$Time <- as.POSIXct(SmokeDetectorCount723$Time,format="%H:%M:%OS")
# SmokeDetectorCount723$Value <- as.numeric(SmokeDetectorCount723$Value)
# 
# Smoke20BeforeDefect723 <- subset(SmokeDetectorCount723, Layer >= DefectList723$Layer[4]-20 & Layer <= DefectList723$Layer[4]-1)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(Smoke20BeforeDefect723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(Smoke20BeforeDefect723$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke723Count <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Smoke Detector Count/","Smoke20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = SmokeCount723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke723Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Smoke20BeforeDefect723$Layer)
#     tempSmoke723Count <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Smoke Detector Count/","Smoke20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = tempSmoke723Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke723Count <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(Smoke20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Smoke Detector Count/","Smoke20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = tempSmoke723Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempSmoke723Count <- filter(SmokeDetectorCount723, Layer == DefectList723$Layer[4])
# SmokeCount723New <- filter(SmokeDetectorCount723, Layer == DefectList723$Layer[4])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Smoke Detector Count/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Smoke723ProcessPlot <- ggplot(data = tempSmoke723Count, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#   ylab("Smoke Detector Count") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Smoke Detector Count \n Layer", DefectList723$Layer[4]))
# 
# plot(Smoke723ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# SmokeDetectorCount723$Time <- as.POSIXct(SmokeDetectorCount723$Time,format="%H:%M:%OS")
# SmokeDetectorCount723$Value <- as.numeric(SmokeDetectorCount723$Value)
# 
# SmokeProcessPlotData723 <- subset(SmokeDetectorCount723, Layer > 85)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(SmokeProcessPlotData723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(SmokeProcessPlotData723$Layer)
#     GroupEnd=GroupStart+4
#     tempSmoke723Count <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Smoke Detector Count/","Smoke723ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = SmokeCount723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempSmoke723Count, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(SmokeProcessPlotData723$Layer)
#     tempSmoke723Count <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Smoke Detector Count/","Smoke723ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = tempSmoke723Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempSmoke723Count <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     SmokeCount723New <- filter(SmokeProcessPlotData723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Smoke Detector Count/","Smoke723ProcessPlot",i,".png" ,sep = ""), 
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Smoke723ProcessPlot <- ggplot(data = tempSmoke723Count, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=SmokeCount723New, mapping = aes(x = Time, y = Value))+
#       ylab("Smoke Detector Count") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Smoke Detector Count \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(SmokeCount723New$Layer)) {
#         if(SmokeCount723New$Layer[j]==Defects723$Layer[i]) {Smoke723ProcessPlot = Smoke723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Smoke723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCount723$Value <- as.numeric(SmokeDetectorCount723$Value)
SmokeCountThreshold723 <- filter(SmokeDetectorCount723, Value >= 200)

###############################################

#####Combine all datasets
SmokeDetectorCountAll <- rbind(SmokeDetectorCount629, 
                               SmokeDetectorCount703, 
                               SmokeDetectorCount710, 
                               SmokeDetectorCount718, 
                               SmokeDetectorCount723)

SmokeDetectorCountAll$Layer_ID <- paste(SmokeDetectorCountAll$Build, "/", SmokeDetectorCountAll$Layer)
#Identify layers where the Smoke Detector Count surpassed 200
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
SmokeDetectorCount_gt200 <- filter(SmokeDetectorCountAll, Value >= 200)

###########################################################################################################################

###############################################
############ High Voltage Arc Trip ############
###############################################

##################
#### Build629 ####
##################

HighVoltageArcTrip629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.ArcTrip")

#Add layer to High Voltage Arc Trip
#Add layer to categorize
Time_new <- HighVoltageArcTrip629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}}
  }
}

HighVoltageArcTrip629$Layer <- Layer_new
HighVoltageArcTrip629 <- subset(HighVoltageArcTrip629, Layer!="0")
head(HighVoltageArcTrip629)

##Arc Trip at Layer 335

##################
#### Build703 ####
##################

HighVoltageArcTrip703 <- subset(Build703 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.ArcTrip")

#Add layer to High Voltage Arc Trip
#Add layer to categorize
Time_new <- HighVoltageArcTrip703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}}
  }
}

HighVoltageArcTrip703$Layer <- Layer_new
HighVoltageArcTrip703 <- subset(HighVoltageArcTrip703, Layer!="0")
head(HighVoltageArcTrip703)

##Arc Trip at Layers 223 and 231

##################
#### Build710 ####
##################

HighVoltageArcTrip710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.ArcTrip")

#Add layer to High Voltage Arc Trip
#Add layer to categorize
Time_new <- HighVoltageArcTrip710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}}
  }
}

HighVoltageArcTrip710$Layer <- Layer_new
HighVoltageArcTrip710 <- subset(HighVoltageArcTrip710, Layer!="0")
head(HighVoltageArcTrip710)

##Arc Trip at Layer 254


##################
#### Build718 ####
##################

HighVoltageArcTrip718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.ArcTrip")

#Add layer to High Voltage Arc Trip
#Add layer to categorize
Time_new <- HighVoltageArcTrip718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}}
  }
}

HighVoltageArcTrip718$Layer <- Layer_new
HighVoltageArcTrip718 <- subset(HighVoltageArcTrip718, Layer!="0")
head(HighVoltageArcTrip718)

##Arc Trip at Layers 335

##################
#### Build723 ####
##################

HighVoltageArcTrip723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.ArcTrip")

#Add layer to High Voltage Arc Trip
#Add layer to categorize
Time_new <- HighVoltageArcTrip723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}}
  }
}

HighVoltageArcTrip723$Layer <- Layer_new
HighVoltageArcTrip723 <- subset(HighVoltageArcTrip723, Layer!="0")
head(HighVoltageArcTrip723)

##Arc Trip at Layers 1 and 104

#####Combine all datasets#####

HighVoltageArcTrip629$Build <- "629"
HighVoltageArcTrip703$Build <- "703"
HighVoltageArcTrip710$Build <- "710"
HighVoltageArcTrip718$Build <- "718"
HighVoltageArcTrip723$Build <- "723"

HighVoltageArcTripAll <- rbind(HighVoltageArcTrip629, 
                               HighVoltageArcTrip703, 
                               HighVoltageArcTrip710, 
                               HighVoltageArcTrip718, 
                               HighVoltageArcTrip723)

HighVoltageArcTripAll <- filter(HighVoltageArcTripAll, X5 == "True")

HighVoltageArcTripAll$Layer_ID <- paste(HighVoltageArcTripAll$Build, "/", HighVoltageArcTripAll$Layer)

###########################################################################################################################

##############################################
################ Beam Current ################ 
############################################## 

##################
#### Build629 ####
##################

BeamCurrent629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.Beam.BeamCurrent")

#Add layer to Beam Current
#Add layer to categorize
Time_new <- BeamCurrent629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}}
  }
}

BeamCurrent629$Layer <- Layer_new
colnames(BeamCurrent629) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
BeamCurrent629 <- subset(BeamCurrent629, Layer > 80 & Layer < max(Defects629Layer))

### Assign processes within each layer
BeamCurrent629Time <- BeamCurrent629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])

ProcessTime629 <- tempOutputDescription629$Time
ProcessPosition629 <- tempOutputDescription629$Process
Process629 <- rep(0, length(BeamCurrent629$Value))

for(i in 1:length(BeamCurrent629Time)) {
  for(j in 1:length(ProcessTime629)) {
    if (j == 1) { 
      if (BeamCurrent629Time[i] < ProcessTime629[j]) { Process629[i]=ProcessPosition629[j-1] }
      else if (BeamCurrent629Time[i] <= ProcessTime629[j+1]) { Process629[i]=ProcessPosition629[j]}}
    else if (j == length(ProcessTime629)) { 
      if (BeamCurrent629Time[i] > ProcessTime629[j]) { Process629[i]=ProcessPosition629[length(ProcessPosition629)] }}
    else {
      if (BeamCurrent629Time[i] >= ProcessTime629[j] && BeamCurrent629Time[i] < ProcessTime629[j+1]) {
        Process629[i] = ProcessPosition629[j]}
    }
  }
}

head(Process629)
BeamCurrent629$Process <- Process629
colnames(BeamCurrent629) <- list("Time", "Variable", "Value", "Layer", "Process")
BeamCurrent629$Build <- "629"

#Create a table to count the number of times an action is performed in each layer
tempBeamCurrentFreq629 <- subset(BeamCurrent629 [c(4,5)],)

Process <- rep(c(unique(OutputDescription629$Process)), length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process))

for(i in 1:length(Process)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempBeamCurrentFreq629 <- subset(tempBeamCurrentFreq629, tempBeamCurrentFreq629$Layer == j)
      N[i] = length(which(temptempBeamCurrentFreq629==Process[i]))
    }
  }
}

ProcessFreqBeamCurrent629 <- cbind.data.frame(Layer, Process , N)
SummaryBeamCurrent629Freq <- aggregate(ProcessFreqBeamCurrent629$N, by=(list((ProcessFreqBeamCurrent629$Process))), FUN=sum)
colnames(SummaryBeamCurrent629Freq) <- c("Process", "Frequency")

SummaryBeamCurrent629Mean <- aggregate(ProcessFreqBeamCurrent629$N, by=(list((ProcessFreqBeamCurrent629$Process))), FUN=mean)
colnames(SummaryBeamCurrent629Mean) <- c("Process", "Average")

SummaryBeam629 <- cbind.data.frame(SummaryBeamCurrent629Freq, SummaryBeamCurrent629Mean [c(2)])
SummaryBeam629 <- SummaryBeam629[order(-SummaryBeam629$Frequency),]

# #Plotting around different layers
# BeamCurrent629$Time <- as.POSIXct(BeamCurrent629$Time,format="%H:%M:%OS")
# BeamCurrent629$Value <- as.numeric(BeamCurrent629$Value)
# 
# BeamCurrent629ProcessPlot <- subset(BeamCurrent629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(BeamCurrent629ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(BeamCurrent629ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam629Current <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent629New <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Beam Current/","BeamCurrent629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam629ProcessPlot <- ggplot(data = BeamCurrent629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam629Current, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent629New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BeamCurrent629New$Layer)) {
#         if(BeamCurrent629New$Layer[j]==Defects629$Layer[i]) {Beam629ProcessPlot = Beam629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BeamCurrent629ProcessPlot$Layer)
#     tempBeam629Current <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent629New <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Beam Current/","BeamCurrent629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam629ProcessPlot <- ggplot(data = tempBeam629Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BeamCurrent629New$Layer)) {
#         if(BeamCurrent629New$Layer[j]==Defects629$Layer[i]) {Beam629ProcessPlot = Beam629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam629Current <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent629New <- filter(BeamCurrent629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Beam Current/","BeamCurrent629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam629ProcessPlot <- ggplot(data = tempBeam629Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BeamCurrent629New$Layer)) {
#         if(BeamCurrent629New$Layer[j]==Defects629$Layer[i]) {Beam629ProcessPlot = Beam629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

BeamCurrent703 <- subset(Build703 [c(2,3,6)], X2=="OPC.PowerSupply.Beam.BeamCurrent")

#Add layer to Beam Current
#Add layer to categorize
Time_new <- BeamCurrent703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}}
  }
}

BeamCurrent703$Layer <- Layer_new
colnames(BeamCurrent703) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
BeamCurrent703 <- subset(BeamCurrent703, Layer > 80 & Layer <= max(Defects703$Layer))

### Assign processes within each layer
BeamCurrent703Time <- BeamCurrent703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])

ProcessTime703 <- tempOutputDescription703$Time
ProcessPosition703 <- tempOutputDescription703$Process
Process703 <- rep(0, length(BeamCurrent703$Value))

for(i in 1:length(BeamCurrent703Time)) {
  for(j in 1:length(ProcessTime703)) {
    if (j == 1) { 
      if (BeamCurrent703Time[i] < ProcessTime703[j]) { Process703[i]=ProcessPosition703[j-1] }
      else if (BeamCurrent703Time[i] <= ProcessTime703[j+1]) { Process703[i]=ProcessPosition703[j]}}
    else if (j == length(ProcessTime703)) { 
      if (BeamCurrent703Time[i] > ProcessTime703[j]) { Process703[i]=ProcessPosition703[length(ProcessPosition703)] }}
    else {
      if (BeamCurrent703Time[i] >= ProcessTime703[j] && BeamCurrent703Time[i] < ProcessTime703[j+1]) {
        Process703[i] = ProcessPosition703[j]}
    }
  }
}

head(Process703)
BeamCurrent703$Process <- Process703
colnames(BeamCurrent703) <- list("Time", "Variable", "Value", "Layer", "Process")
BeamCurrent703$Build <- "703"

#Create a table to count the number of times an action is performed in each layer
tempBeamCurrentFreq703 <- subset(BeamCurrent703 [c(4,5)],)

Process <- rep(c(unique(OutputDescription703$Process)), length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process))

for(i in 1:length(Process)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempBeamCurrentFreq703 <- subset(tempBeamCurrentFreq703, tempBeamCurrentFreq703$Layer == j)
      N[i] = length(which(temptempBeamCurrentFreq703==Process[i]))
    }
  }
}

ProcessFreqBeamCurrent703 <- cbind.data.frame(Layer, Process , N)
SummaryBeamCurrent703Freq <- aggregate(ProcessFreqBeamCurrent703$N, by=(list((ProcessFreqBeamCurrent703$Process))), FUN=sum)
colnames(SummaryBeamCurrent703Freq) <- c("Process", "Frequency")

SummaryBeamCurrent703Mean <- aggregate(ProcessFreqBeamCurrent703$N, by=(list((ProcessFreqBeamCurrent703$Process))), FUN=mean)
colnames(SummaryBeamCurrent703Mean) <- c("Process", "Average")

SummaryBeam703 <- cbind.data.frame(SummaryBeamCurrent703Freq, SummaryBeamCurrent703Mean [c(2)])
SummaryBeam703 <- SummaryBeam703[order(-SummaryBeam703$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList703 <- unique(DefectList703)
# 
# BeamCurrent703$Time <- as.POSIXct(BeamCurrent703$Time,format="%H:%M:%OS")
# BeamCurrent703$Value <- as.numeric(BeamCurrent703$Value)
# 
# Beam20BeforeDefect703 <- subset(BeamCurrent703, Layer >= DefectList703$Layer[1]-20 & Layer <= DefectList703$Layer[1]-1)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(Beam20BeforeDefect703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect703$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam703 <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam703New <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Beam Current/","Beam20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = Beam703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam703New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(Beam703New$Layer)) {
#         if(Beam703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect703$Layer)
#     tempBeam703 <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam703New <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Beam Current/","Beam20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = tempBeam703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam703New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(Beam703New$Layer)) {
#         if(Beam703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam703 <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam703New <- filter(Beam20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Beam Current/","Beam20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = tempBeam703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam703New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(Beam703New$Layer)) {
#         if(Beam703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempBeam703 <- filter(BeamCurrent703, Layer == DefectList703$Layer[1])
# Beam703New <- filter(BeamCurrent703, Layer == DefectList703$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Beam Current/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Beam703ProcessPlot <- ggplot(data = tempBeam703, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=Beam703New, mapping = aes(x = Time, y = Value))+
#   ylab("Beam Current") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Beam Current \n Layer", DefectList703$Layer[1]))
# 
# plot(Beam703ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# BeamCurrent703$Time <- as.POSIXct(BeamCurrent703$Time,format="%H:%M:%OS")
# BeamCurrent703$Value <- as.numeric(BeamCurrent703$Value)
# 
# BeamCurrent703ProcessPlot <- subset(BeamCurrent703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(BeamCurrent703ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(BeamCurrent703ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam703Current <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent703New <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Beam Current/","BeamCurrent703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = BeamCurrent703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam703Current, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent703New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BeamCurrent703New$Layer)) {
#         if(BeamCurrent703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BeamCurrent703ProcessPlot$Layer)
#     tempBeam703Current <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent703New <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Beam Current/","BeamCurrent703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = tempBeam703Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BeamCurrent703New$Layer)) {
#         if(BeamCurrent703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam703Current <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent703New <- filter(BeamCurrent703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Beam Current/","BeamCurrent703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam703ProcessPlot <- ggplot(data = tempBeam703Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BeamCurrent703New$Layer)) {
#         if(BeamCurrent703New$Layer[j]==Defects703$Layer[i]) {Beam703ProcessPlot = Beam703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build710 ####
##################

BeamCurrent710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.Beam.BeamCurrent")

#Add layer to Beam Current
#Add layer to categorize
Time_new <- BeamCurrent710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}}
  }
}

BeamCurrent710$Layer <- Layer_new
colnames(BeamCurrent710) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
BeamCurrent710 <- subset(BeamCurrent710, Layer > 80 & Layer <= max(Defects710$Layer))

### Assign processes within each layer
BeamCurrent710Time <- BeamCurrent710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])

ProcessTime710 <- tempOutputDescription710$Time
ProcessPosition710 <- tempOutputDescription710$Process
Process710 <- rep(0, length(BeamCurrent710$Value))

for(i in 1:length(BeamCurrent710Time)) {
  for(j in 1:length(ProcessTime710)) {
    if (j == 1) { 
      if (BeamCurrent710Time[i] < ProcessTime710[j]) { Process710[i]=ProcessPosition710[j-1] }
      else if (BeamCurrent710Time[i] <= ProcessTime710[j+1]) { Process710[i]=ProcessPosition710[j]}}
    else if (j == length(ProcessTime710)) { 
      if (BeamCurrent710Time[i] > ProcessTime710[j]) { Process710[i]=ProcessPosition710[length(ProcessPosition710)] }}
    else {
      if (BeamCurrent710Time[i] >= ProcessTime710[j] && BeamCurrent710Time[i] < ProcessTime710[j+1]) {
        Process710[i] = ProcessPosition710[j]}
    }
  }
}

head(Process710)
BeamCurrent710$Process <- Process710
colnames(BeamCurrent710) <- list("Time", "Variable", "Value", "Layer", "Process")
BeamCurrent710$Build <- "710"

#Create a table to count the number of times an action is performed in each layer
tempBeamCurrentFreq710 <- subset(BeamCurrent710 [c(4,5)],)

Process <- rep(c(unique(OutputDescription710$Process)), length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process))

for(i in 1:length(Process)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempBeamCurrentFreq710 <- subset(tempBeamCurrentFreq710, tempBeamCurrentFreq710$Layer == j)
      N[i] = length(which(temptempBeamCurrentFreq710==Process[i]))
    }
  }
}

ProcessFreqBeamCurrent710 <- cbind.data.frame(Layer, Process , N)
SummaryBeamCurrent710Freq <- aggregate(ProcessFreqBeamCurrent710$N, by=(list((ProcessFreqBeamCurrent710$Process))), FUN=sum)
colnames(SummaryBeamCurrent710Freq) <- c("Process", "Frequency")

SummaryBeamCurrent710Mean <- aggregate(ProcessFreqBeamCurrent710$N, by=(list((ProcessFreqBeamCurrent710$Process))), FUN=mean)
colnames(SummaryBeamCurrent710Mean) <- c("Process", "Average")

SummaryBeam710 <- cbind.data.frame(SummaryBeamCurrent710Freq, SummaryBeamCurrent710Mean [c(2)])
SummaryBeam710 <- SummaryBeam710[order(-SummaryBeam710$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList710 <- unique(DefectList710)
# 
# BeamCurrent710$Time <- as.POSIXct(BeamCurrent710$Time,format="%H:%M:%OS")
# BeamCurrent710$Value <- as.numeric(BeamCurrent710$Value)
# 
# Beam20BeforeDefect710 <- subset(BeamCurrent710, Layer >= DefectList710$Layer[1]-20 & Layer <= DefectList710$Layer[1]-1)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(Beam20BeforeDefect710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect710$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam710 <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam710New <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Beam Current/","Beam20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = Beam710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam710New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(Beam710New$Layer)) {
#         if(Beam710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect710$Layer)
#     tempBeam710 <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam710New <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Beam Current/","Beam20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = tempBeam710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam710New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(Beam710New$Layer)) {
#         if(Beam710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam710 <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam710New <- filter(Beam20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Beam Current/","Beam20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = tempBeam710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam710New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(Beam710New$Layer)) {
#         if(Beam710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempBeam710 <- filter(BeamCurrent710, Layer == DefectList710$Layer[1])
# Beam710New <- filter(BeamCurrent710, Layer == DefectList710$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Beam Current/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Beam710ProcessPlot <- ggplot(data = tempBeam710, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=Beam710New, mapping = aes(x = Time, y = Value))+
#   ylab("Beam Current") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Beam Current \n Layer", DefectList710$Layer[1]))
# 
# plot(Beam710ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# BeamCurrent710$Time <- as.POSIXct(BeamCurrent710$Time,format="%H:%M:%OS")
# BeamCurrent710$Value <- as.numeric(BeamCurrent710$Value)
# 
# BeamCurrent710ProcessPlot <- subset(BeamCurrent710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(BeamCurrent710ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(BeamCurrent710ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam710Current <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent710New <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Beam Current/","BeamCurrent710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = BeamCurrent710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam710Current, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent710New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BeamCurrent710New$Layer)) {
#         if(BeamCurrent710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BeamCurrent710ProcessPlot$Layer)
#     tempBeam710Current <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent710New <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Beam Current/","BeamCurrent710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = tempBeam710Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BeamCurrent710New$Layer)) {
#         if(BeamCurrent710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam710Current <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent710New <- filter(BeamCurrent710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Beam Current/","BeamCurrent710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam710ProcessPlot <- ggplot(data = tempBeam710Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BeamCurrent710New$Layer)) {
#         if(BeamCurrent710New$Layer[j]==Defects710$Layer[i]) {Beam710ProcessPlot = Beam710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build718 ####
##################

BeamCurrent718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.Beam.BeamCurrent")

#Add layer to Beam Current
#Add layer to categorize
Time_new <- BeamCurrent718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}}
  }
}

BeamCurrent718$Layer <- Layer_new
colnames(BeamCurrent718) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
BeamCurrent718 <- subset(BeamCurrent718, Layer > 80 & Layer < max(Defects718$Layer))

### Assign processes within each layer
BeamCurrent718Time <- BeamCurrent718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])

ProcessTime718 <- tempOutputDescription718$Time
ProcessPosition718 <- tempOutputDescription718$Process
Process718 <- rep(0, length(BeamCurrent718$Value))

for(i in 1:length(BeamCurrent718Time)) {
  for(j in 1:length(ProcessTime718)) {
    if (j == 1) { 
      if (BeamCurrent718Time[i] < ProcessTime718[j]) { Process718[i]=ProcessPosition718[j-1] }
      else if (BeamCurrent718Time[i] <= ProcessTime718[j+1]) { Process718[i]=ProcessPosition718[j]}}
    else if (j == length(ProcessTime718)) { 
      if (BeamCurrent718Time[i] > ProcessTime718[j]) { Process718[i]=ProcessPosition718[length(ProcessPosition718)] }}
    else {
      if (BeamCurrent718Time[i] >= ProcessTime718[j] && BeamCurrent718Time[i] < ProcessTime718[j+1]) {
        Process718[i] = ProcessPosition718[j]}
    }
  }
}

head(Process718)
BeamCurrent718$Process <- Process718
colnames(BeamCurrent718) <- list("Time", "Variable", "Value", "Layer", "Process")
BeamCurrent718$Build <- "718"

#Create a table to count the number of times an action is performed in each layer
tempBeamCurrentFreq718 <- subset(BeamCurrent718 [c(4,5)],)

Process <- rep(c(unique(OutputDescription718$Process)), length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process))

for(i in 1:length(Process)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempBeamCurrentFreq718 <- subset(tempBeamCurrentFreq718, tempBeamCurrentFreq718$Layer == j)
      N[i] = length(which(temptempBeamCurrentFreq718==Process[i]))
    }
  }
}

ProcessFreqBeamCurrent718 <- cbind.data.frame(Layer, Process , N)
SummaryBeamCurrent718Freq <- aggregate(ProcessFreqBeamCurrent718$N, by=(list((ProcessFreqBeamCurrent718$Process))), FUN=sum)
colnames(SummaryBeamCurrent718Freq) <- c("Process", "Frequency")

SummaryBeamCurrent718Mean <- aggregate(ProcessFreqBeamCurrent718$N, by=(list((ProcessFreqBeamCurrent718$Process))), FUN=mean)
colnames(SummaryBeamCurrent718Mean) <- c("Process", "Average")

SummaryBeam718 <- cbind.data.frame(SummaryBeamCurrent718Freq, SummaryBeamCurrent718Mean [c(2)])
SummaryBeam718 <- SummaryBeam718[order(-SummaryBeam718$Frequency),]

# #### Plotting 20 Layers Before the Defect
# Defects <- c(104,223,254)
# 
# BeamCurrent718$Time <- as.POSIXct(BeamCurrent718$Time,format="%H:%M:%OS")
# BeamCurrent718$Value <- as.numeric(BeamCurrent718$Value)
# 
# Beam20BeforeDefect718_1 <- subset(BeamCurrent718, Layer >= Defects[1]-20 & Layer <=Defects[1]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Beam20BeforeDefect718_1$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect718_1$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = BeamCurrent718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect718_1$Layer)
#     tempBeam718 <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# Beam20BeforeDefect718_2 <- subset(BeamCurrent718, Layer >= Defects[2]-20 & Layer <=Defects[2]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Beam20BeforeDefect718_2$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect718_2$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = BeamCurrent718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect718_2$Layer)
#     tempBeam718 <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# Beam20BeforeDefect718_3 <- subset(BeamCurrent718, Layer >= Defects[3]-20 & Layer <=Defects[3]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(Beam20BeforeDefect718_3$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect718_3$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = BeamCurrent718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect718_3$Layer)
#     tempBeam718 <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam718 <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(Beam20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","Beam20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# for(i in 1:length(Defects)) {
#   tempBeam718 <- filter(BeamCurrent718, Layer == Defects[i])
#   BeamCurrent718New <- filter(BeamCurrent718, Layer == Defects[i])
#   
#   #Set Plot location and dimensions
#   png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Beam Current/","DefectPlot",i,".png" ,sep = ""),
#       width = 999,
#       height = 333)
#   
#   #Create Plot
#   theme_update(plot.title = element_text(hjust = 0.5))
#   Beam718ProcessPlot <- ggplot(data = tempBeam718, aes(x=Layer, y=Value)) + 
#     geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#     geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#     ylab("Beam Current") +
#     xlab("Time") +
#     ggtitle(paste("Processes of Beam Current \n Layer", Defects[i]))
#   
#   plot(Beam718ProcessPlot)
#   
#   dev.off()
#   
# }

# #Plotting around different layers
# BeamCurrent718$Time <- as.POSIXct(BeamCurrent718$Time,format="%H:%M:%OS")
# BeamCurrent718$Value <- as.numeric(BeamCurrent718$Value)
# 
# BeamCurrent718ProcessPlot <- subset(BeamCurrent718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(BeamCurrent718ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(BeamCurrent718ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam718Current <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Beam Current/","BeamCurrent718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = BeamCurrent718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam718Current, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BeamCurrent718ProcessPlot$Layer)
#     tempBeam718Current <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Beam Current/","BeamCurrent718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam718Current <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent718New <- filter(BeamCurrent718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Beam Current/","BeamCurrent718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam718ProcessPlot <- ggplot(data = tempBeam718Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BeamCurrent718New$Layer)) {
#         if(BeamCurrent718New$Layer[j]==Defects718$Layer[i]) {Beam718ProcessPlot = Beam718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build723 ####
##################

BeamCurrent723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.Beam.BeamCurrent")

#Add layer to Beam Current
#Add layer to categorize
Time_new <- BeamCurrent723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}}
  }
}

BeamCurrent723$Layer <- Layer_new
colnames(BeamCurrent723) <- list("Time", "Variable", "Value", "Layer")

#Focus on layers excluding support and final layer
BeamCurrent723 <- subset(BeamCurrent723, Layer > 80 & Layer <= max(Defects723$Layer))

### Assign processes within each layer
BeamCurrent723Time <- BeamCurrent723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])

ProcessTime723 <- tempOutputDescription723$Time
ProcessPosition723 <- tempOutputDescription723$Process
Process723 <- rep(0, length(BeamCurrent723$Value))

for(i in 1:length(BeamCurrent723Time)) {
  for(j in 1:length(ProcessTime723)) {
    if (j == 1) { 
      if (BeamCurrent723Time[i] < ProcessTime723[j]) { Process723[i]=ProcessPosition723[j-1] }
      else if (BeamCurrent723Time[i] <= ProcessTime723[j+1]) { Process723[i]=ProcessPosition723[j]}}
    else if (j == length(ProcessTime723)) { 
      if (BeamCurrent723Time[i] > ProcessTime723[j]) { Process723[i]=ProcessPosition723[length(ProcessPosition723)] }}
    else {
      if (BeamCurrent723Time[i] >= ProcessTime723[j] && BeamCurrent723Time[i] < ProcessTime723[j+1]) {
        Process723[i] = ProcessPosition723[j]}
    }
  }
}

head(Process723)
BeamCurrent723$Process <- Process723
colnames(BeamCurrent723) <- list("Time", "Variable", "Value", "Layer", "Process")
BeamCurrent723$Build <- "723"

#Create a table to count the number of times an action is performed in each layer
tempBeamCurrentFreq723 <- subset(BeamCurrent723 [c(4,5)],)

Process <- rep(c(unique(OutputDescription723$Process)), length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process))

for(i in 1:length(Process)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempBeamCurrentFreq723 <- subset(tempBeamCurrentFreq723, tempBeamCurrentFreq723$Layer == j)
      N[i] = length(which(temptempBeamCurrentFreq723==Process[i]))
    }
  }
}

ProcessFreqBeamCurrent723 <- cbind.data.frame(Layer, Process , N)
SummaryBeamCurrent723Freq <- aggregate(ProcessFreqBeamCurrent723$N, by=(list((ProcessFreqBeamCurrent723$Process))), FUN=sum)
colnames(SummaryBeamCurrent723Freq) <- c("Process", "Frequency")

SummaryBeamCurrent723Mean <- aggregate(ProcessFreqBeamCurrent723$N, by=(list((ProcessFreqBeamCurrent723$Process))), FUN=mean)
colnames(SummaryBeamCurrent723Mean) <- c("Process", "Average")

SummaryBeam723 <- cbind.data.frame(SummaryBeamCurrent723Freq, SummaryBeamCurrent723Mean [c(2)])
SummaryBeam723 <- SummaryBeam723[order(-SummaryBeam723$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList723 <- unique(DefectList723)
# 
# BeamCurrent723$Time <- as.POSIXct(BeamCurrent723$Time,format="%H:%M:%OS")
# BeamCurrent723$Value <- as.numeric(BeamCurrent723$Value)
# 
# Beam20BeforeDefect723 <- subset(BeamCurrent723, Layer >= DefectList723$Layer[4]-20 & Layer <= DefectList723$Layer[4]-1)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(Beam20BeforeDefect723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(Beam20BeforeDefect723$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam723 <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam723New <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Beam Current/","Beam20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = Beam723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam723New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(Beam723New$Layer)) {
#         if(Beam723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(Beam20BeforeDefect723$Layer)
#     tempBeam723 <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam723New <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Beam Current/","Beam20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = tempBeam723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam723New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(Beam723New$Layer)) {
#         if(Beam723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam723 <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     Beam723New <- filter(Beam20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Beam Current/","Beam20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = tempBeam723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=Beam723New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(Beam723New$Layer)) {
#         if(Beam723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempBeam723 <- filter(BeamCurrent723, Layer == DefectList723$Layer[4])
# Beam723New <- filter(BeamCurrent723, Layer == DefectList723$Layer[4])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Beam Current/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# Beam723ProcessPlot <- ggplot(data = tempBeam723, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=Beam723New, mapping = aes(x = Time, y = Value))+
#   ylab("Beam Current") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Beam Current \n Layer", DefectList723$Layer[4]))
# 
# plot(Beam723ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# BeamCurrent723$Time <- as.POSIXct(BeamCurrent723$Time,format="%H:%M:%OS")
# BeamCurrent723$Value <- as.numeric(BeamCurrent723$Value)
# 
# BeamCurrent723ProcessPlot <- subset(BeamCurrent723, Layer > 85)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(BeamCurrent723ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(BeamCurrent723ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBeam723Current <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent723New <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Beam Current/","BeamCurrent723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = BeamCurrent723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBeam723Current, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent723New, mapping = aes(x = Time, y = Value))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BeamCurrent723New$Layer)) {
#         if(BeamCurrent723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BeamCurrent723ProcessPlot$Layer)
#     tempBeam723Current <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent723New <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Beam Current/","BeamCurrent723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = tempBeam723Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BeamCurrent723New$Layer)) {
#         if(BeamCurrent723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBeam723Current <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BeamCurrent723New <- filter(BeamCurrent723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Beam Current/","BeamCurrent723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     Beam723ProcessPlot <- ggplot(data = tempBeam723Current, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BeamCurrent723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Beam Current") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Beam Current \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BeamCurrent723New$Layer)) {
#         if(BeamCurrent723New$Layer[j]==Defects723$Layer[i]) {Beam723ProcessPlot = Beam723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(Beam723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#####Combine all datasets#####
BeamCurrentAll <- rbind(BeamCurrent629, 
                        BeamCurrent703, 
                        BeamCurrent710, 
                        BeamCurrent718, 
                        BeamCurrent723)

BeamCurrentAll$Layer_ID <- paste(BeamCurrentAll$Build, "/", BeamCurrentAll$Layer)

########################### Defect: Lack of Fusion ####################################

##############################################
#####Search for "Pauses" in Beam Current #####
##############################################

##################
#### Build629 ####
##################

BeamCurrent629$Value <- as.numeric(BeamCurrent629$Value)
BeamCurrent629$Time <- as.POSIXct(BeamCurrent629$Time,format="%H:%M:%OS")

BeamCurrent629PauseData <- filter(BeamCurrent629, Layer >85)

#Create a table of values where instances of Beam Current "Pauses" Occurred
MinTimeLength <- 3
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamCurrent629PauseData$Value)) {
  if(i==1) {
    Pauses=0
    PauseCounter=0
    BeamCurrent629Pauses <- BeamCurrent629PauseData
    BeamCurrent629Pauses <- NULL
    if((BeamCurrent629PauseData$Value[i] + BeamCurrent629PauseData$Value[i+1]) < 1) {
      tempBeamPause629 <- slice(BeamCurrent629PauseData, i)}
  }
  else if (i==length(BeamCurrent629PauseData$Value)) {
    if ((BeamCurrent629PauseData$Value[i] + BeamCurrent629PauseData$Value[i-1]) < 1) {
      if (difftime(BeamCurrent629PauseData$Time[i],min(tempBeamPause629$Time), units = "secs") < MinTimeLength) {
        tempBeamPause629 <- NULL
        j = length(BeamCurrent629Pauses$Layer)}
      else {
        j=j+1
        tempBeamPause629 <- rbind(tempBeamPause629,slice(BeamCurrent629PauseData, i))
        if (Pauses == 1) {
          BeamCurrent629Pauses <- tempBeamPause629
          PauseCounter <- rep(Pauses, length(tempBeamPause629$Layer))
          Pauses = Pauses + 1
          tempBeamPause629 <- NULL}
        else {
          BeamCurrent629Pauses <- rbind(BeamCurrent629Pauses, tempBeamPause629)
          PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause629$Layer)))
          Pauses = Pauses + 1
          tempBeamPause629 <- NULL}}}
    BeamCurrent629Pauses <- cbind(BeamCurrent629Pauses, PauseCounter)}
  else {
    if ((BeamCurrent629PauseData$Value[i] + BeamCurrent629PauseData$Value[i+1]) < 1) {
      if (Pauses == 0) {
        Pauses=1
        j=1
        tempBeamPause629 <- slice(BeamCurrent629PauseData, i)}
      else {
        j=j+1
        tempBeamPause629 <- rbind(tempBeamPause629,slice(BeamCurrent629PauseData, i))}}
    else {
      if ((BeamCurrent629PauseData$Value[i] + BeamCurrent629PauseData$Value[i-1]) < 1) {
        if (difftime(BeamCurrent629PauseData$Time[i],min(tempBeamPause629$Time), units = "secs") < MinTimeLength) {
          tempBeamPause629 <- NULL
          j = length(BeamCurrent629Pauses$Layer)}
        else {
          j=j+1
          tempBeamPause629 <- rbind(tempBeamPause629,slice(BeamCurrent629PauseData, i))
          if (Pauses == 1) {
            BeamCurrent629Pauses <- tempBeamPause629
            PauseCounter <- rep(Pauses, length(tempBeamPause629$Layer))
            Pauses = Pauses + 1
            tempBeamPause629 <- NULL}
          else {
            BeamCurrent629Pauses <- rbind(BeamCurrent629Pauses, tempBeamPause629)
            PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause629$Layer)))
            Pauses = Pauses + 1
            tempBeamPause629 <- NULL}}
      }
    }
  }
}

colnames(BeamCurrent629Pauses) <- list("Time", "Variable", "Value", "Layer","Process","Build", "Pause")
BeamCurrent629Pauses <- subset(BeamCurrent629Pauses[c(6,1,2,3,4,5,7)])

#Add a column to identify if the Pause occurred near an identified defect
DefectList629 <- subset(unique(filter(OutputDescription629, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
DefectList629 <- unique(DefectList629)

DefectList629 <- filter(DefectList629, Layer != 335)

DefectPause <- rep(0, length(unique(BeamCurrent629Pauses$Pause)))
NumPauses <- seq(1:length(unique(BeamCurrent629Pauses$Pause)))

for(i in 1:length(unique(BeamCurrent629Pauses$Pause))) {
  filteredBeam <- filter(BeamCurrent629Pauses, Pause == i)
  for(j in 1:length(filteredBeam$Layer)) {
    if(filteredBeam$Layer[j] %in% DefectList629$Layer) {DefectPause[i] = 1}
    else {DefectPause[i] =0}
  }
}

KnownDefect <- cbind.data.frame(NumPauses, DefectPause)

for(i in 1:length(BeamCurrent629Pauses$Pause)) {
  for(j in 1:length(KnownDefect$NumPauses)) {
    if(BeamCurrent629Pauses$Pause[i] == KnownDefect$NumPauses[j]) {BeamCurrent629Pauses$Known[i] = KnownDefect$DefectPause[j]}    
  }
}

#Create a table which calculates the length of each Beam Current Pause
BeamCurrent629Pauses$Time <- as.POSIXct(BeamCurrent629Pauses$Time,format="%H:%M:%OS")
for(i in 1:length(unique(BeamCurrent629Pauses$Pause))) {
  if (i==1) {
    filteredBeam <- filter(BeamCurrent629Pauses, Pause == i)
    BeamPauseTimeLength629 <- difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    BeamPauseTimeLength629 <- as.data.frame(BeamPauseTimeLength629)
    
    BeamPauseTimeLength629$EndingLayer[i] <- max(filteredBeam$Layer)
    colnames(BeamPauseTimeLength629) <- list("TimeLength", "EndingLayer")
    
    if(length(unique(BeamCurrent629Pauses$Pause))==1) {
      Pause <- distinct(BeamCurrent629Pauses[c(7,8)],Pause, .keep_all = TRUE)
      BeamPauseTimeLength629 <- cbind(Pause, BeamPauseTimeLength629)
      BeamPauseTimeLength629 <- BeamPauseTimeLength629[c(1,3,4,2)]
      BeamPauseTimeLength629$Build <- "629"}
    }
  
  else if (i==length(unique(BeamCurrent629Pauses$Pause))) {
    filteredBeam <- filter(BeamCurrent629Pauses, Pause == i)
    
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    
    BeamPauseTimeLength629 <- rbind(BeamPauseTimeLength629,NextPause)
    Pause <- distinct(BeamCurrent629Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength629 <- cbind(Pause, BeamPauseTimeLength629)
    BeamPauseTimeLength629 <- BeamPauseTimeLength629[c(1,3,4,2)]
    BeamPauseTimeLength629$Build <- "629"
  }
  else {
    filteredBeam <- filter(BeamCurrent629Pauses, Pause == i)
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    BeamPauseTimeLength629 <- rbind(BeamPauseTimeLength629,NextPause)}
}

#Calculate statistics for Beam Current Pause
BeamPause629Mean <- mean(BeamPauseTimeLength629$TimeLength)
BeamPause629SD <- sd(BeamPauseTimeLength629$TimeLength)
BeamPause629Range <- range(BeamPauseTimeLength629$TimeLength)
BeamPause629Layers <- unique(BeamPauseTimeLength629$EndingLayer)
BeamPause629Layers <- BeamPause629Layers[order(BeamPause629Layers)]

#Add a column which marks if a pause occurred during the layer (instead of at the beginning)
Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
BeamCurrent629Pauses$Time <- as.POSIXct(BeamCurrent629Pauses$Time,format="%H:%M:%OS")

MinTimeLength <- 13
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamPauseTimeLength629$Pause)){
  filteredAbnormal <- filter(BeamCurrent629Pauses, Pause == i)
  if(length(unique(filteredAbnormal$Layer))==1) {
    filteredDefects <- filter(Defects629, Layer == unique(filteredAbnormal$Layer))
    if(difftime(min(filteredAbnormal$Time), filteredDefects$Time, units = "secs") > MinTimeLength) {
      BeamPauseTimeLength629$MiddleOfLayer[i] = 1
      
      #Add a column which marks if a pause occurred without a "MoveRake" process
      if(length(which(filteredAbnormal$Process=="MoveRake"))==0) {
        BeamPauseTimeLength629$NoRaking[i] = 1}
      else {BeamPauseTimeLength629$NoRaking[i] = 0}
      
    }
    else {
      BeamPauseTimeLength629$MiddleOfLayer[i] = 0
      BeamPauseTimeLength629$NoRaking[i] = 0}}
  else {
    BeamPauseTimeLength629$MiddleOfLayer[i] = 0
    BeamPauseTimeLength629$NoRaking[i] = 0}
}

#Order table by length of pause
BeamPauseTimeLength629 <- BeamPauseTimeLength629[order(-BeamPauseTimeLength629$TimeLength),]

##################
#### Build703 ####
##################

BeamCurrent703$Value <- as.numeric(BeamCurrent703$Value)
BeamCurrent703$Time <- as.POSIXct(BeamCurrent703$Time,format="%H:%M:%OS")

BeamCurrent703PauseData <- filter(BeamCurrent703, Layer >85)

#Create a table of values where instances of Beam Current "Pauses" Occurred
MinTimeLength <- 3
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamCurrent703PauseData$Value)) {
  if(i==1) {
    Pauses=0
    PauseCounter=0
    BeamCurrent703Pauses <- subset(BeamCurrent703PauseData[c(1,2,3,4)])
    BeamCurrent703Pauses <- NULL
    if((BeamCurrent703PauseData$Value[i] + BeamCurrent703PauseData$Value[i+1]) < 1) {
      tempBeamPause703 <- slice(BeamCurrent703PauseData, i)}
  }
  else if (i==length(BeamCurrent703PauseData$Value)) {
    if ((BeamCurrent703PauseData$Value[i] + BeamCurrent703PauseData$Value[i-1]) < 1) {
      if (difftime(BeamCurrent703PauseData$Time[i],min(tempBeamPause703$Time), units = "secs") < MinTimeLength) {
        tempBeamPause703 <- NULL
        j = length(BeamCurrent703Pauses$Layer)}
      else {
        j=j+1
        tempBeamPause703 <- rbind(tempBeamPause703,slice(BeamCurrent703PauseData, i))
        if (Pauses == 1) {
          BeamCurrent703Pauses <- tempBeamPause703
          PauseCounter <- rep(Pauses, length(tempBeamPause703$Layer))
          Pauses = Pauses + 1
          tempBeamPause703 <- NULL}
        else {
          BeamCurrent703Pauses <- rbind(BeamCurrent703Pauses, tempBeamPause703)
          PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause703$Layer)))
          Pauses = Pauses + 1
          tempBeamPause703 <- NULL}}}
    BeamCurrent703Pauses <- cbind(BeamCurrent703Pauses, PauseCounter)}
  else {
    if ((BeamCurrent703PauseData$Value[i] + BeamCurrent703PauseData$Value[i+1]) < 1) {
      if (Pauses == 0) {
        Pauses=1
        j=1
        tempBeamPause703 <- slice(BeamCurrent703PauseData, i)}
      else {
        j=j+1
        tempBeamPause703 <- rbind(tempBeamPause703,slice(BeamCurrent703PauseData, i))}}
    else {
      if ((BeamCurrent703PauseData$Value[i] + BeamCurrent703PauseData$Value[i-1]) < 1) {
        if (difftime(BeamCurrent703PauseData$Time[i],min(tempBeamPause703$Time), units = "secs") < MinTimeLength) {
          tempBeamPause703 <- NULL
          j = length(BeamCurrent703Pauses$Layer)}
        else {
          j=j+1
          tempBeamPause703 <- rbind(tempBeamPause703,slice(BeamCurrent703PauseData, i))
          if (Pauses == 1) {
            BeamCurrent703Pauses <- tempBeamPause703
            PauseCounter <- rep(Pauses, length(tempBeamPause703$Layer))
            Pauses = Pauses + 1
            tempBeamPause703 <- NULL}
          else {
            BeamCurrent703Pauses <- rbind(BeamCurrent703Pauses, tempBeamPause703)
            PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause703$Layer)))
            Pauses = Pauses + 1
            tempBeamPause703 <- NULL}}
      }
    }
  }
}

colnames(BeamCurrent703Pauses) <- list("Time", "Variable", "Value", "Layer","Process","Build", "Pause")
BeamCurrent703Pauses <- subset(BeamCurrent703Pauses[c(6,1,2,3,4,5,7)])

#Add a column to identify if the Pause occurred near an identified defect
DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
DefectList703 <- unique(DefectList703)

DefectList703 <- filter(DefectList703, Layer != 335)

DefectPause <- rep(0, length(unique(BeamCurrent703Pauses$Pause)))
NumPauses <- seq(1:length(unique(BeamCurrent703Pauses$Pause)))

for(i in 1:length(unique(BeamCurrent703Pauses$Pause))) {
  filteredBeam <- filter(BeamCurrent703Pauses, Pause == i)
  for(j in 1:length(filteredBeam$Layer)) {
    if(filteredBeam$Layer[j] %in% DefectList703$Layer) {DefectPause[i] = 1}
    else {DefectPause[i] =0}
  }
}

KnownDefect <- cbind.data.frame(NumPauses, DefectPause)

for(i in 1:length(BeamCurrent703Pauses$Pause)) {
  for(j in 1:length(KnownDefect$NumPauses)) {
    if(BeamCurrent703Pauses$Pause[i] == KnownDefect$NumPauses[j]) {BeamCurrent703Pauses$Known[i] = KnownDefect$DefectPause[j]}    
  }
}

#Create a table which calculates the length of each Beam Current Pause
BeamCurrent703Pauses$Time <- as.POSIXct(BeamCurrent703Pauses$Time,format="%H:%M:%OS")
for(i in 1:length(unique(BeamCurrent703Pauses$Pause))) {
  if (i==1) {
    filteredBeam <- filter(BeamCurrent703Pauses, Pause == i)
    BeamPauseTimeLength703 <- difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    BeamPauseTimeLength703 <- as.data.frame(BeamPauseTimeLength703)
    
    BeamPauseTimeLength703$EndingLayer[i] <- max(filteredBeam$Layer)
    colnames(BeamPauseTimeLength703) <- list("TimeLength", "EndingLayer")
    
    if(length(unique(BeamCurrent703Pauses$Pause))==1) {
      Pause <- distinct(BeamCurrent703Pauses[c(7,8)],Pause, .keep_all = TRUE)
      BeamPauseTimeLength703 <- cbind(Pause, BeamPauseTimeLength703)
      BeamPauseTimeLength703 <- BeamPauseTimeLength703[c(1,3,4,2)]
      BeamPauseTimeLength703$Build <- "703"}
    }
  
  else if (i==length(unique(BeamCurrent703Pauses$Pause))) {
    filteredBeam <- filter(BeamCurrent703Pauses, Pause == i)
    
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    
    BeamPauseTimeLength703 <- rbind(BeamPauseTimeLength703,NextPause)
    Pause <- distinct(BeamCurrent703Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength703 <- cbind(Pause, BeamPauseTimeLength703)
    BeamPauseTimeLength703 <- BeamPauseTimeLength703[c(1,3,4,2)]
    BeamPauseTimeLength703$Build <- "703"
  }
  else {
    filteredBeam <- filter(BeamCurrent703Pauses, Pause == i)
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    BeamPauseTimeLength703 <- rbind(BeamPauseTimeLength703,NextPause)}
}

#Calculate statistics for Beam Current Pause
BeamPause703Mean <- mean(BeamPauseTimeLength703$TimeLength)
BeamPause703SD <- sd(BeamPauseTimeLength703$TimeLength)
BeamPause703Range <- range(BeamPauseTimeLength703$TimeLength)
BeamPause703Layers <- unique(BeamPauseTimeLength703$EndingLayer)
BeamPause703Layers <- BeamPause703Layers[order(BeamPause703Layers)]

#Add a column which marks if a pause occurred during the layer (instead of at the beginning)
Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
BeamCurrent703Pauses$Time <- as.POSIXct(BeamCurrent703Pauses$Time,format="%H:%M:%OS")

MinTimeLength <- 13
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamPauseTimeLength703$Pause)){
  filteredAbnormal <- filter(BeamCurrent703Pauses, Pause == i)
  if(length(unique(filteredAbnormal$Layer))==1) {
    filteredDefects <- filter(Defects703, Layer == unique(filteredAbnormal$Layer))
    if(difftime(min(filteredAbnormal$Time), filteredDefects$Time, units = "secs") > MinTimeLength) {
      BeamPauseTimeLength703$MiddleOfLayer[i] = 1
      
      #Add a column which marks if a pause occurred without a "MoveRake" process
      if(length(which(filteredAbnormal$Process=="MoveRake"))==0) {
        BeamPauseTimeLength703$NoRaking[i] = 1}
      else {BeamPauseTimeLength703$NoRaking[i] = 0}
      
    }
    else {
      BeamPauseTimeLength703$MiddleOfLayer[i] = 0
      BeamPauseTimeLength703$NoRaking[i] = 0}}
  else {
    BeamPauseTimeLength703$MiddleOfLayer[i] = 0
    BeamPauseTimeLength703$NoRaking[i] = 0}
}

#Order table by length of pause
BeamPauseTimeLength703 <- BeamPauseTimeLength703[order(-BeamPauseTimeLength703$TimeLength),]


##################
#### Build710 ####
##################

BeamCurrent710$Value <- as.numeric(BeamCurrent710$Value)
BeamCurrent710$Time <- as.POSIXct(BeamCurrent710$Time,format="%H:%M:%OS")

BeamCurrent710PauseData <- filter(BeamCurrent710, Layer >85)

#Create a table of values where instances of Beam Current "Pauses" Occurred
MinTimeLength <- 3
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamCurrent710PauseData$Value)) {
  if(i==1) {
    Pauses=0
    PauseCounter=0
    BeamCurrent710Pauses <- BeamCurrent710PauseData
    BeamCurrent710Pauses <- NULL
    if((BeamCurrent710PauseData$Value[i] + BeamCurrent710PauseData$Value[i+1]) < 1) {
      tempBeamPause710 <- slice(BeamCurrent710PauseData, i)}
  }
  else if (i==length(BeamCurrent710PauseData$Value)) {
    if ((BeamCurrent710PauseData$Value[i] + BeamCurrent710PauseData$Value[i-1]) < 1) {
      if (difftime(BeamCurrent710PauseData$Time[i],min(tempBeamPause710$Time), units = "secs") < MinTimeLength) {
        tempBeamPause710 <- NULL
        j = length(BeamCurrent710Pauses$Layer)}
      else {
        j=j+1
        tempBeamPause710 <- rbind(tempBeamPause710,slice(BeamCurrent710PauseData, i))
        if (Pauses == 1) {
          BeamCurrent710Pauses <- tempBeamPause710
          PauseCounter <- rep(Pauses, length(tempBeamPause710$Layer))
          Pauses = Pauses + 1
          tempBeamPause710 <- NULL}
        else {
          BeamCurrent710Pauses <- rbind(BeamCurrent710Pauses, tempBeamPause710)
          PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause710$Layer)))
          Pauses = Pauses + 1
          tempBeamPause710 <- NULL}}}
    BeamCurrent710Pauses <- cbind(BeamCurrent710Pauses, PauseCounter)}
  else {
    if ((BeamCurrent710PauseData$Value[i] + BeamCurrent710PauseData$Value[i+1]) < 1) {
      if (Pauses == 0) {
        Pauses=1
        j=1
        tempBeamPause710 <- slice(BeamCurrent710PauseData, i)}
      else {
        j=j+1
        tempBeamPause710 <- rbind(tempBeamPause710,slice(BeamCurrent710PauseData, i))}}
    else {
      if ((BeamCurrent710PauseData$Value[i] + BeamCurrent710PauseData$Value[i-1]) < 1) {
        if (difftime(BeamCurrent710PauseData$Time[i],min(tempBeamPause710$Time), units = "secs") < MinTimeLength) {
          tempBeamPause710 <- NULL
          j = length(BeamCurrent710Pauses$Layer)}
        else {
          j=j+1
          tempBeamPause710 <- rbind(tempBeamPause710,slice(BeamCurrent710PauseData, i))
          if (Pauses == 1) {
            BeamCurrent710Pauses <- tempBeamPause710
            PauseCounter <- rep(Pauses, length(tempBeamPause710$Layer))
            Pauses = Pauses + 1
            tempBeamPause710 <- NULL}
          else {
            BeamCurrent710Pauses <- rbind(BeamCurrent710Pauses, tempBeamPause710)
            PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause710$Layer)))
            Pauses = Pauses + 1
            tempBeamPause710 <- NULL}}
      }
    }
  }
}

colnames(BeamCurrent710Pauses) <- list("Time", "Variable", "Value", "Layer","Process","Build", "Pause")
BeamCurrent710Pauses <- subset(BeamCurrent710Pauses[c(6,1,2,3,4,5,7)])

#Add a column to identify if the Pause occurred near an identified defect
DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
DefectList710 <- unique(DefectList710)
DefectList710 <- rbind(DefectList710, 104, 191)

DefectList710 <- filter(DefectList710, Layer != 335)

DefectPause <- rep(0, length(unique(BeamCurrent710Pauses$Pause)))
NumPauses <- seq(1:length(unique(BeamCurrent710Pauses$Pause)))

for(i in 1:length(unique(BeamCurrent710Pauses$Pause))) {
  filteredBeam <- filter(BeamCurrent710Pauses, Pause == i)
  for(j in 1:length(filteredBeam$Layer)) {
    if(filteredBeam$Layer[j] %in% DefectList710$Layer) {DefectPause[i] = 1}
    else {DefectPause[i] =0}
  }
}

KnownDefect <- cbind.data.frame(NumPauses, DefectPause)

for(i in 1:length(BeamCurrent710Pauses$Pause)) {
  for(j in 1:length(KnownDefect$NumPauses)) {
    if(BeamCurrent710Pauses$Pause[i] == KnownDefect$NumPauses[j]) {BeamCurrent710Pauses$Known[i] = KnownDefect$DefectPause[j]}    
  }
}

#Create a table which calculates the length of each Beam Current Pause
BeamCurrent710Pauses$Time <- as.POSIXct(BeamCurrent710Pauses$Time,format="%H:%M:%OS")
for(i in 1:length(unique(BeamCurrent710Pauses$Pause))) {
  if (i==1) {
    filteredBeam <- filter(BeamCurrent710Pauses, Pause == i)
    BeamPauseTimeLength710 <- difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    BeamPauseTimeLength710 <- as.data.frame(BeamPauseTimeLength710)
    
    BeamPauseTimeLength710$EndingLayer[i] <- max(filteredBeam$Layer)
    colnames(BeamPauseTimeLength710) <- list("TimeLength", "EndingLayer")
    
    if(length(unique(BeamCurrent710Pauses$Pause))==1) {
      Pause <- distinct(BeamCurrent710Pauses[c(7,8)],Pause, .keep_all = TRUE)
      BeamPauseTimeLength710 <- cbind(Pause, BeamPauseTimeLength710)
      BeamPauseTimeLength710 <- BeamPauseTimeLength710[c(1,3,4,2)]
      BeamPauseTimeLength710$Build <- "710"}
    }
  
  else if (i==length(unique(BeamCurrent710Pauses$Pause))) {
    filteredBeam <- filter(BeamCurrent710Pauses, Pause == i)
    
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    
    BeamPauseTimeLength710 <- rbind(BeamPauseTimeLength710,NextPause)
    Pause <- distinct(BeamCurrent710Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength710 <- cbind(Pause, BeamPauseTimeLength710)
    BeamPauseTimeLength710 <- BeamPauseTimeLength710[c(1,3,4,2)]
    BeamPauseTimeLength710$Build <- "710"
  }
  else {
    filteredBeam <- filter(BeamCurrent710Pauses, Pause == i)
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    BeamPauseTimeLength710 <- rbind(BeamPauseTimeLength710,NextPause)}
}

#Calculate statistics for Beam Current Pause
BeamPause710Mean <- mean(BeamPauseTimeLength710$TimeLength)
BeamPause710SD <- sd(BeamPauseTimeLength710$TimeLength)
BeamPause710Range <- range(BeamPauseTimeLength710$TimeLength)
BeamPause710Layers <- unique(BeamPauseTimeLength710$EndingLayer)
BeamPause710Layers <- BeamPause710Layers[order(BeamPause710Layers)]

#Add a column which marks if a pause occurred during the layer (instead of at the beginning)
Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
BeamCurrent710Pauses$Time <- as.POSIXct(BeamCurrent710Pauses$Time,format="%H:%M:%OS")

MinTimeLength <- 13
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamPauseTimeLength710$Pause)){
  filteredAbnormal <- filter(BeamCurrent710Pauses, Pause == i)
  if(length(unique(filteredAbnormal$Layer))==1) {
    filteredDefects <- filter(Defects710, Layer == unique(filteredAbnormal$Layer))
    if(difftime(min(filteredAbnormal$Time), filteredDefects$Time, units = "secs") > MinTimeLength) {
      BeamPauseTimeLength710$MiddleOfLayer[i] = 1
      
      #Add a column which marks if a pause occurred without a "MoveRake" process
      if(length(which(filteredAbnormal$Process=="MoveRake"))==0) {
        BeamPauseTimeLength710$NoRaking[i] = 1}
      else {BeamPauseTimeLength710$NoRaking[i] = 0}
      
    }
    else {
      BeamPauseTimeLength710$MiddleOfLayer[i] = 0
      BeamPauseTimeLength710$NoRaking[i] = 0}}
  else {
    BeamPauseTimeLength710$MiddleOfLayer[i] = 0
    BeamPauseTimeLength710$NoRaking[i] = 0}
}

#Order table by length of pause
BeamPauseTimeLength710 <- BeamPauseTimeLength710[order(-BeamPauseTimeLength710$TimeLength),]

##################
#### Build718 ####
##################

BeamCurrent718$Value <- as.numeric(BeamCurrent718$Value)
BeamCurrent718$Time <- as.POSIXct(BeamCurrent718$Time,format="%H:%M:%OS")

BeamCurrent718PauseData <- filter(BeamCurrent718, Layer >85)

#Create a table of values where instances of Beam Current "Pauses" Occurred
MinTimeLength <- 3
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamCurrent718PauseData$Value)) {
  if(i==1) {
    Pauses=0
    PauseCounter=0
    BeamCurrent718Pauses <- BeamCurrent718PauseData
    BeamCurrent718Pauses <- NULL
    if((BeamCurrent718PauseData$Value[i] + BeamCurrent718PauseData$Value[i+1]) < 1) {
      tempBeamPause718 <- slice(BeamCurrent718PauseData, i)}
  }
  else if (i==length(BeamCurrent718PauseData$Value)) {
    if ((BeamCurrent718PauseData$Value[i] + BeamCurrent718PauseData$Value[i-1]) < 1) {
      if (difftime(BeamCurrent718PauseData$Time[i],min(tempBeamPause718$Time), units = "secs") < MinTimeLength) {
        tempBeamPause718 <- NULL
        j = length(BeamCurrent718Pauses$Layer)}
      else {
        j=j+1
        tempBeamPause718 <- rbind(tempBeamPause718,slice(BeamCurrent718PauseData, i))
        if (Pauses == 1) {
          BeamCurrent718Pauses <- tempBeamPause718
          PauseCounter <- rep(Pauses, length(tempBeamPause718$Layer))
          Pauses = Pauses + 1
          tempBeamPause718 <- NULL}
        else {
          BeamCurrent718Pauses <- rbind(BeamCurrent718Pauses, tempBeamPause718)
          PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause718$Layer)))
          Pauses = Pauses + 1
          tempBeamPause718 <- NULL}}}
    BeamCurrent718Pauses <- cbind(BeamCurrent718Pauses, PauseCounter)}
  else {
    if ((BeamCurrent718PauseData$Value[i] + BeamCurrent718PauseData$Value[i+1]) < 1) {
      if (Pauses == 0) {
        Pauses=1
        j=1
        tempBeamPause718 <- slice(BeamCurrent718PauseData, i)}
      else {
        j=j+1
        tempBeamPause718 <- rbind(tempBeamPause718,slice(BeamCurrent718PauseData, i))}}
    else {
      if ((BeamCurrent718PauseData$Value[i] + BeamCurrent718PauseData$Value[i-1]) < 1) {
        if (difftime(BeamCurrent718PauseData$Time[i],min(tempBeamPause718$Time), units = "secs") < MinTimeLength) {
          tempBeamPause718 <- NULL
          j = length(BeamCurrent718Pauses$Layer)}
        else {
          j=j+1
          tempBeamPause718 <- rbind(tempBeamPause718,slice(BeamCurrent718PauseData, i))
          if (Pauses == 1) {
            BeamCurrent718Pauses <- tempBeamPause718
            PauseCounter <- rep(Pauses, length(tempBeamPause718$Layer))
            Pauses = Pauses + 1
            tempBeamPause718 <- NULL}
          else {
            BeamCurrent718Pauses <- rbind(BeamCurrent718Pauses, tempBeamPause718)
            PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause718$Layer)))
            Pauses = Pauses + 1
            tempBeamPause718 <- NULL}}
      }
    }
  }
}

colnames(BeamCurrent718Pauses) <- list("Time", "Variable", "Value", "Layer","Process","Build", "Pause")
BeamCurrent718Pauses <- subset(BeamCurrent718Pauses[c(6,1,2,3,4,5,7)])

#Add a column to identify if the Pause occurred near an identified defect
DefectList718 <- subset(unique(filter(OutputDescription718, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
DefectList718 <- unique(DefectList718)

DefectList718 <- filter(DefectList718, Layer != 335)

DefectPause <- rep(0, length(unique(BeamCurrent718Pauses$Pause)))
NumPauses <- seq(1:length(unique(BeamCurrent718Pauses$Pause)))

for(i in 1:length(unique(BeamCurrent718Pauses$Pause))) {
  filteredBeam <- filter(BeamCurrent718Pauses, Pause == i)
  for(j in 1:length(filteredBeam$Layer)) {
    if(filteredBeam$Layer[j] %in% DefectList718$Layer) {DefectPause[i] = 1}
    else {DefectPause[i] =0}
  }
}

KnownDefect <- cbind.data.frame(NumPauses, DefectPause)

for(i in 1:length(BeamCurrent718Pauses$Pause)) {
  for(j in 1:length(KnownDefect$NumPauses)) {
    if(BeamCurrent718Pauses$Pause[i] == KnownDefect$NumPauses[j]) {BeamCurrent718Pauses$Known[i] = KnownDefect$DefectPause[j]}    
  }
}

#Create a table which calculates the length of each Beam Current Pause
BeamCurrent718Pauses$Time <- as.POSIXct(BeamCurrent718Pauses$Time,format="%H:%M:%OS")
for(i in 1:length(unique(BeamCurrent718Pauses$Pause))) {
  if (i==1) {
    filteredBeam <- filter(BeamCurrent718Pauses, Pause == i)
    BeamPauseTimeLength718 <- difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    BeamPauseTimeLength718 <- as.data.frame(BeamPauseTimeLength718)
    
    BeamPauseTimeLength718$EndingLayer[i] <- max(filteredBeam$Layer)
    colnames(BeamPauseTimeLength718) <- list("TimeLength", "EndingLayer")
  
  if(length(unique(BeamCurrent718Pauses$Pause))==1) {
    Pause <- distinct(BeamCurrent718Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength718 <- cbind(Pause, BeamPauseTimeLength718)
    BeamPauseTimeLength718 <- BeamPauseTimeLength718[c(1,3,4,2)]
    BeamPauseTimeLength718$Build <- "718"}
  }
  else if (i==length(unique(BeamCurrent718Pauses$Pause))) {
    filteredBeam <- filter(BeamCurrent718Pauses, Pause == i)
    
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    
    BeamPauseTimeLength718 <- rbind(BeamPauseTimeLength718,NextPause)
    Pause <- distinct(BeamCurrent718Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength718 <- cbind(Pause, BeamPauseTimeLength718)
    BeamPauseTimeLength718 <- BeamPauseTimeLength718[c(1,3,4,2)]
    BeamPauseTimeLength718$Build <- "718"
  }
  else {
    filteredBeam <- filter(BeamCurrent718Pauses, Pause == i)
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    BeamPauseTimeLength718 <- rbind(BeamPauseTimeLength718,NextPause)}
}

#Calculate statistics for Beam Current Pause
BeamPause718Mean <- mean(BeamPauseTimeLength718$TimeLength)
BeamPause718SD <- sd(BeamPauseTimeLength718$TimeLength)
BeamPause718Range <- range(BeamPauseTimeLength718$TimeLength)
BeamPause718Layers <- unique(BeamPauseTimeLength718$EndingLayer)
BeamPause718Layers <- BeamPause718Layers[order(BeamPause718Layers)]

#Add a column which marks if a pause occurred during the layer (instead of at the beginning)
Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
BeamCurrent718Pauses$Time <- as.POSIXct(BeamCurrent718Pauses$Time,format="%H:%M:%OS")

MinTimeLength <- 13
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamPauseTimeLength718$Pause)){
  filteredAbnormal <- filter(BeamCurrent718Pauses, Pause == i)
  if(length(unique(filteredAbnormal$Layer))==1) {
    filteredDefects <- filter(Defects718, Layer == unique(filteredAbnormal$Layer))
    if(difftime(min(filteredAbnormal$Time), filteredDefects$Time, units = "secs") > MinTimeLength) {
      BeamPauseTimeLength718$MiddleOfLayer[i] = 1
      
      #Add a column which marks if a pause occurred without a "MoveRake" process
      if(length(which(filteredAbnormal$Process=="MoveRake"))==0) {
        BeamPauseTimeLength718$NoRaking[i] = 1}
      else {BeamPauseTimeLength718$NoRaking[i] = 0}
      
    }
    else {
      BeamPauseTimeLength718$MiddleOfLayer[i] = 0
      BeamPauseTimeLength718$NoRaking[i] = 0}}
  else {
    BeamPauseTimeLength718$MiddleOfLayer[i] = 0
    BeamPauseTimeLength718$NoRaking[i] = 0}
}

#Order table by length of pause
BeamPauseTimeLength718 <- BeamPauseTimeLength718[order(-BeamPauseTimeLength718$TimeLength),]

##################
#### Build723 ####
##################

BeamCurrent723$Value <- as.numeric(BeamCurrent723$Value)
BeamCurrent723$Time <- as.POSIXct(BeamCurrent723$Time,format="%H:%M:%OS")

BeamCurrent723PauseData <- filter(BeamCurrent723, Layer >85)

#Create a table of values where instances of Beam Current "Pauses" Occurred
MinTimeLength <- 3
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamCurrent723PauseData$Value)) {
  if(i==1) {
    Pauses=0
    PauseCounter=0
    BeamCurrent723Pauses <- BeamCurrent723PauseData
    BeamCurrent723Pauses <- NULL
    if((BeamCurrent723PauseData$Value[i] + BeamCurrent723PauseData$Value[i+1]) < 1) {
      tempBeamPause723 <- slice(BeamCurrent723PauseData, i)}
  }
  else if (i==length(BeamCurrent723PauseData$Value)) {
    if ((BeamCurrent723PauseData$Value[i] + BeamCurrent723PauseData$Value[i-1]) < 1) {
      if (difftime(BeamCurrent723PauseData$Time[i],min(tempBeamPause723$Time), units = "secs") < MinTimeLength) {
        tempBeamPause723 <- NULL
        j = length(BeamCurrent723Pauses$Layer)}
      else {
        j=j+1
        tempBeamPause723 <- rbind(tempBeamPause723,slice(BeamCurrent723PauseData, i))
        if (Pauses == 1) {
          BeamCurrent723Pauses <- tempBeamPause723
          PauseCounter <- rep(Pauses, length(tempBeamPause723$Layer))
          Pauses = Pauses + 1
          tempBeamPause723 <- NULL}
        else {
          BeamCurrent723Pauses <- rbind(BeamCurrent723Pauses, tempBeamPause723)
          PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause723$Layer)))
          Pauses = Pauses + 1
          tempBeamPause723 <- NULL}}}
    BeamCurrent723Pauses <- cbind(BeamCurrent723Pauses, PauseCounter)}
  else {
    if ((BeamCurrent723PauseData$Value[i] + BeamCurrent723PauseData$Value[i+1]) < 1) {
      if (Pauses == 0) {
        Pauses=1
        j=1
        tempBeamPause723 <- slice(BeamCurrent723PauseData, i)}
      else {
        j=j+1
        tempBeamPause723 <- rbind(tempBeamPause723,slice(BeamCurrent723PauseData, i))}}
    else {
      if ((BeamCurrent723PauseData$Value[i] + BeamCurrent723PauseData$Value[i-1]) < 1) {
        if (difftime(BeamCurrent723PauseData$Time[i],min(tempBeamPause723$Time), units = "secs") < MinTimeLength) {
          tempBeamPause723 <- NULL
          j = length(BeamCurrent723Pauses$Layer)}
        else {
          j=j+1
          tempBeamPause723 <- rbind(tempBeamPause723,slice(BeamCurrent723PauseData, i))
          if (Pauses == 1) {
            BeamCurrent723Pauses <- tempBeamPause723
            PauseCounter <- rep(Pauses, length(tempBeamPause723$Layer))
            Pauses = Pauses + 1
            tempBeamPause723 <- NULL}
          else {
            BeamCurrent723Pauses <- rbind(BeamCurrent723Pauses, tempBeamPause723)
            PauseCounter <- append(PauseCounter,rep(Pauses, length(tempBeamPause723$Layer)))
            Pauses = Pauses + 1
            tempBeamPause723 <- NULL}}
      }
    }
  }
}

colnames(BeamCurrent723Pauses) <- list("Time", "Variable", "Value", "Layer","Process","Build", "Pause")
BeamCurrent723Pauses <- subset(BeamCurrent723Pauses[c(6,1,2,3,4,5,7)])

#Add a column to identify if the Pause occurred near an identified defect
DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
DefectList723 <- unique(DefectList723)

DefectList723 <- filter(DefectList723, Layer != 335)

DefectPause <- rep(0, length(unique(BeamCurrent723Pauses$Pause)))
NumPauses <- seq(1:length(unique(BeamCurrent723Pauses$Pause)))

for(i in 1:length(unique(BeamCurrent723Pauses$Pause))) {
  filteredBeam <- filter(BeamCurrent723Pauses, Pause == i)
  for(j in 1:length(filteredBeam$Layer)) {
    if(filteredBeam$Layer[j] %in% DefectList723$Layer) {DefectPause[i] = 1}
    else {DefectPause[i] =0}
  }
}

KnownDefect <- cbind.data.frame(NumPauses, DefectPause)

for(i in 1:length(BeamCurrent723Pauses$Pause)) {
  for(j in 1:length(KnownDefect$NumPauses)) {
    if(BeamCurrent723Pauses$Pause[i] == KnownDefect$NumPauses[j]) {BeamCurrent723Pauses$Known[i] = KnownDefect$DefectPause[j]}    
  }
}

#Create a table which calculates the length of each Beam Current Pause
BeamCurrent723Pauses$Time <- as.POSIXct(BeamCurrent723Pauses$Time,format="%H:%M:%OS")
for(i in 1:length(unique(BeamCurrent723Pauses$Pause))) {
  if (i==1) {
    filteredBeam <- filter(BeamCurrent723Pauses, Pause == i)
    BeamPauseTimeLength723 <- difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    BeamPauseTimeLength723 <- as.data.frame(BeamPauseTimeLength723)
    
    BeamPauseTimeLength723$EndingLayer[i] <- max(filteredBeam$Layer)
    colnames(BeamPauseTimeLength723) <- list("TimeLength", "EndingLayer")
    
    if(length(unique(BeamCurrent723Pauses$Pause))==1) {
      Pause <- distinct(BeamCurrent723Pauses[c(7,8)],Pause, .keep_all = TRUE)
      BeamPauseTimeLength723 <- cbind(Pause, BeamPauseTimeLength723)
      BeamPauseTimeLength723 <- BeamPauseTimeLength723[c(1,3,4,2)]
      BeamPauseTimeLength723$Build <- "723"}
    }
  
  else if (i==length(unique(BeamCurrent723Pauses$Pause))) {
    filteredBeam <- filter(BeamCurrent723Pauses, Pause == i)
    
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    
    BeamPauseTimeLength723 <- rbind(BeamPauseTimeLength723,NextPause)
    Pause <- distinct(BeamCurrent723Pauses[c(7,8)],Pause, .keep_all = TRUE)
    BeamPauseTimeLength723 <- cbind(Pause, BeamPauseTimeLength723)
    BeamPauseTimeLength723 <- BeamPauseTimeLength723[c(1,3,4,2)]
    BeamPauseTimeLength723$Build <- "723"
  }
  else {
    filteredBeam <- filter(BeamCurrent723Pauses, Pause == i)
    NextPauseLength <-  difftime(max(filteredBeam$Time),min(filteredBeam$Time), units = "secs")
    NextLayer <- max(filteredBeam$Layer)
    NextPause <- cbind(NextPauseLength,NextLayer)
    colnames(NextPause) <- list("TimeLength", "EndingLayer")
    BeamPauseTimeLength723 <- rbind(BeamPauseTimeLength723,NextPause)}
}

#Calculate statistics for Beam Current Pause
BeamPause723Mean <- mean(BeamPauseTimeLength723$TimeLength)
BeamPause723SD <- sd(BeamPauseTimeLength723$TimeLength)
BeamPause723Range <- range(BeamPauseTimeLength723$TimeLength)
BeamPause723Layers <- unique(BeamPauseTimeLength723$EndingLayer)
BeamPause723Layers <- BeamPause723Layers[order(BeamPause723Layers)]

#Add a column which marks if a pause occurred during the layer (instead of at the beginning)
Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
BeamCurrent723Pauses$Time <- as.POSIXct(BeamCurrent723Pauses$Time,format="%H:%M:%OS")

MinTimeLength <- 13
MinTimeLength <- as.difftime(MinTimeLength, format = "%S", units = "secs")

for(i in 1:length(BeamPauseTimeLength723$Pause)){
  filteredAbnormal <- filter(BeamCurrent723Pauses, Pause == i)
  if(length(unique(filteredAbnormal$Layer))==1) {
    filteredDefects <- filter(Defects723, Layer == unique(filteredAbnormal$Layer))
    if(difftime(min(filteredAbnormal$Time), filteredDefects$Time, units = "secs") > MinTimeLength) {
      BeamPauseTimeLength723$MiddleOfLayer[i] = 1
      
      #Add a column which marks if a pause occurred without a "MoveRake" process
      if(length(which(filteredAbnormal$Process=="MoveRake"))==0) {
        BeamPauseTimeLength723$NoRaking[i] = 1}
      else {BeamPauseTimeLength723$NoRaking[i] = 0}
      
    }
    else {
      BeamPauseTimeLength723$MiddleOfLayer[i] = 0
      BeamPauseTimeLength723$NoRaking[i] = 0}}
  else {
    BeamPauseTimeLength723$MiddleOfLayer[i] = 0
    BeamPauseTimeLength723$NoRaking[i] = 0}
}

#Order table by length of pause
BeamPauseTimeLength723 <- BeamPauseTimeLength723[order(-BeamPauseTimeLength723$TimeLength),]

#####Combine all datasets#####
BeamCurrentPausesAll <- rbind(BeamCurrent629Pauses, 
                              BeamCurrent703Pauses, 
                              BeamCurrent710Pauses, 
                              BeamCurrent718Pauses, 
                              BeamCurrent723Pauses)

#####Combine all datasets#####
BeamPauseTimeLengthAll <- rbind(BeamPauseTimeLength629, 
                                BeamPauseTimeLength703, 
                                BeamPauseTimeLength710, 
                                BeamPauseTimeLength718, 
                                BeamPauseTimeLength723)

BeamPauseTimeLengthAll <- BeamPauseTimeLengthAll[order(-BeamPauseTimeLengthAll$TimeLength),]

##Explore##
####Create a table of Beam Current Pauses that were "Abnormal" (occurred in the middle of a layer)####
## And that have NOT been detected yet##
AbnormalPauseLengthsAll <- filter(BeamPauseTimeLengthAll, MiddleOfLayer == 1 & Known==0)
AbnormalPauseLengthsAll <- AbnormalPauseLengthsAll[order(-AbnormalPauseLengthsAll$TimeLength),]
AbnormalPauseLengthsAll$Layer_ID <- paste(AbnormalPauseLengthsAll$Build, "/", AbnormalPauseLengthsAll$EndingLayer)

DefectsListNew <- unique(subset(AbnormalPauseLengthsAll[c(3,5)]))
colnames(DefectsListNew) <- list("Layer", "Date")

#######################################
#Create a histogram of Pause Lengths
BeamPauseTimeLengthAll$TimeLength <- as.numeric(BeamPauseTimeLengthAll$TimeLength)

hist(BeamPauseTimeLengthAll$TimeLength, main= "Histogram of Beam Current Pause Time Lengths", xlab = "Pause Time Length (secs)")

BeamPauseAllMean <- mean(BeamPauseTimeLengthAll$TimeLength)
BeamPauseAllSD <- sd(BeamPauseTimeLengthAll$TimeLength)
BeamPauseAllVar <- var(BeamPauseTimeLengthAll$TimeLength)

BeamPauseAllMedian <- median(BeamPauseTimeLengthAll$TimeLength)
BeamPauseAllMode <- mfv(BeamPauseTimeLengthAll$TimeLength)

BeamPauseAllRange <- range(BeamPauseTimeLengthAll$TimeLength)
BeamPauseAllQuartile <- quantile(BeamPauseTimeLengthAll$TimeLength)
BeamPauseAllLayers <- unique(BeamPauseTimeLengthAll$EndingLayer)
BeamPauseAllLayers <- BeamPauseAllLayers[order(BeamPauseAllLayers)]

#Create a list of outlier
PauseOutliers <- boxplot(BeamPauseTimeLengthAll$TimeLength)$out
PauseOutliersList <-which(BeamPauseTimeLengthAll$TimeLength %in% PauseOutliers)

#Create a dataset of outleirs
for(i in 1:length(BeamPauseTimeLengthAll$TimeLength)) {
  if(i==PauseOutliersList[1]) {PartBeamPauseOutlier <- slice(BeamPauseTimeLengthAll,i)}
  else if(i %in% PauseOutliersList) {PartBeamPauseOutlier <-rbind(PartBeamPauseOutlier, slice(BeamPauseTimeLengthAll,i))}
}

#######################################
#Remove pauses which occurred during "Build Stopped" or "Build Crashed"
BeamCurrentPausesNew <- filter(BeamCurrentPausesAll, Process != "BuildStopped" & Process != "BuildCrashed")
BuildStopCrashed <- filter(BeamCurrentPausesAll, Process == "BuildStopped" | Process == "BuildCrashed")
BuildPause <- subset(BuildStopCrashed[c(1,7)])
BuildPause <- unique(BuildPause)

Count=0
for(i in 1:length(BeamPauseTimeLengthAll$TimeLength)) {
  for(j in 1:length(BuildPause$Pause)) {
    if(BeamPauseTimeLengthAll$Build[i] == BuildPause$Build[j] & BeamPauseTimeLengthAll$Pause[i] == BuildPause$Pause[j]) {
      if(Count==0){
        tempBeamPauseTimeLengthAll <- filter(BeamPauseTimeLengthAll, Build != BuildPause$Build[j] | Pause != BuildPause$Pause[j])
        Count=Count+1}
      else{
        tempBeamPauseTimeLengthAll <- filter(tempBeamPauseTimeLengthAll, Build != BuildPause$Build[j] | Pause != BuildPause$Pause[j])
        Count=Count+1}}
  }
}

BeamPauseTimeLengthNew <- tempBeamPauseTimeLengthAll

#Create a histogram of Pause Lengths
BeamPauseTimeLengthNew$TimeLength <- as.numeric(BeamPauseTimeLengthNew$TimeLength)

hist(BeamPauseTimeLengthNew$TimeLength, main= "Histogram of Beam Current Pause Time Lengths", xlab = "Pause Time Length (secs)")

BeamPauseAllMean <- mean(BeamPauseTimeLengthNew$TimeLength)
BeamPauseAllSD <- sd(BeamPauseTimeLengthNew$TimeLength)
BeamPauseAllVar <- var(BeamPauseTimeLengthNew$TimeLength)

BeamPauseAllMedian <- median(BeamPauseTimeLengthNew$TimeLength)
BeamPauseAllMode <- mfv(BeamPauseTimeLengthNew$TimeLength)

BeamPauseAllRange <- range(BeamPauseTimeLengthNew$TimeLength)
BeamPauseAllQuartile <- quantile(BeamPauseTimeLengthNew$TimeLength)
BeamPauseAllLayers <- unique(BeamPauseTimeLengthNew$EndingLayer)
BeamPauseAllLayers <- BeamPauseAllLayers[order(BeamPauseAllLayers)]

quantile(BeamPauseTimeLengthNew$TimeLength, c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1))

#Create a list of outlier
PauseOutliers <- boxplot(BeamPauseTimeLengthNew$TimeLength)$out
PauseOutliersList <-which(BeamPauseTimeLengthNew$TimeLength %in% PauseOutliers)

#Create a dataset of outleirs
for(i in 1:length(BeamPauseTimeLengthNew$TimeLength)) {
  if(i==PauseOutliersList[1]) {PartBeamPauseOutlier <- slice(BeamPauseTimeLengthNew,i)}
  else if(i %in% PauseOutliersList) {PartBeamPauseOutlier <-rbind(PartBeamPauseOutlier, slice(BeamPauseTimeLengthNew,i))}
}

# #######################################
# #Remove Outliers
# BeamPauseTimeLengthNoOutliers <- BeamPauseTimeLengthNew[-c(1,431,432,433,434,435,436,437,438,439,440,441,442,443,444),]
# hist(BeamPauseTimeLengthNoOutliers$TimeLength, main= "Histogram of Beam Current Pause Time Lengths", xlab = "Pause Time Length")
# 
# BeamPauseNoOutliersMean <- mean(BeamPauseTimeLengthNoOutliers$TimeLength)
# BeamPauseNoOutliersSD <- sd(BeamPauseTimeLengthNoOutliers$TimeLength)
# BeamPauseNoOutliersVar <- var(BeamPauseTimeLengthNoOutliers$TimeLength)
# 
# BeamPauseNoOutliersMedian <- median(BeamPauseTimeLengthNoOutliers$TimeLength)
# BeamPauseNoOutliersMode <- mfv(BeamPauseTimeLengthNoOutliers$TimeLength)
# 
# BeamPauseNoOutliersRange <- range(BeamPauseTimeLengthNoOutliers$TimeLength)
# BeamPauseNoOutliersQuartile <- quantile(BeamPauseTimeLengthNoOutliers$TimeLength)
# BeamPauseNoOutliersLayers <- unique(BeamPauseTimeLengthNoOutliers$EndingLayer)
# BeamPauseNoOutliersLayers <- BeamPauseAllLayers[order(BeamPauseAllLayers)]
# 
# 
# library(fitdistrplus)
# descdist(BeamPauseTimeLengthNoOutliers$TimeLength, discrete = FALSE)

###########################################################################################################################

############################### Defect: Grid Voltage Error #######################################

###############################################
############## High Voltage Grid ##############
###############################################

##################
#### Build629 ####
##################

HighVoltageGrid629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.Grid")

#Add layer to High Voltage Grid and create Plot
#Add layer to categorize
Time_new <- HighVoltageGrid629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

HighVoltageGrid629$Layer <- Layer_new
colnames(HighVoltageGrid629) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageGrid629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageGrid629 <- subset(HighVoltageGrid629, Layer > 80 & Layer < max(Defects629Layer))
head(HighVoltageGrid629)

### Assign processes within each layer
HighVoltageGrid629Time <- HighVoltageGrid629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(HighVoltageGrid629$Value))

for(i in 1:length(HighVoltageGrid629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (HighVoltageGrid629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (HighVoltageGrid629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (HighVoltageGrid629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (HighVoltageGrid629Time[i] >= Process629Time[j] && HighVoltageGrid629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
HighVoltageGrid629$Process <- Process629
colnames(HighVoltageGrid629) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageGrid629Freq <- subset(HighVoltageGrid629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageGrid629Freq <- subset(tempHighVoltageGrid629Freq, tempHighVoltageGrid629Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageGrid629Freq==Process629[i]))
    }
  }
}

ProcessFreqHighVoltageGrid <- cbind.data.frame(Layer, Process629 , N)
SummaryHighVoltageGridFreq <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=sum)
colnames(SummaryHighVoltageGridFreq) <- c("Process", "Frequency")

SummaryHighVoltageGridMean <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=mean)
colnames(SummaryHighVoltageGridMean) <- c("Process", "Average")

SummaryHighVoltageGrid629 <- cbind.data.frame(SummaryHighVoltageGridFreq, SummaryHighVoltageGridMean [c(2)])
SummaryHighVoltageGrid629 <- SummaryHighVoltageGrid629[order(-SummaryHighVoltageGrid629$Frequency),]

# #Plotting around different layers
# HighVoltageGrid629$Time <- as.POSIXct(HighVoltageGrid629$Time,format="%H:%M:%OS")
# HighVoltageGrid629$Value <- as.numeric(HighVoltageGrid629$Value)
# 
# HVGrid629ProcessPlot <- subset(HighVoltageGrid629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(HVGrid629ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(HVGrid629ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid629 <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid629New <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Grid/","HVGrid629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid629ProcessPlot <- ggplot(data = HighVoltageGrid629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHighVoltageGrid629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid629New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(HighVoltageGrid629New$Layer)) {
#         if(HighVoltageGrid629New$Layer[j]==Defects629$Layer[i]) {HighVoltageGrid629ProcessPlot = HighVoltageGrid629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid629ProcessPlot$Layer)
#     tempHighVoltageGrid629 <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid629New <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Grid/","HVGrid629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid629ProcessPlot <- ggplot(data = tempHighVoltageGrid629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid629New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(HighVoltageGrid629New$Layer)) {
#         if(HighVoltageGrid629New$Layer[j]==Defects629$Layer[i]) {HighVoltageGrid629ProcessPlot = HighVoltageGrid629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid629 <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid629New <- filter(HVGrid629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Grid/","HVGrid629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid629ProcessPlot <- ggplot(data = tempHighVoltageGrid629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid629New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(HighVoltageGrid629New$Layer)) {
#         if(HighVoltageGrid629New$Layer[j]==Defects629$Layer[i]) {HighVoltageGrid629ProcessPlot = HighVoltageGrid629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

HighVoltageGrid703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.PowerSupply.HighVoltage.Grid" & Date =="2018-07-05")
HighVoltageGrid703 <- subset(HighVoltageGrid703[c(2,3,4)])

#Add layer to High Voltage Grid and create Plot
#Add layer to categorize
Time_new <- HighVoltageGrid703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

HighVoltageGrid703$Layer <- Layer_new
colnames(HighVoltageGrid703) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageGrid703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageGrid703 <- subset(HighVoltageGrid703, Layer > 80 & Layer <= max(Defects703Layer))
head(HighVoltageGrid703)

### Assign processes within each layer
HighVoltageGrid703Time <- HighVoltageGrid703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(HighVoltageGrid703$Value))

for(i in 1:length(HighVoltageGrid703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (HighVoltageGrid703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (HighVoltageGrid703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (HighVoltageGrid703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (HighVoltageGrid703Time[i] >= Process703Time[j] && HighVoltageGrid703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
HighVoltageGrid703$Process <- Process703
colnames(HighVoltageGrid703) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageGrid703Freq <- subset(HighVoltageGrid703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageGrid703Freq <- subset(tempHighVoltageGrid703Freq, tempHighVoltageGrid703Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageGrid703Freq==Process703[i]))
    }
  }
}

ProcessFreqHighVoltageGrid <- cbind.data.frame(Layer, Process703 , N)
SummaryHighVoltageGridFreq <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=sum)
colnames(SummaryHighVoltageGridFreq) <- c("Process", "Frequency")

SummaryHighVoltageGridMean <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=mean)
colnames(SummaryHighVoltageGridMean) <- c("Process", "Average")

SummaryHighVoltageGrid703 <- cbind.data.frame(SummaryHighVoltageGridFreq, SummaryHighVoltageGridMean [c(2)])
SummaryHighVoltageGrid703 <- SummaryHighVoltageGrid703[order(-SummaryHighVoltageGrid703$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList703 <- unique(DefectList703)
# 
# HighVoltageGrid703$Time <- as.POSIXct(HighVoltageGrid703$Time,format="%H:%M:%OS")
# HighVoltageGrid703$Value <- as.numeric(HighVoltageGrid703$Value)
# 
# HVGrid20BeforeDefect703 <- subset(HighVoltageGrid703, Layer >= DefectList703$Layer[1]-20 & Layer <= DefectList703$Layer[1]-1)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(HVGrid20BeforeDefect703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect703$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid703 <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid703New <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/High Voltage Grid/","HVGrid20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid703ProcessPlot <- ggplot(data = HVGrid703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid703New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HVGrid703New$Layer)) {
#         if(HVGrid703New$Layer[j]==Defects703$Layer[i]) {HVGrid703ProcessPlot = HVGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect703$Layer)
#     tempHVGrid703 <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid703New <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/High Voltage Grid/","Beam20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid703ProcessPlot <- ggplot(data = tempHVGrid703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid703New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HVGrid703New$Layer)) {
#         if(HVGrid703New$Layer[j]==Defects703$Layer[i]) {HVGrid703ProcessPlot = HVGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid703 <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid703New <- filter(HVGrid20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/High Voltage Grid/","Beam20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid703ProcessPlot <- ggplot(data = tempHVGrid703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid703New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HVGrid703New$Layer)) {
#         if(HVGrid703New$Layer[j]==Defects703$Layer[i]) {HVGrid703ProcessPlot = HVGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempHVGrid703 <- filter(HighVoltageGrid703, Layer == DefectList703$Layer[1])
# HVGrid703New <- filter(HighVoltageGrid703, Layer == DefectList703$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/High Voltage Grid/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# HVGrid703ProcessPlot <- ggplot(data = tempHVGrid703, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=HVGrid703New, mapping = aes(x = Time, y = Value))+
#   ylab("High Voltage Grid") +
#   xlab("Time") +
#   ggtitle(paste("Processes of High Voltage Grid \n Layer", DefectList703$Layer[1]))
# 
# plot(HVGrid703ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# HighVoltageGrid703$Time <- as.POSIXct(HighVoltageGrid703$Time,format="%H:%M:%OS")
# HighVoltageGrid703$Value <- as.numeric(HighVoltageGrid703$Value)
# 
# HVGrid703ProcessPlot <- subset(HighVoltageGrid703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(HVGrid703ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(HVGrid703ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid703 <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid703New <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/High Voltage Grid/","HVGrid703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid703ProcessPlot <- ggplot(data = HighVoltageGrid703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHighVoltageGrid703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid703New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HighVoltageGrid703New$Layer)) {
#         if(HighVoltageGrid703New$Layer[j]==Defects703$Layer[i]) {HighVoltageGrid703ProcessPlot = HighVoltageGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid703ProcessPlot$Layer)
#     tempHighVoltageGrid703 <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid703New <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/High Voltage Grid/","HVGrid703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid703ProcessPlot <- ggplot(data = tempHighVoltageGrid703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid703New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HighVoltageGrid703New$Layer)) {
#         if(HighVoltageGrid703New$Layer[j]==Defects703$Layer[i]) {HighVoltageGrid703ProcessPlot = HighVoltageGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid703 <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid703New <- filter(HVGrid703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/High Voltage Grid/","HVGrid703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid703ProcessPlot <- ggplot(data = tempHighVoltageGrid703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid703New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(HighVoltageGrid703New$Layer)) {
#         if(HighVoltageGrid703New$Layer[j]==Defects703$Layer[i]) {HighVoltageGrid703ProcessPlot = HighVoltageGrid703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build710 ####
##################

HighVoltageGrid710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.Grid")

#Add layer to High Voltage Grid and create Plot
#Add layer to categorize
Time_new <- HighVoltageGrid710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

HighVoltageGrid710$Layer <- Layer_new
colnames(HighVoltageGrid710) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageGrid710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageGrid710 <- subset(HighVoltageGrid710, Layer > 80 & Layer <= max(Defects710Layer))
head(HighVoltageGrid710)

### Assign processes within each layer
HighVoltageGrid710Time <- HighVoltageGrid710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(HighVoltageGrid710$Value))

for(i in 1:length(HighVoltageGrid710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (HighVoltageGrid710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (HighVoltageGrid710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (HighVoltageGrid710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (HighVoltageGrid710Time[i] >= Process710Time[j] && HighVoltageGrid710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
HighVoltageGrid710$Process <- Process710
colnames(HighVoltageGrid710) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageGrid710Freq <- subset(HighVoltageGrid710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageGrid710Freq <- subset(tempHighVoltageGrid710Freq, tempHighVoltageGrid710Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageGrid710Freq==Process710[i]))
    }
  }
}

ProcessFreqHighVoltageGrid <- cbind.data.frame(Layer, Process710 , N)
SummaryHighVoltageGridFreq <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=sum)
colnames(SummaryHighVoltageGridFreq) <- c("Process", "Frequency")

SummaryHighVoltageGridMean <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=mean)
colnames(SummaryHighVoltageGridMean) <- c("Process", "Average")

SummaryHighVoltageGrid710 <- cbind.data.frame(SummaryHighVoltageGridFreq, SummaryHighVoltageGridMean [c(2)])
SummaryHighVoltageGrid710 <- SummaryHighVoltageGrid710[order(-SummaryHighVoltageGrid710$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList710 <- unique(DefectList710)
# 
# HighVoltageGrid710$Time <- as.POSIXct(HighVoltageGrid710$Time,format="%H:%M:%OS")
# HighVoltageGrid710$Value <- as.numeric(HighVoltageGrid710$Value)
# 
# HVGrid20BeforeDefect710 <- subset(HighVoltageGrid710, Layer >= DefectList710$Layer[1]-20 & Layer <= DefectList710$Layer[1]-1)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(HVGrid20BeforeDefect710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect710$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid710 <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid710New <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/High Voltage Grid/","HVGrid20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid710ProcessPlot <- ggplot(data = HVGrid710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid710New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HVGrid710New$Layer)) {
#         if(HVGrid710New$Layer[j]==Defects710$Layer[i]) {HVGrid710ProcessPlot = HVGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect710$Layer)
#     tempHVGrid710 <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid710New <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/High Voltage Grid/","Beam20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid710ProcessPlot <- ggplot(data = tempHVGrid710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid710New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HVGrid710New$Layer)) {
#         if(HVGrid710New$Layer[j]==Defects710$Layer[i]) {HVGrid710ProcessPlot = HVGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid710 <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid710New <- filter(HVGrid20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/High Voltage Grid/","Beam20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid710ProcessPlot <- ggplot(data = tempHVGrid710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid710New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HVGrid710New$Layer)) {
#         if(HVGrid710New$Layer[j]==Defects710$Layer[i]) {HVGrid710ProcessPlot = HVGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempHVGrid710 <- filter(HighVoltageGrid710, Layer == DefectList710$Layer[1])
# HVGrid710New <- filter(HighVoltageGrid710, Layer == DefectList710$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/High Voltage Grid/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# HVGrid710ProcessPlot <- ggplot(data = tempHVGrid710, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=HVGrid710New, mapping = aes(x = Time, y = Value))+
#   ylab("High Voltage Grid") +
#   xlab("Time") +
#   ggtitle(paste("Processes of High Voltage Grid \n Layer", DefectList710$Layer[1]))
# 
# plot(HVGrid710ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# HighVoltageGrid710$Time <- as.POSIXct(HighVoltageGrid710$Time,format="%H:%M:%OS")
# HighVoltageGrid710$Value <- as.numeric(HighVoltageGrid710$Value)
# 
# HVGrid710ProcessPlot <- subset(HighVoltageGrid710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(HVGrid710ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(HVGrid710ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid710 <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid710New <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/High Voltage Grid/","HVGrid710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid710ProcessPlot <- ggplot(data = HighVoltageGrid710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHighVoltageGrid710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid710New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HighVoltageGrid710New$Layer)) {
#         if(HighVoltageGrid710New$Layer[j]==Defects710$Layer[i]) {HighVoltageGrid710ProcessPlot = HighVoltageGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid710ProcessPlot$Layer)
#     tempHighVoltageGrid710 <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid710New <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/High Voltage Grid/","HVGrid710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid710ProcessPlot <- ggplot(data = tempHighVoltageGrid710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid710New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HighVoltageGrid710New$Layer)) {
#         if(HighVoltageGrid710New$Layer[j]==Defects710$Layer[i]) {HighVoltageGrid710ProcessPlot = HighVoltageGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid710 <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid710New <- filter(HVGrid710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/High Voltage Grid/","HVGrid710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid710ProcessPlot <- ggplot(data = tempHighVoltageGrid710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid710New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(HighVoltageGrid710New$Layer)) {
#         if(HighVoltageGrid710New$Layer[j]==Defects710$Layer[i]) {HighVoltageGrid710ProcessPlot = HighVoltageGrid710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build718 ####
##################

HighVoltageGrid718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.Grid")

#Add layer to High Voltage Grid and create Plot
#Add layer to categorize
Time_new <- HighVoltageGrid718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

HighVoltageGrid718$Layer <- Layer_new
colnames(HighVoltageGrid718) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageGrid718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageGrid718 <- subset(HighVoltageGrid718, Layer > 80 & Layer < max(Defects718Layer))
head(HighVoltageGrid718)

### Assign processes within each layer
HighVoltageGrid718Time <- HighVoltageGrid718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(HighVoltageGrid718$Value))

for(i in 1:length(HighVoltageGrid718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (HighVoltageGrid718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (HighVoltageGrid718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (HighVoltageGrid718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (HighVoltageGrid718Time[i] >= Process718Time[j] && HighVoltageGrid718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
HighVoltageGrid718$Process <- Process718
colnames(HighVoltageGrid718) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageGrid718Freq <- subset(HighVoltageGrid718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageGrid718Freq <- subset(tempHighVoltageGrid718Freq, tempHighVoltageGrid718Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageGrid718Freq==Process718[i]))
    }
  }
}

ProcessFreqHighVoltageGrid <- cbind.data.frame(Layer, Process718 , N)
SummaryHighVoltageGridFreq <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=sum)
colnames(SummaryHighVoltageGridFreq) <- c("Process", "Frequency")

SummaryHighVoltageGridMean <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=mean)
colnames(SummaryHighVoltageGridMean) <- c("Process", "Average")

SummaryHighVoltageGrid718 <- cbind.data.frame(SummaryHighVoltageGridFreq, SummaryHighVoltageGridMean [c(2)])
SummaryHighVoltageGrid718 <- SummaryHighVoltageGrid718[order(-SummaryHighVoltageGrid718$Frequency),]

# #### Plotting 20 Layers Before the Defect
# Defects <- c(104,223,254)
# 
# HighVoltageGrid718$Time <- as.POSIXct(HighVoltageGrid718$Time,format="%H:%M:%OS")
# HighVoltageGrid718$Value <- as.numeric(HighVoltageGrid718$Value)
# 
# HVGrid20BeforeDefect718_1 <- subset(HighVoltageGrid718, Layer >= Defects[1]-20 & Layer <=Defects[1]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(HVGrid20BeforeDefect718_1$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect718_1$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = HighVoltageGrid718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect718_1$Layer)
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# HVGrid20BeforeDefect718_2 <- subset(HighVoltageGrid718, Layer >= Defects[2]-20 & Layer <=Defects[2]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(HVGrid20BeforeDefect718_2$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect718_2$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = HighVoltageGrid718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect718_2$Layer)
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# HVGrid20BeforeDefect718_3 <- subset(HighVoltageGrid718, Layer >= Defects[3]-20 & Layer <=Defects[3]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(HVGrid20BeforeDefect718_3$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect718_3$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = HighVoltageGrid718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect718_3$Layer)
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid718 <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","HVGrid20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HVGrid718ProcessPlot = HVGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# for(i in 1:length(Defects)) {
#   tempHVGrid718 <- filter(HighVoltageGrid718, Layer == Defects[i])
#   HighVoltageGrid718New <- filter(HighVoltageGrid718, Layer == Defects[i])
#   
#   #Set Plot location and dimensions
#   png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/High Voltage Grid/","DefectPlot",i,".png" ,sep = ""),
#       width = 999,
#       height = 333)
#   
#   #Create Plot
#   theme_update(plot.title = element_text(hjust = 0.5))
#   HVGrid718ProcessPlot <- ggplot(data = tempHVGrid718, aes(x=Layer, y=Value)) + 
#     geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#     geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#     ylab("High Voltage Grid") +
#     xlab("Time") +
#     ggtitle(paste("Processes of High Voltage Grid \n Layer", Defects[i]))
#   
#   plot(HVGrid718ProcessPlot)
#   
#   dev.off()
#   
# }

# #Plotting around different layers
# HighVoltageGrid718$Time <- as.POSIXct(HighVoltageGrid718$Time,format="%H:%M:%OS")
# HighVoltageGrid718$Value <- as.numeric(HighVoltageGrid718$Value)
# 
# HVGrid718ProcessPlot <- subset(HighVoltageGrid718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(HVGrid718ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(HVGrid718ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid718 <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/High Voltage Grid/","HVGrid718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid718ProcessPlot <- ggplot(data = HighVoltageGrid718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHighVoltageGrid718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HighVoltageGrid718ProcessPlot = HighVoltageGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid718ProcessPlot$Layer)
#     tempHighVoltageGrid718 <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/High Voltage Grid/","HVGrid718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid718ProcessPlot <- ggplot(data = tempHighVoltageGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HighVoltageGrid718ProcessPlot = HighVoltageGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid718 <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid718New <- filter(HVGrid718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/High Voltage Grid/","HVGrid718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid718ProcessPlot <- ggplot(data = tempHighVoltageGrid718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid718New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(HighVoltageGrid718New$Layer)) {
#         if(HighVoltageGrid718New$Layer[j]==Defects718$Layer[i]) {HighVoltageGrid718ProcessPlot = HighVoltageGrid718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build723 ####
##################

HighVoltageGrid723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.Grid")

#Add layer to High Voltage Grid and create Plot
#Add layer to categorize
Time_new <- HighVoltageGrid723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

HighVoltageGrid723$Layer <- Layer_new
colnames(HighVoltageGrid723) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageGrid723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageGrid723 <- subset(HighVoltageGrid723, Layer > 80 & Layer <= max(Defects723Layer))
head(HighVoltageGrid723)

### Assign processes within each layer
HighVoltageGrid723Time <- HighVoltageGrid723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(HighVoltageGrid723$Value))

for(i in 1:length(HighVoltageGrid723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (HighVoltageGrid723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (HighVoltageGrid723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (HighVoltageGrid723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (HighVoltageGrid723Time[i] >= Process723Time[j] && HighVoltageGrid723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
HighVoltageGrid723$Process <- Process723
colnames(HighVoltageGrid723) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageGrid723Freq <- subset(HighVoltageGrid723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageGrid723Freq <- subset(tempHighVoltageGrid723Freq, tempHighVoltageGrid723Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageGrid723Freq==Process723[i]))
    }
  }
}

ProcessFreqHighVoltageGrid <- cbind.data.frame(Layer, Process723 , N)
SummaryHighVoltageGridFreq <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=sum)
colnames(SummaryHighVoltageGridFreq) <- c("Process", "Frequency")

SummaryHighVoltageGridMean <- aggregate(ProcessFreqHighVoltageGrid$N, by=(list((ProcessFreqHighVoltageGrid$Process))), FUN=mean)
colnames(SummaryHighVoltageGridMean) <- c("Process", "Average")

SummaryHighVoltageGrid723 <- cbind.data.frame(SummaryHighVoltageGridFreq, SummaryHighVoltageGridMean [c(2)])
SummaryHighVoltageGrid723 <- SummaryHighVoltageGrid723[order(-SummaryHighVoltageGrid723$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList723 <- unique(DefectList723)
# 
# HighVoltageGrid723$Time <- as.POSIXct(HighVoltageGrid723$Time,format="%H:%M:%OS")
# HighVoltageGrid723$Value <- as.numeric(HighVoltageGrid723$Value)
# 
# HVGrid20BeforeDefect723 <- subset(HighVoltageGrid723, Layer >= DefectList723$Layer[4]-20 & Layer <= DefectList723$Layer[4]-1)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(HVGrid20BeforeDefect723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(HVGrid20BeforeDefect723$Layer)
#     GroupEnd=GroupStart+4
#     tempHVGrid723 <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid723New <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/High Voltage Grid/","HVGrid20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid723ProcessPlot <- ggplot(data = HVGrid723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHVGrid723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid723New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HVGrid723New$Layer)) {
#         if(HVGrid723New$Layer[j]==Defects723$Layer[i]) {HVGrid723ProcessPlot = HVGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid20BeforeDefect723$Layer)
#     tempHVGrid723 <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid723New <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/High Voltage Grid/","Beam20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid723ProcessPlot <- ggplot(data = tempHVGrid723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid723New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HVGrid723New$Layer)) {
#         if(HVGrid723New$Layer[j]==Defects723$Layer[i]) {HVGrid723ProcessPlot = HVGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHVGrid723 <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     HVGrid723New <- filter(HVGrid20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/High Voltage Grid/","Beam20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HVGrid723ProcessPlot <- ggplot(data = tempHVGrid723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HVGrid723New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HVGrid723New$Layer)) {
#         if(HVGrid723New$Layer[j]==Defects723$Layer[i]) {HVGrid723ProcessPlot = HVGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HVGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempHVGrid723 <- filter(HighVoltageGrid723, Layer == DefectList723$Layer[4])
# HVGrid723New <- filter(HighVoltageGrid723, Layer == DefectList723$Layer[4])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/High Voltage Grid/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# HVGrid723ProcessPlot <- ggplot(data = tempHVGrid723, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=HVGrid723New, mapping = aes(x = Time, y = Value))+
#   ylab("High Voltage Grid") +
#   xlab("Time") +
#   ggtitle(paste("Processes of High Voltage Grid \n Layer", DefectList723$Layer[4]))
# 
# plot(HVGrid723ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# HighVoltageGrid723$Time <- as.POSIXct(HighVoltageGrid723$Time,format="%H:%M:%OS")
# HighVoltageGrid723$Value <- as.numeric(HighVoltageGrid723$Value)
# 
# HVGrid723ProcessPlot <- subset(HighVoltageGrid723, Layer > 85)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(HVGrid723ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(HVGrid723ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid723 <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid723New <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/High Voltage Grid/","HVGrid723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid723ProcessPlot <- ggplot(data = HighVoltageGrid723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempHighVoltageGrid723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid723New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HighVoltageGrid723New$Layer)) {
#         if(HighVoltageGrid723New$Layer[j]==Defects723$Layer[i]) {HighVoltageGrid723ProcessPlot = HighVoltageGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVGrid723ProcessPlot$Layer)
#     tempHighVoltageGrid723 <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid723New <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/High Voltage Grid/","HVGrid723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid723ProcessPlot <- ggplot(data = tempHighVoltageGrid723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid723New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HighVoltageGrid723New$Layer)) {
#         if(HighVoltageGrid723New$Layer[j]==Defects723$Layer[i]) {HighVoltageGrid723ProcessPlot = HighVoltageGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageGrid723 <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageGrid723New <- filter(HVGrid723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/High Voltage Grid/","HVGrid723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageGrid723ProcessPlot <- ggplot(data = tempHighVoltageGrid723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageGrid723New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Grid") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Grid \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(HighVoltageGrid723New$Layer)) {
#         if(HighVoltageGrid723New$Layer[j]==Defects723$Layer[i]) {HighVoltageGrid723ProcessPlot = HighVoltageGrid723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(HighVoltageGrid723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#     
#   }
# }

#########################################

HighVoltageGrid629$Build <- "629"
HighVoltageGrid703$Build <- "703"
HighVoltageGrid710$Build <- "710"
HighVoltageGrid718$Build <- "718"
HighVoltageGrid723$Build <- "723"

#####Combine all datasets####
HighVoltageGridAll <- rbind(HighVoltageGrid629,
                            HighVoltageGrid703,
                            HighVoltageGrid710,
                            HighVoltageGrid718,
                            HighVoltageGrid723)

HighVoltageGridAll$Layer_ID <- paste(HighVoltageGridAll$Build, "/", HighVoltageGridAll$Layer)
###########################################################################################################################

###############################################
#########@### High Voltage Demand #######@#####
############################################### 

##################
#### Build629 ####
##################

#Voltage Demand only has entries during initialization
VoltageDmd629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageDmd")
Time_new <- VoltageDmd629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

VoltageDmd629$Layer <- Layer_new
VoltageDmd629$Build <- "629"

#Remove values in support layers (<85) and layer 335
VoltageDmd629 <- filter(VoltageDmd629, Layer > 80 & Layer < max(VoltageDmd629$Layer))

##################
#### Build703 ####
##################

#Voltage Demand only has entries during initialization
VoltageDmd703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageDmd" & Date =="2018-07-05")
Time_new <- VoltageDmd703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

VoltageDmd703 <- subset(VoltageDmd703 [c(2,3,4)])
VoltageDmd703$Layer <- Layer_new
VoltageDmd703$Build <- "703"

#Remove values in support layers (<85) and layer 335
VoltageDmd703 <- filter(VoltageDmd703, Layer > 80 & Layer <= max(VoltageDmd703$Layer))

##################
#### Build710 ####
##################

#Voltage Demand only has entries during initialization
VoltageDmd710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageDmd")
Time_new <- VoltageDmd710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

VoltageDmd710$Layer <- Layer_new
VoltageDmd710$Build <- "710"

#Remove values in support layers (<85) and layer 335
VoltageDmd710 <- filter(VoltageDmd710, Layer > 80 & Layer <= max(VoltageDmd710$Layer))

##################
#### Build718 ####
##################

#Voltage Demand only has entries during initialization
VoltageDmd718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageDmd")
Time_new <- VoltageDmd718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

VoltageDmd718$Layer <- Layer_new
VoltageDmd718$Build <- "718"

#Remove values in support layers (<85) and layer 335
VoltageDmd718 <- filter(VoltageDmd718, Layer > 80 & Layer < max(VoltageDmd718$Layer))

##################
#### Build723 ####
##################

#Voltage Demand only has entries during initialization
VoltageDmd723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageDmd")
Time_new <- VoltageDmd723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

VoltageDmd723$Layer <- Layer_new
VoltageDmd723$Build <- "723"

#Remove values in support layers (<85) and layer 335
VoltageDmd723 <- filter(VoltageDmd723, Layer > 80 & Layer <= max(VoltageDmd723$Layer))

#### Combine all Voltage Demand Values ####
VoltageDmdAll <- rbind(VoltageDmd629,
                       VoltageDmd703,
                       VoltageDmd710,
                       VoltageDmd718,
                       VoltageDmd723)

VoltageDmdAll$Layer_ID <- paste(VoltageDmdAll$Build, "/", VoltageDmdAll$Layer)

###############################################
############ High Voltage Feedback ############
############################################### 

##################
#### Build629 ####
##################

#High Voltage Feedback
HighVoltageFeedback629 <- subset(Build629 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageFB")

#Add layer to High Voltage Feedback and create Plot
#Add layer to categorize
Time_new <- HighVoltageFeedback629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

HighVoltageFeedback629$Layer <- Layer_new
colnames(HighVoltageFeedback629) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageFeedback629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageFeedback629 <- subset(HighVoltageFeedback629, Layer > 80 & Layer < max(Defects629Layer))
head(HighVoltageFeedback629)

### Assign processes within each layer
HighVoltageFeedback629Time <- HighVoltageFeedback629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(HighVoltageFeedback629$Value))

for(i in 1:length(HighVoltageFeedback629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (HighVoltageFeedback629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (HighVoltageFeedback629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (HighVoltageFeedback629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (HighVoltageFeedback629Time[i] >= Process629Time[j] && HighVoltageFeedback629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
HighVoltageFeedback629$Process <- Process629
colnames(HighVoltageFeedback629) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageFeedback629Freq <- subset(HighVoltageFeedback629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageFeedbackFreq629 <- subset(tempHighVoltageFeedback629Freq, tempHighVoltageFeedback629Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageFeedbackFreq629==Process629[i]))
    }
  }
}

ProcessFreqHighVoltageFB <- cbind.data.frame(Layer, Process629 , N)
SummaryHighVoltageFBFreq <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=sum)
colnames(SummaryHighVoltageFBFreq) <- c("Process", "Frequency")

SummaryHighVoltageFBMean <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=mean)
colnames(SummaryHighVoltageFBMean) <- c("Process", "Average")

SummaryHighVoltageFeedback629 <- cbind.data.frame(SummaryHighVoltageFBFreq, SummaryHighVoltageFBMean [c(2)])
SummaryHighVoltageFeedback629 <- SummaryHighVoltageFeedback629[order(-SummaryHighVoltageFeedback629$Frequency),]

# #Plotting around different layers
# HighVoltageFeedback629$Time <- as.POSIXct(HighVoltageFeedback629$Time,format="%H:%M:%OS")
# HighVoltageFeedback629$Value <- as.numeric(HighVoltageFeedback629$Value)
# 
# HVFeedback629ProcessPlot <- subset(HighVoltageFeedback629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(HVFeedback629ProcessPlot$Layer))/10)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(HVFeedback629ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback629 <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback629New <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Feedback/","HVFeedback629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback629ProcessPlot <- ggplot(data = HighVoltageFeedback629New, aes(x=Layer, y=Value)) +
#       geom_point(data=tempHighVoltageFeedback629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback629New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     # if(length(tempHighVoltageFeedback629$Layer != 0)) {
#     #   for(i in 1:length(Defects629$Layer)) {
#     #     for(j in 1:length(HighVoltageFeedback629New$Layer)) {
#     #       if(HighVoltageFeedback629New$Layer[j]==Defects629$Layer[i]) {HighVoltageFeedback629ProcessPlot = HighVoltageFeedback629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}}
#     #   }
#     # }
# 
#     plot(HighVoltageFeedback629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HVFeedback629ProcessPlot$Layer)
#     tempHighVoltageFeedback629 <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback629New <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Feedback/","HVFeedback629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback629ProcessPlot <- ggplot(data = tempHighVoltageFeedback629, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback629New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     # if(length(tempHighVoltageFeedback629$Layer != 0)) {
#     #   for(i in 1:length(Defects629$Layer)) {
#     #     for(j in 1:length(HighVoltageFeedback629New$Layer)) {
#     #       if(HighVoltageFeedback629New$Layer[j]==Defects629$Layer[i]) {HighVoltageFeedback629ProcessPlot = HighVoltageFeedback629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}}
#     #   }
#     # }
# 
#     plot(HighVoltageFeedback629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback629 <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback629New <- filter(HVFeedback629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/High Voltage Feedback/","HVFeedback629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback629ProcessPlot <- ggplot(data = tempHighVoltageFeedback629, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback629New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     # if(length(tempHighVoltageFeedback629$Layer != 0)) {
#     #   for(i in 1:length(Defects629$Layer)) {
#     #     for(j in 1:length(HighVoltageFeedback629New$Layer)) {
#     #       if(HighVoltageFeedback629New$Layer[j]==Defects629$Layer[i]) {HighVoltageFeedback629ProcessPlot = HighVoltageFeedback629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}}
#     #   }
#     # }
# 
#     plot(HighVoltageFeedback629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

#High Voltage Feedback
HighVoltageFeedback703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageFB" & Date =="2018-07-05")
HighVoltageFeedback703 <-subset(HighVoltageFeedback703[c(2,3,4)])

#Add layer to High Voltage Feedback and create Plot
#Add layer to categorize
Time_new <- HighVoltageFeedback703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

HighVoltageFeedback703$Layer <- Layer_new
colnames(HighVoltageFeedback703) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageFeedback703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageFeedback703 <- subset(HighVoltageFeedback703, Layer > 80 & Layer <= max(Defects703Layer))
head(HighVoltageFeedback703)

### Assign processes within each layer
HighVoltageFeedback703Time <- HighVoltageFeedback703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(HighVoltageFeedback703$Value))

for(i in 1:length(HighVoltageFeedback703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (HighVoltageFeedback703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (HighVoltageFeedback703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (HighVoltageFeedback703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (HighVoltageFeedback703Time[i] >= Process703Time[j] && HighVoltageFeedback703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
HighVoltageFeedback703$Process <- Process703
colnames(HighVoltageFeedback703) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageFeedback703Freq <- subset(HighVoltageFeedback703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageFeedbackFreq703 <- subset(tempHighVoltageFeedback703Freq, tempHighVoltageFeedback703Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageFeedbackFreq703==Process703[i]))
    }
  }
}

ProcessFreqHighVoltageFB <- cbind.data.frame(Layer, Process703 , N)
SummaryHighVoltageFBFreq <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=sum)
colnames(SummaryHighVoltageFBFreq) <- c("Process", "Frequency")

SummaryHighVoltageFBMean <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=mean)
colnames(SummaryHighVoltageFBMean) <- c("Process", "Average")

SummaryHighVoltageFeedback703 <- cbind.data.frame(SummaryHighVoltageFBFreq, SummaryHighVoltageFBMean [c(2)])
SummaryHighVoltageFeedback703 <- SummaryHighVoltageFeedback703[order(-SummaryHighVoltageFeedback703$Frequency),]

# #Plotting around different layers
# HighVoltageFeedback703$Time <- as.POSIXct(HighVoltageFeedback703$Time,format="%H:%M:%OS")
# HighVoltageFeedback703$Value <- as.numeric(HighVoltageFeedback703$Value)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(HighVoltageFeedback703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(HighVoltageFeedback703$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback703 <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback703New <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback703ProcessPlot <- ggplot(data = HighVoltageFeedback703New, aes(x=Layer, y=Value)) + 
#       geom_point(data=tempHighVoltageFeedback703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback703New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback703$Layer != 0)) {
#       for(i in 1:length(Defects703$Layer)) {
#         for(j in 1:length(HighVoltageFeedback703New$Layer)) {
#           if(HighVoltageFeedback703New$Layer[j]==Defects703$Layer[i]) {HighVoltageFeedback703ProcessPlot = HighVoltageFeedback703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback703ProcessPlot)}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HighVoltageFeedback703$Layer)
#     tempHighVoltageFeedback703 <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback703New <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback703ProcessPlot <- ggplot(data = tempHighVoltageFeedback703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback703New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback703$Layer != 0)) {
#       for(i in 1:length(Defects703$Layer)) {
#         for(j in 1:length(HighVoltageFeedback703New$Layer)) {
#           if(HighVoltageFeedback703New$Layer[j]==Defects703$Layer[i]) {HighVoltageFeedback703ProcessPlot = HighVoltageFeedback703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback703ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback703 <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback703New <- filter(HighVoltageFeedback703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback703ProcessPlot <- ggplot(data = tempHighVoltageFeedback703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback703New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback703$Layer != 0)) {
#       for(i in 1:length(Defects703$Layer)) {
#         for(j in 1:length(HighVoltageFeedback703New$Layer)) {
#           if(HighVoltageFeedback703New$Layer[j]==Defects703$Layer[i]) {HighVoltageFeedback703ProcessPlot = HighVoltageFeedback703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback703ProcessPlot)
#   }
# }

# #Quickly Exporting Images
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build703/703_BeamCurrentProcesses")
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, "C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build703/703_BeamCurrentProcesses", row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0("C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build703/703_BeamCurrentProcesses", 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# 
# file.copy(from=numbered.png.names, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build703/703_BeamCurrentProcesses")

##################
#### Build710 ####
##################

#High Voltage Feedback
HighVoltageFeedback710 <- subset(Build710 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageFB")

#Add layer to High Voltage Feedback and create Plot
#Add layer to categorize
Time_new <- HighVoltageFeedback710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

HighVoltageFeedback710$Layer <- Layer_new
colnames(HighVoltageFeedback710) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageFeedback710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageFeedback710 <- subset(HighVoltageFeedback710, Layer > 80 & Layer <= max(Defects710Layer))
head(HighVoltageFeedback710)

### Assign processes within each layer
HighVoltageFeedback710Time <- HighVoltageFeedback710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(HighVoltageFeedback710$Value))

for(i in 1:length(HighVoltageFeedback710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (HighVoltageFeedback710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (HighVoltageFeedback710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (HighVoltageFeedback710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (HighVoltageFeedback710Time[i] >= Process710Time[j] && HighVoltageFeedback710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
HighVoltageFeedback710$Process <- Process710
colnames(HighVoltageFeedback710) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageFeedback710Freq <- subset(HighVoltageFeedback710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageFeedbackFreq710 <- subset(tempHighVoltageFeedback710Freq, tempHighVoltageFeedback710Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageFeedbackFreq710==Process710[i]))
    }
  }
}

ProcessFreqHighVoltageFB <- cbind.data.frame(Layer, Process710 , N)
SummaryHighVoltageFBFreq <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=sum)
colnames(SummaryHighVoltageFBFreq) <- c("Process", "Frequency")

SummaryHighVoltageFBMean <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=mean)
colnames(SummaryHighVoltageFBMean) <- c("Process", "Average")

SummaryHighVoltageFeedback710 <- cbind.data.frame(SummaryHighVoltageFBFreq, SummaryHighVoltageFBMean [c(2)])
SummaryHighVoltageFeedback710 <- SummaryHighVoltageFeedback710[order(-SummaryHighVoltageFeedback710$Frequency),]

# #Plotting around different layers
# HighVoltageFeedback710$Time <- as.POSIXct(HighVoltageFeedback710$Time,format="%H:%M:%OS")
# HighVoltageFeedback710$Value <- as.numeric(HighVoltageFeedback710$Value)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(HighVoltageFeedback710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(HighVoltageFeedback710$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback710 <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback710New <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback710ProcessPlot <- ggplot(data = HighVoltageFeedback710New, aes(x=Layer, y=Value)) + 
#       geom_point(data=tempHighVoltageFeedback710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback710New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback710$Layer != 0)) {
#       for(i in 1:length(Defects710$Layer)) {
#         for(j in 1:length(HighVoltageFeedback710New$Layer)) {
#           if(HighVoltageFeedback710New$Layer[j]==Defects710$Layer[i]) {HighVoltageFeedback710ProcessPlot = HighVoltageFeedback710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback710ProcessPlot)}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HighVoltageFeedback710$Layer)
#     tempHighVoltageFeedback710 <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback710New <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback710ProcessPlot <- ggplot(data = tempHighVoltageFeedback710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback710New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback710$Layer != 0)) {
#       for(i in 1:length(Defects710$Layer)) {
#         for(j in 1:length(HighVoltageFeedback710New$Layer)) {
#           if(HighVoltageFeedback710New$Layer[j]==Defects710$Layer[i]) {HighVoltageFeedback710ProcessPlot = HighVoltageFeedback710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback710ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback710 <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback710New <- filter(HighVoltageFeedback710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback710ProcessPlot <- ggplot(data = tempHighVoltageFeedback710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback710New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback710$Layer != 0)) {
#       for(i in 1:length(Defects710$Layer)) {
#         for(j in 1:length(HighVoltageFeedback710New$Layer)) {
#           if(HighVoltageFeedback710New$Layer[j]==Defects710$Layer[i]) {HighVoltageFeedback710ProcessPlot = HighVoltageFeedback710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback710ProcessPlot)
#   }
# }

# #Quickly Exporting Images
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build710/710_BeamCurrentProcesses")
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, "C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build710/710_BeamCurrentProcesses", row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0("C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build710/710_BeamCurrentProcesses", 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# 
# file.copy(from=numbered.png.names, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build710/710_BeamCurrentProcesses")

##################
#### Build718 ####
##################

#High Voltage Feedback
HighVoltageFeedback718 <- subset(Build718 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageFB")

#Add layer to High Voltage Feedback and create Plot
#Add layer to categorize
Time_new <- HighVoltageFeedback718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

HighVoltageFeedback718$Layer <- Layer_new
colnames(HighVoltageFeedback718) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageFeedback718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageFeedback718 <- subset(HighVoltageFeedback718, Layer > 80 & Layer < max(Defects718Layer))
head(HighVoltageFeedback718)

### Assign processes within each layer
HighVoltageFeedback718Time <- HighVoltageFeedback718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(HighVoltageFeedback718$Value))

for(i in 1:length(HighVoltageFeedback718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (HighVoltageFeedback718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (HighVoltageFeedback718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (HighVoltageFeedback718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (HighVoltageFeedback718Time[i] >= Process718Time[j] && HighVoltageFeedback718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
HighVoltageFeedback718$Process <- Process718
colnames(HighVoltageFeedback718) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageFeedback718Freq <- subset(HighVoltageFeedback718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageFeedbackFreq718 <- subset(tempHighVoltageFeedback718Freq, tempHighVoltageFeedback718Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageFeedbackFreq718==Process718[i]))
    }
  }
}

ProcessFreqHighVoltageFB <- cbind.data.frame(Layer, Process718 , N)
SummaryHighVoltageFBFreq <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=sum)
colnames(SummaryHighVoltageFBFreq) <- c("Process", "Frequency")

SummaryHighVoltageFBMean <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=mean)
colnames(SummaryHighVoltageFBMean) <- c("Process", "Average")

SummaryHighVoltageFeedback718 <- cbind.data.frame(SummaryHighVoltageFBFreq, SummaryHighVoltageFBMean [c(2)])
SummaryHighVoltageFeedback718 <- SummaryHighVoltageFeedback718[order(-SummaryHighVoltageFeedback718$Frequency),]

# #Plotting around different layers
# HighVoltageFeedback718$Time <- as.POSIXct(HighVoltageFeedback718$Time,format="%H:%M:%OS")
# HighVoltageFeedback718$Value <- as.numeric(HighVoltageFeedback718$Value)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(HighVoltageFeedback718$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(HighVoltageFeedback718$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback718 <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback718New <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback718ProcessPlot <- ggplot(data = HighVoltageFeedback718New, aes(x=Layer, y=Value)) + 
#       geom_point(data=tempHighVoltageFeedback718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback718New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback718$Layer != 0)) {
#       for(i in 1:length(Defects718$Layer)) {
#         for(j in 1:length(HighVoltageFeedback718New$Layer)) {
#           if(HighVoltageFeedback718New$Layer[j]==Defects718$Layer[i]) {HighVoltageFeedback718ProcessPlot = HighVoltageFeedback718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback718ProcessPlot)}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HighVoltageFeedback718$Layer)
#     tempHighVoltageFeedback718 <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback718New <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback718ProcessPlot <- ggplot(data = tempHighVoltageFeedback718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback718New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback718$Layer != 0)) {
#       for(i in 1:length(Defects718$Layer)) {
#         for(j in 1:length(HighVoltageFeedback718New$Layer)) {
#           if(HighVoltageFeedback718New$Layer[j]==Defects718$Layer[i]) {HighVoltageFeedback718ProcessPlot = HighVoltageFeedback718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback718ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback718 <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback718New <- filter(HighVoltageFeedback718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback718ProcessPlot <- ggplot(data = tempHighVoltageFeedback718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback718New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback718$Layer != 0)) {
#       for(i in 1:length(Defects718$Layer)) {
#         for(j in 1:length(HighVoltageFeedback718New$Layer)) {
#           if(HighVoltageFeedback718New$Layer[j]==Defects718$Layer[i]) {HighVoltageFeedback718ProcessPlot = HighVoltageFeedback718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback718ProcessPlot)
#   }
# }

# #Quickly Exporting Images
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build718/718_BeamCurrentProcesses")
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, "C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build718/718_BeamCurrentProcesses", row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0("C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build718/718_BeamCurrentProcesses", 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# 
# file.copy(from=numbered.png.names, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build718/718_BeamCurrentProcesses")

##################
#### Build723 ####
##################

#High Voltage Feedback
HighVoltageFeedback723 <- subset(Build723 [c(2,3,6)], X2=="OPC.PowerSupply.HighVoltage.VoltageFB")

#Add layer to High Voltage Feedback and create Plot
#Add layer to categorize
Time_new <- HighVoltageFeedback723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

HighVoltageFeedback723$Layer <- Layer_new
colnames(HighVoltageFeedback723) <- list("Time", "Variable", "Value", "Layer")
head(HighVoltageFeedback723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
HighVoltageFeedback723 <- subset(HighVoltageFeedback723, Layer > 80 & Layer <= max(Defects723Layer))
head(HighVoltageFeedback723)

### Assign processes within each layer
HighVoltageFeedback723Time <- HighVoltageFeedback723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(HighVoltageFeedback723$Value))

for(i in 1:length(HighVoltageFeedback723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (HighVoltageFeedback723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (HighVoltageFeedback723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (HighVoltageFeedback723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (HighVoltageFeedback723Time[i] >= Process723Time[j] && HighVoltageFeedback723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
HighVoltageFeedback723$Process <- Process723
colnames(HighVoltageFeedback723) <- list("Time", "Variable", "Value", "Layer", "Process")

#Create a table to count the number of times an action is performed in each layer
tempHighVoltageFeedback723Freq <- subset(HighVoltageFeedback723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempHighVoltageFeedbackFreq723 <- subset(tempHighVoltageFeedback723Freq, tempHighVoltageFeedback723Freq$Layer == j)
      N[i] = length(which(temptempHighVoltageFeedbackFreq723==Process723[i]))
    }
  }
}

ProcessFreqHighVoltageFB <- cbind.data.frame(Layer, Process723 , N)
SummaryHighVoltageFBFreq <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=sum)
colnames(SummaryHighVoltageFBFreq) <- c("Process", "Frequency")

SummaryHighVoltageFBMean <- aggregate(ProcessFreqHighVoltageFB$N, by=(list((ProcessFreqHighVoltageFB$Process))), FUN=mean)
colnames(SummaryHighVoltageFBMean) <- c("Process", "Average")

SummaryHighVoltageFeedback723 <- cbind.data.frame(SummaryHighVoltageFBFreq, SummaryHighVoltageFBMean [c(2)])
SummaryHighVoltageFeedback723 <- SummaryHighVoltageFeedback723[order(-SummaryHighVoltageFeedback723$Frequency),]

# #Plotting around different layers
# HighVoltageFeedback723$Time <- as.POSIXct(HighVoltageFeedback723$Time,format="%H:%M:%OS")
# HighVoltageFeedback723$Value <- as.numeric(HighVoltageFeedback723$Value)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(HighVoltageFeedback723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(HighVoltageFeedback723$Layer)
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback723 <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback723New <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback723ProcessPlot <- ggplot(data = HighVoltageFeedback723New, aes(x=Layer, y=Value)) + 
#       geom_point(data=tempHighVoltageFeedback723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback723New, mapping = aes(x = Time, y = Value))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback723$Layer != 0)) {
#       for(i in 1:length(Defects723$Layer)) {
#         for(j in 1:length(HighVoltageFeedback723New$Layer)) {
#           if(HighVoltageFeedback723New$Layer[j]==Defects723$Layer[i]) {HighVoltageFeedback723ProcessPlot = HighVoltageFeedback723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback723ProcessPlot)}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(HighVoltageFeedback723$Layer)
#     tempHighVoltageFeedback723 <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback723New <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback723ProcessPlot <- ggplot(data = tempHighVoltageFeedback723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback723New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback723$Layer != 0)) {
#       for(i in 1:length(Defects723$Layer)) {
#         for(j in 1:length(HighVoltageFeedback723New$Layer)) {
#           if(HighVoltageFeedback723New$Layer[j]==Defects723$Layer[i]) {HighVoltageFeedback723ProcessPlot = HighVoltageFeedback723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback723ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempHighVoltageFeedback723 <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     HighVoltageFeedback723New <- filter(HighVoltageFeedback723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     HighVoltageFeedback723ProcessPlot <- ggplot(data = tempHighVoltageFeedback723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=HighVoltageFeedback723New, mapping = aes(x = Time, y = Value,))+
#       ylab("High Voltage Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of High Voltage Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     if(length(tempHighVoltageFeedback723$Layer != 0)) {
#       for(i in 1:length(Defects723$Layer)) {
#         for(j in 1:length(HighVoltageFeedback723New$Layer)) {
#           if(HighVoltageFeedback723New$Layer[j]==Defects723$Layer[i]) {HighVoltageFeedback723ProcessPlot = HighVoltageFeedback723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}}
#       }
#     }
#     
#     plot(HighVoltageFeedback723ProcessPlot)
#   }
# }

# #Quickly Exporting Images
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build723/723_BeamCurrentProcesses")
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, "C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build723/723_BeamCurrentProcesses", row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0("C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build723/723_BeamCurrentProcesses", 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# 
# file.copy(from=numbered.png.names, to="C:/Users/Ethan/Desktop/Training3/MISC/WeeklyMeeting/Meeting_Week4/Build723/723_BeamCurrentProcesses")


#########################################

HighVoltageFeedback629$Build <- "629"
HighVoltageFeedback703$Build <- "703"
HighVoltageFeedback710$Build <- "710"
HighVoltageFeedback718$Build <- "718"
HighVoltageFeedback723$Build <- "723"

#####Combine all datasets####
HighVoltageFeedbackAll <- rbind(HighVoltageFeedback629,
                            HighVoltageFeedback703,
                            HighVoltageFeedback710,
                            HighVoltageFeedback718,
                            HighVoltageFeedback723)

HighVoltageFeedbackAll$Layer_ID <- paste(HighVoltageFeedbackAll$Build, "/", HighVoltageFeedbackAll$Layer)


###########################################################################################################################

########################### Defect: Curling ####################################

################################################
############## Bottom Temperature ############## 
################################################

##################
#### Build629 ####
##################

BottomTemp629 <- subset(Build629 [c(2,3,6)], X2=="OPC.Temperature.BottomTemperature")

#Add layer to Bottom Temperature and create Plot
Time_new <- BottomTemp629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

BottomTemp629$Layer <- Layer_new
colnames(BottomTemp629) <- list("Time", "Variable", "Value", "Layer")
head(BottomTemp629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BottomTemp629 <- subset(BottomTemp629, Layer > 80 & Layer < max(Defects629Layer))
head(BottomTemp629)

test<- BottomTemp629
test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
test$Value <- as.numeric(test$Value)
test <- subset(test, Layer>85 & Layer<335)
ggplot(data = test) + 
  geom_line(mapping = aes(x = Time, y = Value))
ggplot(data = subset(test, Layer>85 & Layer<335)) +
  geom_line(mapping = aes(x = Time, y = Value))

### Assign processes within each layer
BottomTemp629Time <- BottomTemp629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(BottomTemp629$Value))

for(i in 1:length(BottomTemp629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (BottomTemp629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (BottomTemp629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (BottomTemp629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (BottomTemp629Time[i] >= Process629Time[j] && BottomTemp629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
BottomTemp629$Process <- Process629
BottomTemp629$Build <- "629"

colnames(BottomTemp629) <- list("Time","Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBottomTemp629Freq <- subset(BottomTemp629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempBottomTemp629Freq <- subset(tempBottomTemp629Freq, tempBottomTemp629Freq$Layer == j)
      N[i] = length(which(temptempBottomTemp629Freq==Process629[i]))
    }
  }
}

ProcessFreqBottomTemp <- cbind.data.frame(Layer, Process629 , N)
SummaryBottomTempFreq <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=sum)
colnames(SummaryBottomTempFreq) <- c("Process", "Frequency")

SummaryBottomTempMean <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=mean)
colnames(SummaryBottomTempMean) <- c("Process", "Average")

SummaryBottomTemp629 <- cbind.data.frame(SummaryBottomTempFreq, SummaryBottomTempMean [c(2)])
SummaryBottomTemp629 <- SummaryBottomTemp629[order(-SummaryBottomTemp629$Frequency),]

# #Plotting around different layers
# BottomTemp629$Time <- as.POSIXct(BottomTemp629$Time,format="%H:%M:%OS")
# BottomTemp629$Value <- as.numeric(BottomTemp629$Value)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(BottomTemp629$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(BottomTemp629$Layer)
#     GroupEnd=GroupStart+4
#     tempBottomTemp629 <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp629New <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp629ProcessPlot <- ggplot(data = BottomTemp629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBottomTemp629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp629New, mapping = aes(x = Time, y = Value))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BottomTemp629New$Layer)) {
#         if(BottomTemp629New$Layer[j]==Defects629$Layer[i]) {BottomTemp629ProcessPlot = BottomTemp629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp629ProcessPlot)}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BottomTemp629$Layer)
#     tempBottomTemp629 <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp629New <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp629ProcessPlot <- ggplot(data = tempBottomTemp629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BottomTemp629New$Layer)) {
#         if(BottomTemp629New$Layer[j]==Defects629$Layer[i]) {BottomTemp629ProcessPlot = BottomTemp629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp629ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBottomTemp629 <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp629New <- filter(BottomTemp629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp629ProcessPlot <- ggplot(data = tempBottomTemp629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BottomTemp629New$Layer)) {
#         if(BottomTemp629New$Layer[j]==Defects629$Layer[i]) {BottomTemp629ProcessPlot = BottomTemp629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp629ProcessPlot)
#   }
# }

##################
#### Build703 ####
##################

BottomTemp703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.Temperature.BottomTemperature" & Date =="2018-07-05")
BottomTemp703 <- subset(BottomTemp703[c(2,3,4)])

#Add layer to Bottom Temperature and create Plot
Time_new <- BottomTemp703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

BottomTemp703$Layer <- Layer_new
colnames(BottomTemp703) <- list("Time", "Variable", "Value", "Layer")
head(BottomTemp703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BottomTemp703 <- subset(BottomTemp703, Layer > 80 & Layer < max(Defects703Layer))
head(BottomTemp703)

test<- BottomTemp703
test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
test$Value <- as.numeric(test$Value)
test <- subset(test, Layer>85 & Layer<335)
ggplot(data = test) + 
  geom_line(mapping = aes(x = Time, y = Value))
ggplot(data = subset(test, Layer>85 & Layer<335)) +
  geom_line(mapping = aes(x = Time, y = Value))

### Assign processes within each layer
BottomTemp703Time <- BottomTemp703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(BottomTemp703$Value))

for(i in 1:length(BottomTemp703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (BottomTemp703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (BottomTemp703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (BottomTemp703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (BottomTemp703Time[i] >= Process703Time[j] && BottomTemp703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
BottomTemp703$Process <- Process703
BottomTemp703$Build <- "703"

colnames(BottomTemp703) <- list("Time","Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBottomTemp703Freq <- subset(BottomTemp703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempBottomTemp703Freq <- subset(tempBottomTemp703Freq, tempBottomTemp703Freq$Layer == j)
      N[i] = length(which(temptempBottomTemp703Freq==Process703[i]))
    }
  }
}

ProcessFreqBottomTemp <- cbind.data.frame(Layer, Process703 , N)
SummaryBottomTempFreq <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=sum)
colnames(SummaryBottomTempFreq) <- c("Process", "Frequency")

SummaryBottomTempMean <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=mean)
colnames(SummaryBottomTempMean) <- c("Process", "Average")

SummaryBottomTemp703 <- cbind.data.frame(SummaryBottomTempFreq, SummaryBottomTempMean [c(2)])
SummaryBottomTemp703 <- SummaryBottomTemp703[order(-SummaryBottomTemp703$Frequency),]

# #Plotting around different layers
# BottomTemp703$Time <- as.POSIXct(BottomTemp703$Time,format="%H:%M:%OS")
# BottomTemp703$Value <- as.numeric(BottomTemp703$Value)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(BottomTemp703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(BottomTemp703$Layer)
#     GroupEnd=GroupStart+4
#     tempBottomTemp703 <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp703New <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp703ProcessPlot <- ggplot(data = BottomTemp703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBottomTemp703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp703New, mapping = aes(x = Time, y = Value))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BottomTemp703New$Layer)) {
#         if(BottomTemp703New$Layer[j]==Defects703$Layer[i]) {BottomTemp703ProcessPlot = BottomTemp703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp703ProcessPlot)}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BottomTemp703$Layer)
#     tempBottomTemp703 <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp703New <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp703ProcessPlot <- ggplot(data = tempBottomTemp703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BottomTemp703New$Layer)) {
#         if(BottomTemp703New$Layer[j]==Defects703$Layer[i]) {BottomTemp703ProcessPlot = BottomTemp703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp703ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBottomTemp703 <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp703New <- filter(BottomTemp703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp703ProcessPlot <- ggplot(data = tempBottomTemp703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BottomTemp703New$Layer)) {
#         if(BottomTemp703New$Layer[j]==Defects703$Layer[i]) {BottomTemp703ProcessPlot = BottomTemp703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp703ProcessPlot)
#   }
# }

##################
#### Build710 ####
##################

BottomTemp710 <- subset(Build710 [c(2,3,6)], X2=="OPC.Temperature.BottomTemperature")

#Add layer to Bottom Temperature and create Plot
Time_new <- BottomTemp710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

BottomTemp710$Layer <- Layer_new
colnames(BottomTemp710) <- list("Time", "Variable", "Value", "Layer")
head(BottomTemp710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BottomTemp710 <- subset(BottomTemp710, Layer > 80 & Layer < max(Defects710Layer))
head(BottomTemp710)

test<- BottomTemp710
test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
test$Value <- as.numeric(test$Value)
test <- subset(test, Layer>85 & Layer<335)
ggplot(data = test) + 
  geom_line(mapping = aes(x = Time, y = Value))
ggplot(data = subset(test, Layer>85 & Layer<335)) +
  geom_line(mapping = aes(x = Time, y = Value))

### Assign processes within each layer
BottomTemp710Time <- BottomTemp710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(BottomTemp710$Value))

for(i in 1:length(BottomTemp710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (BottomTemp710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (BottomTemp710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (BottomTemp710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (BottomTemp710Time[i] >= Process710Time[j] && BottomTemp710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
BottomTemp710$Process <- Process710
BottomTemp710$Build <- "710"

colnames(BottomTemp710) <- list("Time","Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBottomTemp710Freq <- subset(BottomTemp710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempBottomTemp710Freq <- subset(tempBottomTemp710Freq, tempBottomTemp710Freq$Layer == j)
      N[i] = length(which(temptempBottomTemp710Freq==Process710[i]))
    }
  }
}

ProcessFreqBottomTemp <- cbind.data.frame(Layer, Process710 , N)
SummaryBottomTempFreq <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=sum)
colnames(SummaryBottomTempFreq) <- c("Process", "Frequency")

SummaryBottomTempMean <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=mean)
colnames(SummaryBottomTempMean) <- c("Process", "Average")

SummaryBottomTemp710 <- cbind.data.frame(SummaryBottomTempFreq, SummaryBottomTempMean [c(2)])
SummaryBottomTemp710 <- SummaryBottomTemp710[order(-SummaryBottomTemp710$Frequency),]

# #Plotting around different layers
# BottomTemp710$Time <- as.POSIXct(BottomTemp710$Time,format="%H:%M:%OS")
# BottomTemp710$Value <- as.numeric(BottomTemp710$Value)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(BottomTemp710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(BottomTemp710$Layer)
#     GroupEnd=GroupStart+4
#     tempBottomTemp710 <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp710New <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp710ProcessPlot <- ggplot(data = BottomTemp710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBottomTemp710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp710New, mapping = aes(x = Time, y = Value))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BottomTemp710New$Layer)) {
#         if(BottomTemp710New$Layer[j]==Defects710$Layer[i]) {BottomTemp710ProcessPlot = BottomTemp710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp710ProcessPlot)}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BottomTemp710$Layer)
#     tempBottomTemp710 <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp710New <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp710ProcessPlot <- ggplot(data = tempBottomTemp710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BottomTemp710New$Layer)) {
#         if(BottomTemp710New$Layer[j]==Defects710$Layer[i]) {BottomTemp710ProcessPlot = BottomTemp710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp710ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBottomTemp710 <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp710New <- filter(BottomTemp710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp710ProcessPlot <- ggplot(data = tempBottomTemp710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BottomTemp710New$Layer)) {
#         if(BottomTemp710New$Layer[j]==Defects710$Layer[i]) {BottomTemp710ProcessPlot = BottomTemp710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp710ProcessPlot)
#   }
# }

##################
#### Build718 ####
##################

BottomTemp718 <- subset(Build718 [c(2,3,6)], X2=="OPC.Temperature.BottomTemperature")

#Add layer to Bottom Temperature and create Plot
Time_new <- BottomTemp718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

BottomTemp718$Layer <- Layer_new
colnames(BottomTemp718) <- list("Time", "Variable", "Value", "Layer")
head(BottomTemp718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BottomTemp718 <- subset(BottomTemp718, Layer > 80 & Layer < max(Defects718Layer))
head(BottomTemp718)

test<- BottomTemp718
test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
test$Value <- as.numeric(test$Value)
test <- subset(test, Layer>85 & Layer<335)
ggplot(data = test) + 
  geom_line(mapping = aes(x = Time, y = Value))
ggplot(data = subset(test, Layer>85 & Layer<335)) +
  geom_line(mapping = aes(x = Time, y = Value))

### Assign processes within each layer
BottomTemp718Time <- BottomTemp718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(BottomTemp718$Value))

for(i in 1:length(BottomTemp718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (BottomTemp718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (BottomTemp718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (BottomTemp718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (BottomTemp718Time[i] >= Process718Time[j] && BottomTemp718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
BottomTemp718$Process <- Process718
BottomTemp718$Build <- "718"

colnames(BottomTemp718) <- list("Time","Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBottomTemp718Freq <- subset(BottomTemp718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempBottomTemp718Freq <- subset(tempBottomTemp718Freq, tempBottomTemp718Freq$Layer == j)
      N[i] = length(which(temptempBottomTemp718Freq==Process718[i]))
    }
  }
}

ProcessFreqBottomTemp <- cbind.data.frame(Layer, Process718 , N)
SummaryBottomTempFreq <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=sum)
colnames(SummaryBottomTempFreq) <- c("Process", "Frequency")

SummaryBottomTempMean <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=mean)
colnames(SummaryBottomTempMean) <- c("Process", "Average")

SummaryBottomTemp718 <- cbind.data.frame(SummaryBottomTempFreq, SummaryBottomTempMean [c(2)])
SummaryBottomTemp718 <- SummaryBottomTemp718[order(-SummaryBottomTemp718$Frequency),]

# #Plotting around different layers
# BottomTemp718$Time <- as.POSIXct(BottomTemp718$Time,format="%H:%M:%OS")
# BottomTemp718$Value <- as.numeric(BottomTemp718$Value)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(BottomTemp718$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(BottomTemp718$Layer)
#     GroupEnd=GroupStart+4
#     tempBottomTemp718 <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp718New <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp718ProcessPlot <- ggplot(data = BottomTemp718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBottomTemp718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp718New, mapping = aes(x = Time, y = Value))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BottomTemp718New$Layer)) {
#         if(BottomTemp718New$Layer[j]==Defects718$Layer[i]) {BottomTemp718ProcessPlot = BottomTemp718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp718ProcessPlot)}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BottomTemp718$Layer)
#     tempBottomTemp718 <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp718New <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp718ProcessPlot <- ggplot(data = tempBottomTemp718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BottomTemp718New$Layer)) {
#         if(BottomTemp718New$Layer[j]==Defects718$Layer[i]) {BottomTemp718ProcessPlot = BottomTemp718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp718ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBottomTemp718 <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp718New <- filter(BottomTemp718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp718ProcessPlot <- ggplot(data = tempBottomTemp718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BottomTemp718New$Layer)) {
#         if(BottomTemp718New$Layer[j]==Defects718$Layer[i]) {BottomTemp718ProcessPlot = BottomTemp718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp718ProcessPlot)
#   }
# }

##################
#### Build723 ####
##################

BottomTemp723 <- subset(Build723 [c(2,3,6)], X2=="OPC.Temperature.BottomTemperature")

#Add layer to Bottom Temperature and create Plot
Time_new <- BottomTemp723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

BottomTemp723$Layer <- Layer_new
colnames(BottomTemp723) <- list("Time", "Variable", "Value", "Layer")
head(BottomTemp723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BottomTemp723 <- subset(BottomTemp723, Layer > 80 & Layer < max(Defects723Layer))
head(BottomTemp723)

test<- BottomTemp723
test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
test$Value <- as.numeric(test$Value)
test <- subset(test, Layer>85 & Layer<335)
ggplot(data = test) + 
  geom_line(mapping = aes(x = Time, y = Value))
ggplot(data = subset(test, Layer>85 & Layer<335)) +
  geom_line(mapping = aes(x = Time, y = Value))

### Assign processes within each layer
BottomTemp723Time <- BottomTemp723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(BottomTemp723$Value))

for(i in 1:length(BottomTemp723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (BottomTemp723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (BottomTemp723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (BottomTemp723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (BottomTemp723Time[i] >= Process723Time[j] && BottomTemp723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
BottomTemp723$Process <- Process723
BottomTemp723$Build <- "723"

colnames(BottomTemp723) <- list("Time","Variable", "Value", "Layer", "Process","Build")

#Create a table to count the number of times an action is performed in each layer
tempBottomTemp723Freq <- subset(BottomTemp723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempBottomTemp723Freq <- subset(tempBottomTemp723Freq, tempBottomTemp723Freq$Layer == j)
      N[i] = length(which(temptempBottomTemp723Freq==Process723[i]))
    }
  }
}

ProcessFreqBottomTemp <- cbind.data.frame(Layer, Process723 , N)
SummaryBottomTempFreq <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=sum)
colnames(SummaryBottomTempFreq) <- c("Process", "Frequency")

SummaryBottomTempMean <- aggregate(ProcessFreqBottomTemp$N, by=(list((ProcessFreqBottomTemp$Process))), FUN=mean)
colnames(SummaryBottomTempMean) <- c("Process", "Average")

SummaryBottomTemp723 <- cbind.data.frame(SummaryBottomTempFreq, SummaryBottomTempMean [c(2)])
SummaryBottomTemp723 <- SummaryBottomTemp723[order(-SummaryBottomTemp723$Frequency),]

# #Plotting around different layers
# BottomTemp723$Time <- as.POSIXct(BottomTemp723$Time,format="%H:%M:%OS")
# BottomTemp723$Value <- as.numeric(BottomTemp723$Value)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(BottomTemp723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(BottomTemp723$Layer)
#     GroupEnd=GroupStart+4
#     tempBottomTemp723 <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp723New <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp723ProcessPlot <- ggplot(data = BottomTemp723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempBottomTemp723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp723New, mapping = aes(x = Time, y = Value))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BottomTemp723New$Layer)) {
#         if(BottomTemp723New$Layer[j]==Defects723$Layer[i]) {BottomTemp723ProcessPlot = BottomTemp723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp723ProcessPlot)}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BottomTemp723$Layer)
#     tempBottomTemp723 <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp723New <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp723ProcessPlot <- ggplot(data = tempBottomTemp723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BottomTemp723New$Layer)) {
#         if(BottomTemp723New$Layer[j]==Defects723$Layer[i]) {BottomTemp723ProcessPlot = BottomTemp723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp723ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBottomTemp723 <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     BottomTemp723New <- filter(BottomTemp723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BottomTemp723ProcessPlot <- ggplot(data = tempBottomTemp723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BottomTemp723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Bottom Temperature") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Bottom Temperature \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BottomTemp723New$Layer)) {
#         if(BottomTemp723New$Layer[j]==Defects723$Layer[i]) {BottomTemp723ProcessPlot = BottomTemp723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(BottomTemp723ProcessPlot)
#   }
# }

#########################################

#####Combine all datasets####
BottomTempAll <- rbind(BottomTemp629,
                       BottomTemp703,
                       BottomTemp710,
                       BottomTemp718,
                       BottomTemp723)

BottomTempAll$Layer_ID <- paste(BottomTempAll$Build, "/", BottomTempAll$Layer)

#########################################

################################################
############## Column Temperature ##############
################################################
##################
#### Build629 ####
##################

ColumnTemp629 <- subset(Build629 [c(2,3,6)], X2=="OPC.Temperature.ColumnTemperature")

Time_new <- ColumnTemp629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

ColumnTemp629$Layer <- Layer_new
ColumnTemp629$Build <- "629"

ColumnTemp629 <- subset(ColumnTemp629, Layer>"85" & Layer < max(Defects629Layer))
head(ColumnTemp629)

##################
#### Build703 ####
##################

ColumnTemp703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.Temperature.ColumnTemperature" & Date =="2018-07-05")
ColumnTemp703 <- subset(ColumnTemp703[c(2,3,4)])

Time_new <- ColumnTemp703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

ColumnTemp703$Layer <- Layer_new
ColumnTemp703$Build <- "703"

ColumnTemp703 <- subset(ColumnTemp703, Layer>"85" & Layer <= max(Defects703Layer))
head(ColumnTemp703)

##################
#### Build710 ####
##################

ColumnTemp710 <- subset(Build710 [c(2,3,6)], X2=="OPC.Temperature.ColumnTemperature")

Time_new <- ColumnTemp710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

ColumnTemp710$Layer <- Layer_new
ColumnTemp710$Build <- "710"

ColumnTemp710 <- subset(ColumnTemp710, Layer>"85" & Layer <= max(Defects710Layer))
head(ColumnTemp710)

##################
#### Build718 ####
##################

ColumnTemp718 <- subset(Build718 [c(2,3,6)], X2=="OPC.Temperature.ColumnTemperature")

Time_new <- ColumnTemp718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

ColumnTemp718$Layer <- Layer_new
ColumnTemp718$Build <- "718"

ColumnTemp718 <- subset(ColumnTemp718, Layer>"85" & Layer < max(Defects718Layer))
head(ColumnTemp718)

##################
#### Build723 ####
##################

ColumnTemp723 <- subset(Build723 [c(2,3,6)], X2=="OPC.Temperature.ColumnTemperature")

Time_new <- ColumnTemp723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

ColumnTemp723$Layer <- Layer_new
ColumnTemp723$Build <- "723"

ColumnTemp723 <- subset(ColumnTemp723, Layer>"85" & Layer <= max(Defects723Layer))
head(ColumnTemp723)

#########################################

#####Combine all datasets####
ColumnTempAll <- rbind(ColumnTemp629,
                       ColumnTemp703,
                       ColumnTemp710,
                       ColumnTemp718,
                       ColumnTemp723)

ColumnTempAll$Layer_ID <- paste(ColumnTempAll$Build, "/", ColumnTempAll$Layer)

###############################################
############### Last Layer Time ###############
###############################################

##################
#### Build629 ####
##################

LastLayerTime629 <- subset(Build629 [c(2,3,6)], X2=="Builds.State.CurrentBuild.LastLayerTime")

#Add layer to Last Layer Time and create plot
Time_new <- LastLayerTime629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

LastLayerTime629$Layer <- Layer_new
colnames(LastLayerTime629) <- list("Time", "Variable", "Value", "Layer")
head(LastLayerTime629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
LastLayerTime629 <- subset(LastLayerTime629, Layer > 80 & Layer < 335)
head(LastLayerTime629)

#Last Layer Time plots
LastLayerTime629$Value <- as.numeric(LastLayerTime629$Value)
ggplot(data = subset(LastLayerTime629, Layer>85 & Layer<335)) + 
  geom_line(mapping = aes(x = Layer, y = Value)) + 
  ylab("Time for Last Layer") +
  xlab("Layer #") +
  ggtitle("Time for Each Layer")

### Assign processes within each layer
LastLayerTime629Time <- LastLayerTime629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(LastLayerTime629$Value))

for(i in 1:length(LastLayerTime629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (LastLayerTime629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (LastLayerTime629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (LastLayerTime629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (LastLayerTime629Time[i] >= Process629Time[j] && LastLayerTime629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
LastLayerTime629$Process <- Process629
LastLayerTime629$Build <- "629"

colnames(LastLayerTime629) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempLastLayerTime629Freq <- subset(LastLayerTime629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempLastLayerTime629Freq <- subset(tempLastLayerTime629Freq, tempLastLayerTime629Freq$Layer == j)
      N[i] = length(which(temptempLastLayerTime629Freq==Process629[i]))
    }
  }
}


ProcessFreqLastLayerTime <- cbind.data.frame(Layer, Process629 , N)
SummaryLastLayerTimeFreq <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=sum)
colnames(SummaryLastLayerTimeFreq) <- c("Process", "Frequency")

SummaryLastLayerTimeMean <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=mean)
colnames(SummaryLastLayerTimeMean) <- c("Process", "Average")

SummaryLastLayerTime629 <- cbind.data.frame(SummaryLastLayerTimeFreq, SummaryLastLayerTimeMean [c(2)])
SummaryLastLayerTime629 <- SummaryLastLayerTime629[order(-SummaryLastLayerTime629$Frequency),]

# #Plotting around different layers
# LastLayerTime629$Time <- as.POSIXct(LastLayerTime629$Time,format="%H:%M:%OS")
# LastLayerTime629$Value <- as.numeric(LastLayerTime629$Value)
# 
# LastLayerTime629ProcessesPlot <- subset(LastLayerTime629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(LastLayerTime629ProcessesPlot$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime629ProcessesPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime629 <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime629New <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Last Layer Time/","LastLayerTime629ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime629ProcessPlot <- ggplot(data = LastLayerTime629New, aes(x=Layer, y=Value)) + 
#       geom_point(data=tempLastLayerTime629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime629New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(LastLayerTime629New$Layer)) {
#         if(LastLayerTime629New$Layer[j]==Defects629$Layer[i]) {LastLayerTime629ProcessPlot = LastLayerTime629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime629ProcessesPlot$Layer)
#     tempLastLayerTime629 <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime629New <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Last Layer Time/","LastLayerTime629ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime629ProcessPlot <- ggplot(data = tempLastLayerTime629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(LastLayerTime629New$Layer)) {
#         if(LastLayerTime629New$Layer[j]==Defects629$Layer[i]) {LastLayerTime629ProcessPlot = LastLayerTime629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime629 <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime629New <- filter(LastLayerTime629ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Last Layer Time/","LastLayerTime629ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime629ProcessPlot <- ggplot(data = tempLastLayerTime629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(LastLayerTime629New$Layer)) {
#         if(LastLayerTime629New$Layer[j]==Defects629$Layer[i]) {LastLayerTime629ProcessPlot = LastLayerTime629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

LastLayerTime703 <- subset(Build703 [c(1,2,3,6)], X2=="Builds.State.CurrentBuild.LastLayerTime" & Date =="2018-07-05")
LastLayerTime703 <- subset(LastLayerTime703[c(2,3,4)])


#Add layer to Last Layer Time and create plot
Time_new <- LastLayerTime703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

LastLayerTime703$Layer <- Layer_new
colnames(LastLayerTime703) <- list("Time", "Variable", "Value", "Layer")
head(LastLayerTime703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
LastLayerTime703 <- subset(LastLayerTime703, Layer > 80 & Layer <= 335)
head(LastLayerTime703)

#Last Layer Time plots
LastLayerTime703$Value <- as.numeric(LastLayerTime703$Value)
ggplot(data = subset(LastLayerTime703, Layer>85 & Layer<335)) + 
  geom_line(mapping = aes(x = Layer, y = Value)) + 
  ylab("Time for Last Layer") +
  xlab("Layer #") +
  ggtitle("Time for Each Layer")

### Assign processes within each layer
LastLayerTime703Time <- LastLayerTime703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(LastLayerTime703$Value))

for(i in 1:length(LastLayerTime703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (LastLayerTime703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (LastLayerTime703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (LastLayerTime703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (LastLayerTime703Time[i] >= Process703Time[j] && LastLayerTime703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
LastLayerTime703$Process <- Process703
LastLayerTime703$Build <- "703"

colnames(LastLayerTime703) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempLastLayerTime703Freq <- subset(LastLayerTime703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempLastLayerTime703Freq <- subset(tempLastLayerTime703Freq, tempLastLayerTime703Freq$Layer == j)
      N[i] = length(which(temptempLastLayerTime703Freq==Process703[i]))
    }
  }
}


ProcessFreqLastLayerTime <- cbind.data.frame(Layer, Process703 , N)
SummaryLastLayerTimeFreq <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=sum)
colnames(SummaryLastLayerTimeFreq) <- c("Process", "Frequency")

SummaryLastLayerTimeMean <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=mean)
colnames(SummaryLastLayerTimeMean) <- c("Process", "Average")

SummaryLastLayerTime703 <- cbind.data.frame(SummaryLastLayerTimeFreq, SummaryLastLayerTimeMean [c(2)])
SummaryLastLayerTime703 <- SummaryLastLayerTime703[order(-SummaryLastLayerTime703$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList703 <- unique(DefectList703)
# 
# LastLayerTime703$Time <- as.POSIXct(LastLayerTime703$Time,format="%H:%M:%OS")
# LastLayerTime703$Value <- as.numeric(LastLayerTime703$Value)
# 
# LastLayerTime20BeforeDefect703 <- subset(LastLayerTime703, Layer >= DefectList703$Layer[1]-20 & Layer <= DefectList703$Layer[1]-1)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(LastLayerTime20BeforeDefect703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect703$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime703 <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Last Layer Time/","LastLayerTime20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = LastLayerTime703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect703$Layer)
#     tempLastLayerTime703 <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Last Layer Time/","LastLayerTime20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = tempLastLayerTime703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime703 <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Last Layer Time/","LastLayerTime20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = tempLastLayerTime703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempLastLayerTime703 <- filter(LastLayerTime703, Layer == DefectList703$Layer[1])
# LastLayerTime703New <- filter(LastLayerTime703, Layer == DefectList703$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Last Layer Time/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# LastLayerTime703ProcessPlot <- ggplot(data = tempLastLayerTime703, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value))+
#   ylab("Last Layer Time") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Last Layer Time \n Layer", DefectList703$Layer[1]))
# 
# plot(LastLayerTime703ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# LastLayerTime703$Time <- as.POSIXct(LastLayerTime703$Time,format="%H:%M:%OS")
# LastLayerTime703$Value <- as.numeric(LastLayerTime703$Value)
# 
# LastLayerTime703ProcessesPlot <- subset(LastLayerTime703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(LastLayerTime703ProcessesPlot$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime703ProcessesPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime703 <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Last Layer Time/","LastLayerTime703ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = LastLayerTime703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime703ProcessesPlot$Layer)
#     tempLastLayerTime703 <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Last Layer Time/","LastLayerTime703ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = tempLastLayerTime703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime703 <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime703New <- filter(LastLayerTime703ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Last Layer Time/","LastLayerTime703ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime703ProcessPlot <- ggplot(data = tempLastLayerTime703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(LastLayerTime703New$Layer)) {
#         if(LastLayerTime703New$Layer[j]==Defects703$Layer[i]) {LastLayerTime703ProcessPlot = LastLayerTime703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build710 ####
##################

LastLayerTime710 <- subset(Build710 [c(2,3,6)], X2=="Builds.State.CurrentBuild.LastLayerTime")

#Add layer to Last Layer Time and create plot
Time_new <- LastLayerTime710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

LastLayerTime710$Layer <- Layer_new
colnames(LastLayerTime710) <- list("Time", "Variable", "Value", "Layer")
head(LastLayerTime710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
LastLayerTime710 <- subset(LastLayerTime710, Layer > 80 & Layer <= 335)
head(LastLayerTime710)

#Last Layer Time plots
LastLayerTime710$Value <- as.numeric(LastLayerTime710$Value)
ggplot(data = subset(LastLayerTime710, Layer>85 & Layer<335)) + 
  geom_line(mapping = aes(x = Layer, y = Value)) + 
  ylab("Time for Last Layer") +
  xlab("Layer #") +
  ggtitle("Time for Each Layer")

### Assign processes within each layer
LastLayerTime710Time <- LastLayerTime710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(LastLayerTime710$Value))

for(i in 1:length(LastLayerTime710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (LastLayerTime710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (LastLayerTime710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (LastLayerTime710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (LastLayerTime710Time[i] >= Process710Time[j] && LastLayerTime710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
LastLayerTime710$Process <- Process710
LastLayerTime710$Build <- "710"

colnames(LastLayerTime710) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempLastLayerTime710Freq <- subset(LastLayerTime710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempLastLayerTime710Freq <- subset(tempLastLayerTime710Freq, tempLastLayerTime710Freq$Layer == j)
      N[i] = length(which(temptempLastLayerTime710Freq==Process710[i]))
    }
  }
}


ProcessFreqLastLayerTime <- cbind.data.frame(Layer, Process710 , N)
SummaryLastLayerTimeFreq <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=sum)
colnames(SummaryLastLayerTimeFreq) <- c("Process", "Frequency")

SummaryLastLayerTimeMean <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=mean)
colnames(SummaryLastLayerTimeMean) <- c("Process", "Average")

SummaryLastLayerTime710 <- cbind.data.frame(SummaryLastLayerTimeFreq, SummaryLastLayerTimeMean [c(2)])
SummaryLastLayerTime710 <- SummaryLastLayerTime710[order(-SummaryLastLayerTime710$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList710 <- unique(DefectList710)
# 
# LastLayerTime710$Time <- as.POSIXct(LastLayerTime710$Time,format="%H:%M:%OS")
# LastLayerTime710$Value <- as.numeric(LastLayerTime710$Value)
# 
# LastLayerTime20BeforeDefect710 <- subset(LastLayerTime710, Layer >= DefectList710$Layer[1]-20 & Layer <= DefectList710$Layer[1]-1)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(LastLayerTime20BeforeDefect710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect710$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime710 <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Last Layer Time/","LastLayerTime20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = LastLayerTime710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect710$Layer)
#     tempLastLayerTime710 <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Last Layer Time/","LastLayerTime20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = tempLastLayerTime710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime710 <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Last Layer Time/","LastLayerTime20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = tempLastLayerTime710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempLastLayerTime710 <- filter(LastLayerTime710, Layer == DefectList710$Layer[1])
# LastLayerTime710New <- filter(LastLayerTime710, Layer == DefectList710$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Last Layer Time/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# LastLayerTime710ProcessPlot <- ggplot(data = tempLastLayerTime710, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value))+
#   ylab("Last Layer Time") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Last Layer Time \n Layer", DefectList710$Layer[1]))
# 
# plot(LastLayerTime710ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# LastLayerTime710$Time <- as.POSIXct(LastLayerTime710$Time,format="%H:%M:%OS")
# LastLayerTime710$Value <- as.numeric(LastLayerTime710$Value)
# 
# LastLayerTime710ProcessesPlot <- subset(LastLayerTime710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(LastLayerTime710ProcessesPlot$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime710ProcessesPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime710 <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Last Layer Time/","LastLayerTime710ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = LastLayerTime710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime710ProcessesPlot$Layer)
#     tempLastLayerTime710 <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Last Layer Time/","LastLayerTime710ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = tempLastLayerTime710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime710 <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime710New <- filter(LastLayerTime710ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Last Layer Time/","LastLayerTime710ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime710ProcessPlot <- ggplot(data = tempLastLayerTime710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(LastLayerTime710New$Layer)) {
#         if(LastLayerTime710New$Layer[j]==Defects710$Layer[i]) {LastLayerTime710ProcessPlot = LastLayerTime710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build718 ####
##################

LastLayerTime718 <- subset(Build718 [c(2,3,6)], X2=="Builds.State.CurrentBuild.LastLayerTime")

#Add layer to Last Layer Time and create plot
Time_new <- LastLayerTime718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

LastLayerTime718$Layer <- Layer_new
colnames(LastLayerTime718) <- list("Time", "Variable", "Value", "Layer")
head(LastLayerTime718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
LastLayerTime718 <- subset(LastLayerTime718, Layer > 80 & Layer < 335)
head(LastLayerTime718)

#Last Layer Time plots
LastLayerTime718$Value <- as.numeric(LastLayerTime718$Value)
ggplot(data = subset(LastLayerTime718, Layer>85 & Layer<335)) + 
  geom_line(mapping = aes(x = Layer, y = Value)) + 
  ylab("Time for Last Layer") +
  xlab("Layer #") +
  ggtitle("Time for Each Layer")

### Assign processes within each layer
LastLayerTime718Time <- LastLayerTime718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(LastLayerTime718$Value))

for(i in 1:length(LastLayerTime718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (LastLayerTime718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (LastLayerTime718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (LastLayerTime718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (LastLayerTime718Time[i] >= Process718Time[j] && LastLayerTime718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
LastLayerTime718$Process <- Process718
LastLayerTime718$Build <- "718"

colnames(LastLayerTime718) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempLastLayerTime718Freq <- subset(LastLayerTime718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempLastLayerTime718Freq <- subset(tempLastLayerTime718Freq, tempLastLayerTime718Freq$Layer == j)
      N[i] = length(which(temptempLastLayerTime718Freq==Process718[i]))
    }
  }
}


ProcessFreqLastLayerTime <- cbind.data.frame(Layer, Process718 , N)
SummaryLastLayerTimeFreq <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=sum)
colnames(SummaryLastLayerTimeFreq) <- c("Process", "Frequency")

SummaryLastLayerTimeMean <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=mean)
colnames(SummaryLastLayerTimeMean) <- c("Process", "Average")

SummaryLastLayerTime718 <- cbind.data.frame(SummaryLastLayerTimeFreq, SummaryLastLayerTimeMean [c(2)])
SummaryLastLayerTime718 <- SummaryLastLayerTime718[order(-SummaryLastLayerTime718$Frequency),]

quantile(LastLayerTime718$Value, c(.9, .95, .975, .99, 1))
# ~97.5% of all layers are less than 79 seconds long

# #### Plotting 20 Layers Before the Defect
# Defects <- c(104,223,254)
# 
# LastLayerTime718$Time <- as.POSIXct(LastLayerTime718$Time,format="%H:%M:%OS")
# LastLayerTime718$Value <- as.numeric(LastLayerTime718$Value)
# 
# LastLayerTime20BeforeDefect718_1 <- subset(LastLayerTime718, Layer >= Defects[1]-20 & Layer <=Defects[1]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(LastLayerTime20BeforeDefect718_1$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect718_1$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = LastLayerTime718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect718_1$Layer)
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# LastLayerTime20BeforeDefect718_2 <- subset(LastLayerTime718, Layer >= Defects[2]-20 & Layer <=Defects[2]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(LastLayerTime20BeforeDefect718_2$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect718_2$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = LastLayerTime718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect718_2$Layer)
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# LastLayerTime20BeforeDefect718_3 <- subset(LastLayerTime718, Layer >= Defects[3]-20 & Layer <=Defects[3]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(LastLayerTime20BeforeDefect718_3$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect718_3$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = LastLayerTime718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect718_3$Layer)
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","LastLayerTime20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# for(i in 1:length(Defects)) {
#   tempLastLayerTime718 <- filter(LastLayerTime718, Layer == Defects[i])
#   LastLayerTime718New <- filter(LastLayerTime718, Layer == Defects[i])
#   
#   #Set Plot location and dimensions
#   png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Last Layer Time/","DefectPlot",i,".png" ,sep = ""),
#       width = 999,
#       height = 333)
#   
#   #Create Plot
#   theme_update(plot.title = element_text(hjust = 0.5))
#   LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#     geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#     geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#     ylab("Last Layer Time") +
#     xlab("Time") +
#     ggtitle(paste("Processes of Last Layer Time \n Layer", Defects[i]))
#   
#   plot(LastLayerTime718ProcessPlot)
#   
#   dev.off()
#   
# }

# #Plotting around different layers
# LastLayerTime718$Time <- as.POSIXct(LastLayerTime718$Time,format="%H:%M:%OS")
# LastLayerTime718$Value <- as.numeric(LastLayerTime718$Value)
# 
# LastLayerTime718ProcessesPlot <- subset(LastLayerTime718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(LastLayerTime718ProcessesPlot$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime718ProcessesPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Last Layer Time/","LastLayerTime718ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = LastLayerTime718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime718ProcessesPlot$Layer)
#     tempLastLayerTime718 <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Last Layer Time/","LastLayerTime718ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime718 <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime718New <- filter(LastLayerTime718ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Last Layer Time/","LastLayerTime718ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime718ProcessPlot <- ggplot(data = tempLastLayerTime718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(LastLayerTime718New$Layer)) {
#         if(LastLayerTime718New$Layer[j]==Defects718$Layer[i]) {LastLayerTime718ProcessPlot = LastLayerTime718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build723 ####
##################

LastLayerTime723 <- subset(Build723 [c(2,3,6)], X2=="Builds.State.CurrentBuild.LastLayerTime")

#Add layer to Last Layer Time and create plot
Time_new <- LastLayerTime723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

LastLayerTime723$Layer <- Layer_new
colnames(LastLayerTime723) <- list("Time", "Variable", "Value", "Layer")
head(LastLayerTime723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
LastLayerTime723 <- subset(LastLayerTime723, Layer > 80 & Layer <= 335)
head(LastLayerTime723)

#Last Layer Time plots
LastLayerTime723$Value <- as.numeric(LastLayerTime723$Value)
ggplot(data = subset(LastLayerTime723, Layer>85 & Layer<335)) + 
  geom_line(mapping = aes(x = Layer, y = Value)) + 
  ylab("Time for Last Layer") +
  xlab("Layer #") +
  ggtitle("Time for Each Layer")

### Assign processes within each layer
LastLayerTime723Time <- LastLayerTime723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(LastLayerTime723$Value))

for(i in 1:length(LastLayerTime723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (LastLayerTime723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (LastLayerTime723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (LastLayerTime723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (LastLayerTime723Time[i] >= Process723Time[j] && LastLayerTime723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
LastLayerTime723$Process <- Process723
LastLayerTime723$Build <- "723"

colnames(LastLayerTime723) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempLastLayerTime723Freq <- subset(LastLayerTime723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempLastLayerTime723Freq <- subset(tempLastLayerTime723Freq, tempLastLayerTime723Freq$Layer == j)
      N[i] = length(which(temptempLastLayerTime723Freq==Process723[i]))
    }
  }
}


ProcessFreqLastLayerTime <- cbind.data.frame(Layer, Process723 , N)
SummaryLastLayerTimeFreq <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=sum)
colnames(SummaryLastLayerTimeFreq) <- c("Process", "Frequency")

SummaryLastLayerTimeMean <- aggregate(ProcessFreqLastLayerTime$N, by=(list((ProcessFreqLastLayerTime$Process))), FUN=mean)
colnames(SummaryLastLayerTimeMean) <- c("Process", "Average")

SummaryLastLayerTime723 <- cbind.data.frame(SummaryLastLayerTimeFreq, SummaryLastLayerTimeMean [c(2)])
SummaryLastLayerTime723 <- SummaryLastLayerTime723[order(-SummaryLastLayerTime723$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList723 <- unique(DefectList723)
# 
# LastLayerTime723$Time <- as.POSIXct(LastLayerTime723$Time,format="%H:%M:%OS")
# LastLayerTime723$Value <- as.numeric(LastLayerTime723$Value)
# 
# LastLayerTime20BeforeDefect723 <- subset(LastLayerTime723, Layer >= DefectList723$Layer[4]-20 & Layer <= DefectList723$Layer[4]-1)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(LastLayerTime20BeforeDefect723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime20BeforeDefect723$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime723 <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Last Layer Time/","LastLayerTime20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = LastLayerTime723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime20BeforeDefect723$Layer)
#     tempLastLayerTime723 <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Last Layer Time/","LastLayerTime20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = tempLastLayerTime723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime723 <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Last Layer Time/","LastLayerTime20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = tempLastLayerTime723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempLastLayerTime723 <- filter(LastLayerTime723, Layer == DefectList723$Layer[4])
# LastLayerTime723New <- filter(LastLayerTime723, Layer == DefectList723$Layer[4])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Last Layer Time/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# LastLayerTime723ProcessPlot <- ggplot(data = tempLastLayerTime723, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value))+
#   ylab("Last Layer Time") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Last Layer Time \n Layer", DefectList723$Layer[4]))
# 
# plot(LastLayerTime723ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# LastLayerTime723$Time <- as.POSIXct(LastLayerTime723$Time,format="%H:%M:%OS")
# LastLayerTime723$Value <- as.numeric(LastLayerTime723$Value)
# 
# LastLayerTime723ProcessesPlot <- subset(LastLayerTime723, Layer > 85)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(LastLayerTime723ProcessesPlot$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(LastLayerTime723ProcessesPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempLastLayerTime723 <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Last Layer Time/","LastLayerTime723ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = LastLayerTime723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempLastLayerTime723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(LastLayerTime723ProcessesPlot$Layer)
#     tempLastLayerTime723 <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Last Layer Time/","LastLayerTime723ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = tempLastLayerTime723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempLastLayerTime723 <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     LastLayerTime723New <- filter(LastLayerTime723ProcessesPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Last Layer Time/","LastLayerTime723ProcessesPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     LastLayerTime723ProcessPlot <- ggplot(data = tempLastLayerTime723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=LastLayerTime723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Last Layer Time") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Last Layer Time \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(LastLayerTime723New$Layer)) {
#         if(LastLayerTime723New$Layer[j]==Defects723$Layer[i]) {LastLayerTime723ProcessPlot = LastLayerTime723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(LastLayerTime723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#########################################

#####Combine all datasets####
LastLayerTimeAll <- rbind(LastLayerTime629,
                       LastLayerTime703,
                       LastLayerTime710,
                       LastLayerTime718,
                       LastLayerTime723)

LastLayerTimeAll$Layer_ID <- paste(LastLayerTimeAll$Build, "/", LastLayerTimeAll$Layer)

###########################################################################################################################

########################### Defect: Powder Issue ####################################

################################################
################# Pulse Length ################# 
################################################

##################
#### Build629 ####
##################

## Special Check ##

# RakeLeftSensor_Pulse <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse")
# RakeLeftSensor_Pulse2 <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse2")
# RakeRightSensor_Pulse <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse")
# RakeRightSensor_Pulse2 <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse2")
# 
# RakeLeftSensor_PulseChanged <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged")
# RakeLeftSensor_PulseChanged2 <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged2")
# RakeRightSensor_PulseChanged <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged")
# RakeRightSensor_PulseChanged2 <- subset(Build629 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged2")
# 
# RakeSensor_PulseChanged <- subset(Build629 [c(2,3,6)], X2=="Process.RakeControl.SensorPulseChanged")
# 
# RakeLeftSensor_Pulse$X5 <- as.numeric(RakeLeftSensor_Pulse$X5)
# RakeLeftSensor_Pulse2$X5 <- as.numeric(RakeLeftSensor_Pulse2$X5)
# RakeRightSensor_Pulse$X5 <- as.numeric(RakeRightSensor_Pulse$X5)
# RakeRightSensor_Pulse2$X5 <- as.numeric(RakeRightSensor_Pulse2$X5)
# 
# NewDatasets <- list(RakeLeftSensor_Pulse,
#                     RakeLeftSensor_Pulse2,
#                     RakeRightSensor_Pulse,
#                     RakeRightSensor_Pulse2,
#                     RakeLeftSensor_PulseChanged,
#                     RakeLeftSensor_PulseChanged2,
#                     RakeRightSensor_PulseChanged,
#                     RakeRightSensor_PulseChanged2,
#                     RakeSensor_PulseChanged)
# 
# for(z in 1:length(NewDatasets)) {
#   #Add layer to Pulse Length and create a plot
#   Time_new <- NewDatasets[[z]]$Time
#   Layer_new <- rep(0, length(Time_new))
#   
#   for(i in 1:length(Time_new)) {
#     for(j in 1:length(Defects629Time)) {
#       if (j == 1) { 
#         if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
#         else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
#       else if (j == length(Defects629Time)) { 
#         if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
#       else {
#         if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
#           Layer_new[i] = Defects629Layer[j]}
#       }
#     }
#   }
#   
#   NewDatasets[[z]]$Layer <- Layer_new
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer")
#   head(NewDatasets[[z]])
#   
#   #Only look at layers excluding the support (> 85) and the final layer (< 335)
#   NewDatasets[[z]] <- subset(NewDatasets[[z]], Layer > 80 & Layer < max(Defects629Layer))
#   head(NewDatasets[[z]])
#   
#   ### Assign processes within each layer
#   PulseLengthTime <- NewDatasets[[z]]$Time
#   tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
#   ProcessTime <- tempOutputDescription629$Time
#   ProcessPosition <- tempOutputDescription629$Process
#   Process <- rep(0, length(NewDatasets[[z]]$Value))
#   
#   for(k in 1:length(PulseLengthTime)) {
#     for(l in 1:length(ProcessTime)) {
#       if (l == 1) { 
#         if (PulseLengthTime[k] < ProcessTime[l]) { Process[k]=ProcessPosition[l-1] }
#         else if (PulseLengthTime[k] <= ProcessTime[l+1]) { Process[k]=ProcessPosition[l]}}
#       else if (l == length(ProcessTime)) { 
#         if (PulseLengthTime[k] > ProcessTime[l]) { Process[k]=ProcessPosition[length(ProcessPosition)] }}
#       else {
#         if (PulseLengthTime[k] >= ProcessTime[l] && PulseLengthTime[k] < ProcessTime[l+1]) {
#           Process[k] = ProcessPosition[l]}
#       }
#     }
#   }
#   
#   head(Process)
#   NewDatasets[[z]]$Process <- Process
#   NewDatasets[[z]]$Build <- "629"
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")
# }
# 
# RakeLeftSensor_Pulse <- NewDatasets[[1]]
# RakeLeftSensor_Pulse2 <- NewDatasets[[2]]
# RakeRightSensor_Pulse <- NewDatasets[[3]]
# RakeRightSensor_Pulse2 <- NewDatasets[[4]]
# 
# RakeLeftSensor_PulseChanged <- NewDatasets[[5]]
# RakeLeftSensor_PulseChanged2 <- NewDatasets[[6]]
# RakeRightSensor_PulseChanged <- NewDatasets[[7]]
# RakeRightSensor_PulseChanged2 <- NewDatasets[[8]]
# 
# RakeSensor_PulseChanged <- NewDatasets[[9]]
# 
# RakeSensor629_Pulse <- rbind(RakeLeftSensor_Pulse, 
#                           RakeLeftSensor_Pulse2, 
#                           RakeRightSensor_Pulse, 
#                           RakeRightSensor_Pulse2)
# 
# RakeSensor629_Pulse <- RakeSensor629_Pulse[order(RakeSensor629_Pulse$Time),]
# 
# RakeSensor629_LR_PulseChanged <- rbind(RakeLeftSensor_PulseChanged, 
#                                  RakeLeftSensor_PulseChanged2, 
#                                  RakeRightSensor_PulseChanged, 
#                                  RakeRightSensor_PulseChanged2)
# 
# RakeSensor629_LR_PulseChanged <- RakeSensor629_LR_PulseChanged[order(RakeSensor629_LR_PulseChanged$Time),]
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(RakeSensor629_Pulse, Layer >= 203 & Layer <=224)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects629, X5== "LowerTable" & Layer == 223)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Rake Sensor Pulse") +
#   xlab("Time") +
#   ggtitle("Processes of Rake Sensor Pulse \n Build 629 Layer 203-223") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

#################################################################################################
PulseLength629 <- rbind(Build629[Build629$X2=="Process.RightRegulator.PulseLength",],Build629[Build629$X2=="Process.LeftRegulator.PulseLength",])
PulseLength629 <- subset(PulseLength629 [c(2,3,6)])
PulseLength629 <- PulseLength629[order(PulseLength629$Time),]

#Add layer to Pulse Length and create a plot
Time_new <- PulseLength629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

PulseLength629$Layer <- Layer_new
colnames(PulseLength629) <- list("Time", "Variable", "Value", "Layer")
head(PulseLength629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
PulseLength629 <- subset(PulseLength629, Layer > 80 & Layer < max(Defects629Layer))
head(PulseLength629)

### Assign processes within each layer
PulseLength629Time <- PulseLength629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(PulseLength629$Value))

for(i in 1:length(PulseLength629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (PulseLength629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (PulseLength629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (PulseLength629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (PulseLength629Time[i] >= Process629Time[j] && PulseLength629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
PulseLength629$Process <- Process629
PulseLength629$Build <- "629"

# #Create a table to count the number of times an action is performed in each layer
# tempPulseLength629Freq <- subset(PulseLength629 [c(4,5)],)
# 
# Process629 <- c(unique(OutputDescription629$Process))
# Process629 <- rep(Process629, length(Defects629Layer))
# Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
# N <- rep(0,length(Process629))
# 
# for(i in 1:length(Process629)) {
#   for(j in 1:length(Defects629Layer)) {
#     if(j==Layer[i]) {
#       temptempPulseLength629Freq <- subset(tempPulseLength629Freq, tempPulseLength629Freq$Layer == j)
#       N[i] = length(which(temptempPulseLength629Freq==Process629[i]))
#     }
#   }
# }
# 
# ProcessFreqPulseLength <- cbind.data.frame(Layer, Process629 , N)
# SummaryPulseLengthFreq <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=sum)
# colnames(SummaryPulseLengthFreq) <- c("Process", "Frequency")
# 
# SummaryPulseLengthMean <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=mean)
# colnames(SummaryPulseLengthMean) <- c("Process", "Average")
# 
# SummaryPulseLength629 <- cbind.data.frame(SummaryPulseLengthFreq, SummaryPulseLengthMean [c(2)])
# SummaryPulseLength629 <- SummaryPulseLength629[order(-SummaryPulseLength629$Frequency),]

# test<- PulseLength629
# test$Time <- as.POSIXct(test$Time,format="%H:%M:%OS")
# test$Value <- as.numeric(test$Value)
# test <- subset(test, Layer>85 & Layer<335)
# ggplot(data = test) + 
#   geom_line(mapping = aes(x = Time, y = Value))
# ggplot(data = subset(test, Layer>85 & Layer<335)) +
#   geom_line(mapping = aes(x = Time, y = Value))

# #Plot the old data over the new data
# RakeSensor629_Pulse$Time <- as.POSIXct(RakeSensor629_Pulse$Time,format="%H:%M:%OS")
# RakeSensor629_Pulse$Value <- as.numeric(RakeSensor629_Pulse$Value)
# PulseLength629$Time <- as.POSIXct(PulseLength629$Time,format="%H:%M:%OS")
# PulseLength629$Value <- as.numeric(PulseLength629$Value)
# 
# Part_RakeSensor_Pulse <- filter(RakeSensor629_Pulse, Layer >= 120 & Layer <= 150)
# Part_PulseLength <- filter(PulseLength629, Layer >= 120 & Layer <= 150)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulseLength_Plot <- ggplot(data = Part_RakeSensor_Pulse) + 
#                       geom_line(mapping = aes(x=Time, y=Value)) +
#                       geom_line(data = Part_PulseLength, mapping = aes(x=Time, y=Value), color = "red", lwd = 1) +
#                       ggtitle("Pulse Length vs Rake Sensor Pulse \n Build 629 Layers 120-150")
# 
# plot(PulseLength_Plot)

# #Plotting around different layers
# PulseLength629$Time <- as.POSIXct(PulseLength629$Time,format="%H:%M:%OS")
# PulseLength629$Value <- as.numeric(PulseLength629$Value)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(PulseLength629$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(PulseLength629$Layer)
#     GroupEnd=GroupStart+4
#     tempPulseLength629 <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength629New <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength629ProcessPlot <- ggplot(data = PulseLength629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempPulseLength629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength629New, mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(PulseLength629New$Layer)) {
#         if(PulseLength629New$Layer[j]==Defects629$Layer[i]) {PulseLength629ProcessPlot = PulseLength629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength629ProcessPlot)}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(PulseLength629$Layer)
#     tempPulseLength629 <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength629New <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength629ProcessPlot <- ggplot(data = tempPulseLength629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(PulseLength629New$Layer)) {
#         if(PulseLength629New$Layer[j]==Defects629$Layer[i]) {PulseLength629ProcessPlot = PulseLength629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength629ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempPulseLength629 <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength629New <- filter(PulseLength629, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength629ProcessPlot <- ggplot(data = tempPulseLength629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(PulseLength629New$Layer)) {
#         if(PulseLength629New$Layer[j]==Defects629$Layer[i]) {PulseLength629ProcessPlot = PulseLength629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength629ProcessPlot)
#   }
# }

##################
#### Build703 ####
##################

## Special Check ##

# RakeLeftSensor_Pulse <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse")
# RakeLeftSensor_Pulse2 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse2")
# RakeRightSensor_Pulse <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse")
# RakeRightSensor_Pulse2 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse2")
# 
# RakeLeftSensor_PulseChanged <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged")
# RakeLeftSensor_PulseChanged2 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged2")
# RakeRightSensor_PulseChanged <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged")
# RakeRightSensor_PulseChanged2 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged2")
# 
# RakeSensor_PulseChanged <- subset(Build703 [c(1,2,3,6)], X2=="Process.RakeControl.SensorPulseChanged")
# 
# RakeLeftSensor_Pulse$X5 <- as.numeric(RakeLeftSensor_Pulse$X5)
# RakeLeftSensor_Pulse2$X5 <- as.numeric(RakeLeftSensor_Pulse2$X5)
# RakeRightSensor_Pulse$X5 <- as.numeric(RakeRightSensor_Pulse$X5)
# RakeRightSensor_Pulse2$X5 <- as.numeric(RakeRightSensor_Pulse2$X5)
# 
# NewDatasets <- list(RakeLeftSensor_Pulse,
#                     RakeLeftSensor_Pulse2,
#                     RakeRightSensor_Pulse,
#                     RakeRightSensor_Pulse2,
#                     RakeLeftSensor_PulseChanged,
#                     RakeLeftSensor_PulseChanged2,
#                     RakeRightSensor_PulseChanged,
#                     RakeRightSensor_PulseChanged2,
#                     RakeSensor_PulseChanged)
# 
# for(z in 1:length(NewDatasets)) {
#   #Add layer to Pulse Length and create a plot
#   Time_new <- NewDatasets[[z]]$Time
#   Layer_new <- rep(0, length(Time_new))
#   
#   for(i in 1:length(Time_new)) {
#     for(j in 1:length(Defects703Time)) {
#       if (j == 1) { 
#         if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
#         else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
#       else if (j == length(Defects703Time)) { 
#         if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
#       else {
#         if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
#           Layer_new[i] = Defects703Layer[j]}
#       }
#     }
#   }
#   
#   NewDatasets[[z]]$Layer <- Layer_new
#   colnames(NewDatasets[[z]]) <- list("Date", "Time", "Variable", "Value", "Layer")
#   head(NewDatasets[[z]])
#   
#   #Only look at layers excluding the support (> 85) and the final layer (< 335)
#   NewDatasets[[z]] <- subset(NewDatasets[[z]], Layer > 80 & Layer <= max(Defects703Layer))
#   head(NewDatasets[[z]])
#   
#   ### Assign processes within each layer
#   PulseLengthTime <- NewDatasets[[z]]$Time
#   tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
#   ProcessTime <- tempOutputDescription703$Time
#   ProcessPosition <- tempOutputDescription703$Process
#   Process <- rep(0, length(NewDatasets[[z]]$Value))
#   
#   for(k in 1:length(PulseLengthTime)) {
#     for(l in 1:length(ProcessTime)) {
#       if (l == 1) { 
#         if (PulseLengthTime[k] < ProcessTime[l]) { Process[k]=ProcessPosition[l-1] }
#         else if (PulseLengthTime[k] <= ProcessTime[l+1]) { Process[k]=ProcessPosition[l]}}
#       else if (l == length(ProcessTime)) { 
#         if (PulseLengthTime[k] > ProcessTime[l]) { Process[k]=ProcessPosition[length(ProcessPosition)] }}
#       else {
#         if (PulseLengthTime[k] >= ProcessTime[l] && PulseLengthTime[k] < ProcessTime[l+1]) {
#           Process[k] = ProcessPosition[l]}
#       }
#     }
#   }
#   
#   head(Process)
#   NewDatasets[[z]]$Process <- Process
#   NewDatasets[[z]]$Build <- "703"
#   colnames(NewDatasets[[z]]) <- list("Date", "Time", "Variable", "Value", "Layer", "Process", "Build")
# }
# 
# RakeLeftSensor_Pulse <- NewDatasets[[1]]
# RakeLeftSensor_Pulse2 <- NewDatasets[[2]]
# RakeRightSensor_Pulse <- NewDatasets[[3]]
# RakeRightSensor_Pulse2 <- NewDatasets[[4]]
# 
# RakeLeftSensor_PulseChanged <- NewDatasets[[5]]
# RakeLeftSensor_PulseChanged2 <- NewDatasets[[6]]
# RakeRightSensor_PulseChanged <- NewDatasets[[7]]
# RakeRightSensor_PulseChanged2 <- NewDatasets[[8]]
# 
# RakeSensor_PulseChanged <- NewDatasets[[9]]
# 
# RakeSensor703_Pulse <- rbind(RakeLeftSensor_Pulse, 
#                              RakeLeftSensor_Pulse2, 
#                              RakeRightSensor_Pulse, 
#                              RakeRightSensor_Pulse2)
# 
# RakeSensor703_Pulse <- filter(RakeSensor703_Pulse, Date =="2018-07-05")
# RakeSensor703_Pulse <- RakeSensor703_Pulse[order(RakeSensor703_Pulse$Time),]
# RakeSensor703_Pulse <- RakeSensor703_Pulse[c(2,3,4,5,6,7)]
# 
# RakeSensor703_LR_PulseChanged <- rbind(RakeLeftSensor_PulseChanged, 
#                                        RakeLeftSensor_PulseChanged2, 
#                                        RakeRightSensor_PulseChanged, 
#                                        RakeRightSensor_PulseChanged2)
# 
# RakeSensor703_LR_PulseChanged <- filter(RakeSensor703_LR_PulseChanged, Date =="2018-07-05")
# RakeSensor703_LR_PulseChanged <- RakeSensor703_LR_PulseChanged[order(RakeSensor703_LR_PulseChanged$Time),]
# RakeSensor703_LR_PulseChanged <- RakeSensor703_LR_PulseChanged[c(2,3,4,5,6,7)]
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(RakeSensor703_Pulse, Layer >= 203 & Layer <=224)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects703, X5== "LowerTable" & Layer == 223)
# LayerDefectStartTime$Time <- as.POSIXct(LayerDefectStartTime$Time,format="%H:%M:%OS")
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(mapping = aes(x = Time, y = Value))+
#       ylab("Rake Sensor Pulse") +
#       xlab("Time") +
#       ggtitle("Processes of Rake Sensor Pulse \n Build 703 Layer 203-223") +
#       geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

#################################################################################################

PulseLength703 <- rbind(Build703[Build703$X2=="Process.RightRegulator.PulseLength",],Build703[Build703$X2=="Process.LeftRegulator.PulseLength",])
PulseLength703 <- filter(PulseLength703, Date =="2018-07-05")
PulseLength703 <- subset(PulseLength703 [c(2,3,6)])
PulseLength703 <- PulseLength703[order(PulseLength703$Time),]


#Add layer to Pulse Length and create a plot
Time_new <- PulseLength703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

PulseLength703$Layer <- Layer_new
colnames(PulseLength703) <- list("Time", "Variable", "Value", "Layer")
head(PulseLength703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
PulseLength703 <- subset(PulseLength703, Layer > 80 & Layer <= max(Defects703Layer))
head(PulseLength703)

### Assign processes within each layer
PulseLength703Time <- PulseLength703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(PulseLength703$Value))

for(i in 1:length(PulseLength703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (PulseLength703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (PulseLength703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (PulseLength703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (PulseLength703Time[i] >= Process703Time[j] && PulseLength703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
PulseLength703$Process <- Process703
PulseLength703$Build <- "703"
colnames(PulseLength703) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempPulseLength703Freq <- subset(PulseLength703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempPulseLength703Freq <- subset(tempPulseLength703Freq, tempPulseLength703Freq$Layer == j)
      N[i] = length(which(temptempPulseLength703Freq==Process703[i]))
    }
  }
}

ProcessFreqPulseLength <- cbind.data.frame(Layer, Process703 , N)
SummaryPulseLengthFreq <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=sum)
colnames(SummaryPulseLengthFreq) <- c("Process", "Frequency")

SummaryPulseLengthMean <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=mean)
colnames(SummaryPulseLengthMean) <- c("Process", "Average")

SummaryPulseLength703 <- cbind.data.frame(SummaryPulseLengthFreq, SummaryPulseLengthMean [c(2)])
SummaryPulseLength703 <- SummaryPulseLength703[order(-SummaryPulseLength703$Frequency),]

#Plot the old data over the new data
RakeSensor703_Pulse$Time <- as.POSIXct(RakeSensor703_Pulse$Time,format="%H:%M:%OS")
RakeSensor703_Pulse$Value <- as.numeric(RakeSensor703_Pulse$Value)
PulseLength703$Time <- as.POSIXct(PulseLength703$Time,format="%H:%M:%OS")
PulseLength703$Value <- as.numeric(PulseLength703$Value)

Part_RakeSensor_Pulse <- filter(RakeSensor703_Pulse, Layer >= 200 & Layer <= 232)
Part_PulseLength <- filter(PulseLength703, Layer >= 200 & Layer <= 232)

theme_update(plot.title = element_text(hjust = 0.5))
PulseLength_Plot <- ggplot(data = Part_RakeSensor_Pulse) + 
  geom_line(mapping = aes(x=Time, y=Value)) +
  geom_line(data = Part_PulseLength, mapping = aes(x=Time, y=Value), color = "red", lwd = 1) +
  ggtitle("Pulse Length vs Rake Sensor Pulse \n Build 703 Layers 200-232")

plot(PulseLength_Plot)

# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength703, Layer >= 203 & Layer <=224)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects703, X5== "LowerTable" & Layer == 223)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle("Processes of Pulse Length \n Layer 203-223") +
#       geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

# #Plotting around different layers
# PulseLength703$Time <- as.POSIXct(PulseLength703$Time,format="%H:%M:%OS")
# PulseLength703$Value <- as.numeric(PulseLength703$Value)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(PulseLength703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(PulseLength703$Layer)
#     GroupEnd=GroupStart+4
#     tempPulseLength703 <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength703New <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength703ProcessPlot <- ggplot(data = PulseLength703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempPulseLength703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength703New, mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(PulseLength703New$Layer)) {
#         if(PulseLength703New$Layer[j]==Defects703$Layer[i]) {PulseLength703ProcessPlot = PulseLength703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength703ProcessPlot)}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(PulseLength703$Layer)
#     tempPulseLength703 <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength703New <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength703ProcessPlot <- ggplot(data = tempPulseLength703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(PulseLength703New$Layer)) {
#         if(PulseLength703New$Layer[j]==Defects703$Layer[i]) {PulseLength703ProcessPlot = PulseLength703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength703ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempPulseLength703 <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength703New <- filter(PulseLength703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength703ProcessPlot <- ggplot(data = tempPulseLength703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(PulseLength703New$Layer)) {
#         if(PulseLength703New$Layer[j]==Defects703$Layer[i]) {PulseLength703ProcessPlot = PulseLength703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength703ProcessPlot)
#   }
# }

##################
#### Build710 ####
##################

## Special Check ##

# RakeLeftSensor_Pulse <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse")
# RakeLeftSensor_Pulse2 <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse2")
# RakeRightSensor_Pulse <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse")
# RakeRightSensor_Pulse2 <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse2")
# 
# RakeLeftSensor_PulseChanged <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged")
# RakeLeftSensor_PulseChanged2 <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged2")
# RakeRightSensor_PulseChanged <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged")
# RakeRightSensor_PulseChanged2 <- subset(Build710 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged2")
# 
# RakeSensor_PulseChanged <- subset(Build710 [c(2,3,6)], X2=="Process.RakeControl.SensorPulseChanged")
# 
# RakeLeftSensor_Pulse$X5 <- as.numeric(RakeLeftSensor_Pulse$X5)
# RakeLeftSensor_Pulse2$X5 <- as.numeric(RakeLeftSensor_Pulse2$X5)
# RakeRightSensor_Pulse$X5 <- as.numeric(RakeRightSensor_Pulse$X5)
# RakeRightSensor_Pulse2$X5 <- as.numeric(RakeRightSensor_Pulse2$X5)
# 
# NewDatasets <- list(RakeLeftSensor_Pulse,
#                     RakeLeftSensor_Pulse2,
#                     RakeRightSensor_Pulse,
#                     RakeRightSensor_Pulse2,
#                     RakeLeftSensor_PulseChanged,
#                     RakeLeftSensor_PulseChanged2,
#                     RakeRightSensor_PulseChanged,
#                     RakeRightSensor_PulseChanged2,
#                     RakeSensor_PulseChanged)
# 
# for(z in 1:length(NewDatasets)) {
#   #Add layer to Pulse Length and create a plot
#   Time_new <- NewDatasets[[z]]$Time
#   Layer_new <- rep(0, length(Time_new))
#   
#   for(i in 1:length(Time_new)) {
#     for(j in 1:length(Defects710Time)) {
#       if (j == 1) { 
#         if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
#         else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
#       else if (j == length(Defects710Time)) { 
#         if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
#       else {
#         if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
#           Layer_new[i] = Defects710Layer[j]}
#       }
#     }
#   }
#   
#   NewDatasets[[z]]$Layer <- Layer_new
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer")
#   head(NewDatasets[[z]])
#   
#   #Only look at layers excluding the support (> 85) and the final layer (< 335)
#   NewDatasets[[z]] <- subset(NewDatasets[[z]], Layer > 80 & Layer <= max(Defects710Layer))
#   head(NewDatasets[[z]])
#   
#   ### Assign processes within each layer
#   PulseLengthTime <- NewDatasets[[z]]$Time
#   tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
#   ProcessTime <- tempOutputDescription710$Time
#   ProcessPosition <- tempOutputDescription710$Process
#   Process <- rep(0, length(NewDatasets[[z]]$Value))
#   
#   for(k in 1:length(PulseLengthTime)) {
#     for(l in 1:length(ProcessTime)) {
#       if (l == 1) { 
#         if (PulseLengthTime[k] < ProcessTime[l]) { Process[k]=ProcessPosition[l-1] }
#         else if (PulseLengthTime[k] <= ProcessTime[l+1]) { Process[k]=ProcessPosition[l]}}
#       else if (l == length(ProcessTime)) { 
#         if (PulseLengthTime[k] > ProcessTime[l]) { Process[k]=ProcessPosition[length(ProcessPosition)] }}
#       else {
#         if (PulseLengthTime[k] >= ProcessTime[l] && PulseLengthTime[k] < ProcessTime[l+1]) {
#           Process[k] = ProcessPosition[l]}
#       }
#     }
#   }
#   
#   head(Process)
#   NewDatasets[[z]]$Process <- Process
#   NewDatasets[[z]]$Build <- "710"
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")
# }
# 
# RakeLeftSensor_Pulse <- NewDatasets[[1]]
# RakeLeftSensor_Pulse2 <- NewDatasets[[2]]
# RakeRightSensor_Pulse <- NewDatasets[[3]]
# RakeRightSensor_Pulse2 <- NewDatasets[[4]]
# 
# RakeLeftSensor_PulseChanged <- NewDatasets[[5]]
# RakeLeftSensor_PulseChanged2 <- NewDatasets[[6]]
# RakeRightSensor_PulseChanged <- NewDatasets[[7]]
# RakeRightSensor_PulseChanged2 <- NewDatasets[[8]]
# 
# RakeSensor_PulseChanged <- NewDatasets[[9]]
# 
# RakeSensor710_Pulse <- rbind(RakeLeftSensor_Pulse, 
#                              RakeLeftSensor_Pulse2, 
#                              RakeRightSensor_Pulse, 
#                              RakeRightSensor_Pulse2)
# 
# RakeSensor710_Pulse <- RakeSensor710_Pulse[order(RakeSensor710_Pulse$Time),]
# 
# RakeSensor710_LR_PulseChanged <- rbind(RakeLeftSensor_PulseChanged, 
#                                        RakeLeftSensor_PulseChanged2, 
#                                        RakeRightSensor_PulseChanged, 
#                                        RakeRightSensor_PulseChanged2)
# 
# RakeSensor710_LR_PulseChanged <- RakeSensor710_LR_PulseChanged[order(RakeSensor710_LR_PulseChanged$Time),]
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(RakeSensor710_Pulse, Layer >= 234 & Layer <=254)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects710, X5== "LowerTable" & Layer == 254)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Rake Sensor Pulse") +
#   xlab("Time") +
#   ggtitle("Processes of Rake Sensor Pulse \n Build 710 Layer 234-254") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

#################################################################################################

PulseLength710 <- rbind(Build710[Build710$X2=="Process.RightRegulator.PulseLength",],Build710[Build710$X2=="Process.LeftRegulator.PulseLength",])
PulseLength710 <- subset(PulseLength710 [c(2,3,6)])
PulseLength710 <- PulseLength710[order(PulseLength710$Time),]

#Add layer to Pulse Length and create a plot
Time_new <- PulseLength710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

PulseLength710$Layer <- Layer_new
colnames(PulseLength710) <- list("Time", "Variable", "Value", "Layer")
head(PulseLength710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
PulseLength710 <- subset(PulseLength710, Layer > 80 & Layer <= max(Defects710Layer))
head(PulseLength710)

### Assign processes within each layer
PulseLength710Time <- PulseLength710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(PulseLength710$Value))

for(i in 1:length(PulseLength710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (PulseLength710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (PulseLength710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (PulseLength710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (PulseLength710Time[i] >= Process710Time[j] && PulseLength710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
PulseLength710$Process <- Process710
PulseLength710$Build <- "710"
colnames(PulseLength710) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempPulseLength710Freq <- subset(PulseLength710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempPulseLength710Freq <- subset(tempPulseLength710Freq, tempPulseLength710Freq$Layer == j)
      N[i] = length(which(temptempPulseLength710Freq==Process710[i]))
    }
  }
}

ProcessFreqPulseLength <- cbind.data.frame(Layer, Process710 , N)
SummaryPulseLengthFreq <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=sum)
colnames(SummaryPulseLengthFreq) <- c("Process", "Frequency")

SummaryPulseLengthMean <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=mean)
colnames(SummaryPulseLengthMean) <- c("Process", "Average")

SummaryPulseLength710 <- cbind.data.frame(SummaryPulseLengthFreq, SummaryPulseLengthMean [c(2)])
SummaryPulseLength710 <- SummaryPulseLength710[order(-SummaryPulseLength710$Frequency),]

#Plot the old data over the new data
RakeSensor710_Pulse$Time <- as.POSIXct(RakeSensor710_Pulse$Time,format="%H:%M:%OS")
RakeSensor710_Pulse$Value <- as.numeric(RakeSensor710_Pulse$Value)
PulseLength710$Time <- as.POSIXct(PulseLength710$Time,format="%H:%M:%OS")
PulseLength710$Value <- as.numeric(PulseLength710$Value)

Part_RakeSensor_Pulse <- filter(RakeSensor710_Pulse, Layer >= 234 & Layer <= 254)
Part_PulseLength <- filter(PulseLength710, Layer >= 234 & Layer <= 254)

theme_update(plot.title = element_text(hjust = 0.5))
PulseLength_Plot <- ggplot(data = Part_RakeSensor_Pulse) + 
  geom_line(mapping = aes(x=Time, y=Value)) +
  geom_line(data = Part_PulseLength, mapping = aes(x=Time, y=Value), color = "red", lwd = 1) +
  ggtitle("Pulse Length vs Rake Sensor Pulse \n Build 710 Layers 234-254")

plot(PulseLength_Plot)

# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength710, Layer >= 234 & Layer <=254)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects710, X5== "LowerTable" & Layer == 254)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Pulse Length") +
#   xlab("Time") +
#   ggtitle("Processes of Pulse Length \n Layer 234-254") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

# #Plotting around different layers
# PulseLength710$Time <- as.POSIXct(PulseLength710$Time,format="%H:%M:%OS")
# PulseLength710$Value <- as.numeric(PulseLength710$Value)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(PulseLength710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(PulseLength710$Layer)
#     GroupEnd=GroupStart+4
#     tempPulseLength710 <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength710New <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength710ProcessPlot <- ggplot(data = PulseLength710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempPulseLength710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength710New, mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(PulseLength710New$Layer)) {
#         if(PulseLength710New$Layer[j]==Defects710$Layer[i]) {PulseLength710ProcessPlot = PulseLength710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength710ProcessPlot)}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(PulseLength710$Layer)
#     tempPulseLength710 <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength710New <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength710ProcessPlot <- ggplot(data = tempPulseLength710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(PulseLength710New$Layer)) {
#         if(PulseLength710New$Layer[j]==Defects710$Layer[i]) {PulseLength710ProcessPlot = PulseLength710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength710ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempPulseLength710 <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength710New <- filter(PulseLength710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength710ProcessPlot <- ggplot(data = tempPulseLength710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(PulseLength710New$Layer)) {
#         if(PulseLength710New$Layer[j]==Defects710$Layer[i]) {PulseLength710ProcessPlot = PulseLength710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength710ProcessPlot)
#   }
# }

##################
#### Build718 ####
##################

## Special Check ##

# RakeLeftSensor_Pulse <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse")
# RakeLeftSensor_Pulse2 <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse2")
# RakeRightSensor_Pulse <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse")
# RakeRightSensor_Pulse2 <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse2")
# 
# RakeLeftSensor_PulseChanged <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged")
# RakeLeftSensor_PulseChanged2 <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged2")
# RakeRightSensor_PulseChanged <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged")
# RakeRightSensor_PulseChanged2 <- subset(Build718 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged2")
# 
# RakeSensor_PulseChanged <- subset(Build718 [c(2,3,6)], X2=="Process.RakeControl.SensorPulseChanged")
# 
# RakeLeftSensor_Pulse$X5 <- as.numeric(RakeLeftSensor_Pulse$X5)
# RakeLeftSensor_Pulse2$X5 <- as.numeric(RakeLeftSensor_Pulse2$X5)
# RakeRightSensor_Pulse$X5 <- as.numeric(RakeRightSensor_Pulse$X5)
# RakeRightSensor_Pulse2$X5 <- as.numeric(RakeRightSensor_Pulse2$X5)
# 
# NewDatasets <- list(RakeLeftSensor_Pulse,
#                     RakeLeftSensor_Pulse2,
#                     RakeRightSensor_Pulse,
#                     RakeRightSensor_Pulse2,
#                     RakeLeftSensor_PulseChanged,
#                     RakeLeftSensor_PulseChanged2,
#                     RakeRightSensor_PulseChanged,
#                     RakeRightSensor_PulseChanged2,
#                     RakeSensor_PulseChanged)
# 
# for(z in 1:length(NewDatasets)) {
#   #Add layer to Pulse Length and create a plot
#   Time_new <- NewDatasets[[z]]$Time
#   Layer_new <- rep(0, length(Time_new))
#   
#   for(i in 1:length(Time_new)) {
#     for(j in 1:length(Defects718Time)) {
#       if (j == 1) { 
#         if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
#         else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
#       else if (j == length(Defects718Time)) { 
#         if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
#       else {
#         if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
#           Layer_new[i] = Defects718Layer[j]}
#       }
#     }
#   }
#   
#   NewDatasets[[z]]$Layer <- Layer_new
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer")
#   head(NewDatasets[[z]])
#   
#   #Only look at layers excluding the support (> 85) and the final layer (< 335)
#   NewDatasets[[z]] <- subset(NewDatasets[[z]], Layer > 80 & Layer < max(Defects718Layer))
#   head(NewDatasets[[z]])
#   
#   ### Assign processes within each layer
#   PulseLengthTime <- NewDatasets[[z]]$Time
#   tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
#   ProcessTime <- tempOutputDescription718$Time
#   ProcessPosition <- tempOutputDescription718$Process
#   Process <- rep(0, length(NewDatasets[[z]]$Value))
#   
#   for(k in 1:length(PulseLengthTime)) {
#     for(l in 1:length(ProcessTime)) {
#       if (l == 1) { 
#         if (PulseLengthTime[k] < ProcessTime[l]) { Process[k]=ProcessPosition[l-1] }
#         else if (PulseLengthTime[k] <= ProcessTime[l+1]) { Process[k]=ProcessPosition[l]}}
#       else if (l == length(ProcessTime)) { 
#         if (PulseLengthTime[k] > ProcessTime[l]) { Process[k]=ProcessPosition[length(ProcessPosition)] }}
#       else {
#         if (PulseLengthTime[k] >= ProcessTime[l] && PulseLengthTime[k] < ProcessTime[l+1]) {
#           Process[k] = ProcessPosition[l]}
#       }
#     }
#   }
#   
#   head(Process)
#   NewDatasets[[z]]$Process <- Process
#   NewDatasets[[z]]$Build <- "718"
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")
# }
# 
# RakeLeftSensor_Pulse <- NewDatasets[[1]]
# RakeLeftSensor_Pulse2 <- NewDatasets[[2]]
# RakeRightSensor_Pulse <- NewDatasets[[3]]
# RakeRightSensor_Pulse2 <- NewDatasets[[4]]
# 
# RakeLeftSensor_PulseChanged <- NewDatasets[[5]]
# RakeLeftSensor_PulseChanged2 <- NewDatasets[[6]]
# RakeRightSensor_PulseChanged <- NewDatasets[[7]]
# RakeRightSensor_PulseChanged2 <- NewDatasets[[8]]
# 
# RakeSensor_PulseChanged <- NewDatasets[[9]]
# 
# RakeSensor718_Pulse <- rbind(RakeLeftSensor_Pulse, 
#                              RakeLeftSensor_Pulse2, 
#                              RakeRightSensor_Pulse, 
#                              RakeRightSensor_Pulse2)
# 
# RakeSensor718_Pulse <- RakeSensor718_Pulse[order(RakeSensor718_Pulse$Time),]
# 
# RakeSensor718_LR_PulseChanged <- rbind(RakeLeftSensor_PulseChanged, 
#                                        RakeLeftSensor_PulseChanged2, 
#                                        RakeRightSensor_PulseChanged, 
#                                        RakeRightSensor_PulseChanged2)
# 
# RakeSensor718_LR_PulseChanged <- RakeSensor718_LR_PulseChanged[order(RakeSensor718_LR_PulseChanged$Time),]
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(RakeSensor718_Pulse, Layer >= 203 & Layer <=224)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects718, X5== "LowerTable" & Layer == 223)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Rake Sensor Pulse") +
#   xlab("Time") +
#   ggtitle("Processes of Rake Sensor Pulse \n Build 718 Layer 203-223") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

#################################################################################################

PulseLength718 <- rbind(Build718[Build718$X2=="Process.RightRegulator.PulseLength",],Build718[Build718$X2=="Process.LeftRegulator.PulseLength",])
PulseLength718 <- subset(PulseLength718 [c(2,3,6)])
PulseLength718 <- PulseLength718[order(PulseLength718$Time),]

#Add layer to Pulse Length and create a plot
Time_new <- PulseLength718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

PulseLength718$Layer <- Layer_new
colnames(PulseLength718) <- list("Time", "Variable", "Value", "Layer")
head(PulseLength718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
PulseLength718 <- subset(PulseLength718, Layer > 80 & Layer < max(Defects718Layer))
head(PulseLength718)

### Assign processes within each layer
PulseLength718Time <- PulseLength718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(PulseLength718$Value))

for(i in 1:length(PulseLength718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (PulseLength718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (PulseLength718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (PulseLength718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (PulseLength718Time[i] >= Process718Time[j] && PulseLength718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
PulseLength718$Process <- Process718
PulseLength718$Build <- "718"
colnames(PulseLength718) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempPulseLength718Freq <- subset(PulseLength718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempPulseLength718Freq <- subset(tempPulseLength718Freq, tempPulseLength718Freq$Layer == j)
      N[i] = length(which(temptempPulseLength718Freq==Process718[i]))
    }
  }
}

ProcessFreqPulseLength <- cbind.data.frame(Layer, Process718 , N)
SummaryPulseLengthFreq <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=sum)
colnames(SummaryPulseLengthFreq) <- c("Process", "Frequency")

SummaryPulseLengthMean <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=mean)
colnames(SummaryPulseLengthMean) <- c("Process", "Average")

SummaryPulseLength718 <- cbind.data.frame(SummaryPulseLengthFreq, SummaryPulseLengthMean [c(2)])
SummaryPulseLength718 <- SummaryPulseLength718[order(-SummaryPulseLength718$Frequency),]

#Plot the old data over the new data
RakeSensor718_Pulse$Time <- as.POSIXct(RakeSensor718_Pulse$Time,format="%H:%M:%OS")
RakeSensor718_Pulse$Value <- as.numeric(RakeSensor718_Pulse$Value)
PulseLength718$Time <- as.POSIXct(PulseLength718$Time,format="%H:%M:%OS")
PulseLength718$Value <- as.numeric(PulseLength718$Value)

Part_RakeSensor_Pulse <- filter(RakeSensor718_Pulse, Layer >= 234 & Layer <= 254)
Part_PulseLength <- filter(PulseLength718, Layer >= 234 & Layer <= 254)

theme_update(plot.title = element_text(hjust = 0.5))
PulseLength_Plot <- ggplot(data = Part_RakeSensor_Pulse) + 
  geom_line(mapping = aes(x=Time, y=Value)) +
  geom_line(data = Part_PulseLength, mapping = aes(x=Time, y=Value), color = "red", lwd = 1) +
  ggtitle("Pulse Length vs Rake Sensor Pulse \n Build 718 Layers 234-254")

plot(PulseLength_Plot)

# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength718, Layer >= 203 & Layer <=224)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects718, X5== "LowerTable" & Layer == 223)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Pulse Length") +
#   xlab("Time") +
#   ggtitle("Processes of Pulse Length \n Layer 203-223") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength718, Layer >= 234 & Layer <=255)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects718, X5== "LowerTable" & Layer == 254)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Pulse Length") +
#   xlab("Time") +
#   ggtitle("Processes of Pulse Length \n Layer 234-254") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength718, Layer >= 84 & Layer <=108)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects718, X5== "LowerTable" & Layer == 104)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Pulse Length") +
#   xlab("Time") +
#   ggtitle("Processes of Pulse Length \n Layer 84-104") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

# #Plotting around different layers
# PulseLength718$Time <- as.POSIXct(PulseLength718$Time,format="%H:%M:%OS")
# PulseLength718$Value <- as.numeric(PulseLength718$Value)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(PulseLength718$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(PulseLength718$Layer)
#     GroupEnd=GroupStart+4
#     tempPulseLength718 <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength718New <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength718ProcessPlot <- ggplot(data = PulseLength718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempPulseLength718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength718New, mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(PulseLength718New$Layer)) {
#         if(PulseLength718New$Layer[j]==Defects718$Layer[i]) {PulseLength718ProcessPlot = PulseLength718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength718ProcessPlot)}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(PulseLength718$Layer)
#     tempPulseLength718 <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength718New <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength718ProcessPlot <- ggplot(data = tempPulseLength718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(PulseLength718New$Layer)) {
#         if(PulseLength718New$Layer[j]==Defects718$Layer[i]) {PulseLength718ProcessPlot = PulseLength718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength718ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempPulseLength718 <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength718New <- filter(PulseLength718, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength718ProcessPlot <- ggplot(data = tempPulseLength718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(PulseLength718New$Layer)) {
#         if(PulseLength718New$Layer[j]==Defects718$Layer[i]) {PulseLength718ProcessPlot = PulseLength718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength718ProcessPlot)
#   }
# }

##################
#### Build723 ####
##################

## Special Check ##

# RakeLeftSensor_Pulse <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse")
# RakeLeftSensor_Pulse2 <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulse2")
# RakeRightSensor_Pulse <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse")
# RakeRightSensor_Pulse2 <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulse2")
# 
# RakeLeftSensor_PulseChanged <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged")
# RakeLeftSensor_PulseChanged2 <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeLeftSensorPulseChanged2")
# RakeRightSensor_PulseChanged <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged")
# RakeRightSensor_PulseChanged2 <- subset(Build723 [c(2,3,6)], X2=="OPC.RakeSensor.RakeRightSensorPulseChanged2")
# 
# RakeSensor_PulseChanged <- subset(Build723 [c(2,3,6)], X2=="Process.RakeControl.SensorPulseChanged")
# 
# RakeLeftSensor_Pulse$X5 <- as.numeric(RakeLeftSensor_Pulse$X5)
# RakeLeftSensor_Pulse2$X5 <- as.numeric(RakeLeftSensor_Pulse2$X5)
# RakeRightSensor_Pulse$X5 <- as.numeric(RakeRightSensor_Pulse$X5)
# RakeRightSensor_Pulse2$X5 <- as.numeric(RakeRightSensor_Pulse2$X5)
# 
# NewDatasets <- list(RakeLeftSensor_Pulse,
#                     RakeLeftSensor_Pulse2,
#                     RakeRightSensor_Pulse,
#                     RakeRightSensor_Pulse2,
#                     RakeLeftSensor_PulseChanged,
#                     RakeLeftSensor_PulseChanged2,
#                     RakeRightSensor_PulseChanged,
#                     RakeRightSensor_PulseChanged2,
#                     RakeSensor_PulseChanged)
# 
# for(z in 1:length(NewDatasets)) {
#   #Add layer to Pulse Length and create a plot
#   Time_new <- NewDatasets[[z]]$Time
#   Layer_new <- rep(0, length(Time_new))
#   
#   for(i in 1:length(Time_new)) {
#     for(j in 1:length(Defects723Time)) {
#       if (j == 1) { 
#         if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
#         else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
#       else if (j == length(Defects723Time)) { 
#         if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
#       else {
#         if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
#           Layer_new[i] = Defects723Layer[j]}
#       }
#     }
#   }
#   
#   NewDatasets[[z]]$Layer <- Layer_new
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer")
#   head(NewDatasets[[z]])
#   
#   #Only look at layers excluding the support (> 85) and the final layer (< 335)
#   NewDatasets[[z]] <- subset(NewDatasets[[z]], Layer > 80 & Layer <= max(Defects723Layer))
#   head(NewDatasets[[z]])
#   
#   ### Assign processes within each layer
#   PulseLengthTime <- NewDatasets[[z]]$Time
#   tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
#   ProcessTime <- tempOutputDescription723$Time
#   ProcessPosition <- tempOutputDescription723$Process
#   Process <- rep(0, length(NewDatasets[[z]]$Value))
#   
#   for(k in 1:length(PulseLengthTime)) {
#     for(l in 1:length(ProcessTime)) {
#       if (l == 1) { 
#         if (PulseLengthTime[k] < ProcessTime[l]) { Process[k]=ProcessPosition[l-1] }
#         else if (PulseLengthTime[k] <= ProcessTime[l+1]) { Process[k]=ProcessPosition[l]}}
#       else if (l == length(ProcessTime)) { 
#         if (PulseLengthTime[k] > ProcessTime[l]) { Process[k]=ProcessPosition[length(ProcessPosition)] }}
#       else {
#         if (PulseLengthTime[k] >= ProcessTime[l] && PulseLengthTime[k] < ProcessTime[l+1]) {
#           Process[k] = ProcessPosition[l]}
#       }
#     }
#   }
#   
#   head(Process)
#   NewDatasets[[z]]$Process <- Process
#   NewDatasets[[z]]$Build <- "723"
#   colnames(NewDatasets[[z]]) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")
# }
# 
# RakeLeftSensor_Pulse <- NewDatasets[[1]]
# RakeLeftSensor_Pulse2 <- NewDatasets[[2]]
# RakeRightSensor_Pulse <- NewDatasets[[3]]
# RakeRightSensor_Pulse2 <- NewDatasets[[4]]
# 
# RakeLeftSensor_PulseChanged <- NewDatasets[[5]]
# RakeLeftSensor_PulseChanged2 <- NewDatasets[[6]]
# RakeRightSensor_PulseChanged <- NewDatasets[[7]]
# RakeRightSensor_PulseChanged2 <- NewDatasets[[8]]
# 
# RakeSensor_PulseChanged <- NewDatasets[[9]]
# 
# RakeSensor723_Pulse <- rbind(RakeLeftSensor_Pulse, 
#                              RakeLeftSensor_Pulse2, 
#                              RakeRightSensor_Pulse, 
#                              RakeRightSensor_Pulse2)
# 
# RakeSensor723_Pulse <- RakeSensor723_Pulse[order(RakeSensor723_Pulse$Time),]
# 
# RakeSensor723_LR_PulseChanged <- rbind(RakeLeftSensor_PulseChanged, 
#                                        RakeLeftSensor_PulseChanged2, 
#                                        RakeRightSensor_PulseChanged, 
#                                        RakeRightSensor_PulseChanged2)
# 
# RakeSensor723_LR_PulseChanged <- RakeSensor723_LR_PulseChanged[order(RakeSensor723_LR_PulseChanged$Time),]
# 
# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(RakeSensor723_Pulse, Layer >= 84 & Layer <=104)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects723, X5== "LowerTable" & Layer == 104)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Rake Sensor Pulse") +
#   xlab("Time") +
#   ggtitle("Processes of Rake Sensor Pulse \n Build 723 Layer 84-104") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

#################################################################################################

PulseLength723 <- rbind(Build723[Build723$X2=="Process.RightRegulator.PulseLength",],Build723[Build723$X2=="Process.LeftRegulator.PulseLength",])
PulseLength723 <- subset(PulseLength723 [c(2,3,6)])
PulseLength723 <- PulseLength723[order(PulseLength723$Time),]

#Add layer to Pulse Length and create a plot
Time_new <- PulseLength723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

PulseLength723$Layer <- Layer_new
colnames(PulseLength723) <- list("Time", "Variable", "Value", "Layer")
head(PulseLength723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
PulseLength723 <- subset(PulseLength723, Layer > 80 & Layer <= max(Defects723Layer))
head(PulseLength723)

### Assign processes within each layer
PulseLength723Time <- PulseLength723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(PulseLength723$Value))

for(i in 1:length(PulseLength723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (PulseLength723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (PulseLength723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (PulseLength723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (PulseLength723Time[i] >= Process723Time[j] && PulseLength723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
PulseLength723$Process <- Process723
PulseLength723$Build <- "723"
colnames(PulseLength723) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempPulseLength723Freq <- subset(PulseLength723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempPulseLength723Freq <- subset(tempPulseLength723Freq, tempPulseLength723Freq$Layer == j)
      N[i] = length(which(temptempPulseLength723Freq==Process723[i]))
    }
  }
}

ProcessFreqPulseLength <- cbind.data.frame(Layer, Process723 , N)
SummaryPulseLengthFreq <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=sum)
colnames(SummaryPulseLengthFreq) <- c("Process", "Frequency")

SummaryPulseLengthMean <- aggregate(ProcessFreqPulseLength$N, by=(list((ProcessFreqPulseLength$Process))), FUN=mean)
colnames(SummaryPulseLengthMean) <- c("Process", "Average")

SummaryPulseLength723 <- cbind.data.frame(SummaryPulseLengthFreq, SummaryPulseLengthMean [c(2)])
SummaryPulseLength723 <- SummaryPulseLength723[order(-SummaryPulseLength723$Frequency),]

#Plot the old data over the new data
RakeSensor723_Pulse$Time <- as.POSIXct(RakeSensor723_Pulse$Time,format="%H:%M:%OS")
RakeSensor723_Pulse$Value <- as.numeric(RakeSensor723_Pulse$Value)
PulseLength723$Time <- as.POSIXct(PulseLength723$Time,format="%H:%M:%OS")
PulseLength723$Value <- as.numeric(PulseLength723$Value)

Part_RakeSensor_Pulse <- filter(RakeSensor723_Pulse, Layer >= 84 & Layer <= 104)
Part_PulseLength <- filter(PulseLength723, Layer >= 84 & Layer <= 104)

theme_update(plot.title = element_text(hjust = 0.5))
PulseLength_Plot <- ggplot(data = Part_RakeSensor_Pulse) + 
  geom_line(mapping = aes(x=Time, y=Value)) +
  geom_line(data = Part_PulseLength, mapping = aes(x=Time, y=Value), color = "red", lwd = 1) +
  ggtitle("Pulse Length vs Rake Sensor Pulse \n Build 723 Layers 84-104")

plot(PulseLength_Plot)

# #Plot the Pulse Length ~20 Layers Before the Defect
# Pulse20BeforeDefect <- filter(PulseLength723, Layer >= 84 & Layer <=104)
# Pulse20BeforeDefect$Time <- as.POSIXct(Pulse20BeforeDefect$Time,format="%H:%M:%OS")
# Pulse20BeforeDefect$Value <- as.numeric(Pulse20BeforeDefect$Value)
# LayerDefectStartTime <- filter(Defects723, X5== "LowerTable" & Layer == 104)
# 
# theme_update(plot.title = element_text(hjust = 0.5))
# PulsePlot <- ggplot(data = Pulse20BeforeDefect, aes(x=Time, y=Value)) +
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(mapping = aes(x = Time, y = Value))+
#   ylab("Pulse Length") +
#   xlab("Time") +
#   ggtitle("Processes of Pulse Length \n Layer 84-104") +
#   geom_vline(xintercept = LayerDefectStartTime$Time, color = "blue")
# plot(PulsePlot)

# #Plotting around different layers
# PulseLength723$Time <- as.POSIXct(PulseLength723$Time,format="%H:%M:%OS")
# PulseLength723$Value <- as.numeric(PulseLength723$Value)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(PulseLength723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(PulseLength723$Layer)
#     GroupEnd=GroupStart+4
#     tempPulseLength723 <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength723New <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength723ProcessPlot <- ggplot(data = PulseLength723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempPulseLength723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength723New, mapping = aes(x = Time, y = Value))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(PulseLength723New$Layer)) {
#         if(PulseLength723New$Layer[j]==Defects723$Layer[i]) {PulseLength723ProcessPlot = PulseLength723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength723ProcessPlot)}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(PulseLength723$Layer)
#     tempPulseLength723 <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength723New <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength723ProcessPlot <- ggplot(data = tempPulseLength723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(PulseLength723New$Layer)) {
#         if(PulseLength723New$Layer[j]==Defects723$Layer[i]) {PulseLength723ProcessPlot = PulseLength723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength723ProcessPlot)}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempPulseLength723 <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     PulseLength723New <- filter(PulseLength723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     theme_update(plot.title = element_text(hjust = 0.5))
#     PulseLength723ProcessPlot <- ggplot(data = tempPulseLength723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=PulseLength723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Pulse Length") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Pulse Length \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(PulseLength723New$Layer)) {
#         if(PulseLength723New$Layer[j]==Defects723$Layer[i]) {PulseLength723ProcessPlot = PulseLength723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(PulseLength723ProcessPlot)
#   }
# }

#########################################

#####Combine all datasets####
PulseLengthAll <- rbind(PulseLength629,
                        PulseLength703,
                        PulseLength710,
                        PulseLength718,
                        PulseLength723)

PulseLengthAll$Layer_ID <- paste(PulseLengthAll$Build, "/", PulseLengthAll$Layer)

###########################################################################################################################

########################### Defect: Other Possible Defects ########################### 

################################################
################ Chamber Vacuum ################ 
################################################ 

##################
#### Build629 ####
##################

ChamberVacuum629 <- subset(Build629 [c(2,3,6)], X2=="OPC.Vacuum.ChamberVacuumGaugeFB")

Time_new <- ChamberVacuum629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

ChamberVacuum629$Layer <- Layer_new
colnames(ChamberVacuum629) <- list("Time", "Variable", "Value", "Layer")
head(ChamberVacuum629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
ChamberVacuum629 <- subset(ChamberVacuum629, Layer > 80 & Layer < max(Defects629Layer))
head(ChamberVacuum629)

### Assign processes within each layer
ChamberVacuum629Time <- ChamberVacuum629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(ChamberVacuum629$Value))

for(i in 1:length(ChamberVacuum629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (ChamberVacuum629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (ChamberVacuum629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (ChamberVacuum629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (ChamberVacuum629Time[i] >= Process629Time[j] && ChamberVacuum629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
ChamberVacuum629$Process <- Process629
ChamberVacuum629$Build <- "629"
colnames(ChamberVacuum629) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempChamberVacuum629Freq <- subset(ChamberVacuum629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempChamberVacuum629Freq <- subset(tempChamberVacuum629Freq, tempChamberVacuum629Freq$Layer == j)
      N[i] = length(which(temptempChamberVacuum629Freq==Process629[i]))
    }
  }
}

ProcessFreqChamberVacuum <- cbind.data.frame(Layer, Process629 , N)
SummaryChamberVacuumFreq <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=sum)
colnames(SummaryChamberVacuumFreq) <- c("Process", "Frequency")

SummaryChamberVacuumMean <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=mean)
colnames(SummaryChamberVacuumMean) <- c("Process", "Average")

SummaryChamberVacuum629 <- cbind.data.frame(SummaryChamberVacuumFreq, SummaryChamberVacuumMean [c(2)])
SummaryChamberVacuum629 <- SummaryChamberVacuum629[order(-SummaryChamberVacuum629$Frequency),]

# #Plotting around different layers
# ChamberVacuum629$Time <- as.POSIXct(ChamberVacuum629$Time,format="%H:%M:%OS")
# ChamberVacuum629$Value <- as.numeric(ChamberVacuum629$Value)
# 
# ChamberVac629ProcessPlot <- subset(ChamberVacuum629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(ChamberVac629ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(ChamberVac629ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum629 <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum629New <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Chamber Vacuum Gauge Feedback/","ChamberVacuum629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum629ProcessPlot <- ggplot(data = ChamberVacuum629New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum629New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(ChamberVacuum629New$Layer)) {
#         if(ChamberVacuum629New$Layer[j]==Defects629$Layer[i]) {ChamberVacuum629ProcessPlot = ChamberVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac629ProcessPlot$Layer)
#     tempChamberVacuum629 <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum629New <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Chamber Vacuum Gauge Feedback/","ChamberVacuum629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum629ProcessPlot <- ggplot(data = tempChamberVacuum629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(ChamberVacuum629New$Layer)) {
#         if(ChamberVacuum629New$Layer[j]==Defects629$Layer[i]) {ChamberVacuum629ProcessPlot = ChamberVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum629 <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum629New <- filter(ChamberVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Chamber Vacuum Gauge Feedback/","ChamberVacuum629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum629ProcessPlot <- ggplot(data = tempChamberVacuum629, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(ChamberVacuum629New$Layer)) {
#         if(ChamberVacuum629New$Layer[j]==Defects629$Layer[i]) {ChamberVacuum629ProcessPlot = ChamberVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

ChamberVacuum703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.Vacuum.ChamberVacuumGaugeFB" & Date =="2018-07-05")
ChamberVacuum703 <- subset(ChamberVacuum703[c(2,3,4)])

Time_new <- ChamberVacuum703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

ChamberVacuum703$Layer <- Layer_new
colnames(ChamberVacuum703) <- list("Time", "Variable", "Value", "Layer")
head(ChamberVacuum703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
ChamberVacuum703 <- subset(ChamberVacuum703, Layer > 80 & Layer <= max(Defects703Layer))
head(ChamberVacuum703)

### Assign processes within each layer
ChamberVacuum703Time <- ChamberVacuum703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(ChamberVacuum703$Value))

for(i in 1:length(ChamberVacuum703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (ChamberVacuum703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (ChamberVacuum703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (ChamberVacuum703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (ChamberVacuum703Time[i] >= Process703Time[j] && ChamberVacuum703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
ChamberVacuum703$Process <- Process703
ChamberVacuum703$Build <- "703"
colnames(ChamberVacuum703) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempChamberVacuum703Freq <- subset(ChamberVacuum703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempChamberVacuum703Freq <- subset(tempChamberVacuum703Freq, tempChamberVacuum703Freq$Layer == j)
      N[i] = length(which(temptempChamberVacuum703Freq==Process703[i]))
    }
  }
}

ProcessFreqChamberVacuum <- cbind.data.frame(Layer, Process703 , N)
SummaryChamberVacuumFreq <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=sum)
colnames(SummaryChamberVacuumFreq) <- c("Process", "Frequency")

SummaryChamberVacuumMean <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=mean)
colnames(SummaryChamberVacuumMean) <- c("Process", "Average")

SummaryChamberVacuum703 <- cbind.data.frame(SummaryChamberVacuumFreq, SummaryChamberVacuumMean [c(2)])
SummaryChamberVacuum703 <- SummaryChamberVacuum703[order(-SummaryChamberVacuum703$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList703 <- subset(unique(filter(OutputDescription703, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList703 <- unique(DefectList703)
# 
# ChamberVacuum703$Time <- as.POSIXct(ChamberVacuum703$Time,format="%H:%M:%OS")
# ChamberVacuum703$Value <- as.numeric(ChamberVacuum703$Value)
# 
# ChamberVacuum20BeforeDefect703 <- subset(ChamberVacuum703, Layer >= DefectList703$Layer[1]-20 & Layer <= DefectList703$Layer[1]-1)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(ChamberVacuum20BeforeDefect703$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(ChamberVacuum20BeforeDefect703$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum703 <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = ChamberVacuum703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVacuum20BeforeDefect703$Layer)
#     tempChamberVacuum703 <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = tempChamberVacuum703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum703 <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVacuum20BeforeDefect703, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect703","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = tempChamberVacuum703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempChamberVacuum703 <- filter(ChamberVacuum703, Layer == DefectList703$Layer[1])
# ChamberVacuum703New <- filter(ChamberVacuum703, Layer == DefectList703$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 703/Chamber Vacuum Gauge Feedback/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# ChamberVacuum703ProcessPlot <- ggplot(data = tempChamberVacuum703, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value))+
#   ylab("Chamber Vacuum") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Chamber Vacuum \n Layer", DefectList703$Layer[1]))
# 
# plot(ChamberVacuum703ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# ChamberVacuum703$Time <- as.POSIXct(ChamberVacuum703$Time,format="%H:%M:%OS")
# ChamberVacuum703$Value <- as.numeric(ChamberVacuum703$Value)
# 
# ChamberVac703ProcessPlot <- subset(ChamberVacuum703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(ChamberVac703ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(ChamberVac703ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum703 <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Chamber Vacuum Gauge Feedback/","ChamberVacuum703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = ChamberVacuum703New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac703ProcessPlot$Layer)
#     tempChamberVacuum703 <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Chamber Vacuum Gauge Feedback/","ChamberVacuum703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = tempChamberVacuum703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum703 <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum703New <- filter(ChamberVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Chamber Vacuum Gauge Feedback/","ChamberVacuum703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum703ProcessPlot <- ggplot(data = tempChamberVacuum703, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(ChamberVacuum703New$Layer)) {
#         if(ChamberVacuum703New$Layer[j]==Defects703$Layer[i]) {ChamberVacuum703ProcessPlot = ChamberVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build710 ####
##################

ChamberVacuum710 <- subset(Build710 [c(2,3,6)], X2=="OPC.Vacuum.ChamberVacuumGaugeFB")

Time_new <- ChamberVacuum710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

ChamberVacuum710$Layer <- Layer_new
colnames(ChamberVacuum710) <- list("Time", "Variable", "Value", "Layer")
head(ChamberVacuum710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
ChamberVacuum710 <- subset(ChamberVacuum710, Layer > 80 & Layer <= max(Defects710Layer))
head(ChamberVacuum710)

### Assign processes within each layer
ChamberVacuum710Time <- ChamberVacuum710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(ChamberVacuum710$Value))

for(i in 1:length(ChamberVacuum710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (ChamberVacuum710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (ChamberVacuum710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (ChamberVacuum710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (ChamberVacuum710Time[i] >= Process710Time[j] && ChamberVacuum710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
ChamberVacuum710$Process <- Process710
ChamberVacuum710$Build <- "710"
colnames(ChamberVacuum710) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempChamberVacuum710Freq <- subset(ChamberVacuum710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempChamberVacuum710Freq <- subset(tempChamberVacuum710Freq, tempChamberVacuum710Freq$Layer == j)
      N[i] = length(which(temptempChamberVacuum710Freq==Process710[i]))
    }
  }
}

ProcessFreqChamberVacuum <- cbind.data.frame(Layer, Process710 , N)
SummaryChamberVacuumFreq <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=sum)
colnames(SummaryChamberVacuumFreq) <- c("Process", "Frequency")

SummaryChamberVacuumMean <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=mean)
colnames(SummaryChamberVacuumMean) <- c("Process", "Average")

SummaryChamberVacuum710 <- cbind.data.frame(SummaryChamberVacuumFreq, SummaryChamberVacuumMean [c(2)])
SummaryChamberVacuum710 <- SummaryChamberVacuum710[order(-SummaryChamberVacuum710$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList710 <- subset(unique(filter(OutputDescription710, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList710 <- unique(DefectList710)
# 
# ChamberVacuum710$Time <- as.POSIXct(ChamberVacuum710$Time,format="%H:%M:%OS")
# ChamberVacuum710$Value <- as.numeric(ChamberVacuum710$Value)
# 
# ChamberVacuum20BeforeDefect710 <- subset(ChamberVacuum710, Layer >= DefectList710$Layer[1]-20 & Layer <= DefectList710$Layer[1]-1)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(ChamberVacuum20BeforeDefect710$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(ChamberVacuum20BeforeDefect710$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum710 <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = ChamberVacuum710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVacuum20BeforeDefect710$Layer)
#     tempChamberVacuum710 <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = tempChamberVacuum710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum710 <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVacuum20BeforeDefect710, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect710","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = tempChamberVacuum710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempChamberVacuum710 <- filter(ChamberVacuum710, Layer == DefectList710$Layer[1])
# ChamberVacuum710New <- filter(ChamberVacuum710, Layer == DefectList710$Layer[1])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 710/Chamber Vacuum Gauge Feedback/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# ChamberVacuum710ProcessPlot <- ggplot(data = tempChamberVacuum710, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value))+
#   ylab("Chamber Vacuum") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Chamber Vacuum \n Layer", DefectList710$Layer[1]))
# 
# plot(ChamberVacuum710ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# ChamberVacuum710$Time <- as.POSIXct(ChamberVacuum710$Time,format="%H:%M:%OS")
# ChamberVacuum710$Value <- as.numeric(ChamberVacuum710$Value)
# 
# ChamberVac710ProcessPlot <- subset(ChamberVacuum710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(ChamberVac710ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(ChamberVac710ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum710 <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Chamber Vacuum Gauge Feedback/","ChamberVacuum710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = ChamberVacuum710New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac710ProcessPlot$Layer)
#     tempChamberVacuum710 <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Chamber Vacuum Gauge Feedback/","ChamberVacuum710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = tempChamberVacuum710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum710 <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum710New <- filter(ChamberVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Chamber Vacuum Gauge Feedback/","ChamberVacuum710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum710ProcessPlot <- ggplot(data = tempChamberVacuum710, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(ChamberVacuum710New$Layer)) {
#         if(ChamberVacuum710New$Layer[j]==Defects710$Layer[i]) {ChamberVacuum710ProcessPlot = ChamberVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build718 ####
##################

ChamberVacuum718 <- subset(Build718 [c(2,3,6)], X2=="OPC.Vacuum.ChamberVacuumGaugeFB")

Time_new <- ChamberVacuum718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

ChamberVacuum718$Layer <- Layer_new
colnames(ChamberVacuum718) <- list("Time", "Variable", "Value", "Layer")
head(ChamberVacuum718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
ChamberVacuum718 <- subset(ChamberVacuum718, Layer > 80 & Layer < max(Defects718Layer))
head(ChamberVacuum718)

### Assign processes within each layer
ChamberVacuum718Time <- ChamberVacuum718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(ChamberVacuum718$Value))

for(i in 1:length(ChamberVacuum718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (ChamberVacuum718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (ChamberVacuum718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (ChamberVacuum718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (ChamberVacuum718Time[i] >= Process718Time[j] && ChamberVacuum718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
ChamberVacuum718$Process <- Process718
ChamberVacuum718$Build <- "718"
colnames(ChamberVacuum718) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempChamberVacuum718Freq <- subset(ChamberVacuum718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempChamberVacuum718Freq <- subset(tempChamberVacuum718Freq, tempChamberVacuum718Freq$Layer == j)
      N[i] = length(which(temptempChamberVacuum718Freq==Process718[i]))
    }
  }
}

ProcessFreqChamberVacuum <- cbind.data.frame(Layer, Process718 , N)
SummaryChamberVacuumFreq <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=sum)
colnames(SummaryChamberVacuumFreq) <- c("Process", "Frequency")

SummaryChamberVacuumMean <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=mean)
colnames(SummaryChamberVacuumMean) <- c("Process", "Average")

SummaryChamberVacuum718 <- cbind.data.frame(SummaryChamberVacuumFreq, SummaryChamberVacuumMean [c(2)])
SummaryChamberVacuum718 <- SummaryChamberVacuum718[order(-SummaryChamberVacuum718$Frequency),]

# #### Plotting 20 Layers Before the Defect
# Defects <- c(104,223,254)
# 
# ChamberVacuum718$Time <- as.POSIXct(ChamberVacuum718$Time,format="%H:%M:%OS")
# ChamberVacuum718$Value <- as.numeric(ChamberVacuum718$Value)
# 
# ChamberVac20BeforeDefect718_1 <- subset(ChamberVacuum718, Layer >= Defects[1]-20 & Layer <=Defects[1]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(ChamberVac20BeforeDefect718_1$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(ChamberVac20BeforeDefect718_1$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = ChamberVacuum718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVac718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac20BeforeDefect718_1$Layer)
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_1, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# ChamberVac20BeforeDefect718_2 <- subset(ChamberVacuum718, Layer >= Defects[2]-20 & Layer <=Defects[2]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(ChamberVac20BeforeDefect718_2$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(ChamberVac20BeforeDefect718_2$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = ChamberVacuum718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVac718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac20BeforeDefect718_2$Layer)
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_2, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+4,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# ChamberVac20BeforeDefect718_3 <- subset(ChamberVacuum718, Layer >= Defects[3]-20 & Layer <=Defects[3]-1)
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(ChamberVac20BeforeDefect718_3$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(ChamberVac20BeforeDefect718_3$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = ChamberVacuum718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVac718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac20BeforeDefect718_3$Layer)
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVac718 <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac20BeforeDefect718_3, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","ChamberVac20BeforeDefect718","_",i+8,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVac718ProcessPlot = ChamberVac718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVac718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# for(i in 1:length(Defects)) {
#   tempChamberVac718 <- filter(ChamberVacuum718, Layer == Defects[i])
#   ChamberVacuum718New <- filter(ChamberVacuum718, Layer == Defects[i])
#   
#   #Set Plot location and dimensions
#   png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 718/Chamber Vacuum Gauge Feedback/","DefectPlot",i,".png" ,sep = ""),
#       width = 999,
#       height = 333)
#   
#   #Create Plot
#   theme_update(plot.title = element_text(hjust = 0.5))
#   ChamberVac718ProcessPlot <- ggplot(data = tempChamberVac718, aes(x=Layer, y=Value)) + 
#     geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#     geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#     ylab("Chamber Vacuum Gauge Feedback") +
#     xlab("Time") +
#     ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layer", Defects[i]))
#   
#   plot(ChamberVac718ProcessPlot)
#   
#   dev.off()
#   
# }

# #Plotting around different layers
# ChamberVacuum718$Time <- as.POSIXct(ChamberVacuum718$Time,format="%H:%M:%OS")
# ChamberVacuum718$Value <- as.numeric(ChamberVacuum718$Value)
# 
# ChamberVac718ProcessPlot <- subset(ChamberVacuum718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(ChamberVac718ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(ChamberVac718ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum718 <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Chamber Vacuum Gauge Feedback/","ChamberVacuum718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum718ProcessPlot <- ggplot(data = ChamberVacuum718New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVacuum718ProcessPlot = ChamberVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac718ProcessPlot$Layer)
#     tempChamberVacuum718 <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Chamber Vacuum Gauge Feedback/","ChamberVacuum718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum718ProcessPlot <- ggplot(data = tempChamberVacuum718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVacuum718ProcessPlot = ChamberVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum718 <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum718New <- filter(ChamberVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Chamber Vacuum Gauge Feedback/","ChamberVacuum718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum718ProcessPlot <- ggplot(data = tempChamberVacuum718, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(ChamberVacuum718New$Layer)) {
#         if(ChamberVacuum718New$Layer[j]==Defects718$Layer[i]) {ChamberVacuum718ProcessPlot = ChamberVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build723 ####
##################

ChamberVacuum723 <- subset(Build723 [c(2,3,6)], X2=="OPC.Vacuum.ChamberVacuumGaugeFB")

Time_new <- ChamberVacuum723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

ChamberVacuum723$Layer <- Layer_new
colnames(ChamberVacuum723) <- list("Time", "Variable", "Value", "Layer")
head(ChamberVacuum723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
ChamberVacuum723 <- subset(ChamberVacuum723, Layer > 80 & Layer <= max(Defects723Layer))
head(ChamberVacuum723)

### Assign processes within each layer
ChamberVacuum723Time <- ChamberVacuum723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(ChamberVacuum723$Value))

for(i in 1:length(ChamberVacuum723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (ChamberVacuum723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (ChamberVacuum723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (ChamberVacuum723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (ChamberVacuum723Time[i] >= Process723Time[j] && ChamberVacuum723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
ChamberVacuum723$Process <- Process723
ChamberVacuum723$Build <- "723"
colnames(ChamberVacuum723) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempChamberVacuum723Freq <- subset(ChamberVacuum723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempChamberVacuum723Freq <- subset(tempChamberVacuum723Freq, tempChamberVacuum723Freq$Layer == j)
      N[i] = length(which(temptempChamberVacuum723Freq==Process723[i]))
    }
  }
}

ProcessFreqChamberVacuum <- cbind.data.frame(Layer, Process723 , N)
SummaryChamberVacuumFreq <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=sum)
colnames(SummaryChamberVacuumFreq) <- c("Process", "Frequency")

SummaryChamberVacuumMean <- aggregate(ProcessFreqChamberVacuum$N, by=(list((ProcessFreqChamberVacuum$Process))), FUN=mean)
colnames(SummaryChamberVacuumMean) <- c("Process", "Average")

SummaryChamberVacuum723 <- cbind.data.frame(SummaryChamberVacuumFreq, SummaryChamberVacuumMean [c(2)])
SummaryChamberVacuum723 <- SummaryChamberVacuum723[order(-SummaryChamberVacuum723$Frequency),]

# #### Plotting 20 Layers Before the Defect
# DefectList723 <- subset(unique(filter(OutputDescription723, Process=="BuildStopped" | Process=="BuildCrashed")) [c(4)])
# DefectList723 <- unique(DefectList723)
# 
# ChamberVacuum723$Time <- as.POSIXct(ChamberVacuum723$Time,format="%H:%M:%OS")
# ChamberVacuum723$Value <- as.numeric(ChamberVacuum723$Value)
# 
# ChamberVacuum20BeforeDefect723 <- subset(ChamberVacuum723, Layer >= DefectList723$Layer[4]-20 & Layer <= DefectList723$Layer[4]-1)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(ChamberVacuum20BeforeDefect723$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(ChamberVacuum20BeforeDefect723$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum723 <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = ChamberVacuum723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVacuum20BeforeDefect723$Layer)
#     tempChamberVacuum723 <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = tempChamberVacuum723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum723 <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVacuum20BeforeDefect723, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Chamber Vacuum Gauge Feedback/","ChamberVacuum20BeforeDefect723","_",i,".png" ,sep = ""),
#         width = 999,
#         height = 333)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = tempChamberVacuum723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }
# 
# #### Plot the defect layer
# tempChamberVacuum723 <- filter(ChamberVacuum723, Layer == DefectList723$Layer[4])
# ChamberVacuum723New <- filter(ChamberVacuum723, Layer == DefectList723$Layer[4])
# 
# #Set Plot location and dimensions
# png(file = paste("C:/Users/Ethan/Desktop/Training3/MISC/Plotting20LayersBeforeDefect/Build 723/Chamber Vacuum Gauge Feedback/","DefectPlot",".png" ,sep = ""),
#     width = 999,
#     height = 333)
# 
# #Create Plot
# theme_update(plot.title = element_text(hjust = 0.5))
# ChamberVacuum723ProcessPlot <- ggplot(data = tempChamberVacuum723, aes(x=Layer, y=Value)) + 
#   geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#   geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value))+
#   ylab("Chamber Vacuum") +
#   xlab("Time") +
#   ggtitle(paste("Processes of Chamber Vacuum \n Layer", DefectList723$Layer[4]))
# 
# plot(ChamberVacuum723ProcessPlot)
# 
# dev.off()

# #Plotting around different layers
# ChamberVacuum723$Time <- as.POSIXct(ChamberVacuum723$Time,format="%H:%M:%OS")
# ChamberVacuum723$Value <- as.numeric(ChamberVacuum723$Value)
# 
# ChamberVac723ProcessPlot <- subset(ChamberVacuum723, Layer > 85 & Layer <104)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(ChamberVac723ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(ChamberVac723ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempChamberVacuum723 <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Chamber Vacuum Gauge Feedback/","ChamberVacuum723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = ChamberVacuum723New, aes(x=Time, y=Value)) + 
#       geom_point(data=tempChamberVacuum723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(ChamberVac723ProcessPlot$Layer)
#     tempChamberVacuum723 <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Chamber Vacuum Gauge Feedback/","ChamberVacuum723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = tempChamberVacuum723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
#   
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempChamberVacuum723 <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     ChamberVacuum723New <- filter(ChamberVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Chamber Vacuum Gauge Feedback/","ChamberVacuum723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     ChamberVacuum723ProcessPlot <- ggplot(data = tempChamberVacuum723, aes(x=Layer, y=Value)) + 
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=ChamberVacuum723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Chamber Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Chamber Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
#     
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(ChamberVacuum723New$Layer)) {
#         if(ChamberVacuum723New$Layer[j]==Defects723$Layer[i]) {ChamberVacuum723ProcessPlot = ChamberVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
#     
#     plot(ChamberVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#########################################

#####Combine all datasets####
ChamberVacuumAll <- rbind(ChamberVacuum629,
                          ChamberVacuum703,
                          ChamberVacuum710,
                          ChamberVacuum718,
                          ChamberVacuum723)

ChamberVacuumAll$Layer_ID <- paste(ChamberVacuumAll$Build, "/", ChamberVacuumAll$Layer)

################################################
################ Backing Vacuum ################ 
################################################ 

##################
#### Build629 ####
##################

BackingVacuum629 <- subset(Build629 [c(2,3,6)], X2=="OPC.Vacuum.BackingVacuumGaugeFB")

Time_new <- BackingVacuum629$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects629Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects629Time[j]) { Layer_new[i]=Defects629Layer[j]-1 }
      else if (Time_new[i] <= Defects629Time[j+1]) { Layer_new[i]=Defects629Layer[j]}}
    else if (j == length(Defects629Time)) { 
      if (Time_new[i] > Defects629Time[j]) { Layer_new[i]=Defects629Layer[length(Defects629Time)] }}
    else {
      if (Time_new[i] >= Defects629Time[j] && Time_new[i] < Defects629Time[j+1]) {
        Layer_new[i] = Defects629Layer[j]}
    }
  }
}

BackingVacuum629$Layer <- Layer_new
colnames(BackingVacuum629) <- list("Time", "Variable", "Value", "Layer")
head(BackingVacuum629)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BackingVacuum629 <- subset(BackingVacuum629, Layer > 80 & Layer < max(Defects629Layer))
head(BackingVacuum629)

### Assign processes within each layer
BackingVacuum629Time <- BackingVacuum629$Time
tempOutputDescription629 <- subset(OutputDescription629 [c(1,3)])
Process629Time <- tempOutputDescription629$Time
Process629Position <- tempOutputDescription629$Process
Process629 <- rep(0, length(BackingVacuum629$Value))

for(i in 1:length(BackingVacuum629Time)) {
  for(j in 1:length(Process629Time)) {
    if (j == 1) { 
      if (BackingVacuum629Time[i] < Process629Time[j]) { Process629[i]=Process629Position[j-1] }
      else if (BackingVacuum629Time[i] <= Process629Time[j+1]) { Process629[i]=Process629Position[j]}}
    else if (j == length(Process629Time)) { 
      if (BackingVacuum629Time[i] > Process629Time[j]) { Process629[i]=Process629Position[length(Process629Position)] }}
    else {
      if (BackingVacuum629Time[i] >= Process629Time[j] && BackingVacuum629Time[i] < Process629Time[j+1]) {
        Process629[i] = Process629Position[j]}
    }
  }
}

head(Process629)
BackingVacuum629$Process <- Process629
BackingVacuum629$Build <- "629"
colnames(BackingVacuum629) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBackingVacuum629Freq <- subset(BackingVacuum629 [c(4,5)],)

Process629 <- c(unique(OutputDescription629$Process))
Process629 <- rep(Process629, length(Defects629Layer))
Layer<- rep(Defects629Layer, each=length(unique(OutputDescription629$Process)))
N <- rep(0,length(Process629))

for(i in 1:length(Process629)) {
  for(j in 1:length(Defects629Layer)) {
    if(j==Layer[i]) {
      temptempBackingVacuum629Freq <- subset(tempBackingVacuum629Freq, tempBackingVacuum629Freq$Layer == j)
      N[i] = length(which(temptempBackingVacuum629Freq==Process629[i]))
    }
  }
}

ProcessFreqBackingVacuum629 <- cbind.data.frame(Layer, Process629 , N)
SummaryBackingVacuum629Freq <- aggregate(ProcessFreqBackingVacuum629$N, by=(list((ProcessFreqBackingVacuum629$Process))), FUN=sum)
colnames(SummaryBackingVacuum629Freq) <- c("Process", "Frequency")

SummaryBackingVacuum629Mean <- aggregate(ProcessFreqBackingVacuum629$N, by=(list((ProcessFreqBackingVacuum629$Process))), FUN=mean)
colnames(SummaryBackingVacuum629Mean) <- c("Process", "Average")

SummaryBackingVacuum629629 <- cbind.data.frame(SummaryBackingVacuum629Freq, SummaryBackingVacuum629Mean [c(2)])
SummaryBackingVacuum629629 <- SummaryBackingVacuum629629[order(-SummaryBackingVacuum629629$Frequency),]

# #Plotting around different layers
# BackingVacuum629$Time <- as.POSIXct(BackingVacuum629$Time,format="%H:%M:%OS")
# BackingVacuum629$Value <- as.numeric(BackingVacuum629$Value)
# 
# BackingVac629ProcessPlot <- subset(BackingVacuum629, Layer > 85)
# 
# Defects629$Time <- as.POSIXct(Defects629$Time,format="%H:%M:%OS")
# Groups629 <- ceiling(length(unique(BackingVac629ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups629) {
#   if(i==1) {
#     GroupStart=min(BackingVac629ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBackingVacuum629 <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum629New <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Backing Vacuum Gauge Feedback/","BackingVac629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum629ProcessPlot <- ggplot(data = BackingVacuum629New, aes(x=Time, y=Value)) +
#       geom_point(data=tempBackingVacuum629, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum629New, mapping = aes(x = Time, y = Value))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BackingVacuum629New$Layer)) {
#         if(BackingVacuum629New$Layer[j]==Defects629$Layer[i]) {BackingVacuum629ProcessPlot = BackingVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups629) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BackingVac629ProcessPlot$Layer)
#     tempBackingVacuum629 <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum629New <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Backing Vacuum Gauge Feedback/","BackingVac629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum629ProcessPlot <- ggplot(data = tempBackingVacuum629, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BackingVacuum629New$Layer)) {
#         if(BackingVacuum629New$Layer[j]==Defects629$Layer[i]) {BackingVacuum629ProcessPlot = BackingVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBackingVacuum629 <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum629New <- filter(BackingVac629ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build629/Backing Vacuum Gauge Feedback/","BackingVac629ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum629ProcessPlot <- ggplot(data = tempBackingVacuum629, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum629New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects629$Layer)) {
#       for(j in 1:length(BackingVacuum629New$Layer)) {
#         if(BackingVacuum629New$Layer[j]==Defects629$Layer[i]) {BackingVacuum629ProcessPlot = BackingVacuum629ProcessPlot + geom_vline(xintercept = Defects629$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum629ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build703 ####
##################

BackingVacuum703 <- subset(Build703 [c(1,2,3,6)], X2=="OPC.Vacuum.BackingVacuumGaugeFB" & Date =="2018-07-05")
BackingVacuum703 <- subset(BackingVacuum703[c(2,3,4)])

Time_new <- BackingVacuum703$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects703Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects703Time[j]) { Layer_new[i]=Defects703Layer[j]-1 }
      else if (Time_new[i] <= Defects703Time[j+1]) { Layer_new[i]=Defects703Layer[j]}}
    else if (j == length(Defects703Time)) { 
      if (Time_new[i] > Defects703Time[j]) { Layer_new[i]=Defects703Layer[length(Defects703Time)] }}
    else {
      if (Time_new[i] >= Defects703Time[j] && Time_new[i] < Defects703Time[j+1]) {
        Layer_new[i] = Defects703Layer[j]}
    }
  }
}

BackingVacuum703$Layer <- Layer_new
colnames(BackingVacuum703) <- list("Time", "Variable", "Value", "Layer")
head(BackingVacuum703)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BackingVacuum703 <- subset(BackingVacuum703, Layer > 80 & Layer < max(Defects703Layer))
head(BackingVacuum703)

### Assign processes within each layer
BackingVacuum703Time <- BackingVacuum703$Time
tempOutputDescription703 <- subset(OutputDescription703 [c(1,3)])
Process703Time <- tempOutputDescription703$Time
Process703Position <- tempOutputDescription703$Process
Process703 <- rep(0, length(BackingVacuum703$Value))

for(i in 1:length(BackingVacuum703Time)) {
  for(j in 1:length(Process703Time)) {
    if (j == 1) { 
      if (BackingVacuum703Time[i] < Process703Time[j]) { Process703[i]=Process703Position[j-1] }
      else if (BackingVacuum703Time[i] <= Process703Time[j+1]) { Process703[i]=Process703Position[j]}}
    else if (j == length(Process703Time)) { 
      if (BackingVacuum703Time[i] > Process703Time[j]) { Process703[i]=Process703Position[length(Process703Position)] }}
    else {
      if (BackingVacuum703Time[i] >= Process703Time[j] && BackingVacuum703Time[i] < Process703Time[j+1]) {
        Process703[i] = Process703Position[j]}
    }
  }
}

head(Process703)
BackingVacuum703$Process <- Process703
BackingVacuum703$Build <- "703"
colnames(BackingVacuum703) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBackingVacuum703Freq <- subset(BackingVacuum703 [c(4,5)],)

Process703 <- c(unique(OutputDescription703$Process))
Process703 <- rep(Process703, length(Defects703Layer))
Layer<- rep(Defects703Layer, each=length(unique(OutputDescription703$Process)))
N <- rep(0,length(Process703))

for(i in 1:length(Process703)) {
  for(j in 1:length(Defects703Layer)) {
    if(j==Layer[i]) {
      temptempBackingVacuum703Freq <- subset(tempBackingVacuum703Freq, tempBackingVacuum703Freq$Layer == j)
      N[i] = length(which(temptempBackingVacuum703Freq==Process703[i]))
    }
  }
}

ProcessFreqBackingVacuum703 <- cbind.data.frame(Layer, Process703 , N)
SummaryBackingVacuum703Freq <- aggregate(ProcessFreqBackingVacuum703$N, by=(list((ProcessFreqBackingVacuum703$Process))), FUN=sum)
colnames(SummaryBackingVacuum703Freq) <- c("Process", "Frequency")

SummaryBackingVacuum703Mean <- aggregate(ProcessFreqBackingVacuum703$N, by=(list((ProcessFreqBackingVacuum703$Process))), FUN=mean)
colnames(SummaryBackingVacuum703Mean) <- c("Process", "Average")

SummaryBackingVacuum703 <- cbind.data.frame(SummaryBackingVacuum703Freq, SummaryBackingVacuum703Mean [c(2)])
SummaryBackingVacuum703 <- SummaryBackingVacuum703[order(-SummaryBackingVacuum703$Frequency),]

# #Plotting an Aggregation by Mean
# BackingVacuum703$Value <- as.numeric(BackingVacuum703$Value)
# Aggregate_By_Layer <- aggregate(BackingVacuum703$Value, by=list(BackingVacuum703$Layer), FUN=mean)
# colnames(Aggregate_By_Layer) <- list("Layer", "Value")
# 
# Filtered_Aggregate_By_Layer703<- filter(Aggregate_By_Layer, (Layer>=203 & Layer<=223))
# 
# BackingVacuum703ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer703, aes(x=Layer, y=Value)) +
#     geom_point(data=Filtered_Aggregate_By_Layer703, mapping = aes(x = Layer, y = Value), size=2) +
#     geom_line(data=Filtered_Aggregate_By_Layer703, mapping = aes(x = Layer, y = Value))+
#     ylab("Backing Vacuum Gauge Feedback") +
#     xlab("Time") +
#     ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 203-223"))
# 
# plot(BackingVacuum703ProcessPlot_Agg)

# #Plotting around different layers
# BackingVacuum703$Time <- as.POSIXct(BackingVacuum703$Time,format="%H:%M:%OS")
# BackingVacuum703$Value <- as.numeric(BackingVacuum703$Value)
# 
# BackingVac703ProcessPlot <- subset(BackingVacuum703, Layer > 85)
# 
# Defects703$Time <- as.POSIXct(Defects703$Time,format="%H:%M:%OS")
# Groups703 <- ceiling(length(unique(BackingVac703ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups703) {
#   if(i==1) {
#     GroupStart=min(BackingVac703ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBackingVacuum703 <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum703New <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Backing Vacuum Gauge Feedback/","BackingVac703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum703ProcessPlot <- ggplot(data = BackingVacuum703New, aes(x=Time, y=Value)) +
#       geom_point(data=tempBackingVacuum703, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum703New, mapping = aes(x = Time, y = Value))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BackingVacuum703New$Layer)) {
#         if(BackingVacuum703New$Layer[j]==Defects703$Layer[i]) {BackingVacuum703ProcessPlot = BackingVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups703) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BackingVac703ProcessPlot$Layer)
#     tempBackingVacuum703 <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum703New <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Backing Vacuum Gauge Feedback/","BackingVac703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum703ProcessPlot <- ggplot(data = tempBackingVacuum703, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BackingVacuum703New$Layer)) {
#         if(BackingVacuum703New$Layer[j]==Defects703$Layer[i]) {BackingVacuum703ProcessPlot = BackingVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBackingVacuum703 <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum703New <- filter(BackingVac703ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build703/Backing Vacuum Gauge Feedback/","BackingVac703ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum703ProcessPlot <- ggplot(data = tempBackingVacuum703, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum703New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects703$Layer)) {
#       for(j in 1:length(BackingVacuum703New$Layer)) {
#         if(BackingVacuum703New$Layer[j]==Defects703$Layer[i]) {BackingVacuum703ProcessPlot = BackingVacuum703ProcessPlot + geom_vline(xintercept = Defects703$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum703ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build710 ####
##################

BackingVacuum710 <- subset(Build710 [c(2,3,6)], X2=="OPC.Vacuum.BackingVacuumGaugeFB")

Time_new <- BackingVacuum710$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects710Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects710Time[j]) { Layer_new[i]=Defects710Layer[j]-1 }
      else if (Time_new[i] <= Defects710Time[j+1]) { Layer_new[i]=Defects710Layer[j]}}
    else if (j == length(Defects710Time)) { 
      if (Time_new[i] > Defects710Time[j]) { Layer_new[i]=Defects710Layer[length(Defects710Time)] }}
    else {
      if (Time_new[i] >= Defects710Time[j] && Time_new[i] < Defects710Time[j+1]) {
        Layer_new[i] = Defects710Layer[j]}
    }
  }
}

BackingVacuum710$Layer <- Layer_new
colnames(BackingVacuum710) <- list("Time", "Variable", "Value", "Layer")
head(BackingVacuum710)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BackingVacuum710 <- subset(BackingVacuum710, Layer > 80 & Layer < max(Defects710Layer))
head(BackingVacuum710)

### Assign processes within each layer
BackingVacuum710Time <- BackingVacuum710$Time
tempOutputDescription710 <- subset(OutputDescription710 [c(1,3)])
Process710Time <- tempOutputDescription710$Time
Process710Position <- tempOutputDescription710$Process
Process710 <- rep(0, length(BackingVacuum710$Value))

for(i in 1:length(BackingVacuum710Time)) {
  for(j in 1:length(Process710Time)) {
    if (j == 1) { 
      if (BackingVacuum710Time[i] < Process710Time[j]) { Process710[i]=Process710Position[j-1] }
      else if (BackingVacuum710Time[i] <= Process710Time[j+1]) { Process710[i]=Process710Position[j]}}
    else if (j == length(Process710Time)) { 
      if (BackingVacuum710Time[i] > Process710Time[j]) { Process710[i]=Process710Position[length(Process710Position)] }}
    else {
      if (BackingVacuum710Time[i] >= Process710Time[j] && BackingVacuum710Time[i] < Process710Time[j+1]) {
        Process710[i] = Process710Position[j]}
    }
  }
}

head(Process710)
BackingVacuum710$Process <- Process710
BackingVacuum710$Build <- "710"
colnames(BackingVacuum710) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBackingVacuum710Freq <- subset(BackingVacuum710 [c(4,5)],)

Process710 <- c(unique(OutputDescription710$Process))
Process710 <- rep(Process710, length(Defects710Layer))
Layer<- rep(Defects710Layer, each=length(unique(OutputDescription710$Process)))
N <- rep(0,length(Process710))

for(i in 1:length(Process710)) {
  for(j in 1:length(Defects710Layer)) {
    if(j==Layer[i]) {
      temptempBackingVacuum710Freq <- subset(tempBackingVacuum710Freq, tempBackingVacuum710Freq$Layer == j)
      N[i] = length(which(temptempBackingVacuum710Freq==Process710[i]))
    }
  }
}

ProcessFreqBackingVacuum710 <- cbind.data.frame(Layer, Process710 , N)
SummaryBackingVacuum710Freq <- aggregate(ProcessFreqBackingVacuum710$N, by=(list((ProcessFreqBackingVacuum710$Process))), FUN=sum)
colnames(SummaryBackingVacuum710Freq) <- c("Process", "Frequency")

SummaryBackingVacuum710Mean <- aggregate(ProcessFreqBackingVacuum710$N, by=(list((ProcessFreqBackingVacuum710$Process))), FUN=mean)
colnames(SummaryBackingVacuum710Mean) <- c("Process", "Average")

SummaryBackingVacuum710710 <- cbind.data.frame(SummaryBackingVacuum710Freq, SummaryBackingVacuum710Mean [c(2)])
SummaryBackingVacuum710710 <- SummaryBackingVacuum710710[order(-SummaryBackingVacuum710710$Frequency),]

#Plotting an Aggregation by Mean
BackingVacuum710$Value <- as.numeric(BackingVacuum710$Value)
Aggregate_By_Layer <- aggregate(BackingVacuum710$Value, by=list(BackingVacuum710$Layer), FUN=mean)
colnames(Aggregate_By_Layer) <- list("Layer", "Value")

Filtered_Aggregate_By_Layer710<- filter(Aggregate_By_Layer, (Layer>=234 & Layer<=254))

BackingVacuum710ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer710, aes(x=Layer, y=Value)) +
  geom_point(data=Filtered_Aggregate_By_Layer710, mapping = aes(x = Layer, y = Value), size=2) +
  geom_line(data=Filtered_Aggregate_By_Layer710, mapping = aes(x = Layer, y = Value))+
  ylab("Backing Vacuum Gauge Feedback") +
  xlab("Time") +
  ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 234-254"))

plot(BackingVacuum710ProcessPlot_Agg)

# #Plotting around different layers
# BackingVacuum710$Time <- as.POSIXct(BackingVacuum710$Time,format="%H:%M:%OS")
# BackingVacuum710$Value <- as.numeric(BackingVacuum710$Value)
# 
# BackingVac710ProcessPlot <- subset(BackingVacuum710, Layer > 85)
# 
# Defects710$Time <- as.POSIXct(Defects710$Time,format="%H:%M:%OS")
# Groups710 <- ceiling(length(unique(BackingVac710ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups710) {
#   if(i==1) {
#     GroupStart=min(BackingVac710ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBackingVacuum710 <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum710New <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Backing Vacuum Gauge Feedback/","BackingVac710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum710ProcessPlot <- ggplot(data = BackingVacuum710New, aes(x=Time, y=Value)) +
#       geom_point(data=tempBackingVacuum710, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum710New, mapping = aes(x = Time, y = Value))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BackingVacuum710New$Layer)) {
#         if(BackingVacuum710New$Layer[j]==Defects710$Layer[i]) {BackingVacuum710ProcessPlot = BackingVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups710) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BackingVac710ProcessPlot$Layer)
#     tempBackingVacuum710 <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum710New <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Backing Vacuum Gauge Feedback/","BackingVac710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum710ProcessPlot <- ggplot(data = tempBackingVacuum710, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BackingVacuum710New$Layer)) {
#         if(BackingVacuum710New$Layer[j]==Defects710$Layer[i]) {BackingVacuum710ProcessPlot = BackingVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBackingVacuum710 <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum710New <- filter(BackingVac710ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build710/Backing Vacuum Gauge Feedback/","BackingVac710ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum710ProcessPlot <- ggplot(data = tempBackingVacuum710, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum710New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects710$Layer)) {
#       for(j in 1:length(BackingVacuum710New$Layer)) {
#         if(BackingVacuum710New$Layer[j]==Defects710$Layer[i]) {BackingVacuum710ProcessPlot = BackingVacuum710ProcessPlot + geom_vline(xintercept = Defects710$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum710ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build718 ####
##################

BackingVacuum718 <- subset(Build718 [c(2,3,6)], X2=="OPC.Vacuum.BackingVacuumGaugeFB")

Time_new <- BackingVacuum718$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects718Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects718Time[j]) { Layer_new[i]=Defects718Layer[j]-1 }
      else if (Time_new[i] <= Defects718Time[j+1]) { Layer_new[i]=Defects718Layer[j]}}
    else if (j == length(Defects718Time)) { 
      if (Time_new[i] > Defects718Time[j]) { Layer_new[i]=Defects718Layer[length(Defects718Time)] }}
    else {
      if (Time_new[i] >= Defects718Time[j] && Time_new[i] < Defects718Time[j+1]) {
        Layer_new[i] = Defects718Layer[j]}
    }
  }
}

BackingVacuum718$Layer <- Layer_new
colnames(BackingVacuum718) <- list("Time", "Variable", "Value", "Layer")
head(BackingVacuum718)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BackingVacuum718 <- subset(BackingVacuum718, Layer > 80 & Layer < max(Defects718Layer))
head(BackingVacuum718)

### Assign processes within each layer
BackingVacuum718Time <- BackingVacuum718$Time
tempOutputDescription718 <- subset(OutputDescription718 [c(1,3)])
Process718Time <- tempOutputDescription718$Time
Process718Position <- tempOutputDescription718$Process
Process718 <- rep(0, length(BackingVacuum718$Value))

for(i in 1:length(BackingVacuum718Time)) {
  for(j in 1:length(Process718Time)) {
    if (j == 1) { 
      if (BackingVacuum718Time[i] < Process718Time[j]) { Process718[i]=Process718Position[j-1] }
      else if (BackingVacuum718Time[i] <= Process718Time[j+1]) { Process718[i]=Process718Position[j]}}
    else if (j == length(Process718Time)) { 
      if (BackingVacuum718Time[i] > Process718Time[j]) { Process718[i]=Process718Position[length(Process718Position)] }}
    else {
      if (BackingVacuum718Time[i] >= Process718Time[j] && BackingVacuum718Time[i] < Process718Time[j+1]) {
        Process718[i] = Process718Position[j]}
    }
  }
}

head(Process718)
BackingVacuum718$Process <- Process718
BackingVacuum718$Build <- "718"
colnames(BackingVacuum718) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBackingVacuum718Freq <- subset(BackingVacuum718 [c(4,5)],)

Process718 <- c(unique(OutputDescription718$Process))
Process718 <- rep(Process718, length(Defects718Layer))
Layer<- rep(Defects718Layer, each=length(unique(OutputDescription718$Process)))
N <- rep(0,length(Process718))

for(i in 1:length(Process718)) {
  for(j in 1:length(Defects718Layer)) {
    if(j==Layer[i]) {
      temptempBackingVacuum718Freq <- subset(tempBackingVacuum718Freq, tempBackingVacuum718Freq$Layer == j)
      N[i] = length(which(temptempBackingVacuum718Freq==Process718[i]))
    }
  }
}

ProcessFreqBackingVacuum718 <- cbind.data.frame(Layer, Process718 , N)
SummaryBackingVacuum718Freq <- aggregate(ProcessFreqBackingVacuum718$N, by=(list((ProcessFreqBackingVacuum718$Process))), FUN=sum)
colnames(SummaryBackingVacuum718Freq) <- c("Process", "Frequency")

SummaryBackingVacuum718Mean <- aggregate(ProcessFreqBackingVacuum718$N, by=(list((ProcessFreqBackingVacuum718$Process))), FUN=mean)
colnames(SummaryBackingVacuum718Mean) <- c("Process", "Average")

SummaryBackingVacuum718718 <- cbind.data.frame(SummaryBackingVacuum718Freq, SummaryBackingVacuum718Mean [c(2)])
SummaryBackingVacuum718718 <- SummaryBackingVacuum718718[order(-SummaryBackingVacuum718718$Frequency),]

# #Plotting an Aggregation by Mean
# BackingVacuum718$Value <- as.numeric(BackingVacuum718$Value)
# Aggregate_By_Layer <- aggregate(BackingVacuum718$Value, by=list(BackingVacuum718$Layer), FUN=mean)
# colnames(Aggregate_By_Layer) <- list("Layer", "Value")
# 
# Filtered_Aggregate_By_Layer1<- filter(Aggregate_By_Layer, (Layer>=84 & Layer<=104))
# BackingVacuum718ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer1, aes(x=Layer, y=Value)) +
#   geom_point(data=Filtered_Aggregate_By_Layer1, mapping = aes(x = Layer, y = Value), size=2) +
#   geom_line(data=Filtered_Aggregate_By_Layer1, mapping = aes(x = Layer, y = Value))+
#   ylab("Backing Vacuum Gauge Feedback") +
#   xlab("Time") +
#   ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 84-104"))
# 
# plot(BackingVacuum718ProcessPlot_Agg)
# 
# Filtered_Aggregate_By_Layer2<- filter(Aggregate_By_Layer, (Layer>=203 & Layer<=223))
# BackingVacuum718ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer2, aes(x=Layer, y=Value)) +
#   geom_point(data=Filtered_Aggregate_By_Layer2, mapping = aes(x = Layer, y = Value), size=2) +
#   geom_line(data=Filtered_Aggregate_By_Layer2, mapping = aes(x = Layer, y = Value))+
#   ylab("Backing Vacuum Gauge Feedback") +
#   xlab("Time") +
#   ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 203-223"))
# 
# plot(BackingVacuum718ProcessPlot_Agg)
# 
# Filtered_Aggregate_By_Layer3<- filter(Aggregate_By_Layer, (Layer>=234 & Layer<=254))
# BackingVacuum718ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer3, aes(x=Layer, y=Value)) +
#   geom_point(data=Filtered_Aggregate_By_Layer3, mapping = aes(x = Layer, y = Value), size=2) +
#   geom_line(data=Filtered_Aggregate_By_Layer3, mapping = aes(x = Layer, y = Value))+
#   ylab("Backing Vacuum Gauge Feedback") +
#   xlab("Time") +
#   ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 234-254"))
# 
# plot(BackingVacuum718ProcessPlot_Agg)

# #Plotting around different layers
# BackingVacuum718$Time <- as.POSIXct(BackingVacuum718$Time,format="%H:%M:%OS")
# BackingVacuum718$Value <- as.numeric(BackingVacuum718$Value)
# 
# BackingVac718ProcessPlot <- subset(BackingVacuum718, Layer > 85)
# 
# Defects718$Time <- as.POSIXct(Defects718$Time,format="%H:%M:%OS")
# Groups718 <- ceiling(length(unique(BackingVac718ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups718) {
#   if(i==1) {
#     GroupStart=min(BackingVac718ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBackingVacuum718 <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum718New <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Backing Vacuum Gauge Feedback/","BackingVac718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum718ProcessPlot <- ggplot(data = BackingVacuum718New, aes(x=Time, y=Value)) +
#       geom_point(data=tempBackingVacuum718, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum718New, mapping = aes(x = Time, y = Value))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BackingVacuum718New$Layer)) {
#         if(BackingVacuum718New$Layer[j]==Defects718$Layer[i]) {BackingVacuum718ProcessPlot = BackingVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups718) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BackingVac718ProcessPlot$Layer)
#     tempBackingVacuum718 <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum718New <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Backing Vacuum Gauge Feedback/","BackingVac718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum718ProcessPlot <- ggplot(data = tempBackingVacuum718, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BackingVacuum718New$Layer)) {
#         if(BackingVacuum718New$Layer[j]==Defects718$Layer[i]) {BackingVacuum718ProcessPlot = BackingVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBackingVacuum718 <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum718New <- filter(BackingVac718ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build718/Backing Vacuum Gauge Feedback/","BackingVac718ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum718ProcessPlot <- ggplot(data = tempBackingVacuum718, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum718New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects718$Layer)) {
#       for(j in 1:length(BackingVacuum718New$Layer)) {
#         if(BackingVacuum718New$Layer[j]==Defects718$Layer[i]) {BackingVacuum718ProcessPlot = BackingVacuum718ProcessPlot + geom_vline(xintercept = Defects718$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum718ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

##################
#### Build723 ####
##################

BackingVacuum723 <- subset(Build723 [c(2,3,6)], X2=="OPC.Vacuum.BackingVacuumGaugeFB")

Time_new <- BackingVacuum723$Time
Layer_new <- rep(0, length(Time_new))

for(i in 1:length(Time_new)) {
  for(j in 1:length(Defects723Time)) {
    if (j == 1) { 
      if (Time_new[i] < Defects723Time[j]) { Layer_new[i]=Defects723Layer[j]-1 }
      else if (Time_new[i] <= Defects723Time[j+1]) { Layer_new[i]=Defects723Layer[j]}}
    else if (j == length(Defects723Time)) { 
      if (Time_new[i] > Defects723Time[j]) { Layer_new[i]=Defects723Layer[length(Defects723Time)] }}
    else {
      if (Time_new[i] >= Defects723Time[j] && Time_new[i] < Defects723Time[j+1]) {
        Layer_new[i] = Defects723Layer[j]}
    }
  }
}

BackingVacuum723$Layer <- Layer_new
colnames(BackingVacuum723) <- list("Time", "Variable", "Value", "Layer")
head(BackingVacuum723)

#Only look at layers excluding the support (> 85) and the final layer (< 335)
BackingVacuum723 <- subset(BackingVacuum723, Layer > 80 & Layer < max(Defects723Layer))
head(BackingVacuum723)

### Assign processes within each layer
BackingVacuum723Time <- BackingVacuum723$Time
tempOutputDescription723 <- subset(OutputDescription723 [c(1,3)])
Process723Time <- tempOutputDescription723$Time
Process723Position <- tempOutputDescription723$Process
Process723 <- rep(0, length(BackingVacuum723$Value))

for(i in 1:length(BackingVacuum723Time)) {
  for(j in 1:length(Process723Time)) {
    if (j == 1) { 
      if (BackingVacuum723Time[i] < Process723Time[j]) { Process723[i]=Process723Position[j-1] }
      else if (BackingVacuum723Time[i] <= Process723Time[j+1]) { Process723[i]=Process723Position[j]}}
    else if (j == length(Process723Time)) { 
      if (BackingVacuum723Time[i] > Process723Time[j]) { Process723[i]=Process723Position[length(Process723Position)] }}
    else {
      if (BackingVacuum723Time[i] >= Process723Time[j] && BackingVacuum723Time[i] < Process723Time[j+1]) {
        Process723[i] = Process723Position[j]}
    }
  }
}

head(Process723)
BackingVacuum723$Process <- Process723
BackingVacuum723$Build <- "723"
colnames(BackingVacuum723) <- list("Time", "Variable", "Value", "Layer", "Process", "Build")

#Create a table to count the number of times an action is performed in each layer
tempBackingVacuum723Freq <- subset(BackingVacuum723 [c(4,5)],)

Process723 <- c(unique(OutputDescription723$Process))
Process723 <- rep(Process723, length(Defects723Layer))
Layer<- rep(Defects723Layer, each=length(unique(OutputDescription723$Process)))
N <- rep(0,length(Process723))

for(i in 1:length(Process723)) {
  for(j in 1:length(Defects723Layer)) {
    if(j==Layer[i]) {
      temptempBackingVacuum723Freq <- subset(tempBackingVacuum723Freq, tempBackingVacuum723Freq$Layer == j)
      N[i] = length(which(temptempBackingVacuum723Freq==Process723[i]))
    }
  }
}

ProcessFreqBackingVacuum723 <- cbind.data.frame(Layer, Process723 , N)
SummaryBackingVacuum723Freq <- aggregate(ProcessFreqBackingVacuum723$N, by=(list((ProcessFreqBackingVacuum723$Process))), FUN=sum)
colnames(SummaryBackingVacuum723Freq) <- c("Process", "Frequency")

SummaryBackingVacuum723Mean <- aggregate(ProcessFreqBackingVacuum723$N, by=(list((ProcessFreqBackingVacuum723$Process))), FUN=mean)
colnames(SummaryBackingVacuum723Mean) <- c("Process", "Average")

SummaryBackingVacuum723723 <- cbind.data.frame(SummaryBackingVacuum723Freq, SummaryBackingVacuum723Mean [c(2)])
SummaryBackingVacuum723723 <- SummaryBackingVacuum723723[order(-SummaryBackingVacuum723723$Frequency),]

# #Plotting an Aggregation by Mean
# BackingVacuum723$Value <- as.numeric(BackingVacuum723$Value)
# Aggregate_By_Layer <- aggregate(BackingVacuum723$Value, by=list(BackingVacuum723$Layer), FUN=mean)
# colnames(Aggregate_By_Layer) <- list("Layer", "Value")
# 
# Filtered_Aggregate_By_Layer723<- filter(Aggregate_By_Layer, (Layer>=84 & Layer<=104))
# 
# BackingVacuum723ProcessPlot_Agg <- ggplot(data = Filtered_Aggregate_By_Layer723, aes(x=Layer, y=Value)) +
#   geom_point(data=Filtered_Aggregate_By_Layer723, mapping = aes(x = Layer, y = Value), size=2) +
#   geom_line(data=Filtered_Aggregate_By_Layer723, mapping = aes(x = Layer, y = Value))+
#   ylab("Backing Vacuum Gauge Feedback") +
#   xlab("Time") +
#   ggtitle(paste("Aggregated Backing Vacuum Gauge Feedback \n Layers 84-104"))
# 
# plot(BackingVacuum723ProcessPlot_Agg)

# #Plotting around different layers
# BackingVacuum723$Time <- as.POSIXct(BackingVacuum723$Time,format="%H:%M:%OS")
# BackingVacuum723$Value <- as.numeric(BackingVacuum723$Value)
# 
# BackingVac723ProcessPlot <- subset(BackingVacuum723, Layer > 85)
# 
# Defects723$Time <- as.POSIXct(Defects723$Time,format="%H:%M:%OS")
# Groups723 <- ceiling(length(unique(BackingVac723ProcessPlot$Layer))/5)
# 
# for(i in 1:Groups723) {
#   if(i==1) {
#     GroupStart=min(BackingVac723ProcessPlot$Layer)
#     GroupEnd=GroupStart+4
#     tempBackingVacuum723 <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum723New <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Backing Vacuum Gauge Feedback/","BackingVac723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum723ProcessPlot <- ggplot(data = BackingVacuum723New, aes(x=Time, y=Value)) +
#       geom_point(data=tempBackingVacuum723, mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum723New, mapping = aes(x = Time, y = Value))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BackingVacuum723New$Layer)) {
#         if(BackingVacuum723New$Layer[j]==Defects723$Layer[i]) {BackingVacuum723ProcessPlot = BackingVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else if(i==Groups723) {
#     GroupStart=GroupEnd+1
#     GroupEnd=max(BackingVac723ProcessPlot$Layer)
#     tempBackingVacuum723 <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum723New <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Backing Vacuum Gauge Feedback/","BackingVac723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum723ProcessPlot <- ggplot(data = tempBackingVacuum723, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BackingVacuum723New$Layer)) {
#         if(BackingVacuum723New$Layer[j]==Defects723$Layer[i]) {BackingVacuum723ProcessPlot = BackingVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()}
# 
#   else {
#     GroupStart=GroupEnd+1
#     GroupEnd=GroupStart+4
#     tempBackingVacuum723 <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
#     BackingVacuum723New <- filter(BackingVac723ProcessPlot, Layer>=GroupStart & Layer<=GroupEnd)
# 
#     #Set Plot location and dimensions
#     png(file = paste("C:/Users/Ethan/Desktop/Training3/ProcessPlots/Build723/Backing Vacuum Gauge Feedback/","BackingVac723ProcessPlot",i,".png" ,sep = ""),
#         width = 500,
#         height = 350)
#     
#     #Create Plot
#     theme_update(plot.title = element_text(hjust = 0.5))
#     BackingVacuum723ProcessPlot <- ggplot(data = tempBackingVacuum723, aes(x=Layer, y=Value)) +
#       geom_point(mapping = aes(x = Time, y = Value, color=Process), size=2) +
#       geom_line(data=BackingVacuum723New, mapping = aes(x = Time, y = Value,))+
#       ylab("Backing Vacuum Gauge Feedback") +
#       xlab("Time") +
#       ggtitle(paste("Processes of Backing Vacuum Gauge Feedback \n Layers", GroupStart, "-", GroupEnd))
# 
#     for(i in 1:length(Defects723$Layer)) {
#       for(j in 1:length(BackingVacuum723New$Layer)) {
#         if(BackingVacuum723New$Layer[j]==Defects723$Layer[i]) {BackingVacuum723ProcessPlot = BackingVacuum723ProcessPlot + geom_vline(xintercept = Defects723$Time[i], color= "blue")}
#       }
#     }
# 
#     plot(BackingVacuum723ProcessPlot)
#     
#     #Run devoff() to create file
#     dev.off()
#   }
# }

#########################################

#####Combine all datasets####
BackingVacuumAll <- rbind(BackingVacuum629,
                          BackingVacuum703,
                          BackingVacuum710,
                          BackingVacuum718,
                          BackingVacuum723)

BackingVacuumAll$Layer_ID <- paste(BackingVacuumAll$Build, "/", BackingVacuumAll$Layer)

###########################################################################################################################


#########################################
#########################################
########### MODEL EXPLORATION ###########
#########################################
#########################################

#### Create a table with layers across all builds ####
#Layer_ID <- "Build / Layer"
Defects629$Date <- "629"
Defects703$Date <- "703"
Defects710$Date <- "710"
Defects718$Date <- "718"
Defects723$Date <- "723"

#This table will be used to index layers in associated variable dataframes
Defects629 <- subset(Defects629, Layer >= 80 & Layer < 335)
Defects703 <- subset(Defects703, Layer >= 80 & Layer < 335)
Defects710 <- subset(Defects710, Layer >= 80 & Layer < 335)
Defects718 <- subset(Defects718, Layer >= 80 & Layer < 335)
Defects723 <- subset(Defects723, Layer >= 80 & Layer < 335)

AllBuildLayers <- rbind(Defects629, Defects703, Defects710, Defects718, Defects723)
AllBuildLayers <- subset(AllBuildLayers[c(5,6)])

AllBuildLayers$Layer_ID <- paste(AllBuildLayers$Date, "/" ,AllBuildLayers$Layer)
AllBuildLayers <- subset(AllBuildLayers[c(3)])

#This table will be our model dataset
Defects629 <- subset(Defects629, Layer >= 86 & Layer < 335)
Defects703 <- subset(Defects703, Layer >= 86 & Layer < 335)
Defects710 <- subset(Defects710, Layer >= 86 & Layer < 335)
Defects718 <- subset(Defects718, Layer >= 86 & Layer < 335)
Defects723 <- subset(Defects723, Layer >= 86 & Layer < 335)

AllData <- rbind(Defects629, Defects703, Defects710, Defects718, Defects723)
AllData <- subset(AllData[c(5,6)])

AllData$Layer_ID <- paste(AllData$Date, "/" ,AllData$Layer)
AllData <- subset(AllData[c(3)])

#Add a Response Column which identifies Layer_ID's where a defect occurred
DefectList629$Date <- "629"
DefectList703$Date <- "703"
DefectList710$Date <- "710"
DefectList718$Date <- "718"
DefectList723$Date <- "723"

AllDefectsList <- rbind(DefectList629,
                        DefectList703,
                        DefectList710,
                        DefectList718,
                        DefectList723,
                        DefectsListNew)

for(i in 1:length(AllDefectsList$Layer)) {
  AllDefectsList$Layer_ID[i] <- paste(AllDefectsList$Date[i], "/" ,AllDefectsList$Layer[i])
}

AllData$Response <- rep(0, length(AllData$Layer_ID))

for(i in 1:length(AllData$Response)) {
  if(AllData$Layer_ID[i] %in% AllDefectsList$Layer_ID) {AllData$Response[i] = 1}
  else{AllData$Response[i] = 0}
}

# AllData$Response <- as.factor(AllData$Response)

######################################################################################################################################
##############################
#### Smoke Detector Count ####
##############################

#### Covariates: Smoke Detector Count Value during Preheat Count ####
AllData$PreHeat_SDC_gt40_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt40_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt40_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 1.) Count how many times in the last 1 layer the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatSDC_gt40$Layer_ID) {
    tempfilterPreheatSDC_gt40 <- filter(tempPreheatSDC_gt40, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PreHeat_SDC_gt40_Count_Last[i] = length(tempfilterPreheatSDC_gt40$Layer_ID)}
  else {AllData$PreHeat_SDC_gt40_Count_Last[i] = 0}
}

## 2.) Count how many times in the last 2 layers the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt40, Layer_ID %in% Last2Layers)
  AllData$PreHeat_SDC_gt40_Count_Last2[i] <- length(tempfilterPreheatSDC$Layer_ID)
}

## 3.) Count how many times in the last 5 layers the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt40, Layer_ID %in% Last5Layers)
  AllData$PreHeat_SDC_gt40_Count_Last5[i] <- length(tempfilterPreheatSDC$Layer_ID)
}


#### Covariates: Smoke Detector Count Value during Preheat Binary ####
AllData$PreHeat_SDC_gt40_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt40_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt40_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 4.) Binary how many times in the last 1 layer the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatSDC_gt40$Layer_ID) {AllData$PreHeat_SDC_gt40_Binary_Last[i] = 1}
  else {AllData$PreHeat_SDC_gt40_Binary_Last[i] = 0}
}

AllData$PreHeat_SDC_gt40_Binary_Last <- as.factor(AllData$PreHeat_SDC_gt40_Binary_Last)

## 5.) Binary how many times in the last 2 layers the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt40, Layer_ID %in% Last2Layers)
  if(length(tempfilterPreheatSDC$Layer_ID)==0) {AllData$PreHeat_SDC_gt40_Binary_Last2[i] <- 0}
  else{AllData$PreHeat_SDC_gt40_Binary_Last2[i] <- 1}
}

AllData$PreHeat_SDC_gt40_Binary_Last2 <- as.factor(AllData$PreHeat_SDC_gt40_Binary_Last2)

## 6.) Binary how many times in the last 5 layers the Smoke Detector Count rises above 40 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[1]: Preheat")
tempPreheatSDC_gt40$Layer_ID <- paste(tempPreheatSDC_gt40$Build, "/", tempPreheatSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt40, Layer_ID %in% Last5Layers)
  if(length(tempfilterPreheatSDC$Layer_ID)==0) {AllData$PreHeat_SDC_gt40_Binary_Last5[i] <- 0}
  else{AllData$PreHeat_SDC_gt40_Binary_Last5[i] <- 1}
}

AllData$PreHeat_SDC_gt40_Binary_Last5 <- as.factor(AllData$PreHeat_SDC_gt40_Binary_Last5)


#### Covariates: Smoke Detector Count Value during Melt Count ####
AllData$Melt_SDC_gt40_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt40_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt40_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 7.) Count how many times in the last 1 layer the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempMeltSDC_gt40$Layer_ID) {
    tempfilterMeltSDC_gt40 <- filter(tempMeltSDC_gt40, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$Melt_SDC_gt40_Count_Last[i] = length(tempfilterMeltSDC_gt40$Layer_ID)}
  else {AllData$Melt_SDC_gt40_Count_Last[i] = 0}
}

## 8.) Count how many times in the last 2 layers the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt40, Layer_ID %in% Last2Layers)
  AllData$Melt_SDC_gt40_Count_Last2[i] <- length(tempfilterMeltSDC$Layer_ID)
}

## 9.) Count how many times in the last 5 layers the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt40, Layer_ID %in% Last5Layers)
  AllData$Melt_SDC_gt40_Count_Last5[i] <- length(tempfilterMeltSDC$Layer_ID)
}


#### Covariates: Smoke Detector Count Value during Melt Binary ####
AllData$Melt_SDC_gt40_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt40_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt40_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 10.) Binary how many times in the last 1 layer the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempMeltSDC_gt40$Layer_ID) {AllData$Melt_SDC_gt40_Binary_Last[i] = 1}
  else {AllData$Melt_SDC_gt40_Binary_Last[i] = 0}
}

AllData$Melt_SDC_gt40_Binary_Last <- as.factor(AllData$Melt_SDC_gt40_Binary_Last)

## 11.) Binary how many times in the last 2 layers the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt40, Layer_ID %in% Last2Layers)
  if(length(tempfilterMeltSDC$Layer_ID)==0) {AllData$Melt_SDC_gt40_Binary_Last2[i] <- 0}
  else{AllData$Melt_SDC_gt40_Binary_Last2[i] <- 1}
}

AllData$Melt_SDC_gt40_Binary_Last2 <- as.factor(AllData$Melt_SDC_gt40_Binary_Last2)

## 12.) Binary how many times in the last 5 layers the Smoke Detector Count rises above 40 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt40 <- filter(SmokeDetectorCountAll, Value > 40 & Process == "[3]: Melt")
tempMeltSDC_gt40$Layer_ID <- paste(tempMeltSDC_gt40$Build, "/", tempMeltSDC_gt40$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt40, Layer_ID %in% Last5Layers)
  if(length(tempfilterMeltSDC$Layer_ID)==0) {AllData$Melt_SDC_gt40_Binary_Last5[i] <- 0}
  else{AllData$Melt_SDC_gt40_Binary_Last5[i] <- 1}
}

AllData$Melt_SDC_gt40_Binary_Last5 <- as.factor(AllData$Melt_SDC_gt40_Binary_Last5)


#### Covariates: Smoke Detector Count Value during Preheat Count ####
AllData$PreHeat_SDC_gt60_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt60_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt60_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 13.) Count how many times in the last 1 layer the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatSDC_gt60$Layer_ID) {
    tempfilterPreheatSDC_gt60 <- filter(tempPreheatSDC_gt60, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PreHeat_SDC_gt60_Count_Last[i] = length(tempfilterPreheatSDC_gt60$Layer_ID)}
  else {AllData$PreHeat_SDC_gt60_Count_Last[i] = 0}
}

## 14.) Count how many times in the last 2 layers the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt60, Layer_ID %in% Last2Layers)
  AllData$PreHeat_SDC_gt60_Count_Last2[i] <- length(tempfilterPreheatSDC$Layer_ID)
}

## 15.) Count how many times in the last 5 layers the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt60, Layer_ID %in% Last5Layers)
  AllData$PreHeat_SDC_gt60_Count_Last5[i] <- length(tempfilterPreheatSDC$Layer_ID)
}


#### Covariates: Smoke Detector Count Value during Preheat Binary ####
AllData$PreHeat_SDC_gt60_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt60_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_SDC_gt60_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 16.) Binary how many times in the last 1 layer the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatSDC_gt60$Layer_ID) {AllData$PreHeat_SDC_gt60_Binary_Last[i] = 1}
  else {AllData$PreHeat_SDC_gt60_Binary_Last[i] = 0}
}

AllData$PreHeat_SDC_gt60_Binary_Last <- as.factor(AllData$PreHeat_SDC_gt60_Binary_Last)

## 17.) Binary how many times in the last 2 layers the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt60, Layer_ID %in% Last2Layers)
  if(length(tempfilterPreheatSDC$Layer_ID)==0) {AllData$PreHeat_SDC_gt60_Binary_Last2[i] <- 0}
  else{AllData$PreHeat_SDC_gt60_Binary_Last2[i] <- 1}
}

AllData$PreHeat_SDC_gt60_Binary_Last2 <- as.factor(AllData$PreHeat_SDC_gt60_Binary_Last2)

## 18.) Binary how many times in the last 5 layers the Smoke Detector Count rises above 60 during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[1]: Preheat")
tempPreheatSDC_gt60$Layer_ID <- paste(tempPreheatSDC_gt60$Build, "/", tempPreheatSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatSDC <- filter(tempPreheatSDC_gt60, Layer_ID %in% Last5Layers)
  if(length(tempfilterPreheatSDC$Layer_ID)==0) {AllData$PreHeat_SDC_gt60_Binary_Last5[i] <- 0}
  else{AllData$PreHeat_SDC_gt60_Binary_Last5[i] <- 1}
}

AllData$PreHeat_SDC_gt60_Binary_Last5 <- as.factor(AllData$PreHeat_SDC_gt60_Binary_Last5)


#### Covariates: Smoke Detector Count Value during Melt Count ####
AllData$Melt_SDC_gt60_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt60_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt60_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 19.) Count how many times in the last 1 layer the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempMeltSDC_gt60$Layer_ID) {
    tempfilterMeltSDC_gt60 <- filter(tempMeltSDC_gt60, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$Melt_SDC_gt60_Count_Last[i] = length(tempfilterMeltSDC_gt60$Layer_ID)}
  else {AllData$Melt_SDC_gt60_Count_Last[i] = 0}
}

## 20.) Count how many times in the last 2 layers the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt60, Layer_ID %in% Last2Layers)
  AllData$Melt_SDC_gt60_Count_Last2[i] <- length(tempfilterMeltSDC$Layer_ID)
}

## 21.) Count how many times in the last 5 layers the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt60, Layer_ID %in% Last5Layers)
  AllData$Melt_SDC_gt60_Count_Last5[i] <- length(tempfilterMeltSDC$Layer_ID)
}


#### Covariates: Smoke Detector Count Value during Melt Binary ####
AllData$Melt_SDC_gt60_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt60_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_gt60_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 22.) Binary how many times in the last 1 layer the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempMeltSDC_gt60$Layer_ID) {AllData$Melt_SDC_gt60_Binary_Last[i] = 1}
  else {AllData$Melt_SDC_gt60_Binary_Last[i] = 0}
}

AllData$Melt_SDC_gt60_Binary_Last <- as.factor(AllData$Melt_SDC_gt60_Binary_Last)

## 23.) Binary how many times in the last 2 layers the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt60, Layer_ID %in% Last2Layers)
  if(length(tempfilterMeltSDC$Layer_ID)==0) {AllData$Melt_SDC_gt60_Binary_Last2[i] <- 0}
  else{AllData$Melt_SDC_gt60_Binary_Last2[i] <- 1}
}

AllData$Melt_SDC_gt60_Binary_Last2 <- as.factor(AllData$Melt_SDC_gt60_Binary_Last2)

## 24.) Binary how many times in the last 5 layers the Smoke Detector Count rises above 60 during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC_gt60 <- filter(SmokeDetectorCountAll, Value > 60 & Process == "[3]: Melt")
tempMeltSDC_gt60$Layer_ID <- paste(tempMeltSDC_gt60$Build, "/", tempMeltSDC_gt60$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterMeltSDC <- filter(tempMeltSDC_gt60, Layer_ID %in% Last5Layers)
  if(length(tempfilterMeltSDC$Layer_ID)==0) {AllData$Melt_SDC_gt60_Binary_Last5[i] <- 0}
  else{AllData$Melt_SDC_gt60_Binary_Last5[i] <- 1}
}

AllData$Melt_SDC_gt60_Binary_Last5 <- as.factor(AllData$Melt_SDC_gt60_Binary_Last5)


#### Covariates: Smoke Detector Count Mean Value during Preheat ####
AllData$Preheat_SDC_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_SDC_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_SDC_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempPreheatSDC <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat")
tempAggPreheatSDC <- aggregate(tempPreheatSDC$Value, by = list(tempPreheatSDC$Layer_ID), FUN=mean)
colnames(tempAggPreheatSDC) <- list("Layer_ID", "Mean")

MissingLayers <- NULL
for(i in 1:length(AllData$Layer_ID)) {
  if(AllData$Layer_ID[i]  %in% tempAggPreheatSDC$Layer_ID == FALSE) {
    if(length(MissingLayers$Layer_ID) == 0) {
      MissingLayers <- cbind.data.frame(AllData$Layer_ID[i],0)
      colnames(MissingLayers) <- list("Layer_ID", "Mean")}
    else{
      MissingLayers <- rbind(MissingLayers, c(AllData$Layer_ID[i], 0)) 
    }
  }
}

tempAggPreheatSDC <- rbind(tempAggPreheatSDC, MissingLayers)

#Separate Build and Layer to sort
tempAggPreheatSDC <- separate(tempAggPreheatSDC, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempAggPreheatSDC$Layer <- as.numeric(tempAggPreheatSDC$Layer)
tempAggPreheatSDC <- tempAggPreheatSDC[order(tempAggPreheatSDC$Build, tempAggPreheatSDC$Layer),]

## 25.) Mean Smoke Detector Count in the last 1 layer during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  AllData$Preheat_SDC_Mean_Last[i] <- tempAggPreheatSDC$Mean[match(AllData$Layer_ID[i], tempAggPreheatSDC$Layer_ID)-1]
}

## 26.) Mean Smoke Detector Count in the last 2 layers during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempAggPreheatSDC_Last2 <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat" & Layer_ID %in% Last2Layers)
  AllData$Preheat_SDC_Mean_Last2[i] <- mean(tempAggPreheatSDC_Last2$Value)
}

## 27.) Mean Smoke Detector Count in the last 5 layers during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempAggPreheatSDC_Last5 <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat" & Layer_ID %in% Last5Layers)
  AllData$Preheat_SDC_Mean_Last5[i] <- mean(tempAggPreheatSDC_Last5$Value)
}


#### Covariates: Smoke Detector Count Mean Value during Melt ####
AllData$Melt_SDC_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempMeltSDC <- filter(SmokeDetectorCountAll, Process == "[3]: Melt")
tempAggMeltSDC <- aggregate(tempMeltSDC$Value, by = list(tempMeltSDC$Layer_ID), FUN=mean)
colnames(tempAggMeltSDC) <- list("Layer_ID", "Mean")

MissingLayers <- NULL
for(i in 1:length(AllData$Layer_ID)) {
  if(AllData$Layer_ID[i]  %in% tempAggMeltSDC$Layer_ID == FALSE) {
    if(length(MissingLayers$Layer_ID) == 0) {
      MissingLayers <- cbind.data.frame(AllData$Layer_ID[i],0)
      colnames(MissingLayers) <- list("Layer_ID", "Mean")}
    else{
      MissingLayers <- rbind(MissingLayers, c(AllData$Layer_ID[i], 0)) 
    }
  }
}

tempAggMeltSDC <- rbind(tempAggMeltSDC, MissingLayers)

#Separate Build and Layer to sort
tempAggMeltSDC <- separate(tempAggMeltSDC, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempAggMeltSDC$Layer <- as.numeric(tempAggMeltSDC$Layer)
tempAggMeltSDC <- tempAggMeltSDC[order(tempAggMeltSDC$Build, tempAggMeltSDC$Layer),]

## 28.) Mean Smoke Detector Count in the last 1 layer during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  AllData$Melt_SDC_Mean_Last[i] <- tempAggMeltSDC$Mean[match(AllData$Layer_ID[i], tempAggMeltSDC$Layer_ID)-1]
}

## 29.) Mean Smoke Detector Count in the last 2 layers during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempAggMeltSDC_Last2 <- filter(SmokeDetectorCountAll, Process == "[3]: Melt" & Layer_ID %in% Last2Layers)
  AllData$Melt_SDC_Mean_Last2[i] <- mean(tempAggMeltSDC_Last2$Value)
}

## 30.) Mean Smoke Detector Count in the last 5 layers during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempAggMeltSDC_Last5 <- filter(SmokeDetectorCountAll, Process == "[3]: Melt" & Layer_ID %in% Last5Layers)
  AllData$Melt_SDC_Mean_Last5[i] <- mean(tempAggMeltSDC_Last5$Value)
}


#### Covariates: Smoke Detector Count Variance Value during Preheat ####
AllData$Preheat_SDC_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_SDC_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_SDC_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 31.) Variance Smoke Detector Count in the last 1 layer during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_SDC <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_SDC$Value) <=1) {AllData$Preheat_SDC_Var_Last[i] <- 0}
  else{AllData$Preheat_SDC_Var_Last[i] <- var(filter_SDC$Value)}
}

## 32.) Variance Smoke Detector Count in the last 2 layer during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(filter_SDC$Value) <=1) {AllData$Preheat_SDC_Var_Last2[i] <- 0}
  else{AllData$Preheat_SDC_Var_Last2[i] <- var(filter_SDC$Value)}
}

## 33.) Variance Smoke Detector Count in the last 5 layer during a Preheat Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(SmokeDetectorCountAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(filter_SDC$Value) <=1) {AllData$Preheat_SDC_Var_Last5[i] <- 0}
  else{AllData$Preheat_SDC_Var_Last5[i] <- var(filter_SDC$Value)}
}

#### Covariates: Smoke Detector Count Variance Value during Melt ####
AllData$Melt_SDC_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_SDC_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 34.) Variance Smoke Detector Count in the last 1 layer during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_SDC <- filter(SmokeDetectorCountAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_SDC$Value)<=1) {AllData$Melt_SDC_Var_Last[i] <- 0}
  else{AllData$Melt_SDC_Var_Last[i] <- var(filter_SDC$Value)}
}

## 35.) Variance Smoke Detector Count in the last 2 layer during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(SmokeDetectorCountAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(filter_SDC$Value)<=1) {AllData$Melt_SDC_Var_Last2[i] <- 0}
  else{AllData$Melt_SDC_Var_Last2[i] <- var(filter_SDC$Value)}
}

## 36.) Variance Smoke Detector Count in the last 5 layer during a Melt Process ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(SmokeDetectorCountAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(filter_SDC$Value)<=1) {AllData$Melt_SDC_Var_Last5[i] <- 0}
  else{AllData$Melt_SDC_Var_Last5[i] <- var(filter_SDC$Value)}
}


#### Covariate: Smoke Detector Count Process Repeats/Resets Count ####
AllData$SDC_ProcessRepeats_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$SDC_ProcessRepeats_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$SDC_ProcessRepeats_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 37.) Count how many times in the last 1 layer the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()

  for(j in 1:length(filteredSmokeProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredSmokeProcess$Process[j]
      if(filteredSmokeProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredSmokeProcess$Layer_ID)) {
      AllData$SDC_ProcessRepeats_Count_Last[i] = ProcessRepeats
    }
    else if(filteredSmokeProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredSmokeProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredSmokeProcess$Process[j+1]}
      else {
        CurrentProcess = filteredSmokeProcess$Process[j+1]}
    }
  }
}

## 38.) Count how many times in the last 2 layers the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredSmokeProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredSmokeProcessLayer <- filter(filteredSmokeProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredSmokeProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredSmokeProcessLayer$Process[l]
        if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredSmokeProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredSmokeProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$SDC_ProcessRepeats_Count_Last2[i] <- RepeatsInLayers
}

## 39.) Count how many times in the last 5 layers the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredSmokeProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredSmokeProcessLayer <- filter(filteredSmokeProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredSmokeProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredSmokeProcessLayer$Process[l]
        if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredSmokeProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredSmokeProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$SDC_ProcessRepeats_Count_Last5[i] <- RepeatsInLayers
}


#### Covariate: Smoke Detector Count Process Repeats/Resets Binary ####
AllData$SDC_ProcessRepeats_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$SDC_ProcessRepeats_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$SDC_ProcessRepeats_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 40.) Binary if in the last 1 layer the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()
  
  for(j in 1:length(filteredSmokeProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredSmokeProcess$Process[j]
      if(filteredSmokeProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredSmokeProcess$Layer_ID)) {
      if(ProcessRepeats == 0) {AllData$SDC_ProcessRepeats_Binary_Last[i] = 0}
      else {AllData$SDC_ProcessRepeats_Binary_Last[i] = 1}
    }
    else if(filteredSmokeProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredSmokeProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredSmokeProcess$Process[j+1]}
      else {
        CurrentProcess = filteredSmokeProcess$Process[j+1]}
    }
  }
}

AllData$SDC_ProcessRepeats_Binary_Last <- as.factor(AllData$SDC_ProcessRepeats_Binary_Last)

## 41.) Binary if in the last 2 layers the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredSmokeProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredSmokeProcessLayer <- filter(filteredSmokeProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredSmokeProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredSmokeProcessLayer$Process[l]
        if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredSmokeProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredSmokeProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$SDC_ProcessRepeats_Binary_Last2[i] = 0}
  else{AllData$SDC_ProcessRepeats_Binary_Last2[i] = 1}
}

AllData$SDC_ProcessRepeats_Binary_Last2 <- as.factor(AllData$SDC_ProcessRepeats_Binary_Last2)

## 42.) Binary if in the last 5 layers the Smoke Detector Count processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredSmokeProcess <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredSmokeProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredSmokeProcessLayer <- filter(filteredSmokeProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredSmokeProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredSmokeProcessLayer$Process[l]
        if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredSmokeProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredSmokeProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredSmokeProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredSmokeProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$SDC_ProcessRepeats_Binary_Last5[i] = 0}
  else{AllData$SDC_ProcessRepeats_Binary_Last5[i] = 1}
}

AllData$SDC_ProcessRepeats_Binary_Last5 <- as.factor(AllData$SDC_ProcessRepeats_Binary_Last5)


######################################################################################################################################
##############################
######## Beam Current ########
##############################

#### Covariates: Beam Current Value during Preheat Count####
AllData$PreHeat_BC_lt4.9_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt4.9_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt4.9_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 43.) Count how many times in the last 1 layer the Beam Current drops below 4.91898 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatBC_lt4.9$Layer_ID) {
    tempfilterPreheatBC_lt4.9 <- filter(tempPreheatBC_lt4.9, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PreHeat_BC_lt4.9_Count_Last[i] = length(tempfilterPreheatBC_lt4.9$Layer_ID)}
  else {AllData$PreHeat_BC_lt4.9_Count_Last[i] = 0}
}

## 44.) Count how many times in the last 2 layers the Beam Current drops below 4.91898 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatBC <- filter(tempPreheatBC_lt4.9, Layer_ID %in% Last2Layers)
  AllData$PreHeat_BC_lt4.9_Count_Last2[i] <- length(tempfilterPreheatBC$Layer_ID)
}

## 45.) Count how many times in the last 5 layers the Beam Current drops below 4.91898 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatBC <- filter(tempPreheatBC_lt4.9, Layer_ID %in% Last5Layers)
  AllData$PreHeat_BC_lt4.9_Count_Last5[i] <- length(tempfilterPreheatBC$Layer_ID)
}


#### Covariates: Beam Current Value during Preheat Binary####
AllData$PreHeat_BC_lt4.9_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt4.9_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt4.9_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 46.) Binary in the last 1 layer does the Beam Current drops below 4.91898 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatBC_lt4.9$Layer_ID) {AllData$PreHeat_BC_lt4.9_Binary_Last[i] = 1}
  else {AllData$PreHeat_BC_lt4.9_Binary_Last[i] = 0}
}

AllData$PreHeat_BC_lt4.9_Binary_Last <- as.factor(AllData$PreHeat_BC_lt4.9_Binary_Last)

## 47.) Binary in the last 2 layer does the Beam Current drops below 4.91898 during a Preheat Process? ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPreheatBC <- filter(tempPreheatBC_lt4.9, Layer_ID %in% Last2Layers)
  if(length(tempfilterPreheatBC$Layer_ID)==0) {AllData$PreHeat_BC_lt4.9_Binary_Last2[i] <- 0}
  else{AllData$PreHeat_BC_lt4.9_Binary_Last2[i] <- 1}
}

AllData$PreHeat_BC_lt4.9_Binary_Last2 <- as.factor(AllData$PreHeat_BC_lt4.9_Binary_Last2)

## 48.) Binary in the last 5 layer does the Beam Current drops below 4.91898 during a Preheat Process? ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt4.9 <- filter(BeamCurrentAll, Value < 4.91898 & Process == "[1]: Preheat")
tempPreheatBC_lt4.9$Layer_ID <- paste(tempPreheatBC_lt4.9$Build, "/", tempPreheatBC_lt4.9$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPreheatBC <- filter(tempPreheatBC_lt4.9, Layer_ID %in% Last5Layers)
  if(length(tempfilterPreheatBC$Layer_ID)==0) {AllData$PreHeat_BC_lt4.9_Binary_Last5[i] <- 0}
  else{AllData$PreHeat_BC_lt4.9_Binary_Last5[i] <- 1}
}

AllData$PreHeat_BC_lt4.9_Binary_Last5 <- as.factor(AllData$PreHeat_BC_lt4.9_Binary_Last5)


#### Covariates: Beam Current Value during Preheat Count####
AllData$PreHeat_BC_lt3_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt3_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt3_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 49.) Count how many times in the last 1 layer the Beam Current drops below 3 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatBC_lt3$Layer_ID) {
    tempfilterPreheatBC_lt3 <- filter(tempPreheatBC_lt3, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PreHeat_BC_lt3_Count_Last[i] = length(tempfilterPreheatBC_lt3$Layer_ID)}
  else {AllData$PreHeat_BC_lt3_Count_Last[i] = 0}
}

## 50.) Count how many times in the last 2 layers the Beam Current drops below 3 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatBC <- filter(tempPreheatBC_lt3, Layer_ID %in% Last2Layers)
  AllData$PreHeat_BC_lt3_Count_Last2[i] <- length(tempfilterPreheatBC$Layer_ID)
}

## 51.) Count how many times in the last 5 layers the Beam Current drops below 3 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPreheatBC <- filter(tempPreheatBC_lt3, Layer_ID %in% Last5Layers)
  AllData$PreHeat_BC_lt3_Count_Last5[i] <- length(tempfilterPreheatBC$Layer_ID)
}


#### Covariates: Beam Current Value during Preheat Binary####
AllData$PreHeat_BC_lt3_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt3_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PreHeat_BC_lt3_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 52.) Binary in the last 1 layer does the Beam Current drops below 3 during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPreheatBC_lt3$Layer_ID) {AllData$PreHeat_BC_lt3_Binary_Last[i] = 1}
  else {AllData$PreHeat_BC_lt3_Binary_Last[i] = 0}
}

AllData$PreHeat_BC_lt3_Binary_Last <- as.factor(AllData$PreHeat_BC_lt3_Binary_Last)

## 53.) Binary in the last 2 layer does the Beam Current drops below 3 during a Preheat Process? ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPreheatBC <- filter(tempPreheatBC_lt3, Layer_ID %in% Last2Layers)
  if(length(tempfilterPreheatBC$Layer_ID)==0) {AllData$PreHeat_BC_lt3_Binary_Last2[i] <- 0}
  else{AllData$PreHeat_BC_lt3_Binary_Last2[i] <- 1}
}

AllData$PreHeat_BC_lt3_Binary_Last2 <- as.factor(AllData$PreHeat_BC_lt3_Binary_Last2)

## 54.) Binary in the last 5 layer does the Beam Current drops below 3 during a Preheat Process? ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC_lt3 <- filter(BeamCurrentAll, Value < 3 & Process == "[1]: Preheat")
tempPreheatBC_lt3$Layer_ID <- paste(tempPreheatBC_lt3$Build, "/", tempPreheatBC_lt3$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPreheatBC <- filter(tempPreheatBC_lt3, Layer_ID %in% Last5Layers)
  if(length(tempfilterPreheatBC$Layer_ID)==0) {AllData$PreHeat_BC_lt3_Binary_Last5[i] <- 0}
  else{AllData$PreHeat_BC_lt3_Binary_Last5[i] <- 1}
}

AllData$PreHeat_BC_lt3_Binary_Last5 <- as.factor(AllData$PreHeat_BC_lt3_Binary_Last5)


#### Covariate: Beam Current Process Repeats/Resets Count ####
AllData$BC_ProcessRepeats_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_ProcessRepeats_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_ProcessRepeats_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 55.) Count how many times in the last 1 layer the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()
  
  for(j in 1:length(filteredBeamProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredBeamProcess$Process[j]
      if(filteredBeamProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredBeamProcess$Layer_ID)) {
      AllData$BC_ProcessRepeats_Count_Last[i] = ProcessRepeats
    }
    else if(filteredBeamProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredBeamProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredBeamProcess$Process[j+1]}
      else {
        CurrentProcess = filteredBeamProcess$Process[j+1]}
    }
  }
}

## 56.) Count how many times in the last 2 layers the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredBeamProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredBeamProcessLayer <- filter(filteredBeamProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredBeamProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredBeamProcessLayer$Process[l]
        if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredBeamProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredBeamProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$BC_ProcessRepeats_Count_Last2[i] <- RepeatsInLayers
}

## 57.) Count how many times in the last 5 layers the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredBeamProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredBeamProcessLayer <- filter(filteredBeamProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredBeamProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredBeamProcessLayer$Process[l]
        if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredBeamProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredBeamProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$BC_ProcessRepeats_Count_Last5[i] <- RepeatsInLayers
}


#### Covariate: Beam Current Process Repeats/Resets Binary ####
AllData$BC_ProcessRepeats_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_ProcessRepeats_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_ProcessRepeats_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 58.) Binary if in the last 1 layer the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()
  
  for(j in 1:length(filteredBeamProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredBeamProcess$Process[j]
      if(filteredBeamProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredBeamProcess$Layer_ID)) {
      if(ProcessRepeats == 0) {AllData$BC_ProcessRepeats_Binary_Last[i] = 0}
      else {AllData$BC_ProcessRepeats_Binary_Last[i] = 1}
    }
    else if(filteredBeamProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredBeamProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredBeamProcess$Process[j+1]}
      else {
        CurrentProcess = filteredBeamProcess$Process[j+1]}
    }
  }
}

AllData$BC_ProcessRepeats_Binary_Last <- as.factor(AllData$BC_ProcessRepeats_Binary_Last)

## 59.) Binary if in the last 2 layers the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredBeamProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredBeamProcessLayer <- filter(filteredBeamProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredBeamProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredBeamProcessLayer$Process[l]
        if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredBeamProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredBeamProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$BC_ProcessRepeats_Binary_Last2[i] = 0}
  else{AllData$BC_ProcessRepeats_Binary_Last2[i] = 1}
}

AllData$BC_ProcessRepeats_Binary_Last2 <- as.factor(AllData$BC_ProcessRepeats_Binary_Last2)

## 60.) Binary if in the last 5 layers the Beam Current processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredBeamProcess <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredBeamProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredBeamProcessLayer <- filter(filteredBeamProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredBeamProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredBeamProcessLayer$Process[l]
        if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredBeamProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredBeamProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredBeamProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredBeamProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$BC_ProcessRepeats_Binary_Last5[i] = 0}
  else{AllData$BC_ProcessRepeats_Binary_Last5[i] = 1}
}

AllData$BC_ProcessRepeats_Binary_Last5 <- as.factor(AllData$BC_ProcessRepeats_Binary_Last5)


#### Covariates: Beam Current Abnormal Pauses in the middle of a layer Count ####
AllData$BC_AbnormalPause_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_AbnormalPause_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_AbnormalPause_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 61.) Count how many abnormal pauses occurred in the last 1 layer of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$BC_AbnormalPause_Count_Last[i] <- length(filteredAbnormalPauses$Layer_ID)
}

## 62.) Count how many abnormal pauses occurred in the last 2 layers of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID %in% Last2Layers)
  AllData$BC_AbnormalPause_Count_Last2[i] <- length(filteredAbnormalPauses$Layer_ID)
}

## 63.) Count how many abnormal pauses occurred in the last 5 layers of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID %in% Last5Layers)
  AllData$BC_AbnormalPause_Count_Last5[i] <- length(filteredAbnormalPauses$Layer_ID)
}


#### Covariates: Beam Current Abnormal Pauses in the middle of a layer Binary ####
AllData$BC_AbnormalPause_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_AbnormalPause_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_AbnormalPause_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 64.) Binary if abnormal pauses occurred in the last 1 layer of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if(length(filteredAbnormalPauses$Layer_ID) == 0) {AllData$BC_AbnormalPause_Binary_Last[i] = 0}
  else{AllData$BC_AbnormalPause_Binary_Last[i] = 1}
}

AllData$BC_AbnormalPause_Binary_Last <- as.factor(AllData$BC_AbnormalPause_Binary_Last)

## 65.) Binary if abnormal pauses occurred in the last 2 layer of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID %in% Last2Layers)
  if(length(filteredAbnormalPauses$Layer_ID) == 0) {AllData$BC_AbnormalPause_Binary_Last2[i] = 0}
  else{AllData$BC_AbnormalPause_Binary_Last2[i] = 1}
}

AllData$BC_AbnormalPause_Binary_Last2 <- as.factor(AllData$BC_AbnormalPause_Binary_Last2)

## 66.) Binary if abnormal pauses occurred in the last 5 layer of Beam Current ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredAbnormalPauses <- filter(AbnormalPauseLengthsAll, Layer_ID %in% Last5Layers)
  if(length(filteredAbnormalPauses$Layer_ID) == 0) {AllData$BC_AbnormalPause_Binary_Last5[i] = 0}
  else{AllData$BC_AbnormalPause_Binary_Last5[i] = 1}
}

AllData$BC_AbnormalPause_Binary_Last5 <- as.factor(AllData$BC_AbnormalPause_Binary_Last5)


#### Covariates: Beam Current Pause Time Length > 15.55 seconds ####
AllData$BC_PauseTime_gt15.55_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt15.55_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt15.55_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 67.) Count how many times Beam Current Pause Time Lengths > 15.55 seconds in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBCPause_gt15.55$Layer_ID) {
    tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_PauseTime_gt15.55_Count_Last[i] = length(tempfilter_BCPause$Layer_ID)}
  else {AllData$BC_PauseTime_gt15.55_Count_Last[i] = 0}
}

## 68.) Count how many times Beam Current Pause Time Lengths > 15.55 seconds in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID %in% Last2Layers)
  AllData$BC_PauseTime_gt15.55_Count_Last2[i] <- length(tempfilter_BCPause$Layer_ID)
}

## 69.) Count how many times Beam Current Pause Time Lengths > 15.55 seconds in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID %in% Last5Layers)
  AllData$BC_PauseTime_gt15.55_Count_Last5[i] <- length(tempfilter_BCPause$Layer_ID)
}


#### Covariates: Beam Current Pause Time Length > 15.55 seconds ####
AllData$BC_PauseTime_gt15.55_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt15.55_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt15.55_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 70.) Binary if Beam Current Pause Time Lengths > 15.55 seconds in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBCPause_gt15.55$Layer_ID) {
    tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt15.55_Binary_Last[i] = 0}
    else {AllData$BC_PauseTime_gt15.55_Binary_Last[i] = 1}
    }
  else {AllData$BC_PauseTime_gt15.55_Binary_Last[i] = 0}
}

AllData$BC_PauseTime_gt15.55_Binary_Last <- as.factor(AllData$BC_PauseTime_gt15.55_Binary_Last)

## 71.) Binary if aBeam Current Pause Time Lengths > 15.55 seconds in the last 2 layerS ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID %in% Last2Layers)
  if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt15.55_Binary_Last2[i] = 0}
  else {AllData$BC_PauseTime_gt15.55_Binary_Last2[i] = 1}
}

AllData$BC_PauseTime_gt15.55_Binary_Last2 <- as.factor(AllData$BC_PauseTime_gt15.55_Binary_Last2)

## 72.) Binary if aBeam Current Pause Time Lengths > 15.55 seconds in the last 5 layerS ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt15.55 <- filter(BeamPauseTimeLengthNew, TimeLength >= 15.55)
tempBCPause_gt15.55$Layer_ID <- paste(tempBCPause_gt15.55$Build, "/", tempBCPause_gt15.55$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt15.55, Layer_ID %in% Last5Layers)
  if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt15.55_Binary_Last5[i] = 0}
  else {AllData$BC_PauseTime_gt15.55_Binary_Last5[i] = 1}
}

AllData$BC_PauseTime_gt15.55_Binary_Last5 <- as.factor(AllData$BC_PauseTime_gt15.55_Binary_Last5)


#### Covariates: Beam Current Pause Time Length > 17.22734 seconds ####
AllData$BC_PauseTime_gt17.22734_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt17.22734_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt17.22734_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 73.) Count how many times Beam Current Pause Time Lengths > 17.22734 seconds in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBCPause_gt17.22734$Layer_ID) {
    tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_PauseTime_gt17.22734_Count_Last[i] = length(tempfilter_BCPause$Layer_ID)}
  else {AllData$BC_PauseTime_gt17.22734_Count_Last[i] = 0}
}
## 74.) Count how many times Beam Current Pause Time Lengths > 17.22734 seconds in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID %in% Last2Layers)
  AllData$BC_PauseTime_gt17.22734_Count_Last2[i] <- length(tempfilter_BCPause$Layer_ID)
}
## 75.) Count how many times Beam Current Pause Time Lengths > 17.22734 seconds in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID %in% Last5Layers)
  AllData$BC_PauseTime_gt17.22734_Count_Last5[i] <- length(tempfilter_BCPause$Layer_ID)
}

#### Covariates: Beam Current Pause Time Length > 17.22734 seconds ####
AllData$BC_PauseTime_gt17.22734_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt17.22734_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BC_PauseTime_gt17.22734_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 76.) Binary if Beam Current Pause Time Lengths > 17.22734 seconds in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBCPause_gt17.22734$Layer_ID) {
    tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt17.22734_Binary_Last[i] = 0}
    else {AllData$BC_PauseTime_gt17.22734_Binary_Last[i] = 1}
  }
  else {AllData$BC_PauseTime_gt17.22734_Binary_Last[i] = 0}
}

AllData$BC_PauseTime_gt17.22734_Binary_Last <- as.factor(AllData$BC_PauseTime_gt17.22734_Binary_Last)

## 77.) Binary if Beam Current Pause Time Lengths > 17.22734 seconds in the last 2 layerS  ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID %in% Last2Layers)
  if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt17.22734_Binary_Last2[i] = 0}
  else {AllData$BC_PauseTime_gt17.22734_Binary_Last2[i] = 1}
}

AllData$BC_PauseTime_gt17.22734_Binary_Last2 <- as.factor(AllData$BC_PauseTime_gt17.22734_Binary_Last2)

## 78.) Binary if Beam Current Pause Time Lengths > 17.22734 seconds in the last 5 layerS  ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBCPause_gt17.22734 <- filter(BeamPauseTimeLengthNew, TimeLength >= 17.22734)
tempBCPause_gt17.22734$Layer_ID <- paste(tempBCPause_gt17.22734$Build, "/", tempBCPause_gt17.22734$EndingLayer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_BCPause <- filter(tempBCPause_gt17.22734, Layer_ID %in% Last5Layers)
  if(length(tempfilter_BCPause$Layer_ID) == 0) {AllData$BC_PauseTime_gt17.22734_Binary_Last5[i] = 0}
  else {AllData$BC_PauseTime_gt17.22734_Binary_Last5[i] = 1}
}

AllData$BC_PauseTime_gt17.22734_Binary_Last5 <- as.factor(AllData$BC_PauseTime_gt17.22734_Binary_Last5)


#### Covariates: Beam Current Mean Value during Preheat ####
AllData$Preheat_BC_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_BC_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_BC_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempPreheatBC <- filter(BeamCurrentAll, Process == "[1]: Preheat")
tempAggPreheatBC <- aggregate(tempPreheatBC$Value, by = list(tempPreheatBC$Layer_ID), FUN=mean)
colnames(tempAggPreheatBC) <- list("Layer_ID", "Mean")

MissingLayers <- NULL
for(i in 1:length(AllData$Layer_ID)) {
  if(AllData$Layer_ID[i]  %in% tempAggPreheatBC$Layer_ID == FALSE) {
    if(length(MissingLayers$Layer_ID) == 0) {
      MissingLayers <- cbind.data.frame(AllData$Layer_ID[i],0)
      colnames(MissingLayers) <- list("Layer_ID", "Mean")}
    else{
      MissingLayers <- rbind(MissingLayers, c(AllData$Layer_ID[i], 0)) 
    }
  }
}

tempAggPreheatBC <- rbind(tempAggPreheatBC, MissingLayers)

#Separate Build and Layer to sort
tempAggPreheatBC <- separate(tempAggPreheatBC, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempAggPreheatBC$Layer <- as.numeric(tempAggPreheatBC$Layer)
tempAggPreheatBC <- tempAggPreheatBC[order(tempAggPreheatBC$Build, tempAggPreheatBC$Layer),]

## 79.) Mean Beam Current in the last 1 layer during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  AllData$Preheat_BC_Mean_Last[i] <- tempAggPreheatBC$Mean[match(AllData$Layer_ID[i], tempAggPreheatBC$Layer_ID)-1]
}

## 80.) Mean Beam Current in the last 2 layers during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempAggPreheatBC_Last2 <- filter(BeamCurrentAll, Process == "[1]: Preheat" & Layer_ID %in% Last2Layers)
  AllData$Preheat_BC_Mean_Last2[i] <- mean(tempAggPreheatBC_Last2$Value)
}

## 81.) Mean Beam Current in the last 5 layers during a Preheat Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempAggPreheatBC_Last5 <- filter(BeamCurrentAll, Process == "[1]: Preheat" & Layer_ID %in% Last5Layers)
  AllData$Preheat_BC_Mean_Last5[i] <- mean(tempAggPreheatBC_Last5$Value)
}


#### Covariates: Beam Current Mean Value during Melt ####
AllData$Melt_BC_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_BC_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_BC_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempMeltBC <- filter(BeamCurrentAll, Process == "[3]: Melt")
tempAggMeltBC <- aggregate(tempMeltBC$Value, by = list(tempMeltBC$Layer_ID), FUN=mean)
colnames(tempAggMeltBC) <- list("Layer_ID", "Mean")

MissingLayers <- NULL
for(i in 1:length(AllData$Layer_ID)) {
  if(AllData$Layer_ID[i]  %in% tempAggMeltBC$Layer_ID == FALSE) {
    if(length(MissingLayers$Layer_ID) == 0) {
      MissingLayers <- cbind.data.frame(AllData$Layer_ID[i],0)
      colnames(MissingLayers) <- list("Layer_ID", "Mean")}
    else{
      MissingLayers <- rbind(MissingLayers, c(AllData$Layer_ID[i], 0)) 
    }
  }
}

tempAggMeltBC <- rbind(tempAggMeltBC, MissingLayers)

#Separate Build and Layer to sort
tempAggMeltBC <- separate(tempAggMeltBC, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempAggMeltBC$Layer <- as.numeric(tempAggMeltBC$Layer)
tempAggMeltBC <- tempAggMeltBC[order(tempAggMeltBC$Build, tempAggMeltBC$Layer),]

## 82.) Mean Beam Current in the last 1 layer during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  AllData$Melt_BC_Mean_Last[i] <- tempAggMeltBC$Mean[match(AllData$Layer_ID[i], tempAggMeltBC$Layer_ID)-1]
}

## 83.) Mean Beam Current in the last 2 layers during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempAggMeltBC_Last2 <- filter(BeamCurrentAll, Process == "[3]: Melt" & Layer_ID %in% Last2Layers)
  AllData$Melt_BC_Mean_Last2[i] <- mean(tempAggMeltBC_Last2$Value)
}

## 84.) Mean Beam Current in the last 5 layers during a Melt Process ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempAggMeltBC_Last5 <- filter(BeamCurrentAll, Process == "[3]: Melt" & Layer_ID %in% Last5Layers)
  AllData$Melt_BC_Mean_Last5[i] <- mean(tempAggMeltBC_Last5$Value)
}


#### Covariates: Beam Current Variance Value during Preheat ####
AllData$Preheat_BC_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_BC_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_BC_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 85.) Variance Beam Current in the last 1 layer during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_BC <- filter(BeamCurrentAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_BC$Value) <= 1) {AllData$Preheat_BC_Var_Last[i] <- 0}
  else{AllData$Preheat_BC_Var_Last[i] <- var(filter_BC$Value)}
}

## 86.) Variance Beam Current in the last 2 layer during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(BeamCurrentAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(filter_BC$Value) <= 1) {AllData$Preheat_BC_Var_Last2[i] <- 0}
  else{AllData$Preheat_BC_Var_Last2[i] <- var(filter_BC$Value)}
}

## 87.) Variance Beam Current in the last 5 layer during a Preheat Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(BeamCurrentAll, Process == "[1]: Preheat" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(filter_BC$Value) <= 1) {AllData$Preheat_BC_Var_Last5[i] <- 0}
  else{AllData$Preheat_BC_Var_Last5[i] <- var(filter_BC$Value)}
}


#### Covariates: Beam Current Variance Value during Melt ####
AllData$Melt_BC_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_BC_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_BC_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 88.) Variance Beam Current in the last 1 layer during a Melt Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_BC <- filter(BeamCurrentAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_BC$Value) <= 1) {AllData$Melt_BC_Var_Last[i] <- 0}
  else {AllData$Melt_BC_Var_Last[i] <- var(filter_BC$Value)}
}

## 89.) Variance Beam Current in the last 2 layer during a Melt Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(BeamCurrentAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(Last2Layers$Value) <= 1) {AllData$Melt_BC_Var_Last2[i] <- 0}
  else{AllData$Melt_BC_Var_Last2[i] <- var(Last2Layers$Value)}
}

## 90.) Variance Beam Current in the last 5 layer during a Melt Process ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(BeamCurrentAll, Process == "[3]: Melt" & (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(Last5Layers$Value) <= 1) {AllData$Melt_BC_Var_Last5[i] <- 0}
  else{AllData$Melt_BC_Var_Last5[i] <- var(Last5Layers$Value)}
}


######################################################################################################################################
###################################
######## High Voltage Grid ########
###################################

#### Covariates: High Voltage Grid Value > 1000 for the Preheat Process ####
AllData$Preheat_HVGrid_gt1000_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_gt1000_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_gt1000_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 91.) Count how many times High Voltage Grid > 1000 in the last 1 layer during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_gt1000$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_gt1000, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$Preheat_HVGrid_gt1000_Count_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$Preheat_HVGrid_gt1000_Count_Last[i] = 0}
}

## 92.) Count how many times High Voltage Grid > 1000 in the last 2 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_gt1000, Layer_ID %in% Last2Layers)
  AllData$Preheat_HVGrid_gt1000_Count_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 93.) Count how many times High Voltage Grid > 1000 in the last 5 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_gt1000, Layer_ID %in% Last5Layers)
  AllData$Preheat_HVGrid_gt1000_Count_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


#### Covariates: High Voltage Grid Value > 1000 for the Preheat Process ####
AllData$Preheat_HVGrid_gt1000_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_gt1000_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_gt1000_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 94.) Binary if the High Voltage Grid > 1000 in the last 1 layer during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_gt1000$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_gt1000, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_gt1000_Binary_Last[i] = 0}
    else{AllData$Preheat_HVGrid_gt1000_Binary_Last[i] = 1}
    }
  else {AllData$Preheat_HVGrid_gt1000_Binary_Last[i] = 0}
}

AllData$Preheat_HVGrid_gt1000_Binary_Last <- as.factor(AllData$Preheat_HVGrid_gt1000_Binary_Last)

## 95.) Binary if the High Voltage Grid > 1000 in the last 2 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_gt1000, Layer_ID %in% Last2Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_gt1000_Binary_Last2[i] = 0}
  else{AllData$Preheat_HVGrid_gt1000_Binary_Last2[i] = 1}
}

AllData$Preheat_HVGrid_gt1000_Binary_Last2 <- as.factor(AllData$Preheat_HVGrid_gt1000_Binary_Last2)

## 96.) Binary if the High Voltage Grid > 1000 in the last 5 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_gt1000 <- filter(HighVoltageGridAll, Value >= 1000 & Process == "[1]: Preheat")
tempHVG_gt1000$Layer_ID <- paste(tempHVG_gt1000$Build, "/", tempHVG_gt1000$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_gt1000, Layer_ID %in% Last5Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_gt1000_Binary_Last5[i] = 0}
  else{AllData$Preheat_HVGrid_gt1000_Binary_Last5[i] = 1}
}

AllData$Preheat_HVGrid_gt1000_Binary_Last5 <- as.factor(AllData$Preheat_HVGrid_gt1000_Binary_Last5)


#### Covariates: High Voltage Grid Value < 450 for the Preheat Process ####
AllData$Preheat_HVGrid_lt450_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_lt450_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_lt450_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 97.) Count how many times High Voltage Grid < 450 in the last 1 layer during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_lt450$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$Preheat_HVGrid_lt450_Count_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$Preheat_HVGrid_lt450_Count_Last[i] = 0}
}

## 98.) Count how many times High Voltage Grid < 450 in the last 2 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_lt450, Layer_ID %in% Last2Layers)
  AllData$Preheat_HVGrid_lt450_Count_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 99.) Count how many times High Voltage Grid < 450 in the last 5 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_lt450, Layer_ID %in% Last5Layers)
  AllData$Preheat_HVGrid_lt450_Count_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


#### Covariates: High Voltage Grid Value < 450 for the Preheat Process ####
AllData$Preheat_HVGrid_lt450_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_lt450_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Preheat_HVGrid_lt450_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 100.) Binary if the High Voltage Grid < 450 in the last 1 layer during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_lt450$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_lt450_Binary_Last[i] = 0}
    else{AllData$Preheat_HVGrid_lt450_Binary_Last[i] = 1}
  }
  else {AllData$Preheat_HVGrid_lt450_Binary_Last[i] = 0}
}

AllData$Preheat_HVGrid_lt450_Binary_Last <- as.factor(AllData$Preheat_HVGrid_lt450_Binary_Last)

## 101.) Binary if the High Voltage Grid < 450 in the last 2 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID %in% Last2Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_lt450_Binary_Last2[i] = 0}
  else{AllData$Preheat_HVGrid_lt450_Binary_Last2[i] = 1}
}

AllData$Preheat_HVGrid_lt450_Binary_Last2 <- as.factor(AllData$Preheat_HVGrid_lt450_Binary_Last2)

## 102.) Binary if the High Voltage Grid < 450 in the last 5 layers during the Preheat Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[1]: Preheat")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID %in% Last5Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Preheat_HVGrid_lt450_Binary_Last5[i] = 0}
  else{AllData$Preheat_HVGrid_lt450_Binary_Last5[i] = 1}
}

AllData$Preheat_HVGrid_lt450_Binary_Last5 <- as.factor(AllData$Preheat_HVGrid_lt450_Binary_Last5)


#### Covariates: High Voltage Grid Value < 450 for the Melt Process ####
AllData$Melt_HVGrid_lt450_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_HVGrid_lt450_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_HVGrid_lt450_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 103.) Count how many times High Voltage Grid < 450 in the last 1 layer during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_lt450$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$Melt_HVGrid_lt450_Count_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$Melt_HVGrid_lt450_Count_Last[i] = 0}
}

## 104.) Count how many times High Voltage Grid < 450 in the last 2 layers during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_lt450, Layer_ID %in% Last2Layers)
  AllData$Melt_HVGrid_lt450_Count_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 105.) Count how many times High Voltage Grid < 450 in the last 5 layers during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_lt450, Layer_ID %in% Last5Layers)
  AllData$Melt_HVGrid_lt450_Count_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


#### Covariates: High Voltage Grid Value < 450 for the Melt Process ####
AllData$Melt_HVGrid_lt450_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$Melt_HVGrid_lt450_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$Melt_HVGrid_lt450_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 106.) Binary if the High Voltage Grid < 450 in the last 1 layer during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_lt450$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Melt_HVGrid_lt450_Binary_Last[i] = 0}
    else{AllData$Melt_HVGrid_lt450_Binary_Last[i] = 1}
  }
  else {AllData$Melt_HVGrid_lt450_Binary_Last[i] = 0}
}

AllData$Melt_HVGrid_lt450_Binary_Last <- as.factor(AllData$Melt_HVGrid_lt450_Binary_Last)

## 107.) Binary if the High Voltage Grid < 450 in the last 2 layers during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID %in% Last2Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Melt_HVGrid_lt450_Binary_Last2[i] = 0}
  else{AllData$Melt_HVGrid_lt450_Binary_Last2[i] = 1}
}

AllData$Melt_HVGrid_lt450_Binary_Last2 <- as.factor(AllData$Melt_HVGrid_lt450_Binary_Last2)

## 108.) Binary if the High Voltage Grid < 450 in the last 5 layers during the Melt Process ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_lt450 <- filter(HighVoltageGridAll, Value <= 450 & Process == "[3]: Melt")
tempHVG_lt450$Layer_ID <- paste(tempHVG_lt450$Build, "/", tempHVG_lt450$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilter_HVG <- filter(tempHVG_lt450, Layer_ID %in% Last5Layers)
  if(length(tempfilter_HVG$Layer_ID) == 0) {AllData$Melt_HVGrid_lt450_Binary_Last5[i] = 0}
  else{AllData$Melt_HVGrid_lt450_Binary_Last5[i] = 1}
}

AllData$Melt_HVGrid_lt450_Binary_Last5 <- as.factor(AllData$Melt_HVGrid_lt450_Binary_Last5)


#### Covariates: High Voltage Grid Process Repeats/Resets Count ####
AllData$HVG_ProcessRepeats_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVG_ProcessRepeats_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVG_ProcessRepeats_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 109.) Count how many times in the last 1 layer the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()
  
  for(j in 1:length(filteredHVGridProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredHVGridProcess$Process[j]
      if(filteredHVGridProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredHVGridProcess$Layer_ID)) {
      AllData$HVG_ProcessRepeats_Count_Last[i] = ProcessRepeats
    }
    else if(filteredHVGridProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredHVGridProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredHVGridProcess$Process[j+1]}
      else {
        CurrentProcess = filteredHVGridProcess$Process[j+1]}
    }
  }
}

## 110.) Count how many times in the last 2 layers the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredHVGridProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredHVGridProcessLayer <- filter(filteredHVGridProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredHVGridProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredHVGridProcessLayer$Process[l]
        if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredHVGridProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredHVGridProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$HVG_ProcessRepeats_Count_Last2[i] <- RepeatsInLayers
}

## 111.) Count how many times in the last 5 layers the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredHVGridProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredHVGridProcessLayer <- filter(filteredHVGridProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredHVGridProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredHVGridProcessLayer$Process[l]
        if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredHVGridProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredHVGridProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
      }
    }
  }
  AllData$HVG_ProcessRepeats_Count_Last5[i] <- RepeatsInLayers
}


#### Covariates: High Voltage Grid Process Repeats/Resets Binary ####
AllData$HVG_ProcessRepeats_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVG_ProcessRepeats_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVG_ProcessRepeats_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 112.) Binary if in the last 1 layer the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####

for(i in 1:length(AllData$Layer_ID)) {
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  ProcessRepeats = 0
  CompletedProcesses = list()
  
  for(j in 1:length(filteredHVGridProcess$Layer_ID)) {
    if(j==1) {
      CurrentProcess = filteredHVGridProcess$Process[j]
      if(filteredHVGridProcess$Process[j+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
    }
    else if(j == length(filteredHVGridProcess$Layer_ID)) {
      if(ProcessRepeats == 0) {AllData$HVG_ProcessRepeats_Binary_Last[i] = 0}
      else {AllData$HVG_ProcessRepeats_Binary_Last[i] = 1}
    }
    else if(filteredHVGridProcess$Process[j+1] != CurrentProcess) {
      CompletedProcesses = append(CompletedProcesses,CurrentProcess)
      if(filteredHVGridProcess$Process[j+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
        ProcessRepeats = ProcessRepeats + 1
        CurrentProcess = filteredHVGridProcess$Process[j+1]}
      else {
        CurrentProcess = filteredHVGridProcess$Process[j+1]}
    }
  }
}

AllData$HVG_ProcessRepeats_Binary_Last <- as.factor(AllData$HVG_ProcessRepeats_Binary_Last)
## 113.) Binary if in the last 2 layers the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last2Layers<- list()
  
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers)
  filteredLayers <- unique(filteredHVGridProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredHVGridProcessLayer <- filter(filteredHVGridProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredHVGridProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredHVGridProcessLayer$Process[l]
        if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredHVGridProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredHVGridProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$HVG_ProcessRepeats_Binary_Last2[i] = 0}
  else{AllData$HVG_ProcessRepeats_Binary_Last2[i] = 1}
}

AllData$HVG_ProcessRepeats_Binary_Last2 <- as.factor(AllData$HVG_ProcessRepeats_Binary_Last2)

## 114.) Binary if in the last 5 layers the High Voltage Grid processes repeat (Preheat --> Melt --> Preheat) ####
for(i in 1:length(AllData$Layer_ID)) {
  RepeatsInLayers = 0
  Last5Layers<- list()
  
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  filteredHVGridProcess <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers)
  filteredLayers <- unique(filteredHVGridProcess$Layer_ID)
  
  for(k in 1:length(filteredLayers)) {
    filteredHVGridProcessLayer <- filter(filteredHVGridProcess, Layer_ID == filteredLayers[k])
    ProcessRepeats = 0
    CompletedProcesses = list()
    
    for(l in 1:length(filteredHVGridProcessLayer$Layer_ID)) {
      if(l==1) {
        CurrentProcess = filteredHVGridProcessLayer$Process[l]
        if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {CompletedProcesses = CurrentProcess}
      }
      else if(l == length(filteredHVGridProcessLayer$Layer_ID)) {
        RepeatsInLayers = RepeatsInLayers + ProcessRepeats
      }
      else if(filteredHVGridProcessLayer$Process[l+1] != CurrentProcess) {
        CompletedProcesses = append(CompletedProcesses,CurrentProcess)
        if(filteredHVGridProcessLayer$Process[l+1] == "[1]: Preheat" & "[1]: Preheat" %in% CompletedProcesses) {
          ProcessRepeats = ProcessRepeats + 1
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
        else {
          CurrentProcess = filteredHVGridProcessLayer$Process[l+1]}
      }
    }
  }
  if(RepeatsInLayers == 0) {AllData$HVG_ProcessRepeats_Binary_Last5[i] = 0}
  else{AllData$HVG_ProcessRepeats_Binary_Last5[i] = 1}
}

AllData$HVG_ProcessRepeats_Binary_Last5 <- as.factor(AllData$HVG_ProcessRepeats_Binary_Last5)


######################################################################################################################################

#################################
######## Last Layer Time ########
#################################

#### Covariate: Last Layer Time Value > 78.8 seconds Binary ####
AllData$LLT_gt78.8_Binary_Last <- rep(0, length(AllData$Layer_ID))
AllData$LLT_gt78.8_Binary_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$LLT_gt78.8_Binary_Last5 <- rep(0, length(AllData$Layer_ID))

## 115.) Binary if the Last Layer Time Value > 78.8 in the last 1 layer ####
LastLayerTimeAll$Value <- as.numeric(LastLayerTimeAll$Value)
tempLLT_gt78.8 <- filter(LastLayerTimeAll, Value >= 78.8)
tempLLT_gt78.8$Layer_ID <- paste(tempLLT_gt78.8$Build, "/", tempLLT_gt78.8$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempLLT_gt78.8$Layer_ID) {AllData$LLT_gt78.8_Binary_Last[i] = 1}
  else {AllData$LLT_gt78.8_Binary_Last[i] = 0}
}

AllData$LLT_gt78.8_Binary_Last <- as.factor(AllData$LLT_gt78.8_Binary_Last)

## 116.) Binary if the Last Layer Time Value > 78.8 in the last 2 layers ####
LastLayerTimeAll$Value <- as.numeric(LastLayerTimeAll$Value)
tempLLT_gt78.8 <- filter(LastLayerTimeAll, Value >= 78.8)
tempLLT_gt78.8$Layer_ID <- paste(tempLLT_gt78.8$Build, "/", tempLLT_gt78.8$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterLLT <- filter(tempLLT_gt78.8, Layer_ID %in% Last2Layers)
  if(length(tempfilterLLT$Layer_ID)==0) {AllData$LLT_gt78.8_Binary_Last2[i] <- 0}
  else{AllData$LLT_gt78.8_Binary_Last2[i] <- 1}
}

AllData$LLT_gt78.8_Binary_Last2 <- as.factor(AllData$LLT_gt78.8_Binary_Last2)

## 117.) Binary if the Last Layer Time Value > 78.8 in the last 5 layers ####
LastLayerTimeAll$Value <- as.numeric(LastLayerTimeAll$Value)
tempLLT_gt78.8 <- filter(LastLayerTimeAll, Value >= 78.8)
tempLLT_gt78.8$Layer_ID <- paste(tempLLT_gt78.8$Build, "/", tempLLT_gt78.8$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterLLT <- filter(tempLLT_gt78.8, Layer_ID %in% Last5Layers)
  if(length(tempfilterLLT$Layer_ID)==0) {AllData$LLT_gt78.8_Binary_Last5[i] <- 0}
  else{AllData$LLT_gt78.8_Binary_Last5[i] <- 1}
}

AllData$LLT_gt78.8_Binary_Last5 <- as.factor(AllData$LLT_gt78.8_Binary_Last5)


######################################################################################################################################
###############################################
######## Backing Vacuum Gauge Feedback ########
###############################################

#### Backing Vacuum Gauge Feedback Variance ####
AllData$BVG_FB_Var_Last <- rep(0, length(AllData$Layer_ID))
AllData$BVG_FB_Var_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_FB_Var_Last5 <- rep(0, length(AllData$Layer_ID))

## 118.) Backing Vacuum Gauge Feedback Variance in the last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_BVG <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_BVG$Value)<=1) {AllData$BVG_FB_Var_Last[i] <- 0}
  else {AllData$BVG_FB_Var_Last[i] <- var(filter_BVG$Value)}
}

## 119.) Backing Vacuum Gauge Feedback Variance in the last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(Last2Layers$Value)<=1) {AllData$BVG_FB_Var_Last2[i]=0}
  else{  AllData$BVG_FB_Var_Last2[i] <- var(Last2Layers$Value)}
}

## 120.) Backing Vacuum Gauge Feedback Variance in the last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(Last5Layers$Value)<=1) {AllData$BVG_FB_Var_Last5[i]=0}
  else{  AllData$BVG_FB_Var_Last5[i] <- var(Last5Layers$Value)}
}



##############################
######## Pulse Length ########
##############################
## Initialize ####
PulseLengthAll$Value <- as.numeric(PulseLengthAll$Value)
tempPulseLengthAll <- NULL

for(i in 1:length(AllData$Layer_ID)) {
  if(i==1){tempPulseLengthAll <- slice(PulseLengthAll,match(AllData$Layer_ID[i], PulseLengthAll$Layer_ID))}
  else if(AllData$Layer_ID[i]  %in% PulseLengthAll$Layer_ID == FALSE) {
    tempPulseLengthAll <- rbind(tempPulseLengthAll, slice(tempPulseLengthAll,length(tempPulseLengthAll$Layer_ID)))
    tempPulseLengthAll$Layer_ID[length(tempPulseLengthAll$Layer_ID)] <- AllData$Layer_ID[i]}
  else{
    tempData <- filter(PulseLengthAll, Layer_ID == AllData$Layer_ID[i])
    tempPulseLengthAll <- rbind(tempPulseLengthAll, tempData)
  }
}

tempPulseLengthAll <- tempPulseLengthAll[c(7,3)]
for(i in 1:length(tempPulseLengthAll$Layer_ID)){
  tempPulseLengthAll$Defect[i] <- AllData$Response[match(tempPulseLengthAll$Layer_ID[i], AllData$Layer_ID)]
}

quantile(tempPulseLengthAll$Value, c(0,.01,.025,.05,.90,.95,.975,.99,1))

#### Covariates: Pulse Length Count ####
AllData$PL_gteq170_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq170_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq170_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 121.) Count how many times in the last 1 layer the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)
for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_gteq170$Layer_ID) {
    tempfilterPL_gteq170 <- filter(tempPL_gteq170, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_gteq170_Count_Last[i] = length(tempfilterPL_gteq170$Layer_ID)}
  else {AllData$PL_gteq170_Count_Last[i] = 0}
}

## 122.) Count how many times in the last 2 layers the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq170, Layer_ID %in% Last2Layers)
  AllData$PL_gteq170_Count_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 123.) Count how many times in the last 5 layers the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq170, Layer_ID %in% Last5Layers)
  AllData$PL_gteq170_Count_Last5[i] <- length(tempfilterPL$Layer_ID)
}


#### Covariates: Pulse Length Binary ####
AllData$PL_gteq170_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq170_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq170_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 124.) Binary if in the last 1 layer the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_gteq170$Layer_ID) {AllData$PL_gteq170_Binary_Last[i] = 1}
  else {AllData$PL_gteq170_Binary_Last[i] = 0}
}

AllData$PL_gteq170_Binary_Last <- as.factor(AllData$PL_gteq170_Binary_Last)

## 125.) Binary if in the last 2 layers the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq170, Layer_ID %in% Last2Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_gteq170_Binary_Last2[i] <- 0}
  else{AllData$PL_gteq170_Binary_Last2[i] <- 1}
}

AllData$PL_gteq170_Binary_Last2 <- as.factor(AllData$PL_gteq170_Binary_Last2)

## 126.) Binary if in the last 5 layers the Pulse Length rises to or above 170 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq170 <- filter(tempPulseLengthAll, Value >= 170)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq170, Layer_ID %in% Last5Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_gteq170_Binary_Last5[i] <- 0}
  else{AllData$PL_gteq170_Binary_Last5[i] <- 1}
}

AllData$PL_gteq170_Binary_Last5 <- as.factor(AllData$PL_gteq170_Binary_Last5)



#### Covariates: Pulse Length Count ####
AllData$PL_gteq160_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq160_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq160_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 127.) Count how many times in the last 1 layer the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_gteq160$Layer_ID) {
    tempfilterPL_gteq160 <- filter(tempPL_gteq160, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_gteq160_Count_Last[i] = length(tempfilterPL_gteq160$Layer_ID)}
  else {AllData$PL_gteq160_Count_Last[i] = 0}
}

## 128.) Count how many times in the last 2 layers the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq160, Layer_ID %in% Last2Layers)
  AllData$PL_gteq160_Count_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 129.) Count how many times in the last 5 layers the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq160, Layer_ID %in% Last5Layers)
  AllData$PL_gteq160_Count_Last5[i] <- length(tempfilterPL$Layer_ID)
}


#### Covariates: Pulse Length Binary ####
AllData$PL_gteq160_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq160_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_gteq160_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 130.) Binary if in the last 1 layer the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_gteq160$Layer_ID) {AllData$PL_gteq160_Binary_Last[i] = 1}
  else {AllData$PL_gteq160_Binary_Last[i] = 0}
}

AllData$PL_gteq160_Binary_Last <- as.factor(AllData$PL_gteq160_Binary_Last)

## 131.) Binary if in the last 2 layers the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq160, Layer_ID %in% Last2Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_gteq160_Binary_Last2[i] <- 0}
  else{AllData$PL_gteq160_Binary_Last2[i] <- 1}
}

AllData$PL_gteq160_Binary_Last2 <- as.factor(AllData$PL_gteq160_Binary_Last2)

## 132.) Binary if in the last 5 layers the Pulse Length rises to or above 160 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gteq160 <- filter(tempPulseLengthAll, Value >= 160)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_gteq160, Layer_ID %in% Last5Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_gteq160_Binary_Last5[i] <- 0}
  else{AllData$PL_gteq160_Binary_Last5[i] <- 1}
}

AllData$PL_gteq160_Binary_Last5 <- as.factor(AllData$PL_gteq160_Binary_Last5)

#### Covariates: Pulse Length Count ####
AllData$PL_lteq100_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq100_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq100_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 133.) Count how many times in the last 1 layer the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_lteq100$Layer_ID) {
    tempfilterPL_lteq100 <- filter(tempPL_lteq100, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_lteq100_Count_Last[i] = length(tempfilterPL_lteq100$Layer_ID)}
  else {AllData$PL_lteq100_Count_Last[i] = 0}
}

## 134.) Count how many times in the last 2 layers the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq100, Layer_ID %in% Last2Layers)
  AllData$PL_lteq100_Count_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 135.) Count how many times in the last 5 layers the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq100, Layer_ID %in% Last5Layers)
  AllData$PL_lteq100_Count_Last5[i] <- length(tempfilterPL$Layer_ID)
}


#### Covariates: Pulse Length Binary ####
AllData$PL_lteq100_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq100_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq100_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 136.) Binary if in the last 1 layer the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_lteq100$Layer_ID) {AllData$PL_lteq100_Binary_Last[i] = 1}
  else {AllData$PL_lteq100_Binary_Last[i] = 0}
}

AllData$PL_lteq100_Binary_Last <- as.factor(AllData$PL_lteq100_Binary_Last)

## 137.) Binary if in the last 2 layers the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq100, Layer_ID %in% Last2Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_lteq100_Binary_Last2[i] <- 0}
  else{AllData$PL_lteq100_Binary_Last2[i] <- 1}
}

AllData$PL_lteq100_Binary_Last2 <- as.factor(AllData$PL_lteq100_Binary_Last2)

## 138.) Binary if in the last 5 layers the Pulse Length rises to or below 100 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq100 <- filter(tempPulseLengthAll, Value <= 100)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq100, Layer_ID %in% Last5Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_lteq100_Binary_Last5[i] <- 0}
  else{AllData$PL_lteq100_Binary_Last5[i] <- 1}
}

AllData$PL_lteq100_Binary_Last5 <- as.factor(AllData$PL_lteq100_Binary_Last5)

#### Covariates: Pulse Length Count ####
AllData$PL_lteq110_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq110_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq110_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 139.) Count how many times in the last 1 layer the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_lteq110$Layer_ID) {
    tempfilterPL_lteq110 <- filter(tempPL_lteq110, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_lteq110_Count_Last[i] = length(tempfilterPL_lteq110$Layer_ID)}
  else {AllData$PL_lteq110_Count_Last[i] = 0}
}

## 140.) Count how many times in the last 2 layers the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq110, Layer_ID %in% Last2Layers)
  AllData$PL_lteq110_Count_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 141.) Count how many times in the last 5 layers the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq110, Layer_ID %in% Last5Layers)
  AllData$PL_lteq110_Count_Last5[i] <- length(tempfilterPL$Layer_ID)
}


#### Covariates: Pulse Length Binary ####
AllData$PL_lteq110_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq110_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$PL_lteq110_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 142.) Binary if in the last 1 layer the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_lteq110$Layer_ID) {AllData$PL_lteq110_Binary_Last[i] = 1}
  else {AllData$PL_lteq110_Binary_Last[i] = 0}
}

AllData$PL_lteq110_Binary_Last <- as.factor(AllData$PL_lteq110_Binary_Last)

## 143.) Binary if in the last 2 layers the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq110, Layer_ID %in% Last2Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_lteq110_Binary_Last2[i] <- 0}
  else{AllData$PL_lteq110_Binary_Last2[i] <- 1}
}

AllData$PL_lteq110_Binary_Last2 <- as.factor(AllData$PL_lteq110_Binary_Last2)

## 144.) Binary if in the last 5 layers the Pulse Length rises to or below 110 ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_lteq110 <- filter(tempPulseLengthAll, Value <= 110)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterPL <- filter(tempPL_lteq110, Layer_ID %in% Last5Layers)
  if(length(tempfilterPL$Layer_ID)==0) {AllData$PL_lteq110_Binary_Last5[i] <- 0}
  else{AllData$PL_lteq110_Binary_Last5[i] <- 1}
}

AllData$PL_lteq110_Binary_Last5 <- as.factor(AllData$PL_lteq110_Binary_Last5)



#######################################
######## High Voltage Feedback ########
#######################################
## Initialize ####
HighVoltageFeedbackAll$Value <- as.numeric(HighVoltageFeedbackAll$Value)
tempHighVoltageFeedbackAll <- NULL

for(i in 1:length(AllData$Layer_ID)) {
  if(i==1){
    if (AllData$Layer_ID[i]  %in% HighVoltageFeedbackAll$Layer_ID) {tempHighVoltageFeedbackAll <- slice(HighVoltageFeedbackAll,match(AllData$Layer_ID[i], HighVoltageFeedbackAll$Layer_ID))}
    else {tempHighVoltageFeedbackAll <- slice(HighVoltageFeedbackAll,1)}}
  else if(AllData$Layer_ID[i]  %in% HighVoltageFeedbackAll$Layer_ID == FALSE) {
    tempHighVoltageFeedbackAll <- rbind(tempHighVoltageFeedbackAll, slice(tempHighVoltageFeedbackAll,length(tempHighVoltageFeedbackAll$Layer_ID)))
    tempHighVoltageFeedbackAll$Layer_ID[length(tempHighVoltageFeedbackAll$Layer_ID)] <- AllData$Layer_ID[i]}
  else{
    tempData <- filter(HighVoltageFeedbackAll, Layer_ID == AllData$Layer_ID[i])
    tempHighVoltageFeedbackAll <- rbind(tempHighVoltageFeedbackAll, tempData)
  }
}

tempHighVoltageFeedbackAll <- tempHighVoltageFeedbackAll[c(7,3)]
tempHighVoltageFeedbackAll$Layer_ID[1] <- AllData$Layer_ID[1]
quantile(tempHighVoltageFeedbackAll$Value, c(0,.01,.025,.05,.1,.25,.90,.95,.975,.99,.995,1))

#### Covariates: High Voltage Feedback Count ####
AllData$HVF_gt60400_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_gt60400_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_gt60400_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 145.) Count how many times in the last 1 layer the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)
for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_gt60400$Layer_ID) {
    tempfilterHVF_gt60400 <- filter(tempHVF_gt60400, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_gt60400_Count_Last[i] = length(tempfilterHVF_gt60400$Layer_ID)}
  else {AllData$HVF_gt60400_Count_Last[i] = 0}
}

## 146.) Count how many times in the last 2 layers the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_gt60400, Layer_ID %in% Last2Layers)
  AllData$HVF_gt60400_Count_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 147.) Count how many times in the last 5 layers the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_gt60400, Layer_ID %in% Last5Layers)
  AllData$HVF_gt60400_Count_Last5[i] <- length(tempfilterHVF$Layer_ID)
}


#### Covariates: High Voltage Feedback Binary ####
AllData$HVF_gt60400_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_gt60400_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_gt60400_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 148.) Binary if in the last 1 layer the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_gt60400$Layer_ID) {AllData$HVF_gt60400_Binary_Last[i] = 1}
  else {AllData$HVF_gt60400_Binary_Last[i] = 0}
}

AllData$HVF_gt60400_Binary_Last <- as.factor(AllData$HVF_gt60400_Binary_Last)

## 149.) Binary if in the last 2 layers the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_gt60400, Layer_ID %in% Last2Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_gt60400_Binary_Last2[i] <- 0}
  else{AllData$HVF_gt60400_Binary_Last2[i] <- 1}
}

AllData$HVF_gt60400_Binary_Last2 <- as.factor(AllData$HVF_gt60400_Binary_Last2)

## 150.) Binary if in the last 5 layers the High Voltage Feedback rises to or above 60400 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gt60400 <- filter(tempHighVoltageFeedbackAll, Value > 60400)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_gt60400, Layer_ID %in% Last5Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_gt60400_Binary_Last5[i] <- 0}
  else{AllData$HVF_gt60400_Binary_Last5[i] <- 1}
}

AllData$HVF_gt60400_Binary_Last5 <- as.factor(AllData$HVF_gt60400_Binary_Last5)




#### Covariates: High Voltage Feedback Count ####
AllData$HVF_lt59600_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt59600_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt59600_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 151.) Count how many times in the last 1 layer the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)
for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt59600$Layer_ID) {
    tempfilterHVF_lt59600 <- filter(tempHVF_lt59600, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_lt59600_Count_Last[i] = length(tempfilterHVF_lt59600$Layer_ID)}
  else {AllData$HVF_lt59600_Count_Last[i] = 0}
}

## 152.) Count how many times in the last 2 layers the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt59600, Layer_ID %in% Last2Layers)
  AllData$HVF_lt59600_Count_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 153.) Count how many times in the last 5 layers the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt59600, Layer_ID %in% Last5Layers)
  AllData$HVF_lt59600_Count_Last5[i] <- length(tempfilterHVF$Layer_ID)
}


#### Covariates: High Voltage Feedback Binary ####
AllData$HVF_lt59600_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt59600_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt59600_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 154.) Binary if in the last 1 layer the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt59600$Layer_ID) {AllData$HVF_lt59600_Binary_Last[i] = 1}
  else {AllData$HVF_lt59600_Binary_Last[i] = 0}
}

AllData$HVF_lt59600_Binary_Last <- as.factor(AllData$HVF_lt59600_Binary_Last)

## 155.) Binary if in the last 2 layers the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt59600, Layer_ID %in% Last2Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt59600_Binary_Last2[i] <- 0}
  else{AllData$HVF_lt59600_Binary_Last2[i] <- 1}
}

AllData$HVF_lt59600_Binary_Last2 <- as.factor(AllData$HVF_lt59600_Binary_Last2)

## 156.) Binary if in the last 5 layers the High Voltage Feedback rises to or above 59600 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt59600 <- filter(tempHighVoltageFeedbackAll, Value < 59600)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt59600, Layer_ID %in% Last5Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt59600_Binary_Last5[i] <- 0}
  else{AllData$HVF_lt59600_Binary_Last5[i] <- 1}
}

AllData$HVF_lt59600_Binary_Last5 <- as.factor(AllData$HVF_lt59600_Binary_Last5)





#### Covariates: High Voltage Feedback Count ####
AllData$HVF_lt35500_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt35500_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt35500_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 157.) Count how many times in the last 1 layer the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)
for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt35500$Layer_ID) {
    tempfilterHVF_lt35500 <- filter(tempHVF_lt35500, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_lt35500_Count_Last[i] = length(tempfilterHVF_lt35500$Layer_ID)}
  else {AllData$HVF_lt35500_Count_Last[i] = 0}
}

## 158.) Count how many times in the last 2 layers the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt35500, Layer_ID %in% Last2Layers)
  AllData$HVF_lt35500_Count_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 159.) Count how many times in the last 5 layers the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt35500, Layer_ID %in% Last5Layers)
  AllData$HVF_lt35500_Count_Last5[i] <- length(tempfilterHVF$Layer_ID)
}


#### Covariates: High Voltage Feedback Binary ####
AllData$HVF_lt35500_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt35500_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt35500_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 160.) Binary if in the last 1 layer the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt35500$Layer_ID) {AllData$HVF_lt35500_Binary_Last[i] = 1}
  else {AllData$HVF_lt35500_Binary_Last[i] = 0}
}

AllData$HVF_lt35500_Binary_Last <- as.factor(AllData$HVF_lt35500_Binary_Last)

## 161.) Binary if in the last 2 layers the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt35500, Layer_ID %in% Last2Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt35500_Binary_Last2[i] <- 0}
  else{AllData$HVF_lt35500_Binary_Last2[i] <- 1}
}

AllData$HVF_lt35500_Binary_Last2 <- as.factor(AllData$HVF_lt35500_Binary_Last2)

## 162.) Binary if in the last 5 layers the High Voltage Feedback rises to or above 35500 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt35500 <- filter(tempHighVoltageFeedbackAll, Value < 35500)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt35500, Layer_ID %in% Last5Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt35500_Binary_Last5[i] <- 0}
  else{AllData$HVF_lt35500_Binary_Last5[i] <- 1}
}

AllData$HVF_lt35500_Binary_Last5 <- as.factor(AllData$HVF_lt35500_Binary_Last5)






#### Covariates: High Voltage Feedback Count ####
AllData$HVF_lt13000_Count_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt13000_Count_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt13000_Count_Last5 <- rep(0,length(AllData$Layer_ID))

## 163.) Count how many times in the last 1 layer the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)
for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt13000$Layer_ID) {
    tempfilterHVF_lt13000 <- filter(tempHVF_lt13000, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_lt13000_Count_Last[i] = length(tempfilterHVF_lt13000$Layer_ID)}
  else {AllData$HVF_lt13000_Count_Last[i] = 0}
}

## 164.) Count how many times in the last 2 layers the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt13000, Layer_ID %in% Last2Layers)
  AllData$HVF_lt13000_Count_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 165.) Count how many times in the last 5 layers the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt13000, Layer_ID %in% Last5Layers)
  AllData$HVF_lt13000_Count_Last5[i] <- length(tempfilterHVF$Layer_ID)
}


#### Covariates: High Voltage Feedback Binary ####
AllData$HVF_lt13000_Binary_Last <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt13000_Binary_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$HVF_lt13000_Binary_Last5 <- rep(0,length(AllData$Layer_ID))

## 166.) Binary if in the last 1 layer the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_lt13000$Layer_ID) {AllData$HVF_lt13000_Binary_Last[i] = 1}
  else {AllData$HVF_lt13000_Binary_Last[i] = 0}
}

AllData$HVF_lt13000_Binary_Last <- as.factor(AllData$HVF_lt13000_Binary_Last)

## 167.) Binary if in the last 2 layers the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt13000, Layer_ID %in% Last2Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt13000_Binary_Last2[i] <- 0}
  else{AllData$HVF_lt13000_Binary_Last2[i] <- 1}
}

AllData$HVF_lt13000_Binary_Last2 <- as.factor(AllData$HVF_lt13000_Binary_Last2)

## 168.) Binary if in the last 5 layers the High Voltage Feedback rises to or above 13000 ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_lt13000 <- filter(tempHighVoltageFeedbackAll, Value < 13000)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilterHVF <- filter(tempHVF_lt13000, Layer_ID %in% Last5Layers)
  if(length(tempfilterHVF$Layer_ID)==0) {AllData$HVF_lt13000_Binary_Last5[i] <- 0}
  else{AllData$HVF_lt13000_Binary_Last5[i] <- 1}
}

AllData$HVF_lt13000_Binary_Last5 <- as.factor(AllData$HVF_lt13000_Binary_Last5)







######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

###################################################
######## Using Build 7/18 has the Standard ######## 
###################################################

##############################
#### Smoke Detector Count ####
##############################

#### Set Max and Min Thresholds for SDC ####
SDC_MaxThreshold <- as.numeric(max(SmokeDetectorCount718$Value))
SDC_MinThreshold <- as.numeric(min(SmokeDetectorCount718$Value))

## Smoke Detector Count > 718 Max ##
AllData$SDC_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 169.) Count how many times the Smoke Detector Count greater than the max of 718 in last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_MaxThreshold)
tempSDC_gtmax718$Layer_ID <- paste(tempSDC_gtmax718$Build, "/", tempSDC_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_gtmax718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_gtmax718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_gtmax718_Last[i] = 0}
}

## 170.) Count how many times the Smoke Detector Count greater than the max of 718 in last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_MaxThreshold)
tempSDC_gtmax718$Layer_ID <- paste(tempSDC_gtmax718$Build, "/", tempSDC_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_gtmax718, Layer_ID %in% Last2Layers)
  AllData$SDC_gtmax718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}



## 171.) Count how many times the Smoke Detector Count greater than the max of 718 in last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_MaxThreshold)
tempSDC_gtmax718$Layer_ID <- paste(tempSDC_gtmax718$Build, "/", tempSDC_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_gtmax718, Layer_ID %in% Last5Layers)
  AllData$SDC_gtmax718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}

## 172.) Count how many times the Smoke Detector Count less than the min of 718 in last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_MinThreshold)
tempSDC_ltmin718$Layer_ID <- paste(tempSDC_ltmin718$Build, "/", tempSDC_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_ltmin718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_ltmin718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_ltmin718_Last[i] = 0}
}

## 173.) Count how many times the Smoke Detector Count less than the min of 718 in last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_MinThreshold)
tempSDC_ltmin718$Layer_ID <- paste(tempSDC_ltmin718$Build, "/", tempSDC_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_ltmin718, Layer_ID %in% Last2Layers)
  AllData$SDC_ltmin718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}

## 174.) Count how many times the Smoke Detector Count less than the min of 718 in last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_MinThreshold)
tempSDC_ltmin718$Layer_ID <- paste(tempSDC_ltmin718$Build, "/", tempSDC_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_ltmin718, Layer_ID %in% Last5Layers)
  AllData$SDC_ltmin718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}


#### Set Max and Min Thresholds for SDC during Preheat ####
SDC_Preheat_MaxThreshold <- as.numeric(max(filter(SmokeDetectorCount718, Process == "[1]: Preheat")$Value))
SDC_Preheat_MinThreshold <- as.numeric(min(filter(SmokeDetectorCount718, Process == "[1]: Preheat")$Value))

## Smoke Detector Count > 718 Max ##
AllData$SDC_Preheat_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 175.) Count how many times the Smoke Detector Count greater than the max of 718 in last 1 layer during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_gtmax718$Layer_ID <- paste(tempSDC_Preheat_gtmax718$Build, "/", tempSDC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_Preheat_gtmax718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_Preheat_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_Preheat_gtmax718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_Preheat_gtmax718_Last[i] = 0}
}

## 176.) Count how many times the Smoke Detector Count greater than the max of 718 in last 2 layers during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_gtmax718$Layer_ID <- paste(tempSDC_Preheat_gtmax718$Build, "/", tempSDC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Preheat_gtmax718, Layer_ID %in% Last2Layers)
  AllData$SDC_Preheat_gtmax718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}

## 177.) Count how many times the Smoke Detector Count greater than the max of 718 in last 5 layers during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_gtmax718$Layer_ID <- paste(tempSDC_Preheat_gtmax718$Build, "/", tempSDC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Preheat_gtmax718, Layer_ID %in% Last5Layers)
  AllData$SDC_Preheat_gtmax718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}


## 178.) Count how many times the Smoke Detector Count less than the min of 718 in last 1 layer during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_ltmin718$Layer_ID <- paste(tempSDC_Preheat_ltmin718$Build, "/", tempSDC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_Preheat_ltmin718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_Preheat_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_Preheat_ltmin718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_Preheat_ltmin718_Last[i] = 0}
}

## 179.) Count how many times the Smoke Detector Count less than the min of 718 in last 2 layers during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_ltmin718$Layer_ID <- paste(tempSDC_Preheat_ltmin718$Build, "/", tempSDC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Preheat_ltmin718, Layer_ID %in% Last2Layers)
  AllData$SDC_Preheat_ltmin718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}



## 180.) Count how many times the Smoke Detector Count less than the min of 718 in last 5 layers during Preheat ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Preheat_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempSDC_Preheat_ltmin718$Layer_ID <- paste(tempSDC_Preheat_ltmin718$Build, "/", tempSDC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Preheat_ltmin718, Layer_ID %in% Last5Layers)
  AllData$SDC_Preheat_ltmin718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}

#### Set Max and Min Thresholds for SDC during Melt ####
SDC_Melt_MaxThreshold <- as.numeric(max(filter(SmokeDetectorCount718, Process == "[3]: Melt")$Value))
SDC_Melt_MinThreshold <- as.numeric(min(filter(SmokeDetectorCount718, Process == "[3]: Melt")$Value))

## Smoke Detector Count > 718 Max ##
AllData$SDC_Melt_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 181.) Count how many times the Smoke Detector Count greater than the max of 718 in last 1 layer during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Melt_MaxThreshold & Process == "[3]: Melt")
tempSDC_Melt_gtmax718$Layer_ID <- paste(tempSDC_Melt_gtmax718$Build, "/", tempSDC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_Melt_gtmax718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_Melt_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_Melt_gtmax718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_Melt_gtmax718_Last[i] = 0}
}

## 182.) Count how many times the Smoke Detector Count greater than the max of 718 in last 2 layers during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Melt_MaxThreshold & Process == "[3]: Melt")
tempSDC_Melt_gtmax718$Layer_ID <- paste(tempSDC_Melt_gtmax718$Build, "/", tempSDC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Melt_gtmax718, Layer_ID %in% Last2Layers)
  AllData$SDC_Melt_gtmax718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}

## 183.) Count how many times the Smoke Detector Count greater than the max of 718 in last 5 layers during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_gtmax718 <- filter(SmokeDetectorCountAll, Value > SDC_Melt_MaxThreshold & Process == "[3]: Melt")
tempSDC_Melt_gtmax718$Layer_ID <- paste(tempSDC_Melt_gtmax718$Build, "/", tempSDC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Melt_gtmax718, Layer_ID %in% Last5Layers)
  AllData$SDC_Melt_gtmax718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}


## 184.) Count how many times the Smoke Detector Count less than the min of 718 in last 1 layer during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Melt_MinThreshold & Process == "[3]: Melt")
tempSDC_Melt_ltmin718$Layer_ID <- paste(tempSDC_Melt_ltmin718$Build, "/", tempSDC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempSDC_Melt_ltmin718$Layer_ID) {
    tempfilter_SDC <- filter(tempSDC_Melt_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_Melt_ltmin718_Last[i] = length(tempfilter_SDC$Layer_ID)}
  else {AllData$SDC_Melt_ltmin718_Last[i] = 0}
}

## 185.) Count how many times the Smoke Detector Count less than the min of 718 in last 2 layers during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Melt_MinThreshold & Process == "[3]: Melt")
tempSDC_Melt_ltmin718$Layer_ID <- paste(tempSDC_Melt_ltmin718$Build, "/", tempSDC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Melt_ltmin718, Layer_ID %in% Last2Layers)
  AllData$SDC_Melt_ltmin718_Last2[i] <- length(tempfilterSDC$Layer_ID)
}

## 186.) Count how many times the Smoke Detector Count less than the min of 718 in last 5 layers during Melt ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)
tempSDC_Melt_ltmin718 <- filter(SmokeDetectorCountAll, Value < SDC_Melt_MinThreshold & Process == "[3]: Melt")
tempSDC_Melt_ltmin718$Layer_ID <- paste(tempSDC_Melt_ltmin718$Build, "/", tempSDC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterSDC <- filter(tempSDC_Melt_ltmin718, Layer_ID %in% Last5Layers)
  AllData$SDC_Melt_ltmin718_Last5[i] <- length(tempfilterSDC$Layer_ID)
}


############

#### Smoke Detector Count Max per layer####
AllData$SDC_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 187.) Record the max Smoke Detector Count in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
    tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$SDC_max_Last[i] = max(tempfilter_SDC$Value)
}

## 188.) Record the max Smoke Detector Count in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers)
  AllData$SDC_max_Last2[i] = max(tempfilter_SDC$Value)
}

## 189.) Record the max Smoke Detector Count in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers)
  AllData$SDC_max_Last5[i] = max(tempfilter_SDC$Value)
}


#### Smoke Detector Count Min per layer####
AllData$SDC_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 190.) Record the min Smoke Detector Count in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$SDC_min_Last[i] = min(tempfilter_SDC$Value)
}

## 191.) Record the min Smoke Detector Count in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers)
  AllData$SDC_min_Last2[i] = min(tempfilter_SDC$Value)
}

## 192.) Record the min Smoke Detector Count in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers)
  AllData$SDC_min_Last5[i] = min(tempfilter_SDC$Value)
}


#### Smoke Detector Count Max during Preheat Last Layer ####
AllData$SDC_Preheat_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 193.) Record the max Smoke Detector Count during Preheat in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_SDC <- filter(tempfilter_SDC, Process == "[1]: Preheat")
  AllData$SDC_Preheat_max_Last[i] = max(tempfilter_SDC$Value)
}

## 194.) Record the max Smoke Detector Count during Preheat in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$SDC_Preheat_max_Last2[i] = max(tempfilter_SDC$Value)
}

## 195.) Record the max Smoke Detector Count during Preheat in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$SDC_Preheat_max_Last5[i] = max(tempfilter_SDC$Value)
}


#### Smoke Detector Count Min during Preheat Last Layer ####
AllData$SDC_Preheat_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Preheat_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 196.) Record the min Smoke Detector Count during Preheat in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_SDC <- filter(tempfilter_SDC, Process == "[1]: Preheat")
  AllData$SDC_Preheat_min_Last[i] = min(tempfilter_SDC$Value)
}

## 197.) Record the min Smoke Detector Count during Preheat in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$SDC_Preheat_min_Last2[i] = min(tempfilter_SDC$Value)
}






## 198.) Record the min Smoke Detector Count during Preheat in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$SDC_Preheat_min_Last5[i] = min(tempfilter_SDC$Value)
}


#### Smoke Detector Count Max during Melt Last Layer ####
AllData$SDC_Melt_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 199.) Record the max Smoke Detector Count during Melt in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_SDC <- filter(tempfilter_SDC, Process == "[3]: Melt")
  AllData$SDC_Melt_max_Last[i] = max(tempfilter_SDC$Value)
}

## 200.) Record the max Smoke Detector Count during Melt in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$SDC_Melt_max_Last2[i] = max(tempfilter_SDC$Value)
}

## 201.) Record the max Smoke Detector Count during Melt in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$SDC_Melt_max_Last5[i] = max(tempfilter_SDC$Value)
}


#### Smoke Detector Count Min during Melt Last Layer ####
AllData$SDC_Melt_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$SDC_Melt_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 202.) Record the min Smoke Detector Count during Melt in the last 1 layer ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_SDC <- filter(tempfilter_SDC, Process == "[3]: Melt")
  AllData$SDC_Melt_min_Last[i] = min(tempfilter_SDC$Value)
}

## 203.) Record the min Smoke Detector Count during Melt in the last 2 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$SDC_Melt_min_Last2[i] = min(tempfilter_SDC$Value)
}




## 204.) Record the min Smoke Detector Count during Melt in the last 5 layers ####
SmokeDetectorCountAll$Value <- as.numeric(SmokeDetectorCountAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_SDC <- filter(SmokeDetectorCountAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$SDC_Melt_min_Last5[i] = min(tempfilter_SDC$Value)
}

##############################
######## Beam Current ########
##############################

#### Set Max and Min Thresholds for BC during Preheat ####
BC_Preheat_MaxThreshold <- as.numeric(max(filter(BeamCurrent718, Process == "[1]: Preheat")$Value))
BC_Preheat_MinThreshold <- as.numeric(min(filter(BeamCurrent718, Process == "[1]: Preheat")$Value))

## Beam Current > 718 Max ##
AllData$BC_Preheat_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))

AllData$BC_Preheat_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 205.) Count how many times the Beam Current greater than the max of 718 in last 1 layer during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_gtmax718 <- filter(BeamCurrentAll, Value > BC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempBC_Preheat_gtmax718$Layer_ID <- paste(tempBC_Preheat_gtmax718$Build, "/", tempBC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBC_Preheat_gtmax718$Layer_ID) {
    tempfilter_BC <- filter(tempBC_Preheat_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_Preheat_gtmax718_Last[i] = length(tempfilter_BC$Layer_ID)}
  else {AllData$BC_Preheat_gtmax718_Last[i] = 0}
}

## 206.) Count how many times the Beam Current greater than the max of 718 in last 2 layers during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_gtmax718 <- filter(BeamCurrentAll, Value > BC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempBC_Preheat_gtmax718$Layer_ID <- paste(tempBC_Preheat_gtmax718$Build, "/", tempBC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Preheat_gtmax718, Layer_ID %in% Last2Layers)
  AllData$BC_Preheat_gtmax718_Last2[i] <- length(tempfilterBC$Layer_ID)
}

## 207.) Count how many times the Beam Current greater than the max of 718 in last 5 layers during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_gtmax718 <- filter(BeamCurrentAll, Value > BC_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempBC_Preheat_gtmax718$Layer_ID <- paste(tempBC_Preheat_gtmax718$Build, "/", tempBC_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Preheat_gtmax718, Layer_ID %in% Last5Layers)
  AllData$BC_Preheat_gtmax718_Last5[i] <- length(tempfilterBC$Layer_ID)
}


## 208.) Count how many times the Beam Current less than the min of 718 in last 1 layer during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_ltmin718 <- filter(BeamCurrentAll, Value < BC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempBC_Preheat_ltmin718$Layer_ID <- paste(tempBC_Preheat_ltmin718$Build, "/", tempBC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBC_Preheat_ltmin718$Layer_ID) {
    tempfilter_BC <- filter(tempBC_Preheat_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_Preheat_ltmin718_Last[i] = length(tempfilter_BC$Layer_ID)}
  else {AllData$BC_Preheat_ltmin718_Last[i] = 0}
}

## 209.) Count how many times the Beam Current less than the min of 718 in last 2 layers during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_ltmin718 <- filter(BeamCurrentAll, Value < BC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempBC_Preheat_ltmin718$Layer_ID <- paste(tempBC_Preheat_ltmin718$Build, "/", tempBC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Preheat_ltmin718, Layer_ID %in% Last2Layers)
  AllData$BC_Preheat_ltmin718_Last2[i] <- length(tempfilterBC$Layer_ID)
}



## 210.) Count how many times the Beam Current less than the min of 718 in last 5 layers during Preheat ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Preheat_ltmin718 <- filter(BeamCurrentAll, Value < BC_Preheat_MinThreshold & Process == "[1]: Preheat")
tempBC_Preheat_ltmin718$Layer_ID <- paste(tempBC_Preheat_ltmin718$Build, "/", tempBC_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Preheat_ltmin718, Layer_ID %in% Last5Layers)
  AllData$BC_Preheat_ltmin718_Last5[i] <- length(tempfilterBC$Layer_ID)
}


#### Set Max and Min Thresholds for BC during Melt ####
BC_Melt_MaxThreshold <- as.numeric(max(filter(BeamCurrent718, Process == "[3]: Melt")$Value))
BC_Melt_MinThreshold <- as.numeric(min(filter(BeamCurrent718, Process == "[3]: Melt")$Value))

## Beam Current > 718 Max ##
AllData$BC_Melt_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))

AllData$BC_Melt_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 211.) Count how many times the Beam Current greater than the max of 718 in last 1 layer during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_gtmax718 <- filter(BeamCurrentAll, Value > BC_Melt_MaxThreshold & Process == "[3]: Melt")
tempBC_Melt_gtmax718$Layer_ID <- paste(tempBC_Melt_gtmax718$Build, "/", tempBC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBC_Melt_gtmax718$Layer_ID) {
    tempfilter_BC <- filter(tempBC_Melt_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_Melt_gtmax718_Last[i] = length(tempfilter_BC$Layer_ID)}
  else {AllData$BC_Melt_gtmax718_Last[i] = 0}
}

## 212.) Count how many times the Beam Current greater than the max of 718 in last 2 layers during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_gtmax718 <- filter(BeamCurrentAll, Value > BC_Melt_MaxThreshold & Process == "[3]: Melt")
tempBC_Melt_gtmax718$Layer_ID <- paste(tempBC_Melt_gtmax718$Build, "/", tempBC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Melt_gtmax718, Layer_ID %in% Last2Layers)
  AllData$BC_Melt_gtmax718_Last2[i] <- length(tempfilterBC$Layer_ID)
}

## 213.) Count how many times the Beam Current greater than the max of 718 in last 5 layers during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_gtmax718 <- filter(BeamCurrentAll, Value > BC_Melt_MaxThreshold & Process == "[3]: Melt")
tempBC_Melt_gtmax718$Layer_ID <- paste(tempBC_Melt_gtmax718$Build, "/", tempBC_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Melt_gtmax718, Layer_ID %in% Last5Layers)
  AllData$BC_Melt_gtmax718_Last5[i] <- length(tempfilterBC$Layer_ID)
}


## 214.) Count how many times the Beam Current less than the min of 718 in last 1 layer during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_ltmin718 <- filter(BeamCurrentAll, Value < BC_Melt_MinThreshold & Process == "[3]: Melt")
tempBC_Melt_ltmin718$Layer_ID <- paste(tempBC_Melt_ltmin718$Build, "/", tempBC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBC_Melt_ltmin718$Layer_ID) {
    tempfilter_BC <- filter(tempBC_Melt_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BC_Melt_ltmin718_Last[i] = length(tempfilter_BC$Layer_ID)}
  else {AllData$BC_Melt_ltmin718_Last[i] = 0}
}

## 215.) Count how many times the Beam Current less than the min of 718 in last 2 layers during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_ltmin718 <- filter(BeamCurrentAll, Value < BC_Melt_MinThreshold & Process == "[3]: Melt")
tempBC_Melt_ltmin718$Layer_ID <- paste(tempBC_Melt_ltmin718$Build, "/", tempBC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Melt_ltmin718, Layer_ID %in% Last2Layers)
  AllData$BC_Melt_ltmin718_Last2[i] <- length(tempfilterBC$Layer_ID)
}

## 216.) Count how many times the Beam Current less than the min of 718 in last 5 layers during Melt ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)
tempBC_Melt_ltmin718 <- filter(BeamCurrentAll, Value < BC_Melt_MinThreshold & Process == "[3]: Melt")
tempBC_Melt_ltmin718$Layer_ID <- paste(tempBC_Melt_ltmin718$Build, "/", tempBC_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBC <- filter(tempBC_Melt_ltmin718, Layer_ID %in% Last5Layers)
  AllData$BC_Melt_ltmin718_Last5[i] <- length(tempfilterBC$Layer_ID)
}


############
#### Beam Current Max during Preheat Last Layer ####
AllData$BC_Preheat_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 217.) Record the max Beam Current during Preheat in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_BC <- filter(tempfilter_BC, Process == "[1]: Preheat")
  AllData$BC_Preheat_max_Last[i] = max(tempfilter_BC$Value)
}

## 218.) Record the max Beam Current during Preheat in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$BC_Preheat_max_Last2[i] = max(tempfilter_BC$Value)
}

## 219.) Record the max Beam Current during Preheat in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$BC_Preheat_max_Last5[i] = max(tempfilter_BC$Value)
}


#### Beam Current Min during Preheat Last Layer ####
AllData$BC_Preheat_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Preheat_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 220.) Record the min Beam Current during Preheat in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_BC <- filter(tempfilter_BC, Process == "[1]: Preheat")
  AllData$BC_Preheat_min_Last[i] = min(tempfilter_BC$Value)
}

## 221.) Record the min Beam Current during Preheat in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$BC_Preheat_min_Last2[i] = min(tempfilter_BC$Value)
}






## 222.) Record the min Beam Current during Preheat in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$BC_Preheat_min_Last5[i] = min(tempfilter_BC$Value)
}

#### Beam Current Max during Melt Last Layer ####
AllData$BC_Melt_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 223.) Record the max Beam Current during Melt in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_BC <- filter(tempfilter_BC, Process == "[3]: Melt")
  AllData$BC_Melt_max_Last[i] = max(tempfilter_BC$Value)
}

## 224.) Record the max Beam Current during Melt in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$BC_Melt_max_Last2[i] = max(tempfilter_BC$Value)
}

## 225.) Record the max Beam Current during Melt in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$BC_Melt_max_Last5[i] = max(tempfilter_BC$Value)
}

#### Beam Current Min during Melt Last Layer ####
AllData$BC_Melt_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BC_Melt_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 226.) Record the min Beam Current during Melt in the last 1 layer ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_BC <- filter(tempfilter_BC, Process == "[3]: Melt")
  AllData$BC_Melt_min_Last[i] = min(tempfilter_BC$Value)
}

## 227.) Record the min Beam Current during Melt in the last 2 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$BC_Melt_min_Last2[i] = min(tempfilter_BC$Value)
}






## 228.) Record the min Beam Current during Melt in the last 5 layers ####
BeamCurrentAll$Value <- as.numeric(BeamCurrentAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BC <- filter(BeamCurrentAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$BC_Melt_min_Last5[i] = min(tempfilter_BC$Value)
}


###############################
###### High Voltage Grid ######
###############################

#### Set Max and Min Thresholds for HVG during Preheat ####
HVG_Preheat_MaxThreshold <- as.numeric(max(filter(HighVoltageGrid718, Process == "[1]: Preheat")$Value))
HVG_Preheat_MinThreshold <- as.numeric(min(filter(HighVoltageGrid718, Process == "[1]: Preheat")$Value))

## High Voltage Grid > 718 Max ##
AllData$HVG_Preheat_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))

AllData$HVG_Preheat_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 229.) Count how many times the High Voltage Grid greater than the max of 718 in last 1 layer during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_gtmax718$Layer_ID <- paste(tempHVG_Preheat_gtmax718$Build, "/", tempHVG_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_Preheat_gtmax718$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_Preheat_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVG_Preheat_gtmax718_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$HVG_Preheat_gtmax718_Last[i] = 0}
}

## 230.) Count how many times the High Voltage Grid greater than the max of 718 in last 2 layers during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_gtmax718$Layer_ID <- paste(tempHVG_Preheat_gtmax718$Build, "/", tempHVG_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Preheat_gtmax718, Layer_ID %in% Last2Layers)
  AllData$HVG_Preheat_gtmax718_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 231.) Count how many times the High Voltage Grid greater than the max of 718 in last 5 layers during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Preheat_MaxThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_gtmax718$Layer_ID <- paste(tempHVG_Preheat_gtmax718$Build, "/", tempHVG_Preheat_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Preheat_gtmax718, Layer_ID %in% Last5Layers)
  AllData$HVG_Preheat_gtmax718_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


## 232.) Count how many times the High Voltage Grid less than the min of 718 in last 1 layer during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Preheat_MinThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_ltmin718$Layer_ID <- paste(tempHVG_Preheat_ltmin718$Build, "/", tempHVG_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_Preheat_ltmin718$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_Preheat_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVG_Preheat_ltmin718_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$HVG_Preheat_ltmin718_Last[i] = 0}
}

## 233.) Count how many times the High Voltage Grid less than the min of 718 in last 2 layers during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Preheat_MinThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_ltmin718$Layer_ID <- paste(tempHVG_Preheat_ltmin718$Build, "/", tempHVG_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Preheat_ltmin718, Layer_ID %in% Last2Layers)
  AllData$HVG_Preheat_ltmin718_Last2[i] <- length(tempfilterHVG$Layer_ID)
}



## 234.) Count how many times the High Voltage Grid less than the min of 718 in last 5 layers during Preheat ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Preheat_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Preheat_MinThreshold & Process == "[1]: Preheat")
tempHVG_Preheat_ltmin718$Layer_ID <- paste(tempHVG_Preheat_ltmin718$Build, "/", tempHVG_Preheat_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Preheat_ltmin718, Layer_ID %in% Last5Layers)
  AllData$HVG_Preheat_ltmin718_Last5[i] <- length(tempfilterHVG$Layer_ID)
}

#### Set Max and Min Thresholds for HVG during Melt ####
HVG_Melt_MaxThreshold <- max(filter(HighVoltageGrid718, Process == "[3]: Melt")$Value)
HVG_Melt_MinThreshold <- min(filter(HighVoltageGrid718, Process == "[3]: Melt")$Value)

## High Voltage Grid Max/Min >/< 718 ##
AllData$HVG_Melt_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))

AllData$HVG_Melt_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 235.) Count how many times the High Voltage Grid greater than the max of 718 in last 1 layer during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Melt_MaxThreshold & Process == "[3]: Melt")
tempHVG_Melt_gtmax718$Layer_ID <- paste(tempHVG_Melt_gtmax718$Build, "/", tempHVG_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_Melt_gtmax718$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_Melt_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVG_Melt_gtmax718_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$HVG_Melt_gtmax718_Last[i] = 0}
}

## 236.) Count how many times the High Voltage Grid greater than the max of 718 in last 2 layers during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Melt_MaxThreshold & Process == "[3]: Melt")
tempHVG_Melt_gtmax718$Layer_ID <- paste(tempHVG_Melt_gtmax718$Build, "/", tempHVG_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Melt_gtmax718, Layer_ID %in% Last2Layers)
  AllData$HVG_Melt_gtmax718_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 237.) Count how many times the High Voltage Grid greater than the max of 718 in last 5 layers during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_gtmax718 <- filter(HighVoltageGridAll, Value > HVG_Melt_MaxThreshold & Process == "[3]: Melt")
tempHVG_Melt_gtmax718$Layer_ID <- paste(tempHVG_Melt_gtmax718$Build, "/", tempHVG_Melt_gtmax718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Melt_gtmax718, Layer_ID %in% Last5Layers)
  AllData$HVG_Melt_gtmax718_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


## 238.) Count how many times the High Voltage Grid less than the min of 718 in last 1 layer during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Melt_MinThreshold & Process == "[3]: Melt")
tempHVG_Melt_ltmin718$Layer_ID <- paste(tempHVG_Melt_ltmin718$Build, "/", tempHVG_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVG_Melt_ltmin718$Layer_ID) {
    tempfilter_HVG <- filter(tempHVG_Melt_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVG_Melt_ltmin718_Last[i] = length(tempfilter_HVG$Layer_ID)}
  else {AllData$HVG_Melt_ltmin718_Last[i] = 0}
}

## 239.) Count how many times the High Voltage Grid less than the min of 718 in last 2 layers during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Melt_MinThreshold & Process == "[3]: Melt")
tempHVG_Melt_ltmin718$Layer_ID <- paste(tempHVG_Melt_ltmin718$Build, "/", tempHVG_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Melt_ltmin718, Layer_ID %in% Last2Layers)
  AllData$HVG_Melt_ltmin718_Last2[i] <- length(tempfilterHVG$Layer_ID)
}

## 240.) Count how many times the High Voltage Grid less than the min of 718 in last 5 layers during Melt ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)
tempHVG_Melt_ltmin718 <- filter(HighVoltageGridAll, Value < HVG_Melt_MinThreshold & Process == "[3]: Melt")
tempHVG_Melt_ltmin718$Layer_ID <- paste(tempHVG_Melt_ltmin718$Build, "/", tempHVG_Melt_ltmin718$Layer)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVG <- filter(tempHVG_Melt_ltmin718, Layer_ID %in% Last5Layers)
  AllData$HVG_Melt_ltmin718_Last5[i] <- length(tempfilterHVG$Layer_ID)
}


############
#### High Voltage Grid Max during Preheat Last Layer ####
AllData$HVG_Preheat_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 241) Record the max High Voltage Grid during Preheat in the last 1 layer ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_HVG <- filter(tempfilter_HVG, Process == "[1]: Preheat")
  AllData$HVG_Preheat_max_Last[i] = max(tempfilter_HVG$Value)
}

## 242.) Record the max High Voltage Grid during Preheat in the last 2 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$HVG_Preheat_max_Last2[i] = max(tempfilter_HVG$Value)
}

## 243.) Record the max High Voltage Grid during Preheat in the last 5 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$HVG_Preheat_max_Last5[i] = max(tempfilter_HVG$Value)
}

#### High Voltage Grid Min during Preheat Last Layer ####
AllData$HVG_Preheat_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Preheat_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 244.) Record the min High Voltage Grid during Preheat in the last 1 layer ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_HVG <- filter(tempfilter_HVG, Process == "[1]: Preheat")
  AllData$HVG_Preheat_min_Last[i] = min(tempfilter_HVG$Value)
}

## 245.) Record the min High Voltage Grid during Preheat in the last 2 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers & Process == "[1]: Preheat")
  AllData$HVG_Preheat_min_Last2[i] = min(tempfilter_HVG$Value)
}






## 246.) Record the min High Voltage Grid during Preheat in the last 5 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers & Process == "[1]: Preheat")
  AllData$HVG_Preheat_min_Last5[i] = min(tempfilter_HVG$Value)
}

#### High Voltage Grid Max during Melt Last Layer ####
AllData$HVG_Melt_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 247.) Record the max High Voltage Grid during Melt in the last 1 layer ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_HVG <- filter(tempfilter_HVG, Process == "[3]: Melt")
  AllData$HVG_Melt_max_Last[i] = max(tempfilter_HVG$Value)
}

## 248.) Record the max High Voltage Grid during Melt in the last 2 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$HVG_Melt_max_Last2[i] = max(tempfilter_HVG$Value)
}

## 249.) Record the max High Voltage Grid during Melt in the last 5 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$HVG_Melt_max_Last5[i] = max(tempfilter_HVG$Value)
}


#### High Voltage Grid Min during Melt Last Layer ####
AllData$HVG_Melt_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVG_Melt_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 250.) Record the min High Voltage Grid during Melt in the last 1 layer ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  tempfilter_HVG <- filter(tempfilter_HVG, Process == "[3]: Melt")
  AllData$HVG_Melt_min_Last[i] = min(tempfilter_HVG$Value)
}

## 251.) Record the min High Voltage Grid during Melt in the last 2 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last2Layers & Process == "[3]: Melt")
  AllData$HVG_Melt_min_Last2[i] = min(tempfilter_HVG$Value)
}










## 252.) Record the min High Voltage Grid during Melt in the last 5 layers ####
HighVoltageGridAll$Value <- as.numeric(HighVoltageGridAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVG <- filter(HighVoltageGridAll, Layer_ID %in% Last5Layers & Process == "[3]: Melt")
  AllData$HVG_Melt_min_Last5[i] = min(tempfilter_HVG$Value)
}


###############################
####### Last Layer Time #######
###############################

#### Record the Last Layer Time for the previous layer ####
AllData$LastLayerTime_Last <- rep(0, length(AllData$Layer_ID))

## 253.) Record the Last Layer Time of the last layer ####

for(i in 1:length(AllData$Layer_ID)) {
  filter_LLT <- filter(LastLayerTimeAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if(length(filter_LLT$Value) != 0) {AllData$LastLayerTime_Last[i] <- sum(filter_LLT$Value)}
  else{AllData$LastLayerTime_Last[i] = AllData$LastLayerTime_Last[i-1]}
}

#### Record the maximum Last Layer Time for the previous 2 layers ####
AllData$LastLayerTime_Last2 <- rep(0, length(AllData$Layer_ID))

## 254.) Record the Last Layer Time of the last layer ####
for(i in 1:length(AllData$Layer_ID)) {
  filter_LLT <- filter(LastLayerTimeAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$LastLayerTime_Last2[i] <- sum(filter_LLT$Value)
  
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_LLT <- filter(LastLayerTimeAll, Layer_ID %in% Last2Layers)
  tempAgg_LastLayerTime <- aggregate(tempfilter_LLT$Value, by = list(tempfilter_LLT$Layer_ID), FUN = "sum")
  AllData$LastLayerTime_Last2[i] = max(tempAgg_LastLayerTime$x)
}

#### Record the maximum Last Layer Time for the previous 5 layers ####
AllData$LastLayerTime_Last5 <- rep(0, length(AllData$Layer_ID))

## 255.) Record the Last Layer Time of the last layer ####
for(i in 1:length(AllData$Layer_ID)) {
  filter_LLT <- filter(LastLayerTimeAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$LastLayerTime_Last5[i] <- sum(filter_LLT$Value)
  
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_LLT <- filter(LastLayerTimeAll, Layer_ID %in% Last5Layers)
  tempAgg_LastLayerTime <- aggregate(tempfilter_LLT$Value, by = list(tempfilter_LLT$Layer_ID), FUN = "sum")
  AllData$LastLayerTime_Last5[i] = max(tempAgg_LastLayerTime$x)
}


################################
######### Pulse Length #########
################################
#### Record the maximum Pulse Length for the previous layer ####
AllData$PulseLength_Max_Last <- rep(0, length(AllData$Layer_ID))
## 256.) Record the Pulse Length of the last layer ####
for(i in 1:length(AllData$Layer_ID)) {
  filter_PL <- filter(tempPulseLengthAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if (length(filter_PL$Value) == 0) {AllData$PulseLength_Max_Last[i] = max(filter(tempPulseLengthAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$PulseLength_Max_Last[i] = max(filter_PL$Value)}
}

#### Record the maximum Pulse Length for the previous 2 layers ####
AllData$PulseLength_Max_Last2 <- rep(0, length(AllData$Layer_ID))

## 257.) Record the Pulse Length of the last 2 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_PL <- filter(tempPulseLengthAll, Layer_ID %in% Last2Layers)
  if (length(tempfilter_PL$Value) == 0) {AllData$PulseLength_Max_Last2[i] = max(filter(tempPulseLengthAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$PulseLength_Max_Last2[i] = max(tempfilter_PL$Value)}
}

#### Record the maximum Pulse Length for the previous 5 layers ####
AllData$PulseLength_Max_Last5 <- rep(0, length(AllData$Layer_ID))

## 258.) Record the Pulse Length of the last 5 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_PL <- filter(tempPulseLengthAll, Layer_ID %in% Last5Layers)
  if (length(tempfilter_PL$Value) == 0) {AllData$PulseLength_Max_Last5[i] = max(filter(tempPulseLengthAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$PulseLength_Max_Last5[i] = max(tempfilter_PL$Value)}
}



#### Set Max and Min Thresholds for PL ####
PL_MaxThreshold <- as.numeric(max(PulseLength718$Value))
PL_MinThreshold <- as.numeric(min(PulseLength718$Value))

## Smoke Detector Count > 718 Max ##
AllData$PL_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$PL_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$PL_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$PL_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$PL_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$PL_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 259.) Count how many times the Pulse Length greater than the max of 718 in last 1 layer ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gtmax718 <- filter(tempPulseLengthAll, Value > PL_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_gtmax718$Layer_ID) {
    tempfilter_PL <- filter(tempPL_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_gtmax718_Last[i] = length(tempfilter_PL$Layer_ID)}
  else {AllData$PL_gtmax718_Last[i] = 0}
}

## 260.) Count how many times the Pulse Length greater than the max of 718 in last 2 layers ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gtmax718 <- filter(tempPulseLengthAll, Value > PL_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPL <- filter(tempPL_gtmax718, Layer_ID %in% Last2Layers)
  AllData$PL_gtmax718_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 261.) Count how many times the Pulse Length greater than the max of 718 in last 5 layers ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_gtmax718 <- filter(tempPulseLengthAll, Value > PL_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPL <- filter(tempPL_gtmax718, Layer_ID %in% Last5Layers)
  AllData$PL_gtmax718_Last5[i] <- length(tempfilterPL$Layer_ID)
}

## 262.) Count how many times the Pulse Length less than the min of 718 in last 1 layer ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_ltmin718 <- filter(tempPulseLengthAll, Value < PL_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempPL_ltmin718$Layer_ID) {
    tempfilter_PL <- filter(tempPL_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$PL_ltmin718_Last[i] = length(tempfilter_PL$Layer_ID)}
  else {AllData$PL_ltmin718_Last[i] = 0}
}

## 263.) Count how many times the Pulse Length less than the min of 718 in last 2 layers ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_ltmin718 <- filter(tempPulseLengthAll, Value < PL_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPL <- filter(tempPL_ltmin718, Layer_ID %in% Last2Layers)
  AllData$PL_ltmin718_Last2[i] <- length(tempfilterPL$Layer_ID)
}

## 264.) Count how many times the Pulse Length less than the min of 718 in last 5 layers ####
tempPulseLengthAll$Value <- as.numeric(tempPulseLengthAll$Value)
tempPL_ltmin718 <- filter(tempPulseLengthAll, Value < PL_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterPL <- filter(tempPL_ltmin718, Layer_ID %in% Last5Layers)
  AllData$PL_ltmin718_Last5[i] <- length(tempfilterPL$Layer_ID)
}



#########################################
######### High Voltage Feedback #########
#########################################
#### Record the maximum High Voltage Feedback for the previous layer ####
AllData$HighVoltageFeedback_Max_Last <- rep(0, length(AllData$Layer_ID))
## 265.) Record the High Voltage Feedback of the last layer ####
for(i in 1:length(AllData$Layer_ID)) {
  filter_HVF <- filter(tempHighVoltageFeedbackAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if (length(filter_HVF$Value) == 0) {AllData$HighVoltageFeedback_Max_Last[i] = max(filter(tempHighVoltageFeedbackAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$HighVoltageFeedback_Max_Last[i] = max(filter_HVF$Value)}
}

#### Record the maximum High Voltage Feedback for the previous 2 layers ####
AllData$HighVoltageFeedback_Max_Last2 <- rep(0, length(AllData$Layer_ID))

## 266.) Record the High Voltage Feedback of the last 2 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVF <- filter(tempHighVoltageFeedbackAll, Layer_ID %in% Last2Layers)
  if (length(tempfilter_HVF$Value) == 0) {AllData$HighVoltageFeedback_Max_Last2[i] = max(filter(tempHighVoltageFeedbackAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$HighVoltageFeedback_Max_Last2[i] = max(tempfilter_HVF$Value)}
}

#### Record the maximum High Voltage Feedback for the previous 5 layers ####
AllData$HighVoltageFeedback_Max_Last5 <- rep(0, length(AllData$Layer_ID))

## 267.) Record the High Voltage Feedback of the last 5 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_HVF <- filter(tempHighVoltageFeedbackAll, Layer_ID %in% Last5Layers)
  if (length(tempfilter_HVF$Value) == 0) {AllData$HighVoltageFeedback_Max_Last5[i] = max(filter(tempHighVoltageFeedbackAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)])$Value)}
  else {AllData$HighVoltageFeedback_Max_Last5[i] = max(tempfilter_HVF$Value)}
}



#### Set Max and Min Thresholds for HVF ####
HVF_MaxThreshold <- as.numeric(max(HighVoltageFeedback718$Value))
HVF_MinThreshold <- as.numeric(min(HighVoltageFeedback718$Value))

## Smoke Detector Count > 718 Max ##
AllData$HVF_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVF_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVF_gtmax718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$HVF_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$HVF_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$HVF_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 268.) Count how many times the High Voltage Feedback greater than the max of 718 in last 1 layer ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gtmax718 <- filter(tempHighVoltageFeedbackAll, Value > HVF_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_gtmax718$Layer_ID) {
    tempfilter_HVF <- filter(tempHVF_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_gtmax718_Last[i] = length(tempfilter_HVF$Layer_ID)}
  else {AllData$HVF_gtmax718_Last[i] = 0}
}

## 269.) Count how many times the High Voltage Feedback greater than the max of 718 in last 2 layers ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gtmax718 <- filter(tempHighVoltageFeedbackAll, Value > HVF_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVF <- filter(tempHVF_gtmax718, Layer_ID %in% Last2Layers)
  AllData$HVF_gtmax718_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 270.) Count how many times the High Voltage Feedback greater than the max of 718 in last 5 layers ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_gtmax718 <- filter(tempHighVoltageFeedbackAll, Value > HVF_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVF <- filter(tempHVF_gtmax718, Layer_ID %in% Last5Layers)
  AllData$HVF_gtmax718_Last5[i] <- length(tempfilterHVF$Layer_ID)
}

## 271.) Count how many times the High Voltage Feedback less than the min of 718 in last 1 layer ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_ltmin718 <- filter(tempHighVoltageFeedbackAll, Value < HVF_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempHVF_ltmin718$Layer_ID) {
    tempfilter_HVF <- filter(tempHVF_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$HVF_ltmin718_Last[i] = length(tempfilter_HVF$Layer_ID)}
  else {AllData$HVF_ltmin718_Last[i] = 0}
}

## 272.) Count how many times the High Voltage Feedback less than the min of 718 in last 2 layers ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_ltmin718 <- filter(tempHighVoltageFeedbackAll, Value < HVF_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVF <- filter(tempHVF_ltmin718, Layer_ID %in% Last2Layers)
  AllData$HVF_ltmin718_Last2[i] <- length(tempfilterHVF$Layer_ID)
}

## 273.) Count how many times the High Voltage Feedback less than the min of 718 in last 5 layers ####
tempHighVoltageFeedbackAll$Value <- as.numeric(tempHighVoltageFeedbackAll$Value)
tempHVF_ltmin718 <- filter(tempHighVoltageFeedbackAll, Value < HVF_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterHVF <- filter(tempHVF_ltmin718, Layer_ID %in% Last5Layers)
  AllData$HVF_ltmin718_Last5[i] <- length(tempfilterHVF$Layer_ID)
}




#############################################
####### Chamber Vacuum Gauge Feedback #######
#############################################
#### Set Max and Min Thresholds for CVG ####
CVG_MaxThreshold <- as.numeric(max(ChamberVacuum718$Value))
CVG_MinThreshold <- as.numeric(min(ChamberVacuum718$Value))

## This only occurred when the build crashed or stopped ##
##########################################################

# ## Chamber Vacuum > 718 Max ##
# AllData$CVG_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))

# ## 135.) Count how many times the Chamber Vacuum greater than the max of 718 in last 1 layer
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold)
# tempCVG_gtmax718$Layer_ID <- paste(tempCVG_gtmax718$Build, "/", tempCVG_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_gtmax718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_gtmax718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_gtmax718_Last[i] = 0}
# }

# ## 136.) Count how many times the Chamber Vacuum greater than the max of 718 in last 2 layers 
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold)
# tempCVG_gtmax718$Layer_ID <- paste(tempCVG_gtmax718$Build, "/", tempCVG_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_gtmax718, Layer_ID %in% Last2Layers)
#   AllData$CVG_gtmax718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }

# ## 137.) Count how many times the Chamber Vacuum less than the min of 718 in last 1 layer 
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold)
# tempCVG_ltmin718$Layer_ID <- paste(tempCVG_ltmin718$Build, "/", tempCVG_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_ltmin718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_ltmin718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_ltmin718_Last[i] = 0}
# }

# ## 138.) Count how many times the Chamber Vacuum less than the min of 718 in last 2 layers 
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold)
# tempCVG_ltmin718$Layer_ID <- paste(tempCVG_ltmin718$Build, "/", tempCVG_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_ltmin718, Layer_ID %in% Last2Layers)
#   AllData$CVG_ltmin718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }

#### Set Max and Min Thresholds for CVG during Preheat ####
CVG_Preheat_MaxThreshold <- as.numeric(max(filter(ChamberVacuum718, Process == "[1]: Preheat")$Value))
CVG_Preheat_MinThreshold <- as.numeric(min(filter(ChamberVacuum718, Process == "[1]: Preheat")$Value))

## This only occurred when the build crashed or stopped ##
##########################################################
# ## Chamber Vacuum > 718 Max ##
# AllData$CVG_Preheat_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_Preheat_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_Preheat_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_Preheat_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
# 
# ## 139.) Count how many times the Chamber Vacuum greater than the max of 718 in last 1 layer during Preheat
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Preheat_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold & Process == "[1]: Preheat")
# tempCVG_Preheat_gtmax718$Layer_ID <- paste(tempCVG_Preheat_gtmax718$Build, "/", tempCVG_Preheat_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_Preheat_gtmax718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_Preheat_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_Preheat_gtmax718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_Preheat_gtmax718_Last[i] = 0}
# }
# 
# ## 140.) Count how many times the Chamber Vacuum greater than the max of 718 in last 2 layers during Preheat
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Preheat_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold & Process == "[1]: Preheat")
# tempCVG_Preheat_gtmax718$Layer_ID <- paste(tempCVG_Preheat_gtmax718$Build, "/", tempCVG_Preheat_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_Preheat_gtmax718, Layer_ID %in% Last2Layers)
#   AllData$CVG_Preheat_gtmax718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }
# 
# ## 141.) Count how many times the Chamber Vacuum less than the min of 718 in last 1 layer during Preheat
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Preheat_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold & Process == "[1]: Preheat")
# tempCVG_Preheat_ltmin718$Layer_ID <- paste(tempCVG_Preheat_ltmin718$Build, "/", tempCVG_Preheat_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_Preheat_ltmin718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_Preheat_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_Preheat_ltmin718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_Preheat_ltmin718_Last[i] = 0}
# }
# 
# ## 142.) Count how many times the Chamber Vacuum less than the min of 718 in last 2 layers during Preheat
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Preheat_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold & Process == "[1]: Preheat")
# tempCVG_Preheat_ltmin718$Layer_ID <- paste(tempCVG_Preheat_ltmin718$Build, "/", tempCVG_Preheat_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_Preheat_ltmin718, Layer_ID %in% Last2Layers)
#   AllData$CVG_Preheat_ltmin718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }

#### Set Max and Min Thresholds for CVG during Melt ####
CVG_Melt_MaxThreshold <- as.numeric(max(filter(ChamberVacuum718, Process == "[3]: Melt")$Value))
CVG_Melt_MinThreshold <- as.numeric(min(filter(ChamberVacuum718, Process == "[3]: Melt")$Value))

## This only occurred when the build crashed or stopped ##
##########################################################
# ## Chamber Vacuum > 718 Max ##
# AllData$CVG_Melt_gtmax718_Last <- rep(0, length(AllData$Layer_ID))
# AllData$CVG_Melt_gtmax718_Last2 <- rep(0, length(AllData$Layer_ID))
# 
# ## 143.) Count how many times the Chamber Vacuum greater than the max of 718 in last 1 layer during Melt
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Melt_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold & Process == "[3]: Melt")
# tempCVG_Melt_gtmax718$Layer_ID <- paste(tempCVG_Melt_gtmax718$Build, "/", tempCVG_Melt_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_Melt_gtmax718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_Melt_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_Melt_gtmax718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_Melt_gtmax718_Last[i] = 0}
# }
# 
# ## 144.) Count how many times the Chamber Vacuum greater than the max of 718 in last 2 layers during Melt
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Melt_gtmax718 <- filter(ChamberVacuumAll, Value > CVG_MaxThreshold & Process == "[3]: Melt")
# tempCVG_Melt_gtmax718$Layer_ID <- paste(tempCVG_Melt_gtmax718$Build, "/", tempCVG_Melt_gtmax718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_Melt_gtmax718, Layer_ID %in% Last2Layers)
#   AllData$CVG_Melt_gtmax718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }
# 
# ## 145.) Count how many times the Chamber Vacuum less than the min of 718 in last 1 layer during Melt
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Melt_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold & Process == "[3]: Melt")
# tempCVG_Melt_ltmin718$Layer_ID <- paste(tempCVG_Melt_ltmin718$Build, "/", tempCVG_Melt_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempCVG_Melt_ltmin718$Layer_ID) {
#     tempfilter_CVG <- filter(tempCVG_Melt_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
#     AllData$CVG_Melt_ltmin718_Last[i] = length(tempfilter_CVG$Layer_ID)}
#   else {AllData$CVG_Melt_ltmin718_Last[i] = 0}
# }
# 
# ## 146.) Count how many times the Chamber Vacuum less than the min of 718 in last 2 layers during Melt
# ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
# tempCVG_Melt_ltmin718 <- filter(ChamberVacuumAll, Value < CVG_MinThreshold & Process == "[3]: Melt")
# tempCVG_Melt_ltmin718$Layer_ID <- paste(tempCVG_Melt_ltmin718$Build, "/", tempCVG_Melt_ltmin718$Layer)
# 
# for(i in 1:length(AllData$Layer_ID)) {
#   Last2Layers<-NULL
#   for(j in 1:2) {
#     Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
#   }
# 
#   tempfilterCVG <- filter(tempCVG_Melt_ltmin718, Layer_ID %in% Last2Layers)
#   AllData$CVG_Melt_ltmin718_Last2[i] <- length(tempfilterCVG$Layer_ID)
# }

############

#### Chamber Vacuum Max per layer####
AllData$CVG_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$CVG_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$CVG_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 274.) Record the max Chamber Vacuum in the last 1 layer ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$CVG_max_Last[i] = max(tempfilter_CVG$Value)
}

## 275.) Record the max Chamber Vacuum in the last 2 layers ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID %in% Last2Layers)
  AllData$CVG_max_Last2[i] = max(tempfilter_CVG$Value)
}

## 276.) Record the max Chamber Vacuum in the last 5 layers ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID %in% Last5Layers)
  AllData$CVG_max_Last5[i] = max(tempfilter_CVG$Value)
}


#### Chamber Vacuum Min per layer####
AllData$CVG_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$CVG_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$CVG_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 277.) Record the min Chamber Vacuum in the last 1 layer ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  AllData$CVG_min_Last[i] = min(tempfilter_CVG$Value)
}

## 278.) Record the min Chamber Vacuum in the last 2 layers ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID %in% Last2Layers)
  AllData$CVG_min_Last2[i] = min(tempfilter_CVG$Value)
}


## 279.) Record the min Chamber Vacuum in the last 5 layers ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_CVG <- filter(ChamberVacuumAll, Layer_ID %in% Last5Layers)
  AllData$CVG_min_Last5[i] = min(tempfilter_CVG$Value)
}




#############################################################################################
#### Covariates: Chamber Vacuum Mean Value ####
AllData$CVG_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$CVG_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$CVG_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)
tempAggCVG <- aggregate(ChamberVacuumAll$Value, by = list(ChamberVacuumAll$Layer_ID), FUN=mean)
colnames(tempAggCVG) <- list("Layer_ID", "Mean")

#Separate Build and Layer to sort
tempAggCVG <- separate(tempAggCVG, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempAggCVG$Layer <- as.numeric(tempAggCVG$Layer)
tempAggCVG <- tempAggCVG[order(tempAggCVG$Build, tempAggCVG$Layer),]

## 280.) Mean Chamber Vacuum in the last 1 layer ####
for(i in 1:length(AllData$Layer_ID)) {
  AllData$CVG_Mean_Last[i] <- tempAggCVG$Mean[match(AllData$Layer_ID[i], tempAggCVG$Layer_ID)-1]
}

## 281.) Mean Chamber Vacuum in the last 2 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }

  tempAggCVG_Last2 <- filter(ChamberVacuumAll, Layer_ID %in% Last2Layers)
  AllData$CVG_Mean_Last2[i] <- mean(tempAggCVG_Last2$Value)
}

## 282.) Mean Chamber Vacuum in the last 5 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempAggCVG_Last5 <- filter(ChamberVacuumAll, Layer_ID %in% Last5Layers)
  AllData$CVG_Mean_Last5[i] <- mean(tempAggCVG_Last5$Value)
}

#### Covariates: Chamber Vacuum Variance Value ####
AllData$CVG_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$CVG_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$CVG_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 283.) Variance Chamber Vacuum in the last 1 layer ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_CVG <- filter(ChamberVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_CVG$Value) <=1) {AllData$CVG_Var_Last[i] <- 0}
  else{AllData$CVG_Var_Last[i] <- var(filter_CVG$Value)}
}

## 284.) Variance Chamber Vacuum in the last 2 layer ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(ChamberVacuumAll,(Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(filter_CVG$Value) <=1) {AllData$CVG_Var_Last2[i] <- 0}
  else{AllData$CVG_Var_Last2[i] <- var(filter_CVG$Value)}
}

## 285.) Variance Chamber Vacuum in the last 5 layer ####
ChamberVacuumAll$Value <- as.numeric(ChamberVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(ChamberVacuumAll,(Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(filter_CVG$Value) <=1) {AllData$CVG_Var_Last5[i] <- 0}
  else{AllData$CVG_Var_Last5[i] <- var(filter_CVG$Value)}
}


###################

#############################################
####### Backing Vacuum Gauge Feedback #######
#############################################

#### Set Max and Min Thresholds for BVG ####
BVG_MaxThreshold <- as.numeric(max(BackingVacuum718$Value))
BVG_MinThreshold <- as.numeric(min(BackingVacuum718$Value))

## Backing Vacuum > 718 Max ##
AllData$BVG_gteq_max718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BVG_gteq_max718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_gteq_max718_Last5 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_ltmin718_Last <- rep(0, length(AllData$Layer_ID))
AllData$BVG_ltmin718_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_ltmin718_Last5 <- rep(0, length(AllData$Layer_ID))

## 286.) Count how many times the Backing Vacuum greater than or equal to the max of 718 in last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_gtmax718 <- filter(BackingVacuumAll, Value >= BVG_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBVG_gtmax718$Layer_ID) {
    tempfilter_BVG <- filter(tempBVG_gtmax718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BVG_gteq_max718_Last[i] = length(tempfilter_BVG$Layer_ID)}
  else {AllData$BVG_gteq_max718_Last[i] = 0}
}

## 287.) Count how many times the Backing Vacuum greater than or equal to the max of 718 in last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_gtmax718 <- filter(BackingVacuumAll, Value >= BVG_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }

  tempfilterBVG <- filter(tempBVG_gtmax718, Layer_ID %in% Last2Layers)
  AllData$BVG_gteq_max718_Last2[i] <- length(tempfilterBVG$Layer_ID)
}

## 288.) Count how many times the Backing Vacuum greater than or equal to the max of 718 in last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_gtmax718 <- filter(BackingVacuumAll, Value >= BVG_MaxThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBVG <- filter(tempBVG_gtmax718, Layer_ID %in% Last5Layers)
  AllData$BVG_gteq_max718_Last5[i] <- length(tempfilterBVG$Layer_ID)
}

## 289.) Count how many times the Backing Vacuum less than the min of 718 in last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_ltmin718 <- filter(BackingVacuumAll, Value < BVG_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  if(AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1] %in% tempBVG_ltmin718$Layer_ID) {
    tempfilter_BVG <- filter(tempBVG_ltmin718, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
    AllData$BVG_ltmin718_Last[i] = length(tempfilter_BVG$Layer_ID)}
  else {AllData$BVG_ltmin718_Last[i] = 0}
}

## 290.) Count how many times the Backing Vacuum less than the min of 718 in last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_ltmin718 <- filter(BackingVacuumAll, Value < BVG_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }

  tempfilterBVG <- filter(tempBVG_ltmin718, Layer_ID %in% Last2Layers)
  AllData$BVG_ltmin718_Last2[i] <- length(tempfilterBVG$Layer_ID)
}

## 291.) Count how many times the Backing Vacuum less than the min of 718 in last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG_ltmin718 <- filter(BackingVacuumAll, Value < BVG_MinThreshold)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempfilterBVG <- filter(tempBVG_ltmin718, Layer_ID %in% Last5Layers)
  AllData$BVG_ltmin718_Last5[i] <- length(tempfilterBVG$Layer_ID)
}

############
#### Backing Vacuum Max per layer####
AllData$BVG_max_Last <- rep(0, length(AllData$Layer_ID))
AllData$BVG_max_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_max_Last5 <- rep(0, length(AllData$Layer_ID))

## 292.) Record the max Backing Vacuum in the last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_max_Last[i] <- AllData$BVG_max_Last[i-1]}
  else {AllData$BVG_max_Last[i] = max(tempfilter_BVG$Value)}
}

## 293.) Record the max Backing Vacuum in the last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID %in% Last2Layers)
  
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_max_Last2[i] = AllData$BVG_max_Last2[i-1]}
  else{AllData$BVG_max_Last2[i] = max(tempfilter_BVG$Value)}
}

## 294.) Record the max Backing Vacuum in the last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID %in% Last5Layers)
  
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_max_Last5[i] = AllData$BVG_max_Last5[i-1]}
  else{AllData$BVG_max_Last5[i] = max(tempfilter_BVG$Value)}
}

#### Backing Vacuum Min per layer####
AllData$BVG_min_Last <- rep(0, length(AllData$Layer_ID))
AllData$BVG_min_Last2 <- rep(0, length(AllData$Layer_ID))
AllData$BVG_min_Last5 <- rep(0, length(AllData$Layer_ID))

## 295.) Record the min Backing Vacuum in the last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1])
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_min_Last[i] <- AllData$BVG_min_Last[i-1]}
  else {AllData$BVG_min_Last[i] = min(tempfilter_BVG$Value)}
}

## 296.) Record the min Backing Vacuum in the last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID %in% Last2Layers)
  
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_min_Last2[i] = AllData$BVG_min_Last2[i-1]}
  else{AllData$BVG_min_Last2[i] = min(tempfilter_BVG$Value)}
}

## 297.) Record the min Backing Vacuum in the last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  tempfilter_BVG <- filter(BackingVacuumAll, Layer_ID %in% Last5Layers)
  
  if (length(tempfilter_BVG$Layer_ID)==0) {AllData$BVG_min_Last5[i] = AllData$BVG_min_Last5[i-1]}
  else{AllData$BVG_min_Last5[i] = min(tempfilter_BVG$Value)}
}

#### Covariates: Backing Vacuum Mean Value ####
AllData$BVG_Mean_Last <- rep(0,length(AllData$Layer_ID))
AllData$BVG_Mean_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BVG_Mean_Last5 <- rep(0,length(AllData$Layer_ID))

## Initialize ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)
tempBVG <- aggregate(BackingVacuumAll$Value, by = list(BackingVacuumAll$Layer_ID), FUN=mean)
colnames(tempBVG) <- list("Layer_ID", "Mean")

#Separate Build and Layer to sort
tempBVG <- separate(tempBVG, Layer_ID, into = c("Build", "Layer"), remove = FALSE)
tempBVG$Layer <- as.numeric(tempBVG$Layer)
tempBVG <- tempBVG[order(tempBVG$Build, tempBVG$Layer),]

## 298.) Mean Backing Vacuum in the last 1 layer  ####

for(i in 1:length(AllData$Layer_ID)) {
  if (AllData$Layer_ID[i] %in% tempBVG$Layer_ID) {AllData$BVG_Mean_Last[i] <- tempBVG$Mean[match(AllData$Layer_ID[i], tempBVG$Layer_ID)-1]}
  else{AllData$BVG_Mean_Last[i] = AllData$BVG_Mean_Last[i-1]}
}

## 299.) Mean Backing Vacuum in the last 2 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers<-NULL
  for(j in 1:2) {
    Last2Layers <- append(Last2Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempBVG_Last2 <- filter(BackingVacuumAll, Layer_ID %in% Last2Layers)
  if (length(tempBVG_Last2$Layer_ID) == 0) {AllData$BVG_Mean_Last2[i-1]}
  else {AllData$BVG_Mean_Last2[i] <- mean(tempBVG_Last2$Value)}

}

## 300.) Mean Backing Vacuum in the last 5 layers ####
for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers<-NULL
  for(j in 1:5) {
    Last5Layers <- append(Last5Layers, AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j])
  }
  
  tempBVG_Last5 <- filter(BackingVacuumAll, Layer_ID %in% Last5Layers)
  if (length(tempBVG_Last5$Layer_ID) == 0) {AllData$BVG_Mean_Last5[i-1]}
  else {AllData$BVG_Mean_Last5[i] <- mean(tempBVG_Last5$Value)}
  
}

#### Covariates: Backing Vacuum Variance Value ####
AllData$BVG_Var_Last <- rep(0,length(AllData$Layer_ID))
AllData$BVG_Var_Last2 <- rep(0,length(AllData$Layer_ID))
AllData$BVG_Var_Last5 <- rep(0,length(AllData$Layer_ID))

## 301.) Variance Backing Vacuum in the last 1 layer ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  filter_BVG <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-1]))
  if(length(filter_BVG$Value) <=1) {AllData$BVG_Var_Last[i] <- 0}
  else{AllData$BVG_Var_Last[i] <- var(filter_BVG$Value)}
}

## 302.) Variance Backing Vacuum in the last 2 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last2Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:2) {
    filteredLayer <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last2Layers <- rbind(Last2Layers, filteredLayer)
  }
  if(length(filter_BVG$Value) <=1) {AllData$BVG_Var_Last2[i] <- 0}
  else{AllData$BVG_Var_Last2[i] <- var(filter_BVG$Value)}
}

## 303.) Variance Backing Vacuum in the last 5 layers ####
BackingVacuumAll$Value <- as.numeric(BackingVacuumAll$Value)

for(i in 1:length(AllData$Layer_ID)) {
  Last5Layers <- NULL
  filteredLayer <- NULL
  for(j in 1:5) {
    filteredLayer <- filter(BackingVacuumAll, (Layer_ID == AllBuildLayers$Layer_ID[match(AllData$Layer_ID[i], AllBuildLayers$Layer_ID)-j]))
    Last5Layers <- rbind(Last5Layers, filteredLayer)
  }
  if(length(filter_BVG$Value) <=1) {AllData$BVG_Var_Last5[i] <- 0}
  else{AllData$BVG_Var_Last5[i] <- var(filter_BVG$Value)}
}


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

write.csv(AllData,"G:\\My Drive\\SOFS-I\\FinalExperimentation\\ExperimentalRuns_Code\\Results\\Modeling_Dataset_2.csv", row.names = FALSE)

Stop.Time <- Sys.time()
Total.Data.Prep.Time <- difftime(Stop.Time, Start.Time)

save(Total.Data.Prep.Time,file="Total.Data.Prep.Time.RData")