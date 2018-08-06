#Read the Data.csv file
setwd('C:/.............')

#Read LasgnaTriers csv File
FoodTriers <- read.csv("Data.csv",header=TRUE)


#Calculate length of outcomes and percentile value for 99th %ile
len <- length(FoodTriers$Income)
quant <-quantile(FoodTriers$Income,0.99)
print(paste("99th percentile score is", quant))

#Conditional 'for loop' to identify those rare events for which income exceeds 99th%ile.
#Mark rare events as TRUE and rest as FALSE
#Question A and B
#Calculating the rarities and appending a new variable rare to dataset
for (i in 1: len){
  if (FoodTriers$Income[i] > quant)
  {
    FoodTriers$Rare[i]=TRUE
    }else
      {
  FoodTriers$Rare[i]=FALSE
  }
  
}


#Append the new variable rare to dataset and calculate the number of rare events
countRareTrue <- nrow(subset(FoodTriers,FoodTriers$Rare==TRUE))
print(paste("Number of rarities is ",countRareTrue))
countRareFalse <-len-countRareTrue


print("**************************Question 6.c***********************************")
#Question 6.c:Generate N=1000 random samples of size = 40.Within each sample,count the rare customers and store this count as
#this count in a vector named rarities

# Sample parameters
sampleSize <- 40
numSamples <- 1000

samples <- data.frame(matrix(nrow = sampleSize,ncol = numSamples))

colnames(samples)<-  paste("Sample",
                           c(1:numSamples))
#What to sample from?-rare event(True/false)
raritySample <- FoodTriers$Rare


counts <- vector(len=numSamples)

#Create 1000 samples of sample size 40

set.seed(1000)

for(i in 1:numSamples) {
  mySample <- raritySample[sample(length(raritySample),
                              sampleSize)]
 counts[i] <- length(subset(mySample, 
                            mySample == 'TRUE'))
 
  samples[ , i] <- mySample
  

}

# Count the rare customers and store the count in a vector called rarities
Rarities <-counts


cat("vector caontaining count of the rare event is Rarities")
print(Rarities)

#*********************************************************************************************************************


#Calculate the levels of Counts by convering it to a factor
count_factor <-as.factor(counts)
level <- length(levels(count_factor))-1

#Create an empty data frame called frequency table to tabulate the outcome vs number of samples data
Frequency_table <- data.frame()
#"For loop" to save outcomes and number of samples in data frame

for (i in 0:level){

numRare <- length(subset(counts, 
                                counts == i))
Frequency_table = rbind(Frequency_table, data.frame("X" = i, "No of Samples" = numRare))

}

print("Print frequency table")
print(Frequency_table)

#Probability of success,p=0.001(as we are looking for rare evets with income exceeding 99th%ile)

#Normalise the frequency table into probability distribution

#calculate probability distribution 
for (i in 1:length(Frequency_table$X)){
  
  Frequency_table$pDist[i] <- (Frequency_table$No.of.Samples[i]/numSamples)
 
}


#Calculate Expected mean and variance from above probability distribuion

# Expected mean
ExpectedMean <- weighted.mean(Frequency_table$X,Frequency_table$pDist)
print(paste("Expected mean is ",ExpectedMean))



#Expected variance


for (i in 1:length(Frequency_table$X)){
  Frequency_table$SquaredMeanDist[i] <-  (Frequency_table$X[i]-ExpectedMean)^2
}
ExpectedVar <- weighted.mean(Frequency_table$SquaredMeanDist,Frequency_table$pDist)
print(paste("Expected variance is ",ExpectedVar))

#***************************************************************************************************************

#Assumption:expected value and variance as approximately similar to np and np(1-p) and n=40.Calculate p
#p is success probability

#Equate expected value with np and calculate p
p1 <- ExpectedMean/sampleSize
print(paste("probability is ",p1))

#Lets Model X with Binomial Distribution.Let's calculate PMF

for (i in 1:length(Frequency_table$X)){
  
  Frequency_table$Binom_PMF[i] <- dbinom(Frequency_table$X[i],sampleSize,p=p1)
  Frequency_table$PredictedCount_Binom[i] <- round(sum(numSamples*Frequency_table$Binom_PMF[i]),digits = 3)
  
}

#********************************************************************************************************************

print("***********************************Question 6.g*****************************************")
#Poisson distribution

#Poisson PMF
for (i in 1:length(Frequency_table$X)){
  Frequency_table$POISSON_PMF[i] <- dpois(Frequency_table$X[i],lambda = ExpectedMean,log=FALSE)
  Frequency_table$PredictedCount_Poisson[i] <- round( sum(numSamples*Frequency_table$POISSON_PMF[i]),digits = 3)
  
}

print(Frequency_table)

#**********************************************************************************************************

#Chi-square goodness of fit test
#Binomial Model
observed <- Frequency_table$Binom_PMF
expected <- Frequency_table$pDist
print("chi-square for binomal model")
print(chisq.test(x=observed,p=expected))


#Poisson
print("chi-square for poisson model")

print(chisq.test(x=Frequency_table$POISSON_PMF,p=Frequency_table$pDist))


print("Observation:p-value is similar in both the Binomial and poisson distribution:p-value=1")
print("#As the p value is similar, we can retain the null hypothesis and conclude that there is a no significant difference between the observed and the expected frequenc")

