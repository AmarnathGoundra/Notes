# Example and code from:https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/

library(mlbench)
data(HouseVotes84)  # This dataset is contained in mlbench library

#barplots for specific issue
plot(as.factor(HouseVotes84[,2]))
title(main='Votes cast for issue 1', xlab="vote", ylab= "Num reps" )

#by party
Repub <-  HouseVotes84$Class=="republican"
Democrat <- HouseVotes84$Class=="democrat"

plot(as.factor(HouseVotes84[Repub,2]))
title(main="Republican votes cast for issue 1", xlab="vote", ylab="Num reps")
plot(as.factor(HouseVotes84[Democrat,2]))
title(main="Democrat votes cast for issue 1", xlab="vote", ylab="Num reps")


# We have a lot of NA. We are going to impute the values

#Functions needed for imputation

#function to return number of NAs by vote and class (democrat or republican)
na_by_col_class <- function (col,cls){return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))}


#function to compute the conditional probability that a member of a party will cast a "yes" vote for
#a particular issue. The probability is based on all members of the party who #actually cast a vote on the issue (ignores NAs).
p_y_col_class <- function(col,cls){
  sum_y<-sum(HouseVotes84[,col]=="y" & HouseVotes84$Class==cls,na.rm = TRUE)
  sum_n<-sum(HouseVotes84[,col]=="n" & HouseVotes84$Class==cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))
}


#Check the prob of yes vote by a democrat in issue 5
p_y_col_class(5,"democrat")

#impute missing values.
# If the republican congresman didn't vote, then we are allocating 'y' or 'n' based on if their
# party voted 'y' or 'n'
for (i in 2:ncol(HouseVotes84)) {
  if(sum(is.na(HouseVotes84[,i])>0)) {
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="democrat",arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="republican",arr.ind = TRUE)
    HouseVotes84[c1,i] <-
      ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    HouseVotes84[c2,i] <-
      ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")}
}


#divide into test and training sets
#create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)

#get col number of train / test indicator column (needed later)
trainColNum <- grep("train",names(HouseVotes84))
#separate training and test sets and remove training column before modeling
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]


#load e1071 library and invoke naiveBayes method
library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)

nb_model


#Lets test the model
nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])

#fraction of correct predictions
mean(nb_test_predict==testHouseVotes84$Class)

#confusion matrix
table(pred=nb_test_predict,true=testHouseVotes84$Class)

