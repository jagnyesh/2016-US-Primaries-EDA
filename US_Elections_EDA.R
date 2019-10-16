## Engineering Analytics I ##
## 2016 US Elections ##
## Team: 6


## Libraries

#installing
#install.packages("randomForest")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("ggplot2")

#loading
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(stringr)
library(dplyr)
library(ggplot2)

## Data

#reading county_facts,cfacts_dict and results
cfacts = read.csv("county_facts.csv", na.strings=c("", "NA"," "), stringsAsFactors = FALSE)
cfacts_dict = read.csv("county_facts_dictionary.csv")
results = read.csv("primary_results.csv", na.strings=c("", "NA"," "), stringsAsFactors = FALSE)

#structure of both dataframes
str(cfacts)
str(results)

#head & tail of both the dataframes
head(cfacts)
tail(cfacts)
head(results)
tail(results)

## Preparing the Data

# Missing Values
#omitting the missing values
cfacts <- na.omit(cfacts)
results <- na.omit(results)

# Renaming Variable
#since area_name from cfacts and county from results, renaming area_name
colnames(cfacts)[2] <- "county"

# Removing by simple inspection
#since values of county in AK are different in cfacts and results
#removing it from cfacts by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "AK")
results <- subset(results, results$state_abbreviation != "AK")

#since values of state MN are not present in results
#removing it from cfacts by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "MN")

#since values of state DC are not present in results
#removing it from cfacts by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "DC")

#since values of state NH are not present in results
#removing it from cfacts by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "NH")

#since values of state and fips for KS are not related in cfacts and results
#removing it from both by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "KS")
results <- subset(results, results$state_abbreviation != "KS")

#since values of state and fips for ND are not related in cfacts and results
#removing it from both by subsetting
cfacts <- subset(cfacts, cfacts$state_abbreviation != "ND")
results <- subset(results, results$state_abbreviation != "ND")

# Lower Case
#converting county in both dataframes to lower case
cfacts$county <- tolower(cfacts$county)
results$county <- tolower(results$county)
tail(results$county)

# Splitting variable to simplify
##we'll split and remove the words "county" and "parish" from variable county in cfacts

#creating a temporary variable to copy values from "county"
testvar <- str_split_fixed(cfacts$county, " ", 5)

#converting to dataframe
testvar <- as.data.frame(testvar)

##let's look at column names of test variable
colnames(testvar)

#removing factors from testvar individially
testvar$V1 <- as.character(testvar$V1)
testvar$V2 <- as.character(testvar$V2)
testvar$V3 <- as.character(testvar$V3)
testvar$V4 <- as.character(testvar$V4)
testvar$V5 <- as.character(testvar$V5)

#removing "county" & "parish" from any columns within testvar
for( i in 1:nrow(testvar)) {
  if(testvar$V5[i] == "county" | testvar$V5[i] == "parish"){
    testvar$V5[i] = ""    
  }
  if(testvar$V4[i] == "county" | testvar$V4[i] == "parish"){
    testvar$V4[i] = ""
  }
  if(testvar$V3[i] == "county" | testvar$V3[i] == "parish"){
    testvar$V3[i] = ""
  }
  if(testvar$V2[i] == "county" | testvar$V2[i] == "parish"){
    testvar$V2[i] = ""
  }
}

#concatenating words into the first row
#combining row 1 and 2
for(i in 1:nrow(testvar)){
  if(!testvar$V2[i] == ""){
    testvar$V1[i] = str_c(testvar$V1[i], testvar$V2[i], sep = " ")
  }
}
#combining row 1 and 3
for(i in 1:nrow(testvar)){
  if(!testvar$V3[i] == ""){
    testvar$V1[i] = str_c(testvar$V1[i], testvar$V3[i], sep = " ")
  }
}
#combining row 1 and 4
for(i in 1:nrow(testvar)){
  if(!testvar$V4[i] == ""){
    testvar$V1[i] = str_c(testvar$V1[i], testvar$V4[i], sep = " ")
  }
}
#combining row 1 and 5
for(i in 1:nrow(testvar)){
  if(!testvar$V5[i] == ""){
    testvar$V1[i] = str_c(testvar$V1[i], testvar$V5[i], sep = " ")
  }
}

#removing redundancies
testvar <- testvar$V1

#since we know that counties in VA have "city" in cfacts not in results
#they have to be substringed or replaced manually
#converting testvar to DF
testvardf <- as.data.frame(testvar)
testvardf$testvar <- as.character(testvardf$testvar)
testvardf$state <- cfacts$state_abbreviation
for(i in 1:nrow(testvardf)){
  if(testvardf$state[i] == "VA"){
    if(strsplit(testvardf$testvar[i], " ")[[1]][2] == "city" && !is.na(strsplit(testvardf$testvar[i], " ")[[1]][2])){
      testvardf$testvar[i] <- (strsplit(testvardf$testvar[i], " ")[[1]][1])
    }
    else if(strsplit(testvardf$testvar[i], " ")[[1]][3] == "city" && !is.na(strsplit(testvardf$testvar[i], " ")[[1]][3])) {
      testvardf$testvar[i] <- paste(strsplit(testvardf$testvar[i], " ")[[1]][1], strsplit(testvardf$testvar[i], " ")[[1]][2], sep = " ")  
    }
    
  }
}

#changing "idaho" to "idaho county" in cfacts to match data format in results
pos = which(testvardf$testvar == "idaho")
testvardf$testvar[pos] = "idaho county"

#changing "st. louis" to "st. louis county" in cfacts to match data format in results
pos = which(testvardf$testvar == "st. louis")
testvardf$testvar[pos] = "st. louis county"

#changing "st. lawrence" to "saint lawrence" in cfacts to match data format in results
pos = which(testvardf$testvar == "st. lawrence")
testvardf$testvar[pos] = "saint lawrence"

#replacing "county" vales by testvar values in cfacts
cfacts$county <- testvardf$testvar

#changing values in cfacts to match those in results, based on fips
pos = which(cfacts$fips == 51036)
cfacts$county[pos] = "charles city"
pos = which(cfacts$fips == 51095)
cfacts$county[pos] = "james city"
pos = which(cfacts$fips == 51600)
cfacts$county[pos] = "fairfax city"
pos = which(cfacts$fips == 51620)
cfacts$county[pos] = "franklin city"
pos = which(cfacts$fips == 51760)
cfacts$county[pos] = "richmond city"
pos = which(cfacts$fips == 51770)
cfacts$county[pos] = "roanoke city"
pos = which(cfacts$fips == 35011)
cfacts$county[pos] = "debaca"
pos = which(cfacts$fips == 17039)
cfacts$county[pos] = "dewitt"
pos = which(cfacts$fips == 22031)
cfacts$county[pos] = "desoto"
pos = which(cfacts$fips == 5123)
cfacts$county[pos] = "saint francis"
pos = which(cfacts$fips == 48285)
cfacts$county[pos] = "la vaca"
pos = which(cfacts$fips == 40079)
cfacts$county[pos] = "leflore"
pos = which(cfacts$fips == 17085)
cfacts$county[pos] = "jodaviess"

#we cannot refine the cfacts data anymore

#refining results to remove values "No Preference" & "Uncommited" from candidate
results<- subset(results, !candidate %in% c(" No Preference", " Uncommitted"))

#making column names understandable for both dataframes
#cfacts
colnames(cfacts)
colnames(cfacts)[3] <- "state"
colnames(cfacts)[4] <- "pop2014"
colnames(cfacts)[7] <- "pop2010"
colnames(cfacts)[11] <- "female"
colnames(cfacts)[13] <- "black"
colnames(cfacts)[14] <- "alaskan"
colnames(cfacts)[15] <- "asian"
colnames(cfacts)[16] <- "islander"
colnames(cfacts)[18] <- "hisplat"
colnames(cfacts)[19] <- "white"
colnames(cfacts)[23] <- "eduhigh"
colnames(cfacts)[24] <- "educollege"
colnames(cfacts)[25] <- "veterans"
colnames(cfacts)[34] <- "income"
colnames(cfacts)[54] <- "density"

#results
colnames(results)
colnames(results)[1] = "statename"
colnames(results)[2] = "state"
colnames(results)[8] = "frvotes"

#selecting meaningful columns from cfacts
cfacts <- subset(cfacts, select = c(fips, county, state, pop2014, pop2010, female, black, alaskan, asian, islander, hisplat, white, eduhigh, educollege, veterans, income, density))

#all columns in results are meaningful, no name changes
##the data has been cleaned and prepared
#for exploring the data, we need to merge the data
#merge the data by county and state, we know that there are mismatches(895)
#hence missing values will be created
#merging the data into a new dataframe "mergedata"

mergedata <- merge(x = results, y = cfacts, by=c("county","state"), all.x = TRUE)

#looking at the missing Y values for all X
sapply(mergedata, function(x) sum(is.na(x)))

#ommiting missing data
mergedata <- na.omit(mergedata)

#removing duplicate column "fips" to simplify merge data
mergedata <- subset(mergedata, select = -c(fips.y))

#renaming the exising fips column
colnames(mergedata)[4] <- "fips"
##  The data is now ready.

# Removing testvardf, pos and i from the workspace
rm(testvardf, i, pos)
# Working with candidates
rev(sort(table(mergedata$candidate)))

# There are 14 primary contenders
# Hillary Clinton, Bernie Sanders, Ted Cruz, John Kasich, Donald Trump and Marco Rubio
# Now we will explore results.

# Visualizing Fraction Votes
mergedata$candidate <- factor(mergedata$candidate, levels=rev(sort(unique(mergedata$candidate))))
library(viridisLite)
ggplot(data = mergedata, aes(y = candidate, x = state)) +
  geom_tile(aes(fill = frvotes)) +
  scale_fill_gradientn(colours = viridis(256)) +
  xlab("States") +
  ylab("Candidates") +
  guides(fill = guide_legend(title="Fraction Votes\nPercentage")) +
  ggtitle("2016 US Election Primary Results")


# Winners for different counties
CheckWinnerPeople <- function (data)
{
  winnerPeople <- data[1,]
  for (i in unique(data$state))
  {
    dfSt <- data[which(data$state == i), ]
    for (j in unique(dfSt$county))
    {
      dfStCo <- dfSt[which(dfSt$county == j),]
      sortDfStCoID <- order(dfStCo$votes, decreasing = T)
      ## calculate vote ratio
      ## the frvotes is useless, it means the fration of votes for specifir candidate
      ## in specific party on specific county
      dfStCo$votes <- dfStCo$votes/sum(dfStCo$votes)
      winnerPeople <- rbind(winnerPeople, dfStCo[sortDfStCoID[1],])
    }
  }
  return(winnerPeople[-1, ])
}

#creating a separate DF for winners
winnerdata <- CheckWinnerPeople(mergedata)

# Exploring county winners
table(winnerdata$candidate)
#removing unnecessary candidates
winnerdata$candidate <- factor(winnerdata$candidate)
#plotting
barplot(table(winnerdata$candidate), main = "County vs Top Candidate", xlab = "Top Contenders", ylab = "No. of Counties", col=c("darkblue","red"))

# Creating the predictor variable "winner"
testdf <- mergedata
testdf$candidate <- as.character(testdf$candidate)
windf <- winnerdata
windf$winner <- 1
testdf<- merge(x = testdf, y= windf, by = c("county","state","statename","party","candidate"), all.x = TRUE)
testdf$winner[is.na(testdf$winner)] <- 0

#now the testdf has redundant variables, removing them
testdf <- subset(testdf, select = -c(fips.y, votes.y, frvotes.y, pop2014.y, pop2010.y, female.y, black.y, asian.y, alaskan.y, islander.y, hisplat.y, white.y, eduhigh.y, educollege.y, veterans.y, income.y, density.y))


# Modelling CART

#setting seed
set.seed(123)
split = sample.split(testdf$winner, SplitRatio = 0.6)
mergeTrain = subset(testdf, split == TRUE)
mergeTest = subset(testdf, split == FALSE)

#building classification and regressiom tree model
set.seed(123)
mergeCART = rpart(winner ~ party + candidate + pop2014.x + pop2010.x + female.x + black.x + white.x, hisplat.x + eduhigh.x + educollege.x + income.x, data = mergeTrain, method = "class")
prp(mergeCART)

#evaluating performace
predictmergeCART = predict(mergeCART, newdata = mergeTest, type = "class")
table(mergeTest$winner, predictmergeCART)

cartacc = (5520+618)/(5520+618+514+366)
cartacc
#accuracy is 87.46%

## Modelling Random Forest

set.seed(123)
mergeRf = randomForest(winner ~ income.x + hisplat.x + white.x + educollege.x + density.x, data = mergeTrain)
#predicting
predictRf = predict(mergeRf, newdata = mergeTest)
#tablulating
table(mergeTest$winner, predictRf >0.5)
rfacc =(5778+1)/(1131+108+1+5778)
rfacc
#accuracy is 82.34%















# Mapping the Data
equation <- (candidate ~ eduhigh + educollege + hisplat + black + asian 
             + white + female + income + density)
bmodel <- glm(formula = equation, family = binomial(link = "logit"),
              data = winnerdata)


# high education
scatterplot = ggplot(bmodel, aes(x =eduhigh , y = candidate))
scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Candidate influence factors with high education")

#educollege
ggplot(bmodel, aes(x = educollege, y = candidate, color =candidate )) + geom_point()

#black
scatterplot = ggplot(bmodel, aes(x =black , y = candidate))
scatterplot + geom_point(colour = "red", size = 3, shape = 10) + ggtitle("Candidate influence factors with black")

#asian
ggplot(bmodel, aes(x = asian, y = candidate)) + geom_point()

#white
ggplot(bmodel, aes(x = white, y = candidate, color =candidate )) + geom_point()

#female
ggplot(bmodel, aes(x = female, y = candidate, color =candidate )) + geom_point()

#income
ggplot(bmodel, aes(x = income, y = candidate, color =candidate )) + geom_point()

#density
ggplot(bmodel, aes(x = density, y = candidate, color =candidate )) + geom_point()


