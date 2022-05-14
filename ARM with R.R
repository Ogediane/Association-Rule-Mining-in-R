#Set working directory

setwd("C:/Users/ogech/OneDrive/Desktop/ASDM Coursework ARM")
getwd()

#Read file into a dataframe

Products <- read.csv("Groceries_dataset.csv", header = T)
View(Products)

# install.packages("arules")
# install.packages("arulesViz")

#load the required libraries

library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)

#Perform data exploration

View(Products)
head(Products)
tail(Products)
summary(Products)
dim(Products)
str(Products)

#Convert Member column from integer to numeric
#Convert Date and ItemDescription columns from character to factor

Products$Member_number <- as.numeric(Products$Member_number)
Products$Date <- as.factor(Products$Date)
Products$itemDescription <- as.factor(Products$itemDescription)

#inspect the structure to ascertain the change

str(Products)

#check for missing data represented by NA

sum(is.na(Products))

#Group all items that were bought together by the same customer on the same date

Supplies <- ddply(Products, c("Member_number","Date"),
                  function(df1)paste(df1$itemDescription,collapse = ","))

head(Supplies,15)
View(Supplies)

#Remove Member_number and Date

Supplies$Member_number <- NULL
Supplies$Date <- NULL
colnames(Supplies) <- c("Supplies")

write.csv(Supplies,"Supplies.csv", quote = FALSE, row.names = TRUE)
head(Supplies)

#Convert CSV file to Basket Format

Trnx = read.transactions(file="Supplies.csv", 
                        rm.duplicates= TRUE, format="basket",sep=",",cols=1);
print(Trnx)

summary(Trnx)

#Remove quotes from transaction using gsub function

Trnx@itemInfo$labels <- gsub("\"","",Trnx@itemInfo$labels)

### APRIORI ALGORITHM ###

#Experimenting with different values

Basket_rulesI <- apriori(Trnx, 
                         parameter = list(maxlen=10, 
                        sup = 0.1, conf = 0.8, target="rules"))

summary(Basket_rulesI)

Basket_rulesII <- apriori(Trnx, 
                         parameter = list(minlen=2, 
                         sup = 0.000001, conf = 0.50, target="rules"))

summary(Basket_rulesII)

inspect(Basket_rulesII[1:15])

plot(Basket_rulesII, jitter = 0)

plot(Basket_rulesII, method = "grouped", control = list(k = 5))

plot(Basket_rulesII, method="paracoord",  control=list(alpha=.5, reorder=TRUE))

Basket_rules <- apriori(Trnx, parameter = list(minlen=2, 
                        sup = 0.001, conf = 0.05, target="rules"))

summary(Basket_rules)

Basket_rules1 <- apriori(Trnx, parameter = list(minlen=3, 
                                               sup = 0.001, conf = 0.05, target="rules"))

summary(Basket_rules1)

#Check the total rules generated

print(length(Basket_rules))

print(Basket_rules1)

summary(Basket_rules)

summary(Basket_rules1)

#Inspecting the basket rules

inspect(Basket_rulesII[1:15])
inspect(Basket_rules1[1:15])
inspect(Basket_rules[1:15])

#Visualizing the Association Rules

top10subRules <- head(Basket_rules, n = 10, by = "confidence")

plot(top10subRules, method = "two-key plot")
top10subRules <- head(Basket_rules1, n = 10, by = "confidence", jitter=0)
top10subRules <- head(Basket_rulesII, n = 10, by = "confidence", jitter=0)


plot(Basket_rules,method="two-key plot")
plot(Basket_rules1,method="two-key plot")
plot(Basket_rulesII,method="two-key plot", jitter=0)

plot(Basket_rules)
plot(Basket_rules1)

plot(Basket_rules, jitter = 0)
plot(Basket_rules1, jitter = 0)
plot(Basket_rulesII, jitter = 0)

{
  p <- plot(Basket_rules, engine = "html")
  htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
  browseURL("arules.html")
}

{
  p <- plot(Basket_rules1, engine = "html")
  htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
  browseURL("arules.html")
}


plot(Basket_rules, method = "grouped", control = list(k = 5))
plot(Basket_rules1, method = "grouped", control = list(k = 5))
plot(Basket_rulesII, method = "grouped", control = list(k = 5))

plot(Basket_rules, method="graph", control=list(type="items"))
plot(Basket_rules1, method="graph", control=list(type="items"))
plot(Basket_rulesII, method="graph", control=list(type="items"))

plot(Basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

#Graph of first 15 rules

plot(Basket_rules[1:15], method="graph")

#Graph of first 30 rules

plot(Basket_rules[1:30], method="graph")

#Graph of first 50 rules

plot(Basket_rules[1:50], method="graph")

#plot and visualize the parallel coordinates 

plot(Basket_rules[1:15], method="paracoord")
plot(Basket_rules[1:20], method="paracoord")
plot(Basket_rules[1:30], method="paracoord")
plot(Basket_rules[1:50], method="paracoord")

#visualize the most frequently bought products

itemFrequencyPlot(Trnx, topN = 10)

itemFrequencyPlot(Trnx, topN = 10, col ="maroon")



#### SHINY APPLICATION ####

# install.packages("Shiny")
library(arulesViz)

ruleExplorer(Basket_rules)
ruleExplorer(Basket_rules1)