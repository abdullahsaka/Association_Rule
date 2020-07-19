#STEP1 Read the data
library(readxl)
df_groceries <- read_excel("C:/Users/Administrator/Desktop/Association Rule/Groceries.xlsx", 
                        col_types = c("numeric", "text", "text"))
str(df_groceries)
df_sorted <- df_groceries[order(df_groceries$Member_number),]
df_sorted$Member_number <- as.numeric(df_sorted$Member_number) #Lets first make sure that the Member numbers are of numeric data type

str(df_sorted)

#install.packages('plyr', dependencies= TRUE)

library(plyr)
# if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
#   detach(package:dplyr, unload=TRUE)
# }
# library(plyr)
# library(dplyr)

#STEP2  Data cleaning and manipulations using R
#The next step is to actually convert the dataframe into basket format,
#based on the Member_number and Date of transaction

df_itemList <- ddply(df_groceries,c("Member_number","Date"), 
                     function(df1)paste(df1$itemDescription, 
                                        collapse = ","))

#The above function ddply() checks the date and member number 
#and pivots the item descriptions with same date and same member number in one line,separated by commas.

df_itemList$Member_number <- NULL
df_itemList$Date <- NULL

#Rename column headers for ease of use
colnames(df_itemList) <- c("itemList")

write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE) 

#STEP3 Find the association Rules

#install.packages("arules", dependencies=TRUE)
library(arules)

txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)

# Parameters: Transaction file: ItemList.csv
# rm.duplicates : to make sure that we have no duplicate transaction entried
# format : basket (row 1: transaction ids, row 2: list of items)
# sep: separator between items, in this case commas
# cols : column number of transaction IDs

# Quotes are introduced in transactions, which are unnecessary and result in some incorrect results.We must get rid of them.
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)


#run the apriori algorithm on the transactions by specifying minimum values for support and confidence
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"));
basket_rules
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}
inspect(basket_rules)

#Alternative to inspect() is to convert rules to a dataframe and then use View()
df_basket <- as(basket_rules,"data.frame")
View(df_basket)

library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 11))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)


itemFrequencyPlot(txn, topN = 5)


