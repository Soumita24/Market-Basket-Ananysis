library(arules)
library(arulesViz)

mba_data<-read.transactions("/Users/soumyapaul/Desktop/store_data.csv",format="basket",sep=",",cols=NULL)
dim(mba_data)
inspect(head(mba_data))
head(mba_data)

#Exploratory Data Analysis
itemFrequencyPlot(mba_data,topN=20,type="absolute",horiz=TRUE)
itemFrequencyPlot(mba_data,topN=20,type="relative",horiz=TRUE)

## -------Finding supports of items-------##
head(itemFrequency((mba_data))) #first 6 supports alphabetically
itemFrequency(mba_data)[67] #support of the 67th item alphabetically
support(itemsets(list(c("mineral water","salad")),itemLabels=mba_data),mba_data) #support of mineral water
length(mba_data)  #number of carts
#number of carts times support of item = number of carts with that item
length(mba_data)*c(itemFrequency(mba_data)[67],itemFrequency(mba_data)["mineral water"])

#Apriori Algorithm
data_rules<-apriori(mba_data,parameter= list(supp=0.001,conf=0.8))
plot(data_rules)
inspect(data_rules)

## ------Sorting by quality measure ----------##

inspect(sort(data_rules, by="lift", decreasing=TRUE)[1:4])
inspect(sort(data_rules, by="confidence", decreasing=TRUE)[1:4])
inspect(sort(data_rules, by="support", decreasing=TRUE)[1:4])


## ------Item frequency------- ##
itemFrequency(data)[grep("milk", itemLabels(data))]


## -------Rules--------##
data_rules<- apriori(data, parameter= list(supp = 0.001), conf=0.8, minlen=2, maxlen=4, control=list(verbose=FALSE))

##------looking at the rules----##
options(digits=3)  #changes the number of digits printed to the screen to 3
inspect(data_rules[1:5])


##------Statistical significance of rules-----##
inspect(head(data_rules,7))  #First 7 rules
head(is.significant(data_rules,transactions=data),7)  #First 7 rules

##-----Visualizations---------##

simplerules <-apriori(data,parameter=list(supp=0.001,conf=0.7,maxlen=3),control=list(verbose=F))
simplerules<-sort(simplerules,by="lift")[c(1:13,22)]
plot(simplerules,method="graph",edgeCol="black",cex=0.7,alpha=1)
plot(simplerules,method="graph",engine="htmlwidget")
#pl<-plot(simplerules,methoid="graph",engine="interactive")
#inspect(simplerules)