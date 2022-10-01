## Install packages

install.packages("arules")
install.packages("arulesViz")

## Calling libraries

library(arules)
library(arulesViz)

## Reading data

BNKSERV = read.transactions("BNKSERV.csv", format = "single", cols=c(1,2), sep=",", skip=1, rm.duplicate=TRUE)
inspect(BNKSERV)
str(BNKSERV)
as(BNKSERV, "data.frame")[1:10,]

## Generating rules

rules = apriori(BNKSERV, parameter=list(support=0.1, confidence=0.7, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) 
inspect(rules.sorted)

rules = apriori(BNKSERV, parameter=list(support=0.05, confidence=0.6, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) 
inspect(rules.sorted)

## Generating a subset of rules

rules.sub = subset(rules, subset = rhs %in% "SVG" & lift > 1) 
inspect(rules.sub)

rules.sub = subset(rules, subset = lhs %in% "SVG" & lift > 1) 
inspect(rules.sub)

## Plotting

plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")

## END