??arules
??arulesViz
install.packages("arules")
install.packages("arulesViz")
install.packages("tweenr")
install.packages("Matrix")
install.packages("graphlayouts")

library("arules")
library("arulesViz")

EITXN <- read.transactions('ElectronidexTransactions2017.csv', format = 'basket', sep=',')

EITXN

inspect(EITXN)
length(EITXN)
size(EITXN)
LIST(EITXN)
itemLabels(EITXN)
summary(EITXN)

## iMac is the most frequent item, followed by HP Laptop, then CYBERPOWER Gamer Desktop
## Customers purchase 1 product most per transactions, and 25, 26, 29, and 30 all had 1 txn
## AVG is 4.384 products in a single TXN

itemFrequencyPlot(EITXN, topN=25)

itemFrequencyPlot(EITXN, topN=25, type='absolute')

image(sample(EITXN, 300))

image(EITXN[1:200, 1:25])

Rules1 <- apriori(EITXN, parameter = list(supp=0.001, conf = 0.8))
inspect(Rules1)
summary(Rules1)
## 635 Rules, Minlen 1, Maxlen 10. 0.1% of the itemsets of the data frame fall into the 80% confidence

Rules2 <- apriori(EITXN, parameter = list(supp=0.001, conf = 0.9, minlen = 1))
inspect(Rules2)
summary(Rules2)
## Increasing CONF, adding MinLen to set min number of items needed in LHS
## 197 Rules, Minlen 1, Maxlen 10. High confidence means strong rule

Rules3 <- apriori(EITXN, parameter = list(supp=0.001, conf = 0.99))
inspect(R3Sort)
summary(Rules3)
## Increasing CONF, decreasing Support
## 42 Rules
## Highest Confidence at 99%, Support at 0.1%
R3Sort <- sort(Rules3, decreasing = TRUE, by = 'support')
inspect(R3Sort[1:10])


Rules4 <- apriori(EITXN, parameter = list(supp=0.05, conf = 0.1, minlen = 1))
inspect(Rules4)
summary(Rules4)
## Increasing Support, decreasing Con. MinLen set to 1
## 10 Rules
inspect(sort(Rules4, decreasing = TRUE, by = 'support'))


Rules5 <- apriori(EITXN, parameter = list(supp=0.2, conf = 0.2))
inspect(Rules5)
summary(Rules5)
## 1 Rule
## Highest Support with 20% Support, Confidence at 20%


Rules6 <- apriori(EITXN, parameter = list(supp=0.001, conf = 0.1))
R6Sort <- sort(Rules6, decreasing = FALSE, by = 'lift')
inspect(R6Sort[1:20])


Rules7 <- apriori(EITXN, parameter = list(supp=0.001, conf = 0.6))
inspect(Rules7)
summary(Rules7)
## 3969 Rules
## Highest amount so far!
R7Sort <- sort(Rules7, decreasing = TRUE, by = 'lift')
inspect(R7Sort[1:20])

Rules8 <- apriori(EITXN, parameter = list(supp=0.005, conf = 0.6))
inspect(Rules8)
summary(Rules8)
## 28 Rules
R8Sort <- sort(Rules8, decreasing = TRUE, by = 'lift')
inspect(R8Sort[1:20])

Rules9 <- apriori(EITXN, parameter = list(supp=0.005, conf = 0.5))
inspect(Rules9)
summary(Rules9)
## 151 Rules
R9Sort <- sort(Rules9, decreasing = TRUE, by = 'lift')
inspect(R9Sort[1:20])


Rules10 <- apriori(EITXN, parameter = list(supp=0.002, conf = 0.7))
inspect(Rules10)
summary(Rules10)
## 135 Rules
R10Sort <- sort(Rules10, decreasing = TRUE, by = 'lift')
inspect(R10Sort[1:20])


ItemRules <- subset(Rules10, items %in% "iMac")
inspect(ItemRules)

?plot
plot(Rules10[1:20], method ="graph", control=list(type="items"))
plot(Rules10)
plot(Rules10, jitter = 0)
plot(Rules10, measure = c("support","lift"), shading="confidence")
