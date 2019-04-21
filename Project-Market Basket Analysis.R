##Install and load the required packages and library##
install.packages(“plyr”)
library(plyr)
install.packages(“arules”)
library(arules)
library(arulesViz)
install.packages("ggplot2")
library(ggplot2)

##Exploratory Analysis##
Setwd(“C:/Users/kmbl82822/desktop/IIMA/instacart_2017_05_01”)
Histo <- read.csv(“Query.csv”, header = T)
Days <- read.csv(“orders.csv”, header=T)
qplot(Histo$NOCs, geom=”histrogram”, bins=97)
qplot(as.factor(days$order_dow))
qplot(as.factor(day$order_hour_Of_day))
qplot(day$days_since_prior_order, data=days, bins= 20, na.rm = TRUE)]
f <- read.csv("C:/Users/kmbl82822/Desktop/IIMA/UserIds4.csv")
g <- read.csv("C:/Users/kmbl82822/Desktop/IIMA/product_freq_yogurt.csv")
qplot(f$Perc_NODYs, geom="histogram", bins = 50)
qplot(as.factor(f$order_dow))
qplot(as.factor(f$order_hour_of_day))
qplot(as.factor(f$days_since_prior_order))

##Eclat Algorithm##
instacart<-read.csv("C:/Users/kmbl82822/Desktop/IIMA/instacart_2017_05_01 - Copy/PIVOT_AISLE.csv", header=T)
instacart_final = instacart[instacart$order_id <= 500000, ]
write.csv(instacart_final,"instacart_final.csv", quote = FALSE, row.names = TRUE)
lookup2 <- read.csv("C:/Users/kmbl82822/Desktop/IIMA/instacart_2017_05_01/aisles.csv")
instacart_final$item_name = lookup2$aisle[match(instacart_final$aisle_id, lookup2$aisle_id)]
instacart_final$aisle_id <- NULL
instacart_final$order_id <- as.numeric(instacart_final$order_id)
instacart_final$item_name <- as.factor(instacart_final$item_name)
df_itemList <- ddply(instacart_final, c("order_id"), function(df1)paste(df1$item_name,collapse = ","))
df_itemList$order_id <- NULL
colnames(df_itemList) <- c("itemList")
View(df_itemList)
write.csv(df_itemList,"ItemList5.csv", quote = FALSE, row.names = TRUE)	
txn = read.transactions(file="ItemList5.csv", rm.duplicates= FALSE, format="basket",sep=",",cols=1)
frequent <- eclat(data = txn, parameter = list(support = 0.01,minlen=3))
df_eclat <- as(frequent,"data.frame")
write.csv(df_eclat,"eclat4.csv")


##Apriori Algorithm##
basket_rules <- apriori(txn,parameter = list(minlen=1,maxlen=2,sup = 0.0001, conf = 0.50, target="rules"),appearance = list(rhs = "yogurt"))
inspect(basket_rules)
inspect(sort(basket_rules, by = 'support')[1:5])
write.csv(df_basket,"rules7.csv")
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)
itemFrequencyPlot(txn,topN=20, type="absolute")
itemFrequencyPlot(txn,topN=20, type="relative")
products <- read.csv("C:/Users/kmbl82822/Desktop/IIMA/instacart_2017_05_01/order_products_prior.csv", header=T)
products$add_to_cart_order <- NULL
products$reordered <- NULL
products_final = products[products$order_id <= 500000, ]
lookup3 <- read.csv("C:/Users/kmbl82822/Desktop/IIMA/instacart_2017_05_01/products.csv",header = T)
products_final$item_name = lookup3$product_name[match(products_final$product_id, lookup3$product_id)]
products_final$product_id <- NULL
products_final$order_id <- as.numeric(products_final$order_id)
products_final$product_id <- as.numeric(products_final$product_id)
df_productList <- ddply(products_final, c("order_id"), function(df1)paste(df1$product_id,collapse = ","))
df_productList$order_id <- NULL
colnames(df_productList) <- c("itemList")
View(df_productList)
write.csv(df_productList,"Product_list.csv", quote = FALSE, row.names = TRUE)
txn2 = read.transactions(file="Product_list.csv", rm.duplicates= FALSE, format="basket",sep=",",cols=1)
basket2 <- apriori(txn2,parameter = list(minlen=1,maxlen=4,sup = 0.0001, conf = 0.50, target="rules"))
df_basket2 <- as(basket2,"data.frame")
View(df_basket2)
write.csv(df_basket2,"total_rule.csv")
