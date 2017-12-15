                                    ## Library declarations. ##
library(readxl)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(lubridate)
library(data.table)
library(arules)
library(arulesViz)
options(max.print = 50000)

                                    ## Downloading the dataset. ##

fileUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"
download.file(fileUrl, destfile = "./data/Online Retail.xlsx", method = "curl")
dateDownloaded <- date()

                                    ## Reading the dataset. ##

Online_Retail <- read_excel("~/R_3.4.2/data/Online Retail.xlsx")
View(Online_Retail)

# 1. Dataset size and dimensions.
print(object.size(Online_Retail), units = "Mb")
dim(Online_Retail)

                                    ## Unclean (Raw) dataset. ##

# 1. Check for the missingness indicator.
colSums(is.na(Online_Retail))

# 2. Verify the correctness of the above for individual variables.
table(Online_Retail$Description, useNA = "ifany")
table(Online_Retail$CustomerID, useNA = "ifany")
table(Online_Retail$Country, useNA = "ifany")

Online_Retail1 <- Online_Retail
Online_Retail1$wrongQuantz = ifelse(Online_Retail$Quantity < 0 , TRUE, FALSE)
table(Online_Retail1$wrongQuantz)

# 3. Unclean data analysis.
summary(Online_Retail)    

                                    ## Tidying the dataset. ##

OnlineRetail_df = as.data.frame(Online_Retail)
CleanOnlineRetail_df <- OnlineRetail_df[complete.cases(OnlineRetail_df), ]
CleanOnlineRetail1_df <- CleanOnlineRetail_df

# 1. Cleaning unspecified countries.
CleanOnlineRetail2_df <- CleanOnlineRetail1_df[!grepl("Unspecified", CleanOnlineRetail1_df$Country),]

# 2. Cleaning irrelevant stock codes.
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("BANK CHARGES", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("C2", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("CRUK", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("D", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("DOT", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("M", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("PADS", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail2_df <- CleanOnlineRetail2_df[!grepl("POST", CleanOnlineRetail2_df$StockCode),]
CleanOnlineRetail3_df <- CleanOnlineRetail2_df

# 3. Cleaning negative quantities.
CleanOnlineRetail4_df <- CleanOnlineRetail3_df[CleanOnlineRetail3_df$Quantity > 0, ]

# 4. Cleaning zero unit prices.
CleanOnlineRetail4_df <- CleanOnlineRetail4_df[CleanOnlineRetail4_df$UnitPrice > 0, ]

# 5. No changes with Invoice number, Invoice date and Description.

colSums(is.na(CleanOnlineRetail4_df))

                                      ## Downstream Analyses. ##

# 1. Probability distributions of Customers.                                    
quantile(CleanOnlineRetail4_df$CustomerID, na.rm = TRUE)

CleanOnlineRetail4_df$custIds = cut2(CleanOnlineRetail4_df$CustomerID, g = 5)
table(CleanOnlineRetail4_df$custIds)
table(CleanOnlineRetail4_df$custIds, CleanOnlineRetail4_df$CustomerID)

# 2. Calculate the number of customers in the dataset.
CleanOnlineRetail4_df$zcf  <- factor(CleanOnlineRetail4_df$CustomerID)
CleanOnlineRetail4_df$zcf  

# 3. Find the country which accounts for maximum purchases.
Coccurences <- table(unlist(CleanOnlineRetail4_df$Country))
Coccurences1 <- as.data.frame(Coccurences)
Coccurences1[order(-Coccurences1$Freq),]

# 4. Calculate per Annum Sales.
CleanOnlineRetail4_df <- mutate(CleanOnlineRetail4_df, Price = Quantity * UnitPrice)
head(select(CleanOnlineRetail4_df, Quantity, UnitPrice, Price))

sum(CleanOnlineRetail4_df$Price)
mean(CleanOnlineRetail4_df$Price)

# 5. Retrieve the day of the year when the sale was maximum.
max(CleanOnlineRetail4_df$Price)
CleanOnlineRetail4_df[CleanOnlineRetail4_df$Price == max(CleanOnlineRetail4_df$Price),]

day = ymd_hms("2011-12-09 09:15:00")
wday(day)

# 6. Calculate the sales per month.
# 6.1 Group by year.
CleanOnlineRetail4_df <- mutate(CleanOnlineRetail4_df, year = as.POSIXlt(InvoiceDate)$year + 1900)
years <- group_by(CleanOnlineRetail4_df, year)
head(select(years, year, CustomerID, Description, Price, Country))
tail(select(years, year, CustomerID, Description, Price, Country))

# 6.2 Group by months.
CleanOnlineRetail4_df <- mutate(CleanOnlineRetail4_df, month = as.POSIXlt(InvoiceDate)$mon + 1)
months <- group_by(CleanOnlineRetail4_df, month)
head(select(months, month, CustomerID, Description, Price, Country))
tail(select(months, month,  CustomerID, Description, Price, Country))

# 6.3 Group by year and months.
CleanOnlineRetail4_df <- group_by(CleanOnlineRetail4_df, year, month)
head(select(CleanOnlineRetail4_df, year, month, CustomerID, Description, Price, Country))
tail(select(CleanOnlineRetail4_df, year, month, CustomerID, Description, Price, Country))

monthlySum <- CleanOnlineRetail4_df %>% group_by(year,month) %>% summarize(monthlySale = sum(Price, na.rm = TRUE))
monthlySum
monthlyMax <- max(monthlySum)
monthlySum[monthlySum$monthlySale == monthlyMax,]

# 7. Calculate the number of items sold in a year.
factor(CleanOnlineRetail4_df$Description)

# 8. Find the 'fast' and 'slow' products in the dataset.
Ioccurences <- table(unlist(CleanOnlineRetail4_df$Description))
Ioccurences1 <- as.data.frame(Ioccurences)
head(Ioccurences1[order(-Ioccurences1$Freq),], n = 500)

# 9. Identify product purchase behaviour according to customer segments.
setDT(CleanOnlineRetail4_df)
CleanOnlineRetail5_df <- CleanOnlineRetail4_df[, .(Description = list(Description)), by = CustomerID]
CleanOnlineRetail5_df$custIds = cut2(CleanOnlineRetail5_df$CustomerID, g = 4)
table(CleanOnlineRetail5_df$custIds)

setDT(CleanOnlineRetail5_df)
CleanOnlineRetail6_df <- CleanOnlineRetail5_df[, .(Description = list(Description)), by = custIds]
CleanOnlineRetail6_df <- arrange(CleanOnlineRetail6_df, custIds)

# 9.1 12346 - 13814 (Children).
row_1 <- CleanOnlineRetail6_df[1,"Description"]
count_1 <- table(unlist(row_1))
avg_1 <- (sum(count_1))/(lengths(row_1)) 

temp_1 <- count_1[count_1 > avg_1]

obj1 <- as.data.frame(temp_1)
obj1[obj1$Freq == max(obj1$Freq),]

# 9.1.1 Find the 35 most frequently purchased items.
head(temp_1[order(-temp_1)], n = 35)

# 9.2 13814 - 15299 (Teenagers).
row_2 <- CleanOnlineRetail6_df[2,"Description"]
count_2 <- table(unlist(row_2))
avg_2 <- (sum(count_2))/(lengths(row_2)) 

temp_2 <- count_2[count_2 > avg_2]

obj2 <- as.data.frame(temp_2)
obj2[obj2$Freq == max(obj2$Freq),]

# 9.2.1 Find the 35 most frequently purchased items.
head(temp_2[order(-temp_2)], n = 35)

# 9.3 15299 - 16780 (Middle-aged).
row_3 <- CleanOnlineRetail6_df[3,"Description"]
count_3 <- table(unlist(row_3))
avg_3 <- (sum(count_3))/(lengths(row_3)) 

temp_3 <- count_3[count_3 > avg_3]

obj3 <- as.data.frame(temp_3)
obj3[obj3$Freq == max(obj3$Freq),]

# 9.3.1 Find the 35 most frequently purchased items.
head(temp_3[order(-temp_3)], n = 35)

# 9.4 16780 - 18287.
row_4 <- CleanOnlineRetail6_df[4,"Description"]
count_4 <- table(unlist(row_4))
avg_4 <- (sum(count_4))/(lengths(row_4)) 

temp_4 <- count_4[count_4 > avg_4]

obj4 <- as.data.frame(temp_4)
obj4[obj4$Freq == max(obj4$Freq),]

# 9.4.1 Find the 35 most frequently purchased items.
head(temp_4[order(-temp_4)], n = 35)

                                      ## Generating Association rules.  ##

# 1. Consider invoice number and description alone.
marketBasket1 <- aggregate(Description ~ InvoiceNo, data = CleanOnlineRetail4_df, paste, collapse = ",")
write.table(marketBasket1, file = "MarketBasket2.csv", sep=",",col.names = FALSE,row.names = FALSE)

# 2. Inspect the sparse matrix created.
mar<-read.transactions("MarketBasket2.csv",sep = ",")
inspect(mar[1:10])

# 3. Apply Apriori algorithm with low support (assumption).
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
mar1

# 3.1 Apply Apriori algorithm with increased support.
mar1<-apriori(mar,parameter = list(support=0.0004,confidence=0.6,minlen=2))
mar1
mar1<-apriori(mar,parameter = list(support=0.0005,confidence=0.6,minlen=2))
mar1

# 3.2 Since rules generated are less, we consider a low support.
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
inspect(sort(mar1,by="lift")[1:20])

# 3.3 Sort the rules by increasing order of support.
inspect(sort(mar1,by="support",decreasing = FALSE)[1:20])

# 3.4 Check and remove the redundant rules. 
marred<-is.redundant(mar1)
summary(marred)

mar1<-mar1[!marred]
mar1
inspect(mar1[1:20])

# 3.5 Display the results in sorted order. 
inspect(sort(mar1,by="lift")[1:20])

                                      ## Exploratory Analyses. ##

# 1. Plotting probability distributions of Customers. 
qplot(custIds, data = CleanOnlineRetail4_df, facets = .~month)

# 2. Plotting Country with the maximum purchase behaviour.
qplot(Country, data = CleanOnlineRetail4_df, fill = Country)

# 3. Plotting per annum sales.
Years <- monthlySum$year
qplot(Years, data = monthlySum, fill = month)

# 4. Plotting monthly sales. 
qplot(year, monthlySale, data = monthlySum, fill = month, facets = .~month)

# 5. Plotting purchase behaviour of Countries.
qplot(month, data = CleanOnlineRetail4_df, fill = Country)

# 6. Plotting sales behaviour of each item.
barplot(table(CleanOnlineRetail4_df$Description), col = "wheat", main = "Frequency of items purchased.")

# 7. Plotting product purchase behaviour according to customer segments.
  # 7.1 Plotting product purchase behaviour of children.

  with(obj1, plot(Var1, Freq, main = "Product purchase behaviour of children.", type = "n"))
  with(subset(obj1, Freq > 200), points(Var1, Freq, col = "blue"))
  legend("topright", pch = 1, col = c("blue", "black"), legend = c("> 200", "<= 200"))

  # 7.2 Plotting product purchase behaviour of teenagers.

  with(obj2, plot(Var1, Freq, main = "Product purchase behaviour of teenagers.", type = "n"))
  with(subset(obj2, Freq > 200), points(Var1, Freq, col = "blue"))
  legend("topright", pch = 1, col = c("blue", "black"), legend = c("> 200", "<= 200"))

  # 7.3 Plotting product purchase behaviour of middle-aged.
  
  with(obj3, plot(Var1, Freq, main = "Product purchase behaviour of middle-aged.", type = "n"))
  with(subset(obj3, Freq > 200), points(Var1, Freq, col = "blue"))

  # 7.4 Plotting product purchase behaviour of seniors.
  
  with(obj4, plot(Var1, Freq, main = "Product purchase behaviour of seniors.", type = "n"))
  with(subset(obj4, Freq > 200), points(Var1, Freq, col = "blue"))

# 8. Plot the frequency of first ten items purchased.
itemFrequencyPlot(mar,topN=10)

# 9. Plot the support of items in increasing order.
mar1<-apriori(mar,parameter = list(support=0.0002,confidence=0.6,minlen=2))
plot(mar1)

# 10. Plot the sorted list of values.
itemFrequencyPlot(items(mar1),topN=5)

# 11. Plot rules in groups.
plot(mar1,method = "grouped")