                                    ## Downloading the dataset. ##

fileUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"
download.file(fileUrl, destfile = "./data/Online Retail.xlsx", method = "curl")
dateDownloaded <- date()

                                    ## Reading the dataset. ##

library(readxl)
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

# 1. Probability distributions of Customer Id's.                                    
quantile(CleanOnlineRetail4_df$CustomerID, na.rm = TRUE)

library(Hmisc)
CleanOnlineRetail4_df$custIds = cut2(CleanOnlineRetail4_df$CustomerID, g = 5)
table(CleanOnlineRetail4_df$custIds)
table(CleanOnlineRetail4_df$custIds, CleanOnlineRetail4_df$CustomerID)

# 2. Calculate the number of customers in the dataset.(Accurate)
CleanOnlineRetail4_df$zcf  <- factor(CleanOnlineRetail4_df$CustomerID)
CleanOnlineRetail4_df$zcf  


















