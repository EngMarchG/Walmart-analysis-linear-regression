setwd(choose.dir())

dataset = read.csv('Walmart_Store_sales.csv')
str(dataset)
View(dataset)

### Count the number of missing data if any
numberOfNA <- lapply(lapply(dataset, is.na), sum)
numberOfNA

### Finding the store with the maximum sales
library(sqldf)
stat_df <- sqldf('SELECT Store, SUM(Weekly_Sales) AS Total_Sales FROM dataset GROUP BY Store')
max_sale <- stat_df[stat_df$Total_Sales == max(stat_df$Total_Sales),]
max_sale
# Hence, we find that store 20 has the highest total sales with 301397792$

### Finding the store with the maximum standard deviation
stat_df$Sales_sd <- sqldf('SELECT STDEV(Weekly_Sales) AS Sales_sd FROM dataset GROUP BY Store')
max_sd <- stat_df[stat_df$Sales_sd == max(stat_df$Sales_sd), ]
max_sd
# Hence, we conclude that Store 14 is the most unstable store with a standard deviation of 317569.9$

# New data frame to compile as we answer further questions
df <- data.frame(Store = max_sales_store[,1], Weekly_Sales_Sum = max_sales_store[,2], 
                 Max_sd = max_sd[,2])
str(df)

### Finding the Q3 sales of each store
dataset$subdate <- paste0(substr(dataset$Date, 7, length(dataset$Date)),
                          substr(dataset$Date, 3, 6),
                          substr(dataset$Date, 1, 2))
dataset$subdate <- type.convert(dataset$subdate, as.is = T)
class(dataset$subdate)
dataset$subdate

dataset$Quarterly <- as.character(as.yearqtr(dataset$subdate,
                                             format = "%Y-%m-%d"))

df <- cbind(df, sqldf('SELECT SUM(Weekly_Sales) as "Q3_Sales" FROM dataset 
                      WHERE Quarterly LIKE "2012 Q3"
                      GROUP By Store'))
df <- cbind(df, sqldf('SELECT SUM(Weekly_Sales) as "Q2_Sales" FROM dataset 
                      WHERE Quarterly LIKE "2012 Q2"
                      GROUP By Store'))
str(df)

sqldf('SELECT Store, ((Q3_Sales - Q2_Sales)/Q3_Sales)*100 as Q3_Growth from df order by Q3_Growth DESC LIMIT 15')
# Store 4 has the highest sales throughout Q3 with up to 27796792$ in revenue. 
# However, in terms of growth, is considered a low performing considering its negative growth. 
# On a different note, Store 7 grew by 11.8% outgrowing all other stores by a large margin.
# This is followed by stores 16 and 35 respectively. Additionally, only 10 stores experience a positive growth.


### Can some holidays have a negative impact on sales? 

# Finding the sum of weekly sales for each store in the required holidays
holiday_df <- NULL
holiday_df <- sqldf('SELECT AVG(Weekly_Sales) as SuperBowl FROM dataset WHERE subdate = "2013-02-08" OR
                          subdate = "2012-02-10" OR subdate = "2011-02-11" 
                          OR subdate = "2010-02-12"')

holiday_df <- cbind(holiday_df,sqldf('SELECT AVG(Weekly_Sales) as LabourDay FROM dataset 
                          WHERE subdate = "2013-09-06" OR
                          subdate = "2012-09-07" OR subdate = "2011-09-09" 
                          OR subdate = "2010-09-10" '))

holiday_df <- cbind(holiday_df,sqldf('SELECT AVG(Weekly_Sales) as Thanksgiving FROM dataset 
                          WHERE subdate = "2013-11-29" OR
                          subdate = "2012-11-23" OR subdate = "2011-11-25" 
                          OR subdate = "2010-11-26" '))

holiday_df <- cbind(holiday_df,sqldf('SELECT AVG(Weekly_Sales) as Christmas FROM dataset 
                          WHERE subdate = "2013-12-27" OR
                          subdate = "2012-12-28" OR subdate = "2011-12-30" 
                          OR subdate = "2010-12-31"'))
View(holiday_df)

# Finding the mean of all stores in all the days not considered as the target holiday
mean_stores_noHoliday <- sqldf('SELECT AVG(Weekly_Sales) FROM dataset 
                          WHERE Holiday_Flag = 0')
mean_stores_noHoliday
mean_stores_noHoliday <-  mean_stores_noHoliday[1,1]   
# Adding the mean as a vector of equal size to the holiday data frame
holiday_df$Mean_noHoliday <- rep(mean_stores_noHoliday, 45)
str(holiday_df)

# Weekly Sales of Each store during the holidays - mean (for non-holiday days)
holiday_df2 <- sqldf('SELECT Store, SuperBowl - Mean_noHoliday AS SuperBowl_diff,
                     LabourDay - Mean_noHoliday AS LabourDay_diff, 
                     Thanksgiving - Mean_noHoliday AS Thanksgiving_diff,
                     Christmas - Mean_noHoliday AS Christmas_diff
                     FROM holiday_df')
holiday_df2

# How many negatives sales when compared to the average and their average
holiday_df2_summary <- cbind(sqldf('SELECT COUNT(CASE WHEN SuperBowl_diff < 0 THEN 1 END) as total_neg_superbowl, 
      COUNT(CASE WHEN LabourDay_diff < 0 THEN 1 END) as total_neg_LabourDay, 
      COUNT(CASE WHEN Thanksgiving_diff < 0 THEN 1 END) as total_neg_Thanksgiving,
      COUNT(CASE WHEN Christmas_diff < 0 THEN 1 END) as total_neg_Christmas
      FROM holiday_df2 '),
                             sqldf('SELECT AVG(SuperBowl_diff), AVG(LabourDay_diff), AVG(Thanksgiving_diff), 
      AVG(Christmas_diff) FROM holiday_df2 '))
holiday_df2_summary
# It is found that the average of weekly sales in normal days is 1041256$ 

# Consequently, Christmas sales perform below average compared to the average sales on normal days
# This could be attributed to the overall spending mostly occurring during the first and last 
# weeks of Christmas, as well as most people spending time with family and friends on other days.
# On the other hand, Thanksgiving had the highest average of 1471273$

# Overall, the average weekly sales on holidays outperform the average weekly sales on normal days


# NOTE: the average can be found then subtracting as seen below BUT the answer is the same i.e
# # New data frame subtracting the values of holiday_df by the Average(mean)
# holiday_df2 <- sqldf('SELECT Store, AVG(SuperBowl) as SuperBowl_Avg,AVG(LabourDay) AS LabourDay_Avg,
#                      AVG(Thanksgiving) AS Thanksgiving_Avg, AVG(Christmas) AS Christmas_Avg
#                      FROM holiday_df')


### Provide a monthly and semester view of Sales in units and give Insights
  
View(dataset)
glimpse(dataset)
dataset$month <- substr(dataset$Date, 4, 5)
monthly_sales_df <- sqldf('SELECT month, SUM(Weekly_Sales) as Monthly_sales, AVG(Weekly_Sales) as Monthly_AVG, 
                          STDEV(Weekly_Sales) as Monthly_sd FROM dataset GROUP By month')
sqldf('SELECT month, Monthly_sales, Monthly_AVG, Monthly_sd from monthly_sales_df 
      ORDER By Monthly_sales DESC')
sqldf('SELECT month, Monthly_sales, Monthly_AVG, Monthly_sd from monthly_sales_df 
      ORDER By Monthly_AVG DESC')
sqldf('SELECT month, Monthly_sales, Monthly_AVG, Monthly_sd from monthly_sales_df 
      ORDER By Monthly_sd DESC')
# Looking at the monthly sales, we can see that the average monthly sales across the stores are highest
# during July. Additionally, the lowest sales are on January. 
# The trend aligns with expectations, since July is often considered month of holidays for students.
# On the other hand, January is preceded by celebrations and parties taken in December for New year's eve and 
# Christmas. 
# However, as noted previously we can see the standard deviation of December is also the greatest due to 
# The imbalance spending throughout the month.

# Additional insight looking at the monthly sales of each stores rather than of all combined.
monthly_sales_dfstores <- sqldf('SELECT Store,month, SUM(Weekly_Sales) as Monthly_sales, AVG(Weekly_Sales) as Monthly_AVG, 
                          STDEV(Weekly_Sales) as Monthly_sd FROM dataset GROUP By Store, month')
sqldf('SELECT Store,month, Monthly_sales,Monthly_AVG, 
                          Monthly_sd FROM monthly_sales_dfstores ORDER By Monthly_sales DESC LIMIT 10')
# Interestingly, Store 14 holds the highest monthly sale and as found before is the store with highest deviation.
# However, Store 20 holds more spots at the top, as expected considering its found to be the most lucrative store. 
# Additionally, as expect July is seen multiple times as a top performing month. 
# Still, April and June are also months performing considerably well too.

# Semester View of sales
dataset$semester <- ifelse(as.numeric(dataset$month) < 7, 1 , 2 )
yearly_sales_df <- sqldf('SELECT semester, SUM(Weekly_Sales) as Semester_sales, AVG(Weekly_Sales) as Semester_AVG, 
                          STDEV(Weekly_Sales) as Semester_sd FROM dataset GROUP By semester')
glimpse(yearly_sales_df)
barplot(yearly_sales_df[, 2] ~ yearly_sales_df[, 1], main = 'Semesterly Revenue', col=c("blue","red"), 
        xlab = "Semester", ylab = "Total Revenue", legend= c("Semester 1", "Semester 2"))
# Semester two has a total of 95.8 million dollars more than semester one in revenue.
# Additionally, It is also higher in terms of average revenue per sale and standard deviation of sales.
# Hence, the trend follows the previous findings considering that July and December are both in semester 2.
# Similarly, a low performing month like January is in the 1st semester.

