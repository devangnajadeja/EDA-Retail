getwd()

## Reading the data set
superstore <- read.csv("SampleSuperstore.csv", header = TRUE)

## Looking at the data and the structure of columns
str(superstore)
head(superstore, 10)
tail(superstore, 10)

colnames(superstore)

dim(superstore)

## Checking if there are any missing values present or not
sum(is.na(superstore))

## Summary of all the variables
summary(superstore)


## Removing the 'Country'column, since the whole data is of the United States
superstore <- within(superstore, rm(Country, Postal.Code))

## Looking at all the category of products in the data set
unique(superstore$Category)

## Number of products in each category
superstore %>%
  group_by(Category) %>%
  summarise(total=n()) %>%
  arrange(desc(total))

## Sub category
n_distinct(superstore$Sub.Category)

superstore %>%
  group_by(Sub.Category) %>%
  summarise(total=n()) %>%
  arrange(desc(total))

## Analyzing patterns in the data set

# Sales and Quantity
ggplot(superstore, aes(x = Quantity, y = Sales, fill = Ship.Mode) ) + 
  geom_bar(stat = "identity") 

## Frequency distribution of quantity ordered
hist(superstore$Quantity, main = "Freq distribution", xlab = "Quantity ordered", 
     ylab = "Frequency", col = "Pink")

## Correlation matrix
ss <- superstore[,c(8,9,10,11)]
ss.cor = cor(ss)
ss.cor
corrplot(ss.cor)

## Effect on sales if discounts are offered
ggplot() + 
  geom_point(superstore, mapping = aes(x = Discount, y = Sales, color = Ship.Mode)) 

## Profits, discounts and Ship.mode
summary(superstore$Profit)
summary(superstore$Discount)

ggplot() + 
  geom_bar(superstore, mapping = aes(x = Discount, y = Profit, fill = Ship.Mode), 
           stat = "identity") 

## Profits, sub.category and region
ggplot() + 
  geom_bar(superstore, mapping = aes(x = Sub.Category, y = Profit, fill = Region), 
           stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Visualizing the frequency of sub category
ggplot(superstore, aes(x= Sub.Category, fill = Category)) + 
  geom_bar() + 
  theme_bw() + 
  coord_flip() + 
  labs(title = "Count by Category and Sub.Category",x = "Sub.Category", y = "Frequency") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


## Total Profit and Sale by Sub_Category
superstore %>% 
  group_by(Sub.Category) %>% 
  summarise( total_sales = sum(Sales),total_profit =sum(Profit)) %>% 
  pivot_longer(c("total_sales", "total_profit")) %>% 
  ggplot(aes(x=Sub.Category, y = value, fill = name)) + 
  geom_col(position = position_dodge()) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Profit and Sales by Sub.Category") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

## Sub category by region
superstore %>% 
  ggplot(aes(x=Sub.Category, fill= Region)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Sub.Category Product Sales by Region")+
  theme(plot.title = element_text(size=15, face="bold",hjust = 0.5))


## Calculating Cost
superstore <- superstore %>% 
  mutate(Cost = Sales - Profit)

## Calculating percentage profit
superstore <- superstore %>% 
  mutate(profit_percent = (Profit/Cost *100))

## Category with 100% profit
superstore %>% 
  select(Category, profit_percent) %>% 
  filter(profit_percent==100) %>% 
  arrange(Category) %>%
  ggplot(aes(x= Category, fill = "Red")) + 
  geom_bar(show.legend = FALSE)+ theme_bw() +
  labs(title = "Count of Category with 100% profit")+
  theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))


## Sales by Region
superstore %>% 
  group_by(Region) %>% 
  summarize(total_sales = round(sum(Sales))) %>% 
  ggplot(aes(area = total_sales, fill = Region, 
             label= paste0(Region, "\n",prettyNum(total_sales, ",")))) + geom_treemap() + geom_treemap_text(color= "white", 
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Sales by Region") + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


## Profit by Region
superstore %>% 
  group_by(Region)%>%
  summarize(total_profit = round(sum(Profit))) %>% 
  ggplot(aes(area = total_profit  , fill = Region, 
             label= paste0(Region, "\n",prettyNum(total_profit, ",")))) +
  geom_treemap() + 
  geom_treemap_text(color= "white", 
                    place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none") + labs(title = "Profit by Region") + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


                                  ## Additional ##
## With just a line of code, we can generate the whole report of EDA, 
#only by installing
# and loading two packages - "Data explorer", and "Tidyverse"
superstore %>%
  create_report(output_file = "EDA_Report", output_dir = "TSF Internship/", 
                y = "Region", report_title = "EDA Report")
