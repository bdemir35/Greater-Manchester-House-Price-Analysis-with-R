#Loading Libraries-------

library(tidyverse)
library(janitor)
library(skimr)
library(scales)
library(cartography)


# Importing and exploring dataset-------

house_price <- read_csv("price_paid_records.csv")
View(house_price)
str(house_price)
glimpse(house_price)

# I am living in Greater Manchester County , so I will only focus this area
# to find any significant changes and patterns over time 

# Cleaning dataset -------


# Let's filter our dataframe and assign a new subset dataframe which will be called as mc

mc <- house_price %>% 
  filter(County == "GREATER MANCHESTER")

  View(mc)
  str(mc)
  
# Our new dataframe has 985772 observations
  
# Let's check any duplicated observations in the dataframe
  
get_dupes(mc)   #get_dupes() function is a part of janitor package to find duplicated observations

# Date of Transfer column format is "YYYY-MM-DD", I will create two new column and show the year and month
#of the transaction
 
mc <- mc %>% 
  mutate(year = format(`Date of Transfer`, format = "%Y"),
         month = format(`Date of Transfer`, format = "%m"))

# For consistency let's work on column names, and make some adjustments in columns
# old_new column has "Y" for a newly built property and "N" for an established residential building
#Property Type D = Detached, S = Semi-Detached, T = Terraced, F = Flats/Maisonettes, O = Other
#Note that:
# I only record the above categories to describe property type, I do not separately identify bungalows.
#end-of-terrace properties are included in the Terraced category above.
#‘Other’ is only valid where the transaction relates to a property type that is not covered by existing values.
#Duration Relates to the tenure: F = Freehold, L= Leasehold etc.
#Note that HM Land Registry does not record leases of 7 years or less in the Price Paid Dataset.

mc <- mc %>% 
  clean_names() %>%
  mutate(old_new = recode(old_new, N = "old",
                                   Y = "new"),
         property_type = recode(property_type, D = "Detached",
                                               S = "Semi-Detached",
                                               T = "Terraced",
                                               F = "Flats/Maisonettes",
                                               O = "Other"),
         duration = recode(duration, F = "Freehold",
                                     L = "Leasehold")) %>% 
  select(-c(9:10)) # Dropped PPD Category Type column Indicates the type of Price Paid transaction.
                   # and Record Status column - monthly file only Indicates additions, changes and deletions to the records 

# Writing new csv file after cleaning process

write_csv(mc,"C:/Users/bdemi/Desktop/Kaggle Datasets/UK House Prices/mc_cleaned.csv")


mc <- read_csv("mc_cleaned.csv")

# Analysis ------

# How much the average price for the property type ?

avg_price_by_type <- mc %>% 
  group_by(property_type) %>% 
  select(price, property_type) %>% 
  summarise(price = mean(price))
  

# How many transaction completed by property type ?

number_of_property_type <- mc %>% 
  group_by(property_type) %>% 
  summarise(count = n())

# How the sales trend changed in property type over time ? (between 1995-2017)

sale_trend_over_time <-  mc %>% 
  group_by(property_type, year) %>% 
  summarise(count = n())

# How did the demand for newly build properties in the market changed over time ?

demand_over_time <- mc %>% 
  group_by(property_type, old_new, year) %>% 
  summarise(count = n())

# How did customers prefer their tenure when they decided  to buy a property ?

tenure <- mc %>% 
  select(property_type, duration , year) %>% 
  group_by(property_type, duration, year) %>%
  filter(duration != "U" ) %>%  # In duration column there are 63 observations like "U"
  summarise(count = n())        # There is no detail about that records means "Unknown" or this relates to something
                                # original documentation (https://www.gov.uk/guidance/about-the-price-paid-data) also do
                                # not say anything about this. I filtered out this value from the dataset.


# Which town/city is the most popular locations for the buyers ?

n_unique(mc$district) ## Shows that how many different location in our dataframe

number_of_sales_by_district <- mc %>% 
  group_by(district) %>% 
  summarise(number_of_sales = n()) %>% 
  arrange(desc(number_of_sales))



# How much the min ,max, avg prices for the sold property in each year ?

mc %>% 
  select(property_type, year, price) %>%
  group_by(property_type, year) %>% 
  summarise(min = min(price),
            avg = mean(price),
            max = max(price))


# Visualization -----

avg_price_by_type %>% 
  ggplot(aes(x = reorder(property_type, -price), y = price)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "#47B9B9")+
  labs(title = "Average Price by Property Type",
       x = "Property Type",
       y = "Average Price (£)")+
  geom_label(label = round(avg_price_by_type$price, 2),
            color = "white",
            fill = "#0A6774")+
  theme_grey()+
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        axis.text.x = element_text(colour = "#081616", size = 10, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        text = element_text(size = 14))

ggsave("avg_price_by_type.jpg", width = 10, height = 10)

# Newly built property selling demand over time

demand_over_time %>%
  filter(old_new == "new") %>% 
  ggplot(aes(x = year, y = count, color = property_type))+
  geom_line()+
  labs(title = "Newly Built Property Sales Demand over Time",
       x = "",
       y = "Number of Sales")+
  geom_path(size = 1.5)+
  theme(
    axis.text.x = element_text(size = rel(1)),
    plot.title.position = "plot",
    panel.grid = element_blank(),
    text = element_text(size = 14))+
  scale_x_continuous(breaks = c(1995:2017))
  

ggsave("newly_built_demand.jpg", width = 20, height = 10)

# Established property selling demand over time
 
demand_over_time %>%
  filter(old_new == "old") %>% 
  ggplot(aes(x = year, y = count, color = property_type))+
  geom_line()+
  labs(title = "Established Property Sales Demand over Time",
       x = "",
       y = "Number of Sales")+
  geom_path(size = 1.5)+
  theme(axis.text.x = element_text(size = rel(1)),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        text = element_text(size = 14))+
  scale_x_continuous(breaks = c(1995:2017))+
  ylim(0,30000)

ggsave("established_property_demand.jpg", width = 20, height = 10)


# Distribution of Sold Property by Type

# First of all we need to make a new column called percentage shows the percentage of each property type sold.
number_of_property_type %>% 
  mutate(percentage = `count`/sum(`count`),
         output = percentage*100) %>% 
  ggplot(aes(x = "", y = output, fill = property_type))+
  geom_col(color = "white")+
  geom_text(aes(label = round(output,2)),
            color = "white",
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  labs(title = "Total Property Sold by Percentage between 1995-2017")+
  scale_fill_manual(values = c("#588EAB","#93BBD1","#D8E6ED","#1A4156","#041F2E"))+
  theme_void()+
  theme(plot.title = element_text(face="bold"))+
  guides(fill = guide_legend(title = "Property Type"))
  
ggsave("Total_property_sold_pie.jpg", height = 10, width = 10)


# Number of Sales by District

number_of_sales_by_district %>% 
  ggplot(aes(reorder(x = district, -number_of_sales), y = number_of_sales))+
  geom_col(fill = "#47B9B9")+
  labs(title = "Number of Total Sales by District between 1995-2017",
       x = "",
       y = "")+
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())+
  geom_label(label = number_of_sales_by_district$number_of_sales,
             fill = "#0A6774",
             color = "white")

ggsave("total_sales_by_district.jpg", height = 10, width = 10)


# Total sales trend by property type

sale_trend_over_time %>% 
  ggplot(aes(x = year, y=count))+
  geom_line(color = "#081616")+
  geom_point(color = "#47B9B9")+
  labs(title = "Number of Total Property Sales Trend between 1995-2017",
       x = "",
       y = "")+
  facet_wrap(~property_type,
             scales = "free")+
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave("total_property_sales_trend.jpg", height = 10, width = 10)


# Let's assign month values to seasons and check the selling trend in different seasons
mc$month <- as.numeric(mc$month)

mc %>% 
  select(month) %>% 
  mutate(Season = case_when(month %in% 3:5 ~ 'Spring',
                            month %in% 6:8 ~ 'Summer',
                            month %in% 9:11 ~ 'Autumn',
                            TRUE ~ 'Winter')) %>%
  group_by(Season) %>% 
  tally() %>% 
  ggplot(aes(reorder(x = Season, -n), y = n, fill = Season))+
  geom_col()+
  geom_text(aes(label = n),
            color = "white",
            position = position_stack(vjust = 0.98))+
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0))+
  labs(title = "Number of Sales by Season between 1995-2017",
       x = "",
       y = "Number of Sales")+
  scale_fill_manual(values = c("#0F4D6C", "#699CB6", "#041E2B", "#ABCADA"))+
  theme(legend.position = "none")

ggsave("season_sales.jpg", height = 10, width = 10)


# Which tenure is most common for buyers over the time ?
# Any changes with the trend ?
# Which property type buyers prefer which tenure type ?

tenure %>% 
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = duration))+
  geom_point()+
  facet_wrap(~property_type,
             scales = "free")+
  labs(title = "Change in Tenure Preference Over Time by Property Type ",
       x = "",
       y = "Number of Property Sold",
       color = "Tenure")+
  theme(plot.title  = element_text(face = "bold"),
        plot.title.position = "plot")

ggsave("tenure_preference.jpg", height = 10, width = 10)
  
  

  
  



 






  
  


