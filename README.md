# Greater Manchester Sold House Prices Changes between 1995-2017

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Images/main.jpg)



# Introduction  
The Price Paid Data includes information on all registered property sales in England and Wales that are sold for full market value.

You might also find the HM Land Registry transaction records to be a useful supplement to this dataset: https://www.kaggle.com/hm-land-registry/uk-land-registry-transactions

Note that where a transaction changes category type due to misallocation (as above) it will be deleted from the original category type and added to the correct category with a new transaction unique identifier.

This data was kindly released by [HM Land Registry](https://www.gov.uk/government/organisations/land-registry/about) under the [ Open Government License 3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). You can find their current release here.

Data produced by HM Land Registry © Crown copyright 2017. 

  
# Important Note  


* The dataset consists all sales transactions occured in the England and Wales, but I will focus only the Greater Manchester County which I live in this area and interested to find different trends and patterns after the analysis.

# Questions  

The questions we are going to try to answer at the end of the project.

* How much the average price changed for each property type over time ?
* Which type of property sold the most ?
* How buyers preference has changed over the time ?
* Did sales trend changed over the time ?
* How did the demand for newly build properties in the market changed over time ?
* How did customers prefer their tenure when they decided  to buy a property ?
* Which district is the most popular locations for the buyers ?
* How much the minimum ,maximum and average prices for the sold property in each year ?

# Import and Exploring  dataset

```{r message=FALSE}
#Loading necessary libraries for analysis

library(tidyverse)
library(janitor)
library(skimr)
library(scales)
```


```{r}
# Import section and checking the structure of dataset

house_price <- read_csv("price_paid_records.csv") # Dataset has 22489348 observations.
View(house_price)
str(house_price)
glimpse(house_price)

```

# Filtering dataset

The code below filtering the data only shows **__Greater Manchester__** area. From here we will call our  
dataframe as "mc".


```{r cache=TRUE, echo=FALSE}

mc <- house_price %>% 
  filter(County == "GREATER MANCHESTER")

  View(mc)
  str(mc) # Our new dataframe has 985772 observations
```

  
# Cleaning Process

* Checking duplicates

```{r}

get_dupes(mc)    #get_dupes() function is a part of janitor package to find duplicated observations

```

  __There is no duplicated observation in the dataset.__


* Data types

Date of Transfer column format is __"YYYY-MM-DD"__, I will create two new column  
and show the **year** and **month** of the transaction.

```{r echo=TRUE, warning=FALSE}

mc <- mc %>% 
  mutate(year = format(`Date of Transfer`, format = "%Y"),
         month = format(`Date of Transfer`, format = "%m"))

# Now we have new two columns but  data types should be correct

class(mc$month)
class(mc$year)

mc$month <- as.character(mc$month)
mc$year <- as.character(mc$year)



```

* Column names explained

In this section we are going to explain our variables in details.
 
1) For consistency let's work on column names, and make some adjustments in columns  

2) old_new column has "Y" for a newly built property and "N" for an established residential building

3) Property Type D = Detached, S = Semi-Detached, T = Terraced, F = Flats/Maisonettes, O = Other  

**__Important Note:__**  

* I only record the above categories to describe property type, I do not separately identify bungalows.  
* end-of-terrace properties are included in the Terraced category above.  
* Other’ is only valid where the transaction relates to a property type that is not covered by existing   values.
* Duration Relates to the tenure: F = Freehold, L= Leasehold etc.  
* Note that HM Land Registry does not record leases of 7 years or less in the Price Paid Dataset.

In the code below we are going to adjust the variable names for consistency.  

And then changing the values with a more readable and understandable format.  

Dropping the PPD Category and Record Status column.  


```{r}
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
  select(-c(10:11)) # Dropped PPD Category Type column Indicates the type of Price Paid transaction.
                   # and Record Status column - monthly file only Indicates additions, changes and deletions to the records 
```

__To avoid overload in the environment I will save this cleaned new dataset as a new csv file.__

# Writing new csv


```{r}
write_csv(mc,"C:/Users/bdemi/Desktop/Kaggle Datasets/UK House Prices/mc_cleaned.csv")
```

From here we will continue with the new cleaned file for memory purposes.

```{r}
mc <- read_csv("mc_cleaned.csv")
```


# Analysis 

* How much the average price for the property type ?


```{r echo=TRUE}

avg_price_by_type <- mc %>% 
  group_by(property_type) %>% 
  select(price, property_type) %>% 
  summarise(price = mean(price)) %>% 
  print(avg_price_by_type)

```
  

* How many transaction completed by property type ?


```{r}

number_of_property_type <- mc %>% 
  group_by(property_type) %>% 
  summarise(count = n()) %>% 
  print(number_of_property_type)

```

* How the sales trend changed in property type over time ? (between 1995-2017)


```{r echo=TRUE, message=FALSE, warning=FALSE}

sale_trend_over_time <-  mc %>% 
  group_by(property_type, year) %>% 
  summarise(count = n()) %>% 
  print(sale_trend_over_time)

```

* How did the demand for newly build properties in the market changed over time ?


```{r echo=TRUE, message=TRUE, warning=FALSE}

demand_over_time <- mc %>% 
  group_by(property_type, old_new, year) %>% 
  summarise(count = n()) %>% 
  print(demand_over_time)

```

* How did customers prefer their tenure when they decided  to buy a property ?


```{r}
tenure <- mc %>% 
  select(property_type, duration , year) %>% 
  group_by(property_type, duration, year) %>%
  filter(duration != "U" ) %>%
  summarise(count = n()) %>% 
  print(tenure)
```
**__Important Note:__**  In duration column there are 63 observations called "U".  
There is no detail about that records means "Unknown" or this relates to something original documentation [here](https://www.gov.uk/guidance/about-the-price-paid-data) also do not say anything about this. I filtered out this value from the dataset.

* Which district is the most popular locations for the buyers ?

```{r}
n_unique(mc$district)  # Shows that how many different location in our dataframe
```


```{r}

number_of_sales_by_district <- mc %>% 
  group_by(district) %>% 
  summarise(number_of_sales = n()) %>% 
  arrange(desc(number_of_sales)) %>% 
  print(number_of_sales_by_district)

```



* How much the minimum ,maximum and average prices for the sold property in each year ?

```{r}
mc %>% 
  select(property_type, year, price) %>%
  group_by(property_type, year) %>% 
  summarise(min = min(price),
            avg = mean(price),
            max = max(price))
```


# Visualization 

* Average Price By Property Type Plot

```{r fig.width=15}
avg_price_by_type %>% 
  ggplot(aes(x = reorder(property_type, -price), y = price)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "#47B9B9")+
  labs(title = "Average Price by Property Type",
       x = "",
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

```
![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/avg_price_by_type.jpg)



* Newly built property selling demand over time Plot

```{r fig.width=15, fig.height=5}

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



```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/newly_built_demand.jpg)



* Established property selling demand over time

```{r fig.width=15, fig.height=5}
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
```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/established_property_demand.jpg)

 
* Distribution of Sold Property by Type

__First of all we need to make a new column called percentage shows the percentage of each property type sold.__

```{r fig.width=15}

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

```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/Total_property_sold_pie.jpg)


* Number of Sales by District

```{r fig.width= 15}

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

```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/total_sales_by_district.jpg)


* Total sales trend by property type

```{r fig.width=15}

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

```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/total_property_sales_trend.jpg)

* Let's have a look at the best and least selling season and 

__Let's assign month values to seasons and check the selling trend in different seasons__

**Please be aware of month data type is character so first change it to a numeric**
```{r fig.width=15}
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

```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/season_sales.jpg)

* Which tenure is most common for buyers over the time ?
* Any changes with the trend ?
* Which property type buyers prefer which tenure type ?

```{r fig.width=15}

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

```

![](https://github.com/bdemir35/Greater-Manchester-House-Price-Analysis-with-R/blob/main/Visuals/tenure_preference.jpg)
  
# Conclusion

* In average property prices between the £205.000 - just below £79.0000 , respectively Detached properties has the highest price in the market and in contrast Terraced  properties has the lowest market value.

* Just after 2001 newly built developments in Flats/Masionettes numbers are significantly increased in the market until 2006 , however after this year it reduced  dramatically until 2012 , but still has the highest demand in the market.  

* Established properties demand for the buyers has highest number for Terraced and Semi-Detached type of properties, almost showing the same trend, where terraced type is more popular for the buyers.

* Between the years __1995-2017__ sold properties number is  in total 985227. Terraced houses has the almost **39%** of total sales, and Semi-Detached properties has the 2nd rank with just above **33.5%**.

* As expected most popular city is the Manchester and Stockport and Wigan following it respectively. They have 41% of the market share.

* All types of property sales have a significant decrease after the year 2007, while gaining increase after the year 2012.

* The most popular season for buyers is Summer while least popular season is Winter. But importantly , there is no notably difference between the number of sales.

* Except for Flat/Masionettes property type , the buyers always prefer Freehold more than Leasehold for Detached, Semi-Detached and Other property types, while for Terraced buyers almost has 50% distribution.
  
  



 






  
  




