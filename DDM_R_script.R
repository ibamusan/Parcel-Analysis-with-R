####Calling Libraries and data.####
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#This is where I set my working directory.
setwd("C:/Users/USER/Desktop/DDM_Assignment/1_data")
getwd()

#This is where I read my data.
read_excel("driver_31.xlsx")
read_excel("parcel_31.xlsx")

#I store driver data as data_driver and parcel data as data_parcel
data_driver <- read_excel("driver_31.xlsx")
data_parcel <- read_excel("parcel_31.xlsx")

####Data Cleaning####
#I checked for NA values 
#I used the map function in the purrr to avoid checking for NA in each of the column, this will make my code efficient.
data_driver %>%
  map(~sum(is.na(.)))

#This is shows there is 487 NA values in priority_delivery and 410 NA values in parcel_payment.
data_parcel %>%
  map(~sum(is.na(.)))

#I used str() to look at the obs. in the colums with NA values.

str(data_parcel$priority_delivery)
str(data_parcel$parcel_payment)

#For priority_delivery, it make sense to replaced the NA value to "no" meaning no priority delivery.
data_parcel_na_replaced <- data_parcel %>%
  replace_na(list(priority_delivery = "no"))

#Hence we can now drop the other 410 NA values in parcel_payment because there is no conversion appropriate for it. Also, the total NA is less than 1% of the total data, removing it will not have effect on the analysis. This action will reduce the total obs. to 49,655.

data_parcel_na_replaced %>%
  drop_na()

#This shows the outlier in parcel_returned.
summary(data_parcel_na_replaced$parcel_returned)

#The final cleaned data is assigned to data_parcel_cleaned. This removed the outliers in the parcel_returned
data_parcel_cleaned <- data_parcel_na_replaced %>%
  drop_na() %>%
  filter(parcel_returned >= 0)

#This is where I joined the two datasets and assigned it to data_driver_parcel_joined.
data_driver_parcel_joined <- full_join(data_driver, data_parcel_cleaned, by = "driver_id", multiple = "all")

#I finally check for NA
data_driver_parcel_joined %>%
  map(~sum(is.na(.)))


####Data Transformation and Analysis####
#I run a descriptive stat. to check for outlier.
summary(data_driver_parcel_joined)

#Check for Outliers using boxplot: I used boxplot to further check for outlier in parcel_returned to be sure those discovered while scanning through the data has been removed during data cleaning.
ggplot(data_driver_parcel_joined, aes(x = "", y = parcel_returned)) +
  geom_boxplot()

table(data_driver_parcel_joined$parcel_status)

#1. This is data that shows column with Driver category based experience and take care of the outliers in the parcel status and time of delivery. This is the main dataset used for my analysis.

driverCategory_data <- data_driver_parcel_joined %>% 
  select(driver_id, experience, parcel_status, parcel_value, work_pattern, gender, van_type, time_of_delivery) %>%
  filter(parcel_status != "dress", time_of_delivery != "kinder") %>% 
  mutate(experience_level = case_when(experience >= 1 & experience <= 3 ~ "Beginner", experience >3 & experience <= 6 ~ "Intermediate", TRUE ~ "Expert"))

#Table 3.1: This table shows the percentage of the total parcel value for each category in parcel status.
percentParcelStatus_data <- driverCategory_data %>%
  select(driver_id, parcel_status, parcel_value) %>%
  group_by(parcel_status) %>%
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percent_total_parcel_value = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#This dataset grouped drivers based on their experience level and gives total parcel value delivered by category.

driverDelivery_data <- driverCategory_data %>%
  select(driver_id, parcel_status, parcel_value, experience_level) %>%
  filter(parcel_status == "delivered") %>% 
  group_by(experience_level) %>%
  summarise(Total_parcel_value = sum(parcel_value))

#Table 3.2:  This table shows the percentage of total parcel value delivered for each category of Driver.

percentDelivered_data <- driverDelivery_data %>% 
  mutate(percentage_delivered = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Appendix 2: This table shows percentage of total parcel value that was lost for each category of the Driver.

Lost_data <- driverCategory_data %>% 
  select(driver_id, experience_level, parcel_status, parcel_value) %>% 
  filter(parcel_status == "lost") %>% 
  group_by(experience_level) %>% 
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_lost = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Appendix 3: This table shows percentage of total parcel value that was lost for each category of the Driver.

ReturnToWarehoure_data <- driverCategory_data %>% 
  select(driver_id, experience_level, parcel_status, parcel_value) %>% 
  filter(parcel_status == "returned to warehouse") %>% 
  group_by(experience_level) %>% 
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_returned = ceiling(Total_parcel_value/sum(Total_parcel_value)*100) )

#Table 3.3: This table shows the impact of work patterns on the total parcel value delivered.

workPatternExplanatory_data <- driverCategory_data %>%
  select(experience_level, parcel_status, parcel_value, work_pattern) %>%
  filter(parcel_status == "delivered") %>% 
  group_by(experience_level, work_pattern) %>%
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_delivered = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Table 3.4: This table shows the impact of Time of delivery on the total parcel value delivered.

TimeDeliveryDExplanatory_data <- driverCategory_data %>%
  select(experience_level, parcel_status, parcel_value, time_of_delivery) %>%
  filter(parcel_status == "delivered") %>% 
  group_by(experience_level, time_of_delivery) %>%
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_delivered = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Table 3.5: This table shows the impact of the type of Van on the Total parcel value delivered.

VanTypeExplanatory_data <- driverCategory_data %>%
  select(experience_level, parcel_status, parcel_value, van_type) %>%
  filter(parcel_status == "delivered") %>% 
  group_by(experience_level, van_type) %>%
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_delivered = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Table 3.6: This table shows the impact of Gender on the Total parcel value delivered.

genderExplanatory_data <- driverCategory_data %>%
  select(experience_level, parcel_status, parcel_value, gender) %>%
  filter(parcel_status == "delivered", gender != "unknown/ don't want to say") %>% 
  group_by(experience_level, gender) %>%
  summarise(Total_parcel_value = sum(parcel_value)) %>% 
  mutate(percentage_delivered = ceiling(Total_parcel_value/sum(Total_parcel_value)*100))

#Visualization####

#Figure 3.1: This graph shows the percentage of the total parcel value for each category in parcel status.
Plot1 <- ggplot(percentParcelStatus_data, aes(x = parcel_status, y = percent_total_parcel_value, fill = parcel_status)) + geom_col(width = 0.6) + 
  labs(x = "Parcel Status",
       y = "Total Parcel Value (%)",
       title = "Fig-1 Parcel Value Distribution") + 
  theme_classic() + scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff")) + 
  theme(legend.position = "none", legend.title = element_blank())


ggsave(filename = "Plot1.png", plot = Plot1, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#Figure 3.2: This graph shows the relationship between Driver’s experience level and the Parcel value lost.
lost_plot <- ggplot(Lost_data, aes(x = experience_level, y = percentage_lost, fill = experience_level)) +
  geom_col(width = 0.6) + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Lost(%)",
       title = "The Relationship between Driver's Experience on Parcel value Lost") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff")) + 
  theme(legend.position = "none", legend.title = element_blank())

#Figure 3.3: This graph shows the relationship between Driver’s experience level and the Parcel value returned to the warehouse.
returned_plot <- ggplot(ReturnToWarehoure_data, aes(x = experience_level, y = percentage_returned, fill = experience_level)) +
  geom_col(width = 0.6) + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Returned to Warehouse(%)",
       title = "The Relationship between Driver's Experience on Parcel value returned to warehouse") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff")) + 
  theme(legend.position = "none", legend.title = element_blank())


#Figure 3.4: This graph shows the relationship between Driver’s experience level and the Parcel value delivered.
Plot2 <- ggplot(percentDelivered_data, aes(x = experience_level, y = percentage_delivered, fill = experience_level)) +
  geom_col(width = 0.6) + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Delivered(%)",
       title = "Fig-2 The Impact of Driver Experience on Parcel value Delivered") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff")) + 
  theme(legend.position = "none", legend.title = element_blank())

ggsave(filename = "Plot2.png", plot = Plot2, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#Figure 3.5: This graph shows the relationship between Driver’s work pattern and the Parcel value delivered.
Plot3 <- ggplot(workPatternExplanatory_data, aes(x = experience_level, y = percentage_delivered, fill = work_pattern)) + 
  geom_col(position = "dodge") + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Delivered(%)",
       title = "Fig-3 The Impact of Work Pattern on Parcel value Delivered", fill = "Work Pattern") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff"))

ggsave(filename = "Plot3.png", plot = Plot3, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#Figure 3.6: This graph shows the relationship between the Driver’s time of delivery and the Parcel value delivered.
Plot4 <- ggplot(TimeDeliveryDExplanatory_data, aes(x = experience_level, y = percentage_delivered, fill = time_of_delivery)) + 
  geom_col(position = "dodge") + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Delivered(%)",
       title = "Fig-4 The Impact of Time of Delivery on Parcel value Delivered", fill = "Time of Delivery") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff"))

ggsave(filename = "Plot4.png", plot = Plot4, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#Figure 3.7: This graph shows the relationship between the Driver’s time of delivery and the Parcel value delivered.
Plot5 <- ggplot(VanTypeExplanatory_data, aes(x = experience_level, y = percentage_delivered, fill = van_type)) + 
  geom_col(position = "dodge") + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Delivered(%)",
       title = "Fig-5 The Impact of Type of Van on Parcel value Delivered", fill = "Type of Van") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff"))

ggsave(filename = "Plot5.png", plot = Plot5, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#Figure 3.8: This graph shows the relationship between the Driver’s gender and the Parcel value delivered.
Plot6 <- ggplot(genderExplanatory_data, aes(x = experience_level, y = percentage_delivered, fill = gender)) + 
  geom_col(position = "dodge") + 
  labs(x = "Driver's Experience Level",
       y = "Parcel Value Delivered(%)",
       title = "Fig-6 The Impact of Gender on Parcel value Delivered", fill = "Gender") +  
  theme_classic() + 
  scale_fill_manual(values = c("#045d5d", "#4169e1", "#addfff"))

ggsave(filename = "Plot6.png", plot = Plot6, path = "C:/Users/USER/Desktop/DDM_Assignment/1_data")

#DASHBOARD: This is the function that call all the plots and put them together as grid to form a Dashboard.
grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6)
