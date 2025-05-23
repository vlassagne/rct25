setwd("~/Desktop/rct25")

library(dyplr)

#### Step 3 ####
orbis_data <- readRDS("data/generated/orbis_panel_berlin.rds")

str(orbis_data)
summary(orbis_data)

data_postcode_13359 <- orbis_data %>%
  filter(postcode == "13359")

EventRental_data <- data_postcode_13359 %>%
  filter(name_native == "EventRental Hempel GmbH")
str(EventRental_data)
summary(EventRental_data)


#### Step 4 ####
data_postcode_13359_year_2021 <- data_postcode_13359 %>%
  filter(year == 2021)

str(data_postcode_13359_year_2021)
colnames(data_postcode_13359_year_2021)

data_sorted_by_toas <- data_postcode_13359_year_2021 %>%
  arrange(desc(toas))

largest_firm_13359_2021 <- as.data.frame(t(data_sorted_by_toas[1, ])) # largest total assets


#### Step 5 ####
data_postcode_13359 <- data_postcode_13359 %>%           
  mutate(equity_ratio = shfd / toas)    
colnames(data_postcode_13359)

orbis_data <- orbis_data  %>%  
  mutate(equity_ratio = shfd / toas)

t.test(data_postcode_13359$equity_ratio, orbis_data$equity_ratio) 
t.test(data_postcode_13359$toas,         orbis_data$toas)          




