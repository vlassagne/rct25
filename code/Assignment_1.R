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
library(broom)
library(tidyr)

orbis_data <- orbis_data %>%           
  mutate(equity_ratio = shfd / toas)    
colnames(orbis_data)

orbis_data <- orbis_data %>%
  mutate(postcode_13359 = if_else(postcode == "13359", 1, 0))
table(orbis_data$postcode_13359)
orbis_data$equity_ratio

t_test_result <- t.test(equity_ratio ~ postcode_13359, data = orbis_data)

tt_summary <- tidy(t_test_result)


library(knitr)
library(kableExtra)

# Select and rename columns for presentation
final_table <- tt_summary %>%
  select(
    Group1_Mean = estimate1,
    Group2_Mean = estimate2,
    t_statistic = statistic,
    df = parameter,
    p_value = p.value,
    CI_Lower = conf.low,
    CI_Upper = conf.high
  ) %>%
  mutate(
    Group1_Mean = round(Group1_Mean, 2),
    Group2_Mean = round(Group2_Mean, 2),
    t_statistic = round(t_statistic, 3),
    df = round(df, 0),
    p_value = round(p_value, 3),
    CI_Lower = round(CI_Lower, 2),
    CI_Upper = round(CI_Upper, 2)
  )

# Output the table with a caption and proper label for crossref
kable(
  final_table,
  format = "latex",
  caption = "\\label{tbl:ttest_summary} Welch Two-Sample t-test Summary with Group Means",
  booktabs = TRUE,
  digits = 3,
  na = ""
) %>%
  kable_styling(latex_options = "hold_position")

