library(tidyr)
library(pastecs)
library(ggplot2)
library(dplyr)
# Define file path
file_path <- "C:/Users/Dell/OneDrive/Documents/datviz/mental-health/wgm_csv.csv"

# Read Excel file into a dataframe
data <- read.csv(file_path)

#	MH8D	Took Prescribed Medication When Anxious/Depressed
# MH9D Level of helpfullness of the mediaction
# change column names to meds, country and medhelpful

colnames(data)[colnames(data) == "MH7C"] <- "depr_more"
colnames(data)[colnames(data) == "MH8D"] <- "meds"
colnames(data)[colnames(data) == "COUNTRYNEW"] <- "country"
colnames(data)[colnames(data) == "MH9D"] <- "medhelpful"
#Change 3 variables to factors

data$meds <- factor(data$meds, 
                           levels = c("1", "2", "99"),
                           labels = c("Yes", "No", "Don't know"))

data$medhelpful <- factor(data$medhelpful, 
                    levels = c("1", "2", "3", "99"),
                    labels = c("Very helpful", "Somewhat helpful", "Not helpful", "Don't know"))

data$country <- factor(data$country)

data$depr_more <- factor(data$depr_more, 
                         levels = c("1", "2", "99"),
                         labels = c("Yes", "No", "Don't know"))


#Depression prevalence
#Keeping complete cases

datad <- data[complete.cases(data$depr_more),]
stat.desc(datad$depr_more)
datad$depr_more
summary(datad)

#Creating the frequency table for depr
freq_table_d <- table(datad$depr_more, datad$country)

#Converting the frequency table to a data frame
freq_df_d <- as.data.frame(freq_table_d)
colnames(freq_df_d) <- c("depr_more", "country", "count")

#Reshaping data to calculate percentages
freq_df_summary_d <- freq_df_d %>%
  group_by(country) %>%
  spread(depr_more, count, fill = 0) %>% 
  mutate(
    total_answers_d = Yes + No,  
    percent_yes_d = (Yes / total_answers_d) * 100,  
    percent_no_d = (No / total_answers_d) * 100  
  )

#Sorting by percent_yes
freq_df_summary_d <- freq_df_summary_d %>%
  arrange(percent_yes_d)

#Reshaping the data to have Yes and No percentages in the same column
freq_df_long_d <- freq_df_summary_d %>%
  gather(key = "answer", value = "percent", percent_yes_d, percent_no_d) %>%
  mutate(percent = ifelse(answer == "percent_no_d", -percent, percent)) 

#Plotting the bar chart
ggplot(freq_df_long_d, aes(x = reorder(country, percent), y = percent, fill = answer)) +
  geom_bar(stat = "identity") +  
  labs(title = "Percentage of Yes and No Answers by Country", 
       x = "Country", 
       y = "Percentage of depression more than once") +
  scale_fill_manual(values = c("percent_yes_d" = "blue", "percent_no_d" = "red")) +  
  coord_flip() +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#Taking meds for depression
#Keeping complete cases

datam <- data[complete.cases(data$meds),]
stat.desc(datam$meds)
datam$meds
summary(datam)

#Creating the frequency table for meds and depr
freq_table_m <- table(datam$meds, datam$country)

#Converting the frequency table to a data frame
freq_df_m <- as.data.frame(freq_table_m)
colnames(freq_df_m) <- c("meds", "country", "count")

#Reshaping data to calculate percentages
freq_df_summary_m <- freq_df_m %>%
  group_by(country) %>%
  spread(meds, count, fill = 0) %>%  
  mutate(
    total_answers_m = Yes + No,  
    percent_yes_m = (Yes / total_answers_m) * 100,  
    percent_no_m = (No / total_answers_m) * 100   
  )

#Sorting by percent_yes
freq_df_summary_m <- freq_df_summary_m %>%
  arrange(percent_yes_m)

#Reshaping the data to have Yes and No percentages in the same column
freq_df_long_m <- freq_df_summary_m %>%
  gather(key = "answer", value = "percent", percent_yes_m, percent_no_m) %>%
  mutate(percent = ifelse(answer == "percent_no_m", -percent, percent)) 

#Plotting the bar chart
ggplot(freq_df_long_m, aes(x = reorder(country, percent), y = percent, fill = answer)) +
  geom_bar(stat = "identity") +  
  labs(title = "Percentage of Yes and No Answers by Country", 
       x = "Country", 
       y = "Percentage") +
  scale_fill_manual(values = c("percent_yes_m" = "blue", "percent_no_m" = "red")) + 
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#Data with helpful
library(tidyverse)
#Keeping data with complete cases and no refusals 
datah <- data[complete.cases(data$medhelpful),]
stat.desc(datah$medhelpful)
datah$medhelpful

datah <- datah[datah$medhelpful != "Don't know", ]
summary(datah$medhelpful)


freq_h <- table(datah$medhelpful, datah$country)

#Calculating percentages med helpful

#Converting the frequency table to a data frame
freq_dfh <- as.data.frame(freq_h)
colnames(freq_dfh) <- c("medhelpful", "country", "count")

#Reshaping data to calculate percentages
freq_df_summaryh <- freq_dfh %>%
  group_by(country) %>%
  spread(medhelpful, count, fill = 0)  

colnames(datah)

#Calculating percentages
freq_df_summaryh <- freq_df_summaryh %>%
  mutate(
    total_answers = `Very helpful` + `Somewhat helpful` + `Not helpful`,  
    percent_very_helpful = (`Very helpful` / total_answers) * 100,
    percent_somewhat_helpful = (`Somewhat helpful` / total_answers) * 100,
    percent_not_helpful = (`Not helpful` / total_answers) * 100
  )


#Reshaping the data to have Yes and No percentages in the same column
freq_df_longh <- freq_df_summaryh %>%
  gather(key = "answer2", value = "percent", percent_very_helpful, percent_somewhat_helpful, percent_not_helpful) %>%
  mutate(percent = ifelse(answer2 == "percent_not_helpful", -percent, percent))  


#Sorting countries by "Very helpful" percentages
#freq_df_sorted <- freq_df_summary %>%
#  mutate(country = reorder(country, freq_df_summary$percent_very_helpful))

#Plotting the bar chart
ggplot(freq_df_longh, aes(x = reorder(country, percent), y = percent, fill = answer2)) +
  geom_bar(stat = "identity") +  
  labs(title = "Percentage of Responses to Medhelpful by Country", 
       x = "Country", 
       y = "Percentage") +
  scale_fill_manual(values = c(
    "percent_very_helpful" = "blue", 
    "percent_somewhat_helpful" = "green", 
    "percent_not_helpful" = "red")) +  
  coord_flip() +  
  theme_minimal()

#Merging answers
med_perc <- freq_df_summary_m
med_help_perc <- freq_df_summaryh
depr_perc <- freq_df_summary_d

library(dplyr)

#Inner join tables by country
total_datax <- inner_join(med_perc, med_help_perc, by = "country")
total_data <- inner_join(total_datax, depr_perc, by = "country")
colnames(total_data)
head(total_data)
summary(total_data)

#Creating a bar graph 
library(ggplot2)
library(dplyr)
library(tidyr)

#Transforming the data to a long format with negative values for helpfulness
data_long <- total_data %>%
  select(country, percent_yes_m, percent_yes_d, percent_very_helpful, percent_somewhat_helpful, percent_not_helpful) %>%
  pivot_longer(
    cols = starts_with("percent_"),
    names_to = "category",
    values_to = "percent"
  ) %>%
  mutate(
    percent = ifelse(category != "percent_yes_m" & category != "percent_yes_d", -percent, percent),
    category = recode(
      category,
      "percent_yes_m" = "Takes meds",
      "percent_yes_d" = "Is depressed",
      "percent_very_helpful" = "Very helpful",
      "percent_somewhat_helpful" = "Somewhat helpful",
      "percent_not_helpful" = "Not helpful"
    )
  )%>%
  #Sort ascending by percent_yes_m
  mutate(country = factor(country, levels = total_data %>% 
                            arrange(percent_yes_m) %>% 
                            pull(country)))

#Creating a bar graph
ggplot(data_long, aes(x = country, y = percent, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() + 
  scale_y_continuous(labels = abs) +  
  labs(
    title = "Responses by Country",
    x = "Country",
    y = "Percentage (%)",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Validation of the self-assessment dataset by correlating it with pharmaceutical sales data

file_path2 <- "C:/Users/Dell/OneDrive/Documents/datviz/mental-health/2023valid.csv"
data_eu <- read.csv(file_path2)

summary(data_eu)
class(data_eu$TIME_PERIOD)
as.factor(data_eu$TIME_PERIOD)
data_eu$TIME_PERIOD <- as.numeric(data_eu$TIME_PERIOD)

data_eu$OBS_VALUE[is.na(data_eu$OBS_VALUE)]
data_eu$TIME_PERIOD[is.na(data_eu$TIME_PERIOD)]

#Calculating cumulative score per country
colnames(data_eu)
colnames(data_eu)[colnames(data_eu) == "Reference.area"] <- "country"
 
data_eu[c("country","OBS_VALUE","TIME_PERIOD")]
data_eu$country <- factor(data_eu$country)
summary(as.factor(data_eu$TIME_PERIOD))
str(data_eu)

library(dplyr)
library(tidyr)

#Creating a subset of 2020 

data_eu2020 <- subset(data_eu, TIME_PERIOD == 2020)
eu2020 <- data_eu2020[c("country","OBS_VALUE")]
#Combining datasets
total_data_valid <- inner_join(total_data, eu2020, by = "country")
colnames(total_data_valid)
final <- total_data_valid[c("country", "OBS_VALUE", "percent_yes_m", "percent_very_helpful","percent_somewhat_helpful", "percent_not_helpful")]
#Creating a correlation

library(ggplot2)
library(dplyr)

#Ensuring numeric data types for OBS_VALUE and percent_yes
final <- final %>%
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE),
    percent_yes = as.numeric(percent_yes_m)
  )

#scatter plot with a correlation line
ggplot(final, aes(x = OBS_VALUE, y = percent_yes_m)) +
  geom_point(aes(color = country), size = 3) + 
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  
  labs(
    title = "Correlation Between OBS_VALUE and Percent Yes by Country",
    x = "OBS_VALUE",
    y = "Percent Yes",
    color = "Country"
  ) +
  theme_minimal()

#Correlation table

correlation_result <- cor.test(final$OBS_VALUE, final$percent_yes_m)

correlation_table <- data.frame(
  Correlation_Coefficient = correlation_result$estimate,
  P_Value = correlation_result$p.value,
  Conf_Int_Lower = correlation_result$conf.int[1],
  Conf_Int_Upper = correlation_result$conf.int[2]
)

print(correlation_table)

#There is a moderate positive correlation between OBS_VALUE and percent_yes (r = 0.52).
#The correlation is statistically significant (p-value = 0.0057), meaning that the relationship observed is unlikely to be due to random chance.
#The confidence interval suggests that the true correlation is likely to fall between approximately 0.17 and 0.75, supporting the strength of the positive relationship.

#Calculating average difference between prevalence of depression and medicine intake:
#mean percentages of "Yes" responses for depression and for meds
#Subseting the data where both 'meds' and 'depr_more' columns are "Yes"
data_depr <- datam %>%
  filter(depr_more == "Yes")
colnames(datam)

mean_depr <- mean(datam$depr_more == "Yes", na.rm = TRUE) * 100
mean_meds <- mean(datam$meds == "Yes", na.rm = TRUE) * 100
mean_meds_in_depr <- mean(data_depr$meds == "Yes", na.rm = TRUE) * 100

mean_depr
mean_meds
mean_meds_in_depr

mean_depr - mean_meds
mean_depr - mean_meds_in_depr 
#26.09% lower mean depr-mean pills
#24.29% mean depr - mean pills out of all depr (difference)

#Plotting depr
library(dplyr)
library(ggplot2)

#Groupping by country and calculating the percentage of 'Yes' answers
country_yes_percentage <- data %>%
  group_by(country) %>%
  summarise(yes_percentage = mean(depr_more == "Yes", na.rm = TRUE) * 100)

#Plotting the data
ggplot(country_yes_percentage, aes(x = reorder(country, yes_percentage), y = yes_percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  labs(title = "Percentage of 'Yes' Answers by Country", 
       x = "Country", 
       y = "Percentage of 'Yes' Answers") +
  theme_minimal()

#Plotting meds
#Groupping by country and calculating percentage of 'Yes' answers
country_yes_percentage2 <- data %>%
  group_by(country) %>%
  summarise(yes_percentage2 = mean(meds == "Yes", na.rm = TRUE) * 100)

# Plot the data
ggplot(country_yes_percentage2, aes(x = reorder(country, yes_percentage2), y = yes_percentage2)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  
  labs(title = "Percentage of 'Yes' Answers by Country", 
       x = "Country", 
       y = "Percentage of 'Yes' Answers") +
  theme_minimal()

#Plotting together
library(dplyr)
library(ggplot2)
library(tidyr)

#Grouping by country and calculating percentage of 'Yes' for both meds and depr_more
country_yes_data <- data %>%
  group_by(country) %>%
  summarise(
    meds_yes_percentage = mean(meds == "Yes", na.rm = TRUE) * 100,
    depr_more_yes_percentage = mean(depr_more == "Yes", na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = c(meds_yes_percentage, depr_more_yes_percentage), 
               names_to = "variable", 
               values_to = "yes_percentage")

#Ploting the data with two bars per country
ggplot(country_yes_data, aes(x = reorder(country, yes_percentage), y = yes_percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +  
  coord_flip() + 
  labs(title = "Percentage of 'Yes' Answers by Country for Meds and Depr_more", 
       x = "Country", 
       y = "Percentage of 'Yes' Answers") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +  
  theme_minimal()

#Correlation depression x meds

#Ensuring numeric data types for OBS_VALUE and percent_yes
total_data_num <- total_data %>%
  mutate(
    percent_yes_m = as.numeric(percent_yes_m),
    percent_yes_d = as.numeric(percent_yes_d)
  )

#Plotting scatter plot with a correlation line
ggplot(total_data_num, aes(x = percent_yes_m, y = percent_yes_d)) +
  geom_point(aes(color = country), size = 3) +  
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) + 
  labs(
    title = "Correlation Between depression and meds intake (Percent)",
    x = "meds intake",
    y = "depression",
    color = "Country"
  ) +
  theme_minimal()


#% accuracy check
print(total_data[total_data$country == "Kazakhstan", ], width = Inf)
colnames(total_data[total_data$country == "Kazakhstan",])

total_data$percent_yes_m == (total_data$Yes.x / total_data$total_answers_m) * 100 
total_data$percent_yes_d == (total_data$Yes.y / total_data$total_answers_d) * 100 
total_data$percent_very_helpful == (total_data$"Very helpful" / total_data$total_answers) * 100 
#Percentages are all TRUE. One NAN
#Addressing nan
na_rows <- total_data[is.na(total_data$percent_very_helpful), ]
print(na_rows)
print(total_data[total_data$country == "Tajikistan", ], width = Inf)

#Removing Tajikistan
total_data <- total_data[total_data$country != "Tajikistan", ]

#Changing column names to more user friendly. m = medicine intake, 
# d = depression occurance more than once, h = level of perceived medicine helpfulness

colnames(total_data)[colnames(total_data) == "Yes.x"] <- "yes_m"
colnames(total_data)[colnames(total_data) == "No.x"] <- "no_m"
colnames(total_data)[colnames(total_data) == "Don't know.x"] <- "refused_m"
colnames(total_data)[colnames(total_data) == "Very helpful"] <- "very_h"
colnames(total_data)[colnames(total_data) == "Somewhat helpful"] <- "somewhat_h"
colnames(total_data)[colnames(total_data) == "Not helpful"] <- "not_h"
colnames(total_data)[colnames(total_data) == "Don't know.y"] <- "refused_h"
colnames(total_data)[colnames(total_data) == "total_answers"] <- "total_answers_h"
colnames(total_data)[colnames(total_data) == "Yes.y"] <- "yes_d"
colnames(total_data)[colnames(total_data) == "No.y"] <- "no_d"
colnames(total_data)[colnames(total_data) == "Don't know"] <- "refused_d"

#Removing redundant columns 
colnames(total_data)

# [1] "country"  ####keep                
# [2] "yes_m"                   
# [3] "no_m"                    
# [4] "refused_m"               
# [5] "total_answers_m"         
# [6] "percent_yes_m"   ####keep        
# [7] "percent_no_m"            
# [8] "very_h"                
# [9] "somewhat_h"             
# [10] "not_h"                    
# [11] "refused_h"               
# [12] "total_answers_h"         
# [13] "percent_very_helpful"  ####keep   
# [14] "percent_somewhat_helpful"####keep 
# [15] "percent_not_helpful"     ####keep 
# [16] "yes_d"                   
# [17] "no_d"                    
# [18] "refused_d"               
# [19] "total_answers_d"         
# [20] "percent_yes_d"       ####keep     
# [21] "percent_no_d"

total_data_exp <- total_data[, c("country",
                                 "percent_yes_d", 
                                 "percent_yes_m", 
                                 "percent_very_helpful", 
                                 "percent_somewhat_helpful", 
                                 "percent_not_helpful" 
)]

#Reversing some columns' scores for the purpose of the visualization
total_data_exp$percent_very_helpful <- -total_data_exp$percent_very_helpful
total_data_exp$percent_somewhat_helpful <- -total_data_exp$percent_somewhat_helpful
total_data_exp$percent_not_helpful <- -total_data_exp$percent_not_helpful

#Region clusters

east_asia_pacific <- subset(total_data_exp, country == "Australia" | 
                             country == "Cambodia" |
                             country == "China" |
                             country == "Hong Kong" | 
                             country == "Indonesia" |
                             country == "Japan" |
                             country == "Laos" |
                             country == "Malaysia" |
                             country == "Mongolia" |
                             country == "Myanmar" |
                             country == "New Zealand" |
                             country == "Philippines" |
                             country == "South Korea" |
                             country == "Taiwan" |
                             country == "Thailand" |
                             country == "Vietnam")

europe <- subset(total_data_exp, country == "Albania" |
                             country == "Austria" |
                             country == "Belgium" |
                             country == "Bosnia Herzegovina" |
                             country == "Bulgaria" |
                             country == "Croatia" |
                             country == "Cyprus" |
                             country == "Czech Republic" |
                             country == "Denmark" |
                             country == "Estonia" |
                             country == "Finland" |
                             country == "France" |
                             country == "Georgia" |
                             country == "Germany" |
                             country == "Greece" |
                             country == "Hungary" |
                             country == "Ireland" |
                             country == "Italy" |
                             country == "Kosovo" |
                             country == "Latvia" |
                             country == "Lithuania" |
                             country == "Macedonia" |
                             country == "Moldova"  |
                             country == "Montenegro" |
                             country == "Netherlands" |
                             country == "Norway" |
                             country == "Poland" |
                             country == "Portugal" |
                             country == "Romania" |
                             country == "Russia" |
                             country == "Serbia" |
                             country == "Slovakia" |
                             country == "Slovenia" |
                             country == "Spain" |
                             country == "Sweden" |
                             country == "Switzerland" |
                             country == "Turkey" |
                             country == "Ukraine" |
                             country == "United Kingdom")

latin_america_carribbean <- subset(total_data_exp, country == "Argentina" |
                             country == "Bolivia" |
                             country == "Brazil" |
                             country == "Chile" |
                             country == "Colombia" |
                             country == "Costa Rica" |
                             country == "Dominican Republic" |
                             country == "Ecuador" |
                             country == "El Salvador" |
                             country == "Mexico" |
                             country == "Nicaragua" |
                             country == "Paraguay" |
                             country == "Peru" |
                             country == "Uruguay" |
                             country == "Venezuela") 

middle_east_north_africa <- subset(total_data_exp, country == "Algeria" |
                             country == "Bahrain" |
                             country == "Egypt" |
                             country == "Iran" |
                             country == "Iraq" |
                             country == "Israel" |
                             country == "Jordan" |
                             country == "Lebanon" |
                             country == "Malta" |
                             country == "Morocco" |
                             country == "Saudi Arabia" |
                             country == "Tunisia" |
                             country == "United Arab Emirates")

north_america <- subset(total_data_exp, country == "Canada" |
                             country == "United States")

south_asia <- subset(total_data_exp, country == "Bangladesh" |
                             country == "India" |
                             country == "Nepal" |
                             country == "Sri Lanka")
subsaharan_africa <- subset(total_data_exp, country == "Benin" |
                            country == "Burkina Faso" |
                            country == "Cameroon" |
                            country == "Congo Brazzaville" |
                            country == "Ethiopia" |
                            country == "Gabon" |
                            country == "Ghana" |
                            country == "Guinea" |
                            country == "Ivory Coast" |
                            country == "Kenya" |
                            country == "Mali" |
                            country == "Mauritius" |
                            country == "Namibia" |
                            country == "Nigeria" |
                            country == "Senegal" |
                            country == "South Africa" |
                            country == "Tanzania" |
                            country == "Uganda" |
                            country == "Zambia" |
                            country == "Zimbabwe")

central_asia <- subset(total_data_exp, country == "Kazakhstan" |
                            country == "Kyrgyzstan" |
                            country == "Tajikistan" |
                            country == "Uzbekistan")

colnames(total_data_exp)

#Example chart for east asia 
#Central Asia graph
central_asia_graph <- central_asia %>%
  group_by(country) %>%
  pivot_longer(cols = c(percent_yes_m, 
                        percent_yes_d, 
                        percent_very_helpful, 
                        percent_somewhat_helpful,
                        percent_not_helpful), 
               names_to = "variable", 
               values_to = "percent_yes")

#Plotting the data with two bars per country
ggplot(central_asia_graph, aes(x = reorder(country, percent_yes), y = percent_yes, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +  
  coord_flip() +  
  labs(title = "Percentage of Answers by Country for meds, Depr_more and helpfulnes", 
       x = "Country", 
       y = "Percentage of Answers") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightyellow", "aquamarine", "pink")) +  
  theme_minimal()

#Middle East& North Africa graph
middle_east_north_africa_graph <- middle_east_north_africa %>%
  group_by(country) %>%
  mutate(percent_yes_m_value = percent_yes_m) %>%  
  pivot_longer(cols = c(percent_yes_m, 
                        percent_yes_d, 
                        percent_very_helpful, 
                        percent_somewhat_helpful,
                        percent_not_helpful), 
               names_to = "variable", 
               values_to = "percent_yes")
#graph
ggplot(middle_east_north_africa_graph, aes(x = reorder(country, percent_yes_m_value), y = percent_yes, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +  
  coord_flip() +  
  labs(title = "Percentage of Answers by Country for meds and Depr_more and helphulness", 
       x = "Country", 
       y = "Percentage of Answers") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightyellow", "aquamarine", "pink")) +  
  theme_minimal()

##############################Calculating missing countries######################################################
#As all data, this data set has missing countries. It is not feasible to fill in all the gaps,
#however due to historical erasure of non-western and non global-north centered voices,
#and the presence of armed conflicts and occupations in 2020 when the data
#was collected, I decided that not including those voices in some form would be a further
#act of erasure. I decided to include a symbolic space with an estimation of depression percentage 
#in those countries based on regional percentages.
#  Middle East & North Africa: Libya, Syria, Yemen, Palestine
#  South Asia: Afghanistan
#  Sub-Saharan Africa: Central African Republic, Chad, Ethiopia, South Sudan, Democratic Republic of the Congo
#  Europe: Armenia, Azerbaijan

#Middle East & North Africa
middle_east_missing_mean <- mean(middle_east_north_africa$percent_yes_d) #74.45
#South Asia
south_asia_missing_mean <- mean(south_asia$percent_yes_d) #68.31
#Sub-Saharan Africa
subsaharan_africa_missing_mean <- mean(subsaharan_africa$percent_yes_d) #69.68
#Europe
europe_missing_mean <- mean(europe$percent_yes_d) #71.71

#Creating missing countries column

middle_east_north_africa <- rbind(
  middle_east_north_africa,
  data.frame(
    country = "missing_mena",
    percent_yes_d = middle_east_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

#south_asia
south_asia <- rbind(
  south_asia,
  data.frame(
    country = "missing_sa",
    percent_yes_d = south_asia_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

#subsaharan_africa
subsaharan_africa <- rbind(
  subsaharan_africa,
  data.frame(
    country = "missing_ssa",
    percent_yes_d = subsaharan_africa_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

#europe
europe <- rbind(
  europe,
  data.frame(
    country = "missing_e",
    percent_yes_d = europe_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)
#total_data_exp
total_data_exp <- rbind(
  total_data_exp,
  data.frame(
    country = "missing_mena",
    percent_yes_d = middle_east_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)


total_data_exp <- rbind(
  total_data_exp,
  data.frame(
    country = "missing_sa",
    percent_yes_d = south_asia_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

total_data_exp <- rbind(
  total_data_exp,
  data.frame(
    country = "missing_ssa",
    percent_yes_d = subsaharan_africa_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

total_data_exp <- rbind(
  total_data_exp,
  data.frame(
    country = "missing_e",
    percent_yes_d = europe_missing_mean, 
    percent_yes_m = NA, 
    percent_very_helpful = NA, 
    percent_somewhat_helpful = NA, 
    percent_not_helpful = NA              
  )
)

#Middle East& North Africa graph
middle_east_north_africa_graph <- middle_east_north_africa %>%
  group_by(country) %>%
  mutate(percent_yes_m_value = percent_yes_m) %>%  
  pivot_longer(cols = c(percent_yes_m, 
                        percent_yes_d, 
                        percent_very_helpful, 
                        percent_somewhat_helpful,
                        percent_not_helpful), 
               names_to = "variable", 
               values_to = "percent_yes")

#Replacing NA values with zero
middle_east_north_africa_graph_clean <- middle_east_north_africa_graph %>%
  mutate(percent_yes = ifelse(is.na(percent_yes), 0, percent_yes))  

ggplot(middle_east_north_africa_graph_clean, aes(x = reorder(country, percent_yes_m_value), y = percent_yes, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +  
  coord_flip() + 
  labs(title = "Percentage of Answers by Country for meds and Depr_more and helpfulness", 
       x = "Country", 
       y = "Percentage of Answers") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightyellow", "aquamarine", "pink")) + 
  theme_minimal()


##Exporting data
getwd()
setwd("C:/Users/Dell/OneDrive/Documents/datviz/mental-health/exp")

# Exporting data sets to CSV files
write.csv(total_data_exp, "total_data_exp.csv", row.names = FALSE)
write.csv(east_asia_pacific, "east_asia_pacific.csv", row.names = FALSE)
write.csv(europe, "europe.csv", row.names = FALSE)
write.csv(latin_america_carribbean, "latin_america_carribbean.csv", row.names = FALSE)
write.csv(middle_east_north_africa, "middle_east_north_africa.csv", row.names = FALSE)
write.csv(north_america, "north_america.csv", row.names = FALSE)
write.csv(south_asia, "south_asia.csv", row.names = FALSE)
write.csv(subsaharan_africa, "subsaharan_africa.csv", row.names = FALSE)
write.csv(central_asia, "central_asia.csv", row.names = FALSE)

#central_asia
#subsaharan_africa
#south_asia
#north_america
#middle_east_north_africa
#latin_america_carribbean
#europe
#east_asia_pacific

##Joining data sets

library(dplyr)

#Adding the "region" column to each data set
central_asia <- central_asia %>% mutate(region = "Central Asia")
subsaharan_africa <- subsaharan_africa %>% mutate(region = "Sub-Saharan Africa")
south_asia <- south_asia %>% mutate(region = "South Asia")
north_america <- north_america %>% mutate(region = "North America")
middle_east_north_africa <- middle_east_north_africa %>% mutate(region = "Middle East & North Africa")
latin_america_carribbean <- latin_america_carribbean %>% mutate(region = "Latin America & Caribbean")
europe <- europe %>% mutate(region = "Europe")
east_asia_pacific <- east_asia_pacific %>% mutate(region = "East Asia & Pacific")

#Combining all data sets into one
combined_data <- bind_rows(
  central_asia,
  subsaharan_africa,
  south_asia,
  north_america,
  middle_east_north_africa,
  latin_america_carribbean,
  europe,
  east_asia_pacific
)

head(combined_data)

#Exporting the data set to CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)
