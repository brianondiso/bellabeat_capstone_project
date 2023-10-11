#libraries --------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(outliers)


daily_activity <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep_day <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
heart_rate_seconds <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
weight_log <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
hour_calories <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hour_intensitities <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hour_steps <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

head(daily_activity, 2)
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(heart_rate_seconds$Id)
n_distinct(weight_log$Id)

daily_activity %>%  
  select(TotalSteps,SedentaryMinutes,Calories,TotalDistance) %>% 
  summary()

sleep_day %>% 
  select(TotalTimeInBed, TotalMinutesAsleep) %>% 
  summary()

weight_log %>% select(WeightKg, BMI) %>% summary()

weight_log_bmi <- weight_log %>%
  mutate(user_type = case_when(
    BMI < 17 ~ "thinness",
    BMI < 18.5 ~ "underweight",
    BMI >= 18.5 & BMI < 24.9 ~ "normal weight",
    BMI >= 25 ~ "overweight",
    BMI >= 30 ~ "obesity" 
  ))

head(weight_log_bmi)

percentage_bmi <- weight_log_bmi %>%
  group_by(user_type) %>%
  summarise(total= n()) %>%
  mutate(total_percent = scales::percent (total/sum(total)))

head(percentage_bmi)

ggplot(percentage_bmi, aes(x = user_type, y = total_percent)) +
  geom_boxplot() +
  labs(
    x = "weight_category",
    y = "BMI Percentage",
    title = "Box Plot of BMI Percentages by Weight Category"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=daily_activity, 
       aes(x=TotalSteps, y=Calories, colour = Calories)) + 
  geom_point()+
  labs(title = "Steps taken vs Calories", subtitle = "Relationship between 
       steps taken per day and Calories burnt")

ggplot(data=sleep_day, 
       aes(x=TotalMinutesAsleep, y=TotalTimeInBed, alpha = TotalMinutesAsleep, alpha = TotalTimeAsleep)) + 
  geom_point()+ labs(title = "Total Time in Bed vs Time Asleep")



combined_data <- merge(sleep_day, daily_activity, by = "Id", all.x = TRUE, all.y = TRUE)
n_distinct(combined_data$Id)
combined_data_clean <- na.omit(combined_data)

ggplot(combined_data, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point() +
  labs(
    x = "Total Minutes Asleep",
    y = "Sedentary in Minutes",
    title = "Relationship Between Sleep Duration and Sedentary Minutes"
  )



combined_data_clean <- combined_data %>% 
  group_by (Id) %>% 
  summarise(avg_daily_steps= mean(TotalSteps), 
            avg_daily_cal= mean(Calories), 
            avg_daily_sleep= mean(TotalMinutesAsleep, 
                                  na.rm = TRUE)) %>% 
  mutate(user_type= case_when(
    avg_daily_steps < 5000 ~ "sedentary",
    avg_daily_steps >= 5000 & avg_daily_steps <7499 ~"lightly active",
    avg_daily_steps >= 7499 & avg_daily_steps <9999 ~"fairly active",
    avg_daily_steps >= 10000 ~"very active"
  ))

head(combined_data_clean)

percentages_user_type_steps <- combined_data_clean %>%
  group_by(user_type) %>%
  summarise(total= n()) %>%
  mutate(total_percent= scales::percent (total/sum(total)))


head(percentages_user_type_steps)

ggplot(combined_data_clean[which(combined_data_clean$avg_daily_steps>0),], 
       aes(user_type,avg_daily_steps, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Box Plot of the Daily Steps by User Type", 
       x= " ", y="total steps")+
  scale_fill_brewer(palette="BuPu")

#dealing with dates -load lubridate()
daily_activity_clean <- daily_activity %>%
  na.omit() %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) 


head(daily_activity_clean)

#create weekday column
Activity_by_week <- daily_activity_clean %>%
  mutate(weekday = weekdays(ActivityDate))

Activity_by_week$weekday <-ordered(Activity_by_week$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                      "Friday", "Saturday", "Sunday"))

head(Activity_by_week)

# Define custom colors and labels
custom_colors_steps <- c('#ff99cc', '#ff6666', '#99ff99', '#cccccc', '#ffcc99', '#33ccff', '#99ccff')
weekday_labels <- unique(Activity_by_week$weekday)

ggplot(Activity_by_week) +
  geom_col(aes(weekday, TotalSteps, fill = weekday)) +
  scale_fill_manual(values = custom_colors_steps, breaks = weekday_labels, labels = weekday_labels) +  # Set custom colors and labels
  labs(title = "Daily Steps per Weekday", x = "Weekdays", y = "Number of Steps") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2)) +
  guides(fill = guide_legend(title = "Weekday"))  # Add legend title


custom_colors_r <- c('#ff99cc', '#ff6666', '#99ff99', '#cccccc', '#ffcc99', '#33ccff', '#99ccff')

ggplot(Activity_by_week) +
  geom_col(aes(weekday, SedentaryMinutes, fill = weekday)) +
  scale_fill_manual(values = custom_colors_r) +  # Set custom colors
  labs(title = "Daily Sedentary Period (Minutes)", x = "Weekdays", y = "Sedentary Minutes") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2))

# Define custom colors
custom_colors <- c('#33ccff', '#ff6666', '#99ff99', '#ffcc99', '#cccccc', '#ff99cc', '#99ccff')

ggplot(Activity_by_week) +
  geom_col(aes(weekday, VeryActiveMinutes, fill = weekday)) +
  geom_text(aes(weekday, VeryActiveMinutes, label = VeryActiveMinutes), vjust = -0.5) +  # Add labels
  labs(title = "Very Active Days per Weekday", x = "Weekdays", y = "Very Active Minutes") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2))

