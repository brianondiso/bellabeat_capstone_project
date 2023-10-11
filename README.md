
# BellaBeat Google Data Analytics Capstone Project
---
#### Title: Bellabeat_case_study
#### Author: Brian Ondiso
#### Date: 2023-10-07

---

## About The Company
Bellabeat, a femtech company with a strong focus on data-driven wellness, is known for creating stylish and technologically advanced wellness products.
They have also established themselves as a rapidly growing provider of wellness subscription services tailored for women.
The popularity of their Leaf and Ivy wearables has allowed the brand to enter global markets and initiate conversations within the industry about the significance of innovation
in the design and creation of tech products.

## 1. Ask
Key Task: Analyze smart device usage data in order to gain insight and help guide marketing strategy for Bellabeat to reveal more opportunities for growth.

## Key Stakeholders

Urška Sršen: Bellabeat’s co-founder and Chief Creative Officer
Bellabeat marketing analytics team

## 2. Prepare
Data Source: [Fitbit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit/data) by MÖBIUS Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. Individual reports can be parsed by export session ID (column A) or timestamp (column B). Variation between output represents the use of different types of Fitbit trackers and individual tracking behaviors/preferences. The data set contains 18 CSV files.


![FitBit Fitness Tracker Data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/image_data_cover.webp)

## Roccc Analysis

* Reliability: Low – dataset was collected from 30 individuals whose gender is unknown.
* Originality: Low – third-party data collected using Amazon Mechanical Turk.
* Comprehensive: Medium – dataset contains multiple fields on daily activity intensity, calories used, daily steps taken, daily sleep time and weight record.
* Current: Low – This data is from May 2016, which means data is not current and user habits may have    changed over the period of time.
* Limitations of the dataset

Data of only 30 users is available, a larger sample size is preferred for the more accurate analysis.
The age of the users is unknown. Therefore ideal judgment criteria can not be defined.
Only 8 users have submitted their weight and 14 unique users submitted their heart rate 
reading.


## Process

## Installing and loading common packages and libraries
```{r }
library(tidyverse)
library(janitor)
library(here)
library(lubridate)

```
## Upload your CSV files to R
Remember to upload your CSV files to your project from the relevant data source:
[FitBit data] (https://www.kaggle.com/arashnic/fitbit)
```{r Upload your CSV files to R }
daily_activity <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep_day <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
heart_rate_seconds <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
weight_log <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
hour_calories <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hour_intensitities <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hour_steps <- read.csv("Data/FitBit Data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

````
## Exploring a few key tables
```{r}
head(daily_activity, 2)

```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_daily_activity.png)
```{r}
glimpse(sleep_day)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_daily_activity.png)
```{r}
glimpse(heart_rate_seconds)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_heart_rate_seconds.png)
```{r}
glimpse(hour_calories)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_hour_calories.png)
```{r}
glimpse(hour_intensitities)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_hour_intensitities.png)
```{r}
glimpse(hour_steps)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_hour_steps.png)
```{r}
glimpse(weight_log)
```
![head data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpse_weight_log.png)
## How many unique participants are there in each dataframe?
```{r}
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(heart_rate_seconds$Id)
n_distinct(weight_log$Id)

```
![distinc_participants](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/unique_users.png)

There are 33 participants in the daily activity data, 24 participants in the sleep data 14 users in the heart rate records and 8 participants in recorded their weights.

What are some quick summary statistics we'd want to know about each data frame?
```{r}
daily_activity %>%  
  select(TotalSteps,SedentaryMinutes,Calories,TotalDistance) %>% 
  summary()

```
![d_data summary](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/sum_daily_data.png)
```{r}
sleep_day %>% 
  select(TotalTimeInBed, TotalMinutesAsleep) %>% 
  summary()
```
![sleepdaysum](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/sleep_day_sum.png)
```{r}
weight_log %>% select(WeightKg, BMI) %>% summary()
```
![weightlogsum](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/weightsum.png)

BMI is a simple index of weight-to-height that is commonly used to classify underweight, overweight and obese in adults

We will put  them into 5 categories
BMI <17.0: thinness
BMI <18.5: underweight
BMI 18.5-24.9: normal weight
BMI ≥25.0: overweight
BMI ≥30.0: obesity.

```{r}
weight_log_bmi <- weight_log %>%
  mutate(user_type = case_when(
    BMI < 17 ~ "thinness",
    BMI < 18.5 ~ "underweight",
    BMI >= 18.5 & BMI < 24.9 ~ "normal weight",
    BMI >= 25 ~ "overweight",
    BMI >= 30 ~ "obesity" 
  ))

head(weight_log_bmi)
```
![weightlogbmi](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/weight_log_bmihead.png)

We put them in percentage
```{r}
percentage_bmi <- weight_log_bmi %>%
  group_by(user_type) %>%
  summarise(total= n()) %>%
  mutate(total_percent = scales::percent (total/sum(total)))

head(percentage_bmi)
```
![percentage_bmi](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/percentage_bmihead.png)

Now we visualize the findings for the presentation

```{r}
ggplot(percentage_bmi, aes(x = user_type, y = total_percent)) +
  geom_boxplot() +
  labs(
    x = "weight_category",
    y = "BMI Percentage",
    title = "Box Plot of BMI Percentages by Weight Category"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
![boxplot](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/Rplotbmipcnt.png)

This shows that 49.3% are overweight. 
Bellabeat can invest in weight loss supplements and also send articles about weight management to their 
customers.

## Plotting a few explorations
The relationship between steps taken in a day and the Calories scatter plot shows a positive relationship, meaning the more steps take in a day, the more calories burnt.
```{r}
ggplot(data=daily_activity, 
       aes(x=TotalSteps, y=Calories, colour = Calories)) + 
  geom_point()+
  labs(title = "Steps taken vs Calories", subtitle = "Relationship between 
       steps taken per day and Calories burnt")
```

![stepsvscalories](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/Rplotsstpsvscalories.png)
## What's the relationship between minutes asleep and time in bed?


```{r}
ggplot(data=sleep_day, 
       aes(x=TotalMinutesAsleep, y=TotalTimeInBed, alpha = TotalMinutesAsleep)) + 
  geom_point()+ labs(title = "Total Time in Bed vs Time Asleep")
```
![timeinbed](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/timeinbedasleep.png)
The relationship is not completely linear, indicating that individuals are spending more time 
in bed but are not sleeping.
The company can invest in creating an App that locks you out of your phone for a period of 7 hours.

## Merging datasets together, sleep day and daily activity by Id column
```{r}
combined_data <- merge(sleep_day, daily_activity, by = "Id", all.x = TRUE, all.y = TRUE)

```

#Exploring the combined data
```{r}
combined_data_clean <- na.omit(combined_data)
```

Relationship between sedentary minutes and sleep duration.

```{r}
ggplot(combined_data, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point() +
  labs(
    x = "Total Minutes Asleep",
    y = "Sedentary in Minutes",
    title = "Relationship Between Sleep Duration and Sedentary Minutes"
  )
```

![ggplot_combined_data](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/ggplot_combined_data.png)


## Steps
Summary Statistics for Combined Data
```{r}
combined_data %>% 
  select(TotalSteps) %>% 
  summary()
```
![steps_summary](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/sumary_total_steps.png)

The average number of steps taken by candidates is 8124. The maximum steps taken is 36019.

We will classify them into four groups such as:
For general fitness, most adults should aim for 10,000 steps per day, with fewer than 5,000 steps being a sign of a sedentary lifestyle. However, the number will depend on a person's age, current fitness level, and health goals. This recommendation comes from the Centers for Disease Control and Prevention (CDC)

Sedentary - less than 5,000 steps a day.
Lightly active - between 5,000 and 7,500 steps per day.
fairly active - between 7,500 and 10,000 steps per day.
Very active - more than 10,000 steps per day

```{r}
combined_data_clean <- combined_data_clean %>% 
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

```
![usertpe](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/user_type_activity_level.png)

```{r}
percentages_user_type_steps <- combined_data_clean %>%
group_by(user_type) %>%
summarise(total= n()) %>%
mutate(total_percent= scales::percent (total/sum(total)))

head(percentages_user_type_steps)
```
![pcntusertype](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/head_pcnt_user_type.png)


The summary shows 29% of the participants are living a sedentary life. 
We visualize the results;

```{r}
ggplot(combined_data_clean[which(combined_data_clean$avg_daily_steps>0),], 
       aes(user_type,avg_daily_steps, fill=user_type))+
  geom_boxplot()+
  stat_summary(fun="mean", geom="point", 
               shape=23,size=2, fill="white")+
  labs(title= "Box Plot of the Daily Steps by User Type", 
       x= " ", y="total steps")+
  scale_fill_brewer(palette="BuPu")
```
![usertpe](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/boxplt_usertype.png)

## We try to find out the most active days of the week

But first we need to glimpse at the data;
```{r}
glimpse(daily_activity)
```
![usertpe](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/glimpsedaily_data.png)

We convert the Activity date to date format

```{r}
#dealing with dates -load lubricate()
daily_activity_clean <- daily_activity %>%
  na.omit() %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) 


head(daily_activity_clean)

```
![head_daily_activ](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/dates.png)

Create a weekday column;

```{r}
Activity_by_week <- daily_activity_clean %>%
  mutate(weekday = weekdays(ActivityDate))

Activity_by_week$weekday <-ordered(Activity_by_week$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
"Friday", "Saturday", "Sunday"))
 
head(Activity_by_week)
```
![week_activity](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/wee_day.png)

We visualize our findings;
```{r}
# Define custom colors and labels
custom_colors_steps <- c('#ff99cc', '#ff6666', '#99ff99', '#cccccc', '#ffcc99', '#33ccff', '#99ccff')
weekday_labels <- unique(Activity_by_week$weekday)

ggplot(Activity_by_week) +
  geom_col(aes(weekday, TotalSteps, fill = weekday)) +
  scale_fill_manual(values = custom_colors_steps, breaks = weekday_labels, labels = weekday_labels) +  # Set custom colors and labels
  labs(title = "Daily Steps per Weekday", x = "Weekdays", y = "Number of Steps") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2)) +
  guides(fill = guide_legend(title = "Weekday"))  # Add legend title

```

![](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/activity_by_week.png)

We can use the dame to visualize sedentary by week.

```{r}

custom_colors_r <- c('#ff99cc', '#ff6666', '#99ff99', '#cccccc', '#ffcc99', '#33ccff', '#99ccff')

ggplot(Activity_by_week) +
  geom_col(aes(weekday, SedentaryMinutes, fill = weekday)) +
  scale_fill_manual(values = custom_colors_r) +  # Set custom colors
  labs(title = "Daily Sedentary Period (Minutes)", x = "Weekdays", y = "Sedentary Minutes") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2))

```
![](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/sedetary_weekbarplot.png)


```{r}
# Define custom colors
custom_colors <- c('#33ccff', '#ff6666', '#99ff99', '#ffcc99', '#cccccc', '#ff99cc', '#99ccff')

ggplot(Activity_by_week) +
  geom_col(aes(weekday, VeryActiveMinutes, fill = weekday)) +
  geom_text(aes(weekday, VeryActiveMinutes, label = VeryActiveMinutes), vjust = -0.5) +  # Add labels
  labs(title = "Very Active Days per Weekday", x = "Weekdays", y = "Very Active Minutes") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.2))

```

![va](https://github.com/brianondiso/bellabeat_capstone_project/blob/main/images/ver_active_barplt.png)


The most active days start on Monday and keep dropping throughout the week with Sunday having the 
lowest activity.


## Act Phase

## Conclusion
There is a noticeable trend where user activity is highest on Tuesday, gradually declining as the week progresses. This pattern indicates a normal activity where activities begin on Monday peaking on Tuesday.


Users are most inactive on Fridays, on average, they spend 16.52 hours in sedentary activities, significantly exceeding the NIH's recommended target of less than 8 hours.
49.3 percent are overweight.
This indicates a high level of inactivity, which can have adverse health implications.

There is a negative correlation between sedentary hours and sleep duration, suggesting that increased sedentary time is not associated with reduced sleep quality.


Recommendations for Bellabeat Marketing Strategy:

Develop features and notifications in the fitness watch and app that encourage users to increase their daily activity.
Segment users to offer challenges, rewards, and reminders to meet the personalized activity goals based on each user's fitness level and progress.

Implement features that help users reduce sedentary hours. These could include information on standing desks for work, and reminders to stand or move after prolonged inactivity. Offer features that track and display sedentary time, helping users become more aware of their habits.

Highlight the sleep-tracking features of the fitness watches in marketing materials. Promote the connection between time in bed and time a sleep. Educate users about the benefits of adequate sleep for overall health and introduce apps that lock you out of your phone when you are in bed.

Develop marketing campaigns and resources aimed at heavy users. Showcase specialized fitness plans, nutritional guidance, and support services tailored to their needs.

