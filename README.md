# Google Capstone Project: Bellabeat case study
**Author: Hang Vo Thuy Nguyen**

_The case study follows the six-step data analysis process:_
### ❓ [Ask](#phase-1-ask)
### 💻 [Prepare](#phase-2-prepare)
### 🛠 [Process](#phase-3-process)
### 📊 [Analyze](#phase-4-analyze)
### 📋 [Share](#phase-5-share)
### 🧗‍♀️ [Act](#phase-6-act)


## PHASE 1: Ask
### 1. Business task:
Analyze how people use non-Bellabeat smart devices to improve Bellabeat's marketing strategy.

### 2. Stakeholders:
• Urška Sršen: Bellabeat’s co-founder and Chief Creative Officer

• Sando Mur: Mathematician and Bellabeat’s cofounder; a key member of the Bellabeat executive team

• Bellabeat marketing analytics team

## PHASE 2: Prepare
### 2.1 Loading packages
```R
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(tidyr)
```
### 2.2 Import datasets
```R
daily_activity <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_intensities <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
daily_steps <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
sleep <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourly_intensities <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read.csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
```
### 2.3 Previewing datasets
```R
head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(sleep)
head(hourly_intensities)
head(hourly_steps)
```
## PHASE 3: Process
### 3.1 Cleaning and formatting
```R
daily_activity$Ymd <- as.Date( daily_activity$ActivityDate, format=" %m/%d/%Y")
str(daily_activity)

daily_steps$Ymd <- as.Date(daily_steps$ActivityDay, format=" %m/%d/%Y")
str(daily_steps)

daily_intensities$Ymd <- as.Date(daily_intensities$ActivityDay, format=" %m/%d/%Y")
str(daily_intensities)

daily_calories$Ymd <- as.Date(daily_calories$ActivityDay, format=" %m/%d/%Y")
str(daily_calories)

sleep$Ymd <- as.Date(sleep$SleepDay, format=" %m/%d/%Y")
str(sleep)

hourly_intensities$ActivityHour=as.POSIXct(hourly_intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_intensities$time <- format(hourly_intensities$ActivityHour, format = "%H:%M:%S")
hourly_intensities$date <- format(hourly_intensities$ActivityHour, format = "%m/%d/%y")
str(hourly_intensities)

hourly_steps$ActivityHour=as.POSIXct(hourly_steps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_steps$time <- format(hourly_steps$ActivityHour, format = "%H:%M:%S")
hourly_steps$date <- format(hourly_steps$ActivityHour, format = "%m/%d/%y")
str(hourly_steps)
```
#### 3.1.1 Verifying the number of participants
We want to know how many different people are represented in each data frame. We will keep the sleep dataset for practice even though it has fewer than 30 unique users.
```R
n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(sleep$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
```
33

33

33

33

24

33

33
#### 3.1.2 Duplicates and remove duplicates
Looking for any duplicate data.
```R
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(daily_steps))
sum(duplicated(sleep))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
```
0

0

0

0

3

0

0
The previous code showed that the sleep dataset has only duplicate records. We will remove these duplicates from the sleep dataset.
```R
sleep <- sleep %>%
  distinct() %>%
  drop_na()
```
Verifying output again that duplicates have been removed
```R
sum(duplicated(sleep))
```
0
### 3.2 Merging datasets
```R
colnames(daily_activity)
colnames(sleep)
```
'Id''ActivityDate''TotalSteps''TotalDistance''TrackerDistance''LoggedActivitiesDistance''VeryActiveDistance''ModeratelyActiveDistance''LightActiveDistance''SedentaryActiveDistance''VeryActiveMinutes''FairlyActiveMinutes''LightlyActiveMinutes''SedentaryMinutes''Calories''Ymd'

'Id''SleepDay''TotalSleepRecords''TotalMinutesAsleep''TotalTimeInBed''Ymd'
```R
colnames(daily_activity) <- gsub("\\s", "", colnames(daily_activity))
colnames(sleep) <- gsub("\\s", "", colnames(sleep))
```
```R
merged_data <- merge(sleep, daily_activity, by = "Id")
glimpse(merged_data)
```
## PHASE 4: Analyze
```R
# activity
daily_activity %>%  
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>%
  summary()
# the number of hours per category
daily_activity_hours <- daily_activity %>%
  mutate(SedentaryHours = round(SedentaryMinutes / 60, 1),
         VeryActiveHours = round(VeryActiveMinutes / 60, 1),
         FairlyActiveHours = round(FairlyActiveMinutes / 60, 1),
         LightActiveHours = round(LightlyActiveMinutes / 60, 1)) %>%
 select(SedentaryHours, VeryActiveHours, FairlyActiveHours, LightActiveHours)
 summary(daily_activity_hours)

# the number of active minutes per category
daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/48eb8794-eb43-49c0-af78-a6a359a5b207)

  **There are some interesting findings from the output:**
* The average person takes 7,638 steps per day, which is a little bit less than the recommended number of 10,000 steps per day by the American Heart Association. The American Heart Association recommends 10,000 steps per day because it is associated with a number of health benefits, including a reduced risk of heart disease, stroke, type 2 diabetes, obesity, and some types of cancer.
* Average calorie consumption is 2304
* The average participant's sedentary time is 991 minutes, equivalent to 16 hours.
* The majority of the participants are lightly active. The output represents that the average of light active hours is more than 3 hours, compared to the average of very active hours and the average of fairly hours hours at 0.3 hours and 0.2 hours respectively
```R
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

sleep_hours <- sleep %>%
 mutate(TotalHoursAsleep = round (TotalMinutesAsleep / 60, 1),
        TotalHoursInBed = round(sleep$TotalTimeInBed / 60, 1)) %>%
 select(TotalHoursAsleep, TotalHoursInBed)
 summary(sleep_hours)
 ```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/1c3d1ca9-40ff-4c80-b5c2-778429276d19)
 * On average, each participant slept nearly 7 hours, equivalent to 419.2 minutes
## PHASE 5: Share
### 5.1 Total Steps vs. Calories
```R
ggplot(data=daily_activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth(color = "red") +
  labs(title="Total Steps vs. Calories") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/60f51728-28da-4e97-9da2-0e7312429c09)

There is a positive correlation between the number of steps taken and the number of calories burned (Total Steps vs Calories). This is to be expected, as the more active we are, the more energy our bodies use.
### 5.2 Total Steps vs. Minutes Asleep
```R
ggplot(data = merged_data, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_jitter() +
  geom_smooth(color="red") +
  labs(title = "Total Steps vs. Minutes Asleep", x = "Total Steps", y = "Minutes Asleep") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/515d7546-c17d-4a96-8210-d3b5a8b2ce10)

There is no correlation between users's daily steps and the amount of minutes asleep per day (Daily Steps and Minutes Asleep).
### 5.3  Activity vs. Calories
```R
ggplot(data=daily_activity, aes(x=LightlyActiveMinutes, y=Calories)) + 
  geom_point() + geom_smooth(color = "red") +
  labs(title="LightlyActiveMinutes vs. Calories") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/360bb098-28d0-4aca-b4a4-a8433b91ed06)

```R
ggplot(data=daily_activity, aes(x=FairlyActiveMinutes, y=Calories)) + 
  geom_point() + geom_smooth(color = "red") +
  labs(title="FairlyActiveMinutes vs. Calories") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/5cb0c568-f83e-4f54-a2b4-5da9ed5bac10)
```R
ggplot(data=daily_activity, aes(x=VeryActiveMinutes, y=Calories)) + 
  geom_point() + geom_smooth(color = "red") +
  labs(title="VeryActiveMinutes vs. Calories") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/bc57fc37-ab67-4e80-97ff-10c16bdfe31b)
```R
ggplot(data=daily_activity, aes(x=SedentaryMinutes, y=Calories)) + 
  geom_point() + geom_smooth(color = "red") +
  labs(title="SedentaryMinutes vs. Calories") +
  theme_classic()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/3c674563-70be-4232-bb20-2e882aa4d017)

There are many positive correlations between the following relationships: LightlyActiveMinutes vs. Calories, FairlyActiveMinutes vs. Calories, VeryActiveMinutes vs. Calories. However, there is a negative correlation between SedentaryMinutes vs. Calories.
### 5.4 Sleep data collection and distribution
```R
ggplot(data = sleep_hours) +
  geom_histogram(
    mapping = aes(x = TotalHoursAsleep), color="black", fill="lightpink",
    bins = 30, show.legend = FALSE) +
  labs(title = "Distribution of sleep records", x = 'Hours Asleep', y = "Count") +
  geom_vline(aes(xintercept=7), linetype = "dashed", color = "green") +
  annotate("text", x=5, y=50, label="7 hours asleep", fontface = "bold", color = "dark blue") +
  theme_light()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/25964ced-f7f6-42ca-af34-bbd7d7b518a0)

The visualization shows that most participants sleep for an average of 7 hours. Additionally, many participants sleep between 6 and 9 hours, which is the recommended amount of sleep for adults. This suggests that most participants are getting enough sleep, which is important for good health.
### 5.5 Typical amount of time spent on apps
```R
daily_activity$total_time = rowSums(daily_activity[c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes","SedentaryMinutes")])

daily_activity %>% 
  group_by(Id) %>% 
  summarise(daily_usage_hours = mean(total_time/60)) %>% 

  ggplot() + 
  geom_histogram(mapping = aes(x=daily_usage_hours), color = "black", fill = "orange", 
  bins = 30, show.legend=FALSE) +
  labs(title="Average App Usage Time (Hours)", x = "App Usage Time")+
  theme_light()
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/7348f626-d7e8-4088-88a1-21f9a72c1084)

According to the visualization, users wear devices almost all day, even when they are sleeping.
### 5.6 Proportion frequency of smart device use 
```R
daily_usage <- daily_activity %>%
  group_by(Id) %>%
  summarise(daily_usage_hours = mean(total_time / 60)) %>%
  mutate(usage = if_else(daily_usage_hours >= 17, "high", "low"))

daily_usage %>%
  count(usage) %>%
  mutate(percentage = n * 100 / sum(n)) %>%
  ggplot(aes(x = "", y = percentage, fill = usage)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = round(percentage)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("lightblue", "pink"),
                    labels = c("High use: > 17 hours", "Low use: < 17 hours")) +
  labs(title = "Percentage frequency of daily usage level of device") +
  theme_void() 
  ```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/cdb6af2d-33f8-4ba4-b1d7-3814c5450e8a)

  The pie chart represents that 76% of users wear devices, while only 24% do not.
  ### 5.7 Average hourly intensity over time
  ```R
  hourly_intensities %>%
  group_by(time) %>%
  summarise(Avg_hourly_int = mean(TotalIntensity)) %>%

  ggplot(aes(x = time, y = Avg_hourly_int)) +
  geom_histogram(aes(fill= Avg_hourly_int), stat="identity")+ 
  scale_fill_gradient(low = "yellow", high = "lightgreen") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Total Intensity vs. Time", x= "Time", y="Mean Total Intensity")
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/f6cf160a-a779-4db5-80a8-4a863ce71c83)
* Most participants active between 6am and 10pm
* The peak of activity intensity occurs during the late afternoon, from 5pm to 7pm. At that time period, almost people are finishing work and going to the gym or for a walk.
### 5.8 Average hourly steps over time
```R
hourly_steps %>%
 group_by(time) %>%
 summarise(Avg_hourly_steps = mean(StepTotal)) %>%

 ggplot(aes(x = time, y = Avg_hourly_steps)) +
 geom_histogram(aes(fill = Avg_hourly_steps), stat = "identity") +
 scale_fill_gradient(low = "pink", high = "lightblue") +
 theme_light() +
 theme(axis.text.x = element_text(angle = 90)) +
 labs(title = "Average Steps Hourly", x="Activity Hour", y="Mean Total Steps")
```
![image](https://github.com/Hannahnv/Google-Capstone-Project-Bellabeat-case-study/assets/102349995/03d9dd09-e4bc-4687-a26d-f4c85539b7b6)

* Most users walk during the day, from 7am to 8pm.
* The most popular time to walk is in the late afternoon, from 5pm to 7pm.

## Bellbeat Data Analysis Dashboard on Tableau
### 🎨 [Bellabeat Dashboard](https://public.tableau.com/app/profile/hang.nguyen6427/viz/BellabeatDashboard_16861590988190/Dashboard1)
## PHASE 6: Act
|Product| Features | Description and recommendation |
|---| ---| ---|
|1. Bellabeat App| Bellabeat App: Provides health data related activity, sleep, stress, menstrual cycle, and mindfulness habits| The average users of Bellabeat app walk more than 7,600 steps per day. We can encourage them to reach the American Heart Association's recommended daily step goal of 10,000. **I recommend sending friendly reminders if they haven't reached that number of steps and by creating posts on app explaining the benefits of reaching that goal**.
|2. Bellabeat Leaf| Tracks activity, heartrate, sleep, and stress| First of all, 24% participants do not wear devices almost all day, even when they are sleeping. **I recommend using Bellabeat Leaf that can be worn as necklaces more than clips and bracelets could help to collect more data about sleep, which could be used to improve sleep quality and overall health.** Second, the intensity of an activity directly affects the number of calories burned. Additionally, the number of steps taken each day is positively correlated with the number of calories burned. Most users burn 2,000 calories per day, which is the daily intake recommendation by the FDA. **I recommend personalized workout help users track and achieve their fitness goals.** 
|3. Bellabeat Time| Tracks activity, heartrate, sleep, and stress| Users sleep less than 8 hours per day. **I recommend emphasizing healthy sleep patterns and rewarding users for daytime activity, especially from 5pm to 7pm. In addition, Offering helpful resources to help users sleep, such as breathing exercises, podcasts with relaxing music, and sleep techniques.**


