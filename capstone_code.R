##installing packages to make sure I have the tools I need to clean and load my data effectively
install.packages('tidyverse')
library('tidyverse')
library(dplyr)
library(lubridate)
##reading in readr so I can use read_csv function so I can use col_types function while reading in weight_log_info
library(readr)
library(ggplot2)

##my first step is loading the CSVs I will be analyzing from the FitBit dataset
##the first is dailyActivity_merged.csv (which has two parts)
##my uploads will be separated by blank spaces and comments
##The following 2 CSVs were uploaded and transformed according to Coursera's provided code:
daily_activity_1 <- read.csv("data/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
head(daily_activity_1)
colnames(daily_activity_1)
n_distinct(daily_activity_1$Id)
daily_activity_1 %>% select(TotalSteps, TotalDistance, SedentaryMinutes) %>% summary()
ggplot(data=daily_activity_1, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()
summary(daily_activity_1$ActivityDate)
daily_activity_1$ActivityDate <- as.Date(daily_activity_1$ActivityDate,format="%m/%d/%Y")
head(daily_activity_1)
colnames(daily_activity_1)

sleep_day <- read.csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
head(sleep_day)
colnames(sleep_day)
n_distinct(sleep_day$Id)
sleep_day %>% select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% summary()
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()
sleep_day$SleepDay <- as.Date(sleep_day$SleepDay,format = "%m/%d/%Y %I:%M:%S %p")
class(sleep_day$SleepDay)
view(sleep_day)

##After uploading the previous CSVs according to Coursera's recommendations, I decided to upload the second half of dailyActivity_merged.csv
##The following code is mine:
daily_activity_2 <- read.csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
head(daily_activity_2)
colnames(daily_activity_2)
class(daily_activity_2$ActivityDate)
daily_activity_2$ActivityDate <- as.Date(daily_activity_2$ActivityDate,format = "%m/%d/%Y")
class(daily_activity_2$ActivityDate)
summary(daily_activity_2$ActivityDate)
n_distinct(daily_activity_2$Id)

##After uploading the second half of dailyActivity_merged.csv and checking both of their distinct Id#s,
##I found that 1 had 35 unqiue Ids and 2 had 33 unique Ids, likely meaning that 2 respondents stopped using their FitBits at some point altogether
##After double checking colnames and ensuring that dates were stored correctly as "date" rather than "character",
##I used rbind() to join the datasets, and then organized them by Id and ActivityDate
daily_activity <- rbind(daily_activity_1, daily_activity_2)
daily_activity <- daily_activity[order(daily_activity$Id, daily_activity$ActivityDate),]
view(daily_activity)

##The next CSVs I decided to upload were the 2 weightLogInfo_merged.csv s
##because I had just handled merging 2 CSVs, so I wanted to merge the other set while the process was fresh in my mind
##My experience in the first case allowed me to streamline my code this time around:
##NOTE: I had to update the end of the read.csv command to ensure that the CSVs were loaded in with LogId being a char value
##EDIT: needed to change read.csv to read_csv to accomodate col_types
weight_log_info_1 <- read_csv("data/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv", col_types = cols(LogId = col_character()))
head(weight_log_info_1)
colnames(weight_log_info_1)
class(weight_log_info_1$Date)
weight_log_info_1$Date <- as.Date(weight_log_info_1$Date,format = "%m/%d/%Y")
class(weight_log_info_1$Date)
n_distinct(weight_log_info_1$Id)

weight_log_info_2 <- read_csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv", col_types = cols(LogId = col_character()))
head(weight_log_info_2)
colnames(weight_log_info_2)
class(weight_log_info_2$Date)
weight_log_info_2$Date <- as.Date(weight_log_info_2$Date,format = "%m/%d/%Y")
class(weight_log_info_2$Date)
n_distinct(weight_log_info_2$Id)

##Using rbind() to merge the weight_log_info CSVs
weight_log_info <- rbind(weight_log_info_1, weight_log_info_2)
weight_log_info <- weight_log_info[order(weight_log_info$Id, weight_log_info$Date),]
view(weight_log_info)

##The following 3 CSVs were uploaded and transformed in the same way, they were uploaded, checked for formatting and structure, 
##had their Activity Day column changed from a string to a date
##and then organized in ascending order by Id and ActivityDay
daily_steps <- read.csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
head(daily_steps)
colnames(daily_steps)
n_distinct(daily_steps$Id)
daily_steps$ActivityDay <- as.Date(daily_steps$ActivityDay,format = "%m/%d/%Y")
class(daily_steps$ActivityDay)
daily_steps <- daily_steps[order(daily_steps$Id, daily_steps$ActivityDay), ]
view(daily_steps)

daily_calories <- read.csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
head(daily_calories)
colnames(daily_calories)
n_distinct(daily_calories$Id)
daily_calories$ActivityDay <- as.Date(daily_calories$ActivityDay,format = "%m/%d/%Y")
class(daily_calories$ActivityDay)
daily_calories <- daily_calories[order(daily_calories$Id, daily_calories$ActivityDay),]
view(daily_calories)

daily_intensities <- read.csv("data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
head(daily_intensities)
colnames(daily_intensities)
n_distinct(daily_intensities$Id)
daily_intensities$ActivityDay <- as.Date(daily_intensities$ActivityDay,format = "%m/%d/%Y")
class(daily_intensities$ActivityDay)
daily_intensities <- daily_intensities[order(daily_intensities$Id, daily_intensities$ActivityDay),]
view(daily_intensities)

##This saves my CSV file locally on my laptop, so I will be able to find it in my files and upload it to Sheets
write.csv(daily_activity,
          "daily_activity_clean.csv",
          row.names = FALSE)
##getwd() shows me where exactly on my computer it was saved to
getwd()

##The following function keeps 1 date for 2016-04-12, deleting the duplicates, grouping by the relevant columns
##and ordering them in descending order, means it will keep the only higher number, which is imperative because it is the final number for the day,
##whereas the smaller number was the record at the time that the data collection switched over to collection #2
daily_activity_clean <- daily_activity %>%
  group_by(Id, ActivityDate) %>%
  arrange(desc(TotalSteps), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()
##viewing to confirm my data looks good
view (daily_activity_clean)

##I will now repeat this for weight_log_info because there are two duplicates in that sheet
##I will not have to for any other CSVs because I am only using 2 sets that were combined (the reason there were duplicates)
weight_log_info_clean <- weight_log_info %>%
  group_by(Id, Date) %>%
  arrange(desc(WeightKg), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()
view(weight_log_info_clean)

##Now I am running these structure functions for my 6 CSVs to confirm that the data types are what they should be
str(daily_activity_clean)
str(daily_calories)
str(daily_intensities)
str(daily_steps)
str(sleep_day)
str(weight_log_info_clean)

##I want to convert Id to char from num, because I want R to understand that it is a label, not something to be calculated
daily_activity_clean$Id <- as.character(daily_activity_clean$Id)
daily_calories$Id <- as.character(daily_calories$Id)
daily_intensities$Id <- as.character(daily_intensities$Id)
daily_steps$Id <- as.character(daily_steps$Id)
sleep_day$Id <- as.character(sleep_day$Id)
weight_log_info_clean$Id <- as.character(weight_log_info_clean$Id)

##Now I must check for null values
colSums(is.na(daily_activity_clean))
colSums(is.na(daily_calories))
colSums(is.na(daily_intensities))
colSums(is.na(daily_steps))
colSums(is.na(sleep_day))
colSums(is.na(weight_log_info_clean))

##There were no null entries to change, so now I will check for impossible entries, such as negative steps
##impossible entry code will look slightly different for each CSV as they do not all use the same colnames and values
##daily_activity_clean
##I will also check for how many rows were deleted using the filter function, using nrow before and after
before_daily_activity_clean <- nrow(daily_activity_clean)

daily_activity_clean <- daily_activity_clean %>%
  filter(TotalSteps >= 0,Calories >= 0,TotalDistance >= 0, 
         TrackerDistance >= 0, LoggedActivitiesDistance >= 0,
         VeryActiveDistance >= 0,ModeratelyActiveDistance >= 0, LightActiveDistance >= 0,
         SedentaryActiveDistance >= 0,VeryActiveMinutes >= 0, FairlyActiveMinutes >= 0, 
         LightlyActiveMinutes >= 0, SedentaryMinutes >= 0)

after_daily_activity_clean <- nrow(daily_activity_clean)

before_daily_activity_clean - after_daily_activity_clean
##daily_calories
before_daily_calories <- nrow(daily_calories)

daily_calories <- daily_calories %>%
  filter(Calories >= 0)

after_daily_calories <- nrow(daily_calories)

before_daily_calories - after_daily_calories
##daily_intensities
before_daily_intensities <- nrow(daily_intensities)

daily_intensities <- daily_intensities %>%
  filter(LightlyActiveMinutes >= 0,SedentaryActiveDistance >= 0,VeryActiveDistance >= 0, 
         FairlyActiveMinutes >= 0, LightActiveDistance >= 0, SedentaryMinutes >= 0, 
         VeryActiveMinutes >= 0, ModeratelyActiveDistance >= 0)

after_daily_intensities <- nrow(daily_intensities)

before_daily_intensities - after_daily_intensities
##daily_steps
before_daily_steps <- nrow(daily_steps)

daily_steps <- daily_steps %>%
  filter(StepTotal >= 0)

after_daily_steps <- nrow(daily_steps)

before_daily_steps - after_daily_steps
##sleep_day
before_sleep_day <- nrow(sleep_day)

sleep_day <- sleep_day %>%
  filter(TotalSleepRecords >= 0,
         TotalMinutesAsleep >= 0,
         TotalTimeInBed >= 0)

after_sleep_day <- nrow(sleep_day)

before_sleep_day - after_sleep_day
##weight_log_info_clean
##With this particular CSV I need to amend the filter code slightly to allow for NA values in "Fat"
##Because Fat is the only column with NA values and I do not want to delete rows just because Fat is null valued
##I also need to ensure that weight is higher than 0
before_weight_log_info_clean <- nrow(weight_log_info_clean)

weight_log_info_clean <- weight_log_info_clean %>%
  filter(WeightKg > 0,
         WeightPounds > 0,
         (Fat > 0 | is.na(Fat)), BMI > 0)

after_weight_log_info_clean <- nrow(weight_log_info_clean)

before_weight_log_info_clean - after_weight_log_info_clean

##Now I will look to locate physiological outliers, meaning, I will use the mutate function to create new columns that will store the outliers
##I also will not have to use mutate on every CSV because daily_activity_clean holds a lot of the info that the other CSVs do
##daily_activity_clean
daily_activity_clean <- daily_activity_clean %>%
  mutate(
    steps_outlier = TotalSteps > 50000,
    distance_outlier = TotalDistance > 50,
    calories_outlier = Calories > 8000,
    active_minutes_outlier = VeryActiveMinutes > 400
  )
##Now to check is any suspicious outliers exist
colSums(daily_activity_clean[, c("steps_outlier",
                                 "distance_outlier",
                                 "calories_outlier",
                                 "active_minutes_outlier")])

##sleep_day
sleep_day <- sleep_day %>%
  mutate(
    asleep_outlier = TotalMinutesAsleep > 1440,
    inbed_outlier = TotalTimeInBed > 1440,
    asleep_out_of_bed = TotalMinutesAsleep > TotalTimeInBed,
    too_many_logs = TotalSleepRecords > 5
  )

##Checking for suspicious outlier existence
colSums(sleep_day[, c("asleep_outlier",
                      "inbed_outlier",
                      "asleep_out_of_bed",
                      "too_many_logs")])

##weight_log_info_clean
##This code is more specific because it needs to calculate if there are any discrepancies in kg vs lbs calculations
##If there is more than a 1lb difference, it will alert me by printing problematic rows to the console
##I am using a different method than mutate because these are not physiological outliers, these are correctness checks
weight_log_info_clean[
  abs(weight_log_info_clean$WeightPounds -
        weight_log_info_clean$WeightKg * 2.20462) > 1,
]

weight_log_info_clean[
  !is.na(weight_log_info_clean$Fat) & weight_log_info_clean$Fat > 75,
]

##Now I must delete the columns that I created when I used mutate
##sleep_day
sleep_day <- sleep_day %>%
  select(-asleep_outlier,-inbed_outlier,-asleep_out_of_bed,-too_many_logs)

##daily_activity_clean
daily_activity_clean <- daily_activity_clean %>%
  select(-steps_outlier,-distance_outlier,-calories_outlier,-active_minutes_outlier)


##Creating variables
##By creating new variables it will be easier to do my analysis
##This will create a new column and it will mention the day of the week
daily_activity_clean$Weekday <- weekdays(daily_activity_clean$ActivityDate)

##This variable will differentiate for the weekend
daily_activity_clean$Weekend <- daily_activity_clean$Weekday %in% c("Saturday","Sunday")

##The next variable will serve as a wear indicator
daily_activity_clean$Worn <- daily_activity_clean$TotalSteps > 0

##The next few variables will serve as activity monitors, determining how active participants were
##In R, the structure of ifelse is: ifelse(condition, value_if_true, value_if_false)
daily_activity_clean$ActivityLevel <- ifelse(!daily_activity_clean$Worn, "Unworn",
                                             ifelse(daily_activity_clean$TotalSteps < 5000, "Low",
                                                    ifelse(daily_activity_clean$TotalSteps < 10000, "Active",
                                                           "High")))

##The following variable will serve to calculate sleep efficiency, which is time asleep divided by time in bed
##Expressed in a percentage
##I need to ensure that time in bed is higher than zero to run this function because otherwise it could ruin my analysis
##As dividing by 0 is undefined and I dont want to include values of zero in my sleep analysis
##NA serves as the else argument in the ifelse statement, meaning, instead of giving a value like "inf"
##It will give the value of NA when TotalTimeInBed is equal to zero
sleep_day$SleepEfficiency <- ifelse(
  sleep_day$TotalTimeInBed > 0,
  sleep_day$TotalMinutesAsleep / sleep_day$TotalTimeInBed * 100,
  NA
)

##This will use kg as the standard weight for this study
##Then I will create a variable for BMI, classifying BMI measurements using an ifelse command
weight_log_info_clean$Weight <- weight_log_info_clean$WeightKg

weight_log_info_clean$BMIhealth <- ifelse(weight_log_info_clean$BMI < 18.5, "Underweight",
                                             ifelse(weight_log_info_clean$BMI > 18.5 & weight_log_info_clean$BMI < 24.9, "Healthy",
                                                    ifelse(weight_log_info_clean$BMI > 25 & weight_log_info_clean$BMI < 29.9, "Overweight",
                                                           "Obese")))

##To begin my analysis I want to look at trends in overall steps throughout the duration of the study
##I will begin by creating a numeric timetable to more easily track progress throughout the study
daily_activity_clean$StudyDay <- as.numeric(
  daily_activity_clean$ActivityDate - 
    min(daily_activity_clean$ActivityDate)
)

##Now I will create a new table so I can review the daily step averages, this will be called daily_trend

daily_trend <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(StudyDay) %>%
  summarise(mean_steps = mean(TotalSteps))

view(daily_trend)

##Now I am creating a table that will state the slope for each participant, it will presenting their average change in steps per day
user_slopes <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(Id) %>%
  summarise(slope = coef(lm(TotalSteps ~ StudyDay))[2])
##This creates a new column that will label positive change, negative change and no change at all
user_slopes$Trend <- ifelse(user_slopes$slope > 0, "Improved",
                            ifelse(user_slopes$slope < 0, "Declined", "No change"))
##Then I will view my table, check for mean and median and the number of participants in each category
view(user_slopes)
mean(user_slopes$slope > 0)
median(user_slopes$slope)
sum(user_slopes$slope < 0)
sum(user_slopes$slope > 0)
sum(user_slopes$slope == 0)

##Now I will determine the average increase 
improvers <- user_slopes[user_slopes$slope > 0, ]
mean(improvers$slope)
median(improvers$slope)

##Now I will the determine the average decreases
decliners <- user_slopes[user_slopes$slope < 0, ]
mean(decliners$slope)
median(decliners$slope)

##This is a histogram plot that will clearly illustrate the spread of participants, it will compare the number of participants with the slope
ggplot(user_slopes, aes(x = slope)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0, color = "red") +
  labs(
    title = "Distribution of Participant Step Trends",
    x = "Slope (change in steps per day)",
    y = "Number of participants"
  )

##Now I want to discover if participants are more active on weekends vs weekdays
##I am creating a new table that only includes when the device was worn
week_data <- daily_activity_clean[daily_activity_clean$Worn, ]

week_summary <- week_data %>%
  group_by(Weekend) %>%
  summarise(mean_steps = mean(TotalSteps))
view(week_summary)

week_summary$DayType <- ifelse(week_summary$Weekend, "Weekend", "Weekday")

##The following graph illustrates the average daily steps on Weekdays vs Weekends
##This is slightly biased because some people joined the study late
ggplot(week_summary, aes(x = DayType, y = mean_steps)) +
  geom_col() +
  labs(
    title = "Average Steps: Weekend vs Weekday",
    x = "",
    y = "Average Daily Steps"
  )

##The following code is to create the mean steps for each participant on weekend vs weekdays
##Then it is graphed in a bar chart, with each participant having 2 bars, one for weekend activity and one for weekday activity
per_user <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(Id, Weekend) %>%
  summarise(mean_steps = mean(TotalSteps), .groups = "drop") %>%
  mutate(
    DayType = ifelse(Weekend, "Weekend", "Weekday")
  )

ggplot(per_user, aes(x = Id, y = mean_steps, fill = DayType)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean Steps per Participant: Weekday vs Weekend",
    x = "Participant ID",
    y = "Mean Daily Steps"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

##Same plot but horizontal
ggplot(per_user, aes(x = Id, y = mean_steps, fill = DayType)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Mean Steps per Participant: Weekday vs Weekend",
    x = "Participant ID",
    y = "Mean Daily Steps"
  )

##Calculating each person's difference
within_user <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(Id, Weekend) %>%
  summarise(mean_steps = mean(TotalSteps), .groups = "drop") %>%
  pivot_wider(names_from = Weekend, values_from = mean_steps)

within_user$diff <- within_user$`TRUE` - within_user$`FALSE`

##Giving me one number to answer the average
median(within_user$diff, na.rm = TRUE)

##Percentage who walk more on weekend
mean(within_user$diff > 0, na.rm = TRUE)

##The following is sets of code that determine is weekday walkers walk more than weekend walkers
##This computes each persons weekend vs weekday difference
within_user <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(Id, Weekend) %>%
  summarise(mean_steps = mean(TotalSteps), .groups = "drop") %>%
  pivot_wider(names_from = Weekend, values_from = mean_steps)

within_user$diff <- within_user$`TRUE` - within_user$`FALSE`

##This classifies them as weekday or weekend walkers
within_user$Type <- ifelse(within_user$diff > 0,
                           "Weekend Walker",
                           "Weekday Walker")

##This computes each person's overall average steps
overall_activity <- daily_activity_clean %>%
  filter(Worn) %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps), .groups="drop")

##This combines the info
comparison <- merge(within_user, overall_activity, by="Id")

#Comparing both groups
comparison %>%
  group_by(Type) %>%
  summarise(mean_steps = mean(avg_steps),
            median_steps = median(avg_steps),
            n = n())

##And then plotting my findings
ggplot(comparison, aes(x = Type, y = avg_steps)) +
  geom_col(stat="summary", fun="mean") +
  labs(
    title = "Average Activity of Weekend vs Weekday Walkers",
    x = "",
    y = "Average Daily Steps"
  )

##Now I am calculating overall adherence per participant
usage_per_user <- daily_activity_clean %>%
  group_by(Id) %>%
  summarise(
    days_worn = sum(Worn),
    total_days = n(),
    adherence = days_worn / total_days
  )
view(usage_per_user)

mean(usage_per_user$adherence)

ggplot(usage_per_user, aes(x = adherence)) +
  geom_histogram(bins = 15) +
  labs(
    title = "Participant Device Adherence",
    x = "Proportion of Days Worn",
    y = "Number of Participants"
  )

##This shows the active device users over time for the whole study
daily_usage <- daily_activity_clean %>%
       group_by(StudyDay) %>%
       summarise(active_users = sum(Worn))

 ggplot(daily_usage, aes(x = StudyDay, y = active_users)) +
       geom_line() +
       geom_smooth(method="lm", se=FALSE, color="red") +
       labs(
             title = "Active Device Users Over Time",
             x = "Study Day",
             y = "Number of Active Users"
         )

##This calculates the mean for worn vs unworn stats
mean(daily_activity_clean$Worn)

##Now I am calculating for sleep efficiency, to see if there are correlations between wearing the device and sleep improving
##Making a "day number" for each person
sleep_day <- sleep_day %>%
  group_by(Id) %>%
  mutate(Day = as.numeric(SleepDay - min(SleepDay))) %>%
  ungroup()

##Confirming there are values for sleep efficiency
sleep_clean <- sleep_day[!is.na(sleep_day$SleepEfficiency), ]

##Now I am calculating a trend for each person
##This will determine whether their sleep efficiency increased or decreased
sleep_slopes <- sleep_clean %>%
  group_by(Id) %>%
  summarise(slope = coef(lm(SleepEfficiency ~ Day))[2])
view(sleep_slopes)

##Now to find the median slope, this is the typical rate of change in sleep efficiency
median(sleep_slopes$slope, na.rm = TRUE)

##This calculates the proportion of participants who imrpoved their sleep efficiency
mean(sleep_slopes$slope > 0, na.rm = TRUE)

##And a graph

sleep_slopes$Trend <- ifelse(sleep_slopes$slope > 0, "Improved", "Worsened")

ggplot(sleep_slopes, aes(x = Trend)) +
  geom_bar() +
  labs(
    title = "Did Sleep Efficiency Improve During the Study?",
    x = "",
    y = "Number of Participants"
  )

##The following code is for determining whether weight changes occurred:
# 1) Keep valid rows (Fat can be NA). (BMI > 0 is also a good safety check.)
weight_clean <- weight_log_info_clean %>%
  filter(Weight > 0, BMI > 0, (Fat > 0 | is.na(Fat)))

# 2) For each person: baseline BMI (first), first & last weight, and weight change
weight_change <- weight_clean %>%
  group_by(Id) %>%
  arrange(Date) %>%
  summarise(
    baseline_BMI = first(BMI),
    first_date = first(Date),
    last_date  = last(Date),
    first_weight = first(Weight),
    last_weight  = last(Weight),
    delta_kg = last_weight - first_weight,
    .groups = "drop"
  ) %>%
  filter(first_date != last_date) %>%   # keep only people with 2+ measurements
  mutate(
    BMI_Group = ifelse(baseline_BMI < 18.5, "Underweight",
                       ifelse(baseline_BMI < 25, "Healthy",
                              ifelse(baseline_BMI < 30, "Overweight", "Obese")))
  )

# 3) Summary numbers (how much each group changed)
summary_by_group <- weight_change %>%
  group_by(BMI_Group) %>%
  summarise(
    n = n(),
    mean_change_kg = mean(delta_kg),
    median_change_kg = median(delta_kg),
    .groups = "drop"
  )

print(summary_by_group)

# 4) Better graph: points + group mean bar, red line = no change
ggplot(weight_change, aes(x = BMI_Group, y = delta_kg)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_col(stat = "summary", fun = "mean") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6) +
  labs(
    title = "Weight Change by Baseline BMI Group",
    subtitle = "Each dot = one participant (last − first). Bar = group mean. Red line = no change.",
    x = "Baseline BMI Group",
    y = "Weight change (kg)"
  )