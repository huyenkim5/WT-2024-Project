#load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#read file
student_performances <- read_csv("data/CSE_student_performances.csv")
student_performances
glimpse(student_performances)
View(student_performances)

#change variable names to snake_case
student_performances <- student_performances |> 
  rename(
    age = Age,
    gender = Gender,
    academic_performance = AcademicPerformance,
    taking_note_in_class = TakingNoteInClass,
    depression_status = DepressionStatus,
    face_challenges_to_complete_academic_task = FaceChallangesToCompleteAcademicTask,
    like_presentation = LikePresentation,
    sleep_per_day_hours = SleepPerDayHours,
    number_of_friend = NumberOfFriend,
    like_new_things = LikeNewThings
  )
View(student_performances)

#check missing values
student_performances |> 
  filter(if_any(everything(), is.na)) |> 
  relocate(number_of_friend)

#specify factor order
yes_no_levels <- c("No", "Sometimes", "Yes")
performance_levels <- c("Below average", "Average", "Good", "Excellent")

#change column types
student_performances <- student_performances |> 
  mutate(
    gender = factor(gender, ordered = FALSE),
    academic_performance = factor(academic_performance, levels = performance_levels, ordered = TRUE),
    taking_note_in_class = factor(taking_note_in_class, levels = yes_no_levels, ordered = TRUE),
    depression_status = factor(depression_status, levels = yes_no_levels, ordered = TRUE),
    face_challenges_to_complete_academic_task = factor(face_challenges_to_complete_academic_task, levels = yes_no_levels, ordered = TRUE),
    like_new_things = factor(like_new_things, levels = yes_no_levels, ordered = FALSE),
    like_presentation = factor(like_presentation, levels = yes_no_levels, ordered = FALSE)
  )
glimpse(student_performances)

#check factor orders
sort(student_performances$academic_performance)
sort(student_performances$depression_status)

##exploratory data analysis
#distribution of gender: 43 females, 56 males
student_performances |> 
  group_by(gender) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = gender)) +
  geom_bar() 

#distribution of academic performance: mostly average and good
student_performances |> 
  group_by(academic_performance) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = academic_performance)) +
  geom_bar()


#distribution of note taking: yes > sometimes > no
student_performances |> 
  group_by(taking_note_in_class) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = taking_note_in_class)) +
  geom_bar()

#distribution of depression status: sometimes > yes > no
student_performances |> 
  group_by(depression_status) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = depression_status)) +
  geom_bar()


#distribution of academic challenges: yes > no = sometimes (not much difference)
student_performances |> 
  group_by(face_challenges_to_complete_academic_task) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = face_challenges_to_complete_academic_task)) +
  geom_bar()

#distribution of like presentation: yes > no (double)
student_performances |> 
  group_by(like_presentation) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = like_presentation)) +
  geom_bar()

#distribution of like new things: mostly yes
student_performances |> 
  group_by(like_new_things) |> 
  summarize(n=n())

ggplot(student_performances, aes(x = like_new_things)) +
  geom_bar()

#distribution of sleep hours: positive skew
summary(student_performances$sleep_per_day_hours)

ggplot(student_performances, aes(x = sleep_per_day_hours)) +
  geom_histogram()

#distribution of number of friends: positive skew: most ppl have < 25 friends
summary(student_performances$number_of_friend)

ggplot(student_performances, aes(x = number_of_friend)) +
  geom_histogram(binwidth = 25, na.rm = TRUE)


#recode academic performance into a numeric variable
student_performances <- student_performances |> 
  mutate(
  numeric_academic_performance = recode(academic_performance,
                                      "Below average" = "1",
                                      "Average" = "2",
                                      "Good" = "3",
                                      "Excellent" = "4"),
  numeric_academic_performance = as.numeric(numeric_academic_performance)
)
student_performances
#regression: sleep and academic performance: no significant relationship
ggplot(student_performances, aes(x = sleep_per_day_hours, y = numeric_academic_performance, color = gender))+
  geom_point(position = "jitter")

sleep_performance_lm <- lm(numeric_academic_performance ~ sleep_per_day_hours, data = student_performances)
summary(sleep_performance_lm) #not significant

#anova: depression - academic performance: no relationship
depression_academic_aov <- aov(numeric_academic_performance ~ depression_status, data = student_performances)
summary(depression_academic_aov) #not significant

#remove outlier for number of friend (values > 25)
student_performances_friend_outlier <- student_performances |> 
  filter(number_of_friend <= 25)
student_performances_friend_outlier

#anova: depression status & number of friends
#students who are depressed have significantly less friends than students who are sometimes or not depressed.
ggplot(student_performances_friend_outlier, aes(x = depression_status, y = number_of_friend))+
  geom_boxplot()

depression_friend_aov <- aov(number_of_friend ~ depression_status, data = student_performances_friend_outlier)
summary(depression_friend_aov) #significant

#post-hoc: paired comparisons (t-test)
sometimes_depressed <- student_performances_friend_outlier |> 
  filter(depression_status == "Sometimes") |> 
  select(number_of_friend)
yes_depressed <- student_performances_friend_outlier |> 
  filter(depression_status == "Yes") |> 
  select(number_of_friend)
no_depressed <- student_performances_friend_outlier |> 
  filter(depression_status == "No") |> 
  select(number_of_friend)

t.test(sometimes_depressed, yes_depressed, var.equal = TRUE) #significant
t.test(sometimes_depressed, no_depressed, var.equal = TRUE) #not significant
t.test(no_depressed, yes_depressed, var.equal = TRUE) #significant

