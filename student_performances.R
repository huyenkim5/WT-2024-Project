#load packages
library(tidyverse)
library(ggplot2)

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
    gender = factor(gender),
    academic_performance = factor(academic_performance, levels = performance_levels, ordered = TRUE),
    taking_note_in_class = factor(taking_note_in_class, levels = yes_no_levels, ordered = TRUE),
    depression_status = factor(depression_status, levels = yes_no_levels, ordered = TRUE),
    face_challenges_to_complete_academic_task = factor(face_challenges_to_complete_academic_task, levels = yes_no_levels, ordered = TRUE),
    like_new_things = factor(like_new_things, levels = yes_no_levels, ordered = TRUE),
    like_presentation = factor(like_presentation, levels = yes_no_levels, ordered = TRUE)
  )
glimpse(student_performances)

#check factor orders
sort(student_performances$academic_performance)
sort(student_performances$depression_status)

