#libraries
library(dplyr)
#https://rdivanji.shinyapps.io/VoicesReport/
#set working directory
setwd("Data")
#grab data
student_data <- read.csv("TestData_VoicesFromTheField2016.csv")

#sort data

#attendance 2015
attendance_data_2015 <- student_data %>% 
  select(VOICES.June2015, ENROLLED2014.15, PRESENT2014.15, ENROLLED2015.16, PRESENT2015.16) %>% 
  filter(VOICES.June2015 != "NULL", ENROLLED2014.15 !="NULL", PRESENT2014.15 != "NULL", ENROLLED2015.16 != "NULL", PRESENT2015.16 != "NULL") %>%
  mutate(diff_14.15 = as.numeric(as.character(ENROLLED2014.15))-as.numeric(as.character(PRESENT2014.15)), 
         diff_15.16 =as.numeric(as.character(ENROLLED2015.16))-as.numeric(as.character(PRESENT2015.16))) %>% na.omit()
#calculate average absence for each year
attendance_2015_num <- as.numeric(nrow(attendance_data_2015))
avg.absent_14.15 <- round(mean(attendance_data_2015$diff_14.15), 4)
avg.absent_15.16 <- round(mean(attendance_data_2015$diff_15.16), 4)
#t-test on absence columns 14.15 and 15.16 
attendance_2015_results <- t.test(attendance_data_2015$diff_14.15, attendance_data_2015$diff_15.16)
attendance.2015_t.obtained <- attendance_2015_results$statistic
attendance.2015_df <- attendance_2015_results$parameter
attendance.2015_p.value <- attendance_2015_results$p.value
attendance.2015_conf.interval <- attendance_2015_results$conf.int
attendance.2015_estimate.means <- attendance_2015_results$estimate
#Voices 2015 t-test results as dataframe 
results2015.names <- c("Sample Size", "Average Absence 2014-15","Average Absence 2015-16", "Test Statistic", "Degrees of Freedom", "P-value", "Confidence Interval Low", "Confidence Interval High")
results2015.values <- c(attendance_2015_num, avg.absent_14.15, avg.absent_15.16, attendance.2015_t.obtained, attendance.2015_df, attendance.2015_p.value, attendance.2015_conf.interval)
results2015.df <- data.frame(results2015.names, results2015.values)

#attendance 2016
attendance_data_2016 <- student_data %>% 
  select(VOICESJune2016, ENROLLED2015.16, PRESENT2015.16, ENROLLED2016.2017, PRESENT2016.17) %>% 
  filter(VOICESJune2016 != "NULL", ENROLLED2015.16 !="NULL", PRESENT2015.16 != "NULL", ENROLLED2016.2017 != "NULL", PRESENT2016.17 != "NULL") %>%
  mutate(diff_15.16 = as.numeric(as.character(ENROLLED2015.16))-as.numeric(as.character(PRESENT2015.16)), 
         diff_16.17 = as.numeric(as.character(ENROLLED2016.2017))-as.numeric(as.character(PRESENT2016.17))) %>% na.omit()
#calculate average absence for each year
attendance_2016_num <- as.numeric(nrow(attendance_data_2016))
avg2016.absent_15.16 <- round(mean(attendance_data_2016$diff_15.16), 4)
avg2016.absent_16.17 <- round(mean(attendance_data_2016$diff_16.17), 4)
#t-test on absence columns 15.16 and 16.17
attendance_2016_results <- t.test(attendance_data_2016$diff_15.16, attendance_data_2016$diff_16.17)
attendance.2016_t.obtained <- attendance_2016_results$statistic
attendance.2016_df <- attendance_2016_results$parameter
attendance.2016_p.value <- attendance_2016_results$p.value
attendance.2016_conf.interval <- attendance_2016_results$conf.int
attendance.2016_estimate.means <- attendance_2016_results$estimate


#math 2015
math_data_2015 <- student_data %>%
  select(VOICES.June2015, MATH2014, MATH2015) %>%
  filter(VOICES.June2015 != "NULL", MATH2014 != "NULL", MATH2015 != "NULL")

#math 2016
math_data_2016 <- student_data %>%
  select(VOICESJune2016, MATH2015, MATH2016) %>%
  filter(VOICESJune2016 != "NULL", MATH2015 != "NULL", MATH2016 != "NULL")

#reading for JUNE2015 participants comparing 2014 v 2016 no 2015 data given
reading_data_2015 <- student_data %>%
  select(VOICES.June2015, READING2014, READING2016) %>%
  filter(VOICES.June2015 != "NULL", READING2014 != "NULL", READING2016 != "NULL")

#science 2015
science_data_2015 <- student_data %>%
  select(VOICES.June2015, SCIENCE2014, SCIENCE2015) %>%
  filter(VOICES.June2015 != "NULL", SCIENCE2014 != "NULL", SCIENCE2015 != "NULL")

#science 2016
science_data_2016 <- student_data %>%
  select(VOICESJune2016, SCIENCE2015, SCIENCE2016) %>%
  filter(VOICESJune2016 != "NULL", SCIENCE2015 != "NULL", SCIENCE2016 != "NULL")
                                                                                                                                               
                                                                                                                                               
                                                                                                                                              