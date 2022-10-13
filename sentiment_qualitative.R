#Clear Environment####

rm(list = ls())

 ##Install Packages####
# 
# if (!require(tidyverse)) install.packages('tidyverse')        #for data wrangling
# if (!require(data.table)) install.packages('data.table')      #for munging date data
# if (!require(readxl)) install.packages('readxl')              #for importing excel files
# if (!require(writexl)) install.packages('writexl')            #for exporting excel files
# if (!require(janitor)) install.packages('janitor')            #for cleaning variable names
# if (!require(reshape2)) install.packages('reshape2')          #for manipulating strings
# if (!require(arrow)) install.packages('arrow')                #for reading parquet files

##Load libraries####
suppressPackageStartupMessages(library(tidyverse))   #for data wrangling
suppressPackageStartupMessages(library(data.table))  #for munging date data
suppressPackageStartupMessages(library(readxl))      #for importing excel files
suppressPackageStartupMessages(library(writexl))     #for exporting excel files
suppressPackageStartupMessages(library(janitor))     #for cleaning variable names
suppressPackageStartupMessages(library(reshape2))    #for manipulating strings
suppressPackageStartupMessages(library(arrow))       #for reading parquet files

#Set working directory for files####
wd <- getwd()

#Read in files ####
#File name:
file_response <- file.path(wd,'response_all.parquet')
file_survey <- file.path(wd,'survey_all.csv')
file_sentiment <- file.path(wd,"sentiment_score.csv")
file_item <- file.path(wd,"SFS Item Bank - iLab.csv")

#Files:
response <- clean_names(read_parquet(file_response, as_tibble = TRUE))
survey <- clean_names(read_csv(file_survey))
sentiment <- clean_names(read_csv(file_sentiment))
item <- clean_names(read_csv(file_item))

sentiment <- select(sentiment,-1)

sentiment$sentiment_score <- fifelse(sentiment$sentiment_score == "[0]",
                                     "Negative",sentiment$sentiment_score)
sentiment$sentiment_score <- fifelse(sentiment$sentiment_score == "[1]",
                                     "Neutral",sentiment$sentiment_score)
sentiment$sentiment_score <- fifelse(sentiment$sentiment_score == "[2]",
                                     "Positive",sentiment$sentiment_score)
sentiment$sentiment_score <- as.factor(sentiment$sentiment_score)


##Merge files ####
response_QQ <- merge(x=response, y=sentiment, by='resp_id', all.x=TRUE)
survey <- select(survey,-1)

response_QQ <- merge(x=response_QQ, y=survey, by.x="resp_survey_id", by.y="sur_id", all=TRUE)
response_QQ <- merge(x=response_QQ, y=item, by.x = "resp_q_id", by.y = "id", all = TRUE)

glimpse(response_QQ)

unique(response_QQ$sur_faculty_name)

#fix faculty name (slight variation)
response_QQ$sur_faculty_name <- fifelse(response_QQ$sur_faculty_name == "Arts and Social Science",
                                        "Arts and Social Sciences",response_QQ$sur_faculty_name)

##Remove observations where faculty is missing from dataframe ####
response_QQ <- response_QQ %>% 
  filter(!is.na(sur_faculty_name))

#remove some variables from the final dataframe
response_QQ <- select(response_QQ,
                      -resp_image,
                      -resp_teacher_incorrect,
                      -sur_tobe_deleted,
                      -sur_delete_reason,
                      -sur_resp_count,
                      -sur_p_course_on,
                      -sur_p_deadline,
                      -sur_notes,
                      -sur_p_printed,
                      -sur_report_limit,
                      -sur_questionaire_modes,
                      -sur_coordinator_order_access)
#reorder variables
response_QQi <- select(response_QQ,
                      sur_year,
                      sur_halfyear,
                      resp_q_id,
                      resp_survey_id,
                      resp_id,
                      resp_points,
                      text,
                      resp_comment,
                      sentiment_score,
                      resp_teacher_id,
                      resp_student_id_hash,
                      sur_subject_id,
                      sur_subject_name,
                      sur_campus_id,
                      sur_studymode_id,
                      sur_faculty_id,
                      sur_faculty_name,
                      sur_acadunit_id,
                      sur_location_id,
                      sur_coordinator_id,
                      sur_order_id,
                      sur_teacher_id)

##Create a sentiment score for points ####
response_QQi$sentiment_score <- as.character(response_QQi$sentiment_score)

response_QQi$sentiment_score <- fifelse(is.na(response_QQi$sentiment_score) &
                                          response_QQi$resp_points <3,
                                        "Negative",response_QQi$sentiment_score)

response_QQi$sentiment_score <- fifelse(is.na(response_QQi$sentiment_score) &
                                          response_QQi$resp_points ==3,
                                        "Neutral",response_QQi$sentiment_score)

response_QQi$sentiment_score <- fifelse(is.na(response_QQi$sentiment_score) &
                                          response_QQi$resp_points >3,
                                        "Positive",response_QQi$sentiment_score)

#Write file for use in PowerBI ####
write_csv(response_QQi,paste(wd,"/q&q.csv", sep = ""))

