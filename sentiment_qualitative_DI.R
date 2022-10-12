#Clear Environment####

rm(list = ls())

 ##Install Packages####
# 
# if (!require(tidyverse)) install.packages('tidyverse')        #for data wrangling
# if (!require(data.table)) install.packages('data.table')      #for munging date data
# if (!require(readxl)) install.packages('readxl')              #for importing excel files
# if (!require(writexl)) install.packages('writexl')            #for exporting excel files
# if (!require(janitor)) install.packages('janitor')            #for cleaning variable names
# if (!require(dlookr)) install.packages('dlookr')              #for EDA
# if (!require(DataExplorer)) install.packages('DataExplorer')  #for EDA
# if (!require(prettydoc)) install.packages('prettydoc')        #for EDA reports
# if (!require(forecast)) install.packages('forecast')          #for EDA report
# if (!require(Hmisc)) install.packages('Hmisc')                #for EDA
# if (!require(rpart.plot)) install.packages('rpart.plot')      #for plots
# if (!require(DiagrammeR)) install.packages('DiagrammeR')      #for plots
# if (!require(reshape2)) install.packages('reshape2')          #for manipulating strings
# if (!require(arrow)) install.packages('arrow')                #for reading parquet files

##Load libraries####
suppressPackageStartupMessages(library(tidyverse))   #for data wrangling
suppressPackageStartupMessages(library(data.table))  #for munging date data
suppressPackageStartupMessages(library(readxl))      #for importing excel files
suppressPackageStartupMessages(library(writexl))     #for exporting excel files
suppressPackageStartupMessages(library(janitor))     #for cleaning variable names
suppressPackageStartupMessages(library(dlookr))      #for EDA
suppressPackageStartupMessages(library(DataExplorer))#for EDA
suppressPackageStartupMessages(library(prettydoc))   #for EDA reports
suppressPackageStartupMessages(library(forecast))    #for EDA report
suppressPackageStartupMessages(library(Hmisc))       #for EDA
suppressPackageStartupMessages(library(rpart.plot))  #for plotting
suppressPackageStartupMessages(library(DiagrammeR))  #for plotting
suppressPackageStartupMessages(library(reshape2))    #for manipulating strings
suppressPackageStartupMessages(library(arrow))       #for reading parquet files

#Set working directory for files####
wd <- getwd()

##Read in files ####
#File name:
file_response <- file.path(wd,'response_all.parquet')
file_survey <- file.path(wd,'survey_all.csv')
file_sentiment <- file.path(wd,"sentiment_score.csv")
file_item <- file.path(wd,"SFS Item Bank - iLab.csv")

#File:
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

response_QQ$sur_faculty_name <- fifelse(response_QQ$sur_faculty_name == "Arts and Social Science",
                                        "Arts and Social Sciences",response_QQ$sur_faculty_name)

#remove these observations from dataframe

response_QQ <- response_QQ %>% 
  filter(!is.na(sur_faculty_name))

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

# Response by faculty####
##"Design, Architecture and Building"####
DAB <- response_QQi %>% 
  filter(sur_faculty_name == "Design, Architecture and Building") %>% 
  filter(!is.na(resp_q_id))

##"Arts and Social Sciences"####
ASS <- response_QQi %>% 
  filter(sur_faculty_name =="Arts and Social Sciences") %>% 
  filter(!is.na(sentiment_score))

##"Law"####
LAW <- response_QQi %>% 
  filter(sur_faculty_name =="Law") %>% 
  filter(!is.na(sentiment_score))

##"Engineering and Information Technology"####
EIT<- response_QQi %>% 
  filter(sur_faculty_name == "Engineering and Information Technology")%>% 
  filter(!is.na(sentiment_score))

##"Business"####        
BUS <- response_QQi %>% 
  filter(sur_faculty_name =="Business" ) %>% 
  filter(!is.na(sentiment_score))

##"Science"####
SCI <- response_QQi %>% 
  filter(sur_faculty_name == "Science") %>% 
  filter(!is.na(sentiment_score))

##"Graduate School of Health"####
GSH  <- response_QQi %>% 
  filter(sur_faculty_name == "Graduate School of Health") %>% 
  filter(!is.na(sentiment_score))

##"Health"####       
HEL <- response_QQi %>% 
  filter(sur_faculty_name =="Health") %>% 
  filter(!is.na(sentiment_score))

##"Transdisciplinary Innovation"####
TDI <- response_QQi %>% 
  filter(sur_faculty_name == "Transdisciplinary Innovation") %>% 
  filter(!is.na(sentiment_score))


#### Write files ####
 write_csv(DAB,paste(wd,"/DAB.csv",sep = ""))
# write_csv(ASS,paste(wd,"/ASS.csv",sep = ""))
# write_csv(LAW,paste(wd,"/LAW.csv",sep = ""))
# write_csv(EIT,paste(wd,"/EIT.csv",sep = ""))
# write_csv(BUS,paste(wd,"/BUS.csv",sep = ""))
# write_csv(SCI,paste(wd,"/SCI.csv",sep = ""))
# write_csv(GSH,paste(wd,"/GSH.csv",sep = ""))
# write_csv(HEL,paste(wd,"/HEL.csv",sep = ""))
# write_csv(TDI,paste(wd,"/TDI.csv",sep = ""))

write_csv(response_QQi,paste(wd,"/q&q.csv", sep = ""))

glimpse(response_QQi)
#EDA ####
# #eda_report(response, output_dir = ".", output_format="html")
# 
# resp_eda <- select(response_QQ,
#                    -resp_comment,
#                    -resp_image,
#                    -resp_teacher_incorrect,
#                    -resp_import_batch,
#                    -resp_comment_hidden)
# 
# resp_eda$resp_points <- as.factor(resp_eda$resp_points)
# resp_eda$resp_majors <- as.factor(resp_eda$resp_majors)
# resp_eda$resp_course <- as.factor(resp_eda$resp_course)
# resp_eda$resp_fieldofstudy <- as.factor(resp_eda$resp_fieldofstudy)
# resp_eda$resp_form_key <- as.factor(resp_eda$resp_form_key)
# resp_eda$resp_dmg_gender <- as.factor(resp_eda$resp_dmg_gender)
# resp_eda$resp_dmg_indigenous <- as.factor(resp_eda$resp_dmg_indigenous)
# resp_eda$resp_dmg_cob <- as.factor(resp_eda$resp_dmg_cob)
# resp_eda$resp_dmg_citizen <- as.factor(resp_eda$resp_dmg_citizen)
# resp_eda$resp_dmg_language <- as.factor(resp_eda$resp_dmg_language)
# resp_eda$resp_dmg_home <- as.factor(resp_eda$resp_dmg_home)
# resp_eda$resp_dmg_term <- as.factor(resp_eda$resp_dmg_term)
# resp_eda$resp_dmg_ses <- as.factor(resp_eda$resp_dmg_ses)
# resp_eda$resp_dmg_liability_cat <- as.factor(resp_eda$resp_dmg_liability_cat)
# resp_eda$resp_dmg_int_attend_type <- as.factor(resp_eda$resp_dmg_int_attend_type)
# resp_eda$resp_dmg_commencing <- as.factor(resp_eda$resp_dmg_commencing)
# resp_eda$resp_dmg_course_campus <- as.factor(resp_eda$resp_dmg_course_campus)
# resp_eda$resp_dmg_sp_adm_scheme <- as.factor(resp_eda$resp_dmg_sp_adm_scheme)
# resp_eda$resp_dmg_scholarship <- as.factor(resp_eda$resp_dmg_scholarship)
# resp_eda$resp_dmg_mode_of_attend <- as.factor(resp_eda$resp_dmg_mode_of_attend)
# resp_eda$resp_dmg_school_leaver <- as.factor(resp_eda$resp_dmg_school_leaver)
# resp_eda$resp_dmg_previous_insti <- as.factor(resp_eda$resp_dmg_previous_insti)
# resp_eda$resp_dmg_student_comm_yr <- as.factor(resp_eda$resp_dmg_student_comm_yr)
# resp_eda$resp_dmg_grad_lvl <- as.factor(resp_eda$resp_dmg_grad_lvl)
# resp_eda$sentiment_score <- as.factor(resp_eda$sentiment_score)
# 
# #eda_web_report(resp_eda, output_dir = ".",
# #               author = "Deena Iamsiri", output_file = "Response_sentiment.html",
# #               theme = "blue", browse = TRUE)
# 
# 
# rm(resp_eda)
# 
# #eda_web_report(resp_comm, output_dir = ".",
# #               author = "Deena Iamsiri", output_file = "Response_sentiment_noNA.html",
# #               theme = "blue", browse = TRUE)
# 
# 
# #merge survey to resp_comm
# df <- merge(x=resp_comm, y=survey, by.x = "resp_survey_id", by.y = "sur_id", all.x = TRUE)
# # #merge df to item for questions

# 
# df <- merge(x=df, y=item, by.x = "resp_q_id", by.y = "id", all.x = TRUE)
# 
# glimpse(df)
# 
# df$resp_date<- as.Date(df$resp_date, origin = "1899-12-30")
# df$sur_semester<- as.factor(df$sur_semester)
# df$sur_class_1<- as.factor(df$sur_class_1)
# df$sur_class_2<- as.factor(df$sur_class_2)
# df$sur_faculty_name<- as.factor(df$sur_faculty_name)
# df$sur_acadunit_name<- as.factor(df$sur_acadunit_name)
# df$sur_day<- as.factor(df$sur_day)
# df$sur_location_name<- as.factor(df$sur_location_name)
# df$sur_grouping_code<- as.factor(df$sur_grouping_code)
# df$sur_questionaire<- as.factor(df$sur_questionaire)
# df$sur_teacher_firstname<- as.character(df$sur_teacher_firstname)
# df$sur_teacher_lastname<- as.character(df$sur_teacher_lastname)
# df$sur_teacher_email<- as.character(df$sur_teacher_email)
# df$sur_tobe_deleted<- as.character(df$sur_tobe_deleted)
# df$sur_delete_reason<- as.character(df$sur_delete_reason)
# df$sur_form<- as.factor(df$sur_form)
# df$sur_teacher_status<- as.factor(df$sur_teacher_status)
# df$sur_resp_count<- as.character(df$sur_resp_count)
# df$sur_p_course_on<- as.character(df$sur_p_course_on)
# df$sur_notes<- as.character(df$sur_notes)
# df$sur_p_printed<- as.character(df$sur_p_printed)
# df$sur_questionaire_modes<- as.character(df$sur_questionaire_modes)
# df$sur_coordinator_order_access<- as.character(df$sur_coordinator_order_access)
# df$sur_status<- as.factor(df$sur_status)
# df$sur_student_report_status<- as.factor(df$sur_student_report_status)
# df$x64<- as.factor(df$x64)
# df$x65<- as.factor(df$x65)
# df$type<- as.factor(df$type)
# df$sentiment_score <- as.numeric(df$sentiment_score)
# #df$sentiment_score <- as.factor(df$sentiment_score)
# x <- select(df,
#             -sur_teacher_firstname,
#             -sur_teacher_lastname,
#             -sur_teacher_email,
#             -sur_tobe_deleted,
#             -sur_delete_reason,
#             -sur_resp_count,
#             -sur_p_course_on,
#             -sur_notes,
#             -sur_p_printed,
#             -sur_questionaire_modes,
#             -sur_coordinator_order_access,
#             -sur_report_limit,
#             -resp_form_key,
#             -sur_lastchange_date,
#             -sur_lastchange_id,
#             -sur_p_import_date,
#             -sur_coordinator_lastname,
#             -sur_coordinator_firstname,
#             #-resp_points,#here
#             #-resp_majors,
#             #-resp_course,
#             #-resp_fieldofstudy,
#             #-resp_dmg_gender,
#             #-resp_dmg_indigenous,
#             #-resp_dmg_cob,
#             #-resp_dmg_citizen,
#             #-resp_dmg_language,
#             #-resp_dmg_home,
#             #-resp_dmg_term,
#             #-resp_dmg_ses,
#             #-resp_dmg_liability_cat,
#             #-resp_dmg_int_attend_type,
#             #-resp_dmg_commencing,
#             #-resp_dmg_course_campus,
#             #-resp_dmg_sp_adm_scheme,
#             #-resp_dmg_scholarship,
#             #-resp_dmg_mode_of_attend,
#             #-resp_dmg_school_leaver,
#             #-resp_dmg_previous_insti,
#             #-resp_dmg_student_comm_yr,
#             #-resp_dmg_grad_lvl,
#             #-sentiment_score,
#             #-sur_semester,
#             #-sur_class_1,
#             #-sur_class_2,
#             #-sur_faculty_name,
#             #-sur_acadunit_name,
#             #-sur_day,
#             #-sur_location_name,
#             #-sur_grouping_code,
#             #-sur_questionaire,
#             #-sur_form,
#             #-sur_teacher_status,
#             #-sur_status,
#             #-sur_student_report_status,
#             #-x64,
#             #-x65,
#             #-type,
#             -text)
# glimpse(x)
# #eda_report(x, output_dir = ".", output_format="html")
# 
# #eda_web_report(x, output_dir = ".",
# #               author = "Deena Iamsiri", output_file = "Q&A.html",
# #               theme = "blue", browse = TRUE)
# x <- x %>% 
#   filter(!is.na(sentiment_score))
# x$sur_faculty_name <- fifelse(x$sur_faculty_name = "")
# 
# write_csv(x,paste(wd,"/Q&A.csv",sep = ""))
