#Clear Environment####

rm(list = ls())

##Install Packages####

if (!require(tidyverse)) install.packages('tidyverse')        #for data wrangling
if (!require(data.table)) install.packages('data.table')      #for munging date data
if (!require(readxl)) install.packages('readxl')              #for importing excel files
if (!require(writexl)) install.packages('writexl')            #for exporting excel files
if (!require(janitor)) install.packages('janitor')            #for cleaning variable names
if (!require(dlookr)) install.packages('dlookr')              #for EDA
if (!require(DataExplorer)) install.packages('DataExplorer')  #for EDA
if (!require(prettydoc)) install.packages('prettydoc')        #for EDA reports
if (!require(forecast)) install.packages('forecast')          #for EDA report
if (!require(Hmisc)) install.packages('Hmisc')                #for EDA
if (!require(factoextra)) install.packages('factoextra')      #for PCA visualisation
if (!require(FactoMineR)) install.packages('FactoMineR')      #for PCA
if (!require(caret)) install.packages('caret')                #for Machine Learning (ML)
if (!require(xgboost)) install.packages('xgboost')            #for ML
if (!require(Metrics)) install.packages('Metrics')            #for ML
if (!require(rpart.plot)) install.packages('rpart.plot')      #for plots
if (!require(DiagrammeR)) install.packages('DiagrammeR')      #for plots
if (!require(reshape2)) install.packages('reshape2')          #for manipulating strings
if (!require(arrow)) install.packages('arrow')                #for reading parquet files

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
suppressPackageStartupMessages(library(factoextra))  #for PCA visualisation
suppressPackageStartupMessages(library(FactoMineR))  #for PCA
suppressPackageStartupMessages(library(caret))       #for Machine Learning
suppressPackageStartupMessages(library(xgboost))     #for ML
suppressPackageStartupMessages(library(Metrics))     #for ML
suppressPackageStartupMessages(library(rpart.plot))  #for plotting
suppressPackageStartupMessages(library(DiagrammeR))  #for plotting
suppressPackageStartupMessages(library(reshape2))    #for manipulating strings
suppressPackageStartupMessages(library(arrow))       #for reading parquet files

#Set working directory for files####
wd <- getwd()

#File name:
file_response <- file.path(wd,'response_all.parquet')
file_survey <- file.path(wd,'survey_all.csv')
file_sentiment <- file.path(wd,"sentiment_score.csv")

#File:
response <- clean_names(read_parquet(file_response, as_tibble = TRUE))
survey <- clean_names(read_csv(file_survey))
sentiment <- clean_names(read_csv(file_sentiment))

glimpse(response)


#EDA####

response%>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#old version
eda_report(response, output_dir = ".", output_format="html")

#change logical to character
response$resp_dmg_scholarship <- as.character(response$resp_dmg_scholarship)

#stepwise adding/removing variables to run EDA report

resp_eda <- select(response,
                   resp_id,
                   resp_survey_id,
                   resp_q_id,
                   resp_points,
                   -resp_comment,
                   resp_majors,
                   -resp_image,
                   resp_teacher_id,
                   -resp_teacher_incorrect,
                   resp_date,
                   resp_course,
                   resp_fieldofstudy,
                   -resp_import_batch,
                   resp_form_key,
                   resp_dmg_gender,
                   resp_dmg_age,
                   resp_dmg_indigenous,
                   resp_dmg_cob,
                   resp_dmg_citizen,
                   resp_dmg_language,
                   resp_dmg_home,
                   resp_dmg_term,
                   resp_dmg_ses,
                   resp_dmg_basis_of_adm,
                   resp_dmg_liability_cat,
                   resp_dmg_int_attend_type,
                   resp_dmg_commencing,
                   resp_dmg_course_campus,
                   resp_dmg_sp_adm_scheme,
                   resp_dmg_scholarship,
                   resp_dmg_mode_of_attend,
                   resp_dmg_school_leaver,
                   resp_dmg_uac_preference,
                   resp_dmg_atar_on_adm,
                   resp_dmg_previous_credit,
                   resp_dmg_previous_insti,
                   resp_dmg_student_comm_yr,
                   resp_dmg_grad_lvl,
                   resp_student_id_hash,
                   -resp_comment_hidden)

eda_web_report(resp_eda, output_dir = ".",
               author = "Deena Iamsiri", output_file = "Response_no_comment.html",
               theme = "blue", browse = TRUE)
glimpse(resp_eda)

resp_eda$resp_points <- as.factor(resp_eda$resp_points)
resp_eda$resp_majors <- as.factor(resp_eda$resp_majors)
resp_eda$resp_course <- as.factor(resp_eda$resp_course)
resp_eda$resp_fieldofstudy <- as.factor(resp_eda$resp_fieldofstudy)
resp_eda$resp_form_key <- as.factor(resp_eda$resp_form_key)
resp_eda$resp_dmg_gender <- as.factor(resp_eda$resp_dmg_gender)
resp_eda$resp_dmg_indigenous <- as.factor(resp_eda$resp_dmg_indigenous)
resp_eda$resp_dmg_cob <- as.factor(resp_eda$resp_dmg_cob)
resp_eda$resp_dmg_citizen <- as.factor(resp_eda$resp_dmg_citizen)
resp_eda$resp_dmg_language <- as.factor(resp_eda$resp_dmg_language)
resp_eda$resp_dmg_home <- as.factor(resp_eda$resp_dmg_home)
resp_eda$resp_dmg_term <- as.factor(resp_eda$resp_dmg_term)
resp_eda$resp_dmg_ses <- as.factor(resp_eda$resp_dmg_ses)
resp_eda$resp_dmg_liability_cat <- as.factor(resp_eda$resp_dmg_liability_cat)
resp_eda$resp_dmg_int_attend_type <- as.factor(resp_eda$resp_dmg_int_attend_type)
resp_eda$resp_dmg_commencing <- as.factor(resp_eda$resp_dmg_commencing)
resp_eda$resp_dmg_course_campus <- as.factor(resp_eda$resp_dmg_course_campus)
resp_eda$resp_dmg_sp_adm_scheme <- as.factor(resp_eda$resp_dmg_sp_adm_scheme)
resp_eda$resp_dmg_scholarship <- as.factor(resp_eda$resp_dmg_scholarship)
resp_eda$resp_dmg_mode_of_attend <- as.factor(resp_eda$resp_dmg_mode_of_attend)
resp_eda$resp_dmg_school_leaver <- as.factor(resp_eda$resp_dmg_school_leaver)
resp_eda$resp_dmg_previous_insti <- as.factor(resp_eda$resp_dmg_previous_insti)
resp_eda$resp_dmg_student_comm_yr <- as.factor(resp_eda$resp_dmg_student_comm_yr)
resp_eda$resp_dmg_grad_lvl <- as.factor(resp_eda$resp_dmg_grad_lvl)


eda_web_report(resp_eda, output_dir = ".",
               author = "Deena Iamsiri", output_file = "Response_factor.html",
               theme = "blue", browse = TRUE)

#old version
eda_report(resp_eda, output_dir = ".", output_format="html")

#old version
eda_report(survey, output_dir = ".", output_format="html")
#remove variables with all NA on them

survey <- select(survey
                 ,-sur_location_name
                 ,-sur_coordinator_firstname
                 ,-sur_coordinator_lastname
                 ,-sur_coordinator_email
                 ,-sur_teacher_firstname
                 ,-sur_teacher_lastname
                 ,-sur_teacher_email
                 ,-sur_tobe_deleted
                 ,-sur_delete_reason
                 ,-sur_resp_count
                 ,-sur_p_course_on
                 ,-sur_p_printed
                 ,-sur_questionaire_modes
                 ,-sur_coordinator_order_access)

eda_web_report(survey, output_dir = ".",
               author = "Deena Iamsiri", output_file = "Survey.html",
               theme = "blue", browse = TRUE)

##Review and format data ####
glimpse(response)
glimpse(survey)


