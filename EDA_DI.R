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

#File:
response <- clean_names(read_parquet(file_response, as_tibble = TRUE))
survey <- clean_names(read_csv(file_survey))

#EDA####
#old version
eda_report(response, output_dir = ".", output_format="html")

#remove variables with only NA
response <- select(response, -resp_image, -resp_dmg_school_leaver)
#change logical to character

response$resp_dmg_scholarship <- as.character(response$resp_dmg_scholarship)
response$resp_import_batch <- as.character(response$resp_import_batch)


eda_web_report(response, output_dir = ".",
               author = "Deena Iamsiri", output_file = "Response.html",
               theme = "blue", browse = TRUE)

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

