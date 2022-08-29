library(readxl)
library(dplyr)
library(data.table)
library(arrow)
library(dlookr)


# EDA on 2021 SFS data ----
response <- read_excel("/Users/yixinzhang/Desktop/SFS_data/response/SFS_Responses_Export_File_2021.xlsx", sheet = 1)
survey<-read_excel("/Users/yixinzhang/Desktop/SFS_data/SFS_Surveys_Export_File_2021.xlsx", sheet = 1)

#select columns for each dataset
survey_cols<-survey[c('SUR_ID','SUR_YEAR','SUR_SUBJECT_ID','SUR_SUBJECT_NAME','SUR_FACULTY_ID','SUR_STUDENT_COUNT','SUR_STUDENT_COUNT_ONL',
                 'SUR_FORM','SUR_QUESTIONAIRE')]

response_cols<-response[c('RESP_ID','RESP_SURVEY_ID','RESP_Q_ID','RESP_POINTS','RESP_COMMENT','RESP_COURSE','RESP_DATE','RESP_DMG_GENDER'
                ,'RESP_DMG_AGE','RESP_DMG_COB','RESP_DMG_INT_ATTEND_TYPE','RESP_DMG_MODE_OF_ATTEND','RESP_DMG_GRAD_LVL','RESP_DMG_MODE_OF_ATTEND')]

# Load all files ----
path<-"/Users/yixinzhang/Desktop/SFS_data/response"
file.list <- list.files(path,pattern="*.xlsx",full.names = TRUE, recursive = TRUE)

df.list <- sapply(file.list, read_excel,simplify=FALSE)
response_all <- df.list%>%
  lapply(\(x) mutate(x, across(RESP_COMMENT, as.character))) %>%
  lapply(\(x) mutate(x, across(RESP_IMAGE, as.character)))%>%
  lapply(\(x) mutate(x, across(RESP_DMG_GENDER, as.character)))%>%
  lapply(\(x) mutate(x, across(RESP_FIELDOFSTUDY, as.character)))%>%
                bind_rows()

path<-"/Users/yixinzhang/Desktop/SFS_data/survey"
file.list1 <- list.files(path,pattern="*.xlsx",full.names = TRUE, recursive = TRUE)

df.list1 <- sapply(file.list1, read_excel,simplify=FALSE)
survey_all <- df.list1%>%
  lapply(\(x) mutate(x, across(SUR_CLASS_2, as.character)))%>%
  lapply(\(x) mutate(x, across(SUR_STD_QUESTIONS, as.character)))%>%
  lapply(\(x) mutate(x, across(SUR_EXT_QUESTIONS, as.character)))%>%
  bind_rows()

write.csv(survey_all, "survey_all.csv")
write_parquet(response_all, "survey_all.parquet")

#Load combined files

survey<-read.csv("survey_all.csv")
response<-read_parquet("response_all.parquet",as_tibble = TRUE)

eda_report(response, output_dir = ".", output_format="html")






#join two datasets by survey ID. 


#remove nulls for freetext columns

#sentimental analysis
#data dictionary