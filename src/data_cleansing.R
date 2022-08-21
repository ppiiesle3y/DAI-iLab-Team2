library("readxl")

#read 2021 SFS data
response <- read_excel("/Users/yixinzhang/Desktop/SFS_data/SFS_Responses_Export_File_2021.xlsx", sheet = 1)
survey<-read_excel("/Users/yixinzhang/Desktop/SFS_data/SFS_Surveys_Export_File_2021.xlsx", sheet = 1)

#select columns for each dataset
survey_cols<-survey[c('SUR_ID','SUR_YEAR','SUR_SUBJECT_ID','SUR_SUBJECT_NAME','SUR_FACULTY_ID','SUR_STUDENT_COUNT','SUR_STUDENT_COUNT_ONL',
                 'SUR_FORM','SUR_QUESTIONAIRE')]

response_cols<-response[c('RESP_ID','RESP_SURVEY_ID','RESP_Q_ID','RESP_POINTS','RESP_COMMENT','RESP_COURSE','RESP_DATE','RESP_DMG_GENDER'
                ,'RESP_DMG_AGE','RESP_DMG_COB','RESP_DMG_INT_ATTEND_TYPE','RESP_DMG_MODE_OF_ATTEND','RESP_DMG_GRAD_LVL','RESP_DMG_MODE_OF_ATTEND')]

#join two datasets by survey ID. 
#remove nulls for freetext columns

#sentimental analysis
#data dictionary