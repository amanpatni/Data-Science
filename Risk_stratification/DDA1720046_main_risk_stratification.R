#load the file 
payer_frame<-read.csv("diabetic_data.csv",header = T, stringsAsFactors = F);


library(ggplot2)
library(tidyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(plyr)
library("sqldf")
View(payer_frame)
str(payer_frame)
head(payer_frame)



times_patient<-sqldf("select patient_nbr,count(*) from payer_frame GROUP BY patient_nbr order by count(*) desc")
times_patient_question<- sqldf("select weight,count(*) from payer_frame group by weight order by count(*) desc")

payer_frame1<-payer_frame

payer_frame1<-payer_frame1[,-c(6,23,25:41,43:47)]
payer_frame2<-payer_frame1
###################################payer_frame1<-payer_frame2
View(payer_frame1)

str(payer_frame1)
summary(payer_frame1)

length(unique((payer_frame1$encounter_id))) 		#101766 
length(payer_frame1$encounter_id)               #101766 , its confirmed that encounter ID is unique.
# as there are very less values in weigth column we have to remove it .
#TREATMENT OF COLUMNS HAVING VALUES ='?' 

payer_frame1$race[payer_frame1$race == '?'] <-'Other'
payer_frame1$medical_specialty[payer_frame1$medical_specialty == '?'] <-'Other'
payer_frame1$payer_code[payer_frame1$payer_code == '?'] <-'Other'

payer_frame1$age<-ifelse(payer_frame1$age=='[0-10)',5,
                         ifelse(payer_frame1$age=='[10-20)',15,
                                ifelse(payer_frame1$age=='[20-30)',25,
                                       ifelse(payer_frame1$age=='[30-40)',35,
                                              ifelse(payer_frame1$age=='[40-50)',45,
                                                     ifelse(payer_frame1$age=='[50-60)',55,
                                                            ifelse(payer_frame1$age=='[60-70)',65,
                                                                   ifelse(payer_frame1$age=='[70-80)',75,
                                                                          ifelse(payer_frame1$age=='[80-90)',85,
                                                                                 ifelse(payer_frame1$age=='[90-100)',95,'please check'))))))))))
           
                    

# Changing the readmitted attribute values from <30, >30 to YES .
payer_frame1$readmitted<-ifelse(payer_frame1$readmitted== '>30' | payer_frame1$readmitted== '<30','YES','NO')
payer_frame1$readmitted<-ifelse(payer_frame1$readmitted== 'YES',1,0)
#payer_frame1$glipizide.metformin<-ifelse(payer_frame1$glipizide.metformin== 'Yes',1,0)
payer_frame1$diabetesMed<-ifelse(payer_frame1$diabetesMed== 'Yes' ,1,0)
payer_frame1$change<-ifelse(payer_frame1$change== 'Ch',1,0)
#Creating dummy variables for categorical variable 


#Creating dummies for the categorical variable having more than 2 values

#For creating the dervied metrics Comorbidity--------------------------------------------------------


payer_frame1$diag_1<-as.numeric(payer_frame1$diag_1)
payer_frame1$diag_2<-as.numeric(payer_frame1$diag_2)
payer_frame1$diag_3<-as.numeric(payer_frame1$diag_3)

payer_frame1$diag_1[is.na(payer_frame1$diag_1)]<-0

payer_frame1$diag_2[is.na(payer_frame1$diag_2)]<-0

payer_frame1$diag_3[is.na(payer_frame1$diag_3)]<-0


payer_frame1$circulatorydisease<-ifelse(payer_frame1$diag_1 >= 390 & payer_frame1$diag_1<460 |
                                    payer_frame1$diag_2>=390 & payer_frame1$diag_2<460 |
                                    payer_frame1$diag_3>=390 & payer_frame1$diag_3<460 ,1,0)
payer_frame1$diabetespatient<-ifelse(payer_frame1$diag_1>=250 & payer_frame1$diag_1<251 | 
                                          payer_frame1$diag_2>=250 & payer_frame1$diag_2<251 |
                                          payer_frame1$diag_3>=250 & payer_frame1$diag_3<251 ,1,0)

payer_frame1$comorbidity<-ifelse(payer_frame1$diabetespatient==0 & payer_frame1$circulatorydisease==0,0,
                                ifelse(payer_frame1$diabetespatient==1 & payer_frame1$circulatorydisease==0,1,
                                       ifelse(payer_frame1$diabetespatient==0 & payer_frame1$circulatorydisease==1,2,
                                              ifelse(payer_frame1$diabetespatient==1 & payer_frame1$circulatorydisease==1,3,4))))
sqldf("select comorbidity,count(*) from payer_frame1 group by comorbidity")
## Only using data related to Diabetes patient 
payer_frame_diabetes<-sqldf("select * from payer_frame1 where diabetesMed==1")
View(payer_frame_diabetes)
nrow(payer_frame_diabetes)

payer_frame_factor <- payer_frame_diabetes[, -c(1,2,5,9,12,13,14,15,16,17,18,19,20,21,24,25,26,27,28)]

# converting categorical attributes to factor
payer_fact <- data.frame(sapply(payer_frame_factor, function(x) factor(x)))
str(payer_fact)

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(payer_fact, 
                             function(x) data.frame(model.matrix(~x-1, data=payer_fact))[, -1]))

# Final dataset


payer_frame_final <- cbind(payer_frame_diabetes[ ,c(1,2,5,9,12,13,14,15,16,17,18,19,20,21,24,25,26,27,28)], dummies)
View(payer_frame_final)
payer_frame_final<-payer_frame_final[,-c(1,2,11,12,13,16,18,19)]
 
payer_frame_final$age<-as.numeric(payer_frame_final$age)

str(payer_frame_final)


##Data Exploration

box_theme <- theme(axis.line=element_blank(),axis.title=element_blank(), 
                   axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y <- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                     legend.position="none")

plot_grid(ggplot(payer_frame_final, aes(age))+ geom_histogram(binwidth = 10),
          ggplot(payer_frame_final, aes(x="",y=age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Scaling numerical attributes
payer_frame_final$age 						<- scale(payer_frame_final$age)
payer_frame_final$num_lab_procedures 				<- scale(payer_frame_final$num_lab_procedures)
payer_frame_final$num_procedures 				<- scale(payer_frame_final$num_procedures)
payer_frame_final$num_medications 				<- scale(payer_frame_final$num_medications)
payer_frame_final$number_outpatient 				<- scale(payer_frame_final$number_outpatient)
payer_frame_final$number_emergency				<- scale(payer_frame_final$number_emergency)
payer_frame_final$number_inpatient				<- scale(payer_frame_final$number_inpatient)
payer_frame_final$number_diagnoses				<- scale(payer_frame_final$number_diagnoses)

View(payer_frame_final)

#=================================
##CHECKPOINT 4: Logistic Regression
#=================================
# Dividing data into Model and Test


set.seed(100)
indices = sample.split(payer_frame_final$readmitted, SplitRatio = 0.7)
train = payer_frame_final[indices, ]
test = payer_frame_final[!(indices), ]

#
model_1 = glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1) 

typeof(model_1)
vif(model_1)
model_2=stepAIC(model_1,direction = "both")
model_2= glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+num_medications+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+change+race.xAsian+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x5+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
        discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
          discharge_disposition_id.x5+discharge_disposition_id.x6+discharge_disposition_id.x7+payer_code.xDM+payer_code.xHM+
          payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
          medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
          medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+medical_specialty.xPodiatry+
          medical_specialty.xSurgery.Vascular+medical_specialty.xSurgicalSpecialty+A1Cresult.xNone+
          insulin.xNo+insulin.xSteady ,family = "binomial", data = train)


model_3<-stepAIC(model_2,direction = 'both')
vif(model_3)
summary(model_3)

## We are removing num_medications

model_4= glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+change+race.xAsian+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x5+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+discharge_disposition_id.x7+payer_code.xDM+payer_code.xHM+
               payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+medical_specialty.xSurgicalSpecialty+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_4)
vif(model_4)

# removing change and surgery speaciality
model_5<-glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xAsian+race.xCaucasian+race.xOther+gender.xMale.xMale+
               admission_type_id.x2+admission_type_id.x5+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+discharge_disposition_id.x7+payer_code.xDM+payer_code.xHM+
               payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)

summary(model_5)
vif(model_5)


#removing race.xAsian, race.admission_type_id.x5,discharge_disposition_id.x7
model_6<-glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+payer_code.xDM+payer_code.xHM+
               payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_6)

#removing payer.xHM
model_7<-glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+payer_code.xDM+
               payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_7)

#removing payer.xDM
model_8<-glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+
               payer_code.xMC+payer_code.xMD+payer_code.xOG+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_8)

#removing payer_code.xOG

model_9<-glm(formula=readmitted ~ age + time_in_hospital + num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               discharge_disposition_id.x18+discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+
               payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_9)
vif_df<-vif(model_9)
vif_df[order(vif_df,decreasing=TRUE)]

#removing dischage x18
model_10<-glm(formula=readmitted ~ age  +time_in_hospital+ num_lab_procedures +num_procedures+number_outpatient+
               number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
               admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
               discharge_disposition_id.x14+
               +discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
               discharge_disposition_id.x5+discharge_disposition_id.x6+
               payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
               medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
               medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
               medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
               insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_10)



model_11<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+discharge_disposition_id.x23+discharge_disposition_id.x28+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
                medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_11)

#disposition id x28
model_12<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+discharge_disposition_id.x23+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
                medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                medical_specialty.xSurgery.Vascular+A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_12)

#removing medical _speciality Vascular 
model_13<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+discharge_disposition_id.x23+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
                medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_13)

#removing discharge_disposition_id.x23                  
model_14<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice+
                medical_specialty.xHematology+medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_14)

#remvoing medical_specialty.xHematology
model_15<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+discharge_disposition_id.x13+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice
                +medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_15)

#remvoing discharge_disposition_id.x13
model_16<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice
              +medical_specialty.xInfectiousDiseases+medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_16)

#remvoing circulatorydisease,medical_specialty.xInfectiousDiseases
model_17<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x2+admission_type_id.x6+
                discharge_disposition_id.x14+
                +discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+medical_specialty.xFamily.GeneralPractice
              +medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_17)

#remvoing medical_specialty.xFamily.GeneralPractice,admission_type_id.x2
model_18<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+gender.xMale+
                admission_type_id.x6+discharge_disposition_id.x14+discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+
                medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_18)


#remvoing gender.xMale
model_19<-glm(formula=readmitted ~ age + num_lab_procedures +num_procedures+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+race.xCaucasian+race.xOther+
                admission_type_id.x6+discharge_disposition_id.x14+discharge_disposition_id.x22+
                discharge_disposition_id.x5+discharge_disposition_id.x6+
                payer_code.xMC+payer_code.xMD+payer_code.xOther+payer_code.xSP+
                medical_specialty.xNephrology+medical_specialty.xObstetricsandGynecology+
                medical_specialty.xOrthopedics.Reconstructive+medical_specialty.xPediatrics.Endocrinology+
                +A1Cresult.xNone+
                insulin.xNo+insulin.xSteady ,family = "binomial", data = train)
summary(model_19)

final_model<-model_19
###---------------------------------------------------------Logistic Regression Model END--------------------------------------###

test_pred = predict(final_model, type = "response", 
                    newdata = test[, -2])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$readmitted==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

library(e1071)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#####end logistic regression

#decision tree 

library(rpart)
library(rpart.plot)
library(caret)

#1 build tree model- default hyperparameters
tree.model <- rpart(readmitted ~ .,                     # formula
                    data = train,                   # training data
                    method = "class")               # classification or regression
# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$readmitted, positive='0')

#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree.model <- rpart(readmitted ~ .,                     # formula
                    data = train,                   # training data
                    method = "class",               # classification or regression
                    parms = list(split = "information")
)

#3 Tune the hyperparameters ----------------------------------------------------------
tree.model <- rpart(readmitted ~ .,                               
                    data = train,                            
                    method = "class",                        
                    control = rpart.control(minsplit = 1000,  
                                            minbucket = 1000, 
                                            cp = 0.05))       

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$readmitted, positive = "1")  

#4 A more complex tree -----------------------------------------------------------------
tree.model <- rpart(readmitted ~ .,                                
                    data = train,                             
                    method = "class",                         
                    control = rpart.control(minsplit = 1,     
                                            minbucket = 1,    
                                            cp = 0.001))     

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$readmitted, positive = "1") 

