library(readr)
library(tidyr)
library(randomForest)
library(dplyr)
library(caret)
library(kernlab)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(corrplot)


#install.packages("randomForest")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("caret")

################################################################################################################################################################
readmissions_and_Deaths_Hospital <- read.csv("Readmissions and Deaths - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

readmissions_and_Deaths_Hospital <- within(readmissions_and_Deaths_Hospital,rm("Hospital.Name",
                                                                               "Address",
                                                                               "City",
                                                                               "State",
                                                                               "County.Name",
                                                                               "Phone.Number",
                                                                               "Measure.Name",
                                                                               "Measure.Start.Date",
                                                                               "Measure.End.Date",
                                                                               "ZIP.Code","Compared.to.National",
                                                                               "Denominator","Lower.Estimate","Higher.Estimate",
                                                                               "Footnote"))
################################################################################################################################################################
#EDA
################################################################################################################################################################


################################################################################################################################################################

sum(is.na(readmissions_and_Deaths_Hospital$Measure.ID))
sum(is.na(readmissions_and_Deaths_Hospital$Score))

levels(as.factor(readmissions_and_Deaths_Hospital$Measure.ID))
readmissions_and_Deaths_Hospital$Score[readmissions_and_Deaths_Hospital$Score=="Not Available"]<-"-1"
readmissions_and_Deaths_Hospital$Score[readmissions_and_Deaths_Hospital$Score=="-1"]<-median(as.numeric(readmissions_and_Deaths_Hospital$Score))

readmissiondf<-spread(readmissions_and_Deaths_Hospital,Measure.ID, Score)
View(readmissiondf)
write.csv(readmissiondf,file = "readmissiondf.csv")
################################################################################################################################################################

complications_hospital <- read.csv("Complications - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

complications_hospital <- within(complications_hospital,rm("Hospital.Name",
                                                           "Address",
                                                           "City",
                                                           "State",
                                                           "County.Name",
                                                           "Phone.Number",
                                                           "Measure.Name",
                                                           "ZIP.Code",
                                                           "Footnote",
                                                           "Lower.Estimate",
                                                           "Higher.Estimate",
                                                           "Measure.Start.Date",
                                                           "Compared.to.National",
                                                           "Denominator",
                                                           "Measure.End.Date"))

################################################################################################################################################################
#EDA
################################################################################################################################################################

################################################################################################################################################################

sum(is.na(complications_hospital$Provider.ID))
sum(is.na(complications_hospital$Measure.ID))
sum(is.na(complications_hospital$Score))

complications_hospital$Score[complications_hospital$Score=="Not Available"] <- -1 
complications_hospital$Score[complications_hospital$Score=="-1"]<-median(as.numeric(complications_hospital$Score))


levels(as.factor(complications_hospital$Measure.ID))
complications_hospital<-spread(complications_hospital,Measure.ID, Score)
complications_hospital<- subset(complications_hospital,select=(c("Provider.ID","PSI_4_SURG_COMP","COMP_HIP_KNEE","PSI_90_SAFETY")))
View(complications_hospital)
write.csv(complications_hospital,file = "complications_hospitaldf.csv")
################################################################################################################################################################
#concat PSI_4_SURG_COMP_Score
df<-merge(readmissiondf,complications_hospital,x.by="Provider_ID")
View(df)

################################################################################################################################################################
timely_and_Effective_Care_Hospital <- read.csv("Timely and Effective Care - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))


timely_and_Effective_Care_Hospital <- within(timely_and_Effective_Care_Hospital,rm("Hospital.Name",
                                                                                   "Address",
                                                                                   "City",
                                                                                   "State",
                                                                                   "County.Name",
                                                                                   "Phone.Number",
                                                                                   "Measure.Name",
                                                                                   "Measure.Start.Date",
                                                                                   "Sample",
                                                                                   "Footnote",
                                                                                   "Condition",
                                                                                   "Measure.End.Date"))

################################################################################################################################################################
#EDA
################################################################################################################################################################

################################################################################################################################################################
sum(is.na(timely_and_Effective_Care_Hospital$Measure.ID))
sum(is.na(timely_and_Effective_Care_Hospital$Score))

levels(as.factor(timely_and_Effective_Care_Hospital$Measure.ID))

timely_and_Effective_Care_Hospital$Score[timely_and_Effective_Care_Hospital$Score=="Not Available"]<-"-1"

k<-timely_and_Effective_Care_Hospital[!timely_and_Effective_Care_Hospital$Score=="High (40,000 - 59,999 patients annually)",]
j<-k[!k$Score=="Very High (60,000+ patients annually)",]
l<-j[!j$Score=="Low (0 - 19,999 patients annually)",]
m<-l[!l$Score=="Medium (20,000 - 39,999 patients annually)",]
timely_and_Effective_Care_Hospital<-m
levels(as.factor(timely_and_Effective_Care_Hospital$Score))

timely_and_Effective_Care_Hospital$Score[timely_and_Effective_Care_Hospital$Score=="-1"]<-mean(as.numeric(timely_and_Effective_Care_Hospital$Score))

timely_and_Effective_Care_Hospital<-spread(timely_and_Effective_Care_Hospital,Measure.ID, Score)

timely_and_Effective_Care_Hospital<- subset(timely_and_Effective_Care_Hospital,select=c("Provider.ID","ED_1b","ED_1b","OP_18b",
                                                                                        "OP_20","OP_21","OP_3b","OP_5","CAC_3",
                                                                                        "IMM_2","IMM_3_OP_27_FAC_ADHPCT","OP_22",
                                                                                        "OP_23","OP_29","OP_30","OP_4","PC_01","STK_4",
                                                                                        "STK_5","STK_6","STK_8","VTE_1","VTE_2","VTE_3",
                                                                                        "VTE_5","VTE_6"))

View(timely_and_Effective_Care_Hospital)
write.csv(timely_and_Effective_Care_Hospital,file = "timely_and_Effective_Care_Hospitaldf.csv")
################################################################################################################################################################
#concat timely df
df<-merge(df,timely_and_Effective_Care_Hospital,x.by="Provider_ID")
View(df)

################################################################################################################################################################

outpatient_Imaging_Efficiency_Hospital <- read.csv("Outpatient Imaging Efficiency - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

outpatient_Imaging_Efficiency_Hospital <- within(outpatient_Imaging_Efficiency_Hospital,rm("Hospital.Name",
                                                                                           "Address",
                                                                                           "City",
                                                                                           "State",
                                                                                           "County.Name",
                                                                                           "Phone.Number",
                                                                                           "Measure.Name",
                                                                                           "Footnote",
                                                                                           "ZIP.Code",
                                                                                           "Measure.Start.Date",
                                                                                           "Measure.End.Date"))

################################################################################################################################################################
#EDA
################################################################################################################################################################

################################################################################################################################################################

outpatient_Imaging_Efficiency_Hospital$Score[outpatient_Imaging_Efficiency_Hospital$Score=="Not Available"]<-"-1"

outpatient_Imaging_Efficiency_Hospital$Score[outpatient_Imaging_Efficiency_Hospital$Score=="-1"]<-median(as.numeric(outpatient_Imaging_Efficiency_Hospital$Score))

levels(as.factor(outpatient_Imaging_Efficiency_Hospital$Measure.ID))

outpatient_Imaging_Efficiency_Hospital<-spread(outpatient_Imaging_Efficiency_Hospital,Measure.ID, Score)

outpatient_Imaging_Efficiency_Hospital<- subset(outpatient_Imaging_Efficiency_Hospital,select=c("Provider.ID","OP_10","OP_11","OP_13","OP_14","OP_8"))

View(outpatient_Imaging_Efficiency_Hospital)
write.csv(outpatient_Imaging_Efficiency_Hospital,file = "outpatient_Imaging_Efficiency_Hospitaldf.csv")
################################################################################################################################################################
#concat outpatient df
df<-merge(df,outpatient_Imaging_Efficiency_Hospital,x.by="Provider_ID")
View(df)

################################################################################################################################################################
# Healthcare associated infection
################################################################################################################################################################
healthcareInfections <- read.csv("Healthcare Associated Infections - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

healthcareInfections <- within(healthcareInfections,rm("Hospital.Name","Address","City","State",
                                                       "County.Name",
                                                       "Phone.Number",
                                                       "Measure.Name",
                                                       "Footnote",
                                                       "ZIP.Code",
                                                       "Compared.to.National",
                                                       "Measure.Start.Date",
                                                       "Measure.End.Date"))

################################################################################################################################################################
#EDA
################################################################################################################################################################

################################################################################################################################################################

sum(is.na(healthcareInfections$Measure.ID))
sum(is.na(healthcareInfections$Provider.ID))
sum(is.na(healthcareInfections$Score))

levels(as.factor(healthcareInfections$Measure.ID))
levels(as.factor(healthcareInfections$Score))


healthcareInfections$Score[healthcareInfections$Score=="Not Available"]<-"-1"

healthcareInfections$Score[healthcareInfections$Score=="-1"]<-median(as.numeric(healthcareInfections$Score))

healthcareInfections<-spread(healthcareInfections,Measure.ID, Score)

healthcareInfections<- subset(healthcareInfections,select=c("Provider.ID","HAI_1_SIR","HAI_2_SIR","HAI_3_SIR","HAI_4_SIR","HAI_5_SIR","HAI_6_SIR"))


View(healthcareInfections)
write.csv(healthcareInfections,file = "healthcareInfectionsdf.csv")
################################################################################################################################################################
#concat outpatient df
df<-merge(df,healthcareInfections,x.by="Provider_ID")
View(df)


################################################################################################################################################################
#HAHPS data
################################################################################################################################################################
HCAHPS_Hospital <- read.csv("HCAHPS - Hospital.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

HCAHPS_Hospital <- within(HCAHPS_Hospital,rm("Hospital.Name","Address",
                                             "City","State",
                                             "County.Name",
                                             "Phone.Number",
                                             "HCAHPS.Question","HCAHPS.Answer.Description","Patient.Survey.Star.Rating",
                                             "Patient.Survey.Star.Rating.Footnote","HCAHPS.Answer.Percent","HCAHPS.Answer.Percent.Footnote","Number.of.Completed.Surveys",
                                             "Number.of.Completed.Surveys.Footnote","Survey.Response.Rate.Percent","Survey.Response.Rate.Percent.Footnote",
                                             "Measure.Start.Date","Measure.End.Date",
                                             "ZIP.Code"))

sum(is.na(HCAHPS_Hospital$HCAHPS.Measure.ID))
sum(is.na(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value))

levels(as.factor(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value))

HCAHPS_Hospital$HCAHPS.Linear.Mean.Value[HCAHPS_Hospital$HCAHPS.Linear.Mean.Value=="Not Applicable"]<-"-1"
HCAHPS_Hospital$HCAHPS.Linear.Mean.Value[HCAHPS_Hospital$HCAHPS.Linear.Mean.Value=="Not Available"]<-"-1"

HCAHPS_Hospital$HCAHPS.Linear.Mean.Value[HCAHPS_Hospital$HCAHPS.Linear.Mean.Value=="-1"]<-mean(as.numeric(HCAHPS_Hospital$HCAHPS.Linear.Mean.Value))

HCAHPS_Hospital<-spread(HCAHPS_Hospital,HCAHPS.Measure.ID, HCAHPS.Linear.Mean.Value)

colnames(HCAHPS_Hospital)
HCAHPS_Hospital_df <- subset(HCAHPS_Hospital,select=c("Provider.ID","H_CLEAN_LINEAR_SCORE",
                                                      "H_COMP_1_LINEAR_SCORE","H_COMP_2_LINEAR_SCORE","H_COMP_3_LINEAR_SCORE",
                                                      "H_COMP_4_LINEAR_SCORE","H_COMP_5_LINEAR_SCORE","H_COMP_6_LINEAR_SCORE","H_COMP_7_LINEAR_SCORE",
                                                      "H_HSP_RATING_LINEAR_SCORE","H_QUIET_LINEAR_SCORE","H_RECMND_LINEAR_SCORE"))

View(HCAHPS_Hospital_df)
write.csv(HCAHPS_Hospital_df,file = "HCAHPS_Hospital_df.csv")
################################################################################################################################################################
#concat HCAHPS df
df<-merge(df,HCAHPS_Hospital_df,x.by="Provider_ID")
View(df)


################################################################################################################################################################
#General Hospital Information
################################################################################################################################################################
Hospital_General_Information <- read.csv("Hospital General Information.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))
hospital_rating<- subset(Hospital_General_Information,select=c("Provider.ID","Hospital.overall.rating"))
View(hospital_rating)
write.csv(hospital_rating,file = "hospital_ratingdf.csv")

#concat PSI_4_SURG_COMP_Score
df<-merge(df,hospital_rating,x.by="Provider_ID")
View(df)

afterNARemovalDf<-df[!df$Hospital.overall.rating=="Not Available",]
View(afterNARemovalDf)
write.csv(afterNARemovalDf,file = "finalDf.csv")
################################################################################################################################################################
#Model building
################################################################################################################################################################
################################################################################################################################################################
#Random Forest modelling
################################################################################################################################################################
afterNARemovalDf$Hospital.overall.rating<- as.factor(afterNARemovalDf$Hospital.overall.rating)

n<-nrow(afterNARemovalDf)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)

trainDf<-afterNARemovalDf[trainIndex ,]
testDf<-afterNARemovalDf[-trainIndex,]
modelCms<-randomForest(Hospital.overall.rating ~ . ,trainDf, ntree=800)
predictdf<-predict(modelCms,testDf[,1:65])

summary(predictdf) 
print(predictdf)
#confusion matrix - Linear Kernel
cf_linear<-confusionMatrix(predictdf,as.factor(testDf$Hospital.overall.rating))
cf_linear
plot(cf_linear$table)

confusionDF.linear <- data.frame(confusionMatrix(predictdf,as.factor(testDf$Hospital.overall.rating))$table)
confusionDF.linear$Reference = with(confusionDF.linear, 
                                    factor(Reference, levels = rev(levels(Reference))))
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

ggplot(
  confusionDF.linear, aes(x = Prediction, y = Reference, fill = Freq)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  labs(x = "Predicted", y = "Actual") +
  scale_fill_gradient2(
    low = jBuPuPalette[1],
    mid = jBuPuPalette[paletteSize/2],
    high = jBuPuPalette[paletteSize],
    midpoint = (max(confusionDF.linear$Freq) + min(confusionDF.linear$Freq)) / 2,
    name = "") +
  theme(legend.key.height = unit(2, "cm"))
################################################################################################################################################################
#CrossValidation results for random forest
################################################################################################################################################################
#Confusion Matrix and Statistics

#Reference
#Prediction   1   2   3   4   5
#1   5   0   0   0   0
#2  25 126   9   0   0
#3   1  81 470 113   0
#4   0   0  34 193  18
#5   0   0   0   3  16

#Overall Statistics

#Accuracy : 0.7404          
#95% CI : (0.7133, 0.7662)
#No Information Rate : 0.4689          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.5836          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
#Sensitivity           0.16129   0.6087   0.9162   0.6246  0.47059
#Specificity           1.00000   0.9617   0.6644   0.9338  0.99717
#Pos Pred Value        1.00000   0.7875   0.7068   0.7878  0.84211
#Neg Pred Value        0.97612   0.9133   0.8998   0.8634  0.98326
#Prevalence            0.02834   0.1892   0.4689   0.2824  0.03108
#Detection Rate        0.00457   0.1152   0.4296   0.1764  0.01463
#Detection Prevalence  0.00457   0.1463   0.6079   0.2239  0.01737
#Balanced Accuracy     0.58065   0.7852   0.7903   0.7792  0.73388


################################################################################################################################################################
#CrossValidation with GBM
################################################################################################################################################################
fitControl <-trainControl(method ="cv",number=5)

grid <- expand.grid(interaction.depth = 2,
                    n.trees = 500,
                    shrinkage = 0.1,
                    n.minobsinnode = 10)
set.seed(999)

fit <- train(Hospital.overall.rating ~ ., data = trainDf,
             method = "gbm",
             trControl = fitControl,
             verbose = FALSE,
             tuneGrid = grid)

predicted = predict(fit,testDf[,1:65])
eval.gbm<-confusionMatrix(predicted,as.factor(testDf$Hospital.overall.rating))
eval.gbm
plot(eval.gbm$table)

confusionDF.gbm <- data.frame(confusionMatrix(predicted,as.factor(testDf$Hospital.overall.rating))$table)
confusionDF.gbm$Reference = with(confusionDF.gbm, 
                                 factor(Reference, levels = rev(levels(Reference))))
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

ggplot(
  confusionDF.gbm, aes(x = Prediction, y = Reference, fill = Freq)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  labs(x = "Predicted", y = "Actual") +
  scale_fill_gradient2(
    low = jBuPuPalette[1],
    mid = jBuPuPalette[paletteSize/2],
    high = jBuPuPalette[paletteSize],
    midpoint = (max(confusionDF.gbm$Freq) + min(confusionDF.gbm$Freq)) / 2,
    name = "") +
  theme(legend.key.height = unit(2, "cm"))

################################################################################################################################################################
#factor analysis
################################################################################################################################################################
View(afterNARemovalDf[,2:66])
sapply(afterNARemovalDf, class)

complications_hospitaldf <- read.csv("complications_hospitaldf.csv", stringsAsFactors=FALSE)
hcahps_hospital_df <- read.csv("HCAHPS_Hospital_df.csv", stringsAsFactors=FALSE)
healthcareInfectionsdf <- read.csv("healthcareInfectionsdf.csv", stringsAsFactors=FALSE)
readmissionsdf <- read.csv("readmissiondf.csv", stringsAsFactors=FALSE)
timely_and_Effective_df <- read.csv("timely_and_Effective_Care_Hospitaldf.csv", stringsAsFactors=FALSE)
outpatient_imaging_df<-read.csv("outpatient_Imaging_Efficiency_Hospitaldf.csv", stringsAsFactors=FALSE)

complications_df<-within(complications_hospitaldf,rm("X"))
hcahps_hospital_df<-within(hcahps_hospital_df,rm("X"))
healthcareInfectionsdf<- within(healthcareInfectionsdf,rm("X"))
readmissionsdf<- within(readmissionsdf,rm("X"))
timely_and_Effective_df<- within(timely_and_Effective_df,rm("X"))
outpatient_imaging_df<- within(outpatient_imaging_df,rm("X"))

#Checking for the principal components for complications group to find the underlying factors.
pa_complications<- princomp(complications_df[,2:4],scores=TRUE,cor = TRUE)
summary(pa_complications)
plot(pa_complications)
corrplot(cor(complications_df))
pa_complications_fa<- factanal(complications_df[,2:4],factors = 1,rotation = "varimax" , scores = "regression")
pa_complications_fa
complications_score<-pa_complications_fa$scores
colnames(complications_score)<- c("complications_latent_variable_score")
complications_with_score<-cbind(complications_df,complications_score)

#Checking for the principal components for hcahps group to find the underlying factors.
pa_hcahps<- princomp(hcahps_hospital_df[,2:12],scores=TRUE,cor = TRUE)
summary(pa_hcahps)
plot(pa_hcahps)
corrplot(cor(hcahps_hospital_df[,2:12]))
hcahps_hospital_fa<- factanal(hcahps_hospital_df[,2:12],factors = 1,rotation = "varimax" , scores = "regression")
hcahps_hospital_fa
hcahps_hospital_score<-hcahps_hospital_fa$scores
colnames(hcahps_hospital_score)<- c("hca_hps_latent_variable_score")
hcahps_hospital_df_with_score<-cbind(hcahps_hospital_df,complications_score)

#Checking for the principal components for HealthCare Infections group to find the underlying factors.
pa_healthcareInfections<- princomp(healthcareInfectionsdf,scores=TRUE,cor = TRUE)
summary(pa_healthcareInfections)
plot(pa_healthcareInfections)
corrplot(cor(healthcareInfectionsdf[,2:7]))
healthcareInfections_fa<- factanal(healthcareInfectionsdf[,2:7],factors = 3,rotation = "varimax" , scores = "regression")
healthcareInfections_fa
healthInfections_score<-healthcareInfections_fa$scores
View(healthInfections_score)
colnames(healthInfections_score)<- c("healthInfection_f1","healthInfection_f2","healthInfection_f3")
healthcareInfectionsdf_with_score<-cbind(healthcareInfectionsdf,healthInfections_score)

#Checking for the principal components for readmissions group to find the underlying factors.
pa_readmissionsdf<- princomp(readmissionsdf,scores=TRUE,cor = TRUE)
loadings(pa_readmissionsdf)
screeplot(pa_readmissionsdf,type="line",main="scree plot")
biplot(pa_readmissionsdf)
summary(pa_readmissionsdf)
plot(pa_readmissionsdf)
corrplot(cor(readmissionsdf))
ncol(readmissionsdf)
readmissions_fa<- factanal(readmissionsdf[,2:15],factors = 6,rotation = "varimax" , scores = "regression")
readmissions_fa
readmissions_score<-readmissions_fa$scores
colnames(readmissions_score)<- c("readmissions_f1","readmissions_f2","readmissions_f3","readmissions_f4","readmissions_f5","readmissions_f6")
readmissionssdf_with_score<-cbind(readmissionsdf,readmissions_score)


#Checking for the principal components for readmissions group to find the underlying factors.
pa_timely_and_Effective<- princomp(timely_and_Effective_df,scores=TRUE,cor = TRUE)
summary(pa_timely_and_Effective)
plot(pa_timely_and_Effective)
corrplot(cor(timely_and_Effective_df))
#deleting the cols
timely_and_Effective_df<- within(timely_and_Effective_df, rm("OP_4","ED_1b.1"))
View(timely_and_Effective_df[,2:24])
timely_effective_fa<- factanal(timely_and_Effective_df[,2:24],factors = 4,rotation = "varimax" , scores = "regression")
timely_effective_fa
timely_effective_score<-timely_effective_fa$scores
View(timely_effective_score)
colnames(timely_effective_score)<- c("timely_effective_f1","timely_effective_f2","timely_effective_f3","timely_effective_f4")
timely_and_Effective_with_score<-cbind(timely_and_Effective_df,timely_effective_score)

#Checking for the principal components for outpatient imaging group to find the underlying factors.
pa_outpatient_imaging<- princomp(outpatient_imaging_df[,2:6],scores=TRUE,cor = TRUE)
summary(pa_outpatient_imaging)
plot(pa_outpatient_imaging)
outpatient_imaging_fa<- factanal(outpatient_imaging_df[,2:6],factors = 2,rotation = "varimax" , scores = "regression")
outpatient_imaging_fa
outpatient_imaging_score<-outpatient_imaging_fa$scores
View(outpatient_imaging_score)
colnames(outpatient_imaging_score)<- c("outpatient_imaging_f1","outpatient_imaging_f2")
outpatient_imaging_with_score<-cbind(outpatient_imaging_df,outpatient_imaging_score)

################################################################################################################################################################
#merge all scores
################################################################################################################################################################
final_score<-cbind(complications_score,hcahps_hospital_score)
final_score<-cbind(final_score,hcahps_hospital_score)
final_score<-cbind(final_score,readmissions_score)
final_score<-cbind(final_score,timely_effective_score)
final_score<-cbind(final_score,outpatient_imaging_score)
View(final_score)

################################################################################################################################################################
#K-means clustering model
################################################################################################################################################################

install.packages('clue')
library(clue)
set.seed(20)

final_score_scaled<-scale(final_score)
n<-nrow(final_score_scaled)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)

trainDF<-final_score_scaled[trainIndex,]
testDF<-final_score_scaled[-trainIndex,]
View(trainDF)
View(testDF)
cmsCluster_train<- kmeans(trainDF, 5,20)
View(cmsCluster_train)
predictdf_kmeans<-cl_predict(cmsCluster_train,testDF)
summary(predictdf_kmeans)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   3.088   4.000   5.000 

print(predictdf_kmeans)

################################################################################################################################################################
################################################################################################################################################################
#Hclust clustering model
################################################################################################################################################################
set.seed(88)

n<-nrow(final_score_scaled)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)

trainDf<-final_score_scaled[trainIndex ,]
testDf<-final_score_scaled[-trainIndex,]
clusters <- hclust(dist(final_score_scaled))
plot(clusters)
clusterCut <- cutree(clusters, 5)
clusterCut

################################################################################################################################################################
#Clustering is not supposed to "classify" new data, as the name suggests - it is the core concept of classification.

#Some of the clustering algorithms (like those centroid based - kmeans, kmedians etc.) can "label" new instance based on the model created. Unfortunately hierarchical clustering is not one of them - it does not partition the input space, it just "connects" some of the objects given during clustering, so you cannot assign the new point to this model.

#The only "solution" to use the hclust in order to "classify" is to create another classifier on top of the labeled data given by hclust. 
#For example you can now train knn (even with k=1) on the data with labels from hclust and use it to assign labels to new points.
