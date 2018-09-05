## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
## This installation needs to be done only for the first time. Comment it once installed.
install_github('adam-m-mcelhinney/helpRFunctions')

library(dplyr)
library(tidyr)
library(devtools)
library(helpRFunctions)
library(ggplot2)
library(reshape2)
library(lubridate)
library(GGally)

## ------------------------------------------------------------------------
data <- read.csv("loan.csv", stringsAsFactors = FALSE)

## ---- fun----------------------------------------------------------------
### Function to get the data types of the columns of a data frame
getDataTypesOfDF = function(df) {
t <- list.df.var.types(newData)
# t$factor
# t$integer
# t$logical
# t$numeric
# t$character
return (t)
}

## Add dummy date to the date columns and convert them to date format
prepDateColumns = function(vec) {
  vec <- paste("01-", vec, sep="")
  vec <- as.Date(vec, format="%d-%b-%y")

## Handling years before 1970
  vec <- as.Date(ifelse(vec >= Sys.Date(), vec - years(100), vec), origin = "1970-01-01")
  return (vec)
}

## Difference between date vectors in months.
getDiffInDates = function(d1, d2) {
  diff <- vector(mode="integer", length(d1))
  for (i in 1:length(d1)) {
    if(!is.na(d1[i]) && !is.na(d2[i])) {
      diff[i] <- length(format(seq(as.Date(d1[i]),as.Date(d2[i]),by="month")))-1
    }
  }
  return(diff)
}

## ---- use_fun------------------------------------------------------------
### Data preparation and cleaning

numDuplicates <- nrow(duplicated(data))

### numDuplicates is NULL
### No duplicate rows are present

## Remove columns that has the same value in all the rows apart from NAs
newData <- data[sapply(data, function(x) length(unique(na.omit(x)))>1)]

## Remove months from the term column and convert it to integer
newData$term = as.numeric(sapply(newData$term, gsub, pattern=" months", replacement=""))

## Remove % int_rate and convert it to integer
newData$int_rate <- as.numeric(sapply(newData$int_rate, gsub, pattern="\\%", replacement=""))
## Remove % in revol_util and convert it to integer
newData$revol_util <- as.numeric(sapply(newData$revol_util, gsub, pattern="\\%", replacement=""))


### Cleaning/Preparing up the Month-Year columns

## Convert Date columns to date format.
newData$issue_d <- prepDateColumns(newData$issue_d)
newData$last_pymnt_d <- prepDateColumns(newData$last_pymnt_d)
newData$next_pymnt_d <- prepDateColumns(newData$next_pymnt_d)
newData$last_credit_pull_d <- prepDateColumns(newData$last_credit_pull_d)
newData$earliest_cr_line <- prepDateColumns(newData$earliest_cr_line)


## Extract the difference between the issue date and last payment date and check if they show any trend in defaulting
newData$act_pymnt_term <- getDiffInDates(newData$issue_d, newData$last_pymnt_d)

### Difference between earliest_cr_line and issue_d
### Commenting this as we are not any trend from this difference
### newData$diff_ear_cr_ln_iss <- getDiffInDates(newData$earliest_cr_line, newData$issue_d)



### id - loan id and member_id are unique for each row and can be removed from the analysis
newData <- within(newData, rm(id)) 
newData <- within(newData, rm(member_id))


## url is in the format https://lendingclub.com/browse/loanDetail.action?loan_id=<loan_id>
## Check if the url has any additional information - if not, we need not consider url in our analysis
if (sum(stringi::stri_cmp(newData$url, paste("https://lendingclub.com/browse/loanDetail.action?loan_id", newData$id, sep="=" ))) == 0){
  message ("url doesn't have any additional information. Ignore")
} else {
  message ("url has some information. Analyse further if it has any valid info")
}

#### Further reducing the number of variables
### Find the correlation between all the numeric and integer variables.
### Remove variables that are highly correlated.

t <- getDataTypesOfDF(newData)
quantVars <- c(t$numeric, t$integer)
resCorr <- cor(newData[quantVars],use="pairwise.complete.obs")

ggsave(file="Correlation_Plot.jpg", device="jpeg", ggcorr(data=NULL, cor_matrix = resCorr,angle=-45, label_alpha = 0.5, size=3,legend.position = "top", nbreaks=8,name = expression(rho)), width = 20, height = 20, units = "cm")

## Remove the variables used for the analysis based on their correlation
#removeVars <- findCorrelation(resCorr, cutoff = 0.5, names=TRUE, exact=TRUE, verbose = TRUE)
#quantVars <- quantVars[removeVars]
#newData <- within(newData, rm(list = removeVars))


#### Removing Categorical variables that are not needed.

### url doesn't have any additional information. Remove that column
newData <- within(newData, rm(url))

### desc contains borrower comment and the date on which the borrower added the comment. Purpose of the loan is mentioned in the Purpose column. So desc column can be removed
newData <- within(newData, rm(desc))

### title - also has multiple information like year of loan, purpose of loan. As such information are covered in other columns, title can be removed.
newData <- within(newData, rm(title))
newData <- within(newData, rm(emp_title))


## addr_state and zip_code - not required for the analysis
newData <- within(newData, rm(addr_state))
newData <- within(newData, rm(zip_code))



#write.csv(x=newData, file = "CleanedLoan1.csv",na = "NA")


#write.csv(x=newData, file = "CleanedLoan2.csv",na = "NA")


## ------------------------------------------------------------------------
## More number of plots are generated in the r code. Plots that are impacting the analysis are put in the presentation


## Get the variables of the Data frame by their data type.
t = getDataTypesOfDF(newData)

## Plot the univariate analysis of the categorical variables - Add loanstatus to the fill color to understand their relationship to the loan_Status
varTypes=t$character
for (i in 1:length(varTypes)) {
    name = paste(varTypes[i], "_Plot.jpg")
    message(varTypes[i])
    stat <- ggplot(data=newData, aes_string(x=varTypes[i], fill="loan_status")) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") + labs(title = paste("Univariate Analysis of ", varTypes[i]))
    ggsave(filename = name, device = "jpeg", plot=stat + geom_bar(stat="count", na.rm = TRUE, width=0.5 ))
  
}  

## Plot the univariate analysis of the integer and numeric variables using boxplot. Add loan status to fill color to understand their relationship to the loan_status
varTypes <- c(t$integer, t$numeric)
for (i in 1:length(varTypes)) {
    name = paste(varTypes[i], "uva_BoxPlot.jpg")
    message(varTypes[i])
    # For adjusting the box plots to handle the outliers
    sts <- boxplot.stats(newData[varTypes[i]][,1])$stats
    stat <- ggplot(data=newData, aes_string(x=factor(0), y=varTypes[i])) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") + labs(title = paste("Univariate Analysis of ", varTypes[i]))
    ggsave(filename = name, device="jpeg", plot = stat + geom_boxplot(stat="boxplot", na.rm = TRUE, outlier.colour = "red" )+ coord_cartesian(ylim = c(sts[1],max(sts)*1.05)))
   
}

## Plot the boxplots of the integer and numeric variables. Add loan status to fill color to understand their relationship to the loan_status
varTypes <- c(t$integer, t$numeric)
categIntVars = c("delinq_2yrs","inq_last_6mths","mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "total_acc", "pub_rec_bankruptcies", "term")
varTypes <- varTypes[! varTypes %in% categIntVars]
for (i in 1:length(varTypes)) {
    name = paste(varTypes[i], "bv_BoxPlot.jpg")
    message(varTypes[i])
    stat <- ggplot(data=newData, aes_string(x=factor(0), y=varTypes[i], color="loan_status")) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),legend.position = "top") + labs(title = paste("Bivariate Analysis of ", varTypes[i]))
    sts <- boxplot.stats(newData[varTypes[i]][,1])$stats
    ggsave(filename = name, device="jpeg", plot = stat + geom_boxplot(stat="boxplot", na.rm = TRUE, position = "dodge", outlier.colour = "red" ) + coord_cartesian(ylim = c(sts[1],max(sts)*1.05)))
}

for (i in 1:length(categIntVars)) {
    name = paste(categIntVars[i], "bv_Plot.jpg")
    message(categIntVars[i])
    stat <- ggplot(data=newData, aes_string(x="loan_status", y=categIntVars[i], fill="loan_status")) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + labs(title = paste("Bivariate Analysis of ", categIntVars[i])) + stat_summary(fun.y = "mean", size = 2, geom = "bar", position = "dodge", show.legend = TRUE)
    ggsave(filename = name, device="jpeg", plot=stat)

}




#}

## ------------------------------------------------------------------------
## Plotting the total funded amount and received amount for each loan_status
means <- aggregate(newData[c("loan_amnt", "funded_amnt_inv", "total_pymnt", "total_pymnt_inv")], by=list(newData$loan_status), mean, na.action = na.pass, na.rm = TRUE)
plot1 <- ggplot(melt(means, id.vars="Group.1" ) , aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge")
ggsave("Amnt_Plot.jpg", plot=plot1, device="jpeg")
plot1


## Plot the number of derogatory public records, public record bankruptcies
means <- aggregate(newData[c("pub_rec", "pub_rec_bankruptcies")], by=list(newData$loan_status), mean, na.action = na.pass, na.rm = TRUE)

plot1 <- ggplot(melt(means, id.vars="Group.1" ) , aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge")
ggsave("pub_rec_bnk.jpg", plot=plot1, device="jpeg")
plot1

## Number of derogatory public records and pub_rec_bankruptcies are high for loans that are charged off

## Plot the number of derogatory public records and pub_rec_bankruptcies for different subgrades
means <- aggregate(newData[c("pub_rec_bankruptcies", "pub_rec")], by=list(newData$grade), mean, na.action = na.pass, na.rm = TRUE)

plot1 <- ggplot(melt(means, id.vars="Group.1" ) , aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") 
ggsave("pub_rec_bnk_sg.jpg", plot=plot1, device="jpeg")
plot1



## Plot the average annual income against grade and loan_status for both 36 and 60 term

plot1 <- ggplot(newData, aes(x = grade, y = annual_inc, fill=factor(loan_status))) +  stat_summary(fun.y = "mean", size = 2, geom = "bar", position = "dodge") + facet_wrap( ~ factor(term), ncol=2) + theme(legend.position = "bottom")
ggsave("ann_inc_gr_ls_tm.jpg", plot=plot1, device="jpeg")
plot1

## Plot the average annual income against emp_length and loan_status

plot1 <- ggplot(newData, aes(x = emp_length, y = annual_inc, fill=factor(loan_status))) +  stat_summary(fun.y = "mean", size = 2, geom = "bar", position = "dodge") + facet_wrap( ~ factor(term), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") 
ggsave("ann_inc_emp_len_ls.jpg", plot=plot1, device="jpeg")
plot1

## Plot the average annual income against emp_length and loan_status and verification_Status

ggplot(newData, aes(x = emp_length, y = annual_inc, fill=factor(loan_status))) +  stat_summary(fun.y = "mean", size = 2, geom = "bar", position = "dodge") + facet_wrap( ~ factor(verification_status), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") 

ggplot(newData, aes(x = emp_length, fill=factor(loan_status))) +  geom_bar(size = 2, position = "dodge") + facet_wrap( ~ factor(verification_status), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") 


## Plot the average int_rate against grade and loan_status

ggplot(newData, aes(x = grade, y = int_rate, fill=factor(loan_status))) + stat_summary(fun.y = "mean", size = 2, geom = "bar",position = "dodge") + facet_wrap( ~ factor(term), ncol=2) + theme(legend.position = "bottom")


## Plot the average installment against grade and loan_status

ggplot(newData, aes(x = grade, y = installment, fill=factor(loan_status))) + stat_summary(fun.y = "mean", size = 2, geom = "bar",position = "dodge") + facet_wrap( ~ factor(term), ncol=2) + theme(legend.position = "bottom")


plot1 <- ggplot(newData, aes(x = grade, fill=factor(loan_status))) + geom_bar(size=2, position="dodge") + facet_wrap( ~ factor(purpose), ncol=3) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top")
ggsave("gr_pur_ls_cnt.jpg", plot=plot1, device="jpeg")
plot1



## Plot the average dti against grade and loan_status

plot1 <- ggplot(newData, aes(x = purpose, y = dti, fill=factor(loan_status))) + stat_summary(fun.y = "median", size = 2, geom = "bar",position = "dodge") + facet_wrap( ~ factor(grade), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=4), axis.text.y = element_text(size=4), legend.position = "top", legend.text = element_text(size=4))
ggsave("dti_pur_gr_ls.jpg", plot=plot1, device="jpeg")
plot1


## Plot the total_acc against loan status and other categorical variables.

plot1 <- ggplot(newData, aes(x = purpose, y = total_acc, fill=factor(loan_status))) + stat_summary(fun.y = "median", size = 2, geom = "bar",position = "dodge") + facet_wrap( ~ factor(grade), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=4), axis.text.y = element_text(size=4), legend.position = "top", legend.text = element_text(size=4))
ggsave("tot_acc_gr_pur.jpg", plot=plot1, device="jpeg")
plot1

## Plot the total_acc against emp_length for different loan_Status and grade

plot1 <- ggplot(newData, aes(x = emp_length, y = total_acc, fill=factor(loan_status))) + stat_summary(fun.y = "median", size = 2, geom = "bar",position = "dodge") + facet_wrap( ~ factor(grade), ncol=2) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=4), axis.text.y = element_text(size=4), legend.position = "top", legend.text = element_text(size=4))
ggsave("total_acc_emp_len_gr.jpg", plot=plot1, device="jpeg")
plot1


