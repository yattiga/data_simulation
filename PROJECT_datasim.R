set.seed(12345)
library(truncnorm)
library(GGally)
GHQn <- 20065
CSn <- 20435
GHQmean <- 10.899
GHQstd <- 5.108
MCSmean <- 51.084
MCSstd <- 9.517
PCSmean <- 50.385
PCSstd <- 10.562

GHQage_false <- rnorm(GHQn, 47.195, 16.619)
GHQage <- rtruncnorm(GHQn, a=16, b= 110, 47.195, 16.619)
CSage <- rtruncnorm(CSn, a=16, b= 110, 47.157, 16.679)
GHQsex <- rbinom(GHQn, 1, 0.56058) #1 is female
CSsex <- rbinom(CSn, 1, 0.56046)
GHQsex <- ifelse(GHQsex==1, 'female', 'male')
CSsex <- ifelse(CSsex==1, 'female', 'male')
GHQrace <- as.factor(sample(c('white', 'asian', 'black', 'mixed', 'other'), GHQn, replace=TRUE, prob=c(0.87944, 0.06394, 0.03025, 0.01510, 0.01126)))
CSrace <- as.factor(sample(c('white', 'asian', 'black', 'mixed', 'other'), CSn, replace=TRUE, prob=c(0.87649, 0.06543, 0.03030, 0.01515, 0.1130)))
GHQdf <- data.frame(GHQage, GHQsex)
CSdf <- data.frame(CSage, CSsex)
colnames(GHQdf) <- c('age', 'sex')
colnames(CSdf) <- c('age', 'sex')

# This dataset has ~5 times more unemployed people than reality in the U.K. (house of commons library: http://researchbriefings.parliament.uk/ResearchBriefing/Summary/SN06385)
# Let's find the factor of difference between reality and this data set

actual_unemploy <- as.vector(c(0.05, 0.12, 0.06, 0.12, 0.09))
study_unemploy <- 0.39
un_diff <- study_unemploy/actual_unemploy

actual_employ <- as.vector(c(0.95, 0.88, 0.94, 0.88, 0.91))
study_employ <- 0.61
diff <- study_employ/actual_employ

# Now we can use that factor to better estimate what our dataset looks like
race_employ <- function(race){
    if (race =='white') {
      return(c(0.691*diff[1], 0.259*diff[1], 0.05*un_diff[1]))
  } else if (race == 'black') {
      return(c(0.64*diff[2], 0.24*diff[2], 0.12*un_diff[2]))
  } else if (race == 'asian') {
      return(c(0.684*diff[3], 0.256*diff[3], 0.06*un_diff[3]))
  } else if (race == 'mixed') {
      return(c(0.64*diff[4], 0.24*diff[4], 0.12*un_diff[4]))
  } else {
      return(c(0.662*diff[5], 0.248*diff[5], 0.09*un_diff[5]))
  }
}
GHQemploy <- vector()
CSemploy <- vector()
for (row in GHQrace){
  GHQemploy <- append(GHQemploy, sample(c('full time', 'part time', 'none'), 1, replace=TRUE, prob=race_employ(row)))
}
GHQdf_race <- data.frame(GHQrace, GHQemploy)
colnames(GHQdf_race) <- c('race', 'employ')

for (row in CSrace){
  CSemploy <- append(CSemploy, sample(c('full time', 'part time', 'none'), 1, replace=TRUE, prob=race_employ(row)))
}
CSdf_race <- data.frame(CSrace, CSemploy)
colnames(CSdf_race) <- c('race', 'employ')
# Next we'll do the same thing with education, using race as the correlating variable
# No adjustment needed between actual and study data since study data closesly matches actual data in terms of average educational levels
# data from http://www.ethnicity.ac.uk/medialibrary/briefingsupdated/how-are-ethnic-inequalities-in-education-changing.pdf

race_edu <- function(race){
  if (race =='white') {
    return(c(0.435, 0.475, 0.095))
  } else if (race == 'black') {
    return(c(0.40, 0.48, 0.11))
  } else if (race == 'asian') {
    return(c(0.44, 0.42, 0.15))
  } else if (race == 'mixed') {
    return(c(0.38, 0.48, 0.14))
  } else {
    return(c(0.42, 0.48, 0.1))
  }
}
GHQedu <- vector()
CSedu <- vector()

for (row in GHQrace){
  GHQedu <- append(GHQedu, sample(c('degree', 'other', 'none'), 1, replace=TRUE, prob=race_edu(row)))
}
GHQdf_race$education <- as.factor(GHQedu)

for (row in CSrace){
  CSedu <- append(CSedu, sample(c('degree', 'other', 'none'), 1, replace=TRUE, prob=race_edu(row)))
}
CSdf_race$education <- as.factor(CSedu)
# Since BMI is correlated with multiple variables, I plan to first find relationships between BMI and each variable, and then create what is essentially my 
## own LR model using the equations from individual correlations.

bmi_func <- function(sex) {
  if (sex=='male') {
    return(c(39.3, 39.9, 19.9))
  }
  else {
    return(c(45.9, 25.7, 25.5))
  }
}

bmi_value_func <- function(category) {
  if (category == 'normal') {
    return(24.99 - (rnorm(1, 1, 3)**2)**(1/2) + rnorm(1)) #using rnorm at end to help introduce some more variance
  }  
  else if (category == 'over') {
    return(25 + (rnorm(1, 0, 3)**2)**(1/2) + rnorm(1))
  }
  else {
    return(29 + (rnorm(1, 0, 4)**2)**(1/2) + rnorm(1)) 
  }
}

GHQdummy_bmi <- vector()
GHQbmi_value <- vector() #for assigning a value from rnorm distribution based on BMI category
for (row in GHQdf$sex) {
  d <- (sample(c('normal', 'over', 'obese'), 1, replace=TRUE, prob=bmi_func(row)))
  GHQdummy_bmi <- append(GHQdummy_bmi, d)
  GHQbmi_value <- append(GHQbmi_value, bmi_value_func(d))
}

GHQbmi_sex <- GHQbmi_value
GHQbmi_age <- 0.7* (-0.003 * (GHQage - 55)^2) + 28.5 + 0.3* (rnorm(GHQn, sd=5.019))
GHQdf$bmi <- (GHQbmi_age + GHQbmi_sex)/2 + rnorm(GHQn, sd=3) #using rnorm one last time to ensure variance in final BMI calculation
GHQcombined <- data.frame(GHQdf, GHQdf_race)


CSdummy_bmi <- vector()
CSbmi_value <- vector() #for assigning a value from rnorm distribution based on BMI category
for (row in CSdf$sex) {
  d <- (sample(c('normal', 'over', 'obese'), 1, replace=TRUE, prob=bmi_func(row)))
  CSdummy_bmi <- append(CSdummy_bmi, d)
  CSbmi_value <- append(CSbmi_value, bmi_value_func(d))
}

CSbmi_sex <- CSbmi_value
CSbmi_age <- 0.7* (-0.003 * (CSage - 55)^2) + 28.5 + 0.3* (rnorm(CSn, sd=5.024))
CSdf$bmi <- (CSbmi_age + CSbmi_sex)/2 + rnorm(CSn, sd=3) #using rnorm one last time to ensure variance in final BMI calculation
CScombined <- data.frame(CSdf, CSdf_race)
# Create predictor variable factors (wave 1 data)

simulate_wave1_stats <- function(score_mean, std, N) {
  quantity <- as.factor(sample(c('<6', '6-8', '8+'), N, replace=TRUE, prob=c(0.11698, 0.77013, 0.11288)))
  quality <- as.factor(sample(c('very bad', 'fairly bad', 'fairly good', 'very good'), N, replace=TRUE, prob=c(0.03774, 0.18192, 0.53752, 0.24281)))
  meds <- as.factor(sample(c('none past month', '<1 per week', '1-2 per week', '3+ per week'), N, replace=TRUE, prob=c(0.84122, 0.04499, 0.02605, 0.08774)))
  
  #Scores are dependent on sleep variables, so this code creates numeric representations of each factor to help create wave 1 scores
  quantity_score <- ifelse(quantity == '<6', 0, ifelse(quantity == '6-8', 0.5, 1))
  quality_score <- ifelse(quality == 'very bad', 0, ifelse(quality == 'fairly bad', 0.33, ifelse(quality == 'fairly good', 0.67, 1)))
  meds_score <- ifelse(meds == 'none past month', 0, ifelse(quality == '<1 per week', 0.33, ifelse(quality == '1-2 per week', 0.67, 1)))
  
  score <- quantity_score+quality_score+meds_score
  score_mean_diff <- score_mean - mean(score)
  
  score <- score + rtruncnorm(N, a=0, b = Inf, score_mean_diff, std)
  wave1 <- data.frame(quantity, quality, meds, score)
  return(wave1)
}

GHQwave1 <- simulate_wave1_stats(GHQmean, GHQstd, GHQn)
MCSwave1 <- simulate_wave1_stats(MCSmean, MCSstd, CSn)
PCSwave1 <- simulate_wave1_stats(PCSmean, PCSstd, CSn)
CSwave1 <- data.frame(MCSwave1, PCSwave1$score)
colnames(CSwave1) <- c('quantity', 'quality', 'meds', 'MCS_score_1', 'PCS_score_1')


# Create CHANGE predictor variable randomly based on reported probabilities in paper (see last paragraph after first table)
# Since certain categories cannot increase or decrease (e.g. someone sleeping 8+ hours cannot increase using the study's guidelines),
## I had to set some rules to ensure increases and decreases were assigned correctly
delta_quantity <- function(quant){
  if (quant =='6-8') {
    return(c(0.189, 0.717, 0.094))
  } else if (quant == '<6') {
    return(c(0, 0.717, 0.283))
  } else {
    return(c(0.283, 0.717, 0))
  }
}

delta_quality <- function(qual){
  if (qual =='very bad') {
    return(c(0, 0.543, 0.437))
  } else if (qual == 'fairly bad') {
    return(c(0.212, 0.543, 0.245))
  } else if (qual =='fairly good') {
    return(c(0.212, 0.543, 0.245))
  } else {
    return(c(0.437,0.543,0))
  }
}

delta_meds <- function(med){
  if (med =='none past month') {
    return(c(0, 816, 0.184))
  } else if (med == '<1 per week') {
    return(c(0.134, 0.816, 0.05))
  } else if (med =='1-2 per week') {
    return(c(0.134, 0.816, 0.05))
  } else {
    return(c(0.184, 0.816, 0))
  }
}

GHQd_quant <- vector()
GHQd_qual <- vector()
GHQd_meds <- vector()

CSd_quant <- vector()
CSd_qual <- vector()
CSd_meds <- vector()

for(i in 1:GHQn) {
  GHQd_quant <- append(GHQd_quant, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quantity(GHQwave1[i,1])))
  GHQd_qual <- append(GHQd_qual, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quality(GHQwave1[i,2])))
  GHQd_meds <- append(GHQd_meds, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_meds(GHQwave1[i,3])))
}

GHQwave1$delta_quantity <- as.factor(GHQd_quant)
GHQwave1$delta_quality <- as.factor(GHQd_qual)
GHQwave1$delta_meds <- as.factor(GHQd_meds)
GHQcombined2 <- data.frame(GHQcombined, GHQwave1)

for(i in 1:CSn) {
  CSd_quant <- append(CSd_quant, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quantity(CSwave1[i,1])))
  CSd_qual <- append(CSd_qual, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quality(CSwave1[i,2])))
  CSd_meds <- append(CSd_meds, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_meds(CSwave1[i,3])))
}

CSwave1$delta_quantity <- as.factor(CSd_quant)
CSwave1$delta_quality <- as.factor(CSd_qual)
CSwave1$delta_meds <- as.factor(CSd_meds)
CScombined2 <- data.frame(CScombined, CSwave1)


# ALL OF THE ABOVE WAS JUST TO CREATE THE PREDICTOR AND CONFOUNDING DATA. NOW I WILL USE THE COEFFICIENTS FROM THE STUDY TO CREATE OUTCOME SCORES
## USED SLEEP QUALITY COEFFICIENTS

#Chnage factor level reference to match study 
GHQcombined2 <- within(GHQcombined2, race <- relevel(race, ref='white')) # borrowed from http://stackoverflow.com/questions/3872070/how-to-force-r-to-use-a-specified-factor-level-as-reference-in-a-regression
GHQcombined2 <- within(GHQcombined2, delta_quantity <- relevel(delta_quantity, ref='no change'))
GHQcombined2 <- within(GHQcombined2, delta_quality <- relevel(delta_quality, ref='no change'))
GHQcombined2 <- within(GHQcombined2, delta_meds <- relevel(delta_meds, ref='no change'))

GHQdummy_matrix <- model.matrix(~score+sex+age+race+bmi+education+employ+delta_quantity+delta_quality+delta_meds, GHQcombined2)

CScombined2 <- within(CScombined2, race <- relevel(race, ref='white')) # borrowed from http://stackoverflow.com/questions/3872070/how-to-force-r-to-use-a-specified-factor-level-as-reference-in-a-regression
CScombined2 <- within(CScombined2, delta_quantity <- relevel(delta_quantity, ref='no change'))
CScombined2 <- within(CScombined2, delta_quality <- relevel(delta_quality, ref='no change'))
CScombined2 <- within(CScombined2, delta_meds <- relevel(delta_meds, ref='no change'))

CSdummy_matrix <- model.matrix(~MCS_score_1+PCS_score_1+sex+age+race+bmi+education+employ+delta_quantity+delta_quality+delta_meds, CScombined2)

GHQwave4_score <- vector()
for (i in 1:GHQn) {
  row <- GHQdummy_matrix[i,]
  GHQwave4_score <- append(GHQwave4_score, row[2]*0.394 -0.626*row[3] -0.019*row[4] + 0.791*row[5] -0.533*row[6] -1.044*row[7] +0.56*row[8] + 
                           0.03*row[9] +0.085*row[10] +0.11*row[11] +0.433*row[12] -0.185*row[13] +1.913*row[14] -1.031*row[15] +2.348*row[16] - 
                             2.031*row[17] + 2.595*row[18] - 1.929*row[19]) 
}

MCSwave4_score <- vector()
for (i in 1:CSn) {
  row <- CSdummy_matrix[i,]
  MCSwave4_score <- append(MCSwave4_score, -0.640*row[2] +0.954*row[4] +0.087*row[5] - 1.425*row[6] +0.656*row[7] +0.592*row[8] -0.851*row[9] + 
                          -0.014*row[10] +0.140*row[11] -0.502*row[12] +0.551*row[13] -0.772*row[14] -2.628*row[15] +1.531*row[16] -3.514*row[17] - 
                          +3.027*row[18] -4.567*row[19] +3.106*row[20]) 
}

PCSwave4_score <- vector()
for (i in 1:CSn) {
  row <- CSdummy_matrix[i,]
  PCSwave4_score <- append(PCSwave4_score, -0.424*row[3] -0.035*row[4] -0.104*row[5] -1.963*row[6] -1.243*row[7] -1.272*row[8] -0.822*row[9] + 
                             -0.117*row[10] -0.932*row[11] -1.792*row[12] -0.477*row[13] -2.027*row[14] -1.526*row[15] -0.071*row[16] -1.867*row[17] - 
                             +0.924*row[18] -3.024*row[19] +2.633*row[20]) 
}

# Need to add the bias to the score. Since the paper did not give an intercept, I will use the difference between the wave 1 and 4 means as the intercept
## Note that I'm also introducing some variance to maintain the wave 4 range, stdev, and mean based on the original wave 1 scores
GHQcombined2$outcome_score <- (GHQwave4_score + (GHQmean - mean(GHQwave4_score))) * rtruncnorm(GHQn, mean=1, a=0.01, b=1.99, 0.4)
CScombined2$MCSoutcome_score <- (MCSwave4_score + (MCSmean - mean(MCSwave4_score)))* rtruncnorm(CSn, mean=1, a=0.01, b=1.99, 0.1)
CScombined2$PCSoutcome_score <- (PCSwave4_score + (PCSmean - mean(PCSwave4_score)))* rtruncnorm(CSn, mean=1, a=0.01, b=1.99, 0.1)

write.csv(GHQcombined2,file="GHQ_sim_data.csv")
write.csv(CScombined2, file="CS_sim_data.csv")

# TIME FOR SOME EXPLORATORY PLOTS AND ANALYTICS

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = GHQcombined2, aes(x=age, y=bmi)) + geom_point(aes(col=outcome_score)) + geom_smooth(method='lm')+ggtitle('BMI vs. Age, Colored by Score') + theme(text = element_text(size=20))
plot2 <- ggplot(data = GHQcombined2, aes(x=age, y=bmi)) + geom_point(aes(col=sex)) + ggtitle('BMI vs. Age, Colored by Sex') + theme(text = element_text(size=20))
plot3 <- ggplot(data = GHQcombined2, aes(x=age, y=bmi)) + geom_point(aes(col=education)) + ggtitle('BMI vs. Age, Colored by Education') + theme(text = element_text(size=20))
plot4 <- ggplot(data = GHQcombined2, aes(x=age, y=bmi)) + geom_point(aes(col=quality)) + ggtitle('BMI vs. Age, Colored by Sleep Quality') + theme(text = element_text(size=20))
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

p11 <- ggplot(data = GHQcombined2, aes(x=outcome_score-score)) + geom_histogram(aes(fill=delta_quantity)) + ggtitle('GHQ Change in Score Based on Change in Sleep Quantity') + theme(text = element_text(size=8))
p12 <- ggplot(data = CScombined2, aes(x=MCSoutcome_score-MCS_score_1)) + geom_histogram(aes(fill=delta_quantity)) + ggtitle('MCS Change in Score Based on Change in Sleep Quantity') + theme(text = element_text(size=8))
p13 <- ggplot(data = CScombined2, aes(x=PCSoutcome_score-PCS_score_1)) + geom_histogram(aes(fill=delta_quantity)) + ggtitle('PCS Change in Score Based on Change in Sleep Quantity') + theme(text = element_text(size=8))
p21 <- ggplot(data = GHQcombined2, aes(x=outcome_score-score)) + geom_histogram(aes(fill=delta_quality)) + ggtitle('GHQ Change in Score Based on Change in Sleep Quality') + theme(text = element_text(size=8))
p22 <- ggplot(data = CScombined2, aes(x=MCSoutcome_score-MCS_score_1)) + geom_histogram(aes(fill=delta_quality)) + ggtitle('MCS Change in Score Based on Change in Sleep Quality') + theme(text = element_text(size=8))
p23 <- ggplot(data = CScombined2, aes(x=PCSoutcome_score-PCS_score_1)) + geom_histogram(aes(fill=delta_quality)) + ggtitle('PCS Change in Score Based on Change in Sleep Quality') + theme(text = element_text(size=8))
p31 <- ggplot(data = GHQcombined2, aes(x=outcome_score-score)) + geom_histogram(aes(fill=delta_meds)) + ggtitle('GHQ Change in Score Based on Change in Meds') + theme(text = element_text(size=8))
p32 <- ggplot(data = CScombined2, aes(x=MCSoutcome_score-MCS_score_1)) + geom_histogram(aes(fill=delta_meds)) + ggtitle('MCS Change in Score Based on Change in Meds') + theme(text = element_text(size=8))
p33 <- ggplot(data = CScombined2, aes(x=PCSoutcome_score-PCS_score_1)) + geom_histogram(aes(fill=delta_meds)) + ggtitle('PCS Change in Score Based on Change in Meds') + theme(text = element_text(size=8))
grid.arrange(p11,p12,p13,p21,p22,p23,p31,p32,p33, nrow=3, ncol=3)


# WHAT HAPPENDS WHEN I CREATE A REGRESSION MODEL WITHOUT THE ORIGINAL SCORE AS A PREDICTOR?
# all the fits that begin with ws (with score) have the wave 1 score as a predictor
GHQ_quantity_fit <- lm(outcome_score ~ sex+age+race+education+employ+delta_quantity, data=GHQcombined2)
GHQ_quality_fit <- lm(outcome_score ~ sex+age+race+education+employ+delta_quality, data=GHQcombined2)
GHQ_meds_fit <- lm(outcome_score ~ sex+age+race+education+employ+delta_meds, data=GHQcombined2)
GHQ_overall_fit <- lm(outcome_score ~ sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=GHQcombined2)

wsGHQ_quantity_fit <- lm(outcome_score ~ score+sex+age+race+education+employ+delta_quantity, data=GHQcombined2)
wsGHQ_quality_fit <- lm(outcome_score ~ score+sex+age+race+education+employ+delta_quality, data=GHQcombined2)
wsGHQ_meds_fit <- lm(outcome_score ~ score+sex+age+race+education+employ+delta_meds, data=GHQcombined2)
wsGHQ_overall_fit <- lm(outcome_score ~ score+sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=GHQcombined2)

MCS_quantity_fit <- lm(MCSoutcome_score ~ sex+age+race+education+employ+delta_quantity, data=CScombined2)
MCS_quality_fit <- lm(MCSoutcome_score ~ sex+age+race+education+employ+delta_quality, data=CScombined2)
MCS_meds_fit <- lm(MCSoutcome_score ~ sex+age+race+education+employ+delta_meds, data=CScombined2)
MCS_overall_fit <- lm(MCSoutcome_score ~ sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=CScombined2)

wsMCS_quantity_fit <- lm(MCSoutcome_score ~ MCS_score_1+sex+age+race+education+employ+delta_quantity, data=CScombined2)
wsMCS_quality_fit <- lm(MCSoutcome_score ~ MCS_score_1+sex+age+race+education+employ+delta_quality, data=CScombined2)
wsMCS_meds_fit <- lm(MCSoutcome_score ~ MCS_score_1+sex+age+race+education+employ+delta_meds, data=CScombined2)
wsMCS_overall_fit <- lm(MCSoutcome_score ~ MCS_score_1+sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=CScombined2)

PCS_quantity_fit <- lm(PCSoutcome_score ~ sex+age+race+education+employ+delta_quantity, data=CScombined2)
PCS_quality_fit <- lm(PCSoutcome_score ~ sex+age+race+education+employ+delta_quality, data=CScombined2)
PCS_meds_fit <- lm(PCSoutcome_score ~ sex+age+race+education+employ+delta_meds, data=CScombined2)
PCS_overall_fit <- lm(PCSoutcome_score ~ sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=CScombined2)

wsPCS_quantity_fit <- lm(PCSoutcome_score ~ PCS_score_1+sex+age+race+education+employ+delta_quantity, data=CScombined2)
wsPCS_quality_fit <- lm(PCSoutcome_score ~ PCS_score_1+sex+age+race+education+employ+delta_quality, data=CScombined2)
wsPCS_meds_fit <- lm(PCSoutcome_score ~ PCS_score_1+sex+age+race+education+employ+delta_meds, data=CScombined2)
wsPCS_overall_fit <- lm(PCSoutcome_score ~ PCS_score_1+sex+age+race+education+employ+delta_quantity+delta_quality+delta_meds, data=CScombined2)


quantity <- c(summary(GHQ_quantity_fit)$r.squared, summary(wsGHQ_quantity_fit)$r.squared, summary(MCS_quantity_fit)$r.squared, summary(wsMCS_quantity_fit)$r.squared, summary(PCS_quantity_fit)$r.squared, summary(wsPCS_quantity_fit)$r.squared)
quality <- c(summary(GHQ_quality_fit)$r.squared, summary(wsGHQ_quality_fit)$r.squared, summary(MCS_quality_fit)$r.squared, summary(wsMCS_quality_fit)$r.squared, summary(PCS_quality_fit)$r.squared, summary(wsPCS_quality_fit)$r.squared)
meds <- c(summary(GHQ_meds_fit)$r.squared, summary(wsGHQ_meds_fit)$r.squared, summary(MCS_meds_fit)$r.squared, summary(wsMCS_meds_fit)$r.squared, summary(PCS_meds_fit)$r.squared, summary(wsPCS_meds_fit)$r.squared)
overall <- c(summary(GHQ_overall_fit)$r.squared, summary(wsGHQ_overall_fit)$r.squared, summary(MCS_overall_fit)$r.squared, summary(wsMCS_overall_fit)$r.squared, summary(PCS_overall_fit)$r.squared, summary(wsPCS_overall_fit)$r.squared)

score_influence <- data.frame(quantity, quality, meds, overall, row.names=c("GHQ no score", "GHQ with score", "MCS no score", "MCS with score", "PCS no score", "PCS with score"))
par(mfrow=c(2,2), oma=c(0,0,2,0))
barplot(quantity, names.arg = c("GHQ no score", "GHQ with score", "MCS no score", "MCS with score", "PCS no score", "PCS with score"), col=c('blue', 'red', 'blue','red','blue','red'), main="QUANTITY")
barplot(quality, names.arg = c("GHQ no score", "GHQ with score", "MCS no score", "MCS with score", "PCS no score", "PCS with score"), col=c('blue', 'red', 'blue','red','blue','red'), main="QUALITY")
barplot(meds, names.arg = c("GHQ no score", "GHQ with score", "MCS no score", "MCS with score", "PCS no score", "PCS with score"), col=c('blue', 'red', 'blue','red','blue','red'), main="MEDS")
barplot(overall, names.arg = c("GHQ no score", "GHQ with score", "MCS no score", "MCS with score", "PCS no score", "PCS with score"), col=c('blue', 'red', 'blue','red','blue','red'), main='OVERALL')
title("R Squared Values of Fits", outer=TRUE)

