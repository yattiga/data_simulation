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
GHQsex <- rbinom(GHQn, 1, 0.56058) #1 is female
GHQsex <- ifelse(GHQsex==1, 'female', 'male')
GHQrace <- as.factor(sample(c('white', 'asian', 'black', 'mixed', 'other'), GHQn, replace=TRUE, prob=c(0.87944, 0.06394, 0.03025, 0.01510, 0.01126)))
df <- data.frame(GHQage, GHQsex)
colnames(df) <- c('age', 'sex')

# library(ggplot2)
# ggplot(data = df, aes(x=GHQage, y=GHQbmi)) + geom_point() + ggtitle('BMI as a function of Age') + theme(text = element_text(size=20))

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
for (row in GHQrace){
  GHQemploy <- append(GHQemploy, sample(c('full time', 'part time', 'none'), 1, replace=TRUE, prob=race_employ(row)))
}
df_race <- data.frame(GHQrace, GHQemploy)
colnames(df_race) <- c('race', 'employ')

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
for (row in GHQrace){
  GHQedu <- append(GHQedu, sample(c('degree', 'other', 'none'), 1, replace=TRUE, prob=race_edu(row)))
}
df_race$education <- as.factor(GHQedu)

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

dummy_bmi <- vector()
bmi_value <- vector() #for assigning a value from rnorm distribution based on BMI category
for (row in df$sex) {
  d <- (sample(c('normal', 'over', 'obese'), 1, replace=TRUE, prob=bmi_func(row)))
  dummy_bmi <- append(dummy_bmi, d)
  bmi_value <- append(bmi_value, bmi_value_func(d))
}

GHQbmi_sex <- bmi_value
GHQbmi_age <- 0.7* (-0.003 * (GHQage - 55)^2) + 28.5 + 0.3* (rnorm(GHQn, sd=5.019))
df$bmi <- (GHQbmi_age + GHQbmi_sex)/2 + rnorm(GHQn, sd=3) #using rnorm one last time to ensure variance in final BMI calculation
combined <- data.frame(df, df_race)


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

wave1 <- simulate_wave1_stats(GHQmean, GHQstd, GHQn)


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

d_quant <- vector()
d_qual <- vector()
d_meds <- vector()
for(i in 1:GHQn) {
  d_quant <- append(d_quant, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quantity(wave1[i,1])))
  d_qual <- append(d_qual, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_quality(wave1[i,2])))
  d_meds <- append(d_meds, sample(c('decrease', 'no change', 'increase'), 1, replace=TRUE, prob=delta_meds(wave1[i,3])))
}

wave1$delta_quantity <- as.factor(d_quant)
wave1$delta_quality <- as.factor(d_qual)
wave1$delta_meds <- as.factor(d_meds)
combined2 <- data.frame(combined, wave1)


# ALL OF THE ABOVE WAS JUST TO CREATE THE PREDICTOR AND CONFOUNDING DATA. NOW I WILL USE THE COEFFICIENTS FROM THE STUDY TO CREATE OUTCOME SCORES
## USED SLEEP QUALITY COEFFICIENTS

#Chnage factor level reference to match study 
combined2 <- within(combined2, race <- relevel(race, ref='white')) # borrowed from http://stackoverflow.com/questions/3872070/how-to-force-r-to-use-a-specified-factor-level-as-reference-in-a-regression
combined2 <- within(combined2, delta_quantity <- relevel(delta_quantity, ref='no change'))
combined2 <- within(combined2, delta_quality <- relevel(delta_quality, ref='no change'))
combined2 <- within(combined2, delta_meds <- relevel(delta_meds, ref='no change'))

dummy_matrix <- model.matrix(~score+sex+age+race+bmi+education+employ+delta_quantity+delta_quality+delta_meds, combined2)

wave4_score <- vector()
for (i in 1:GHQn) {
  row <- dummy_matrix[i,]
  wave4_score <- append(wave4_score, row[2]*0.394 -0.626*row[3] -0.019*row[4] + 0.791*row[5] -0.533*row[6] -1.044*row[7] +0.56*row[8] + 
                           0.03*row[9] +0.085*row[10] +0.11*row[11] +0.433*row[12] -0.185*row[13] +1.913*row[14] -1.031*row[15] +2.348*row[16] - 
                             2.031*row[17] + 2.595*row[18] - 1.929*row[19]) 
}

# Need to add the bias to the score. Since the paper did not give an intercept, I will use the difference between the wvae 1 and 4 means as the intercept
## Note that I'm also introducing some variance to maintain the wave 4 range, stdev, and mean based on the original wave 1 scores
combined2$outcome_score <- (wave4_score + (mean(GHQmean) - mean(wave4_score))) * rtruncnorm(GHQn, mean=1, a=0.01, b=1.99, 0.4)
fit1 <- lm(outcome_score ~ delta_quantity, data=combined2)
summary(fit1)
range(combined2$outcome_score)
range(combined2$score)
mean(combined2$outcome_score)
sd(combined2$outcome_score)
