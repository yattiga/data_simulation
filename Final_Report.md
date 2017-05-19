## Introduction
I looked at a 2017 study that assessed the effects of changing sleep-related behavior on three different health and well-being tests. The paper can be found here: https://0-academic-oup-com.ignacio.usfca.edu/sleep/article/2845958/Changes

Many studies have demonstrated that people who get more sleep OR a higher quality sleep with fewer sleep medications perform better on mental and physical well-being tests, indicating a correlation at the very least. However, not much research has been done on the effects of CHANGING (i.e. delta) any of those behaviors. Specifically, this study investigates if an increase or decrease in any of the aforementioned behaviors predict how well a person will perform on assessments?

### Outcome Variables
The original dataset is from the UK Household Longitudinal Survey (UKLHS) and contains over 20,000 samples. The specific outcome variables were scores from the General Health Questionnaire (GHQ-12) and the 12-Item Short-Form Health Survey (SF-12) mental (MCS) and physical (PCS) component scores. These scores were obtained at baseline and 4 years later, which means the initial scores can also serve as predictor variables. So just to summarize, the acronymized outcome variables are the GHQ score, MCS, and PCS from the wave 4 group (4 years after baseline).

### Predictor Variables
For each model, the predictor variables were (1) delta sleep quality, (2) delta sleep quantity, (3) delta sleep medication, and (4) wave 1 score. The original authors created three different models for each test, with each of the three models including either (1), (2), or (3) as a predictor. This gave a total of 9 models to replicate. 

The authors considered many confounding variables, including sex, age, BMI, ethnicity (white, asian, black, mixed, other), employment (FT, PT, not), education (university+, any other, none). 

## Simulating the Data
This was easily the bulk of the project for me since so many correlated variables existed. To simulate the data, I used the authors mean and standard deviation information, along with rnorm, to create vectors of age, sex, and race. The GHQ models used a different dataset than the MCS and PCS models, so I created two separate datasets.

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
    
Variables such as employment, education, and BMI are all correlated with at least one of the already generated vectors (age, sex, race), so those vectors were created using probabilities from noted U.K. government sources. One problem I ran into is that this dataset has ~5 times more unemployed people than reality in the U.K. (house of commons library: http://researchbriefings.parliament.uk/ResearchBriefing/Summary/SN06385), so before I could develop an employment vector, I had to create ratios that accounted for the difference. 

    actual_unemploy <- as.vector(c(0.05, 0.12, 0.06, 0.12, 0.09))
    study_unemploy <- 0.39
    un_diff <- study_unemploy/actual_unemploy

    actual_employ <- as.vector(c(0.95, 0.88, 0.94, 0.88, 0.91))
    study_employ <- 0.61
    diff <- study_employ/actual_employ
    
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

A similar process was done to create education vectors, using race as the correlating variable. No adjustment was needed between actual and study data since study data closesly matched actual data in terms of average educational levels (data from http://www.ethnicity.ac.uk/medialibrary/briefingsupdated/how-are-ethnic-inequalities-in-education-changing.pdf).

Since BMI was correlated with multiple variables, I first obesity stats as it pertains to age and sex to create the appropriate probabilities. Next I took random samples with some rnorm variance, and then averaged the two results (along with some extra added variance) to create a correlated BMI.

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
    
Wave 1 statistics (sleep quantity, sleep quality, and medication amount) were generated using the following function (note that scores were dependent on sleep variables, so this code creates numeric representations of each factor to help create wave 1 scores:    

    simulate_wave1_stats <- function(score_mean, std, N) {
      quantity <- as.factor(sample(c('<6', '6-8', '8+'), N, replace=TRUE, prob=c(0.11698, 0.77013, 0.11288)))
      quality <- as.factor(sample(c('very bad', 'fairly bad', 'fairly good', 'very good'), N, replace=TRUE, prob=c(0.03774, 0.18192, 0.53752, 0.24281)))
      meds <- as.factor(sample(c('none past month', '<1 per week', '1-2 per week', '3+ per week'), N, replace=TRUE, prob=c(0.84122, 0.04499, 0.02605, 0.08774)))

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
    
The paper stated that "twice as many participants reported a decrease (18.9%) than an increase (9.4%) in sleep quantity. Change in sleep quality was split between both directions, with 24.5% noting better sleep quality and 21.2% noting worse sleep quality over time. The number of participants reporting a reduction in sleep medication use (13.4%) was nearly three times higher than those reporting an increase (5%)." Those percentages were used to create somewhat randomized delta statistics. However, one major obstacle to creating these predictor variables was the fact that certain people could only have certain deltas by definition. For example, someone who slept less than 6 hours in wave 1 cannot decrease in sleep quantity, since less than 6 hours was the lowest factor. Conversely, someone who slept more than 8 hours could not have "increase" as their delta, since >8 hours was the maximum. Keep this in mind when reviewing my analysis later. 

Lastly, I created the outcome variable vectors. An example of such code is shown below:

    GHQwave4_score <- vector()
    for (i in 1:GHQn) {
      row <- GHQdummy_matrix[i,]
      GHQwave4_score <- append(GHQwave4_score, row[2]*0.394 -0.626*row[3] -0.019*row[4] + 0.791*row[5] -0.533*row[6] -1.044*row[7] +0.56*row[8] + 
                               0.03*row[9] +0.085*row[10] +0.11*row[11] +0.433*row[12] -0.185*row[13] +1.913*row[14] -1.031*row[15] +2.348*row[16] - 
                                 2.031*row[17] + 2.595*row[18] - 1.929*row[19]) 
    }
    
The datesets I created, both as csv files, can be found in this repo.

## Exploratory Plots

![] (/explore.png)

These plots show how some correlated variables are related. With 20,000+ data points, it mostly looks like random noise, which is a good thing since that more accurately reflects real life. In reality, regression models show that features like BMI and age are indeed related, as intended.
