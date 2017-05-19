# H2 Introduction
I looked at a 2017 study that assessed the effects of changing sleep-related behavior on three different health and well-being tests. The paper can be found here: https://0-academic-oup-com.ignacio.usfca.edu/sleep/article/2845958/Changes

Many studies have demonstrated that people who get more sleep OR a higher quality sleep with fewer sleep medications perform better on mental and physical well-being tests, indicating a correlation at the very least. However, not much research has been done on the effects of CHANGING (i.e. delta) any of those behaviors. Specifically, this study investigates if an increase or decrease in any of the aforementioned behaviors predict how well a person will perform on assessments?

# H3 Outcome Variables
The original dataset is from the UK Household Longitudinal Survey (UKLHS) and contains over 20,000 samples. The specific outcome variables were scores from the General Health Questionnaire (GHQ-12) and the 12-Item Short-Form Health Survey (SF-12) mental (MCS) and physical (PCS) component scores. These scores were obtained at baseline and 4 years later, which means the initial scores can also serve as predictor variables. So just to summarize, the acronymized outcome variables are the GHQ score, MCS, and PCS from the wave 4 group (4 years after baseline).

# H3 Predictor Variables
For each model, the predictor variables were (1) delta sleep quality, (2) delta sleep quantity, (3) delta sleep medication, and (4) wave 1 score. The original authors created three different models for each test, with each of the three models including either (1), (2), or (3) as a predictor. This gave a total of 9 models to replicate. 

The authors considered many confounding variables, including sex, age, BMI, ethnicity (white, asian, black, mixed, other), employment (FT, PT, not), education (university+, any other, none). 
