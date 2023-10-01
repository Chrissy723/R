# Data Cleaning
# Handling Missing Values
library ("sas7bdat") 

setwd ("/Users/EP850 Applications of Intermediate Epidemiology/Class 3") 

dat <- read.sas7bdat("session3class.sas7bdat", debug=FALSE) # N=5,601
session3class <- dat

str(session3class)
nrow(session3class) 
ncol(session3class) 
summary(session3class)

# Categorical Variables 
table(session3class$RSCRRACE)
table(session3class$RSCRRACE, useNA = 'ifany')

# Recode missing values in a new data - recode (replace), get single column($)
session3class$RSCRRACE[session3class$RSCRRACE == 7] <- NA
table(session3class$RSCRRACE, useNA = 'ifany')

library(dplyr)
library(janitor)

# Sorting data
session3class <- session3class[order(session3class$EVERPREG), ]

# Computing frequency tables
table(session3class$NPREGS_S, session3class$EVERPREG)

# Modifying data - forward pipe operator(%%)
session3class$NPREGS_S <- ifelse(session3class$EVERPREG == 5 & session3class$NPREGS_S %in% c(".", 98, 99), 0, session3class$NPREGS_S)

# Re-sorting data
session3class <- session3class[order(session3class$EVERPREG), ]

# Computing frequency tables
table(session3class$NPREGS_S, session3class$EVERPREG)

# QC work
session3class <- dat %>%
  mutate(NPREGS_S_clean = ifelse(EVERPREG == 5 & NPREGS_S %in% c(NaN, 98, 99), 0, NPREGS_S))

# Filtering, selecting certain columns and viewing the first 10 rows
session3class %>% 
  filter(EVERPREG == 5 & NPREGS_S %in% c(NaN, 98, 99)) %>%
  select(EVERPREG, NPREGS_S, NPREGS_S_clean) %>%
  slice_head(n = 10)

# Cross-tab of variable pre and post cleaning
# subset the data where everpreg = 5
subset_session3class <- session3class[session3class$EVERPREG == 5, ]

# Perform frequency analysis
freq_table <- table(subset_session3class$NPREGS_S, subset_session3class$NPREGS_S_clean, useNA = 'ifany')

# Print frequency table
print(freq_table)

# Examining Outliers

# Computing summary statistics
summary(session3class$BMI)

session3class %>%
  filter(BMI == 95) %>%
  select(BMI, RWEIGHT, INCHES, AGE_R) %>%
  slice_head(n = 20)

# Re-code setting a BMI of 95 to missing
session3class <- session3class %>%
  mutate(BMI = ifelse(BMI == 95, NaN, BMI))

# Computing summary statistics again
summary(session3class$BMI)

ggplot(session3class, aes(x = BMI)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE) +
  ggtitle("BMI, after setting 95 to NA")

# Examining Missing Data

# Sorting data
session3class <- dat
session3class <- session3class[order(session3class$CASEID), ]

session3class %>%
  filter(is.na(BMI)) %>%
  select(BMI, RWEIGHT, INCHES, AGE_R, CURRPREG) %>%
  slice_head(n = 10)
subset_session3class <- session3class[session3class$AGE_R >= 20, ]

summary(subset_session3class$BMI)

# Creating categories

# BMI Categories
session3class <- session3class %>%
  mutate(
    BMI = ifelse(BMI == 95, NaN, BMI),
    bmicat = case_when(
      BMI >= 16 & BMI < 19 ~ 1,
      BMI >= 19 & BMI < 25 ~ 2,
      BMI >= 25 & BMI < 30 ~ 3,
      BMI >= 30 ~ 4,
      TRUE ~ NA_integer_
    )
  )

# Frequency table
table(session3class$bmicat)

grouped_data <- split(session3class$BMI, session3class$bmicat)
summary_stats <- lapply(grouped_data, function(x) {
  Mean <- mean(x, na.rm = TRUE)
  StdDev <- sd(x, na.rm = TRUE)
  Min <- min(x, na.rm = TRUE)
  Max <- max(x, na.rm = TRUE)
  N <- sum(!is.na(x))
  
  return(data.frame(Mean = Mean, StdDev = StdDev, Min = Min, Max = Max, N = N))
})

print(summary_stats)

# Reducing number of categories
# Gravidity Category
session3class <- session3class %>%
  mutate(
    gravid = case_when(
      NPREGS_S == 0 ~ 0,
      NPREGS_S == 1 ~ 1,
      NPREGS_S == 2 ~ 2,
      NPREGS_S == 3 ~ 3,
      NPREGS_S == 4 ~ 4,
      NPREGS_S >= 5 ~ 5,
      is.na(NPREGS_S) ~ NA_integer_
    )
  )

# Contingency table
table(session3class$NPREGS_S, session3class$gravid, useNA = "ifany")

# BMI Categories;
session3class <- session3class %>%
  mutate(
    BMI = ifelse(BMI == 95, NA, BMI),
    bmicat = case_when(
      BMI >= 16 & BMI < 19 ~ 1,
      BMI >= 19 & BMI < 25 ~ 2,
      BMI >= 25 & BMI < 30 ~ 3,
      BMI >= 30 ~ 4,
      TRUE ~ NA_integer_
    )
  )

# Dummy variables
session3class <- session3class %>%
  mutate(
    bmicat1 = ifelse(bmicat == 1, 1, ifelse(!is.na(bmicat), 0, NA)),
    bmicat2 = ifelse(bmicat == 2, 1, ifelse(!is.na(bmicat), 0, NA)),
    bmicat3 = ifelse(bmicat == 3, 1, ifelse(!is.na(bmicat), 0, NA)),
    bmicat4 = ifelse(bmicat == 4, 1, ifelse(!is.na(bmicat), 0, NA))
  )

# Create a contingency table
table(session3class$bmicat, session3class$bmicat1, session3class$bmicat2, session3class$bmicat3, session3class$bmicat4, useNA = "ifany")

# Calculate minimum and maximum of 'bmi' grouped by 'bmicat', 'bmicat1', 'bmicat2', 'bmicat3', 'bmicat4'
session3class %>%
  group_by(bmicat, bmicat1, bmicat2, bmicat3, bmicat4) %>%
  summarise(min_bmi = min(BMI, na.rm = F),
            max_bmi = max(BMI, na.rm = F),
            .groups = "drop") %>%
  mutate(across(c(min_bmi, max_bmi), round, 1))

# Reference is no pregnancies
session3class$gravid1 <- ifelse(session3class$gravid == 1, 1, 0)
session3class$gravid2 <- ifelse(session3class$gravid == 2, 1, 0)
session3class$gravid3 <- ifelse(session3class$gravid == 3, 1, 0)
session3class$gravid4 <- ifelse(session3class$gravid == 4, 1, 0)
session3class$gravid5 <- ifelse(session3class$gravid == 5, 1, 0)

# Create a contingency table
table(session3class$gravid, session3class$gravid1, session3class$gravid2, session3class$gravid3, session3class$gravid4, session3class$gravid5)

# Final analytic dataset
# Drop, Keep, Rename
session3class <- session3class %>%
  mutate(
    RSCRRACE = ifelse(RSCRRACE == 7, NaN, RSCRRACE),
    RACE = RSCRRACE,
    NPREGS_S = ifelse(EVERPREG == 5 & NPREGS_S %in% c(NaN, 98, 99), 0, NPREGS_S)
  ) %>%
  select(-RSCRRACE)

# Gravidity Category
session3class <- session3class %>%
  mutate(
    gravid = case_when(
      NPREGS_S == 0 ~ 0,
      NPREGS_S == 1 ~ 1,
      NPREGS_S == 2 ~ 2,
      NPREGS_S == 3 ~ 3,
      NPREGS_S == 4 ~ 4,
      NPREGS_S >= 5 ~ 5,
      is.na(NPREGS_S) ~ NA_integer_
    )
  )

# BMI Categories
session3class <- session3class %>%
  mutate(
    BMI = ifelse(BMI == 95, NA, BMI),
    BMICAT = case_when(
      BMI >= 16 & BMI < 19 ~ 1,
      BMI >= 19 & BMI < 25 ~ 2,
      BMI >= 25 & BMI < 30 ~ 3,
      BMI >= 30 ~ 4,
      TRUE ~ NA_integer_
    )
  )

table(session3class$RSCRRACE)

# Labels and formats
session3class$BMICAT <- factor(session3class$BMICAT,
                               levels = c(1, 2, 3, 4),
                               labels = c("Underweight", "Normal", "Overweight", "Obese"))

# get the frequency distribution of 'bmicat'
frequency <- table(session3class$BMICAT)
print(frequency)

# Removing Formats/Labels 
session3class$BMICAT <- unclass(session3class$BMICAT)
frequency <- table(session3class$BMICAT)
print(frequency)

# Convert numeric to character:
session3class_converted <- session3class

session3class_converted$BMICAT <- as.character(session3class_converted$BMI)

# Convert character to numeric:
# Before converting, make sure the character string is valid for conversion. If not, NA will be returned.
session3class_converted$BMI <- as.numeric(session3class_converted$BMICAT)

# Sorting the datasets
session3class <- session3class[order(session3class$CASEID),]
newdataset <- newdataset[order(newdataset$CASEID),]

# Converting categorical variables to factors
data$sex <- as.factor(data$sex)
data$smoking_status <- as.factor(data$smoking_status)

# Exploratory Data Analysis (EDA)
# Histogram for Disease Prevalence
ggplot(data, aes(x = disease_prevalence)) +
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram for Disease Prevalence',
       x = 'Disease Prevalence',
       y = 'Frequency')

# Boxplot for Age against Disease Prevalence
ggplot(data, aes(x = age, y = disease_prevalence)) +
  geom_boxplot() +
  labs(title = 'Boxplot of Age against Disease Prevalence',
       x = 'Age',
       y = 'Disease Prevalence')

# Correlation Matrix
cor_matrix <- cor(data %>% select(-sex, -smoking_status))
print(cor_matrix)

# Data Splitting for Model
set.seed(123)
splitIndex <- createDataPartition(data$disease_prevalence, p = .7, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# Linear Regression Model
model <- lm(disease_prevalence ~ age + sex + bmi + smoking_status, data = train_data)
summary(model)

# Model Evaluation
predictions <- predict(model, test_data)
mse <- mean((predictions - test_data$disease_prevalence)^2)
print(paste('Mean Squared Error on Test Data: ', mse))

# Random Forest

## Disease Prevalence by Age and Sex
ggplot(data, aes(x = age, y = disease_prevalence, color = sex)) +
  geom_point() +
  labs(title = 'Disease Prevalence by Age and Sex',
       x = 'Age',
       y = 'Disease Prevalence') +
  facet_wrap(~smoking_status, scales = 'free')

## BMI Distribution
ggplot(data, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = 'green', color = 'black', alpha = 0.7) +
  labs(title = 'BMI Distribution',
       x = 'BMI',
       y = 'Frequency')

# Correlation Matrix
cor_matrix <- cor(data %>% select(-sex, -smoking_status))
print(cor_matrix)

# Data Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$age <- normalize(data$age)
data$bmi <- normalize(data$bmi)
data$disease_prevalence <- normalize(data$disease_prevalence)

# Data Splitting for Model
set.seed(123)
splitIndex <- createDataPartition(data$disease_prevalence, p = .7, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# Random Forest Model
model_rf <- randomForest(disease_prevalence ~ age + sex + bmi + smoking_status, 
                         data = train_data, ntree = 100, mtry = 2, importance = TRUE)
print(model_rf)

# Model Evaluation
predictions_rf <- predict(model_rf, test_data)
mse_rf <- mean((predictions_rf - test_data$disease_prevalence)^2)
print(paste('Mean Squared Error for Random Forest on Test Data: ', mse_rf))

# Feature Importance
importance(model_rf)
varImpPlot(model_rf, type = 1)

#Two-way ANOVA
library(aov4tukey) 

data <- read.csv("dataset.csv")

# Convert treatment and gender to factor
data$treatment <- as.factor(data$treatment)
data$gender <- as.factor(data$gender)

# Preliminary Data Exploration
str(data)
head(data)

# Conducting Two-way ANOVA
model <- aov(response ~ treatment * gender, data = data)

# Summary of the Two-way ANOVA
summary(model)

# Tukey Post-hoc test to assess pairwise comparisons
tukey <- TukeyHSD(model)
print(tukey)

# Plotting the results of the post-hoc test
plot(tukey)

