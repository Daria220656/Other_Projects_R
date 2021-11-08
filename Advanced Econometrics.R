library(rsample)
library(recipes)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(mfx)
library(DescTools)
library(LogisticDx)
library(blorr)
library(ggplot2)

options(scipen = 999)
#setwd("C:\\Users\\daria\\OneDrive\\Desktop\\datasets\\data for R project")
# Data Exploration --------------------------------------------------------


# loading data
data <- read_csv('income.csv')

# dimension of the data. our dataset has 48842 rows and 15 columns.
dim(data)

colnames(data)

head(data)

# checking for missing values. 
colSums(is.na(data))

# counts for different income groups
data %>% count(income_bracket)

# minimum and maximum age
min(data$age)
max(data$age)

# plots
data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', '')) %>%
           ggplot(aes(x = age, fill = income_bracket)) + geom_histogram() + theme_bw()

data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', '')) %>%
  ggplot(aes(x = marital_status, fill = income_bracket)) + geom_bar(position = "fill") + theme_bw() + coord_flip()

data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', '')) %>%
  ggplot(aes(x = gender, fill = income_bracket)) + geom_bar(position="dodge") + theme_bw()

data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', '')) %>%
  ggplot(aes(x = race, fill = income_bracket)) + geom_bar(position="dodge") + theme_bw()+
  coord_flip()

data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', '')) %>%
  ggplot(aes(x = income_bracket, y = hours_per_week, fill = income_bracket)) + geom_boxplot()


ggplot(data, aes(education_num)) + geom_histogram() + theme_bw()

ggplot(data = data) +
  aes(x=education_num, fill=education) +
  geom_bar() + 
  theme_bw()


# counts for differnt workclass groups. We noticed "?" value in workclass variable.
data %>% count(workclass)


# fnlwgt will be excluded in the following step of data preprocessing beacause it's just a weight calculated based on demographical
# factors already included in the dataset such as gender, nationality etc. 


# Education (type and number of years)
count(data, education)

max(data$education_num)
min(data$education_num)


# counts for Martial status
count(data, marital_status)


# Data Preprocessing ------------------------------------------------------

### Cleaning the dataset ###
# Outcome variable

# Remove "." & Recode 1/0 and rename to "income"
data <- data %>% 
  mutate(income_bracket = str_replace(income_bracket, '\\.', ''),
         income_bracket = ifelse(income_bracket == '<=50K', '0', '1')) %>% 
  rename(income = income_bracket)

data["income"] = as.numeric(data$income)

# counts for income variable
count(data, income)

# excluding final weight (fnlwgt) from dataset

#data = select(data, -fnlwgt)

#data["fnlwgt"] = NA

data = data[-3]

# checking for missing values
colSums(is.na(data))

# replacing "?" with NA to remove observations 
data[data == "?"] <- NA

data = na.omit(data)

colSums(is.na(data))

### Merge underrepresented classes ###


count(data, marital_status)

data <- data %>% 
  mutate(
    marital_status = ifelse(marital_status %in% c('Married-spouse-absent', 
                                                  'Married-AF-spouse', 
                                                  'Married-civ-spouse'),
                            'Married', marital_status)
  )

count(data, occupation)

data <- data %>% 
  mutate(
    occupation = ifelse(occupation == 'Armed-Forces', 'Protective-serv', 
                        occupation)
  )


count(data, native_country) %>% arrange(-n)

data <- data %>% 
  mutate(
    native_country = ifelse(native_country %in% c('United-States'), 
                            native_country, 'other')
  )

count(data, education)

data <- data %>%
  mutate(
    education = ifelse(education %in% c('11th', '7th-8th', "5th-6th", '12th', "10th", "1st-4th", "9th", 'Preschool', "HS-grad"), 'School',  ifelse( education %in% c("Some-college", "Prof-school"), "College", ifelse( education %in% c("Assoc-voc", 'Assoc-acdm'), "Associate", education))))



count(data, workclass)


data = data %>% 
  mutate(
    workclass = ifelse(workclass %in% c("Federal-gov", "Local-gov", "State-gov"), "governmental position", ifelse(workclass %in% c("Self-emp-inc", "Self-emp-not-inc"), "Self-emp", ifelse(workclass %in% c('Without-pay', 'Never-worked'), "No-Pay", workclass)
  )))

count(data, race)
data = data %>% 
  mutate(
    race = ifelse(race %in% c("Amer-Indian-Eskimo", "Asian-Pac-Islander"), "Other", race)
  )



categorical_vars <- 
  sapply(data, is.character) %>% 
  which() %>% 
  names()

categorical_vars

for (variable in categorical_vars) {
  data[[variable]] <- as.factor(data[[variable]])
}

data = data %>% 
  mutate(
    education = factor(education, levels = c('School', 'College', 'Bachelors', 'Masters', 'Doctorate', 'Associate')))

# Analysis ----------------------------------------------------------------

# Linear Probability model
lpm = lm(income ~ ., data=data)
summary(lpm)

lpm.residuals = lpm$residuals
bptest(lpm.residuals ~., data=data)

# Breusch-Pagan test shows that there is no heteroskedasticity in our data. Its a first sing showing that there are 
#some problems with our data. Following tests will provide better understandings.

# probit model
myprobit <- glm(income ~., data = data, 
                family=binomial(link="probit"))
summary(myprobit)

# logit model
mylogit <- glm(income~., data = data, 
                family=binomial(link="logit"))
summary(mylogit)

# Based on AIC information criteria we should choose logit model.
# All the variable in our model are statistically significant at 5% significance level.

logit_non_linear = glm(income ~ age + workclass + education + education_num+ marital_status + occupation + relationship +
                         race+ gender + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
                       data, family=binomial(link="logit"))

summary(logit_non_linear)

#Comparing logit model with interaction and variable to power and simple logit model, 
#first model will be chosen based on AIC information criteria

# creating quality table
library(stargazer)
stargazer(myprobit, mylogit, logit_non_linear, lpm, type="text")

# Adds ratio
1.63 / 0.88
#Having a child will decrease the probability of having an income more than 50K almost twice as not having a family
1.12 / 0.57 
# Having a wife will increase the probability of having income more than 50K almost twice as being widowed
0.24 / 0.08
# Working at Sales position will increase the probability of having income more than 50K triple times as belonging to privet workclass
0.47 / 0.24
# Working at Prof-Specialty position will increase the probability of having income more than 50K twice as Working at Sales position

# Marginal effects

marr_effects=logitmfx( formula = income ~ age + workclass + education + education_num+ marital_status + occupation + relationship +
            race+ gender + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
          data, atmean = T)

class(marr_effects)



# Interpretation of R^2

PseudoR2(logit_non_linear, which ="Tjur")
PseudoR2(logit_non_linear, which ="McKelveyZavoina")

# adjusted R^2
blr_rsq_adj_count(logit_non_linear) # function from package blorr

#count R^2
blr_rsq_count(logit_non_linear) # function from package blorr


# Testing hypothesis

# H0: education = 0
# H1: education != 0

logit_unrestricted = glm(income ~ age + workclass + race + education + education_num+ marital_status + occupation + relationship +
                         race+ gender + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
                       data, family=binomial(link="logit"))

# removing education variable from the model
logit_restricted = glm(income ~ age + workclass + race + education_num+ marital_status + occupation + relationship +
              race+ gender + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
            data, family=binomial(link="logit"))

lrtest(logit_unrestricted, logit_restricted)


# H0: gender = 0
# H1: gender != 0

logit_unrestricted1 = glm(income ~ age + workclass + race + education + education_num+ marital_status + occupation + relationship +
                           race+ gender + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
                         data, family=binomial(link="logit"))

# removing education variable from the model
logit_restricted1 = glm(income ~ age + workclass + race + education + education_num+ marital_status + occupation + relationship +
                         race + capital_loss + capital_gain + hours_per_week + native_country + I(age^2) + age*race,
                       data, family=binomial(link="logit"))

lrtest(logit_unrestricted1, logit_restricted1) 


# Hosmer-Lemeshow test

# H0: Model has no omitted variables
# H1: Model has omitted variables

gof.results = gof(logit_non_linear)
gof.results$gof


# Link Test

source("linktest.R")
linktest_results = linktest(logit_non_linear)
summary(linktest_results)

#quality table
stargazer(logit_non_linear, type = "text",  title="Logit non-linear Results", out = "table1.txt")



