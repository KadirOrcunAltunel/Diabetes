library(tidyverse)
library(readxl)
library(hrbrthemes)
library(viridis)
library(caret)
library(rattle)


#data reading and wrangling

diabetes = read_xlsx("/Users/kadiraltunel/Documents/R Codes/diabetes.xlsx", 
                     sheet='diabetes')

head(diabetes)
summary(diabetes)
str(diabetes)

colSums(is.na(diabetes))

diabetes.cleaned = diabetes %>%
  select(-id, -bp.2s, -bp.2d) %>%
  mutate(waist.hip.ratio = waist/hip) %>%
  mutate(glucose.level = with(diabetes, ifelse(stab.glu <= 99, "normal", 
                                               ifelse(stab.glu > 100 & stab.glu <= 125, 
                                                      "prediabetes", "diabetes")))) %>%
  mutate(bmi = weight/(height)^2 * 703) %>% 
  drop_na() 

diabetes.cleaned

str(diabetes.cleaned)

diabetes.cleaned$location = as_factor(diabetes.cleaned$location)
diabetes.cleaned$frame = as_factor(diabetes.cleaned$frame)
diabetes.cleaned$gender = as_factor(diabetes.cleaned$gender)
diabetes.cleaned$glucose.level = as_factor(diabetes.cleaned$glucose.level)

str(diabetes.cleaned)

#Linear Model

linear.model = lm(stab.glu ~ waist.hip.ratio, data = diabetes.cleaned)
summary(linear.model)

ggplot(diabetes.cleaned) +
  aes(x = waist.hip.ratio, y = stab.glu) +
  geom_point(colour = "blue") +
  xlab("Waist Hip Ratio") +
  ylab("Stabilized Glucose") +
  geom_smooth() +
  theme_minimal()

diabetes.data.adjusted = diabetes.cleaned %>%
  select(-glucose.level, -bmi)

diabetes.data.adjusted

linear.model.full = lm(stab.glu ~., data = diabetes.data.adjusted)
summary(linear.model.full)

percentage.change = (linear.model$coefficients[2] - linear.model.full$coefficients[16])/linear.model$coefficients[2]*100
percentage.change


male = diabetes.cleaned[diabetes.cleaned$gender == "male", ]
female = diabetes.cleaned[diabetes.cleaned$gender == "female", ]

t.test(
  male$ratio,
  female$ratio,
  mu = 0,
  alternative = "two.sided"
)

diabetes.cleaned %>% 
  ggplot(aes(x = ratio, y = gender, fill = gender)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  ggtitle("Gender vs. Ratio of Cholesterol and HDL ") +
  xlab("Ratio") +
  ylab("")


linear.model.gender = lm(ratio ~ gender, data = diabetes.cleaned)
summary(linear.model.gender)

anova.gender = aov(ratio ~ gender, data = diabetes.cleaned)
summary(anova.gender)

TukeyHSD(anova.gender)

full.linear.model= lm(ratio ~., data = diabetes.data.adjusted)
summary(full.linear.model)

percentage.change.gender = (linear.model.gender$coefficients[2] - full.linear.model$coefficients[8])/linear.model.gender$coefficients[2]*100
percentage.change.gender

linear.model.age = lm(ratio ~ age, data = diabetes.cleaned)
summary(linear.model.age)

ggplot(diabetes.cleaned) +
  aes(x = age, y = ratio) +
  geom_point(colour = "blue") +
  xlab("Age") +
  ylab("Ratio") +
  geom_smooth() +
  theme_minimal()

percentage.change.age = (linear.model.age$coefficients[2] - full.linear.model$coefficients[8])/linear.model.age$coefficients[2]*100
percentage.change.age

#Age itself is not a predictor in predicting ratio. As the percentage
#change shows, it is a confounding variable which means the association between ratio
#and age is confounded by the other predictors in the full model.

louisa = diabetes.cleaned[diabetes.cleaned$location == "Louisa", ]
buckingham = diabetes.cleaned[diabetes.cleaned$location == "Buckingham", ]

t.test(
  louisa$ratio,
  buckingham$ratio,
  mu = 0,
  alternative = "two.sided"
)

diabetes.cleaned %>% 
  ggplot(aes(x = ratio, y = location, fill = location)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  ggtitle("Location vs. Ratio of Cholesterol and HDL ") +
  xlab("Ratio") +
  ylab("")

linear.model.location = lm(ratio ~ location, data = diabetes.cleaned)
summary(linear.model.location)


anova.location = aov(ratio ~ location, data = diabetes.cleaned)
anova.location

TukeyHSD(anova.location)

#Location itself doesn't affect ratio

medium = diabetes.cleaned[diabetes.cleaned$frame == "medium", ]
large = diabetes.cleaned[diabetes.cleaned$frame == "large", ]
small = diabetes.cleaned[diabetes.cleaned$frame == "small", ]

t.test(
  medium$ratio,
  large$ratio,
  mu = 0,
  alternative = "two.sided"
)

t.test(
  small$ratio,
  large$ratio,
  mu = 0,
  alternative = "two.sided"
)

t.test(
  small$ratio,
  medium$ratio,
  mu = 0,
  alternative = "two.sided"
)

diabetes.cleaned %>% 
  ggplot(aes(x = ratio, y = frame, fill = frame)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  ggtitle("Frame vs. Ratio of Cholesterol and HDL ") +
  xlab("Ratio") +
  ylab("")

linear.model.frame = lm(ratio ~ frame, data = diabetes.cleaned)
summary(linear.model.frame)

anova.frame = aov(ratio ~ frame, data = diabetes.cleaned)
summary(anova.frame)

TukeyHSD(anova.frame)

percentage.change.frame.small = (linear.model.frame$coefficients[3] - full.linear.model$coefficients[12])/linear.model.age$coefficients[2]*100
percentage.change.frame.small

#Frame itself is not a predictor in predicting ratio. As the percentage
#change shows, it is a confounding variable which means the association between ratio
#and frame is confounded by the other predictors in the full model.

bp.comparision.s = diabetes %>%
  select(bp.1s, bp.2s)

bp.comparision.s

t.test(
  bp.comparision.s$bp.1s,
  bp.comparision.s$bp.2s,
  mu = 0,
  alternative = "two.sided",
  paired = TRUE
)

#diastolic
bp.comparision.d = diabetes %>%
  select(bp.1d, bp.2d)

bp.comparision.d

t.test(
  bp.comparision.d$bp.1d,
  bp.comparision.d$bp.2d,
  mu = 0,
  alternative = "two.sided",
  paired = TRUE
)

bp.comparision.d.transform = bp.comparision.d %>%
  gather(bp.1d, bp.2d, bp.1d:bp.2d) %>%
  rename(days = bp.1d, bp = bp.2d) %>%
  mutate(days = ifelse(days == "bp.1d", "Day1", "Day2"))

bp.comparision.d.transform

str(bp.comparision.d.transform)

bp.comparision.d.transform$days = as_factor(bp.comparision.d.transform$days)

str(bp.comparision.d.transform)

bp.comparision.d.transform %>% 
  ggplot(aes(x = bp, y = days, fill = days)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  ggtitle("Effect of Days on Diastolic Blood Pressure") +
  xlab("Diastolic Values") +
  ylab("")


linear.model.bp.d = lm(bp ~ days, data = bp.comparision.d.transform)
summary(linear.model.bp.d)

anova.bp.d = aov(bp ~ days, data = bp.comparision.d.transform)
summary(anova.bp.d)

TukeyHSD(anova.bp.d)


bp.comparision.s.transform = bp.comparision.s %>%
  gather(bp.1s, bp.2s, bp.1s:bp.2s) %>%
  rename(days = bp.1s, bp = bp.2s) %>%
  mutate(days = ifelse(days == "bp.1s", "Day1", "Day2"))

bp.comparision.s.transform

str(bp.comparision.s.transform)


bp.comparision.s.transform$days = as_factor(bp.comparision.s.transform$days)

str(bp.comparision.s.transform)

bp.comparision.s.transform %>% 
  ggplot(aes(x = bp, y = days, fill = days)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  theme_ipsum() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  ggtitle("Effect of Days on Systolic Blood Pressure") +
  xlab("Systolic Values") +
  ylab("")

linear.model.bp.s = lm(bp ~ days, data = bp.comparision.s.transform)
summary(linear.model.bp.s)

anova.bp.s = aov(bp ~ days, data = bp.comparision.s.transform)
summary(anova.bp.s)

TukeyHSD(anova.bp.s)

linear.model.d = lm(glyhb ~ bp.1d, data = diabetes.cleaned)
summary(linear.model.d)

#Diastolic is not significant on glyhb

ggplot(diabetes.cleaned) +
  aes(x = bp.1d, y = glyhb) +
  geom_point(colour = "blue") +
  xlab("Diastolic Blood Pressure") +
  ylab("Glycosolated Hemoglobin") +
  geom_smooth() +
  theme_minimal()


linear.model.s = lm(glyhb ~ bp.1s, data = diabetes.cleaned)
summary(linear.model.s)

ggplot(diabetes.cleaned) +
  aes(x = bp.1s, y = glyhb) +
  geom_point(colour = "blue") +
  xlab("Systolic Blood Pressure") +
  ylab("Glycosolated Hemoglobin") +
  geom_smooth() +
  theme_minimal()

diabetes.data.adjusted = diabetes.cleaned %>%
  select(-glucose.level, -bmi)

diabetes.data.adjusted

full.linear.model= lm(glyhb ~., data = diabetes.data.adjusted)
summary(full.linear.model)

percentage.change.s = (linear.model.s$coefficients[2] - full.linear.model$coefficients[13])/linear.model.s$coefficients[2]*100
percentage.change.s

##Systolic Blood Pressure itself is not a predictor in predicting glyhb. As the percentage
#change shows, it is a confounding variable which means the association between glyhb
#and s-bp is confounded by the other predictors in the full model.

linear.model.chol.d = lm(chol ~ bp.1d, data = diabetes.cleaned)
summary(linear.model.chol.d)

ggplot(diabetes.cleaned) +
  aes(x = bp.1d, y = chol) +
  geom_point(colour = "blue") +
  xlab("Diastolic Blood Pressure") +
  ylab("Cholesterol") +
  geom_smooth() +
  theme_minimal()

full.linear.model.chol= lm(chol ~., data = diabetes.data.adjusted)
summary(full.linear.model.chol)

percentage.change.chol.d = (linear.model.chol.d$coefficients[2] - full.linear.model.chol$coefficients[14])/linear.model.chol.d$coefficients[2]*100
percentage.change.chol.d

##Diastolic Blood Pressure itself is not a predictor in predicting cholesterol. As the percentage
#change shows, it is a confounding variable which means the association between cholesterol
#and d-bp is confounded by the other predictors in the full model.

linear.model.chol.s = lm(chol ~ bp.1s, data = diabetes.cleaned)
summary(linear.model.chol.s)


ggplot(diabetes.cleaned) +
  aes(x = bp.1s, y = chol) +
  geom_point(colour = "blue") +
  xlab("Systolic Blood Pressure") +
  ylab("Cholesterol") +
  geom_smooth() +
  theme_minimal()

percentage.change.chol.s = (linear.model.chol.s$coefficients[2] - full.linear.model.chol$coefficients[13])/linear.model.chol.s$coefficients[2]*100
percentage.change.chol.s

##Systolic Blood Pressure itself is not a predictor in predicting cholesterol. As the percentage
#change shows, it is a confounding variable which means the association between cholesterol
#and s-dp is confounded by the other predictors in the full model.

linear.model.hdl.d = lm(hdl ~ bp.1d, data = diabetes.cleaned)
summary(linear.model.hdl.d)

#Diastolic is not significant

ggplot(diabetes.cleaned) +
  aes(x = bp.1d, y = hdl) +
  geom_point(colour = "blue") +
  xlab("Diastolic Blood Pressure") +
  ylab("HDL") +
  geom_smooth() +
  theme_minimal()

linear.model.hdl.s = lm(hdl ~ bp.1s, data = diabetes.cleaned)
summary(linear.model.hdl.s)

#Systolic BP is not significant

ggplot(diabetes.cleaned) +
  aes(x = bp.1s, y = hdl) +
  geom_point(colour = "blue") +
  xlab("Systolic Blood Pressure") +
  ylab("HDL") +
  geom_smooth() +
  theme_minimal()

full.linear.model.hdl = lm(hdl ~., data = diabetes.data.adjusted)
summary(full.linear.model.hdl)


#Predicting glycosylated hemoglobin

train.control = trainControl(method = "cv", number = 10)

#Linear Regression

model.linear = train(glyhb ~., data = diabetes.data.adjusted, method = "lm",
                     trControl = train.control)

model.linear


model.stepwise = train(glyhb ~., data = diabetes.data.adjusted, method = "leapBackward", 
                       tuneGrid = data.frame(nvmax = 1:15),
                       trControl = train.control)

plot(model.stepwise)

model.stepwise$bestTune

#This may change on each run due to train control but the result 
#shouldn't vary alot

summary(model.stepwise$finalModel)

coef(model.stepwise$finalModel, 8)


model.best.linear = lm(glyhb ~ chol + stab.glu + ratio + location + age +
                         frame + hip + time.ppn + waist.hip.ratio,
                       data = diabetes.data.adjusted)
summary(model.best.linear)

#Ridge Regression

model.ridge = train(glyhb ~., data = diabetes.data.adjusted, method = "ridge", 
                    trControl = train.control)
model.ridge

plot(model.ridge)

#Lasso Regression

model.lasso = train(glyhb ~., data = diabetes.data.adjusted, method = "lasso", 
                    trControl = train.control)
model.lasso

plot(model.lasso)

#Elastic Net

model.enet = train(glyhb ~., data = diabetes.data.adjusted, method = "enet", 
                   trControl = train.control)
model.enet

plot(model.enet)

lambdaGrid = expand.grid(lambda = 10^seq(10, -2, length=100))

#Hyper Ridge

hyper.ridge = train(glyhb ~., data = diabetes.data.adjusted, method = "ridge", 
                    trControl = train.control,
                    preProcess = c('scale', 'center'),
                    tuneGrid = lambdaGrid)
hyper.ridge

plot(hyper.ridge)

train.control.svm = trainControl(method = 'repeatedcv', number = 10, repeats = 10)

#SVM
model.svm = train(glyhb ~., data = diabetes.data.adjusted, 
                  method = "svmLinear", trControl = train.control.svm,  
                  preProcess = c("center","scale"))
model.svm


#Hyper SVM

grid = expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
hyper.svm =  train(glyhb ~., data = diabetes.data.adjusted, 
                   method = "svmLinear", trControl = train.control.svm,  
                   preProcess = c("center","scale"),
                   tuneGrid =grid, tuneLength = 10)

hyper.svm

plot(hyper.svm)

hyper.svm$bestTune

result = as_tibble(hyper.svm$results[which.min(hyper.svm$results[,2]),])
result

#Lasso Regression peforms the best

#Predicting Glucose Level

diabetes.numerical = diabetes.cleaned %>%
  select(glucose.level, chol, stab.glu, hdl, ratio, glyhb, age, height, weight, bp.1s, 
         bp.1d, waist, hip, time.ppn, waist.hip.ratio)

str(diabetes.numerical)

set.seed(123)

sample.data= sample(c(TRUE, FALSE), nrow(diabetes.numerical), 
                    replace=TRUE, prob=c(0.7,0.3))

train.data  = diabetes.numerical[sample.data, ]
test.data  = diabetes.numerical[!sample.data, ]

train.control = trainControl(method = "cv", number = 10)

diabetes.tree = train(glucose.level ~., data = train.data,
                      method = "rpart",
                      trControl = train.control)

diabetes.tree

summary(diabetes.tree$finalModel)

plot(diabetes.tree$finalModel, uniform=TRUE, main="Classification Tree")

text(diabetes.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(diabetes.tree$finalModel)

diabetes.pred = predict(diabetes.tree, newdata = test.data)

table(diabetes.pred, test.data$glucose.level)

error.rate = round(mean(diabetes.pred != test.data$glucose.level), 2)
error.rate

diabetes.neural =  train(glucose.level ~., data = train.data,
                         method = "nnet", 
                         trControl = train.control,
                         trace = FALSE)

diabetes.neural

plot(diabetes.neural)

diabetes.pred.nn= predict(diabetes.neural, newdata = test.data)
table(diabetes.pred.nn, test.data$glucose.level)

error.rate.nn = round(mean(diabetes.pred.nn != test.data$glucose.level), 2)
error.rate.nn

diabetes.randomforest = train(glucose.level ~ ., data = train.data, 
                              method = "rf", trControl = train.control,
                              trace = FALSE)
diabetes.randomforest

plot(diabetes.randomforest)

diabetes.pred.rf= predict(diabetes.randomforest, newdata = test.data)
table(diabetes.pred.rf, test.data$glucose.level)

error.rate.rf = round(mean(diabetes.pred.rf != test.data$glucose.level), 2)
error.rate.rf

#Random Forest performs the best




