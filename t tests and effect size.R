# install packages if necessary
# install.packages('psych')
# install.packages('car')
# install.packages('lsr')
# install.packages('reshape')


# load packages
library(psych)
library(car)
library(lsr)
library(reshape)
library(ggplot2)

# read data into dataframe called wm
wm = read.table('workingmemory.txt', header = T)
View(wm)

# About the data:
# This is an experiment on working memory training (n = 120)
# Dependent variable (DV) is the number of items answered accurately during an intelligence test
# There are 3 independent levels:
#   Time (2 levels): Pre and Post Training
#   Training (2 levels): Training (1) and Control (0) (n_training = 80, n_control = 40)
#   Training sessions (4 levels): 8, 12, 17, 19 (n = 20 for each level)

#summary statistics of all groups
describeBy(wm, wm$cond)

# create subsets for control group and training group
wm_c = subset(wm, wm$train == '0')
wm_t = subset(wm, wm$train == '1')

# using dplyr
# wm_c <- wm %>%
#   filter (train == '0')
# 
# using dplyr
# wm_t <- wm %>%
#   filter (train == '1')

# Dependent t-tests (i.e., paired, non-independent, two-group)

# compare between pre and post scores in control group
t.test(wm_c$post, wm_c$pre, pair = T)

# compare between pre and post scores in training group
t.test(wm_t$post, wm_t$pre, pair = T)

# effect size for control group
cohensD(wm_c$post, wm_c$pre, method = 'paired')

# effect size for control group
cohensD(wm_t$post, wm_t$pre, method = 'paired')

# Cohen's d for dependent t-test
# d = mean of difference / standard deviation of difference
(3.49/2.15)

# box plot for visualization
long_wm <- melt(wm, id = c('cond', 'train', 'gain'))

ggplot(long_wm, aes(x = cond, y = value, color = variable)) +
  geom_boxplot() + 

# so it seems that the practice effect (i.e., the effect of doing the test once before
# doing it again) is significant.  What about the effect of training?

# Independent t-test
t.test(wm$gain ~ wm$train, var.equal = T)
 
cohensD(wm$gain ~ wm$train, method = 'pooled')

# Cohen's d for independent t-test
# d = mean of difference / pooled standard deviation of difference
pooled_sd = (79/118 * 2.15) + (39/118 * 1.39)
(3.4875 - 1.975)/pooled_sd

# boxplot
ggplot(wm, aes(x=cond, y=gain, fill = cond)) +
  geom_boxplot()

# ANOVA
aov.model <- aov(wm$gain ~ wm$cond)
summary(aov.model)

summary(wm)

