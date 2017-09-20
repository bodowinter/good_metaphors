## Bodo Winter
## March 19, 2017; Changed Sep 19, 2017
## Analysis of Spanish metaphors

##------------------------------------------------------------------
## Data carpentry #1, General:
##------------------------------------------------------------------

## Load in packages:

library(stringr)
library(car)
library(tidyverse)

## Load in data:

setwd('/Users/winterb/Research/metaphormagnet/analysis/data/')
span <- read_csv('spanish_eprime.csv')

## Get rid of weird column names:

span <- select(span, -`Running[Block]`,
	-`Procedure[Block]`,
	-`Procedure[Trial]`,
	-`Running[Trial]`)

## Take only those needed:

span <- select(span,
	Subject, Age,
	FirstLanguage, Occupation,
	Sex, Trial, CrossLangDiff,
	Form_Form_Chinese:Novelty_European,
	Q1.RESP, Q1.RT,
	Q2.RESP, Q2.RT,
	Q3.RESP, Q3.RT,
	Q4.RESP, Q4.RT,
	Stimuli)


##------------------------------------------------------------------
## Experiment analysis, correlation of Q1, Q3 and Q4 across stims:
##------------------------------------------------------------------

## Q2 is omitted because it is nested, only for 'yes' Q1 responses:

## Get makes sense yes/no responses per stimulus:

Q1tab <- as_tibble(with(span, table(Stimuli, Q1.RESP)))
Q1tab <- Q1tab %>% spread(Q1.RESP, n)

## Get proportions of makes sense question, Q1:

Q1tab <- mutate(Q1tab,
	YesProp = Yes / 21)

## Get average Q3 (quality ratings):

Q3tab <- span %>% group_by(Stimuli) %>%
	summarize(quality = round(mean(Q3.RESP), 2),
		qualitySD = round(sd(Q3.RESP), 2))

## Get Q4 proportions:

Q4tab <- as_tibble(with(span, table(Stimuli, Q4.RESP)))
Q4tab <- Q4tab %>% spread(Q4.RESP, n) %>%
	mutate(HumanProp = human / 21)

## Get average RTs:

RT_tab <- span %>% group_by(Stimuli) %>%
	summarize(RT1 = mean(Q1.RT),
		# RT2 = mean(Q2.RT),
		RT3 = mean(Q3.RT),
		RT4 = mean(Q4.RT)) %>%
	mutate(TotalRT = RT1 + RT3 + RT4,
		AvgRT = TotalRT / 3)

## Merge into one matrix:

allstim <- left_join(Q1tab, Q3tab) %>%
	left_join(Q4tab) %>% left_join(RT_tab)

## Extract variables for correlations:

allcorrs <- allstim %>%
	select(YesProp, quality, HumanProp:RT4)

## Get correlation table:

cor(allcorrs)
pairs(allcorrs)

## Perform pairwise correlations:

with(allcorrs, cor.test(YesProp, quality, method = 'spearman'))
with(allcorrs, cor.test(YesProp, HumanProp, method = 'spearman'))
with(allcorrs, cor.test(quality, HumanProp, method = 'spearman'))

## Plot correlations for the three questions:

pairs(allcorrs[, 1:3])

## Perform PCA:

## Standardize all values:

allcorrs_z <- apply(allcorrs, 2,
	FUN = function(x) (x - mean(x)) / sd(x))

## Get PCAs:

allcorrs_pca <- prcomp(allcorrs_z[, 1:3], center = TRUE, scale = TRUE)
	# prcomp preferred over princomp because SVD has more numerical precision

## Inspect:

summary(allcorrs_pca)

## Inspect loadings:

round(allcorrs_pca$rotation, 1)

## Extract first component:

allstim$PC1 <- allcorrs_pca$x[, 1]

## Make it into a positive thing so high ratings = high quality:

allstim$PC1 <- allstim$PC1 * -1


##------------------------------------------------------------------
## Main novelty hypothesis and other condition variables:
##------------------------------------------------------------------

## Merge form-form and novelty in there:

allstim$novelty <- span[match(allstim$Stimuli, span$Stimuli), ]$Novelty_European
allstim$human <- span[match(allstim$Stimuli, span$Stimuli), ]$HumanORComputer

## Make form-form and novelty into polynomial contrasts (by hand):

allstim <- mutate(allstim,
	novelty_c = novelty - mean(novelty),
	novelty_c2 = novelty_c ^ 2,
	human_c = ifelse(human == 'h', -0.5, 0.5))

## Construct models for novelty:

summary(xmdl.full <- lm(PC1 ~ novelty_c + novelty_c2 +
	human_c, data = allstim))
summary(xmdl.noquadraticnovelty <- lm(PC1 ~ novelty_c + 
	human_c, data = allstim))
summary(xmdl.nonovelty <- lm(PC1 ~ 
	human_c, data = allstim))

## Construct models for article and human:

summary(xmdl.nohuman <- lm(PC1 ~ novelty_c + novelty_c2, data = allstim))

## Perform F-test of linear and quadratic effect together (omnibus):

anova(xmdl.nonovelty, xmdl.full, test = 'F')

## Perform F-test of quadratic effect:

anova(xmdl.noquadraticnovelty, xmdl.full, test = 'F')

## Perform F-test of linear effect:

anova(xmdl.nonovelty, xmdl.noquadraticnovelty, test = 'F')

## Perform F-test of linear effect:

anova(xmdl.nonovelty, xmdl.noquadraticnovelty, test = 'F')

## Perform F-tests of human:

anova(xmdl.nohuman, xmdl.full, test = 'F')

## R-squared comparisons:

summary(xmdl.full)$r.squared
summary(xmdl.noquadraticnovelty)$r.squared
summary(xmdl.full)$r.squared - summary(xmdl.noquadraticnovelty)$r.squared	# 3%
summary(xmdl.nonovelty)$r.squared
summary(xmdl.noquadraticnovelty)$r.squared - summary(xmdl.nonovelty)$r.squared	# 34%
summary(xmdl.nohuman)$r.squared
summary(xmdl.full)$r.squared - summary(xmdl.nohuman)$r.squared	# 3%



##------------------------------------------------------------------
## Compute predictions for plotting:
##------------------------------------------------------------------

## Empty data frame with values to predict:

newdata <- tibble(human_c = 0,
	novelty_c = sort(unique(stims$novelty_c))) %>%
	mutate(novelty_c2 = novelty_c ^ 2)

## Fill with predictions:

newdata <- bind_cols(newdata,
	as_tibble(predict(xmdl.full, newdata, se.fit = T)[1:2]))

## Compute 95% confidence intervals:

newdata <- mutate(newdata,
	UB = fit + 1.96 * se.fit,
	LB = fit - 1.96 * se.fit)

## Write to file:

write_csv(newdata, 'spanish_novelty_predictions.csv')


