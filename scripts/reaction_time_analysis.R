## Bodo Winter
## Sep 25, 2017
## Analysis of English reaction times

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Load in packages:

library(stringr)
library(tidyverse)
library(lme4)
library(afex)
library(MuMIn)

## Load in data:

setwd('/Users/winterb/Research/metaphormagnet/analysis/data/')
eng <- read_csv('english_eprime.csv')
chin <- read_csv('chinese_eprime.csv')
span <- read_csv('spanish_eprime.csv')

## Get rid of weird column names:

eng <- select(eng, -`Running[Block]`,
	-`Procedure[Block]`,
	-`Procedure[Trial]`,
	-`Running[Trial]`)
span <- select(span, -`Running[Block]`,
	-`Procedure[Block]`,
	-`Procedure[Trial]`,
	-`Running[Trial]`)

## Take only those columns needed:

eng <- select(eng,
	Subject, Handedness, Age,
	FirstLanguage, Occupation,
	Sex, Trial, CrossLangDiff,
	Form_Form_Chinese:Novelty_European,
	Q1.RESP, Q1.RT,
	Q2.RESP, Q2.RT,
	Q3.RESP, Q3.RT,
	Q4.RESP, Q4.RT,
	Stimuli)
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
chin <- select(chin,
	Subject, Age,
	FirstLanguage, Occupation,
	Sex, Trial, CrossLangDiff,
	Form_Form_Chinese:Novelty_European,
	Q1RESP, Q1RT,
	Q2RESP, Q2RT,
	Q3RESP, Q3RT,
	Q4RESP, Q4RT,
	Stimuli)		# NO HANDEDNESS INFO

## Rename Chinese for consistency:

chin <- rename(chin,
	Q1.RESP = Q1RESP,
	Q1.RT = Q1RT)

## Get rid of "war is an exciting love" (ungrammatical):

eng <- filter(eng,
	Stimuli != 'War is an exciting love.')

## Log transform reaction times:

eng <- mutate(eng,
	LogRT = log10(Q1.RT))
span <- mutate(span,
	LogRT = log10(Q1.RT))
chin <- mutate(chin,
	LogRT = log10(Q1.RT))


##------------------------------------------------------------------
## Clean RTs out of 2.5 SDs the subject average:
##------------------------------------------------------------------

## Get standard deviations (for some reason doesn't work with dplyr):

eng.sd <- aggregate(LogRT ~ Subject, eng, sd)
span.sd <- aggregate(LogRT ~ Subject, span, sd)
chin.sd <- aggregate(LogRT ~ Subject, chin, sd)

## Standard deviation cut-off value:

cutoff <- 1.96

## Get subject RT averages and cut-off values:

eng.sub <- eng %>%
	group_by(Subject) %>%
	summarize(LogRT = mean(LogRT)) %>%
	left_join(eng.sd, by = c('Subject' = 'Subject')) %>%
	mutate(SubMax = LogRT.x + cutoff * LogRT.y,
		SubMin = LogRT.x - cutoff * LogRT.y)

span.sub <- span %>%
	group_by(Subject) %>%
	summarize(LogRT = mean(LogRT)) %>%
	left_join(span.sd, by = c('Subject' = 'Subject')) %>%
	mutate(SubMax = LogRT.x + cutoff * LogRT.y,
		SubMin = LogRT.x - cutoff * LogRT.y)	
	
chin.sub <- chin %>%
	group_by(Subject) %>%
	summarize(LogRT = mean(LogRT)) %>%
	left_join(chin.sd, by = c('Subject' = 'Subject')) %>%
	mutate(SubMax = LogRT.x + cutoff * LogRT.y,
		SubMin = LogRT.x - cutoff * LogRT.y)

## Function for looping through,
## takes subject data file and main data file:

clean_by_sd <- function(subject_file = eng.sub,
	data = eng) {
		this_df <- data
		newdf <- c()
		for (i in 1:nrow(subject_file)) {
			this_sub <- subject_file[i, ]
			temp_df <- filter(this_df,
				Subject == this_sub$Subject,
				LogRT < this_sub$SubMax,
				LogRT > this_sub$SubMin)
			newdf <- bind_rows(newdf,
				temp_df)
			}
		return(newdf)
	}

## Clean English:

eng_cleaned <- clean_by_sd(eng.sub, eng)
nrow(eng) - nrow(eng_cleaned)
(nrow(eng) - nrow(eng_cleaned)) / nrow(eng)

## Clean Spanish:

span_cleaned <- clean_by_sd(subject_file = span.sub,
	data = span)
nrow(span) - nrow(span_cleaned)
(nrow(span) - nrow(span_cleaned)) / nrow(span)

## Clean Chinese:

chin_cleaned <- clean_by_sd(subject_file = chin.sub,
	data = chin)
nrow(chin) - nrow(chin_cleaned)
(nrow(chin) - nrow(chin_cleaned)) / nrow(chin)



##------------------------------------------------------------------
## Does metaphor quality predict reaction times?
##------------------------------------------------------------------

## Load in PCA data files:

engPCA <- read_csv('english_allstims_summary_PCA.csv')
spanPCA <- read_csv('spanish_allstims_summary_PCA.csv')
chinPCA <- read_csv('chinese_allstims_summary_PCA.csv')

## Merge:

eng <- left_join(eng, engPCA)
span <- left_join(span, spanPCA)
chin <- left_join(chin, chinPCA)

## Perform mixed models of metaphor quality:

print(eng.PCA.afex <- mixed(LogRT ~ PC1 +
	(1 + PC1|Subject) +
	(1|Stimuli),
	data = eng, method = 'LRT', REML = FALSE))
print(chin.PCA.afex <- mixed(LogRT ~ PC1 +
	(1 + PC1|Subject) +
	(1|Stimuli),
	data = chin, method = 'LRT', REML = FALSE))
print(span.PCA.afex <- mixed(LogRT ~ PC1 +
	(1 + PC1|Subject) +
	(1|Stimuli),
	data = span, method = 'LRT', REML = FALSE))

## Perform mixed models of appreciation:

print(eng.quality.afex <- mixed(LogRT ~ quality +
	(1 + quality|Subject) +
	(1|Stimuli),
	data = eng, method = 'LRT', REML = FALSE))
print(chin.PCA.afex <- mixed(LogRT ~ quality +
	(1 + quality|Subject) +
	(1|Stimuli),
	data = chin, method = 'LRT', REML = FALSE))
print(span.quality.afex <- mixed(LogRT ~ quality +
	(1 + quality|Subject) +
	(1|Stimuli),
	data = span, method = 'LRT', REML = FALSE))



	
##------------------------------------------------------------------
## Experiment analysis, reaction times:
##------------------------------------------------------------------

## Make form-form and novelty into polynomial contrasts (by hand):

eng <- mutate(eng,
	novelty_c = Novelty_European - mean(Novelty_European),
	novelty_c2 = novelty_c ^ 2,
	human_c = ifelse(HumanORComputer == 'c', -0.5, 0.5),
	Q1.RESP_c = ifelse(Q1.RESP == 'No', -0.5, 0.5))
chin <- mutate(chin,
	novelty_c = Novelty_Chinese - mean(Novelty_Chinese),
	novelty_c2 = novelty_c ^ 2,
	human_c = ifelse(HumanORComputer == 0, -0.5, 0.5),
	Q1.RESP_c = ifelse(Q1.RESP == 'No', -0.5, 0.5))
span <- mutate(span,
	novelty_c = Novelty_European - mean(Novelty_European),
	novelty_c2 = novelty_c ^ 2,
	human_c = ifelse(HumanORComputer == 'c', -0.5, 0.5),
	Q1.RESP_c = ifelse(Q1.RESP == 'No', -0.5, 0.5))

## Construct models for novelty:

print(eng.RT.afex <- mixed(LogRT ~ (novelty_c + novelty_c2) * Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = eng, method = 'LRT', REML = FALSE))
print(chin.RT.afex <- mixed(LogRT ~ (novelty_c + novelty_c2) * Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = chin, method = 'LRT', REML = FALSE))
print(span.RT.afex <- mixed(LogRT ~ (novelty_c + novelty_c2) * Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = span, method = 'LRT', REML = FALSE))

## Block-wise entry, decomposing the interaction novelty versus sensibility:

eng.RT.no_int <- lmer(LogRT ~ (novelty_c + novelty_c2) + Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = eng, REML = FALSE)
chin.RT.no_int <- lmer(LogRT ~ (novelty_c + novelty_c2) + Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = chin, REML = FALSE)
span.RT.no_int <- lmer(LogRT ~ (novelty_c + novelty_c2) + Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = span, REML = FALSE)

## Block-wise entry, no novelty:

eng.RT.no_novelty <- lmer(LogRT ~ Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = eng, REML = FALSE)
chin.RT.no_novelty <- lmer(LogRT ~ Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = chin, REML = FALSE)
span.RT.no_novelty <- lmer(LogRT ~ Q1.RESP_c + 
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = span, REML = FALSE)

## Block-wise entry, no Q1:

eng.RT.no_Q1 <- lmer(LogRT ~ (novelty_c + novelty_c2) +
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = eng, REML = FALSE)
chin.RT.no_Q1 <- lmer(LogRT ~ (novelty_c + novelty_c2) +
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = chin, REML = FALSE)
span.RT.no_Q1 <- lmer(LogRT ~ (novelty_c + novelty_c2) +
	human_c +
	(1 + novelty_c + novelty_c2 + Q1.RESP_c|Subject) +
	(1 + Q1.RESP_c|Stimuli),
	data = span, REML = FALSE)

## Perform likelihood ratio test comparisons, interactions:

anova(eng.RT.no_int, eng.RT.afex$full.model, test = 'Chisq')
anova(span.RT.no_int, span.RT.afex$full.model, test = 'Chisq')
anova(chin.RT.no_int, chin.RT.afex$full.model, test = 'Chisq')

## Perform likelihood ratio test comparisons, conjoined novelty effect:

anova(eng.RT.no_novelty, eng.RT.no_int, test = 'Chisq')
anova(span.RT.no_novelty, span.RT.no_int, test = 'Chisq')
anova(chin.RT.no_novelty, chin.RT.no_int, test = 'Chisq')

## Perform likelihood ratio test comparisons, Q1 effect:

anova(eng.RT.no_Q1, eng.RT.no_int, test = 'Chisq')
anova(span.RT.no_Q1, span.RT.no_int, test = 'Chisq')
anova(chin.RT.no_Q1, chin.RT.no_int, test = 'Chisq')

## Check R-Squared of full model:

r.squaredGLMM(eng.RT.afex$full.model)
r.squaredGLMM(span.RT.afex$full.model)
r.squaredGLMM(chin.RT.afex$full.model)

## Check descriptive averages:

aggregate(Q1.RT ~ Novelty_European * Q1.RESP, eng, mean) %>%
	mutate(round(Q1.RT, -1))
aggregate(Q1.RT ~ Novelty_European * Q1.RESP, span, mean) %>%
	mutate(round(Q1.RT, -1))
aggregate(Q1.RT ~ Novelty_Chinese * Q1.RESP, chin, mean) %>%
	mutate(round(Q1.RT, -1))

aggregate(Q1.RT ~ HumanORComputer, eng, mean) %>%
	mutate(round(Q1.RT, -1))
aggregate(Q1.RT ~ HumanORComputer, span, mean) %>%
	mutate(round(Q1.RT, -1))
aggregate(Q1.RT ~ HumanORComputer, chin, mean) %>%
	mutate(round(Q1.RT, -1))

## Get empty data frame for predictions:

newdata <- tibble(novelty_c = unique(eng$novelty_c),
	novelty_c2 = unique(eng$novelty_c) ^ 2, human_c = 0,
	Q1.RESP_c = -0.5)
newdata <- bind_rows(newdata, newdata)
newdata[4:6, ]$Q1.RESP_c <- +0.5

## Load my predict function:

source('predict.glmm.R')

## Get predictions for English:

english_pred <- predict.glmm(eng.RT.afex$full.model,
	newdata = newdata)
spanish_pred <- predict.glmm(span.RT.afex$full.model,
	newdata = newdata)
chinese_pred <- predict.glmm(chin.RT.afex$full.model,
	newdata = newdata)

## Write into files for separate plotting script:

write_csv(english_pred, 'english_novelty_RT_predictions.csv')
write_csv(spanish_pred, 'spanish_novelty_RT_predictions.csv')
write_csv(chinese_pred, 'chinese_novelty_RT_predictions.csv')

# ## Create predictions (old bootstrapping approach, takes too much time):
# ## http://www.remkoduursma.com/blog/2017/06/01/confidence-intervals-on-predictions-from-mixed-effects-models/

# library(bootpredictlme4)
# newdata <- data.frame(novelty_c = unique(eng$novelty_c),
	# novelty_c2 = unique(eng$novelty_c) ^ 2, human_c = 0,
	# Q1.RESP_c = -0.5)

# newdata.preds <- predict(xmdl.RT.afex$full.model,
	# newdata = newdata, re.form=NA, se.fit=TRUE, nsim=5)

