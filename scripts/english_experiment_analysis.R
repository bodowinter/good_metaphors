## Bodo Winter
## March 19, 2017; Changed Sep 19, 2017
## Analysis of English metaphors

##------------------------------------------------------------------
## Data carpentry #1, General:
##------------------------------------------------------------------

## Load in packages:

library(lavaan)
library(stringr)
library(tidyverse)
library(irr)

## Load in data:

setwd('/Users/winterb/Research/metaphormagnet/analysis/data/')
eng <- read_csv('english_eprime.csv')

## Get rid of weird column names:

eng <- select(eng, -`Running[Block]`,
	-`Procedure[Block]`,
	-`Procedure[Trial]`,
	-`Running[Trial]`)

## Take only those needed:

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

## Get rid of "war is an exciting love" (ungrammatical):

eng <- filter(eng,
	Stimuli != 'War is an exciting love.')


##------------------------------------------------------------------
## Inter-rater reliability analysis:
##------------------------------------------------------------------

## First question:

Q1 <- select(eng, Subject,
	Q1.RESP, Stimuli)
Q1 <- Q1 %>% spread(key = Subject, value = Q1.RESP)

## Kappa fleiss for Q1:

kappam.fleiss(Q1[, 2:ncol(Q1)])

## Third question:

Q3 <- select(eng, Subject,
	Q3.RESP, Stimuli)
Q3 <- Q3 %>% spread(key = Subject, value = Q3.RESP)

## Kappa fleiss for Q1:

icc(Q3[, 2:ncol(Q3)])

## Fourth question:

Q4 <- select(eng, Subject,
	Q4.RESP, Stimuli)
Q4 <- Q4 %>% spread(key = Subject, value = Q4.RESP)

## Kappa fleiss for Q1:

kappam.fleiss(Q4[, 2:ncol(Q4)])




##------------------------------------------------------------------
## Experiment analysis, correlation of Q1, Q3 and Q4 across stims:
##------------------------------------------------------------------

## Get makes sense yes/no responses per stimulus:

Q1tab <- as_tibble(with(eng, table(Stimuli, Q1.RESP)))
Q1tab <- Q1tab %>% spread(Q1.RESP, n)

## Get proportions of makes sense question, Q1:

Q1tab <- mutate(Q1tab,
	YesProp = Yes / 21)

## Get average Q3 (quality ratings):

Q3tab <- eng %>% group_by(Stimuli) %>%
	summarize(quality = round(mean(Q3.RESP), 2),
		qualitySD = round(sd(Q3.RESP), 2))

## Get Q4 proportions:

Q4tab <- as_tibble(with(eng, table(Stimuli, Q4.RESP)))
Q4tab <- Q4tab %>% spread(Q4.RESP, n) %>%
	mutate(HumanProp = human / 21)

## Get average RTs:

RT_tab <- eng %>% group_by(Stimuli) %>%
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
# pairs(allcorrs)

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

## Merge form-form and novelty in there:

allstim$form <- eng[match(allstim$Stimuli, eng$Stimuli), ]$Form_Form_English
allstim$novelty <- eng[match(allstim$Stimuli, eng$Stimuli), ]$Novelty_European
allstim$human <- eng[match(allstim$Stimuli, eng$Stimuli), ]$HumanORComputer

## Sanity check, CFA:

myFormula <- '
	# measurement model
	Quality =~ YesProp + quality + HumanProp
	'

## Fit and analyze confirmatory factor analysis:

myFit <- cfa(myFormula, data = stims)
summary(myFit)		# also significantly correlated

## Write to file:

write_csv(allstim, 'english_allstims_summary_PCA.csv')



##------------------------------------------------------------------
## Add asymmetry values (external data):
##------------------------------------------------------------------

## Load in stimulus characteristics:

stimchar <- read_csv('stimuli_characteristics.csv')

## Create unique identifier:

stimchar <- mutate(stimchar,
	UID = str_c(target, ':', source))

## Create unique identifier for 'allstim' to match:

allstim$UID <- allstim %>% mutate(Stimuli = str_to_lower(Stimuli),
	Stimuli = str_replace(Stimuli, '\\.', ''),
	Stimuli = str_replace(Stimuli, '(a )|(an )', '')) %>%
	pull(Stimuli) %>%
		str_split(' is ', simplify = T) %>%
			as_tibble() %>%
				mutate(UID = str_c(V1, ':', V2)) %>% pull(UID)

## Join:

stims <- left_join(allstim, stimchar)



##------------------------------------------------------------------
## Publication-ready pairs plot:
##------------------------------------------------------------------

## Extract correlation values:

allcorrs <- allcorrs[, 1:3]

## Rename so the names appear nicely on the plot:

allcorrs <- rename(allcorrs, Sense = YesProp,
	Appreciation = quality,
	Humanness = HumanProp)

## Get variable names for plotting:

these_vars <- colnames(allcorrs)

## Function for setting up empty plots in loop below:

emptyplot <- function(...) {
	plot(1, 1, type = 'n',
		xlab = '', ylab = '',
		xaxt = 'n', yaxt = 'n',
		bty = 'n', ...)
	}

## Axis size parameter:

axis_cex <- 1.15

## The actual plot matrix:

par(mfrow = c(3, 3),
	mai = rep(0.1, 4), omi = c(1, 1, 0.25, 0.25))
for (i in 1:3) {
	for (j in 1:3) {
		if (i %in% c(1, 3)) {	# if proportion
			xlims = c(0, 1)
			} else {
				xlims = c(1, 5)
				}
		if (j %in% c(1, 3)) {	# if proportion
			ylims = c(0, 1)
			} else {
				ylims = c(1, 5)
				}
		emptyplot(xlim = ylims, ylim = xlims)
		if (i == j) {
			box(lwd = 1.5)
			text(x = mean(xlims),
				y = mean(ylims), font = 2,
				labels = these_vars[i],
				cex = 2.25)
			}
		if (i != j) {
			abline(lm(pull(allcorrs, i) ~ pull(allcorrs, j)),
				lwd = 2, col = 'darkgrey')
			points(x = pull(allcorrs, j),
				y = pull(allcorrs, i), cex = 1.6,
				pch = 21, col = NA, bg = rgb(0, 0, 0, 0.6))
			box(lwd = 1.5)
			}
		if (j == 1 & i %in% c(1, 3)) {
			axis(side = 2,
				at = seq(0, 1, 0.25),
				las = 2, lwd = 1.5,
				lwd.ticks = 1.5,
				font = 2, cex.axis = axis_cex)
			}
		if (j == 1 & i == 2) {
			axis(side = 2,
				at = 1:5,
				las = 2, lwd = 1.5,
				lwd.ticks = 1.5,
				font = 2, cex.axis = axis_cex)
			}
		if (i == 3 & j %in% c(1, 3)) {
			axis(side = 1,
				at = seq(0, 1, 0.25),
				lwd = 1.5,
				lwd.ticks = 1.5,
				font = 2, cex.axis = axis_cex)
			}
		if (i == 3 & j == 2) {
			axis(side = 1,
				at = 1:5,
				lwd = 1.5,
				lwd.ticks = 1.5,
				font = 2, cex.axis = axis_cex)
			}
		}
	}



##------------------------------------------------------------------
## Main novelty hypothesis and other condition variables:
##------------------------------------------------------------------

## Make form-form and novelty into polynomial contrasts (by hand):

stims <- mutate(stims,
	novelty_c = novelty - mean(novelty),
	novelty_c2 = novelty_c ^ 2,
	form_c = form - mean(form),
	form_c2 = form_c ^ 2,
	human_c = ifelse(human == 'c', -0.5, 0.5),
	article_c = ifelse(article == 'indefinite', -0.5, 0.5))

## Construct models for novelty:

summary(xmdl.full <- lm(PC1 ~ novelty_c + novelty_c2 +
	human_c, data = stims))
summary(xmdl.noquadraticnovelty <- lm(PC1 ~ novelty_c + 
	human_c, data = stims))
summary(xmdl.nonovelty <- lm(PC1 ~ 
	human_c, data = stims))

## Construct models for article and human:

summary(xmdl.nohuman <- lm(PC1 ~ novelty_c + novelty_c2, data = stims))
summary(xmdl.witharticle <- lm(PC1 ~ novelty_c + novelty_c2 +
	human_c + article_c, data = stims))

## Perform F-test of linear and quadratic effect together (omnibus):

anova(xmdl.nonovelty, xmdl.full, test = 'F')

## Perform F-test of quadratic effect:

anova(xmdl.noquadraticnovelty, xmdl.full, test = 'F')

## Perform F-test of linear effect:

anova(xmdl.nonovelty, xmdl.noquadraticnovelty, test = 'F')

## Perform F-tests of human and article:

anova(xmdl.nohuman, xmdl.full, test = 'F')
anova(xmdl.full, xmdl.witharticle, test = 'F')

## R-squared comparisons:

summary(xmdl.full)$r.squared
summary(xmdl.noquadraticnovelty)$r.squared
summary(xmdl.full)$r.squared - summary(xmdl.noquadraticnovelty)$r.squared	# 5%
summary(xmdl.nonovelty)$r.squared
summary(xmdl.noquadraticnovelty)$r.squared - summary(xmdl.nonovelty)$r.squared	# 46%
summary(xmdl.nohuman)$r.squared
summary(xmdl.full)$r.squared - summary(xmdl.nohuman)$r.squared	# 3%



##------------------------------------------------------------------
## Analysis of humanness:
##------------------------------------------------------------------

## Get matrix:

myM <- cbind(as.matrix(select(stims, computer)),
	21 - as.matrix(select(stims, computer)))
colnames(myM) <- c('computer', 'human')

## Extract only novelty = 1 & 2:

only_these <- stims$novelty_c < 1	# (1.1355 is the value for novelty = 3 after centering)
myM <- myM[only_these, ]

## Change order (modeling proportion of human judgments):

myM <- myM[, c(2, 1)]

## Extract human or not human vector:

human_yesno <- pull(stims, human)[only_these]

## Extract novelty as control variable:

novelty_c <- pull(stims, novelty_c)[only_these]

## Analyze using logistic regression:

summary(glm(myM ~ human_yesno + novelty_c, family = 'binomial'))





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

write_csv(newdata, 'english_novelty_predictions.csv')



##------------------------------------------------------------------
## Sanity check, structural equation model (SEM):
##------------------------------------------------------------------

## (In contrast to PCA, SEM accounts for measurement error)

## Create formula:

myFormula <- '
	# measurement model
	Quality =~ YesProp + quality + HumanProp
	# regression model
	Quality ~ novelty_c + novelty_c2 + human_c + article_c
	'
	
## Fit and analyze model:

myFit <- sem(myFormula, data = stims)
summary(myFit)



##------------------------------------------------------------------
## Check additional characteristics:
##------------------------------------------------------------------

## Create valence, concreteness and frequency overlap for adj-noun:

stims <- mutate(stims,
	valDiff = abs(adjVal - nounVal),
	freqDiff = abs(adjFreq - nounFreq),
	concDiff = abs(adjConc - nounConc))

## Create means for the B side of things (sources):

stims <- mutate(stims,
	sourceFreq = (adjFreq + nounFreq) / 2,
	sourceConc = (adjConc + nounConc) / 2,
	sourceVal = (adjVal + nounVal) / 2,
	sourceAbsVal = (adjAbsVal + nounAbsVal) / 2,
	sourceContVal = (adjContVal + nounContVal) / 2,
	sourceContAbsVal = (adjContAbsVal + nounContAbsVal) / 2)

## Create A versus B asymmetries:

stims <- mutate(stims,
	freqAsym = sourceFreq - targetFreq,
	concAsym = sourceConc - targetConc,
	valAsym = sourceVal - targetVal,
	absValAsym = sourceAbsVal - targetAbsVal,
	contValAsym = sourceContVal - targetContVal,
	contAbsValAsym = sourceContAbsVal - targetContAbsVal)

## Combined model:

summary(stims.char.mdl <- lm(PC1 ~ freqAsym + concAsym + valAsym, data = stims))
summary(stims.char.noConc <- lm(PC1 ~ freqAsym + valAsym, data = stims))
summary(stims.char.noFreq <- lm(PC1 ~ concAsym + valAsym, data = stims))
summary(stims.char.noVal <- lm(PC1 ~ freqAsym + concAsym, data = stims))

## Compare R-squareds:

summary(stims.char.mdl)$r.squared - summary(stims.char.noConc)$r.squared
summary(stims.char.mdl)$r.squared - summary(stims.char.noFreq)$r.squared
summary(stims.char.mdl)$r.squared - summary(stims.char.noVal)$r.squared

## Separate because otherwise a lot is deleted due to missingness:

summary(freq.mdl <- lm(PC1 ~ freqAsym, data = stims))
summary(conc.mdl <- lm(PC1 ~ concAsym, data = stims))
summary(val.mdl <- lm(PC1 ~ valAsym, data = stims))
summary(absval.mdl <- lm(PC1 ~ absValAsym, data = stims))
summary(contval.mdl <- lm(PC1 ~ contValAsym, data = stims))
summary(contabsval.mdl <- lm(PC1 ~ contAbsValAsym, data = stims))

## To see whether we have missed anything, quick random forest:

library(party)
myforest <- cforest(PC1 ~ adjFreq + nounFreq +
	adjConc + nounConc + adjVal + nounVal +
	adjAbsVal + nounAbsVal + adjContVal + nounContVal +
	adjContAbsVal + nounContAbsVal +
	freqAsym + concAsym + valAsym + absValAsym +
	contValAsym + contAbsValAsym +
	valDiff + freqDiff + concDiff, data = stims)
stims$PC1_pred <- as.vector(predict(myforest))
with(stims, cor(PC1, PC1_pred))	# r = 0.80

myvarimps <- varimp(myforest)
myvarimps <- sort(myvarimps)

## Check variable importances:

library(lattice)
dotplot(myvarimps)



##------------------------------------------------------------------
## Make a plot for paper:
##------------------------------------------------------------------

## Predictions:

xvals <- seq(-6, 6, 0.01)
freq_preds <- as_tibble(predict(freq.mdl,
	newdata = data.frame(freqAsym = xvals), se.fit = T)[1:2]) %>%
	mutate(UB = fit + 1.96 * se.fit,
		LB = fit - 1.96 * se.fit)
val_preds <- as_tibble(predict(val.mdl,
	newdata = data.frame(valAsym = xvals), se.fit = T)[1:2]) %>%
	mutate(UB = fit + 1.96 * se.fit,
		LB = fit - 1.96 * se.fit)
conc_preds <- as_tibble(predict(conc.mdl,
	newdata = data.frame(concAsym = xvals), se.fit = T)[1:2]) %>%
	mutate(UB = fit + 1.96 * se.fit,
		LB = fit - 1.96 * se.fit)

## Parameters:

x_axis_fac <- 0.05

## The actual plot:

quartz('', 11, 4)
par(mfrow = c(1, 3),
	mai = rep(0.2, 4), omi = c(1.15, 1.5, 0.5, 0.25))
# Plot 1
emptyplot(xlim = c(-5, 4), ylim = c(-4, 4))
text(x = -5 + x_axis_fac / 2 * 9,
	y = 3.7, labels = '(a)', font = 2, cex = 1.45)
axis(side = 1, at = seq(-5, 4, 1),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
axis(side = 2, at = seq(-4, 4, 1),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25,
	las = 2)
mtext(side = 2, text = 'Metaphor Quality (PC1)',
	line = 3.4, cex = 1.35, font = 2)
mtext(side = 1, text = 'Frequency Asymmetry',
	line = 3.5, cex = 1.35, font = 2)
axis(side = 1,
	at = c(-5 + x_axis_fac * 9, 4 - x_axis_fac * 9),
	labels = c('A > B', 'B > A'), font = 2, cex.axis = 1.5,
	tick = F, line = 4.5)
abline(v = 0, lty = 2, lwd = 2, col = 'darkgrey')
polygon(x = c(xvals, rev(xvals)),
	y = c(freq_preds$UB, rev(freq_preds$LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
abline(freq.mdl, lwd = 2)
with(stims,
	points(x = freqAsym, y = PC1, pch = 21,
	bg = rgb(0, 0, 0, 0.6), col = 'black', cex = 1.35))
box(lwd = 2)
# Plot 2
emptyplot(xlim = c(-3, 5), ylim = c(-4, 4))
text(x = -3 + x_axis_fac / 2 * 8,
	y = 3.7, labels = '(b)', font = 2, cex = 1.45)
axis(side = 1, at = seq(-3, 5, 1),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Valence Asymmetry',
	line = 3.5, cex = 1.35, font = 2)
axis(side = 1,
	at = c(-3 + x_axis_fac * 8, 5 - x_axis_fac * 8),
	labels = c('A > B', 'B > A'), font = 2, cex.axis = 1.5,
	tick = F, line = 4.5)
abline(v = 0, lty = 2, lwd = 2, col = 'darkgrey')
polygon(x = c(xvals, rev(xvals)),
	y = c(val_preds$UB, rev(val_preds$LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
abline(val.mdl, lwd = 2)
with(stims,
	points(x = valAsym, y = PC1, pch = 21,
	bg = rgb(0, 0, 0, 0.6), col = 'black', cex = 1.35))
box(lwd = 2)
# Plot 3
emptyplot(xlim = c(-3, 3), ylim = c(-4, 4))
text(x = -3 + x_axis_fac / 2 * 6,
	y = 3.7, labels = '(c)', font = 2, cex = 1.45)
axis(side = 1, at = seq(-3, 3, 1),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Concreteness Asymmetry',
	line = 3.5, cex = 1.35, font = 2)
axis(side = 1,
	at = c(-3 + x_axis_fac * 6, 3 - x_axis_fac * 6),
	labels = c('A > B', 'B > A'), font = 2, cex.axis = 1.5,
	tick = F, line = 4.5)
abline(v = 0, lty = 2, lwd = 2, col = 'darkgrey')
polygon(x = c(xvals, rev(xvals)),
	y = c(conc_preds$UB, rev(conc_preds$LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
abline(conc.mdl, lwd = 2)
with(stims,
	points(x = concAsym, y = PC1, pch = 21,
	bg = rgb(0, 0, 0, 0.6), col = 'black', cex = 1.35))
box(lwd = 2)






