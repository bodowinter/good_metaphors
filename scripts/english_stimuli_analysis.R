## Bodo Winter
## March 19, 2017; Changed Sep 19, 2017
## Preprocessing of English stimuli characteristics

##------------------------------------------------------------------
## Data carpentry #1, General:
##------------------------------------------------------------------

## Load in packages:

library(stringr)
library(lme4)
library(afex)
library(car)
library(lsmeans)
library(MuMIn)
library(tidyverse)

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



##------------------------------------------------------------------
## Data carpentry #2, process English stimuli:
##------------------------------------------------------------------

## Extract unique stimuli:

stims <- unique(pull(eng, Stimuli))

## Get their IDs:

metaphorIDs <- eng[match(stims, eng$Stimuli), ]$MetaphorID

## Make 'em lowercase:

stims <- str_to_lower(stims)

## Get rid of the final dot:

stims <- str_replace(stims, '\\.', '')

## Split them into source and target:

stims <- str_split(stims, ' is ', simplify = T)

## Make that into a tibble:

stims <- as_tibble(stims)

## Rename cols:

stims <- rename(stims,
	target = V1, source = V2)

## Create a column for whether there was an indefinite article:

stims <- mutate(stims,
	article = str_detect(source, '(a )|(an )'),
	article = ifelse(article, 'indefinite', 'no_article'),
	source = str_replace(source, '(a )|(an )', ''))

## Split adjective and noun:

stims <- stims %>% separate(source, c('adj', 'noun'), remove = F)

## Add IDs:

stims$ID <- metaphorIDs

## Arrange:

stims <- arrange(stims, metaphorIDs)



##------------------------------------------------------------------
## Data carpentry #3, process auxiliary files:
##------------------------------------------------------------------

## Load in additional datasets:

BLP <- read_csv('BLP.csv')
conc <- read_csv('brysbaert_2014_concreteness.csv')
war <- read_csv('warriner_2013_valence.csv')
snef <- read_delim('snefjella_kuperman_2016_valence.csv',
	delim = ' ')

## Process Warriner et al. (2013), including absolute valence:

war <- war %>%
	rename(val = V.Mean.Sum, word = Word) %>%
		mutate(val_z = (val - mean(val)) / sd(val),
		absVal = abs(val_z)) %>%
	select(word, val_z, absVal)

## Process Snefjella and Kuperman (2016) dataset:

snef <- snef %>%
	rename(contVal = Context_Valence,
		word = Word) %>%
		mutate(contVal_z = (contVal - mean(contVal)) / sd(contVal),
		contAbsVal = abs(contVal_z)) %>%
	select(word, contVal_z, contAbsVal)

## Process Brysbaert et al. (2014) concreteness:

conc <- conc %>%
	rename(conc = Conc.M, word = Word) %>%
	mutate(conc_z = (conc - mean(conc)) / sd(conc)) %>%
	select(word, conc_z)

## Process BLP:

BLP <- BLP %>%
	rename(word = spelling, freq = subtlex.frequency) %>%
	mutate(logfreq = log10(freq + 1),
		logfreq_z = (logfreq - mean(logfreq)) / sd(logfreq)) %>%
	select(word, freq, logfreq, logfreq_z)


##------------------------------------------------------------------
## Data carpentry #4, merge auxiliary data into stims for targets:
##------------------------------------------------------------------

## Create empty columns to be filled with data:

stims$adjFreq <- NA
stims$nounFreq <- NA
stims$targetFreq <- NA
stims$adjConc <- NA
stims$nounConc <- NA
stims$targetConc <- NA
stims$adjVal <- NA
stims$nounVal <- NA
stims$targetVal <- NA
stims$targetAbsVal <- NA
stims$adjAbsVal <- NA
stims$nounAbsVal <- NA
stims$adjContVal <- NA
stims$nounContVal <- NA
stims$targetContVal <- NA
stims$adjContAbsVal <- NA
stims$nounContAbsVal <- NA
stims$targetContAbsVal <- NA

## Merge targets in there:

stims$targetFreq <- BLP[match(stims$target, BLP$word), ]$logfreq_z
stims$targetConc <- conc[match(stims$target, conc$word), ]$conc_z
stims$targetVal <- war[match(stims$target, war$word), ]$val_z
stims$targetAbsVal <- war[match(stims$target, war$word), ]$absVal
stims$targetContVal <- snef[match(stims$target, snef$word), ]$contVal_z
stims$targetContAbsVal <- snef[match(stims$target, snef$word), ]$contAbsVal

## Get missing frequencies for those that are NA (= 0 in BLP):

stims[is.na(stims$targetFreq), ]$targetFreq <- -0.70494049		# z-score of 0


##------------------------------------------------------------------
## Data carpentry #5, merge auxiliary data into stims for sources:
##------------------------------------------------------------------

## Adjectives:

stims$adjFreq <- BLP[match(stims$adj, BLP$word), ]$logfreq_z
stims$adjConc <- conc[match(stims$adj, conc$word), ]$conc_z
stims$adjVal <- war[match(stims$adj, war$word), ]$val_z
stims$adjAbsVal <- war[match(stims$adj, war$word), ]$absVal
stims$adjContVal <- snef[match(stims$adj, snef$word), ]$contVal_z
stims$adjContAbsVal <- snef[match(stims$adj, snef$word), ]$contAbsVal

## Nouns:

stims$nounFreq <- BLP[match(stims$noun, BLP$word), ]$logfreq_z
stims$nounConc <- conc[match(stims$noun, conc$word), ]$conc_z
stims$nounVal <- war[match(stims$noun, war$word), ]$val_z
stims$nounAbsVal <- war[match(stims$noun, war$word), ]$absVal
stims$nounContVal <- snef[match(stims$noun, snef$word), ]$contVal_z
stims$nounContAbsVal <- snef[match(stims$noun, snef$word), ]$contAbsVal

## Get missing frequencies for those that are NA (= 0 in BLP):

stims[is.na(stims$adjFreq), ]$adjFreq <- -0.70494049		# z-score of 0
stims[is.na(stims$nounFreq), ]$nounFreq <- -0.70494049		# z-score of 0

## Write to external:

write_csv(stims, 'stimuli_characteristics.csv')


