## Bodo Winter
## Sep 20, 2017
## Plot predictions for novelty effect across three languages

##------------------------------------------------------------------
## Load in predictions:
##------------------------------------------------------------------

## Load in libraries:

library(tidyverse)

## Load in data:

setwd('/Users/winterb/Research/metaphormagnet/analysis/data/')
chin <- read_csv('chinese_novelty_predictions.csv')
span <- read_csv('spanish_novelty_predictions.csv')
eng <- read_csv('english_novelty_predictions.csv')



##------------------------------------------------------------------
## Make the plot:
##------------------------------------------------------------------

## Function for setting up empty plots in loop below:

emptyplot <- function(...) {
	plot(1, 1, type = 'n',
		xlab = '', ylab = '',
		xaxt = 'n', yaxt = 'n',
		bty = 'n', ...)
	}

## Parameters:

x_axis_fac <- 0.05

## The actual plot:

quartz('', 11, 4)
par(mfrow = c(1, 3),
	mai = rep(0.2, 4), omi = c(1.15, 1.5, 0.5, 0.25))
# Plot 1
emptyplot(xlim = c(0.5, 3.5), ylim = c(-3, 2))
text(x = 0.5 + x_axis_fac / 2 * 3,
	y = 1.8, labels = '(a)', font = 2, cex = 1.45)
axis(side = 1, at = 1:3,
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
axis(side = 2, at = seq(-4, 4, 1),
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25,
	las = 2)
points(1:3, y = eng$fit,
	pch = 15, cex = 1.5, type = 'b')
arrows(x0 = 1:3, x1 = 1:3,
	y0 = eng$LB, y1 = eng$UB,
	lwd = 2, code = 3, length = 0.1, angle = 90)
mtext(side = 2, text = 'Metaphor Quality (PC1)',
	line = 3.4, cex = 1.35, font = 2)
mtext(side = 1, text = 'Novelty\n(English)',
	line = 5.5, cex = 1.35, font = 2)
box(lwd = 2)
# Plot 2
emptyplot(xlim = c(0.5, 3.5), ylim = c(-3, 2))
text(x = 0.5 + x_axis_fac / 2 * 3,
	y = 1.8, labels = '(b)', font = 2, cex = 1.45)
axis(side = 1, at = 1:3,
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Novelty\n(Spanish)',
	line = 5.5, cex = 1.35, font = 2)
points(1:3, y = span$fit,
	pch = 15, cex = 1.5, type = 'b')
arrows(x0 = 1:3, x1 = 1:3,
	y0 = span$LB, y1 = span$UB,
	lwd = 2, code = 3, length = 0.1, angle = 90)
box(lwd = 2)
# Plot 3
emptyplot(xlim = c(0.5, 3.5), ylim = c(-3, 2))
text(x = 0.5 + x_axis_fac / 2 * 3,
	y = 1.8, labels = '(c)', font = 2, cex = 1.45)
axis(side = 1, at = 1:3,
	lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.25)
mtext(side = 1, text = 'Novelty\n(Chinese)',
	line = 5.5, cex = 1.35, font = 2)
points(1:3, y = chin$fit,
	pch = 15, cex = 1.5, type = 'b')
arrows(x0 = 1:3, x1 = 1:3,
	y0 = chin$LB, y1 = chin$UB,
	lwd = 2, code = 3, length = 0.1, angle = 90)
box(lwd = 2)

