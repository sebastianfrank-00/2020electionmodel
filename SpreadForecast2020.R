setwd("~/Desktop/Econometrics/Election Forecast")
## reading and creating spread model
read.csv("R-D Spread Regression.csv")
data3 <- read.csv("R-D Spread Regression.csv")
attach(data3)
spreadmodel <- lm(FinalSpread ~ PollSpread, data  = data3)
summary(spreadmodel)

## reading and introducing current spread data
read.csv("FinalSpreads.csv")
data4 <- read.csv("FinalSpreads.csv")
attach(data4)

## checking for complete data set
good <- complete.cases(data4)
data4[good, ]
identical(data4, data4[good, ])

##opens applicable libraries
library(scales)

##for loop for state ev by party 
rev <- 0
dev <- 0
rstates <- c()
dstates <- c()
for (row in 1:nrow(data4)) {
  spreadhat <- .039334  + 1.286697 * data4[row, 'Pspreadact'] 
  ## assigns ev based on pv winner per state  
  if (spreadhat > 0) {
    rev <- data4[row, 'EV'] + rev
    rstates <- c(rstates, as.character(data4[row, 'State']))
  } else {
    dev <- data4[row, 'EV'] + dev
    dstates <- c(dstates, as.character(data4[row, 'State']))
  }
}

## for loop for state pv results by party 
dpv <- 0
rpv <- 0
for (row in 1:nrow(data4)) {
  spreadhat <- .039334 + 1.286697 * data4[row, 'Pspreadact'] 
  if (spreadhat > 0) {
    staterpv <- .5 + (spreadhat / 2)
    statedpv <- 1 - staterpv 
  } 
  if (spreadhat < 0) {
    statedpv <- .5 + (abs(spreadhat) / 2)
    staterpv <- 1 - statedpv
  }
  ## returns individual state results 
  print(c(as.character(data4[row, 'State']),"Trump:", percent(round(staterpv, digits = 4), accuracy = .01), "Biden:", percent(round(statedpv, digits = 4), accuracy = .01)))
  ## calculates national pv totals per candidate  
  dpv <- (statedpv * data4[row, 'Turnout']) + dpv
  rpv <- (staterpv * data4[row, 'Turnout']) + rpv
}

## runs 1000 state simulations 
for (row in 1:nrow(data4)) {
  spreadhat <- .039334  + 1.286697 * data4[row, 'Pspreadact'] 
  electionsimulationdf <- data.frame(simulate(spreadmodel, nsim = 1000, seed = NULL), row.names = data4[, 'State'])
}

attach(electionsimulationdf)

## checking for complete data set
good1 <- complete.cases(electionsimulationdf)
electionsimulationdf[good, ]
identical(electionsimulationdf, electionsimulationdf[good, ])

## returns simulated victories (out of 1000 sims) for Biden
dsimcountvector <- c()
dsimcount[] <- 0
for (row in 1:nrow(electionsimulationdf)) {
  for (col in 1:ncol(electionsimulationdf)) {
    if (as.numeric(electionsimulationdf[row, col]) < 0) {
      dsimcount[row] <- dsimcount[row] + 1
      }
    } 
  dsimcountvector <- c(dsimcountvector, dsimcount[row])
}

data.frame(dsimcountvector,  names = data4[,  'State'])

## displaying the results
print(c("Hello world, awaiting forecast projections..."))
print(c("The forecast projects Biden will receive", percent(round(dpv, digits = 4), accuracy = .01), "of the popular vote and", dev, "electoral votes."))
print(c("The forecast projects Trump will receive", percent(round(rpv, digits = 4), accuracy = .01), "of the popular vote and", rev, "electoral votes"))
print(c("The forecast projects that Biden will win the following states:", dstates))
print(c("The forecast projects that Trump will win the following states:", rstates))

## VISUALIZED 

electionsimulationdf$State <- data4[,  'State']
electionsimulationdf$EV <- data4[, 'EV']

statevect <- c()
simdpvlist <- c()
simrpvlist <- c()
for (row in 1:nrow(electionsimulationdf)) {
  for (q in electionsimulationdf[row, ]) {
    statevect <- c(q, statevect)
      for (v in statevect)  {
        if (v > 0) {
          simrpv <- .5 + (v / 2)
          simdpv <- 1 - simrpv
        } 
        if (v < 0) {
          simdpv <- .5 + (abs(v) / 2)
          simrpv <- 1 - simdpv
        }
      }
  }
  simdpvlist <- c(simdpv, simdpvlist)
  simrpvlist <- c(simrpv, simrpvlist)
  hist(simdpvlist, main = paste("Histogram of", "Biden simulation outcomes"))
  hist(simrpvlist, main = paste("Histogram of", "Trump simulation outcomes"))
  
  }

## AL example 
statevect <- c()
simdpvlist <- c()
simrpvlist <- c()
for (i in electionsimulationdf[3, ]) {
    statevect <- c(i, statevect)
    for (i in statevect) {
      if (i > 0) {
        simrpv <- .5 + (i / 2)
        simdpv <- 1 - simrpv
      } 
      if (i < 0) {
        simdpv <- .5 + (abs(i) / 2)
        simrpv <- 1 - simdpv
      }
      simdpvlist <- c(simdpv, simdpvlist)
      simrpvlist <- c(simrpv, simrpvlist)
      print(hist(simdpvlist, main = paste("Histogram of", "Biden simulation outcomes for Alabama")))
      print(hist(simrpvlist, main = paste("Histogram of", "Trump simulation outcomes for Alabama")))
    }
}
  
## National charts
  natlvect <- c()
  natldpvlist <- c()
  natlrpvlist <- c()
  for (i in electionsimulationdf[, 1]) {
    natlvect <- c(i,natlvect)
  }
  for (i in natlvect) {
    if (i > 0) {
      simrpv <- .5 + (i / 2)
      simdpv <- 1 - simrpv
    } 
    if (i < 0) {
      simdpv <- .5 + (abs(i) / 2)
      simrpv <- 1 - simdpv
    }
    dpv <- (simdpv * electionsimulationdf[row, 'EV']) / 538 + dpv
    rpv <- (simrpv * electionsimulationdf[row, 'EV']) / 538 + rpv

    natldpvlist <- c(dpv, natldpvlist)
    natlrpvlist <- c(rpv, natlrpvlist)
    hist(natldpvlist)
    hist(natlrpvlist)
  }
  


