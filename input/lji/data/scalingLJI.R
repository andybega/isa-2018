## 
## Replication file for Linzer, Drew and Jeffrey K. Staton. 2015. "A Global Measure of Judicial Independence, 1948-2012." Journal of Law and Courts.
## Drew Linzer and Jeff Staton
## dlinzer@gmail.com, jkstato@emory.edu
## May 22, 2015
## 
## scalingJI.R
## 

library(R2jags)
library(coda)
library(lattice)
library(rjags)


saveplots <- FALSE

recode <- function(x) {
  y <- rep(NA, length(x))
  nt <- as.numeric(names(table(x)))
  for (i in 1:length(nt)) {
    y[x==nt[i]] <- i
  }
  return(y)
}

cymat <- function(x, cnum, yr) {
  ret <- matrix(NA, nrow=length(table(cnum)), ncol=length(table(yr)))
  v.cnum <- as.numeric(names(table(cnum)))
  v.year <- as.numeric(names(table(yr)))
  for (i in 1:nrow(ret)) {
    for (j in 1:ncol(ret)) {
      ins <- x[cnum==v.cnum[i] & yr==v.year[j]]
      if (length(ins)>0) {
        ret[i, j] <- ins
      }
    }
  }
  return(ret)
}

rescale01 <- function(x, lo=NULL, hi=NULL) {
  if (is.null(lo)) lo <- min(x, na.rm=TRUE)
  if (is.null(hi)) hi <- max(x, na.rm=TRUE)
  y <- (x-lo)/(hi-lo) # maps [lo, hi] -> [0, 1]
  return(y)
}

write.est <- function(dat, bugs.model, filename) {
  # Output latent variable estimates in a convenient .csv spreadsheet
  res <- data.frame(country=dat$country, 
                    abbr=dat$abbr, 
                    ccode=dat$cow, 
                    year=dat$year, 
                    LJI=round(bugs.model$mean$x, 4), 
                    post.sd=round(bugs.model$sd$x, 4))
  write.csv(res, filename, row.names=FALSE)
  invisible()
}


## Load data and country codes
## Merge country names and abbreviations
## Sort by country-year
indat <- read.csv("jimeasures_082113.csv")
ccode <- read.csv("COW-State-list.csv")
indat <- merge(indat, ccode, all.x=T, sort=FALSE)
indat <- indat[order(indat$cow, indat$year), ]

# Fix problematic xconst coding for cow 529, 769, 818
# 529 Ethiopia -> 530
indat$xconst[indat$cow==530 & indat$year>=1995 & indat$year<=2012] <- indat$xconst[indat$cow==529 & indat$year>=1995 & indat$year<=2012]
indat <- indat[indat$cow != 529, ]
# 769 Pakistan -> 770
indat$xconst[indat$cow==770 & indat$year>=1948 & indat$year<=1968] <- indat$xconst[indat$cow==769 & indat$year>=1948 & indat$year<=1968]
indat <- indat[indat$cow != 769, ]
# 818 Vietnam -> 816
indat$xconst[indat$cow==816 & indat$year>=1976 & indat$year<=2010] <- indat$xconst[indat$cow==818 & indat$year>=1976 & indat$year<=2010]
indat <- indat[indat$cow != 818, ]
# Remove Republic of Vietnam which is inconsistently measured by indicators
indat <- indat[indat$cow !=817, ]


# remove country-years that are all NA for first/last years
datsplit <- split(indat, indat$cow)
dat <- NULL
for (i in 1:length(datsplit)) {
  yvars <- datsplit[[i]][, c('laworder', 'ciri', 'hc', 'gcr', 'fvfacto80_03', 'keith', 'cim', 'xconst')]
  first <- as.numeric(which(cumsum(!apply(is.na(yvars), 1, all))==1))
  last <- nrow(datsplit[[i]])-as.numeric(which(cumsum(rev(!apply(is.na(yvars), 1, all)))==1))+1
  if (length(first) > 1) {first <- min(first)}
  if (length(last) > 1) {last <- min(last)}
  if (length(first) > 0) {
    dat <- rbind(dat, datsplit[[i]][first:last, ])
  }
}

# Drop countries with fewer than 10 years of data, start to end
# sort(table(dat$cow))
dat <- dat[!(dat$cow %in% as.numeric(names(table(dat$cow))[table(dat$cow) < 10])), ]

# Not important, but there are a few countries with gaps; 30 country-years
#dat[apply(is.na(dat[, c('laworder', 'ciri', 'hc', 'gcr', 'fvfacto80_03', 'keith', 'cim', 'xconst')]), 1, all), ]

# Remove unused factor levels
dat$abbr <- dat$abbr[, drop=T]
dat$country <- dat$country[, drop=T]

# Make vectors of country names & abbr in order of dataset
cnames <- as.character(dat$country[1])
cabbr <- as.character(dat$abbr[1])
for (i in 2:nrow(dat)) {
  if (dat$country[i] != dat$country[i-1]) {
    cnames <- c(cnames, as.character(dat$country[i]))
    cabbr <- c(cabbr, as.character(dat$abbr[i]))
  }
}

# convert levels: continuous -> ordinal
dat$cim.cat <- cut(dat$cim, c(-1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), FALSE)
dat$fvfacto.cat <- cut(dat$fvfacto80_03, 6, FALSE)
dat$laworder.cat <- cut(dat$laworder, c(-1, 1, 2, 3, 4, 5, 6), FALSE)
dat$gcr.cat <- cut(dat$gcr, 6, FALSE)

# Tabulate manifest variables
if (F) {
  table(dat$keith)            # y1: 0, 1, 2
  table(dat$hc)               # y2: 0, 1, 2
  table(dat$ciri)             # y3: 0, 1, 2
  table(dat$xconst)           # y4: 1, 2, 3, 4, 5, 6, 7
  table(dat$cim.cat)          # y5: 1, 2, 3, 4, 5, 6, 7, 8
  table(dat$fvfacto.cat)      # y6: 1, 2, 3, 4, 5, 6
  table(dat$laworder.cat)     # y7: 1, 2, 3, 4, 5, 6
  table(dat$gcr.cat)          # y8: 1, 2, 3, 4, 5, 6
}

##
## RUN MODEL
## 8 indicators: 200 countries, 65 years, 9815 country-years
##

# set up indicators
y1 <- 1+dat$keith
y2 <- 1+dat$hc
y3 <- 1+dat$ciri
y4 <- dat$xconst
y5 <- dat$cim.cat
y6 <- dat$fvfacto.cat
y7 <- dat$laworder.cat
y8 <- dat$gcr.cat
y <- cbind(y1, y2, y3, y4, y5, y6, y7, y8)

# indexing for countries and years
country <- recode(dat$cow)
year <- recode(dat$year)

# other inits for model
J <- ncol(y)
K <- apply(y, 2, max, na.rm=TRUE)
N <- nrow(y)
Mc <- length(table(country))
coffset <- as.vector(c(1, 1+cumsum(table(country))))
koffset <- as.vector(c(1, 1+cumsum(K-1)))

inits <-  function() { list(cutp.raw=c(0.3, 0.6, 
                                       0.4, 0.6, 
                                       0.2, 0.6, 
                                       0.1, 0.2, 0.35, 0.4, 0.5, 0.6, 
                                       -0.7, -0.4, -0.2, 0, 0.2, 0.4, 0.8, 
                                       -0.2, 0.2, 0.6, 0.8, 1.3, 
                                       -0.3, 0.1, 0.4, 0.7, 1.1, 
                                       -0.3, 0.3, 0.6, 0.9, 1.3), 
                            beta=runif(J, 0, 10), 
                            x.sd=runif(Mc, 0, 1), 
                            x=runif(N, 0, 1)) }

jagsfit <- jags.parallel(data=c("y", "J", "K", "N", "Mc", "coffset", "koffset", "country"), 
                         inits=inits, 
                         parameters.to.save=c("beta", "cutp", "x", "x.sd"), 
                         model.file="DynamicGRM.txt", 
                         n.chains=3, 
                         n.iter=2000)
bugs.model <- jagsfit$BUGSoutput

# save(bugs.model, file="bugs-3x2000.RData")
# load("bugs-3x2000.RData")

# check convergence
if (F) {
  par(mfrow=c(1, 1), ask=FALSE)
  hist(bugs.model$summary[, 8], xlab="Rhat", main="", breaks=50, col="gray")
  traceplot(bugs.model, mfrow=c(5, 7))
  traceplot(bugs.model, mfrow=c(1, 2), varname="x.sd")
}

xhat <- cymat(bugs.model$mean$x, country, year)
x.sd <- cymat(bugs.model$sd$x, country, year)
#write.est(dat, bugs.model, "LJI-estimates-20140422.csv")


##
## Assess model fit: posterior predictive check
## Simulate data from estimates of latent values x and cutpoints on y
##
vars <- c("Keith", "Howard-Carey", "CIRI", "XCONST", 
          "CIM", "Feld-Voigt", "PRS", "GCR")
cutp.list <- list()
for (j in 1:J) {
  cutp.list[[j]] <- bugs.model$sims.list$cutp[, koffset[j]:(koffset[j+1]-1)]
}

nsims <- 1000 # this takes a while
resamp <- sample(c(1:bugs.model$n.sims), nsims)
sim.tab <- list(y1=NULL, y2=NULL, y3=NULL, y4=NULL, 
                y5=NULL, y6=NULL, y7=NULL, y8=NULL)
pb <- txtProgressBar(min=1, max=nsims, style=3)
for (r in 1:nsims) {      # redraw parameter estimtes
  iter <- resamp[r]
  setTxtProgressBar(pb, r)
  for (i in 1:J) {        # loop through manifest vars
    sim.vals <- NULL
    for (j in which(!is.na(y[, i]))) {    # loop through observed country-years
      Qvec <- plogis((cutp.list[[i]][iter, ] - bugs.model$sims.list$x[iter, j])*
                       bugs.model$sims.list$beta[iter, i])
      pvec <- Qvec[1]
      for (k in 2:length(Qvec)) {
        pvec <- c(pvec, Qvec[k]-Qvec[k-1])
      }
      pvec <- c(pvec, 1-sum(pvec))
      sim.vals <- c(sim.vals, which(rmultinom(1, 1, pvec)==1))
    }
    sim.tab[[i]] <- rbind(sim.tab[[i]], table(sim.vals))
  }
}

quartz(width=12, height=6)
par(mfrow=c(2, 4), mar=c(4, 4, 2, 0.5), las=1)
misses.total <- 0
for (i in 1:J) {
  hist(y[, i], breaks=seq(-0.5, K[i]+0.5, 1), xlim=c(0, K[i]+1), ylim=c(0, max(table(y[, i]))*1.3), 
       col="gray", xaxt="n", xlab="Rating Level\n", ylab="Rating Frequency", main=vars[i])
  axis(1, c(1:K[i]), c(1:K[i]))
  points(c(1:K[i]), apply(sim.tab[[i]], 2, mean), pch=19)
  segments(c(1:K[i]), apply(sim.tab[[i]], 2, quantile, 0.025), c(1:K[i]), apply(sim.tab[[i]], 2, quantile, 0.975))
  cat("\n\n", i, vars[i], "\n")
  hi <- apply(sim.tab[[i]], 2, quantile, 0.975)
  lo <- apply(sim.tab[[i]], 2, quantile, 0.025)
  resmat <- round(rbind(hi, table(y[, i]), lo))
  rownames(resmat) <- c("97.5%", "actual", "2.5%")
  print(resmat)
  misses <- sum((table(y[, i]) < lo) | (table(y[, i]) > hi))
  misses.total <- misses.total+misses
  cat("misses:", misses, "\n")
  for (j in 1:K[i]) {
    segments(j-0.15, apply(sim.tab[[i]], 2, quantile, 0.025)[j], j+0.15, apply(sim.tab[[i]], 2, quantile, 0.025)[j])
    segments(j-0.15, apply(sim.tab[[i]], 2, quantile, 0.975)[j], j+0.15, apply(sim.tab[[i]], 2, quantile, 0.975)[j])
  }
}
if (saveplots) { dev.copy2pdf(file="posterior-fit.pdf") }
misses.total/sum(K)


##
## Compare cutpoints on y
##
quartz(width=12, height=6)
par(mar=c(2.5, 4, 1, 0), ask=F, las=1, mfrow=c(1, 1))
cutpoints <- bugs.model$mean$cutp
plot(0, col="white", xlab="", ylab="Threshold value", bty="n", xaxt="n", 
     xlim=c(1.4, length(cutpoints)-0.5), ylim=c(-1, 1.5))
cur <- 1
abline(v=cur-0.5, lty=3, col="gray10")
for (i in 1:J) {
  sm <- as.matrix(bugs.model$sims.list$cutp[, koffset[i]:(koffset[i+1]-1)])
  for (j in 1:ncol(sm)) {
    polygon(c(cur-0.35, cur-0.35, cur+0.35, cur+0.35), 
            c(quantile(sm[, j], 0.025), quantile(sm[, j], 0.975), quantile(sm[, j], 0.975), quantile(sm[, j], 0.025)), 
            border="gray80", col="gray80")
    segments(cur-0.35, mean(sm[, j]), cur+0.35, mean(sm[, j]), lwd=2)
    cur <- cur+1
  }
  abline(v=cur-0.5, lty=3, col="gray10")
}
axis(1, c(1.5, 3.5, 5.5, 9.5, 16, 22, 27, 32), 
     c("Keith\n", "Howard-\nCarey", "CIRI\n", "XCONST\n", "CIM\n", 
       "Feld-Voigt\n", "PRS\n", "GCR\n"), tick=F, cex.axis=0.7)
if (saveplots) { dev.copy2pdf(file="cutpoints.pdf") }


##
## Plot latent variable estimates for all countries
##
simsort <- apply(bugs.model$sims.list$x, 2, sort)
yr <- c(min(dat$year):max(dat$year))
quartz(width=8, height=11)
par(mfrow=c(8, 5), mar=c(2.5, 2, 1, 0.5), las=1, ask=F, cex.axis=0.6, cex.main=0.9) # for 40 per page in appendix
idx <- 1
for (i in order(cnames)) {
  plot(0, col="white", xlim=c(min(yr), max(yr)), ylim=c(-0.1, 1.1), 
       xlab="", ylab="", main=cnames[i], bty="n")
  sel <- dat[country==i, ]
  xax <- c(min(sel$year):max(sel$year))
  polygon(c(xax, rev(xax)), 
          c(simsort[floor(bugs.model$n.sims*0.1), coffset[i]:(coffset[i+1]-1)], 
            rev(simsort[ceiling(bugs.model$n.sims*0.9), coffset[i]:(coffset[i+1]-1)])), 
          col="gray80", border=NA)
  lines(yr, xhat[i, ], col="black", lwd=2)
  
  if (idx/40 == round(idx/40)) {
    if (saveplots) { dev.copy2pdf(file=paste("alltrends", idx/40, ".pdf", sep="")) }
  }
  idx <- idx+1
}


##
## Make individual country plots
##
quartz(width=4, height=4)
par(mar=c(3, 3, 3, 1), mfrow=c(1, 1), las=1, ask=FALSE)
yr <- c(min(dat$year):max(dat$year))
toplot <- c("United States of America", "Cuba", "Chile", "Thailand", "Spain", "Argentina", "Venezuela", "Egypt")
for (i in toplot) {
  sel <- dat[dat$country==i, ]
  idx <- which(cnames==i)
  xax <- c(min(sel$year):max(sel$year))
  plot(0, col="white", xlim=c(min(yr), max(yr)), ylim=c(-0.1, 1.1), xlab="", ylab="", 
       main=i, bty="n")
  polygon(c(xax, rev(xax)), 
          c(simsort[floor(bugs.model$n.sims*0.1), coffset[idx]:(coffset[idx+1]-1)], 
            rev(simsort[ceiling(bugs.model$n.sims*0.9), coffset[idx]:(coffset[idx+1]-1)])), 
          col="gray80", border=NA)
  lines(sel$year, rowMeans(cbind(rescale01(sel$keith, 0, 2), 
                                 rescale01(sel$hc, 0, 2), 
                                 rescale01(sel$ciri, 0, 2), 
                                 rescale01(sel$xconst, 1, 7), 
                                 sel$cim, 
                                 sel$fvfacto80_03, 
                                 sel$laworder/6, 
                                 sel$gcr/10), na.rm=TRUE), lwd=2, col="gray40")
  lines(yr, xhat[idx, ], col="black", lwd=3)
  jit <- 0.04
  points(sel$year, rescale01(sel$keith, 0, 2)+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[1]/30), pch=19, cex=1.3)
  points(sel$year, rescale01(sel$hc, 0, 2)+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[2]/30), pch=19, cex=1.3)
  points(sel$year, rescale01(sel$ciri, 0, 2)+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[3]/30), pch=19, cex=1.3)
  points(sel$year, rescale01(sel$xconst, 1, 7)+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[4]/30), pch=19, cex=1.3)
  points(sel$year, sel$cim+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[5]/30), pch=17, cex=1.3)
  points(sel$year, sel$fvfacto80_03+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[6]/30), pch=19, cex=1.3)
  points(sel$year, sel$laworder/6+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[7]/30), pch=19, cex=1.3)
  points(sel$year, sel$gcr/10+runif(nrow(sel), -jit, jit), col=rgb(0, 0, 0, bugs.model$mean$beta[8]/30), pch=19, cex=1.3)
  if (saveplots) { dev.copy2pdf(file=paste("trendfit-", gsub(" ", "", i), ".pdf", sep="")) }
}




##
## Plot JI for all countries in 2010
##
plotyear <- max(year) - 2
keep <- !is.na(xhat[, plotyear])
x <- xhat[keep, plotyear]
cn.keep <- cnames[keep]
o <- order(x)
ci <- NULL
for (i in 1:max(country)) {
  sims <- sort(bugs.model$sims.list$x[, country==i & year==plotyear])
  nsim <- length(sims)
  ci <- rbind(ci, c(sims[floor(0.1*nsim)], sims[ceiling(0.9*nsim)]))
}

# Low JI group
quartz(width=4, height=11)
par(mar=c(2.5, 6.7, 0, 0), las=1)
lowgrp <- floor(sum(keep)/2)
plot(x[o][1:lowgrp], rev(1:lowgrp), xlab="", pch=19, ylab="", 
     yaxt="n", bty="n", cex.axis=0.8, xlim=c(0, 1))
segments(0, 1, 0, lowgrp, col="gray50")
segments(1, 1, 1, lowgrp, col="gray50")
segments(rep(0, lowgrp), c(1:lowgrp), rep(1, lowgrp), c(1:lowgrp), col="gray50", lty=3)
segments(ci[o, 1][1:lowgrp], rev(1:lowgrp), ci[o, 2][1:lowgrp], rev(1:lowgrp), lwd=2)
axis(2, rev(1:lowgrp), cn.keep[o][1:lowgrp], cex.axis=0.5, tick=F, line=-1)
if (saveplots) { dev.copy2pdf(file="xhat2010-low.pdf") }

# High JI group
quartz(width=4, height=11)
par(mar=c(2.5, 6.7, 0, 0), las=1)
higrp <- lowgrp+1
plot(x[o][higrp:sum(keep)], rev(higrp:sum(keep)), xlab="", pch=19, ylab="", 
     yaxt="n", bty="n", cex.axis=0.8, xlim=c(0, 1))
segments(0, higrp, 0, sum(keep), col="gray50")
segments(1, higrp, 1, sum(keep), col="gray50")
segments(rep(0, higrp), c(higrp:sum(keep)), rep(1, higrp), c(higrp:sum(keep)), col="gray50", lty=3)
segments(ci[o, 1][higrp:sum(keep)], rev(higrp:sum(keep)), ci[o, 2][higrp:sum(keep)], rev(higrp:sum(keep)), lwd=2)
axis(2, rev(higrp:sum(keep)), cn.keep[o][higrp:sum(keep)], cex.axis=0.5, tick=F, line=-1)
if (saveplots) { dev.copy2pdf(file="xhat2010-hi.pdf") }




##
## Re-fit the model without CIM (y5) for robustness
## 
y <- cbind(y1, y2, y3, y4, y6, y7, y8)
J <- ncol(y)
K <- apply(y, 2, max, na.rm=TRUE)
koffset <- as.vector(c(1, 1+cumsum(K-1)))
inits <-  function() { list(cutp.raw=c(0.3, 0.6, 
                                       0.4, 0.6, 
                                       0.2, 0.6, 
                                       0.1, 0.2, 0.4, 0.45, 0.5, 0.6, 
                                       -0.2, 0.2, 0.6, 0.8, 1.3, 
                                       -0.2, 0.1, 0.4, 0.7, 1, 
                                       -0.2, 0.3, 0.6, 0.9, 1.2), 
                            beta=runif(J, 0, 10), 
                            x.sd=runif(Mc, 0, 1), 
                            x=runif(N, 0, 1)) }

jagsfit.noCIM <- jags.parallel(data=c("y", "J", "K", "N", "Mc", "coffset", "koffset", "country"), 
                               inits=inits, 
                               parameters.to.save=c("beta", "cutp", "x", "x.sd"), 
                               model.file="DynamicGRM.txt", 
                               n.chains=3, 
                               n.iter=2000)
bugs.model.noCIM <- jagsfit.noCIM$BUGSoutput

# save(bugs.model.noCIM, file="bugs-3x2000-noCIM.RData")
# load("bugs-3x2000-noCIM.RData")

xhat.noCIM <- cymat(bugs.model.noCIM$mean$x, country, year)
x.sd.noCIM <- cymat(bugs.model.noCIM$sd$x, country, year)
# write.est(dat, bugs.model.noCIM, "LJI-estimates-noCIM-20140422.csv")

# compare estimates of xhat, excluding CIM
cor(matrix(xhat), matrix(xhat.noCIM), use="pairwise.complete.obs")
corvec <- NULL
for (i in 1:nrow(xhat)) {
  corvec <- c(corvec, cor(xhat[i, ], xhat.noCIM[i, ], use="pairwise.complete.obs"))
}
table(corvec>0.95)/200


##
## Re-fit the model without Feld and Voigt (fvfacto, y6) for robustness
y <- cbind(y1           ## keith
           , y2         ## hc
           , y3         ## ciri 
           , y4         ## xconst
           , y5         ## cim
           #      , y6         ## fvfacto
           , y7         ## laworder
           , y8         ## gcr
)

# indexing for countries and years
country <- recode(dat$cow)
year <- recode(dat$year)

# other inits for model
J <- ncol(y)
K <- apply(y, 2, max, na.rm=TRUE)
N <- nrow(y)
Mc <- length(table(country))
coffset <- as.vector(c(1, 1+cumsum(table(country))))
koffset <- as.vector(c(1, 1+cumsum(K-1)))

inits <-  function() { list(cutp.raw=c(0.3, 0.6, 
                                       0.4, 0.6, 
                                       0.2, 0.6, 
                                       0.1, 0.2, 0.35, 0.4, 0.5, 0.6, 
                                       -0.7, -0.4, -0.2, 0, 0.2, 0.4, 0.8, 
                                       #   -0.2, 0.2, 0.6, 0.8, 1.3, 
                                       -0.3, 0.1, 0.4, 0.7, 1.1, 
                                       -0.3, 0.3, 0.6, 0.9, 1.3), 
                            beta=runif(J, 0, 10), 
                            x.sd=runif(Mc, 0, 1), 
                            x=runif(N, 0, 1)) }

jagsfit.noFacto <- jags.parallel(data=c("y", "J", "K", "N", "Mc", "coffset", "koffset", "country"), 
                                 inits=inits, 
                                 parameters.to.save=c("beta", "cutp", "x", "x.sd"), 
                                 model.file="DynamicGRM.txt", 
                                 n.chains=3, 
                                 n.iter=2000)
bugs.model.noFacto <- jagsfit$BUGSoutput

#save(bugs.model.noFacto, file="bugs-3x2000.NO.FACTO.RData")
#load("bugs-3x2000.NO.FACTO.RData")

# check convergence
if (F) {
  par(mfrow=c(1, 1), ask=FALSE)
  hist(bugs.model$summary[, 8], xlab="Rhat", main="", breaks=50, col="gray")
  traceplot(bugs.model, mfrow=c(5, 7))
  traceplot(bugs.model, mfrow=c(1, 2), varname="x.sd")
}

xhat.noFacto <- cymat(bugs.model$mean$x, country, year)
x.sd.noFacto <- cymat(bugs.model$sd$x, country, year)
#write.est(dat, bugs.model.noFacto, "LJI-estimates-noFacto-20150422.csv")

# compare estimates of xhat, excluding FVFACTO
cor(matrix(xhat), matrix(xhat.noFacto), use="pairwise.complete.obs")
corvec <- NULL
for (i in 1:nrow(xhat)) {
  corvec <- c(corvec, cor(xhat[i, ], xhat.noFacto[i, ], use="pairwise.complete.obs"))
}
table(corvec>0.95)/200


## 
## Apply model to the democracy data from UDS project
## 

load("democracy1946.2008.rda")
# drops 11 obs. in 3 countries:
democracy <- democracy[!(democracy$cowcode %in% 
                           names(table(democracy$cowcode))[table(democracy$cowcode)<15]), ]
dat <- NULL
for (i in names(table(democracy$cowcode))) {
  sel <- democracy[democracy$cowcode==i, ]
  if (nrow(sel) <= diff(range(sel$year))) {
    allyears <- c(min(sel$year):max(sel$year))
    newmat <- as.data.frame(matrix(NA, length(allyears), ncol(sel)))
    names(newmat) <- names(sel)
    newmat$year <- allyears
    newmat$cowcode <- i
    newmat[allyears %in% sel$year, ] <- sel
    sel <- newmat
  }
  dat <- rbind(dat, sel)
}
dat$cowcode <- as.numeric(dat$cowcode)

# set up variables, following Pemstein et al.
y1 <- cut(dat$arat, c(-1, seq(50, 110, 10)), FALSE, right=FALSE)
y2 <- cut(dat$blm, c(-1, 0.4, 0.9, 1.1), F)
y3 <- cut(dat$bollen, c(-1, seq(10, 90, 10), 101), FALSE, right=FALSE)
y4 <- cut(dat$freedomhouse, c(-1, seq(1.5, 7.5, 0.5)), FALSE, right=FALSE)
y5 <- cut(dat$hadenius, c(-1, 1, 2, 3, 4, 7, 8, 9, 11), FALSE, right=FALSE)
y6 <- 1+dat$pacl
y7 <- 11+dat$polity
y8 <- 1+dat$polyarchy
y9 <- dat$prc
dat$vanhanen[dat$vanhanen==0] <- NA
y10 <- cut(dat$vanhanen, c(-1, 5, 10, 15, 20, 25, 30, 35, 40, 50), FALSE, right=FALSE)
y <- cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)

country <- recode(dat$cowcode)
year <- recode(dat$year)

J <- ncol(y)
K <- apply(y, 2, max, na.rm=TRUE)
N <- nrow(y)
Mc <- length(table(country))
coffset <- as.vector(c(1, 1+cumsum(table(country))))
koffset <- as.vector(c(1, 1+cumsum(K-1)))

inits <-  function() { list(cutp.raw=c(-0.11, 0.06, 0.30, 0.45, 0.64, 1.02, 0.50, 1.05, -0.49, 
                                       -0.12, 0.07, 0.23, 0.33, 0.45, 0.55, 0.64, 0.82, -0.07, 
                                       0.03, 0.12, 0.23, 0.32, 0.40, 0.48, 0.56, 0.64, 0.73, 
                                       0.84, 0.95, -1.08, -0.32, 0.23, 0.50, 0.76, 1.01, 1.43, 
                                       0.51, -1.36, -0.78, -0.58, -0.03, 0.11, 0.19, 0.25, 0.32, 
                                       0.38, 0.44, 0.51, 0.54, 0.59, 0.62, 0.70, 0.79, 0.91, 
                                       1.04, 1.24, 1.43, -0.43, -0.32, -0.12, 0.01, 0.12, 0.26, 
                                       0.42, 0.57, 0.70, 0.89, 0.57, 0.64, 0.97, 0.27, 0.45, 
                                       0.60, 0.75, 0.86, 0.98, 1.13, 1.29), 
                            beta=runif(J, 0, 10), 
                            x.sd=runif(Mc, 0, 1), 
                            x=runif(N, 0, 1)) }

jagsfit <- jags.parallel(data=c("y", "J", "K", "N", "Mc", "coffset", "koffset", "country"), 
                         inits=inits, 
                         parameters.to.save=c("beta", "cutp", "x", "x.sd"), 
                         model.file="DynamicGRM.txt", 
                         n.chains=3, 
                         n.iter=2000)
bugs.uds <- jagsfit$BUGSoutput

save(bugs.uds, file="bugs-uds-3x2000.RData")
# load("bugs-uds-3x3000.RData")

# check convergence
if (F) {
  par(mfrow=c(1, 1), ask=FALSE)
  hist(bugs.uds$summary[, 8], xlab="Rhat", main="", breaks=50, col="gray")
  traceplot(bugs.uds, mfrow=c(2,3))
}

xhat <- cymat(bugs.uds$mean$x, country, year)
x.sd <- cymat(bugs.uds$sd$x, country, year)

res <- data.frame(country=dat$country, 
                  ccode=dat$cowcode, 
                  year=dat$year, 
                  x=round(bugs.uds$mean$x, 4), 
                  post.sd=round(bugs.uds$sd$x, 4))
# write.csv(res, "UDS-estimates.csv", row.names=FALSE)

## Verify model fit
vars <- names(dat)[c(4, 5, 6, 7, 8, 11, 12, 13, 14, 15)]
cutp.list <- list()
for (j in 1:J) {
  cutp.list[[j]] <- as.matrix(bugs.uds$sims.list$cutp[, koffset[j]:(koffset[j+1]-1)])
}

nsims <- 1000
resamp <- sample(c(1:bugs.uds$n.sims), nsims)
sim.tab <- list(y1=NULL, y2=NULL, y3=NULL, y4=NULL, y5=NULL, 
                y6=NULL, y7=NULL, y8=NULL, y9=NULL, y10=NULL)
pb <- txtProgressBar(min=1, max=nsims, style=3)
for (r in 1:nsims) {      # redraw parameter estimtes
  iter <- resamp[r]
  setTxtProgressBar(pb, r)
  for (i in 1:J) {        # loop through items
    sim.vals <- NULL
    for (j in which(!is.na(y[, i]))) {    # loop through observed country-years
      Qvec <- plogis((cutp.list[[i]][iter, ]-bugs.uds$sims.list$x[iter, j]) *
                       bugs.uds$sims.list$beta[iter, i])
      pvec <- Qvec[1]
      if (length(Qvec)>1) {
        for (k in 2:length(Qvec)) { pvec <- c(pvec, Qvec[k]-Qvec[k-1]) }
        pvec <- c(pvec, 1-sum(pvec))
      } else {
        pvec <- c(Qvec, 1-Qvec)
      }
      sim.vals <- c(sim.vals, which(rmultinom(1, 1, pvec)==1))
    }
    sim.tab[[i]] <- rbind(sim.tab[[i]], table(sim.vals))
  }
}

quartz(width=12, height=6)
par(mfrow=c(2, 5), mar=c(4, 4, 2, 0.5), las=1)
for (i in 1:J) {
  hist(y[, i], breaks=seq(-0.5, K[i]+0.5, 1), xlim=c(0, K[i]+1), ylim=c(0, max(table(y[, i]))*1.25), 
       col="gray", xaxt="n", xlab="Rating Level\n", ylab="Rating Frequency", main=vars[i])
  axis(1, c(1:K[i]), c(1:K[i]))
  points(c(1:K[i]), apply(sim.tab[[i]], 2, mean), pch=19)
  segments(c(1:K[i]), apply(sim.tab[[i]], 2, quantile, 0.025), c(1:K[i]), apply(sim.tab[[i]], 2, quantile, 0.975))
  cat("\n\n", i, vars[i], "\n")
  print(round(rbind(apply(sim.tab[[i]], 2, quantile, 0.9755), table(y[, i]), apply(sim.tab[[i]], 2, quantile, 0.025))))
  for (j in 1:K[i]) {
    segments(j-0.15, apply(sim.tab[[i]], 2, quantile, 0.025)[j], j+0.15, apply(sim.tab[[i]], 2, quantile, 0.025)[j])
    segments(j-0.15, apply(sim.tab[[i]], 2, quantile, 0.975)[j], j+0.15, apply(sim.tab[[i]], 2, quantile, 0.975)[j])
  }
}


## Compare to published UDS estimates
uds <- read.csv("uds_summary.csv")

# merge data sets
dat$xhat <- bugs.uds$mean$x
uds.merge <- merge(dat, uds, by=c("cowcode", "year"), all=TRUE)
uds.merge$country.y <- NULL
uds.merge$median <- NULL
uds.merge$sd <- NULL
uds.merge$pct025 <- NULL
uds.merge$pct975 <- NULL
names(uds.merge)[names(uds.merge)=="country.x"] <- "country"
names(uds.merge)[names(uds.merge)=="mean"] <- "UDS"

min.uds <- min(uds.merge$UDS, na.rm=TRUE)
max.uds <- max(uds.merge$UDS, na.rm=TRUE)

# rescaled mean of 10 indicators
uds.merge$mean.raw <- rowMeans(cbind(rescale01(uds.merge$arat, 29, 109), 
                                     rescale01(uds.merge$blm, 0, 1), 
                                     rescale01(uds.merge$bollen, 0, 100), 
                                     rescale01(uds.merge$freedomhouse, 1, 7), 
                                     rescale01(uds.merge$hadenius, 0, 10), 
                                     rescale01(uds.merge$pacl, 0, 1), 
                                     rescale01(uds.merge$polity, -10, 10), 
                                     rescale01(uds.merge$polyarchy, 0, 10), 
                                     rescale01(uds.merge$prc, 1, 4), 
                                     rescale01(uds.merge$vanhanen, 0.1, 49)), na.rm=TRUE)

# Calculate correlations between different estimators
cor(uds.merge$xhat, uds.merge$UDS, use="pairwise.complete.obs") # 0.98
cor(uds.merge$mean.raw, uds.merge$UDS, use="pairwise.complete.obs") # 0.96
cor(uds.merge$mean.raw, uds.merge$xhat, use="pairwise.complete.obs") # 0.97

corvec <- NULL
for (i in names(table(uds.merge$cowcode))) {
  corvec <- c(corvec, cor(uds.merge$xhat[uds.merge$cowcode==i], uds.merge$UDS[uds.merge$cowcode==i], use="pairwise.complete.obs"))
}
median(corvec, na.rm=TRUE)

# Scatterplot to compare differences in detail
quartz(width=6, height=5)
par(mar=c(4.5, 4, 1, 1), las=1)
plot(uds.merge$xhat~uds.merge$UDS, xlab="Unified Democracy Score ", ylab="Dynamic Bounded GRM", 
     bty="n", cex=0.4, cex.axis=1.1, cex.lab=1.2)
if (saveplots) { dev.copy2pdf(file="UDS-comparison.pdf") }


## Make individual country plots
quartz(width=4, height=4)
par(mar=c(3, 3, 3, 1), mfrow=c(1, 1), las=1, ask=FALSE)
yr <- as.numeric(names(table(dat$year)))
toplot <- c("Switzerland", "Portugal", "Syria", "Pakistan")
for (i in toplot) {
  plot(0, col="white", xlim=c(min(yr), max(yr)), ylim=c(-0.1, 1.1), xlab="", ylab="", 
       main=i, bty="n", cex.main=1.5, cex.axis=1)
  sel <- uds.merge[uds.merge$country==i & !is.na(uds.merge$country), ]
  k <- mean(country[dat$country==i], na.rm=TRUE)
  lines(sel$year, sel$mean.raw)
  lines(sel$year, rescale01(sel$UDS, min.uds, max.uds), col="gray50", lwd=3)
  lines(sel$year, bugs.uds$mean$x[coffset[k]:(coffset[k+1]-1)], col="black", lwd=3)
  if (saveplots) { dev.copy2pdf(file=paste("trendfit-UDS-", i, ".pdf", sep="")) }
}


# Plot observed versus latent estimates for all countries
# SCALED ESTIMATES: BLACK, THICK
# VARIABLE MEANS: THIN
quartz(width=8, height=11)
par(mfrow=c(8, 5), mar=c(2.5, 2, 1, 0.5), las=1, ask=FALSE, 
    cex.axis=0.6, cex.main=0.9) # 40 per page
yr <- as.numeric(names(table(dat$year)))
idx <- 1
plotnum <- 1
for (i in as.numeric(names(table(uds.merge$cowcode)))) {
  plot(0, col="white", xlim=c(min(yr), max(yr)), ylim=c(-0.1, 1.1), 
       xlab="", ylab="", main=uds.merge$country[uds.merge$cowcode==i][1], bty="n")
  sel <- uds.merge[uds.merge$cowcode==i, ]
  lines(sel$year, sel$mean.raw)
  lines(sel$year, sel$xhat, col="black", lwd=2)
  lines(sel$year, rescale01(sel$UDS, min.uds, max.uds), col="magenta", lwd=2)
  if ((idx/40 == round(idx/40)) | (idx==197)) {
    if (saveplots) { dev.copy2pdf(file=paste("alltrends-UDS", plotnum, ".pdf", sep="")) }
    plotnum <- plotnum+1
  }
  idx <- idx+1
}


# end of file.
