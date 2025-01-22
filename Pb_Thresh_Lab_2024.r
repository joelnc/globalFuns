## Collection of funs to compute thresholds


## Lead
LeadFun <- function(hd) {
    chronT <- 25
        chron <- (1.46203-(log(hd)*0.145712))*(exp(1.273*log(hd)-4.705))
        ac <- (1.46203-(log(hd)*0.145712))*(exp(1.273*log(hd)-1.46))
        chronV <- "yes"
        acV <- "yes"
    }
    return(list(chron=chron, chronV=chronV, ac=ac, acV=acV, chronT=chronT))
}

pb <- seq(1,100)
ac <- (1.46203-(log(pb)*0.145712))*(exp(1.273*log(pb)-1.46))
