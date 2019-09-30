library(dplyr)

## Fun to round up in base 10
log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}

## fun to round down in base 10
log10_floor <- function(x) {
    10^(floor(log10(x)))
}

## New fun to return only the base 10 major ticks
logMajor <- function(parUsr3, maxY) {

    ## First get floor of par(1)
    startAt <- log10_ceiling(parUsr3)
    
    ## then do a sequence as in orign fun
    majSeq <- startAt*(10^seq(0,11))

    ## then clip down to only those less than axis max
    majSeq <- majSeq[majSeq<=(log10_floor(maxY)+1) &
                     majSeq>(log10_floor(parUsr3)*1.01)]
    
    ## return
    return(majSeq)
}

## New fun to return only the base 10 major ticks
logMinor <- function(parUsr3) {

    startAt <- log10_floor(parUsr3)

    minSeq <- startAt*(10^seq(0,10))

    minorAppend <- NULL

    for (i in 1:10) {
        temp <- minSeq[i]*seq(1:9)
        minorAppend <- rbind(minorAppend, temp)
        rm(temp)
    }

    return(minorAppend)
}


## Orig
## Function to spit out minor ticks for log plot
logSpace <- function(stAt, OsoM) {

    c <- stAt*(10^seq(0,OsoM-1))

    rs <- NULL

    for (i in 1:OsoM) {
        temp <- c[i]*seq(1:9)
        rs <- rbind(rs,temp)
        rm(temp)
    }

    return(sort(matrix(rs, nrow=(9*OsoM), ncol=1)))
}
