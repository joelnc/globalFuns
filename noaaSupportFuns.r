library(dplyr)
library(plotly)
library(DT)

## Collection of support functions for filtering NOAA LCD records
## down to match GHCN daily totals


## Minutes after the hour analysis function
## 
minFun <- function(hourlyMod, year) {

    bb <- filter(hourlyMod, yr==year)

    ## Plotly hist of minutes with axis spec'd to 0,59
    p1 <- plot_ly() %>%
        add_trace(data=bb, x=~min,type="histogram",
                  text=bb$dtLoc, hoverinfo="text")
    p1

    ## Barplot with most freqent as one color, next most frqeq another, all others brigth
    v1 <- as.numeric(names(sort(table(bb$min),decreasing=TRUE)[1]))
    v2 <- as.numeric(names(sort(table(bb$min),decreasing=TRUE)[2]))
    v3 <- as.numeric(names(sort(table(bb$min),decreasing=TRUE)[3]))

    darkBlue <- "rgb(0,0,102)"
    darkGreen <- "rgb(0,103,51)"
    darkPurp <- "rgb(102,0,102)"
    ltGn <- "rgb(204,229,255)"
    ltBl <- "rgb(153,204,255)"
    ltRd <- "rgb(255,102,102)"
    
    p1 <- plot_ly() %>%
        add_trace(data=bb[which(bb$min==v1),], x=~dtLoc,
                  y=~min, type="bar", marker=list(color=ltGn),
                  text=bb$dtLoc[which(bb$min==v1)],
                  hoverinfo="text", name="1st") %>%
        add_trace(data=bb[which(bb$min==v2),], x=~dtLoc,
                  y=~min, type="bar", marker=list(color=ltBl),
                  text=bb$dtLoc[which(bb$min==v2)],
                  hoverinfo="text", name="2nd") %>%
        add_trace(data=bb[which(bb$min==v3),], x=~dtLoc,
                  y=~min, type="bar", marker=list(color=ltRd),
                  text=bb$dtLoc[which(bb$min==v3)],
                  hoverinfo="text", name="3rd") %>%
        add_trace(data=bb[which(!bb$min %in% c(v1,v2,v3)),],
                  y=~min, x=~dtLoc,
                  type="bar", marker=list(color="black"),
                  text=bb$dtLoc[which(!bb$min %in% c(v1,v2,v3))],
                  hoverinfo="text", name="Other")
                
    p1

    ## Nope, use v1,v2,v3 to add an indicator column and then color bars by that column
    
    ## cumulative time plot? something to pop when not 1 hr difference


}


## Print sorted table to console of annual discrepencies
tableFun <- function(hourlyMod) {

    mm <- merge(dailyTab, hourlyMod, all=TRUE)
    colnames(mm)<- c("yr", "daily" ,"hourly")
    mm$Diff <- mm$hourly-mm$daily
    mm <- mm[order(mm$Diff, decreasing=TRUE),]
    head(mm,20)
}

## Print sorted table to console of annual discrepencies
tableFunAbs <- function(hourlyMod, sortBy) {

    ## Group adjusted hourly records by date, sum by date
    hrDaily <- hourlyMod %>%
        group_by(Date) %>%
        summarize(hourlyDailyP=sum(pcp, na.rm=TRUE)) %>%
        mutate(yr=as.numeric(format(Date, "%Y"))) %>%
        as.data.frame()

    ## Then merge hourly date totals with gncn date totals
    newTab <- merge(hrDaily, daily, by.x="Date", by.y="dt", all=TRUE)

    ## Abs daily diffs, for summing by year
    newTab$absDailyDiff <- abs(newTab$pcpIn-newTab$hourlyDailyP)

    ## Daily diffs, for identifying biggest daily diff per cal yr 
    newTab$rawDailyDiff <- newTab$pcpIn-newTab$hourlyDailyP # pos=ghcn higher

    ## Then sum of abs diffs grouped by year
    finalTab <- newTab %>%
        group_by(yr.y) %>%
        summarize(absAnnSum=sum(absDailyDiff, na.rm=TRUE)) %>% #,
                  ## annMaxDif=max(rawDailyDiff,na.rm=TRUE)) %>%
        as.data.frame()
    
    ## Initialize columns in final table for max daily discrepencies
   finalTab$maxDailyDiff <- NA
    finalTab$diffDate <- as.Date(NA)

    ## loop over table and write. Clunky, but fast enough
    for (i in 1:nrow(finalTab)) {
        ## Index rows that are of year i 
        yrIndex <- which(newTab$yr.y==finalTab$yr.y[i])

        tempTable <- newTab[yrIndex,]

        winner <- which.max(abs(tempTable$rawDailyDiff))

        if (length(winner)==0) {
            finalTab$maxDailyDiff[i] <- NA
            finalTab$diffDate[i] <- NA
        } else {
            finalTab$maxDailyDiff[i] <- tempTable$rawDailyDiff[winner]  
            finalTab$diffDate[i] <- as.Date(tempTable$Date[winner])
        }
    }
    
    ## sort and write out table
    if (missing(sortBy)) {
        finalTab <- finalTab[order(finalTab$absAnnSum, decreasing=TRUE),]
    } else if (sortBy=="byYear") {
        finalTab <- finalTab[order(finalTab$absAnnSum, decreasing=TRUE),]
    } else {
        finalTab <- finalTab[order(abs(finalTab$maxDailyDiff), decreasing=TRUE),]
    }
    
    head(finalTab,60)
}


## Pop daily diffs plotly, and write csv file for "inspect this yr"
## need to add to this to write test files with LCD and CDO for confirming
## hourly daily discrpencies...

ddfFun <- function(hourlyMod, year) {

    ## Tack a date on (why not here already?)
    useTz <- attr(hourlyMod$dtLoc[1], "tzone")
    hourlyMod$Date <- as.Date(hourlyMod$dtLoc, tz=useTz)

    ## Filter combined hourly down to yr of interst
    comb_hourly <- hourlyMod %>%
        filter(yr==year)

    ## Group sum by day/date
    comb_daily  <- comb_hourly %>%
        group_by(Date) %>%
        summarize(pcpTot=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()

    ## Filter daily down to year of interst only 
    dailyF <- daily %>%
        filter(yr==year)

    ## Also filter down pure cdo and lcd for confirming gaps, etc
    lcdCur <- lcd %>%
        filter(yr==year)
    cdoCur <- cdo %>%
        filter(yr==year)
    

    ## Colors and plotting of daily vs combined hourly
    dailyCol <- 'rgb(26,26,255)'
    lcdCol <- 'rgb(255,83,26)'

    tsP <- plot_ly() %>%
        add_trace(x=dailyF$dt,
                  y=dailyF$pcpIn,
                  type="scatter", mode="markers",
                  name="GHCN",
                  marker = list(color = dailyCol, symbol="circle")
                  ) %>%
        add_trace(x=comb_daily$Date,
                  y=comb_daily$pcpTot,
                  type="scatter", mode="markers",
                  name="HPCP/LCD",          
                  marker = list (color = lcdCol, symbol="circle")
                  )
    ## Csv file for combined yr
    write.table(comb_hourly[,1:8],
                file=paste0("NOAA/combHourly_", year,".csv"),
                sep=",", row.names=FALSE)
    ## Csv files rawish lcd and cdo
    write.table(lcdCur,
                file=paste0("NOAA/lcd_", year,".csv"),
                sep=",", row.names=FALSE)
    write.table(cdoCur,
                file=paste0("NOAA/cdo_", year,".csv"),
                sep=",", row.names=FALSE)

    
    ## Plot call
    tsP

    ## Dt table for yr?

}
