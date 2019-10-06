library(dplyr)
library(plotly)
library(DT)

## Collection of support functions for filtering NOAA LCD records
## down to match GHCN daily totals

summaryFun <- function(hourlyMod) {
    ######################################################################
    # Print site nubmer from lcd and cdo 
    print(paste("Site: ", lcd$id[1]))
    print(paste("Site: ", cdo$STATION[1]))
    print(paste("Site: ", cdo$STATION_NAME[1]))
    print("......................................")

    ######################################################################
    ## GHCN data
    date1 <- daily$dt[min(which(daily$dt %in% dat1$Date))]
    date2 <- daily$dt[max(which(daily$dt %in% dat1$Date))]
    fullDates <- seq.Date(date1, date2, by="day")

    useDaily <- daily %>%
        filter(dt<as.Date("2019-01-01"),
               dt>=min(hourlyMod$Date, na.rm=TRUE))
    
    missingDates <- useDaily$dt[which(!(fullDates %in% useDaily$dt))]
    naDates <- useDaily$dt[which(is.na(useDaily$pcpIn))]
    print(paste("GHCN Records to use:", date1, "-", date2))
    print(paste("GHCN days not present in records:", length(missingDates)))
    print(paste("GHCN days with NA records:", length(naDates)))
    print("......................................")

    ######################################################################
    ## Sums/houlryMod condition rel to GHCN
    ghcnSum <- sum(useDaily$pcpIn, na.rm=TRUE)
    hrSum <- sum(hourlyMod$pcp, na.rm=TRUE)
    totDiff <- hrSum-ghcnSum

    print(paste("Daily/GHCN Sum:", round(ghcnSum,1)))
    print(paste("Hourly LCD/CDO Sum:", round(hrSum,1)))

    if (totDiff<0) {
        print(paste("Mising from Hourlies:", round(totDiff,1), "inches"))
    } else if (totDiff>0) {
        print(paste("Extra Precip in Hourlies:", round(totDiff,1), "inches"))
    } else {
        print("No difference!")
    }

    ######################################################################
    ## Do annual  diffs, return summary level info

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

    nYears <- length(which(finalTab$absAnnSum>=0.25))
    listYears <- finalTab$yr.y[which(finalTab$absAnnSum>=0.25)]

    print(paste("Number of yrs with 0.25 inch discrep:", nYears))
    print("Yrs with 0.25 inch discrep:...")
    print(listYears)

    ######################################################################
    ## Lastly, do number of days with 0.10" discrps

    ## Group hourly data in to a year daily table
    hrData <- hourlyMod %>%
        group_by(Date) %>%
        summarize(dailyHrSum=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()
    
    ## Then merge hourly date totals with gncn date totals
    newTab <- merge(hrData, daily, by.x="Date", by.y="dt", all=TRUE)
    newTab$DailyDiff <- newTab$pcpIn-newTab$dailyHrSum
    
    ## Sort by abs value and return
    newTab$AbsDailyDiff <- abs(newTab$DailyDiff)
    newTab <- newTab[order(newTab$AbsDailyDiff, decreasing=TRUE),]

    ## Summary and print
    nDays <- length(which(newTab$AbsDailyDiff>=0.05))
    top15 <- newTab[1:15,c("Date","DailyDiff")]

    print(paste("Number of days with 0.05 inches discrep:",nDays))
    print("Top 15:")
    top15
    
    
    ## This shoudl return
    ## - Start date, end date, days missing in GHCN (rel to earliest hourly)
    ## - Sum of dailies, sum of hourlies, diff
    ## - List of years with ann diff >0.25"
    ## - # of days with daily diff >0.10"

    

}



## fun to take final hourly data set and compute hr/daily discrepencies by year,
## write out to a csv that i cna add notes to / cross check
reportFun <- function(hourlyMod) {

    ## Group hourly data in to a year daily table
    hrData <- hourlyMod %>%
        group_by(Date) %>%
        summarize(dailyHrSum=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()
    
    ## Then merge hourly date totals with gncn date totals
    newTab <- merge(hrData, daily, by.x="Date", by.y="dt", all=TRUE)
    newTab$DailyDiff <- newTab$pcpIn-newTab$dailyHrSum
    
    ## Sort by abs value and return
    newTab$AbsDailyDiff <- abs(newTab$DailyDiff)

    newTabSort <- newTab %>%
        mutate(yr=as.numeric(format(Date, "%Y"))) %>%
        group_by(yr) %>%
        arrange(desc(AbsDailyDiff)) %>%
        as.data.frame()
    

    newTab <- newTab[order(newTab$AbsDailyDiff, decreasing=TRUE),]

    write.csv(newTabSort[which(newTabSort$AbsDailyDiff>=0.05),],
              file="reportTest.csv")
    return(newTabSort[which(newTabSort$AbsDailyDiff>=0.05),c(1,2,4,5,8)])
    ## head(newTabSort[,c(1,2,4,5,8)],40)
    ## head(newTab[,c(1,2,4,5,8)],40)
}


## Minutes after the hour analysis function 
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

## New fun inspired by Atl 1996, lots of small diffs.
## return table of daily diffs for given calendar year
dailyDiffsTable <- function(hourlyMod, year) {

    ## filter and group hourly data in to a year specifici daily table
    hrData <- hourlyMod %>%
        mutate(yr=as.numeric(format(Date, "%Y"))) %>%
        filter(yr==year) %>%
        group_by(Date) %>%
        summarize(dailyHrSum=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()

    ## Filter daily down to year of interst only 
    dailyF <- daily %>%
        filter(yr==year)
    
    ## Then merge hourly date totals with gncn date totals
    newTab <- merge(hrData, dailyF, by.x="Date", by.y="dt", all=TRUE)
    newTab$DailyDiff <- newTab$pcpIn-newTab$dailyHrSum
    
    ## Sort by abs value and return
    newTab$AbsDailyDiff <- abs(newTab$DailyDiff)
    newTab <- newTab[order(newTab$AbsDailyDiff, decreasing=TRUE),]
    head(newTab[,c(1,2,4,5,8)],60)

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
        filter(yr==year) %>%
        select(DATE,REPORT_TYPE,SOURCE,dtLoc,pcp,pcpFlag,
               HourlyPrecipitation, min)
    cdoCur <- cdo %>%
        filter(yr==year) %>%
        select(STATION,HPCP,Measurement.Flag,
               Quality.Flag, dt, dtLoc, min, hrMin)
    

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



## Print sorted table to console of annual discrepencies
tableFun <- function(hourlyMod) {

    mm <- merge(dailyTab, hourlyMod, all=TRUE)
    colnames(mm)<- c("yr", "daily" ,"hourly")
    mm$Diff <- mm$hourly-mm$daily
    mm <- mm[order(mm$Diff, decreasing=TRUE),]
    head(mm,20)
}
