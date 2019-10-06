library(dplyr)
library(plotly)
library(DT)

## Collection of support functions for filtering NOAA LCD records
## down to match GHCN daily totals


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
tableFun <- function(hourlyMod) {

    mm <- merge(dailyTab, hourlyMod, all=TRUE)
    colnames(mm)<- c("yr", "daily" ,"hourly")
    mm$Diff <- mm$hourly-mm$daily
    mm <- mm[order(mm$Diff, decreasing=TRUE),]
    head(mm,20)
}

## Print sorted table to console of abs annual discrepencies
tableFunAbs <- function(hourlyMod) {

    ## Need to sum lcdMod2 by date,
    lcdDaily <- hourlyMod %>%
        group_by(Date) %>%
        summarize(hourlyDailyP=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()

    ## Then merge hourly date totals with gncn date totals
    newTab <- merge(lcdDaily, daily, by.x="Date", by.y="dt", all=TRUE)
    newTab$absDailyDiff <- abs(newTab$pcpIn-newTab$hourlyDailyP)

    ## then sum of abs diffs grouped by year
    finalTab <- newTab %>%
        group_by(yr) %>%
        summarize(absSum=sum(absDailyDiff,na.rm=TRUE)) %>%
        as.data.frame()

    finalTab <- finalTab[order(finalTab$absSum, decreasing=TRUE),]
    head(finalTab,40)
}


## Pop daily diffs plotly, and write csv file for "inspect this yr"
ddfFun <- function(hourlyMod, year) {

    useTz <- attr(hourlyMod$dtLoc[1], "tzone")
    hourlyMod$Date <- as.Date(hourlyMod$dtLoc, tz=useTz)

    ## plotly daily diffs for yr
    lcd_hourly <- hourlyMod %>%
        filter(yr==year)

    lcd_daily  <- lcd_hourly %>%
        group_by(Date) %>%
        summarize(pcpTot=sum(pcp, na.rm=TRUE)) %>%
        as.data.frame()

    
    dailyF <- daily %>%
        filter(yr==year)

    dailyCol <- 'rgb(26,26,255)'
    lcdCol <- 'rgb(255,83,26)'

    tsP <- plot_ly() %>%
        add_trace(x=dailyF$dt,
                  y=dailyF$pcpIn,
                  type="scatter", mode="markers",
                  name="GHCN",
                  marker = list(color = dailyCol, symbol="circle")
                  ) %>%
        add_trace(x=lcd_daily$Date,
                  y=lcd_daily$pcpTot,
                  type="scatter", mode="markers",
                  name="LCD",          
                  marker = list (color = lcdCol, symbol="circle")
                  )
    ## Csv file for yr
    write.table(lcd_hourly[,1:8], file="NOAA/test.csv",
                sep=",", row.names=FALSE)

    ## Plot call
    tsP

    ## Dt table for yr?

}
