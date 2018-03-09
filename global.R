# Global function for the shiny app in conjunction with Modelling With Impact
# As presented 27 Feb, 2018, by Craig Savage
#
# Note that highcharts is not free for commercial or government use.

## Packages ----
library(shiny)
library(highcharter)
library(purrr)
library(dplyr)
library( lubridate )

## Load the data (see forecastMacro.R) ----
load( './baseMacro.RData' )

## Functions for feedback of highchart to server back-end ----
fnUnemp <- "function(){
console.log('Category: ' + this.category + ', value: ' + this.y + ', series: ' + this.series.name);
ds = this.series.data.map(function(e){ return {x: e.x, y: e.y  }  }); 
Shiny.onInputChange('hcUnemp', {category: this.category, name: this.series.name, data: ds, type: this.series.type})
}"

fnWage <- "function(){
console.log('Category: ' + this.category + ', value: ' + this.y + ', series: ' + this.series.name);
ds = this.series.data.map(function(e){ return {x: e.x, y: e.y  }  }); 
Shiny.onInputChange('hcWage', {category: this.category, name: this.series.name, data: ds, type: this.series.type})
}"

fnGDP <- "function(){
console.log('Category: ' + this.category + ', value: ' + this.y + ', series: ' + this.series.name);
ds = this.series.data.map(function(e){ return {x: e.x, y: e.y  }  }); 
Shiny.onInputChange('hcGDP', {category: this.category, name: this.series.name, data: ds, type: this.series.type})
}"


## Functions ----

# Update the VAR based on the updated data
updateVAR <- function( ecoData ){
  # See what the best lag number is for the macroData
  bestVar <- VARselect( ecoData %>% dplyr::select( -Date ), type='both', lag.max=10 )
  # Use the AIC as the criterion for the lag
  myP <- as.numeric( bestVar$selection[1] )
  
  # Hopefully, you found the best lag was 4. Get a model for that....
  myVar <- VAR( ecoData %>% dplyr::select( -Date ), p=myP, type='both' )
  
  return( myVar )
}

# The predictions from vars::predict can be a bit inconvenient.
# This extracts only the forecast information into a data frame
predict2forecastDF <- function( predictedData, startDate ){
  forecastDF <- data.frame( Date=startDate %m+% months( seq( from=3, by=3, to=60 ) )
                            , wageGrowth=predictedData$fcst$wageGrowth[,1]
                            , deltaGDP=predictedData$fcst$deltaGDP[,1]
                            , unempRate=predictedData$fcst$unempRate[,1] )
  
  return( forecastDF )
}

## Generate initial forecast ----
# Get the last bit of data, and increment it for next quarter
nextDate <- max( macroData1$Date ) %m+% months( 3 )
lastRow <- macroData1[nrow( macroData1 ), ]

nextData <- data.frame( Date=nextDate, unempRate=lastRow$unempRate, wageGrowth=lastRow$wageGrowth, deltaGDP=lastRow$deltaGDP )

# Update the next quarter's data based on the VAR alone
updateData <- bind_rows( macroData1, nextData )
varObject <- updateVAR( updateData )
predictData <- predict( varObject, 20 )
forecastData <- predict2forecastDF( predictData, nextDate )

## Setup reactive values for inclusion in additional processing/debugging ----
highRV <- reactiveValues( nextData=nextData
                          , varObject=varObject
                          , forecastData=forecastData )

