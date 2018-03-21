library(shiny)
library(highcharter)
library(purrr)
library(dplyr)
library( lubridate )
library( vars )

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


## Extract 
nextDate <- max( macroData1$Date ) %m+% months( 3 )
lastRow <- macroData1[nrow( macroData1 ), ]

nextData <- data.frame( Date=nextDate %m+% months( seq( from=0, to=12, by=3))
                        , unempRate=lastRow$unempRate
                        , wageGrowth=lastRow$wageGrowth
                        , deltaGDP=lastRow$deltaGDP )


## Functions ----
updateVAR <- function( ecoData ){
  # See what the best lag number is for the macroData
  bestVar <- VARselect( ecoData %>% dplyr::select( -Date ), type='both', lag.max=10 )
  # Use the AIC as the criterion for the lag
  myP <- as.numeric( bestVar$selection[1] )
  
  # Hopefully, you found the best lag was 4. Get a model for that....
  myVar <- VAR( ecoData %>% dplyr::select( -Date ), p=myP, type='both' )
  
  return( myVar )
}

predict2forecastDF <- function( predictedData, startDate ){
  forecastDF <- data.frame( Date=startDate %m+% months( seq( from=3, by=3, to=60 ) )
                            , wageGrowth=predictedData$fcst$wageGrowth[,1]
                            , deltaGDP=predictedData$fcst$deltaGDP[,1]
                            , unempRate=predictedData$fcst$unempRate[,1] )
  
  return( forecastDF )
}

## Generate initial forecast ----
updateData <- bind_rows( macroData1, nextData )
varObject <- updateVAR( updateData )
predictData <- predict( varObject, 20 )
forecastData <- predict2forecastDF( predictData, max( nextData$Date ) %m+% months(3) )

highRV <- reactiveValues( nextData=nextData
                          , varObject=varObject
                          , forecastData=forecastData )

