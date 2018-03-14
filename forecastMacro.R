## Forecast of vector macro-economic data

## Library load ----
# Don't load readr -- scope the functions needed
# library( readr ). Including it here so it's easy to find. 
library( lubridate )
library( vars )
library( dplyr )
library( magrittr )
library( animation )
library( shiny )

## Get data ----
# Note that Excel will sometimes save CSV data as tab-separated (TSV).
# Check your data parse/load....

# APRA Data:
#  See: http://www.apra.gov.au/adi/Publications/Pages/adi-quarterly-performance-statistics.aspx
# I've modified the normal download to:
# 1) Be organised by column rather than row
# 2) Contain fields Date | Stage1 | Stage2 | Stage3
#     a. Date is the column header
#     b. Stage1 is derived as 1-Stage2-Stage3
#     c. Stage2 is Past Due to Loans and Advances. Note I've recalculated this to get more digits of accuracy.
#     d. Stage3 is Impaired Facilities to Loans and Advances. Note I've recalculated this to get more digits of accuracy.
apraData <- readr::read_csv( 'apraData1.csv' )

# ABS Data:
#   See: http://www.abs.gov.au/ausstats/abs@.nsf/mf/1364.0.15.003
# I've modified the download to:
# 1) Cut out the header
# 2) Derive fields: wageGrowth, unempRate, deltaGDP
#   a) Wage growth: (w_{t+1}-w_t)/w_t for non-farm payroll
#   b) unempRate: Unemployed Persons/Labour Force
#   c) deltaGDP: (g_{t+1}-g_t)/g_t: Nominal GDP

macroData <- readr::read_tsv( 'ausMacroEcon.csv' )

# Downselect fields
apraData1 <- apraData %>% 
  mutate( Date=dmy( paste0( '01-', Date) ) ) %>%
  dplyr::select( Date, Stage1, Stage2, Stage3 ) %>%
  mutate( Stage1Lead=lead( Stage1 ), Stage2Lead=lead( Stage2 ), Stage3Lead=lead( Stage3 ) )

# And with macroData
macroData1 <- macroData%>%
  dplyr::select( Date, wageGrowth, deltaGDP, unempRate ) %>%
  filter( !is.na( wageGrowth ) )

## Save the data here for the shiny app...
save(macroData1, file='./baseMacro.RData' )

fullData <- inner_join( macroData1, apraData1 )

## Setup the optimisation problem ---
# Vector of probabilities -- assuming the third column derived from the other 2.
# Two-by-three matrix, read row-wise
p0 <- c( 0.99, 0.005, 0.005
         , 0.75, 0, 0.25 )

## Auxillary functions to interface with the alabama package----
# See ?alabama::auglag
# Cost function
deltaFunction <- function( p, s1, s2, s3
                           , s1Lead, s2Lead, s3Lead){
  s1Pred <- s1*p[1] + s2*p[4] 
  s2Pred <- s1*p[2] + s2*p[5]
  s3Pred <- s1*p[3] + s2*p[6] 
  
  sPred <- c( s1Pred, s2Pred, s3Pred )
  sNew <- c( s1Lead, s2Lead, s3Lead )
  
  delta <- sum( (sPred-sNew)^2 )
  
  return( delta )
}

# Inequality constraints
# Note the "..." is required as the additional inputs to deltaFunction will be passed to everything
ineqFunction <- function( p, ... ){
  # Probabilities are between zero and one
  h <- c(p, 1-p)
  
  return( h )
}

# Equality constraints
eqFunction <- function( p, ... ){
  # Probabilities sum to one
  j <- c(1-p[1]-p[2]-p[3]
         , 1-p[4]-p[5]-p[6] )
  
  return( j ) 
}

# Intialise
N <- nrow( fullData )
p11 <- rep( 0, N )
p12 <- rep( 0, N )
p13 <- rep( 0, N )
p21 <- rep( 0, N )
p22 <- rep( 0, N )
p23 <- rep( 0, N )

# Fit and record the transition probabilities
for( timeCount in 1:(N-1) ){
  r1 <- auglag( p0, deltaFunction, hin=ineqFunction, heq=eqFunction
                , s1=apraData1$Stage1[timeCount], s2=apraData1$Stage2[timeCount], s3=apraData1$Stage3[timeCount] 
                , s1Lead=apraData1$Stage1Lead[timeCount], s2Lead=apraData1$Stage2Lead[timeCount], s3Lead=apraData1$Stage3Lead[timeCount])
  
  p11[timeCount] <- r1$par[1]
  p12[timeCount] <- r1$par[2]
  p13[timeCount] <- r1$par[3]
  p21[timeCount] <- r1$par[4]
  p22[timeCount] <- r1$par[5]
  p23[timeCount] <- r1$par[6]
}

# Bind the predictions to fulldata
pMatrix <- data.frame( P11=p11, P12=p12, P13=p13, P21=p21, P22=p22, P23=p23 )
fullData1 <- bind_cols( fullData, pMatrix )

## Fit regression models to the transition probabilities ----
# Note I'm using glm without a link function, so it defaults to linear models
# You may wish to use alternate regressions (e.g. logistic) so that probabilities are bound between 0 and 1, etc.
p11Model <- glm( P11 ~ wageGrowth + deltaGDP + unempRate, data=fullData1 )
p12Model <- glm( P12 ~ wageGrowth + deltaGDP + unempRate, data=fullData1 )
# P13 will be 1-p11-p12
p21Model <- glm( P21 ~ wageGrowth + deltaGDP + unempRate, data=fullData1 )
p22Model <- glm( P22 ~ wageGrowth + deltaGDP + unempRate, data=fullData1 )
# P23 will be 1-p21-p22


## Need to run the shiny app to get nextData, based on the highcharter update ----
# Run the app -- I've assumed the files are in this directory!
runApp()
# Note that sometimes the script keeps going without waiting for the shiny to finish. You may need to restart from here.
# It worked for me (but I had already run it!) your results might be different...
stopifnot( exists( highRV ) )

# After that's working, keep going!
macroData2 <- bind_rows( macroData1, isolate( highRV$nextData ) )
myVar <- isolate( highRV$myVar )
# Get the forecast data from the shiny app
nextData <- isolate( highRV$nextData )
forecastData <- isolate( highRV$forecastData )
forecastData1 <- bind_rows( nextData, forecastData )

# Include the predictions for stage transitions for all future quarters
forecastData1 %<>% mutate(
  p11=predict( p11Model, newdata=. )
  , p12=predict( p12Model, newdata=. )
  , p13=1-p11-p12
  , p21=predict( p21Model, newdata=. )
  , p22=predict( p22Model, newdata=. )
  , p23=1-p21-p22
  , Stage1=0
  , Stage2=0
  , Stage3=0 )

# Start with the amount in Stage 2 for lifetime loss estimates
lifeLoss <- fullData1[N, 'Stage2']

# The first time period will evolve according to the probability distribution (everything is coming from Stage 2)
forecastData1[1,'Stage1'] <- forecastData1[1,'p21']
forecastData1[1,'Stage2'] <- forecastData1[1,'p22']
forecastData1[1,'Stage3'] <- forecastData1[1,'p23']
# Do the rest of the predictions
for( predCount in 2:nrow( forecastData1 ) ){
  forecastData1[predCount, 'Stage1'] <- forecastData1[predCount-1,'Stage1']*forecastData1[predCount,'p11'] +
    forecastData1[predCount-1,'Stage2']*forecastData1[predCount,'p21']
  forecastData1[predCount, 'Stage2'] <- forecastData1[predCount-1,'Stage1']*forecastData1[predCount,'p12'] +
    forecastData1[predCount-1,'Stage2']*forecastData1[predCount,'p22']
  forecastData1[predCount, 'Stage3'] <- forecastData1[predCount-1,'Stage1']*forecastData1[predCount,'p13'] +
    forecastData1[predCount-1,'Stage2']*forecastData1[predCount,'p23']
}


plotData <- forecastData1 %>% mutate( cumLoss=0.4*cumsum( Stage3 ) ) %>% dplyr::select( Date, Stage2, Stage3, cumLoss ) %>% gather( Key, Value, -Date ) %>%
  mutate( plotKey=factor( Key, levels=c('Stage2', 'Stage3', 'cumLoss'), ordered=TRUE ) )

dateVec <- unique( plotData$Date )

## Make an animation of the Stage distribution ----
# Requires some additional software - see ?ani.record
# Disable the animation recorder
if( FALSE ){
  # Initialise the recorder
  ani.record( reset=TRUE )
  
  # Loop through making plots
  # Note: You may need to run this code interactively: there are some quirks to the plot flushing if it's run within the script
  for( dateCount in 1:length( dateVec ) ){
    # Select one quarter at a time
    thisDate <- dateVec[dateCount]
    thisPlotData <- plotData[plotData$Date == thisDate, ]
    # Make the plot
    thisPlot <- ggplot( thisPlotData ) +
      geom_bar( aes( x=plotKey, y=Value, fill=plotKey ), stat='identity' ) +
      theme( legend.position = 'none' ) +
      ggtitle( thisDate ) +
      ylim( 0, max( plotData$Value )  )
    
    # Display the plot
    thisPlot
    # Record the plot
    ani.record()
  }
  
  # See the saveSWF help file -- additional (free) software may be needed
  # There's then a bit of configuration (e.g. registering SWF files to a player)
  saveSWF( ani.replay(), swf.name = 'Impact1.swf' )
}

## Calculate the lifetime expected loss ----
lossData <- plotData %>% filter( Key=='Stage3' ) %>% 
  arrange( Date ) %>% 
  mutate( fvLoss=0.4*Value # 0.4 is the LGD value (estimated)
          , pvDiscount=exp(-0.01*row_number() ) # 0.01 is the quarterly discount rate I'm using 
          , pvLoss=fvLoss*pvDiscount )

# Final impact calculation
ifrsLoss <- lossData %>%
  summarise( Loss=sum( pvLoss )*lifeLoss*2505019 ) # 2505019 is the last gross loans from apraData

# Final impact
ifrsLoss

#### End ----