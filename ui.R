## Very basic UI for IFRS 9
#
# To be used in conjunction with Modelling with Impact, as presented 27 Feb, 2018.
#
# Copyright, Craig Savage. Feel free to use, modify, etc.
#
# highchartOutput is part of the highcharter package to interface between R and highcharts.
# Note that highcharts is not free for government or commercial use. 
#
# See:
# highcharts: https://www.highcharts.com/
# highcharter (highcharts - R interface): http://jkunst.com/highcharter/

# There really isn't much to this - it's a demo, after all...
ui <- fluidPage(
  h2("Interactive Forecasts"),
  tabsetPanel(
    tabPanel("IFRS Forecast Demo",
             # Three rows, with 2 charts side-by-side.
             fluidRow(
               column(width = 6, highchartOutput("highUnemp")),
               column(width = 6, highchartOutput("highUnemp2"))
             )
             , fluidRow(
               column(width = 6, highchartOutput("highWage")),
               column(width = 6, highchartOutput("highWage2"))
             )
             , fluidRow(
               column(width = 6, highchartOutput("highGDP")),
               column(width = 6, highchartOutput("highGDP2"))
             )
             
    )
  )
)
