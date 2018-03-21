server = function(input, output) {
  
  
  output$highUnemp <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series(data = macroData1, type = "line"
                    , hcaes( x=Date, y=unempRate ) 
                    , name = "Unemployment"
                    , colorIndex=1) %>%
      hc_add_series( data=nextData, type='scatter'
                     , hcaes( x=Date, y=unempRate )
                     , name='Future'
                     , draggableY=TRUE ) %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS(fnUnemp),
              drop = JS(fnUnemp)
            )
          )
        )
      ) %>%
      hc_tooltip( enabled = FALSE ) %>%
      hc_yAxis( format="{round(value,4)}") %>%
      hc_xAxis( dataTimeLabelFormats='month' )
    
    
    hc 
    
  })
  observeEvent( input$hcUnemp, {
    
    inputaux <- input$hcUnemp
    
    if ( !is.null(inputaux) ){
      inputaux$data <- map_df(inputaux$data
                              , as_data_frame)
      
      highRV$nextData$unempRate <- inputaux$data$y
      print( highRV$nextData )
      
      updateData <- bind_rows( macroData1, highRV$nextData )
      print( tail( updateData ) )
      
      highRV$varObject <- updateVAR( updateData )
      print( tail( highRV$varObject$y ))
      predictData <- predict( highRV$varObject, n.ahead=20 )
      highRV$forecastData <- predict2forecastDF( predictData
                                                 , max( nextData$Date ) %m+% months(3) )
    }
    
  })
  
  output$highUnemp2 <- renderHighchart({
    
    
    hc1 <- highchart() %>%
      hc_add_series(data = macroData1
                    , hcaes( x=Date, y=unempRate )
                    , type = "line"
                    , name = "Historical"
                    , colorIndex=1 ) %>%
      hc_add_series( data=highRV$nextData
                     , hcaes( x=Date, y=unempRate )
                     , type="line"
                     , name="Input"
                      ) %>%
      hc_add_series( data=highRV$forecastData
                     , hcaes( x=Date, y=unempRate )
                     , type='line'
                     , name='Forecast Unemployment' ) %>%
      hc_tooltip( enabled = FALSE ) %>%
      hc_yAxis( format="{round(value,4)}")
    
    
    
    
    hc1
    
  })
  
  output$highWage <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series(data = macroData1, type = "line"
                    , hcaes( x=Date, y=wageGrowth ) 
                    , name = "Wage Growth"
                    , colorIndex=2 ) %>%
      hc_add_series( data=nextData, type='scatter'
                     , hcaes( x=Date, y=wageGrowth )
                     , name='Future Wage Growth'
                     , draggableY=TRUE ) %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS(fnWage),
              drop = JS(fnWage)
            )
          )
        )
      ) %>%
      hc_tooltip( enabled=FALSE ) %>%
      hc_yAxis( format="{round(value,4)}")
    
    
    
    hc 
    
  })
  
  observeEvent( input$hcWage, {
    
    inputaux <- input$hcWage
    
    if ( !is.null(inputaux) ){
      inputaux$data <- map_df(inputaux$data
                              , as_data_frame)
      
      highRV$nextData$wageGrowth <- inputaux$data$y
      print( highRV$nextData )
      
      updateData <- bind_rows( macroData1, highRV$nextData )
      
      highRV$varObject <- updateVAR( updateData )
      
      predictData <- predict( highRV$varObject, 20 )
      highRV$forecastData <- predict2forecastDF( predictData
                                                 , max( nextData$Date ) %m+% months(3) )
    }
    
  })
  
  output$highWage2 <- renderHighchart({
    
    hc1 <- highchart() %>%
      hc_add_series(data = macroData1
                    , hcaes( x=Date, y=wageGrowth )
                    , type = "line"
                    , name = "Historical Wage Growth"
                    , colorIndex=2) %>%
      hc_add_series( data=highRV$nextData
                     , hcaes( x=Date, y=wageGrowth )
                     , type="line"
                     , name="Input"
      ) %>%
      hc_add_series( data=highRV$forecastData
                     , hcaes( x=Date, y=wageGrowth )
                     , type='line'
                     , name='Forecast Wage Growth' ) %>%
      hc_tooltip( enabled=FALSE ) %>%
      hc_yAxis( format="{round(value,4)}")
    
    
    
    
    hc1
    
  })
  
  
  output$highGDP <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series(data = macroData1, type = "line"
                    , hcaes( x=Date, y=deltaGDP ) 
                    , name = "GDP" 
                    , colorIndex=3) %>%
      hc_add_series( data=nextData, type='scatter'
                     , hcaes( x=Date, y=deltaGDP )
                     , name='Future GDP'
                     , draggableY=TRUE ) %>%
      hc_tooltip( enabled=FALSE ) %>%
      hc_yAxis( format="{round(value,4)}") %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS(fnGDP),
              drop = JS(fnGDP)
            )
          )
        )
      ) 
    
    hc 
    
  })
  
  observeEvent( input$hcGDP, {
    inputaux <- input$hcGDP
    
    if ( !is.null(inputaux) ){
      inputaux$data <- map_df(inputaux$data
                              , as_data_frame)
      
      highRV$nextData$deltaGDP <- inputaux$data$y
      print( highRV$nextData )
      
      updateData <- bind_rows( macroData1, highRV$nextData )
      
      highRV$varObject <- updateVAR( updateData )
      
      predictData <- predict( highRV$varObject, 20 )
      highRV$forecastData <- predict2forecastDF( predictData, nextDate )
      
    }
    
  })
  
  output$highGDP2 <- renderHighchart({
    
    
    hc1 <- highchart() %>%
      hc_add_series(data = macroData1
                    , hcaes( x=Date, y=deltaGDP )
                    , type = "line"
                    , name = "Historical GDP"
                    , colorIndex=3 ) %>%
      hc_add_series( data=highRV$nextData
                     , hcaes( x=Date, y=deltaGDP )
                     , type="line"
                     , name="Input"
      ) %>%
      hc_add_series( data=highRV$forecastData
                     , hcaes( x=Date, y=deltaGDP )
                     , type='line'
                     , name='Forecast GDP' ) %>%
      hc_tooltip( enabled=FALSE ) %>%
      hc_yAxis( format="{round(value,4)}")
    
    
    
    hc1
    
  })
  
}
