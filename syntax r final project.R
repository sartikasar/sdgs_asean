library(shiny)
library(forecast)

# Define UI ----
ui <- fluidPage(
  titlePanel("Sustainable Development Goals Report ASEAN : No Poverty"),
  
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      h4('Indicator for Goal No Poverty'),
      radioButtons(inputId='indic', label = "Choose Indicator for Poverty Headcount Ratio", 
                   choices = c("$1.9/days", "$3.2/days")),
      h4('Country in ASEAN'),
      selectInput(inputId = 'country', label = "Select Country", choices = c("All", "Brunei Darussalam", "Cambodia",
                                                                             "Indonesia", "Lao PDR(Laos)", "Malaysia", 
                                                                             "Myanmar", "Philippines", "Singapore",
                                                                             "Thailand", "Vietnam"))
      #radioButtons(inputId = "mod", label = "Choose Year", choices = c("y1", "y2"), choiceValues = "")
    ),
    mainPanel(
      #tabsetPanel(
        #tabPanel("Chart", plotOutput('plot1')),
        #tabPanel("Forecast)", tableOutput('table')))
      h3(strong("Indicator : Poverty Headcount Ratio"), align='center', style = 'font-family : verdana;'),
      plotOutput(outputId="plot1", width = "90%",)
      
      #h3("Plot Hasil peramalan"),
      #plotOutput(outputId="forecast")
      )
      
    ),
  h3(strong("Time Series Analysis"), align = 'center', style = 'font-family : verdana;'),
  fluidRow(
    column(7, align ='center',
           h4('Score Goal 1 (2000-2021)'),
          plotOutput(outputId = 'plot2', width = '95%')
          ),
    column(5, align = 'center',
           h4('Forecasting Score Goal 1 for 3 Periode'),
           dataTableOutput(outputId = 'table'))
  #h4("Forecasting Score Goal 1 for 3 Periode"),
  #fluidRow(dataTableOutput((outputId = 'table')))
  #fixedRow(column(6, 'level2'), dataTableOutput(outputId='table'),
           #column(3, 'level3')),
  
  #h3("Forecasting Score Goal 1", align="center"),
  #fluidRow(verbatimTextOutput(outputId="text")),
  #fluidRow(dataTableOutput((outputId = 'table')))
  )
)


# Define server logic ----
server <- function(input, output) {
  
  output$plot1 <- renderPlot ({
    data <- read.csv("D:/dataeas.csv", sep=";", header = TRUE)
    data
    
    library(Hmisc)
    data$sdg1_wpc <- with(data, impute(sdg1_wpc, median))
    data$sdg1_320pov <- with(data, impute(sdg1_320pov, median))
    summary(data)
    
    #Robust Scaler
    library(DescTools)
    robust<- RobScale(data$sdg1_wpc, center = TRUE, scale = FALSE)
    #hist(robust, col='dark blue', main='Robust Transformed')
    
    library(dplyr)
    library(ggplot2)
    if (input$indic == '$1.9/days'){
      bar <- data %>%
        select(Country, sdg1_wpc) %>%
        arrange(desc(sdg1_wpc))
      bar
      plot1 <- ggplot(bar, aes(x = reorder(Country, sdg1_wpc), y = sdg1_wpc)) + xlab('Country') +ylab ('Poverty Headcount Ratio (%)') + geom_col() + geom_bar(stat = 'identity', fill='darkred') + theme_light() + ggtitle("Poverty Headcount Ratio $1.9/days") + geom_text(aes(label = sdg1_wpc), vjust = -0.5)
    } else if (input$indic == '$3.2/days') {
      barr <- data %>%
        select(Country, sdg1_320pov) %>%
        arrange(desc(sdg1_320pov))
      barr
      plot1 <- ggplot(barr, aes(x = reorder(Country, sdg1_320pov), y = sdg1_320pov)) + xlab('Country') +ylab ('Poverty Headcount Ratio (%)') + geom_col() + geom_bar(stat = 'identity', fill='darkred') + theme_light() + ggtitle("Poverty Headcount Ratio $3.2/days") + geom_text(aes(label = sdg1_320pov), vjust = -0.5)
    }
    plot1
  })
  output$plot2 <- renderPlot ({
    datats <- read.csv("D:/datatsevd.csv", sep=";", header = TRUE)
    datats
    library(dplyr)
    if((input$country) == "Brunei Darussalam"){
      brn <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'BRN')
      brn
      plot2 <- ggplot(brn, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Brunei Darussalam") +  geom_line(size = 2, color='black') + geom_point()
    } else if(input$country =="Cambodia") {
      cam <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'KHM')
      cam
      plot2 <- ggplot(cam, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) +  theme_linedraw() + ggtitle("Goal Score of Cambodia") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country=="Indonesia") {
      idn <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'IDN')
      idn
      plot2 <- ggplot(idn, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) +  theme_linedraw() + ggtitle("Goal Score of Indonesia") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country=="Lao PDR(Laos)") {
      lao <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'LAO')
      lao
      plot2 <- ggplot(lao, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Laos (Lao PDR)") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country=='Malaysia') {
      mys <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'MYS')
      mys
      plot2 <- ggplot(mys, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Malaysia") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "Myanmar") {
      mmr <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'MMR')
      mmr
      plot2 <- ggplot(mmr, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Myanmar") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "Philippines") {
      phl <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'PHL')
      phl
      plot2 <- ggplot(phl, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Philippines") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "Singapore") {
      sgp <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'SGP')
      sgp
      plot2 <- ggplot(sgp, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Singapore") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "Thailand") {
      tha <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'THA')
      tha
      plot2 <- ggplot(tha, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Thailand") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "Vietnam") {
      vnm <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'VNM')
      vnm
      plot2 <- ggplot(vnm, aes (x = factor(Year), y = Goal.1.Score, colour = Country, group=Country)) + theme_linedraw() + ggtitle("Goal Score of Vietnam") + geom_line(size = 2, color='black') + geom_point()
    } else if (input$country == "All") {
      datats <- read.csv("D:/datatsevd.csv", sep=";", header = TRUE)
      datats
      plot2 <- ggplot(datats, aes(x = factor(Year), y = Goal.1.Score, colour = Country, group = Country)) + theme_linedraw() + ggtitle("Goal Score of All Country ASEAN") + geom_line(size = 2) + geom_point()
    } #else {
      #plot2 <- ggplot(datats, aes(x = factor(Year), y = Goal.1.Score, colour = Country, group = Country)) + theme_linedraw() +  ggtitle("Goal Score of All Country ASEAN") + geom_line(size = 2)
    #}
    plot2
  })

  output$table <- renderDataTable({
    datats <- read.csv("D:/datatsevd.csv", sep=";", header = TRUE)
    datats
    library(dplyr)
    library(forecast)
    if((input$country) == "Brunei Darussalam"){
      brn <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'BRN')
      brn
      library(forecast)
      model <- Arima(cam$Goal.1.Score, order = c(1,1,0), include.mean = F) #model terpilih
      
    } else if(input$country =="Cambodia") {
      cam <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'KHM')
      cam
      Acf(diff(cam$Goal.1.Score, lag=1))
      Pacf(diff(cam$Goal.1.Score, lag=1))
      model <- Arima(cam$Goal.1.Score, order = c(1,1,0), include.mean = F) #model terpilih

    } else if (input$country=="Indonesia") {
      idn <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'IDN')
      idn
      Acf(diff(idn$Goal.1.Score, lag=1))
      Pacf(diff(idn$Goal.1.Score, lag=1))
      model <- Arima(idn$Goal.1.Score, order = c(1,1,0), include.mean = F)
      #model <- Arima(idn$Goal.1.Score, order = c(0,1,1), include.mean = F)
      
    } else if (input$country=="Lao PDR(Laos)") {
      lao <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'LAO')
      lao
      Acf(diff(lao$Goal.1.Score, lag=1))
      Pacf(diff(lao$Goal.1.Score, lag=1))
      model <- Arima(lao$Goal.1.Score, order = c(1,1,0), include.mean = F)
      #model <- Arima(lao$Goal.1.Score, order = c(0,1,1), include.mean = F)
      
    } else if (input$country=='Malaysia') {
      mys <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'MYS')
      mys
      Acf(mys$Goal.1.Score)
      Pacf(mys$Goal.1.Score)
      model <- Arima(mys$Goal.1.Score, order = c(1,0,0), include.mean = F)
      #model <- Arima(mys$Goal.1.Score, order = c(1,0,1), include.mean = F)
      
    } else if (input$country == "Myanmar") {
      mmr <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'MMR')
      mmr
      Acf(diff(mmr$Goal.1.Score, lag=1))
      Pacf(diff(mmr$Goal.1.Score, lag=1))
      model <- Arima(mmr$Goal.1.Score, order = c(1,1,0), include.mean = F)
      #model <- Arima(mmr$Goal.1.Score, order = c(0,1,1), include.mean = F)
      
    } else if (input$country == "Philippines") {
      phl <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'PHL')
      phl
      #Acf(diff(phl$Goal.1.Score, lag=1))
      #Pacf(diff(phl$Goal.1.Score, lag=1))
      model <- Arima(phl$Goal.1.Score, order = c(1,1,0), include.mean = F)
      #model <- Arima(phl$Goal.1.Score, order = c(0,1,1), include.mean = F)
      
    } else if (input$country == "Singapore") {
      sgp <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'SGP')
      sgp
      #Acf(sgp$Goal.1.Score)
      #Pacf(sgp$Goal.1.Score)
      #model <- Arima(sgp$Goal.1.Score, order = c(1,0,0),include.mean = F)
      model <- Arima(sgp$Goal.1.Score, order = c(0,0,1),include.mean = F)
      
    } else if (input$country == "Thailand") {
      tha <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'THA')
      tha
      #Acf(tha$Goal.1.Score)
      #Pacf(tha$Goal.1.Score)
      model <- Arima(tha$Goal.1.Score, order = c(1,0,0),include.mean = F)
      #model <- Arima(tha$Goal.1.Score, order = c(0,0,1),include.mean = F)
      
    } else if (input$country == "Vietnam") {
      vnm <- datats %>%
        select(Country.Code.ISO3, Country, Year, Goal.1.Score) %>%
        filter (Country.Code.ISO3 == 'VNM')
      vnm
      #Acf(vnm$Goal.1.Score)
      #Pacf(vnm$Goal.1.Score)
      model <- Arima(vnm$Goal.1.Score, order = c(1,1,0), include.mean = F)
      #model <- Arima(vnm$Goal.1.Score, order = c(0,1,1), include.mean = F)
      
    } 
    Periode <- c("2022", "2023", "2024")
    result <- forecast(model, h=3)
    result <- cbind.data.frame(Periode, result)
    result
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)