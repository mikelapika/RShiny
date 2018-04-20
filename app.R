#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyBS)
library(haven)
library(sas7bdat)
library(survey)
library(gmodels)
library(summarytools)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(Matrix)
library(questionr)
library(formattable)
library(car)
library(scatterplot3d)
library(dplyr)
library(RPostgreSQL)

# Load dataset
#me0015_m = read_sas("C:/BRFSS_MIKE/me0015_m.sas7bdat")
#me0015_m = read_sas("//Users/mikelapika/Desktop/me0015_m.sas7bdat")
datap1 <- me0015_m

datap <- subset(datap1,select = c(`_STSTR`, `_LLCPWT`,`_FINALQ1`,`_FINALWT`,`_LCPWTV1`,`_LCPWTV2`,`_LANDWT`, `_psu`, `GENHLTH`,`_AGEG5YR`,
                                  `year`, `county`, `SEX`,`iyear`,`_AGE65YR`,`_AGEG_`,`PHYSHLTH`,`POORHLTH`))

dat2015 <- dplyr::filter(me0015_m, year == '2015')
dat2015 <- dplyr::filter(datap1, year == '2015')
dat2014 <- dplyr::filter(datap1, year == '2014')
dat2013 <- dplyr::filter(datap1, year == '2013')
dat2012 <- dplyr::filter(datap1, year == '2012')

mydesign2 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )

#Format data general health
GENHELTH <- factor(dat2015$GENHLTH, levels=c(1, 2, 3, 4,5,7,9),labels=c("Excellent", "Very good", "Good","Fair","Poor","Don't know/Not sure","Refused"))
# Format data county
COUNTY <- factor(dat2015$county, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),labels=c("Androscoggin", "Aroostook", "Cumberland","Franklin","Hancock","Kennebec","Knox","Lincoln","Oxford","Penobscot","Piscataquis","Sagadahoc","Somerset","Waldo","Washington","York"))
# Format Gender
GENDER <- factor(dat2015$SEX, levels=c(1, 2),labels=c("Male", "Female"))
# Format Age
AGE <- factor(dat2015$`_AGEG5YR`, levels=c(1, 2, 3, 4,5,6,7,8,9,10,11,12,13,14),labels=c("18-24", "25-29", "30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+","Refused"))

###


library(shinyBS)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Analyze Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Checkbox if file has header ----
      #checkboxInput("header", "Header", TRUE),

      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("-","FlightDelays","ME_BRFSS","BCL_Data","US_BRFSS","Rock", "Pressure", "Cars", "Iris")),
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),
      # select year if needed
      selectInput(inputId = "year",
                  label = "Select Year:",
                  choices = c("-","2015"="2015", "2014"="2014", "2013"="2013","2012","2011","2010","2009","2008",
                              "2007","2006","2005","2004","2003","2002","2001"="2001")),
      #choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1))
      selectInput(inputId = "question",
                  label = "Select variable:",
                  choices = c("GENHLTH", "PHYSHLTH", "POORHLTH")),

      selectInput(inputId = "analys",
                  label = "Select analysis:",
                  choices = c("-","Frequency","Mean", "Percentage","CONF.INT","SVYTOTAL")),

      selectInput(inputId = "analoption",
                  label = "Select analysis option:",
                  choices = c("-","county", "SEX","_AGEG5YR")),

      selectInput(inputId = "typeplot",
                  label = "Select Type of Graph:",
                  choices = c("-","BoxPlot", "Histogram","Density","Scatterplots","Barplot")),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",All = "all"),selected = "head")),
    #
    mainPanel(

      # Output: Data file ----
      #tableOutput("contents"),
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("mytable")),
                  #tabPanel("Table", tableOutput("contents")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Analysis", tableOutput("analysis")),
                  tabPanel("CHI-2", tableOutput("testchi2")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Datasets", tableOutput("datasets")),
                  tabPanel("About", tableOutput("about"))
      )

    )

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  options(shiny.maxRequestSize=1000*1024^2)

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Rock" = rock,
           "Pressure" = pressure,"US_BRFSS"=llcp2015,
           "Cars" = cars, "Iris" = iris,"FlightDelays"=FlightDelays,"BCL_Data"=bcl_data,"ME_BRFSS"=me0015_m)
  })



  output$plot <- renderPlot({
    if(input$typeplot== "-") {
      return() }
    else if(input$typeplot == "Boxplot") {
      return (boxplot(rnorm(datasetInput()), col="lightblue",notch=TRUE,main = "Boxplot of normal generated data", xlab="Sample data",
                      las=1,xlim=c(1,7),ylim=c(0,40),density=50,angle=60
      )) }
    else if(input$typeplot == "Histogram") {
      Ixos_h=rnorm(datasetInput())
      Primadur=Ixos_h+rnorm(datasetInput())
      mtext("Primadur : Distribution and correlation ", outer = TRUE, cex = 1.5, font=4, col=rgb(0.1,0.3,0.5,0.5))
      return (hist(rnorm(datasetInput()),breaks=10, col=rgb(0.3,0.5,1,0.4), main="Histogram with Normal Curve", xlab="Sample data", ylab="frequency"))}

    else if(input$typeplot == "Density") {
      Ixos_d=rnorm(datasetInput())
      return (densityPlot(Ixos_d)) }
    else if(input$typeplot == "Scatterplots") {
      Ixos_s=rnorm(datasetInput())
      Primadur=Ixos_s+rnorm(datasetInput())
      return ( plot(rnorm(datasetInput()) , Primadur,  main="Scatterplot" , pch=20 , cex=1.5 , col=rgb(0.3,0.5,1,0.4)  , xlab="primadur" , ylab="Ixos" )) }
    else if(input$typeplot == "Barplot") {
      Ixos_d=rnorm(datasetInput())
      return (barplot((Ixos_d),ylim = c(-1,1), col=rgb(0.3,0.5,1,0.4))) }
    else  {
      return (boxplot(rnorm(datasetInput()), col="lightblue",main = "Boxplot of normal generated data", xlab="Sample data"))
    }

  })

  #content
  # output$contents <- renderTable({
  # in case I load data manually
  #  req(datap)
  # df1 <- (datap)

  # if(input$disp == "head") {
  #   return(head(df1))
  #  }
  #  else {
  #   head((df1), n = input$obs)
  # }
  # in case I load data using the library I create
  #   head(datasetInput(), n = input$obs)
  #  })

  # different look of table output
  output$mytable <- DT::renderDataTable({
    if(input$dataset== "-") {
      return() }
    else if(input$dataset == "ME_BRFSS" & input$year == "2015" ) {
      dat2015 <- dplyr::filter(me0015_m, year == '2015')
      mydesign2015 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
      DT::datatable(head(dat2015,n=input$obs), options = list(orderClasses = TRUE)) }
    else if(input$dataset == "ME_BRFSS" & input$year == "2014" ) {
      dat2014 <- dplyr::filter(me0015_m, year == '2014')
      mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
      DT::datatable(head(dat2014,n=input$obs), options = list(orderClasses = TRUE)) }
    else if(input$dataset == "ME_BRFSS" & input$year == "2013" ) {
      dat2013 <- dplyr::filter(me0015_m, year == '2013')
      mydesign2013 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2013, nest = TRUE )
      DT::datatable(head(dat2013,n=input$obs), options = list(orderClasses = TRUE)) }
    else if(input$dataset == "ME_BRFSS" & input$year == "2012" ) {
      dat2012 <- dplyr::filter(me0015_m, year == '2012')
      mydesign2012 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2012, nest = TRUE )
      DT::datatable(head(dat2012,n=input$obs), options = list(orderClasses = TRUE)) }
    else if(input$dataset == "ME_BRFSS" & input$year == "2011" ) {
      dat2011 <- dplyr::filter(me0015_m, year == '2011')
      mydesign2011 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2011, nest = TRUE )
      DT::datatable(head(dat2011,n=input$obs), options = list(orderClasses = TRUE)) }
    else if(input$dataset == "ME_BRFSS" & input$year == "2010" ) {
      dat2010 <- dplyr::filter(me0015_m, year == '2010')
      mydesign2010 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2010, nest = TRUE )
      DT::datatable(head(dat2010,n=input$obs), options = list(orderClasses = TRUE)) }
    else{ DT::datatable(head(datasetInput(),n=input$obs), options = list(orderClasses = TRUE))}

  })

  # Perform Analysis
  output$analysis <- renderTable({
    options(shiny.maxRequestSize=1000*1024^2)
    #req(datap)
    # select analysis


    ######## ( Working for genhelth)#########
    if(input$analys== "-") {
      return() }

    #####
    # else if(input$dataset == "ME_BRFSS" & input$year == "2015" & input$analys == "Frequency" & input$analoption == "county" ) {
    #   svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2,svytotal ,deff=F) }

    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2015" ) {
      dat2015 <- dplyr::filter(me0015_m, year == '2015')
      mydesign2015 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2015,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2014" ) {
      dat2014 <- dplyr::filter(me0015_m, year == '2014')
      mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2014,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2013" ) {
      dat2013 <- dplyr::filter(me0015_m, year == '2013')
      mydesign2013 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2013, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2013,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2012" ) {
      dat2012 <- dplyr::filter(me0015_m, year == '2012')
      mydesign2012 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2012, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2012,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2011" ) {
      dat2011 <- dplyr::filter(me0015_m, year == '2011')
      mydesign2011 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2011, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2011,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "county" & input$year== "2010" ) {
      dat2010 <- dplyr::filter(me0015_m, year == '2010')
      mydesign2010 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2010, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2010,svytotal ,deff=F) }

     ### gender ##

    else if(input$analys == "Frequency" & input$analoption == "SEX" & input$year== "2015" ) {
      dat2015 <- dplyr::filter(me0015_m, year == '2015')
      mydesign2015 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~GENDER ,mydesign2015,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "SEX" & input$year== "2014" ) {
      dat2014 <- dplyr::filter(me0015_m, year == '2014')
      mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~GENDER ,mydesign2014,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "SEX" & input$year== "2013" ) {
      dat2013 <- dplyr::filter(me0015_m, year == '2013')
      mydesign2013 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2013, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~GENDER ,mydesign2013,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "SEX" & input$year== "2012" ) {
      dat2012 <- dplyr::filter(me0015_m, year == '2012')
      mydesign2012 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2012, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~GENDER ,mydesign2012,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "SEX" & input$year== "2011" ) {
      dat2011 <- dplyr::filter(me0015_m, year == '2011')
      mydesign2011 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2011, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~GENDER ,mydesign2011,svytotal ,deff=F) }

    ### age ###

    else if(input$analys == "Frequency" & input$analoption == "_AGEG5YR" & input$year== "2015" ) {
      dat2015 <- dplyr::filter(me0015_m, year == '2015')
      mydesign2015 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~AGE ,mydesign2015,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "_AGEG5YR" & input$year== "2014" ) {
      dat2014 <- dplyr::filter(me0015_m, year == '2014')
      mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~AGE ,mydesign2014,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "_AGEG5YR" & input$year== "2013" ) {
      dat2013 <- dplyr::filter(me0015_m, year == '2013')
      mydesign2013 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2013, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~AGE ,mydesign2013,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "_AGEG5YR" & input$year== "2012" ) {
      dat2012 <- dplyr::filter(me0015_m, year == '2012')
      mydesign2012 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2012, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~AGE ,mydesign2012,svytotal ,deff=F) }
    else if(input$analys == "Frequency" & input$analoption == "_AGEG5YR" & input$year== "2011" ) {
      dat2011 <- dplyr::filter(me0015_m, year == '2011')
      mydesign2011 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2011, nest = TRUE )
      svyby(~GENHLTH ,~GENHELTH+~AGE ,mydesign2011,svytotal ,deff=F) }
    #### Mean #####
    else if(input$analys == "Mean") {
      svymean(~factor(GENHELTH), mydesign2) }
    else if(input$dataset == "ME_BRFSS" & input$analys == "Mean" & input$analoption == "county" & input$year== "2014" ) {
      dat2014 <- dplyr::filter(me0015_m, year == '2014')
      mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
      # svymean(~factor(GENHELTH) ,mydesign2015)
      svymean(~factor(GENHELTH) ,mydesign2014) }
    #### PERCENTAGE #####
    else if(input$analys == "Percentage") {
      prop.table(svytable(~GENHELTH, design = mydesign2)) }
    #### CONF.INT #####
    else if(input$analys == "CONF.INT") {
      confint.default(svytotal(~GENHELTH, mydesign2),level = 0.90) }
    #### SVY TOTAL #####
    else if(input$analys == "SVYTOTAL") {
      svytotal(~GENHELTH,mydesign2,deff=T) }
    else {
      table(GENHELTH, useNA="ifany")
    }
  })
  ###########################(until here)###################################

  ######## ( Testing All dataset)#########


  ######## ( End)#########

  ################# select analysis option#####################

  #  if(input$analoption == "NA") {
  #     return("NA") }
  #   else if(input$analoption == "county") {
  #     return(table(COUNTY, useNA="ifany")) }
  #   else if(input$analoption == "SEX") {
  #     return(table(GENDER, useNA="ifany")) }
  #   else if(input$analoption == "_AGEG5YR") {
  #    return(table(AGE, useNA="ifany")) }
  #    else {
  #     table(GENHLTH, useNA="ifany")
  #  }

  #  if(input$analoption== "-") {
  #    return(input$analys) }
  #  else if(input$analys == "Frequency" ) {
  #    return (table(GENHELTH, useNA="ifany")) }
  #   else if(input$analys == "Frequency" & input$analoption == "county") {
  #     return (svyby(~GENHLTH ,~GENHELTH+~COUNTY ,mydesign2,svytotal ,deff=F)) }
  #  else if(input$analoption == "Mean") {
  #    return(svymean(~GENHELTH, mydesign2)) }
  #  else if(input$analoption == "Percentage") {
  #    return(prop.table(svytable(~GENHELTH, design = mydesign2))) }
  #   else if(input$analoption == "CONF.INT") {
  #    return(confint.default(svytotal(~GENHELTH, mydesign2),level = 0.90)) }
  #  else if(input$analoption == "SVYTOTAL") {
  #    return(svytotal(~GENHELTH,mydesign2,deff=T)) }
  #  else {
  #    table(GENHELTH, useNA="ifany")
  #  }

  ###########################################################################

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
    # req(datap)
    # df1 <- datap
    # summary(df1)
  })

  ############# TEST CHI2#########################

  # Generate test chi2 of the data ----
  output$testchi2 <- renderPrint({
    chi2 <- datasetInput()
    chisq.test(chi2)

  })
  #####################################

}


# Run the application
shinyApp(ui = ui, server = server)
