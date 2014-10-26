
library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
GDP <- read.csv("./data/getdata_data_GDP.csv", sep = ",", header = TRUE,check.names=FALSE)
edu <- read.csv("./data/getdata_data_EDSTATS_Country.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names=FALSE)

GDP[complete.cases(GDP[,1]),]
edu[complete.cases(edu[,1]),]
temp <- merge(GDP,edu,by.x = "Code", by.y="CountryCode")
colnames(temp)[5]<-"GDP"
data <-temp[,c("Code","Long Name","GDP","Income Group","Region")]
# Define the overall UI
shinyUI(


        fluidPage(
                titlePanel("Countries by GDP"),

                # Create a new Row in the UI for selectInputs
                fluidRow(#column(4,  selectInput('xcol', 'X Variable', names(data)),selected=names(data)[[4]]),
                        column(4,  selectInput('xcol', 'X Variable', c("Region","Income Group")))),
                fluidRow(  plotOutput('plot1')),
                fluidRow(
                                                                                              
                        column(4, 
                               selectInput("GDP", 
                                           "By GDP:", 
                                           c("All", 
                                             unique(as.character(c( "0-5K","5k - 15K","15K-50K", "50K-100K",">100K")))))
                        ),
                        column(4, 
                               selectInput("Group", 
                                           "By Group:", 
                                           c("All", 
                                             unique(as.character(data[,"Income Group"] ))))
                        ),
                        column(4, 
                               selectInput("Region", 
                                           "By Region:", 
                                           c("All", 
                                             unique(as.character(data[,"Region"]))))
                        )        
                ),
                # Create a new row for the table.
                fluidRow(
                        dataTableOutput(outputId="table")
                )   
        )  
)