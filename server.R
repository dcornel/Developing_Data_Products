library(shiny)
library(plyr)
library(lattice)
library(ggplot2)
library(scales)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
GDP <- read.csv("./data/getdata_data_GDP.csv", sep = ",", header = TRUE,check.names=FALSE,stringsAsFactors = FALSE)
edu <- read.csv("./data/getdata_data_EDSTATS_Country.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names=FALSE)

GDP[complete.cases(GDP[,1]),]
edu[complete.cases(edu[,1]),]
temp <- merge(GDP,edu,by.x = "Code", by.y="CountryCode")
colnames(temp)[5]<-"GDP"
datatemp <-temp[,c("Code","Long Name","GDP","Income Group","Region")]
datatemp[,"GDP"] <- as.numeric(gsub(",","", datatemp[,"GDP"]))

# Define a server for the Shiny app
shinyServer(function(input, output, session) {
        
       
        output$table <- renderDataTable({
               

                data<-datatemp
                # Filter data based on selections
                if (input$GDP != "All"){
                        if(input$GDP == "0-5K"){ 
                        data <- data[ as.numeric(gsub(",","", data$GDP)) >0 & as.numeric(gsub(",","", data$GDP)) <=5000,]
                        }
                        if(input$GDP == "5k - 15K"){ 
                                data <- data[as.numeric(gsub(",","", data$GDP)) >5000 & as.numeric(gsub(",","", data$GDP)) <15000,]
                        }
                        if(input$GDP == "15K-50K"){ 
                                data <- data[as.numeric(gsub(",","", data$GDP)) >15000 & as.numeric(gsub(",","", data$GDP)) <50000,]
                        }
                        if(input$GDP == "50K-100K"){ 
                                data <- data[as.numeric(gsub(",","", data$GDP)) >50000 & as.numeric(gsub(",","", data$GDP)) <100000,]
                        }
                        if(input$GDP == ">100K"){ 
                                data <- data[as.numeric(gsub(",","", data$GDP)) >100000,]
                        }
                        
                }
                if (input$Group != "All"){
                        data <- data[data[,"Income Group"] == input$Group,]
                }
                if (input$Region != "All"){
                        data <- data[data[,"Region"] == input$Region,]
                }
                data
        })
        
        # Combine the selected variables into a new data frame
       

        mydata<-datatemp
        selectedData <- reactive({
                mydata<-datatemp
                
                mydata<-mydata[, c(input$xcol, "GDP")]
                y<-input$xcol
                #mydata<-mydata[, c("GDP", "Region")]
                #lmfit = lm( input$xcol ~ input$ycol, mydata )
               #lmfit = lm( GDP ~ Region, mydata )
               mydata$count<- 1
               mydata$count<-as.numeric(as.character(mydata$count))
               mydata$GDP<-as.numeric(as.character(mydata$GDP))
               #mydata<-aggregate(mydata, by = list(mydata[, input$xcol]), sum)
               mydata<- ddply(mydata, 1, function(x) colSums(x[c("GDP","count")])) 
               mydata$GDP<-mydata$GDP/mydata$count
               #aggregate(DF[, 9:118], by = list(DF[, 4]), FUN = mean)
               # mydata<-mydata[,c("Group.1","GDP")]
               mydata
               #lmfit
        })
        
        clusters <- reactive({
                kmeans(selectedData(), input$clusters)
        })
        
        output$plot1 <- renderPlot({
                par(mar = c(5.1, 4.1, 0, 1))
                
                m<- selectedData()
                tt<-paste("`",names(m)[1],"`", sep ="")
                #qplot(GDP, m$Group.1, m)
                ggplot(data=m, aes_string(x=tt, y= "GDP")) + ylab("Average GDP(milions of dollars)")+xlab(tt) + scale_size_continuous(range = c(3,13))+
                        geom_point(aes(size = count)) + scale_y_continuous(labels = comma) + theme(axis.text.x=element_text(angle=90))
                #plot(m
                     #col = clusters()$cluster,
                    
                #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
        })
        
        
})