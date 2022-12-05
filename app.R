library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)

df1 <- read.csv(file = "datafile (1).csv")
colnames(df1) <- c("Crop", "State", 
                   "Cost_of_cultivation_A2_FL", 
                   "Cost_of_cultivation_C2", 
                   "Cost_of_Production_C2", "Yield")

df2 <- read.csv(file = "datafile (2).csv")
colnames(df2) <- c("Crop",
                   "Production_2006_07", "Production_2007_08", "Production_2008_09",
                   "Production_2009_10", "Production_2010_11", 
                   "Area_2006_07", "Area_2007_08", "Area_2008_09", "Area_2009_10",
                   "Area_2010_11", "Yield_2006_07", "Yield_2007_08", "Yield_2008_09",
                   "Yield_2009_10", "Yield_2010_11")

df3 <- read.csv(file = "datafile (3).csv")

df0 <- read.csv(file = "datafile.csv")
colnames(df0) <- c("Crop", "2004_05", "2005_06", "2006_07", "2007_08", "2008_09", 
                   "2009_10", "2010_11", "2011_12")
df0 <- df0 %>% filter(Crop != "")

states <- c("Andhra Pradesh", "Bihar", "Gujarat", "Haryana", "Karnataka", "Madhya Pradesh", "Maharashtra", "Orissa", "Punjab", "Rajasthan", "Tamil Nadu", "Uttar Pradesh", "West Bengal")

crops <- c("ARHAR", "COTTON", "GRAM", "GROUNDNUT", "MAIZE", "MOONG", "PADDY", "RAPESEED AND MUSTARD", "SUGARCANE", "WHEAT")

apy <- c("Area", "Production", "Yield")

years <- c("2006-07", "2007-08", "2008-09", "2009-10", "2010-11")

agri_cr <- c("All Agriculture", "Coarse Cereals", "Eggs, Fish and Meat", "Fibers", "Fruits", "Milk", "Oilseeds", "Pulses", "Rice", "Sugarcane", "Vegetables", "Wheat")

ui <- dashboardPage(
        dashboardHeader(title = "Visualization"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("About", tabName = "about"),
            menuItem("Analysis of Cost", tabName = "cost"),
            menuItem("Crops Production", tabName = "tab3"),
            menuItem("Agriculture Crops Production", tabName = "agri")
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "about", 
              fluidPage(h1("Agriculture Crop Production in India", align = "center"),
                    br(),
                    p("India is one of the major players in the agriculture sector worldwide and it is the primary source of livelihood for about 58% of India's population. India's production of food grains has been increasing every year, and India is among the top producers of several crops such as wheat, rice, pulses, sugarcane and cotton. It is the highest producer of milk and second highest producer of fruits and vegetables. In 2013, India contributed 25% to the world's pulses production, the highest for any one country, 22% to the rice production and 13% to the wheat production. It also accounted for about 25% of the total quantity of cotton produced, besides being the second highest exporter of cotton for the past several years. Agriculture sector in India holds the record for second-largest agricultural land in the world generating employment for about half of the country's population. Thus, farmers become an integral part of the sector to provide us with means of sustenance."),
                    
                    p("Key issues affecting agricultural productivity include the decreasing sizes of agricultural land holdings, continued dependence on the monsoon, inadequate access to irrigation, imbalanced use of soil nutrients resulting in loss of fertility of soil, uneven access to modern technology in different parts of the country, lack of access to formal agricultural credit, limited procurement of food grains by government agencies, and failure to provide remunerative prices to farmers."),
                    br(),
                    fluidRow(
                      column(4, img(src = "Agriculture_main.jpg", height = 240, width = 310)),
                      column(4, img(src = "farming.jpeg", height = 240, width = 310)),
                      column(4, img(src = "Indian-Agriculture.jpg", height = 240, width = 310))),
                    br(),
                    p("We are going to observe the production of agriculture in India on data available for 8 years. It is of interest to look into cost of cultivation, cost of production and yield of different crops in various states in India. We would also look into the production, land area used and yield of different crops."),
                    p("In this study, we would mainly focus on the following:"),
                    p("- observing which agriculture crop has maximum production over the years"),
                    p("- observing which state has more yield and less cost of cultivation and production in various crops"),
                    p("- observing which crop is using lesser land area and giving more production and yield"),
                    p("- observing maximum number of varieties in a crop and their most common duration of cultivation and production")
              )
            ),
            
            tabItem(tabName = "cost", h2("Analysis of Cost"),
              tabsetPanel(
                tabPanel("State-wise", 
                  fluidPage(
                      selectInput("choice_st", label = "Select the state:", choices = states), 
                      actionButton("go_2_1", label = "Show graphs"),
                      br(), br(),
                      fluidRow(column(6, plotOutput("st_cult")), column(6, plotOutput("st_prod"))),
                      br(),
                      plotOutput("st_yield")
                  )
                ),
                tabPanel("Crop-wise",
                  fluidPage(
                    selectInput("choice_cr", label = "Select the crop:", choices = crops), 
                    actionButton("go_2_2", label = "Show graphs"),
                    br(), br(),
                    fluidRow(column(6, plotOutput("cr_cult")), column(6, plotOutput("cr_prod"))),
                    br(),
                    plotOutput("cr_yield")
                  )
                )
              )
            ),
            
            tabItem(tabName = "tab3", h2("Crops Production"),
              fluidPage(
                selectInput("choice_apy", label = "Select variable:", choices = apy),
                selectInput("choice_yr", label = "Select the year:", choices = years),
                actionButton("go_3_1", label = "Show graph"),
                plotOutput("apy_plot")
              )
            ),
            tabItem(tabName = "agri", h2("Agriculture Crops Production"),
              fluidPage(
                selectInput("choice_ag", label = "Select the agriculture crop:", choices = agri_cr),
                actionButton("go_4_1", label = "Show graph"), br(), br(), 
                plotOutput("agri_plot")
              )
            )
          )
        )
      )


server <- function(input, output, session){
  
  ## Server for tab 2 panel 1
  
  x <- reactive({
    if(input$choice_st == "Andhra Pradesh"){
      df1 %>% filter(State == "Andhra Pradesh")
    }
    else if(input$choice_st == "Bihar"){
      df1 %>% filter(State == "Bihar")
    }
    else if(input$choice_st == "Gujarat"){
      df1 %>% filter(State == "Gujarat")
    }
    else if(input$choice_st == "Haryana"){
      df1 %>% filter(State == "Haryana")
    }
    else if(input$choice_st == "Karnataka"){
      df1 %>% filter(State == "Karnataka")
    }
    else if(input$choice_st == "Madhya Pradesh"){
      df1 %>% filter(State == "Madhya Pradesh")
    }
    else if(input$choice_st == "Maharashtra"){
      df1 %>% filter(State == "Maharashtra")
    }
    else if(input$choice_st == "Orissa"){
      df1 %>% filter(State == "Orissa")
    }
    else if(input$choice_st == "Punjab"){
      df1 %>% filter(State == "Punjab")
    }
    else if(input$choice_st == "Rajasthan"){
      df1 %>% filter(State == "Rajasthan")
    }
    else if(input$choice_st == "Tamil Nadu"){
      df1 %>% filter(State == "Tamil Nadu")
    }
    else if(input$choice_st == "Uttar Pradesh"){
      df1 %>% filter(State == "Uttar Pradesh")
    }
    else if(input$choice_st == "West Bengal"){
      df1 %>% filter(State == "West Bengal")
    }
  })
  
  observeEvent(input$go_2_1, output$st_cult <- renderPlot({
    dfst = data.frame(isolate(x()))
    dfstm = melt(dfst[, c("Crop", "Cost_of_cultivation_A2_FL", 
                           "Cost_of_cultivation_C2")], id.vars = 1)
    ggplot(dfstm, aes(Crop, value/1000))+
      geom_bar(aes(fill = variable), stat = "identity", fill = "Skyblue") +
      ylab("Cost of Cultivation/Hectare in thousands") + 
      ggtitle("Cost of Cultivation", isolate(input$choice_st))
  }))
  
  observeEvent(input$go_2_1, output$st_prod <- renderPlot({
    dfst2 = data.frame(isolate(x()))
    ggplot(dfst2, aes(Crop, Cost_of_Production_C2/1000))+
      geom_bar(stat = "identity", fill = "blue") +
      ylab("Cost of Production/Quintal in thousands") + 
      ggtitle("Cost of Production", isolate(input$choice_st))
  }))
  
  observeEvent(input$go_2_1, output$st_yield <- renderPlot({
    dfst3 = data.frame(isolate(x()))
    ggplot(dfst3, aes(Crop, Yield)) +
      geom_bar(stat = "identity", fill = "green") +
      ylab("Yield Quintal/Hectare") +
      ggtitle("Yield", isolate(input$choice_st))
  }))
  
  ## Server for tab 2 panel 2
  
  cr <- reactive({
    if(input$choice_cr == "ARHAR"){
      df1 %>% filter(Crop == "ARHAR")
    }
    else if(input$choice_cr == "COTTON"){
      df1 %>% filter(Crop == "COTTON")
    }
    else if(input$choice_cr == "GRAM"){
      df1 %>% filter(Crop == "GRAM")
    }
    else if(input$choice_cr == "GROUNDNUT"){
      df1 %>% filter(Crop == "GROUNDNUT")
    }
    else if(input$choice_cr == "MAIZE"){
      df1 %>% filter(Crop == "MAIZE")
    }
    else if(input$choice_cr == "MOONG"){
      df1 %>% filter(Crop == "MOONG")
    }
    else if(input$choice_cr == "PADDY"){
      df1 %>% filter(Crop == "PADDY")
    }
    else if(input$choice_cr == "RAPESEED AND MUSTARD"){
      df1 %>% filter(Crop == "RAPESEED AND MUSTARD")
    }
    else if(input$choice_cr == "SUGARCANE"){
      df1 %>% filter(Crop == "SUGARCANE")
    }
    else if(input$choice_cr == "WHEAT"){
      df1 %>% filter(Crop == "WHEAT")
    }
  })
  
  observeEvent(input$go_2_2, output$cr_cult <- renderPlot({
    dfcr <- data.frame(isolate(cr()))
    dfcrm <- melt(dfcr[, c("State", "Cost_of_cultivation_A2_FL", 
                           "Cost_of_cultivation_C2")], id.vars = 1)
    dfcrm <- dfcrm %>% group_by(State) %>% 
      summarise(sum_obs = sum(value)) %>% 
      mutate(perc = sum_obs/sum(sum_obs)) %>%
      mutate(labels = scales::percent(perc, accuracy = 0.01))
    
    ggplot(dfcrm, aes("", perc, fill = State)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) + xlab("") + ylab("") +
      geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
      ggtitle("Cost of Cultivation", isolate(input$choice_cr))
  }))
  
  observeEvent(input$go_2_2, output$cr_prod <- renderPlot({
    dfcr2 <- data.frame(isolate(cr()))
    dfcrm2 <- dfcr2 %>%
      mutate(perc = (Cost_of_Production_C2/1000)/sum(Cost_of_Production_C2/1000)) %>%
      mutate(labels = scales::percent(perc, accuracy = 0.01))
    
    ggplot(dfcrm2, aes("", perc, fill = State)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) + xlab("") + ylab("") +
      geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
      ggtitle("Cost of Production", isolate(input$choice_cr))
  }))
  
  observeEvent(input$go_2_2, output$cr_yield <- renderPlot({
    dfcr3 <- data.frame(isolate(cr()))
    dfcr3 <- dfcr3 %>% mutate(perc = Yield/sum(Yield)) %>%
      mutate(labels = scales :: percent(perc, accuracy = 0.01))
    
    ggplot(dfcr3, aes("", perc, fill = State)) + geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) + xlab("") + ylab("") +
      geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
      ggtitle("Yield", isolate(input$choice_cr))
  }))
  
  ## Server for tab 3
  
  v <- reactive({
  if(input$choice_apy == "Area"){
      if(input$choice_yr == "2006-07"){
        df2$Area_2006_07
      }
      else if(input$choice_yr == "2007-08"){
        df2$Area_2007_08
      }
      else if(input$choice_yr == "2008-09"){
        df2$Area_2008_09
      }
      else if(input$choice_yr == "2009-10"){
        df2$Area_2009_10
      }
      else if(input$choice_yr == "2010-11"){
        df2$Area_2010_11
      }
    }
    else if(input$choice_apy == "Production"){
      if(input$choice_yr == "2006-07"){
        df2$Production_2006_07
      }
      else if(input$choice_yr == "2007-08"){
        df2$Production_2007_08
      }
      else if(input$choice_yr == "2008-09"){
        df2$Production_2008_09
      }
      else if(input$choice_yr == "2009-10"){
        df2$Production_2009_10
      }
      else if(input$choice_yr == "2010-11"){
        df2$Production_2010_11
      }
    }
    else if(input$choice_apy == "Yield"){
      if(input$choice_yr == "2006-07"){
        df2$Yield_2006_07
      }
      else if(input$choice_yr == "2007-08"){
        df2$Yield_2007_08
      }
      else if(input$choice_yr == "2008-09"){
        df2$Yield_2008_09
      }
      else if(input$choice_yr == "2009-10"){
        df2$Yield_2009_10
      }
      else if(input$choice_yr == "2010-11"){
        df2$Yield_2010_11
      }
    }
})
  
  observeEvent(input$go_3_1, output$apy_plot <- renderPlot({
    ggplot(df2, aes(Crop, isolate(v()), group = 1)) +
      geom_line(color = "blue") + ylab(input$choice_apy) + xlab("Crops") +
      ggtitle(isolate(input$choice_apy), isolate(input$choice_yr)) + 
      theme(axis.text.x = element_text(angle = 90))
  }))
  
  ## Server for tab 4
  
  df0_years <- as.data.frame(t(df0))
  df0_years <- cbind(rownames(df0_years), df0_years)
  rownames(df0_years) <- 1:nrow(df0_years)
  colnames(df0_years) <- c("Year", df0_years[1, 2:13])
  df0_years <- df0_years[2:9,]
  
  ag <- reactive({
    if(input$choice_ag == "All Agriculture"){
      df0_years$`All Agriculture`
    }
    else if(input$choice_ag == "Coarse Cereals"){
      df0_years$`Coarse Cereals`
    }
    else if(input$choice_ag == "Eggs, Fish and Meat"){
      df0_years$`Eggs, Fish and Meat`
    }
    else if(input$choice_ag == "Fibers"){
      df0_years$Fibers
    }
    else if(input$choice_ag == "Fruits"){
      df0_years$Fruits
    }
    else if(input$choice_ag == "Milk"){
      df0_years$Milk
    }
    else if(input$choice_ag == "Oilseeds"){
      df0_years$Oilseeds
    }
    else if(input$choice_ag == "Pulses"){
      df0_years$Pulses
    }
    else if(input$choice_ag == "Rice"){
      df0_years$Rice
    }
    else if(input$choice_ag == "Sugarcane"){
      df0_years$Sugarcane
    }
    else if(input$choice_ag == "Vegetables"){
      df0_years$Vegetables
    }
    else if(input$choice_ag == "Wheat"){
      df0_years$Wheat
    }
  })
  
  observeEvent(input$go_4_1, output$agri_plot <- renderPlot({
    ggplot(df0_years, aes(Year, isolate(ag()))) +
      geom_line(group = 1, color = "blue") +  geom_point(color = "blue") +
      ylab("Production") + xlab("Years") +
      ggtitle(paste("Production of", isolate(input$choice_ag)))
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
