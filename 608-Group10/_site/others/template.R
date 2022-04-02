
# libraries
library(wordcloud)
library(RColorBrewer)
library(memoise)
library(wordcloud2)
library(topicmodels)
library(shinythemes)
packages = c('plotly', 'shiny', 'LDAvis', 'lda', 'lubridate', 
             'DT',  'tidyverse', 'stopwords', 'tm',
             'readxl', 'kableExtra', 'knitr', 'tidyquant','rmarkdown','tidyr',
             'data.table','XML','xml2','httr','dplyr','knitr', 'tokenizers', 'shinydashboard', 'tidytext')

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# value

ui <- dashboardPage(
  
  skin="black",
  dashboardHeader(title = "Government Procrument", titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      width=350,
      menuItem("ExploratoryDataAnalysis" ,tabName = "EDA", icon = icon("dashboard")),
      menuItem("Network Analysis",tabName = "Network", icon = icon("globe")),
      menuItem("TextAnalysis", tabName = "Text", icon = icon("address-card-o"))
    )
  ),
  
  #DB page
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = 'EDA',
        titlePanel("Exploratory Data Analysis"),
      ),# tab item EDA 
      
      tabItem(
        tabName = 'Network',
        titlePanel("Network Analysis"),
        #
      ),# tab item Network
      
      
      tabItem(
        tabName = 'Text',
        titlePanel("Text Mining Analysis"),
 
        
      )# tab item Text 
      
      
    )# Tab items
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # EDA Analysis Output
  
  
  # Network Analysis Output
  
  
  # Text Analysis Output
  

  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
