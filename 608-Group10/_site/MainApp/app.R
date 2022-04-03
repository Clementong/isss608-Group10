

if(!require(networkD3)){
  devtools::install_github("juandaserni/networkD3", uninstall=TRUE)
}


library(colourpicker)
library(d3treeR)
library(data.table)
library(dplyr)
library(DT)
library(ggraph)
library(ggplot2)
library(googleVis)
library(hrbrthemes)
library(htmlwidgets)
library(httr)
library(igraph)
library(kableExtra)
library(knitr)
library(LDAvis)
library(lda)
library(lubridate)
library(memoise)
library(networkD3)
library(plotly)
library(readr)
library(readxl)
library(RColorBrewer)
library(rmarkdown)
library(rsconnect)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(stopwords)
library(stringr)
library(tidygraph)
library(tidyquant)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)
library(tokenizers)
library(topicmodels)
library(treemap)
library(visNetwork)
library(wordcloud)
library(wordcloud2)
library(XML)
library(xml2)



packages = c('plotly', 'shiny', 'LDAvis', 'lda', 'lubridate', 
             'DT',  'tidyverse', 'stopwords', 'tm',
             'readxl', 'kableExtra', 'knitr', 'tidyquant','rmarkdown','tidyr',
             'data.table','XML','xml2','httr','dplyr','knitr', 'tokenizers', 'shinydashboard', 'tidytext',
             'plotly', 'shiny', 'LDAvis', 'lda', 'lubridate', 
             'DT',  'tidyverse', 'stopwords', 'tm',
             'readxl', 'kableExtra', 'knitr', 'tidyquant','rmarkdown','tidyr',
             'data.table','XML','xml2','httr','dplyr','knitr', 'tokenizers', 'shinydashboard', 'tidytext',
             'treemap','d3treeR','ggplot2','stringr')



for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

if(!require(d3treeR)){
  devtools::install_github("timelyportfolio/d3treeR")
}

if(!require(data.tree)){
  devtools::install_github("gluc/data.tree")
}




# EDA
#data
record <- read_csv("data/government-procurement-via-gebiz.csv")
ministry <- read_csv("data/AgencyMinistry.csv")

#join datasets
govprocurem <- left_join(record,ministry, by = c("agency"))




# NETWORK ANALYSIS
# Read csv
procurement_data <- read_csv("data/government-procurement-via-gebiz.csv")
agency_ministry <- read_xlsx("data/AgencyMinistry.xlsx")

# Join the 2 datasets
merged_data <- merge(procurement_data, agency_ministry, by="agency")

# Convert award_date string values to YYYY-MM-DD date format
merged_data$award_date <- as.Date(merged_data$award_date, "%d/%m/%Y")
merged_data$wog <- "Public Sector"



# FUnction to sort and filter top X suppliers
top_suppliers_apply <- function(data, number){
  
  sorted_data <- data[order(data$total_amt,
                            na.last = TRUE,
                            decreasing = TRUE),]
  
  if(number<=length(sorted_data$supplier_name)){
    sorted_data<-sorted_data[1:number,]
  }
  if(number>length(sorted_data$supplier_name)){
    sorted_data}
  sorted_data
}


# Function to generate IDsource and IDtarget
IDgenerate <- function(data1, data2){
  IDlist <- c()
  for (row in data1){
    a <- match(row,data2) - 1
    IDlist <- append(IDlist,a)
  }
  IDlist <- as.data.frame(IDlist)
}

# Function for drilldown
funct <-
  function(n){
    isp <- sprintf("Select * From df Where df.name='%s';", n)
    isd <- sqldf::sqldf(isp)
    return(isd)
  }

# Layouts for Network Diagram
layouts <- data.frame(c("layout_as_star",
                        "layout_components",
                        "layout_in_circle",
                        "layout_nicely",
                        "layout_on_grid",
                        "layout_on_sphere",
                        "layout_randomly",
                        "layout_with_dh",
                        "layout_with_drl",
                        "layout_with_fr",
                        "layout_with_gem",
                        "layout_with_graphopt",
                        "layout_with_kk",
                        "layout_with_lgl",
                        "layout_with_mds"))
names(layouts)[1]<- "layout"



# TEXT ANALYSIS
df <- read.csv("./data/cleaned_text.csv")
df$supplier_name <- gsub("unknown", "Untendered",df$supplier_name)
df$supplier_name <- gsub("pte.", "",df$supplier_name)
df$supplier_name <- gsub("ltd.", "",df$supplier_name)
df <- df %>%
  mutate(award_date=as.Date(award_date, format = "%d/%m/%Y"))
df <-  df[!(is.na(df$topic) | df$topic==""), ]
df$topic= factor(df$topic, levels=c('1','2','3','4'),
                 labels=c('Installation and Manpower','Planning and Management','Construction and Systems','Public Facilities'))

# remove all rows with missing values

agencies <- unique(df$agency)
agencies <- c( c('all'), agencies)

suppliers <- unique(df$supplier_name)
suppliers <- c( c('all'), suppliers)


topics = unique(df$topic)

## lda

# min date window 
min_date<-min(df$award_date)
max_date<-max(df$award_date)
#Amt window
min_amt <- min(df$awarded_amt)  
max_amt <- max(df$awarded_amt)


# building lda json object
topicmodels2LDAvis <- function(x, n, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]],
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    R = n
  )
}

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

# value

ui <- dashboardPage(
  
  skin="black",
  dashboardHeader(title = "GPExploreR:Singapore Government Procurement Analysis", titleWidth = 650),
  dashboardSidebar(
    sidebarMenu(
      width=350,
      menuItem("Overview" ,tabName = "Intro", icon = icon("book")),
      menuItem("Exploratory Data Analysis" ,tabName = "EDA", icon = icon("dashboard")),
      menuItem("Network Analysis",tabName = "Network", icon = icon("globe")),
      menuItem("Text Analysis", tabName = "Text", icon = icon("address-card-o"))
    )
  ),
  
  #DB page
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = 'Intro',
        titlePanel("Overview"),
        tabsetPanel(
          tabPanel("About the project",
                   fluidRow(column(6,htmlOutput("about_proj"))))
        )
      ),
      
      tabItem(
        tabName = 'EDA',
        titlePanel("Exploratory Data Analysis"),
        tabsetPanel(
          tabPanel("Procurement Amount Overview", 
                   plotOutput("treemap_eda")),
          
          tabPanel("Procurement Pattern",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "ministryvar",
                                   label = "Ministry",
                                   choices = c("Organs of State"= "Organs of State",
                                               "Ministry of Communications and Information"="MCI",
                                               "Ministry of Foreign Affairs"="MFA",
                                               "Ministry of Law"="MINLAW",
                                               "Ministry of Manpower"="MOM",
                                               "Ministry of Finance"="MOF",
                                               "Ministry of Sustainability and the Environment"="MSE",
                                               "Ministry of National Development"="MND",
                                               "Ministry of Transport"="MOT",
                                               "Ministry of Trade and Industry"="MTI",
                                               "Ministry of Culture, Community and Youth"="MCCY",
                                               "Ministry of Social and Family Development"="MSF",
                                               "Ministry of Education"="MOE",
                                               "Ministry of Home Affairs"="MHA",
                                               "Prime Minister's Office"="PMO",
                                               "Ministry of Defence"="MINDEF",
                                               "Ministry of the Environment and Water Resources"="MEWR",
                                               "Ministry of Health"="MOH"),
                                   selected = "MOE"),
                       sliderInput(inputId = "number",
                                   label = "Number of top suppliers",
                                   min = 1,
                                   max = 50,
                                   value = 10),
                     ),
                     
                     mainPanel(
                       fluidRow(
                         column(5,
                                plotlyOutput(
                                  outputId="linePlot", 
                                  width="500px",
                                  height="500px")),
                         column(1,
                                imageOutput("Picture2")),
                         column(5,
                                plotlyOutput(
                                  outputId="Plot2", 
                                  width="500px",
                                  height="500px"))
                       )
                     )
                   )
          ),
          
          tabPanel("Unawarded Tenders",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "ministryvar2",
                                   label = "Ministry",
                                   choices = c("Organs of State"= "Organs of State",
                                               "Ministry of Communications and Information"="MCI",
                                               "Ministry of Foreign Affairs"="MFA",
                                               "Ministry of Law"="MINLAW",
                                               "Ministry of Manpower"="MOM",
                                               "Ministry of Finance"="MOF",
                                               "Ministry of Sustainability and the Environment"="MSE",
                                               "Ministry of National Development"="MND",
                                               "Ministry of Transport"="MOT",
                                               "Ministry of Trade and Industry"="MTI",
                                               "Ministry of Culture, Community and Youth"="MCCY",
                                               "Ministry of Social and Family Development"="MSF",
                                               "Ministry of Education"="MOE",
                                               "Ministry of Home Affairs"="MHA",
                                               "Prime Minister's Office"="PMO",
                                               "Ministry of Defence"="MINDEF",
                                               "Ministry of the Environment and Water Resources"="MEWR",
                                               "Ministry of Health"="MOH"),
                                   selected = "MOE")
                     ),
                     
                     mainPanel(
                       plotlyOutput("agencybar"),
                       br(),
                       fluidRow(
                         column(5,
                                plotlyOutput(
                                  outputId="totalbar", 
                                  width="550px",
                                  height="400px")),
                         column(1,
                                imageOutput("Picture1")),
                         
                         column(5,
                                plotlyOutput(
                                  outputId="unawardedyearly", 
                                  width="550px",
                                  height="400px"))
                       )
                     )
                   )),
        )
        
      ),# tab item EDA 
      
      tabItem(
        tabName = 'Network',
        titlePanel("Network Analysis"),
        fluidPage(
          tabsetPanel(
            tabPanel("Sankey Diagram",
                     column(3,
                            selectInput(inputId = "sankey_input_level",
                                        label = "Suppliers Across",
                                        choices = c("Public Sector", "Ministry", "Agency"),
                                        selected = "Public Sector"),
                            selectInput(inputId = "sankey_input_ministry",
                                        label = "Select Ministry",
                                        selected = "MCCY",
                                        choices = sort(as.vector(unique(merged_data$ministry)))),
                            selectInput(inputId = "sankey_input_agency",
                                        label = "Select Agency",
                                        selected = "Accounting and Corporate Regulatory Authority",
                                        choices = sort(as.vector(unique(merged_data$agency)))),
                            sliderInput(inputId = "sankey_input_top_suppliers", 
                                        label = "Top Number of Suppliers",
                                        min = 1,
                                        max = 50,
                                        value=20),
                            dateRangeInput(inputId = "sankey_input_award_daterange",
                                           label = "Award Date Range",
                                           start = as.Date("2018-01-01"),
                                           end = as.Date("2021-12-31"),
                                           min = as.Date("2015-01-01"),
                                           max = as.Date("2021-12-31"))
                            
                     ),
                     column(9,
                            sankeyNetworkOutput("sankeyPlot", width="100%"),
                            plotlyOutput("SupplierCustomerBar")
                     )
            ),
            
            tabPanel("Network Diagram",
                     column(3,
                            selectInput(inputId = "input_level",
                                        label = "Suppliers Across",
                                        choices = c("Public Sector", "Ministry", "Agency"),
                                        selected = "Public Sector"),
                            selectInput(inputId = "input_ministry",
                                        label = "Select Ministry",
                                        selected = "MCCY",
                                        choices = sort(as.vector(unique(merged_data$ministry)))),
                            selectInput(inputId = "input_agency",
                                        label = "Select Agency",
                                        selected = "Accounting and Corporate Regulatory Authority",
                                        choices = sort(as.vector(unique(merged_data$agency)))),
                            sliderInput(inputId = "input_top_suppliers", 
                                        label = "Top Number of Suppliers",
                                        min = 1,
                                        max = 50,
                                        value=10),
                            selectInput(inputId = "input_layout",
                                        label = "Layout",
                                        choices = layouts$layout,
                                        selected = "layout_with_fr"), 
                            selectInput(inputId = "centrality_measure",
                                        label = "Centrality Measure",
                                        choices = c("Betweenness","Closeness","Degree"),
                                        selected = "Degree"), 
                            dateRangeInput(inputId = "input_award_daterange",
                                           label = "Award Date Range",
                                           start = as.Date("2018-01-01"),
                                           end = as.Date("2021-12-31"),
                                           min = as.Date("2015-01-01"),
                                           max = as.Date("2021-12-31"))
                            
                     ),
                     
                     column(9,
                            visNetworkOutput("networkPlot"))
            )
            
          )
        ))
      ,# tab item Network
      
      
      tabItem(
        tabName = 'Text',
        titlePanel("Text Analysis Dashboard"),
        fluidPage(
          # Create a container for tab panels
          tabsetPanel(
            ## START AGENCY TAB
            tabPanel(
              title = "Text Analysis of Tender Projects by Agency",
              sidebarLayout(
                sidebarPanel(
                  h4("Filters for All Charts"),hr(),
                  radioButtons(
                    inputId = "topic",
                    label = "Project Types (Topics)",
                    choices = topics,
                    selected = topics[1]
                  ),
                  selectInput(
                    inputId = "agency",
                    label = "Specific Agency",
                    choices = agencies,
                    multiple = FALSE,
                    selected = agencies[1]
                  ),
                  
                  sliderInput(
                    "dateslider",
                    "Specific Date Range",
                    min = min_date,
                    max = max_date,
                    value =  c(min_date, min_date+240)
                  ),
                  br(),h4("Filters for Word Cloud"),hr(),
                  sliderInput("num",
                              "Maximum Number of Words:",
                              min = 10,  max = 100,  value = 50),
                  colourInput("col", "Background color", value = "white"),
                  selectInput(
                    inputId = "wordcol",
                    label = "Pick Word Colors",
                    choices = c('random-light','random-dark'),
                    multiple = FALSE,
                    selected = 'random-dark'
                  ),
                  
                  checkboxInput("remove_words", " Check to add 5 additional words that will be removed from the Word Cloud", FALSE),
                  conditionalPanel(
                    condition = "input.remove_words == 1",
                    textAreaInput("words_to_remove1", "Type your word (one per text box), a new text box will appear once typed", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                    textAreaInput("words_to_remove2", "", rows = 1)
                  ),conditionalPanel(
                    condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                    textAreaInput("words_to_remove3", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                    textAreaInput("words_to_remove4", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                    textAreaInput("words_to_remove5", "", rows = 1)
                  ),
                  
                  #actionButton("refresh", "Refresh Selection")
                  
                  
                ),
                mainPanel(
                  
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width:100%;margin-top:10px;",
                        h2("Agency Tender Descriptions Analysis Dashboard"),
                        p("Through text mninig methods , this dashboard aims to discover projects put out by different agencies based on their tender descriptions"),
                        hr(),
                    ),
                    div(style="display: inline-block;vertical-align:top; float:left;width:100%;margin-top:15px;",
                        h4("Frequency Word Cloud of Tender Descriptions"),
                        p("Note: Bigger words represent high frequnecy of appearance of a word in tender description of projects. Hover over the words to see their frequency"),
                        wordcloud2Output("cloud",width='800px'),
                        hr()
                    )
                    ,
                  ),  
                  
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width: 100%;margin-top:25px;",
                        h4("Tree Map Representing Suppliers with top 10 greatest amount award"),
                        p("Notice that for different project types, there are different sets of suppliers tendering for a project. Hover over to view their Awarded Amount"),
                        box(plotlyOutput("treemap",width='800px'), width=1000)
                    ),
                  ),hr(),
                  
                  
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width: 100%;margin-top:20px;",
                        h4("Raw Data Table showing filtered values"),
                        p("All description of the values used in the above charts are displayed here"),
                        DT::dataTableOutput("mytable", height = "30em", width='100%'))
                  )
                  
                  
                )
              )
            ),# end main agency
            tabPanel(
              'Text Analysis of Tender Projects by Suppliers',
              sidebarLayout(
                sidebarPanel(
                  h4("Filters for All Charts"),hr(),
                  radioButtons(
                    inputId = "topic_supplier",
                    label = "Project Types (Topics)",
                    choices = topics,
                    selected = topics[1]
                  ),
                  selectInput(
                    inputId = "supplier",
                    label = "Specific Supplier",
                    choices = suppliers,
                    multiple = FALSE,
                    selected = suppliers[1]
                  ),
                  
                  sliderInput(
                    "dateslider_supplier",
                    "Specific Date Range",
                    min = min_date,
                    max = max_date,
                    value =  c(min_date, min_date+240)
                  ),
                  br(),h4("Filters for Word Cloud"),hr(),
                  sliderInput("num_supplier",
                              "Maximum Number of Words:",
                              min = 10,  max = 100,  value = 50),
                  colourInput("col_supplier", "Background color", value = "white"),
                  selectInput(
                    inputId = "wordcol_supplier",
                    label = "Pick Word Colors",
                    choices = c('random-light','random-dark'),
                    multiple = FALSE,
                    selected = 'random-dark'
                  ),
                  
                  checkboxInput("remove_words_supplier", " Check to add 5 additional words that will be removed from the Word Cloud", FALSE),
                  conditionalPanel(
                    condition = "input.remove_words_supplier == 1",
                    textAreaInput("words_to_remove1_supplier", "Type your word (one per text box), a new text box will appear once typed", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words_supplier == 1 && input.words_to_remove1_supplier.length > 0",
                    textAreaInput("words_to_remove2_supplier", "", rows = 1)
                  ),conditionalPanel(
                    condition = "input.remove_words_supplier == 1 && input.words_to_remove2_supplier.length > 0",
                    textAreaInput("words_to_remove3_supplier", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words_supplier == 1 && input.words_to_remove3_supplier.length > 0",
                    textAreaInput("words_to_remove4_supplier", "", rows = 1)
                  ),
                  conditionalPanel(
                    condition = "input.remove_words_supplier == 1 && input.words_to_remove4_supplier.length > 0",
                    textAreaInput("words_to_remove5_supplier", "", rows = 1)
                  ),
                  
                  #actionButton("refresh_supplier", "Refresh Selection")
                  
                  
                ),mainPanel(
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width:100%;margin-top:10px;",
                        h2("Supplier Tender Descriptions Analysis Dashboard"),
                        p("Through text mining methods , this dashboard aims to discover projects tendered by a specific supplier based on project tendered descriptions"),
                        hr(),
                    ),
                    div(style="display: inline-block;vertical-align:top; float:left;width:100%;margin-top:15px;",
                        h4("Frequency Word Cloud of Tender Descriptions"),
                        p("Note: Bigger words represent high frequnecy of appearance of a word in tender description of projects. Hover over the words to see their frequency"),
                        box(wordcloud2Output("cloud_supplier",width='800px'),width=1000),
                        hr()
                    )
                    
                  ),
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width: 100%;margin-top:25px;",
                        h4("Tree Map Representing Agencies with top 10 greatest amount awarded from"),
                        p("Notice that for some suppliers they only deal with specific ministries and some tendering for only large projects"),
                        box(plotlyOutput("treemap_supplier",width='800px'), width=1000)
                    ),
                  ),hr(),
                  
                  fluidRow(
                    div(style="display: inline-block;vertical-align:top; float:left;width: 100%;margin-top:20px;",
                        h4("Raw Data Table showing filtered values"),
                        p("All description of the values used in the above charts are displayed here"),
                        DT::dataTableOutput("mytable_supplier", height = "30em", width='100%'))
                    
                  ),
                )
              )
              
            ),# end supplier
            
            tabPanel(
              
              'Keywords by Project Type using LDAvis',
              fluidPage(
                h3("Keywords Distribution by Project Type"), hr(),
                
                p("The four topics (found to be optimal coherence) are listed below; in brackets their relevant keywords:"),
                
                tags$ul(
                  uiOutput('list')
                ), br(),
                div(style="display: inline-block;vertical-align:top; float:left;width:100%;margin-top:30px;",
                    sliderInput("nTerms", "Number of terms to display", width = '100%',min = 20, max = 40, value = 30), 
                    visOutput('LDAvis')
                )
                
              )
              
              
            )# end LDA
            
            
          )
        )
        
        
      )# tab item Text 
      
      
    )# Tab items
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #intro
  #1.0 Overview
  
  output$'about_proj' = renderUI(
    tagList(
      tags$p("In recent years, there have been high profile cases of public sector 
              procurement fraud and corruption in Singapore, such as that involving 
              Mr Henry Foo Yung Thye, the former Deputy Group Director at the Land 
              Transport Authority (LTA). In this case, various contractors were found 
              to have provided inducements to Mr Foo to advance their business interests 
              with LTA."
      ),
      tags$p("In another case of procurement fraud, a couple cheated the Ministry of 
             Home Affairs into awarding them a contract. The couple submitted fictitious 
             bids to ensure that their bids were always lowest and selected by MHA."
      ),
      tags$p("Singapore public sector procurement is mainly done via GeBIZ, an 
              e-procurement portal where public agencies publish invitations for quotations 
              and tenders. "
      ),
      tags$p("For public sector procurement, there is a need to identify areas where 
              there is possible over-reliance on a particular supplier. Such 
              over-reliance could point to risks or possible irregularities that would 
              need to be investigated. "
      ),
      tags$p("Currently GeBiz has two separate procurement analytics tools, namely GeBIZ 
              InSIGHT and GeBIZ Management Console (GMC). GeBiz InSIGHT aims at allowing 
              procurement officers to gain insights into the potential procurement opportunities. 
              GeBIZ Management Console (GMC) aims at providing decision makers with visibility 
              of public procurement. Although these tools allow insight gathering for decision making, 
              they are aimed at public sector level. An improvement on this would be making 
              it transparent to the suppliers as well, enabling them to gain insights on 
              potential market opportunities. A single platform consisting of analytics 
              targeted at both supplier and public agencies would also improve the p
              rocurement efficiencies."
      ),
      tags$p("The objective of this project is to provide suitable visualisations for users to: "
      ),
      tags$ul(
        tags$li("Conduct exploratory data analysis to gain overall understanding of the 
                procurement pattern of both awarded and unawarded tenders at ministry 
                or agency levels"), 
        tags$li("Network analysis to identify the key interactions between public agencies
                  and suppliers, and to indentify key suppliers that are heavily relied on by 
                  the public sector"), 
        tags$li("Text mining through word cloud and frequency analysis to identify 
              the common nature of procurement of each public agency. Topic modelling 
              was also used to study the salient terms and identify project types 
              from tender description. ")
      )
    )
    
  )
  
  
  # EDA Analysis Output
  
  # treemap
  govprocurem_tree <- govprocurem %>%
    group_by(`ministry`, `agency`) %>%
    summarise(num_of_orders = n(), awarded_amount = sum(awarded_amt/1000000))%>%
    ungroup()
  
  # replace the "&" in agency entries to avoid xmlParseEntityRef: no name error
  govprocurem_tree$agency = gsub("[&]","and",govprocurem_tree$agency)
  
  output$treemap_eda <- renderPlot({
    treemap(govprocurem_tree,
            index=c("ministry","agency"),
            vSize="num_of_orders",
            vColor="awarded_amount",
            type="value",
            sortID = "awarded_amount",
            palette = "Blues",
            bg.labels=c("white"),
            title = "Government Procurement by Ministry and Agency",
            title.legend = "Awarded Amount (million S$) ",
            fontsize.legend = 12,
            fontsize.title = 14,
            fontsize.labels = 12,
            align.labels=list(
              c("center", "center"), 
              c("right", "bottom"))  
    )
  })
  # yearly plot
  output$linePlot <- renderPlotly({
    govprocurem$awardyear <- format(as.Date(govprocurem$award_date,
                                            format = "%d/%m/%Y"),"%Y")
    
    govprocurem_lineministry <- govprocurem %>%
      group_by(`ministry`, `awardyear`) %>%
      summarise(awardedamount = sum(awarded_amt/1000000))%>%
      ungroup()
    
    p1<- ggplot( govprocurem_lineministry %>% 
                   filter(ministry == input$ministryvar), 
                 aes(x=awardyear,y=awardedamount,
                     text = paste(
                       "Year=", awardyear,
                       "\nAmount=", awardedamount
                     ))) +
      geom_point(aes(y=awardedamount))+
      geom_line(aes(y=awardedamount, group =1), color = "#0072B2") +
      labs(title = "Yearly Award Amount",
           x = "Year",
           y = "Awarded Amount (Million S$)")+
      theme_minimal()+
      theme(panel.grid.major.x = element_blank(),
            plot.title = element_text(color="#0072B2", size=12, face="bold"))
    ggplotly(p1, tooltip = "text")
  })
  
  # Top suppliers by awarded amount
  output$Plot2 <- renderPlotly({
    govprocurem_2 <- govprocurem%>%
      group_by(ministry,agency, `supplier_name`)%>%
      summarise(awardedamt = sum(awarded_amt/1000000))%>%
      ungroup()
    
    p2 <- ggplot( govprocurem_2 %>%
                    filter(ministry == input$ministryvar)%>%
                    head(input$number),
                  aes(x= reorder(supplier_name,-awardedamt),
                      y=awardedamt,
                      text = paste(
                        "Supplier Name=",supplier_name,
                        "\nAmount=",awardedamt
                      ))) +
      geom_bar(stat = 'identity', fill = "#0072B2")+
      labs(title = "Top suppliers by awarded amount",
           x = "Supplier Name",
           y = "Awarded Amount (Million S$)")+
      theme_minimal()+
      theme(panel.grid.major.x = element_blank(),
            plot.title = element_text(color="#0072B2", size=12, face="bold"))+
      theme(axis.text.x = element_text(angle = 90, size = 6))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
    ggplotly(p2, tooltip = "text")
  })
  
  #Number of tenders not awarded 
  output$totalbar <- renderPlotly({
    
    #filter for not awarded 
    govprocurem_noaward <- govprocurem %>%
      filter(supplier_name == "Unknown")
    
    #extract year from date 
    govprocurem_noaward$awardyear <- format(as.Date(govprocurem_noaward$award_date,
                                                    format = "%d/%m/%Y"),"%Y")
    
    govprocurem_4 <- govprocurem_noaward[-c(1,2,4,5,7)]
    
    totalbar <- ggplot( govprocurem_5 <- govprocurem_4%>%
                          group_by(`ministry`)%>%
                          summarise(numunawarded=n()),
                        aes(x= reorder(ministry,numunawarded),
                            y=numunawarded,
                            text = paste(
                              "Ministry=",ministry,
                              "\nNo.unawarded=",numunawarded
                            ))) +
      geom_bar(stat = 'identity', fill = "#0072B2")+
      labs(title = "Total number of unawarded tenders by ministry",
           x = "Ministry",
           y = "Number of unawarded tenders")+
      theme_minimal()+
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(color="#0072B2", size=12, face="bold"))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
      coord_flip()
    ggplotly(totalbar, tooltip = "text")
  })
  
  output$unawardedyearly <- renderPlotly({
    #filter for not awarded 
    govprocurem_noaward <- govprocurem %>%
      filter(supplier_name == "Unknown")
    
    #extract year from date 
    govprocurem_noaward$awardyear <- format(as.Date(govprocurem_noaward$award_date,
                                                    format = "%d/%m/%Y"),"%Y")
    
    govprocurem_4 <- govprocurem_noaward[-c(1,2,4,5,7)]
    
    unawardedyearly<- ggplot(govprocurem_7 <- govprocurem_4%>%
                               group_by(awardyear)%>%
                               summarise(numunawarded = n()),
                             aes(x=awardyear,
                                 y=numunawarded,
                                 text = paste(
                                   "Year=",awardyear,
                                   "\nNo.unawarded=",numunawarded
                                 ))) +
      geom_point(aes(y=numunawarded))+
      geom_line(aes(y=numunawarded, group =1), color = "#0072B2") +
      labs(title = "Total number of tenders unawarded yearly",
           x = "Year",
           y = "Number of tenders unawarded")+
      theme_minimal()+
      theme(panel.grid.major.x = element_blank(),
            plot.title = element_text(color="#0072B2", size=12, face="bold"))+
      coord_flip()
    
    ggplotly(unawardedyearly, tooltip = "text")
  })
  
  output$agencybar <- renderPlotly({
    #filter for not awarded 
    govprocurem_noaward <- govprocurem %>%
      filter(supplier_name == "Unknown")
    
    #extract year from date 
    govprocurem_noaward$awardyear <- format(as.Date(govprocurem_noaward$award_date,
                                                    format = "%d/%m/%Y"),"%Y")
    
    govprocurem_4 <- govprocurem_noaward[-c(1,2,4,5,7)]
    
    agencybar <- ggplot( govprocurem_6 <- govprocurem_4%>%
                           filter(ministry == input$ministryvar2)%>%
                           group_by(`agency`)%>%
                           summarise(numunawarded=n()),
                         aes(x= reorder(agency,numunawarded),
                             y=numunawarded,
                             text = paste(
                               "Agency=",agency,
                               "\nNo.unawarded=",numunawarded
                             ))) +
      geom_bar(stat = 'identity', fill = "#0072B2")+
      labs(title = "Number of unawarded tenders by selected ministry",
           x = "Agency",
           y = "Number of unawarded tenders")+
      theme_minimal()+
      theme(panel.grid.major.y = element_blank(),
            plot.title = element_text(color="#0072B2", size=12, face="bold"))+
      coord_flip()
    ggplotly(agencybar, tooltip = "text")
  })
  
  
  # Network Analysis Output
  # SANKEY
  procurement_data <- read_csv("data/government-procurement-via-gebiz.csv")
  agency_ministry <- read_xlsx("data/AgencyMinistry.xlsx")
  
  # Join the 2 datasets
  merged_data <- merge(procurement_data, agency_ministry, by="agency")
  
  # Convert award_date string values to YYYY-MM-DD date format
  merged_data$award_date <- as.Date(merged_data$award_date, "%d/%m/%Y")
  merged_data$wog <- "Public Sector"
  
  output$sankeyPlot <- renderSankeyNetwork({
    
    # Filter by date range
    date_filtered_data<- merged_data %>% filter(award_date >= input$sankey_input_award_daterange[1]
                                                & award_date <= input$sankey_input_award_daterange[2])
    
    # Aggregate data at WOG, Ministry and Agency levels
    # WOG level aggregation
    wog_grouped_data <- date_filtered_data%>%
      group_by(wog, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    # Ministry level aggregation
    ministry_grouped_data <- date_filtered_data%>%
      group_by(ministry, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    # Agency level aggregation
    agency_grouped_data <- date_filtered_data%>%
      group_by(agency, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    if(input$sankey_input_level == "Public Sector"){
      wog_supplier_filtered_data <- top_suppliers_apply(wog_grouped_data,input$sankey_input_top_suppliers)
      top_suppliers <- as.data.frame(unique(wog_supplier_filtered_data$supplier_name))
      names(top_suppliers)[1] <- "supplier_name"
      supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name)
    }
    
    if(input$sankey_input_level == "Ministry"){
      ministry_supplier_filtered_data <- filter(ministry_grouped_data, ministry == input$sankey_input_ministry)
      ministry_supplier_filtered_data <- top_suppliers_apply(ministry_supplier_filtered_data,input$sankey_input_top_suppliers)
      top_suppliers <- as.data.frame(unique(ministry_supplier_filtered_data$supplier_name))
      names(top_suppliers)[1] <- "supplier_name"
      supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name
                                       & ministry==input$sankey_input_ministry)
    }
    
    if(input$sankey_input_level == "Agency"){
      agency_supplier_filtered_data <- filter(agency_grouped_data, agency == input$sankey_input_agency)
      agency_supplier_filtered_data <- top_suppliers_apply(agency_supplier_filtered_data,input$sankey_input_top_suppliers)
      top_suppliers <- as.data.frame(unique(agency_supplier_filtered_data$supplier_name))
      names(top_suppliers)[1] <- "supplier_name"
      supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name
                                       & agency==input$sankey_input_agency) 
    } 
    
    
    # Create links
    min_agency_links <- supplier_filtered_data%>%
      group_by(ministry, agency) %>%
      summarize(total_amt = sum(awarded_amt))
    names(min_agency_links)[1]<- "source"
    names(min_agency_links)[2]<- "target"
    min_agency_links$group<- min_agency_links$source 
    
    
    agency_supplier_links <- supplier_filtered_data%>%
      group_by(agency, supplier_name, ministry) %>%
      summarize(total_amt = sum(awarded_amt))
    names(agency_supplier_links)[1]<- "source"
    names(agency_supplier_links)[2]<- "target"
    names(agency_supplier_links)[3]<- "group" 
    
    
    links <- rbind(min_agency_links,agency_supplier_links)
    
    # Create a data frame for nodes
    nodes <- data.frame(
      name=c(as.character(links$source), 
             as.character(links$target)) %>% unique()
    )
    
    #nodes <- as.data.frame(links$source)
    #names(nodes)[1]<- "name"
    
    #nodes_target <- as.data.frame(links$target)
    #names(nodes_target)[1]<- "name"
    
    #nodes <- rbind(nodes,nodes_target)
    
    
    # Find node IDs for links
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    #links$IDsource <- IDgenerate(links$source, nodes$name)
    
    #links$IDtarget <- IDgenerate(links$target, nodes$name)
    
    san <- sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "total_amt",
      NodeID = "name",
      width = 1200,
      height = 900,
      nodeWidth = 5,
      nodePadding = 3,
      fontSize = 10,
      #sinksRight = FALSE,
      LinkGroup = "group",
      units = "SGD")
    
    clickFun <- 
      'function() { 
          d3.selectAll(".node").on("mousedown.drag", null);
          d3.selectAll(".node").on("click",function(d){ Shiny.onInputChange("id", d.name); });
        }'
    
    onRender(san, clickFun)
    
    
  })
  
  #output$SupplierCustomerBar <- renderPlotly({
  #  output$table <- DT::renderDataTable(DT::datatable(funct(input$id)))
  
  output$SupplierCustomerBar <- renderPlotly({
    
    # Filter by date range
    date_filtered_data<- merged_data %>% filter(award_date >= input$sankey_input_award_daterange[1]
                                                & award_date <= input$sankey_input_award_daterange[2])
    
    # Agency level aggregation
    agency_grouped_data <- date_filtered_data%>%
      group_by(agency, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    clickID <- input$id
    if(is.null(clickID)) return(NULL)
    
    p <- agency_grouped_data %>%
      filter(supplier_name == clickID) %>%
      ggplot(aes(x=reorder(agency,-total_amt),
                 y=total_amt,
                 text = paste(agency, "\nTotal amount: $", format(round(total_amt,1),big.mark=","))
                 
                 
      ))+
      geom_bar(stat="identity", fill="steelblue")+
      labs(title = paste("Top Customers of Supplier: ", clickID), x = "Agency", y = "Total amount($)")+
      theme(axis.text.x = element_text(angle=30,vjust=0.5,hjust=1),
            panel.background = element_rect(fill = "white"))+
      scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
    theme_minimal()
    ggplotly(p, tooltip="text")
  })
  
  #Network
  
  output$networkPlot <- renderVisNetwork({
    # Filter by date range
    date_filtered_data<- filter(merged_data, award_date >= input$input_award_daterange[1]
                                & award_date <= input$input_award_daterange[2] 
                                & tender_detail_status != "Awarded to No Suppliers")
    
    # Filter data based on WOG, Ministry or Agency level
    if(input$input_level == "Ministry"){
      date_filtered_data <- filter(date_filtered_data, ministry == input$input_ministry)}
    
    
    if(input$input_level == "Agency"){
      date_filtered_data <- filter(date_filtered_data, agency == input$input_agency)}
    
    # Aggregate awarded_amt based on Agency-Supplier
    agency_grouped_data <- date_filtered_data%>%
      group_by(ministry, agency, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    #Extract top X suppliers of each agency
    
    agencies <- as.data.frame(unique(agency_grouped_data$agency))
    names(agencies)[1] <- "agency"
    
    
    topXsuppliers <- data.frame(matrix(ncol=1, nrow=0))
    names(topXsuppliers)[1] <- "supplier_name"
    
    
    for(agencyname in agencies){
      a <- agencyname
      b <- dplyr::filter(agency_grouped_data, agency %in% a) %>%
        arrange(desc(total_amt)) %>%
        slice(1:input$input_top_suppliers)
      c <- data.frame(b$supplier_name)
      names(c)[1] <- "supplier_name"
      topXsuppliers <- rbind(topXsuppliers,c)}
    
    # filter agency_grouped_data based on top suppliers
    agency_records_with_topXsuppliers <- dplyr::filter(agency_grouped_data, supplier_name %in% topXsuppliers$supplier_name)
    
    # Generate list of nodes
    
    agency_nodes <- agency_records_with_topXsuppliers %>% 
      summarise(name =agency) %>%
      unique()
    
    agency_nodes$type <- agency_nodes$ministry
    agency_nodes <- subset(agency_nodes, select=-c(ministry, agency))
    
    supplier_nodes <- subset(agency_records_with_topXsuppliers, select=-c(ministry, agency, total_amt))
    supplier_nodes <- supplier_nodes %>% 
      summarise(name = supplier_name) %>%
      unique()
    supplier_nodes$type <- "Supplier"
    
    all_nodes <- rbind(agency_nodes , supplier_nodes)
    all_nodes$id <- as.integer(rownames(all_nodes))
    all_nodes <- all_nodes %>%
      rename(group = type, label = name)
    all_nodes$title <- all_nodes$label
    
    # Edges
    
    n_edges <- agency_records_with_topXsuppliers
    n_edges <- rename(n_edges, sourceLabel = agency, targetLabel = supplier_name)
    n_edges <- subset(n_edges, select = -c(ministry))
    n_edges <- left_join(n_edges, all_nodes, by = c("sourceLabel"="label")) %>%
      select(sourceLabel,targetLabel,total_amt,id)
    n_edges <- rename(n_edges, source = id)
    n_edges <- left_join(n_edges, all_nodes, by = c("targetLabel"="label")) %>%
      select(sourceLabel,targetLabel,total_amt, source, id)
    n_edges <- rename(n_edges, target = id)
    
    
    ########## Static graph
    static_graph <- tbl_graph(nodes = all_nodes,
                              edges = n_edges, 
                              directed = TRUE)
    
    # Interactive network graph
    # data prep
    n_edges_aggregated <- n_edges %>%
      left_join(all_nodes, by = c("sourceLabel" = "label")) %>%
      rename(from = id) %>%
      left_join(all_nodes, by = c("targetLabel" = "label")) %>%
      rename(to = id) %>%
      group_by(from, to) %>%
      filter(total_amt > 1) %>%
      ungroup()
    
    
    n_edges_aggregated$Weight <- n_edges_aggregated$total_amt
    n_edges_aggregated$title <- paste("$", formatC(n_edges_aggregated$Weight, big.mark = ",", format = "f", digits = 0))
    n_edges_aggregated$color.opacity <- 0.1*n_edges_aggregated$Weight/100000
    
    ####################
    
    
    all_nodes$betweenness_centrality <- betweenness(static_graph)
    all_nodes$degree_centrality <- degree(static_graph)
    all_nodes$closeness_centrality <- closeness(static_graph, normalized=TRUE)
    
    
    # Assigning degree centrality measures to node size 
    all_nodes$size <- degree(static_graph)/2
    
    if(input$centrality_measure == "Betweenness"){centrality<- "betweenness_centrality"} 
    if(input$centrality_measure == "Closeness"){centrality<- "closeness_centrality"} 
    if(input$centrality_measure == "Degree"){centrality<- "degree_centrality"}
    
    
    
    # visNetwork
    visNetwork(all_nodes, n_edges_aggregated, width="100%", height = "100%", main = "Network") %>%
      visGroups(groupname = "Supplier", shape = "triangle", color = "orange") %>%
      visIgraphLayout(layout = input$input_layout) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE,
                 selectedBy = centrality) %>%
      visNodes(label="")%>%
      visInteraction() %>%
      visLegend() %>%
      visLayout(randomSeed = 123) %>%
      visEdges(color = palette()[n_edges_aggregated$Weight])
    
  })
  
  
  # Text Analysis Output
  
  ## TEXT ANALYSIS
  observeEvent(input$dateslider,{
    if (input$dateslider[[2]] - input$dateslider[[1]] < 240){
      values <- c(input$dateslider[[1]], input$dateslider[[1]]+240)
      updateSliderInput(session, "dateslider", min =min_date,max=max_date, value = values)
      
    }
    
  }) 
  
  data_source <- reactive({
    
    if(input$agency == 'all'){
      data_raw <- df %>%
        filter(topic == input$topic ) %>%
        filter(award_date >= input$dateslider[[1]] & award_date <= input$dateslider[[2]]) 
      
    }else{
      data_raw <- df %>%
        filter(topic == input$topic ) %>%
        filter(agency == input$agency) %>%
        filter(award_date >= input$dateslider[[1]] & award_date <= input$dateslider[[2]]) 
      
    }
    
    return(data_raw)
  })
  
  
  data_source_lda <- reactive({
    df_ldavis <- df
    # LDA viz 
    df_ldavis$doc_id <- 1:nrow(df)
    
    df_text<- df_ldavis %>%
      select(doc_id,tender_description,agency)
    names(df_text) <- c('doc_id','text','document')
    
    df_text$document = paste0(df_text$document, '_', df_text$doc_id)
    
    # split into words
    df_dtm <- df_text %>%
      unnest_tokens(word, text)
    
    # find document-word counts
    df_dtm <- df_dtm %>%
      anti_join(stop_words) %>%
      count(document, word, sort = TRUE)
    
    df_dtm  <- df_dtm %>%
      cast_dtm(document, word, n) 
    
    # CHANGE NUMBER OF TOPICS BY CHANGING K 
    dflda <- LDA(df_dtm, k = 4, control = list(seed = 1234))
    return(dflda)
    
  })
  
  # LDA output
  output$LDAvis <- renderVis({
    if(!is.null(input$nTerms)){
      topicmodels2LDAvis(data_source_lda(), input$nTerms)
    } 
  })
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a  of word frequencies
    
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, c("ltd","pte","contracts","nov", 'acra', 'singapore', 'work', 'republic','board'))
    corpus <- tm_map(corpus, removeWords, as.vector(strsplit(input$agency, "\\s+")[[1]]))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
    
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    validate(need(nrow(data) > 3,  "Select Another Agency or Increase Date Range Not enough Data Points")) 
    
    wordcloud2a(data, backgroundColor = input$col, color = input$wordcol)
  }
  
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = as.numeric(input$num),
                     background = input$col
    )
  })
  
  ## raw table 
  
  
  output$mytable = DT::renderDataTable(
    data_source(),
    server = FALSE,
    selection = list(mode = "multiple", target = "column", selected = c(1)),
    options = list(pageLength = 5, autoWidth = TRUE)
  )
  
  
  
  output$treemap <- renderPlotly({
    map <- data_source()
    map <- map %>%
      select(supplier_name,awarded_amt) %>%
      group_by(supplier_name) %>%
      summarise(count = n(), awarded_amt = sum(awarded_amt))%>%
      top_n(10)
    
    print(map$supplier_name)
    label_text = lapply(map$count, as.character)
    map$label_text <- paste("Tendered", label_text, "Project" , sep=" ")
    
    fig <- plot_ly(
      map,
      labels = ~ supplier_name,
      parents = NA,
      values = ~ awarded_amt,
      #values = ~ count, #Number of Projects Tendered
      name = "name1",
      type = 'treemap',
      #text = ~ label_text,
      hovertemplate = "Supplier: %{label}<br> Awarded Amount: %{value}<extra></extra>"
    )%>% layout( paper_bgcolor = 'lightcyan')
    
    fig
    
  })
  
  ## start supplier
  
  
  data_source_supplier <- reactive({
    
    if(input$supplier == 'all'){
      data_raw_s <- df %>%
        filter(topic == input$topic_supplier ) %>%
        filter(award_date >= input$dateslider_supplier[[1]] & award_date <= input$dateslider_supplier[[2]]) 
      
    }else{
      data_raw_s <- df %>%
        filter(topic == input$topic_supplier ) %>%
        filter(supplier_name == input$supplier) %>%
        filter(award_date >= input$dateslider_supplier[[1]] & award_date <= input$dateslider_supplier[[2]]) 
      print(data_raw_s)
    }
    
    return(data_raw_s)
  })
  
  
  create_wordcloud_s <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a  of word frequencies
    
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, c("ltd","pte","contracts","nov", 'acra', 'singapore', 'work', 'republic','board'))
    corpus <- tm_map(corpus, removeWords, as.vector(strsplit(input$agency, "\\s+")[[1]]))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1_supplier))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2_supplier))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3_supplier))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4_supplier))
    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5_supplier))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
    
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data_s <- head(data, n = num_words)
    if (nrow(data_s) == 0) {
      return(NULL)
    }
    print(data_s)
    validate(need(nrow(data_s) > 3,  "Select Another Agency or Increase Date Range Not enough Data Points")) 
    
    wordcloud2a(data_s, backgroundColor = input$col_supplier, color = input$wordcol_supplier)
  }
  
  output$cloud_supplier <- renderWordcloud2({
    create_wordcloud_s(data_source_supplier(),
                       num_words = as.numeric(input$num_supplier),
                       background = input$col_supplier
    )
  })
  
  
  
  output$treemap_supplier <- renderPlotly({
    map_s <- data_source_supplier()
    map_s <- map_s %>%
      select(agency,awarded_amt) %>%
      group_by(agency) %>%
      summarise(count = n(), awarded_amt = sum(awarded_amt))%>%
      top_n(10)
    print(map_s)
    label_text = lapply(map_s$count, as.character)
    map_s$label_text <- paste("Tendered", label_text, "Project" , sep=" ")
    
    #These are the layout attributes for Y
    
    
    fig <- plot_ly(
      map_s,
      labels = ~ agency,
      parents = NA,
      values = ~ awarded_amt,
      #values = ~ count, #Number of Projects Tendered
      name = "name1",
      type = 'treemap',
      color_continuous_scale='balance',
      showlegend=TRUE,
      #text = ~ label_text,
      hovertemplate = "Agency: %{label}<br> Awarded Amount: %{value}<extra></extra>"
    )  %>% layout( paper_bgcolor = 'lightcyan')
    
    
    fig
    
  })
  
  ## raw table 
  output$mytable_supplier = DT::renderDataTable(
    data_source_supplier(),
    server = FALSE,
    selection = list(mode = "multiple", target = "column", selected = c(1)),
    options = list(pageLength = 5, autoWidth = TRUE)
  )
  ## refresh button
  # observeEvent(input$refresh_supplier, {
  #   js$refresh();
  # })
  # 
  
  
  ### END SUPPLIER
  
  ## refresh button
  # observeEvent(input$refresh, {
  #   updateNumericInput(session, "mynumber", value = 20)
  #   updateTextInput(session, "mytext", value = "test")
  # })
  
  df_list <- data.frame(item = c('1: Installation and Manpower ("Train","Install","Build","Appoint","Maintance")',
                                 '2: Planning and Management ("Framework","Conduct","Manage","Project","Communicate")',
                                 '3: Construction and System ("Build","Develop","System","Engine","Program")',
                                 '4: Public Facilities ("School","Ministry","Government","Water","Road")'), num = c(1,2,3,4))
  output$list <- renderUI({
    apply(df_list, 1, function(x) tags$li("Topic ",x['item']))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
