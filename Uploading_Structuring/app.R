#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Defining libraries
library(dplyr)
library(purrr)
library(stringr)
library(readxl)
library(writexl)

# Helpers, external files

source("helpers/arianna.R")
source("helpers/wizzad.R")

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading and Downloading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel( width = 3,
            
            # Input: Select a file ----
            fileInput("file1", "Choose xls or xlsx File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".xls",
                                 ".xlsx")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            #checkboxInput("header", "Header", TRUE),

            # Input: Select separator ----
            # radioButtons("sep", "Separator",
            #              choices = c(Comma = ",",
            #                          Semicolon = ";",
            #                          Tab = "\t"),
            #              selected = ","),

            # Input: Select quotes ----
            # radioButtons("quote", "Quote",
            #              choices = c(None = "",
            #                          "Double Quote" = '"',
            #                          "Single Quote" = "'"),
            #              selected = '"'),

            # Horizontal line ----
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     Tail = "tail"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),
            
            # Downdload Button
            downloadButton("dl", "Download")
            
        ),
        
            
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    # reactive data
    Data <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or tail if selected, will be shown.
        file1 <- input$file1
        req(file1)
       
        
        # reading xlsx files
        tryCatch(
            {
                df <- read_excel(file1$datapath)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        # deciding which data select depending on file's name
        if(str_detect( tolower(file1$name),"wizzad")) {
            get_wizzad_data(df)
        }
        else {
            get_arianna_data(df)
        }
        

    })
    
    # render table
    output$contents <- renderTable({
        
        # ht is head or tail from data
        ht <- input$disp
        
        if(ht == "head") {
           head(Data() ,5)
        }
        else {
            Data()  %>% tail(5) 
        }
        
       
    })
    
    output$dl <- downloadHandler(
        
        filename = function(file1) {
            file1 <- input$file1
            file_name <- str_split(file1$name,"_")[[1]][1]
            paste0("Telecom_Competitive_", file_name, ".xlsx") 
        },
        content = function(file) {write_xlsx(Data(), path = file)}
    )
    
}

# Create Shiny app ----
shinyApp(ui, server)
