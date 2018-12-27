library(shiny)
library(shinythemes)
library(tm)
library(wordcloud)
library(memoise)
library(xts)
library(dygraphs)
library(data.table)

setwd()
getwd()

# To read Dataset
TableView <- read.csv("NewSocial.csv",header=TRUE)
TableView <- as.data.frame(TableView)
print(str(TableView))

txtm_bag <- fread("TMBagofWords.csv")
print(str(txtm_bag))
# To Show App Category Type
apptype <<- list("SOCIAL","SHOPPING","EDUCATION","GAMES")

books <<- list("Positive User Reveiw" = "positive",
               "Negative User Review" = "negative",
               "Filtered Bag of Positive Words" = "filteredpositve",
               "Filtered Bag of Negative Words" = "filterednegative")


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  
  if (!(book %in% books))
   
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt.gz", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords(""), "thy", "is","thou","he","she","was","it","is","were","should",
                      "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})



#############################################################################################################
#############################           R Shiny Web Apps          ###########################################

# Define UI for data upload app ----
ui <- fluidPage(
  theme = shinytheme('cerulean'),  # <--- To use a theme, uncomment this
  # App title ----
  titlePanel("Text Mining of Google Play Store User Reviews with Bag of words! "),
  
  navbarPage("SOCIAL",
             
             
             
             
             
             #######
             # 1st Navigation Bar
             tabPanel("View Dataset",
                      "TableView",
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          
                          conditionalPanel(
                            'input.dataset === "socialdata"'
                           
                          ),
                          selectInput("select_apptype", "Choose Dataset:",choices = apptype)   ,
                       
                          helpText("Current Dataset is SOCIAL , Other type have to do it in feature! "),
                          
                          
                          tags$hr() ,
                          
                          tags$hr() ,
                          
                          
                          "Visualization",
                          sliderInput("priceInput", "TotalUserRating", 1, 5, c(2, 4.4)),
                          helpText("The Slider control work for Visualize Tab! "),
                          
                          radioButtons("typeInput", "Paid_Type",
                                       choices = c("Free", "Paid"),
                                       selected = "Paid"),
                          
                          selectInput("appInput", "TotalAppDownload",
                                      choices = c("1,00,000,", "5,00,000", "10,00,000","50,00,000",
                                                  "10,000,000", "50,000,000", "10,000,000"))
                          
                          
                          
                          ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          tabsetPanel(
                            id = 'dataset',
                            tabPanel("TableView", DT::dataTableOutput("mytable1")),
                            tabPanel("Visualization", plotOutput("mytable2"),
                                     
                                     ######## For Graph #######
                                     plotOutput("visualplot1", height = 350),
                                     plotOutput("visualplot2",height = 350),
                                     plotOutput("visualplot3"),
                                     #plotOutput("visualplot4"),
                                     #plotOutput("visualplot5"),
                                     br(), br()
                                     #plotOutput("visualplot6",height=350)      
                           
                                     ),
                            tabPanel("Summary", verbatimTextOutput("mytable3"))
                            
                          )
                        )
                        
                      )
             ),
             ########
             
             # add new tab in navbar model
             # 2nd Navigation Bar
             tabPanel("Exploration",
                      
                      titlePanel("Data Exploration "),
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          selectInput("selection", "Choose Bag Option:",choices = books),
                          actionButton("update", "Change"),
                          hr(),
                          sliderInput("freq","Minimum Frequency:",
                                      min = 1,  max = 100, value = 15),
                          sliderInput("max", "Maximum Number of Words:",
                                      min = 1,  max = 500,  value = 100) 
                          
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Data file ----
                          plotOutput("plot")
                          
                        )
                        
                      )
             ),
             
             
             # add new tab in navbar model
             # 3rd Navigation Bar
             tabPanel("Bag-of-Words",
                      
                      titlePanel("Find Word in Bags "),
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          textInput("textmin", "Enter a Word", value = ""),
                          submitButton("Submit"),
                          br(),
                          p("Note: Allows lower case of single word,Not accept sentences"),
                          p("The User Input Words are checked with Bags of Words!")
                          # textInput("txt", "Word Prediction:", "Type Some word"),
                          # hr(),
                          # actionButton("action2", "Predict!", class = "btn-primary") 
                          
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Data file ----
                          #plotOutput("plot")
                          dygraphOutput("dygraph")
                          
                        )
                        
                      )
             ),
             
             
             
             # add new tab in navbar model
             navbarMenu("More",
                        
                        # add new tab in navbar model
                        # 4th Navigation Bar
                        tabPanel("Upload",
                                 # Sidebar layout with input and output definitions ----
                                 sidebarLayout(
                                   
                                   # Sidebar panel for inputs ----
                                   sidebarPanel(
                                     
                                     # Input: Select a file ----
                                     fileInput("file1", "Choose CSV File",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     
                                     # Horizontal line ----
                                     tags$hr(),
                                     
                                     # Input: Checkbox if file has header ----
                                     checkboxInput("header", "Header", TRUE),
                                     
                                     # Input: Select separator ----
                                     radioButtons("sep", "Separator",
                                                  choices = c(Comma = ",",
                                                              Semicolon = ";",
                                                              Tab = "\t"),
                                                  selected = ","),
                                     
                                     # Input: Select quotes ----
                                     radioButtons("quote", "Quote",
                                                  choices = c(None = "",
                                                              "Double Quote" = '"',
                                                              "Single Quote" = "'"),
                                                  selected = '"'),
                                     
                                     # Horizontal line ----
                                     tags$hr(),
                                     
                                     # Input: Select number of rows to display ----
                                     radioButtons("disp", "Display",
                                                  choices = c(Head = "head",
                                                              All = "all"),
                                                  selected = "head")
                                     
                                   ),
                                   
                                   # Main panel for displaying outputs ----
                                   mainPanel(
                                     
                                     # Output: Data file ----
                                     tableOutput("contents")
                                     
                                   )
                                   
                                 )    
                        ),
                        # add new tab in navbar model
                        # 5th Navigation Bar
                        tabPanel("About",
                                 
                                 tags$h1("About this App!"),
                                 p("The Application shows with Bags-of-Words Approaches ,Load dataset ,Load User reviews text files ,etc., Word Cloud, Plot , Slider Control, etc.,"),
                                 p("1 .View Dataset "),
                                 p("2 .Data Exploration "),
                                 p("3 .Bags-of-words "),
                                 p("4 . More")
                                 
                                 )
             )
             
             # add new tab in navbar model
             
         
             
  )
)
# Define server logic to read selected file ----
server <- function(input, output,session) {
  
  output$contents <- renderTable({
   
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
    
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  #### Word Cloud Button change
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  

  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  ##
  # TableView
  #social2 = socialdata[sample(nrow(socialdata), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(TableView, options = list(orderClasses = TRUE))
  })
  
  
  # Visualization
  output$mytable2 <- renderPlot({
    
    
    #1: Number of Apps in Paid Category
    filtered <-
      TableView %>%
      group_by(Paid_Type) %>%
      summarise(Number = n_distinct(AppName))
    
    ggplot(filtered, aes(reorder(Paid_Type,-Number), Number, fill = Paid_Type)) + geom_bar(stat = 'identity') + xlab('Category')  +scale_fill_brewer(palette = 'Paired') +
      theme(plot.title = element_text(hjust = 0.5), legend.position="none") + ggtitle('Number of App in Paid Category')
    
    
   

    
    })
  
  ###### Child plot
  output$visualplot1 <- renderPlot({
    
    apps_cor = filter(TableView, Price<150)
    ggplot(apps_cor, aes(x =Price , y = TotalUserRating)) + geom_point(color = 'RED') + ggtitle('Correlation Between Rating and Price (0.05)') +
      theme(plot.title = element_text(hjust = 50))
    
  
    ####
    
    
    
  })
  
  #2 : Total Number of Paid Type Category Visualize =
  output$visualplot2 <- renderPlot({
    
   
    
    apps_paid = TableView %>%
      filter(Price != 0) 
    
    apps_paid$Price = cut(apps_paid$Price, breaks=c(0.5, 1, 2, 3, 4, 6, 10, 30, 60, 100))
    labels1 = c('0.5-0.99', '1-2', '2-3', '3-4', '4-6', '6-10', '10-30', '30-60', '60-100', '100-400')
    
    ggplot(apps_paid, aes(Price)) + geom_bar(stat = 'count', fill = "CYAN") +scale_x_discrete(labels = labels1) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) + ggtitle('App Price Distribution') + xlab('Price in (Euro)')
    
    
  })
  
  
  
  #3 : Price Distribution Visualize
  output$visualplot3 <- renderPlot({
    
    
    #plot(TableView)
    plot.data <- melt(TableView, id.vars = 'TotalAppDownload')
    #not sure if input$cnt is a list or a vector
    #may need to manipulate that before passing
    plot.data <- plot.data[plot.data$variable %in% input$cnt, ]
    ggplot(plot.data) +
      geom_line(mapping = aes(x = TotalAppDownload, y = value, colour = variable)) +
      labs (x = "Price", y = "TotalAppDownload", title = "TotalAppDownload") +
      scale_colour_discrete(name = "AppName")
    
  })
  
  
  
  #4 : Total User Rating with Price Visualize
  output$visualplot4 <- renderPlot({
    
    
    filtered <-
      TableView %>%
      filter(TotalUserRating >= input$priceInput[1],
             TotalUserRating <= input$priceInput[2],
             Paid_Type == as.character(input$typeInput),
             TotalAppDownload == input$appInput
             
             
      )
    head(TableView,6)
    
    # 1: General Visualize of Rating with Free/Paid
    #ggplot(filtered, aes(Alcohol_Content)) + geom_histogram()
    ggplot(filtered, aes(AppSize)) + geom_bar(stat = "Count",colour = "Yellow",fill="ORANGE") + labs(x = "App Size")   
    
     
  })
  
  
  #5 : Other
  output$visualplot5 <- renderPlot({
    
    
    print(paste("input$typeInput: ", input$typeInput))
    print(paste("input$appInput: ",input$appInput))
    
    filtered <-
      TableView %>%
      filter(TotalUserRating >= input$priceInput[1],
             TotalUserRating <= input$priceInput[2],
             #Type == input$typeInput,
             #Country == input$countryInput
             Paid_Type == as.character(input$typeInput),
             TotalAppDownload == input$appInput
             
             
      )
    #head(TableView,6)
    
    # 1: General Visualize of Rating with Free/Paid
    #ggplot(filtered, aes(Alcohol_Content)) + geom_histogram()
    ggplot(filtered, aes(AppSize)) + geom_bar(stat = "Count",colour = "Yellow",fill="ORANGE") + labs(x = "App Size")
    
    
    
    
  })
  
  
  # Summary Details
  output$mytable3 <- renderPrint({
    summary(head(TableView))
    #summary(TableView)
  })
  
  # WordFrequency Details
  output$mytable4 <- renderPlot({
    
  })
  
  
  
  ##### Text Minig Word Bag Prediction
  enter_inwrd <- reactive({
    validate(
      need(input$textmin != "", "Once you type a single word ,Plot will be display here !")
    )
    select_city <- subset(txtm_bag, TxtMin_WordBag == input$textmin)
    validate(
      # Alert message
      need(input$textmin %in% select_city$TxtMin_WordBag, "Typed-word was  not found in the text mining bag. Please enter another word.")
    )
    select_city[, .(FREQUENCY, Value)]
    xts(select_city$FREQUENCY,  as.Date(select_city$Value))
    
    
    
    
  })
  
    output$dygraph <- renderDygraph({
  
    dygraph(enter_inwrd(),
            main = paste("Typed Word",input$textmin,"is Matched in Text Mining Bag-Words !")) %>%
      dyAxis("y", label = "Frequency Value(C)") %>%
      #dyAxis("y2", label = "PredictType", independentTicks = TRUE) %>%
      dySeries("V1", label = "Frequency of the Word is:",fillGraph = TRUE,axis = 'y') %>%
      dyRangeSelector(height = 20, strokeColor = "GREEN") %>%
      dyLegend(show = "follow")
   
    
  })
  

   
  
}

# Create Shiny app ----
shinyApp(ui, server)