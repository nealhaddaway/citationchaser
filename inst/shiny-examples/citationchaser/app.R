
#

library(shiny)
library(DT)
library(shinycssloaders)
library(data.table)

source('functions.R')

articles_eg <- read.csv('www/articles.csv', stringsAsFactors = FALSE)
references_eg <- read.csv('www/references.csv', stringsAsFactors = FALSE)
references_report_eg <- read.table('www/report_ref.txt')[1,1]
citations_eg <- read.csv('www/citations.csv', stringsAsFactors = FALSE)
citations_report_eg <- read.table('www/report_cit.txt')[1,1]

# Define UI for application that draws a histogram
ui <- navbarPage("citationchaser",
    
                 # Sidebar with a slider input for number of bins 
                 tabPanel("Home",
                          fluidRow(
                              column(10,
                                     h3('Welcome to citationchaser!'),
                                     br(),
                                     'In searching for research articles, we often want to obtain lists of references from across studies, and also obtain lists of articles that cite a particular study. In systematic reviews, this supplementary search technique is known as "citation chasing": forward citation chasing looks for all records citing one or more articles of known relevance; backward ciation chasing looks for all records referenced in one or more articles.',
                                     br(),br(),
                                     'Traditionally, this process would be done manually, and the resulting records would need to be checked one-by-one against included studies in a review to identify potentially relevant records that should be included in a review.',
                                     br(),br(),
                                     'This package contains functions to automate this process by making use of the Lens.org API. An input article list can be used to return a list of all referenced records, and/or all citing records in the Lens.org database (consisting of PubMed, PubMed Central, CrossRef, Microsoft Academic Graph and CORE; ',
                                     tags$a(href="https://www.lens.org", "read more here."), 'USERS MUST OBTAIN A TOKEN FOR THE LENS.ORG SCHOLARLY API ',
                                     tags$a(href="https://www.lens.org/lens/user/subscriptions#scholar", "(available for free here)."),'Consider asking your library to support Lens.org to continue to enable Open Discovery of research articles.'),
                              column(2,
                                     br(),tags$img(height = 200, src = "https://github.com/nealhaddaway/citationchaser/blob/master/inst/extdata/citationchaser.png?raw=true")),
                              column(12,
                                     br(),
                                     h4('Follow these steps to start chasing!'),
                                     icon("arrow-right"),' Obtain a Lens.org scholarly API token',tags$a(href="https://www.lens.org/lens/user/subscriptions#scholar", "(here)"),
                                     br(),br(),
                                     icon("arrow-right"),' In the "Article input" tab, paste a list of article identifiers (e.g. DOIs) and your API token',
                                     br(),br(),
                                     icon("arrow-right"),' Check the articles returned are the ones your interested in',
                                     br(),br(),
                                     icon("arrow-right"),' If you want to perform backward citation chasing (which articles did my articles reference?) then proceed to the "References" tab and click "Search for all referenced articles in Lens.org"',
                                     br(),br(),
                                     icon("arrow-right"),' If you want to perform forward citation chasing (which articles have cited my articles?) then proceed to the "Citations" tab and click "Search for all citing articles in Lens.org"',
                                     br(),br(),
                                     icon("arrow-right"),' You can download in RIS format a list of your input articles, referenced articles, and citing articles for easy integration with your reference/review management workflow',
                                     hr(),
                                     'If you\'d like to cite this work, please use:',
                                     br(),
                                     'Haddaway, N. R. (2021) citationchaser: an R package for forward and backward citations chasing in academic searching. doi:', tags$a(href="https://www.doi.org/10.5281/zenodo.4533747", "10.5281/zenodo.4533747"),
                                     br(),
                                     icon("save"),tags$a(href="citation.ris", "Download package citation (.ris)", download=NA, target="_blank"),
                                     br(),br(),
                                     icon("github"),tags$a(href="https://github.com/nealhaddaway/citationchaser", "See the GitHub repository")
                                     )
                          )
                 ),
                 
    # Sidebar with a slider input for number of bins 
    tabPanel("Article input",
        fluidRow(
            column(12,
                   h4('Enter the articles that you want to start from. We will first check the full citations in the Lens.org database.'),
                   br(),
                   splitLayout(textAreaInput("article_ids","Article identifiers (comma separated)", placeholder = "separate identifiers with a comma", width = '90%'),
                               selectInput("type", "Identifier type:",
                                           c("DOI" = "doi",
                                             "PUbMed ID" = "pmid",
                                             "PUbMed Central ID" = "pmcid",
                                             "CORE ID" = "coreid",
                                             "Microsoft Academic ID" = "magid",
                                             "Article title" = "title"), width = '50%'),
                               tags$head(tags$style(HTML("
                                                       .shiny-split-layout > div {
                                                       overflow: visible;
                                                       }
                              "))),
                               cellWidths = c(500,400)),
                   textInput("token", "Paste your Lens.org token here:"),
                   'Visit Lens.org to obtain a token for access to their scholarly API',
                   br(),
                   br(),
                   actionButton("find_inputs", "Check my input articles"),
                   actionButton("eg_inputs", "See example input articles", class = "btn-info"),
                   actionButton("reset", "Reset", class = "btn-warning")
                   ),
            
            # Show a plot of the generated distribution
            column(12,
                   h3('Your input articles'),
                   dataTableOutput('article_ref'),
                   br(),
                   conditionalPanel(
                       condition='input.find_inputs!=null && input.find_inputs!=""',
                       downloadButton('input_ris', 'Download an RIS file of your articles (including abstracts)')),
            )
        )
    ),
    tabPanel("References",
             fluidRow(
                 column(12,
                        h3('References from your articles (backward citation chasing)'),
                        actionButton("find_refs", "Search for all referenced articles in Lens.org"),
                        actionButton("eg_refs", "See example referenced articles in Lens.org", class = "btn-info"),
                        actionButton("reset2", "Reset", class = "btn-warning"),
                        br(),br(),
                        textOutput('refs_report'),
                        br(),
                        conditionalPanel(
                            condition='input.find_refs!=null && input.find_refs!=""',
                            downloadButton('refs_ris', 'Download an RIS file of referenced articles (including abstracts)')),
                        br(),
                        dataTableOutput('references')
                 )
             )
    ),
    tabPanel("Citations",
             fluidRow(
                 column(12,
                        h3('Citations of your articles (forward citation chasing)'),
                        actionButton("find_cits", "Search for all citing articles in Lens.org"),
                        actionButton("eg_cits", "See example citing articles in Lens.org", class = "btn-info"),
                        actionButton("reset3", "Reset", class = "btn-warning"),
                        br(),br(),
                        textOutput('cits_report'),
                        br(),
                        conditionalPanel(
                            condition='input.find_cits!=null && input.find_cits!=""',
                            downloadButton('cits_ris', 'Download an RIS file of citing articles (including abstracts)')),
                        br(),
                        dataTableOutput('citations')
                 )
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rv <- reactiveValues()
    
    # load examples
    observeEvent(input$eg_inputs,{
        rv$articles <- articles_eg
        rv$articles_ris <- unlist(read.table('www/articles.ris')[2])
    })
    observeEvent(input$eg_refs,{
        rv$refs_display <- references_eg
        rv$refs_report <- references_report_eg
        rv$refs_ris <- unlist(read.table('www/references.ris')[2])
    })
    observeEvent(input$eg_cits,{
        rv$cits_display <- citations_eg
        rv$cits_report <- citations_report_eg
        rv$cits_ris <- unlist(read.table('www/citations.ris')[2])
    })
    
    # reset button
    observeEvent(input$reset,{
        rv$articles <- NULL
        rv$articles_ris <- NULL
    })
    observeEvent(input$reset2,{
        rv$refs_display <- NULL
        rv$refs_report <- NULL
        rv$refs_ris <- NULL
    })
    observeEvent(input$reset3,{
        rv$cits_display <- NULL
        rv$cits_report <- NULL
        rv$cits_ris <- NULL
    })
    
    # build articles table
    observeEvent(input$find_inputs,{
        article_ref <- get_citation(input$article_ids, 
                                    type = input$type, 
                                    token = input$token)
        rv$articles <- article_ref$display
        rv$articles_ris <- article_ref$ris
    })
    # render articles table
    output$article_ref <- renderDataTable({
        rv$articles
    }, options = list(dom = 'tpl'), rownames = FALSE)
    # download articles as RIS
    output$input_ris <- downloadHandler(
        filename = function(){
            paste("articles-", Sys.Date(), ".ris", sep = "")
        },
        content = function(file) {
            write.table(rv$articles_ris, file,col.names=FALSE)
        }
    )
    
    # build references table
    observeEvent(input$find_refs,{
        references <- get_refs(input$article_ids, 
                               get_records = 'references',
                               type = input$type, 
                               token = input$token)
        rv$refs_display <- references$display
        rv$refs_report <- references$report
        rv$refs_ris <- references$ris
    })
    # render references table
    output$references <- renderDataTable({
        rv$refs_display
    }, rownames = FALSE, options = list(dom = 'tpl'))
    # render report text
    output$refs_report <- renderText({
        rv$refs_report
    })
    # download references as RIS
    output$refs_ris <- downloadHandler(
        filename = function(){
            paste("references-", Sys.Date(), ".ris", sep = "")
        },
        content = function(file) {
            write.table(rv$refs_ris, file, col.names=FALSE)
        }
    )
   
    # build citations table
    observeEvent(input$find_cits,{
        citations <- get_refs(input$article_ids, 
                              get_records = 'citations',
                              type = input$type, 
                              token = input$token)
        rv$cits_display <- citations$display
        rv$cits_report <- citations$report
        rv$cits_ris <- citations$ris
    })
    # render citations table
    output$citations <- renderDataTable({
        rv$cits_display
    }, rownames = FALSE, options = list(dom = 'tpl'))
    # render report text
    output$cits_report <- renderText({
        rv$cits_report
    })
    # download references as RIS
    output$cits_ris <- downloadHandler(
        filename = function(){
            paste("citations-", Sys.Date(), ".ris", sep = "")
        },
        content = function(file) {
            write.table(rv$cits_ris, file, col.names=FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
