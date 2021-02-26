
#

library(shiny)
library(DT)
library(shinycssloaders)
library(data.table)
library(tibble)
library(dplyr)
library(httr)
library(expss)
library(scales)
library(tidyr)
library(networkD3)
library(stringr)
library(shinybusy)

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
                                     tags$a(href="https://www.lens.org/lens/user/subscriptions#scholar", "(available for free here)."),
                                     br(),br(),
                                     'Please note: The Lens API limits single exports to 1,000 records. This app splits your search into chunks to avoid hitting the limit, but any single record with more than 1,000 citations (or references) will produce only the first 1,000 records.',
                                     'Large searches may take several minutes to complete, so please be patient.',
                                     br(),br(),
                                     'Consider asking your library to support Lens.org to continue to enable Open Discovery of research articles.'),
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
                                     tags$img(height = 30, src = "https://static.lens.org/lens/7.2.3/img/home-page/Lens-logo-tagline.png"),
                                     br(),
                                     'The Lens is a service provided by the not-for-profit organisation Cambia. This application and its developers have no financial or other affiliation or relationship with The Lens or with Cambia.',
                                     br(),br(),
                                     'If you\'d like to cite this work, please use:',
                                     br(),
                                     'Haddaway, N. R., Grainger, M. J., Gray, C. T. (2021) citationchaser: An R package and Shiny app for forward and backward citations chasing in academic searching. doi:', tags$a(href="https://www.doi.org/10.5281/zenodo.4543513", "10.5281/zenodo.4543513"),
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
                   conditionalPanel(
                       condition='input.find_inputs!=null && input.find_inputs!=""',
                       h3('Your input articles'),
                       br(),
                       textOutput('article_report'),
                       br(),
                       downloadButton('input_ris', 'Download an RIS file of your articles (including abstracts)'),
                       br(),br()),
                   add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                   dataTableOutput('article_ref')
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
                        add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
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
                        add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                        dataTableOutput('citations')
                 )
             )
    ),
    tabPanel("Network",
             fluidRow(
                 column(12,
                        h3('Visualise the citation network'),
                        actionButton("get_network", "Visualise"), tags$img(height = 50, src = "legend.png"),
                        'The network visualisation may take a few moments to generate. Zoom in and out using your mouse wheel or two fingers on a trackpad. Move around the network by clicking and dragging.',
                        br(),
                        add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                        conditionalPanel(
                            condition='input.get_network!=null && input.get_network!=""',
                            forceNetworkOutput("force", height = '1100px'))
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
        rv$article_number <- 1 + str_count(input$article_ids,",")
        rv$articles <- article_ref$display
        rv$articles_ris <- article_ref$ris
        rv$articles_df <- article_ref$df
    })
    # render article report text
    output$article_report <- renderText({
        paste0('You provided ', rv$article_number, ' starting articles.')
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
        rv$refs_df <- references$df
    })
    # render references table
    output$references <- renderDataTable({
        rv$refs_display
    }, rownames = FALSE, options = list(dom = 'tpl'))
    # render references report text
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
        rv$cits_df <- citations$df
    })
    # render citations table
    output$citations <- renderDataTable({
        rv$cits_display
    }, rownames = FALSE, options = list(dom = 'tpl'))
    # render citations report text
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
    
    
    # prepare lens_ids for network visualisation
    observeEvent(input$get_network,{
        input_refs <- unnest(rv$articles_df, data.references)
        input_refs <- data.frame(input_lensID = input_refs$data.lens_id, reference_lensID = input_refs$lens_id, type = 'reference')

        input_cits <- unnest(rv$articles_df, data.scholarly_citations)
        input_cits <- data.frame(input_lensID = input_cits$data.lens_id, reference_lensID = input_cits$data.scholarly_citations, type = 'citation')
        
        rv$network <- rbind(input_refs, input_cits)
     
    })
    #network viz
    output$force <- renderForceNetwork({
        network=rv$network
        inputarticles=network$input_lensID
        print(inputarticles)
        tmp1<-data.frame("IDs"=network$input_lensID, "Group"= network$type)
        tmp2<-data.frame("IDs"=network$reference_lensID, "Group"= network$type)
        tmp=rbind(tmp1,tmp2)
        Nodes=unique(tmp)
        Nodes=Nodes %>% 
            mutate(Group2=ifelse(IDs%in%inputarticles, 0, Group)) %>% 
            mutate(Group2=as.character(Group2)) %>% 
            mutate(Group2=dplyr::recode(Group2, "0"="input", "1"="reference", "2"="citation"))
        
        Nodes2<-Nodes %>% 
            filter(!Group2=="input")
        
        inputs<-Nodes %>% 
            filter(Group2=="input") %>% 
            distinct(.,IDs,Group2, .keep_all=TRUE)
        
        Nodes=rbind(inputs, Nodes2)
        
        # make a links data frame using the indexes (0-based) of nodes in 'nodes'
        links <- data.frame(source = match(network$input_lensID, Nodes$IDs) - 1,
                            target = match(network$reference_lensID,Nodes$IDs) - 1)
        
        links<-links %>% 
            drop_na()
        
        n_net<-forceNetwork(Links = links, Nodes = Nodes, Source = "source",
                            Target = "target", NodeID ="IDs", Group="Group2", 
                            linkColour = "black",
                            opacity = 1, opacityNoHover = 1, zoom=TRUE, colourScale = JS('d3.scaleOrdinal().range(["black", "#a50026","#4575b4"]);'))
        
        n_net$x$nodes$hyperlink<-paste0('https://www.lens.org/lens/search/scholar/list?q=lens_id:', Nodes$IDs, '&p=0&n=10&s=_score&d=%2B&f=false&e=false&l=en&authorField=author&dateFilterField=publishedYear&orderBy=%2B_score&presentation=false&stemmed=true&useAuthorId=false')
        n_net$x$options$clickAction = 'window.open(d.hyperlink)'
        n_net
                   })
}

# Run the application 
shinyApp(ui = ui, server = server)
