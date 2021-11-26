suppressPackageStartupMessages({
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
    library(sendmailR)
})

options(shiny.sanitize.errors = TRUE)

source('functions new.R')

# error supression CSS
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

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
                                     'This package contains functions to automate this process by making use of the Lens.org API. An input article list can be used to return a list of all referenced records, and/or all citing records in the Lens.org database (consisting of PubMed, PubMed Central, CrossRef, Microsoft Academic Graph and CORE); ',
                                     tags$a(href="https://www.lens.org", "read more here."), 
                                     br(),br(),
                                     'Large searches may take several minutes to complete, so please be patient.',
                                     br(),br(),
                                     'Consider asking your library to support Lens.org to continue to enable Open Discovery of research articles.'),
                              column(2,
                                     br(),tags$img(height = 200, src = "https://github.com/nealhaddaway/citationchaser/blob/master/inst/extdata/citationchaser.png?raw=true")),
                              column(12,
                                     br(),
                                     h4('Follow these steps to start chasing!'),
                                     icon("arrow-right"),' In the "Article input" tab, paste a list of article identifiers (e.g. DOIs)',
                                     br(),br(),
                                     icon("arrow-right"),' Check the articles returned are the ones your interested in',
                                     br(),br(),
                                     icon("arrow-right"),' If you want to perform backward citation chasing (which articles did my articles reference?) then proceed to the "References" tab and click "Search for all referenced articles in Lens.org"',
                                     br(),br(),
                                     icon("arrow-right"),' If you want to perform forward citation chasing (which articles have cited my articles?) then proceed to the "Citations" tab and click "Search for all citing articles in Lens.org"',
                                     br(),br(),
                                     icon("arrow-right"),' You can download in RIS format a list of your input articles, referenced articles, and citing articles for easy integration with your reference/review management workflow',
                                     br(),br(),
                                     icon("arrow-right"),' Once you have finished citation chasing, why not check out the citation network visualisation in the "Network" tab?',
                                     hr(),
                                     h4('Developers'),
                                     'Note that you can now refer users to citationchaser with a preloaded set of article identifiers. Simply concatenate the following URL stem with a comma-separated list of identifiers:',br(),
                                     'https://estech.shinyapps.io/citationchaser/?dois=[doi1],[doi2],[doi3]',br(),br(),
                                     'This works for DOIs (\'?dois=\'), PubMed IDs (\'?pmids=\'), PMC IDs (\'?pmcids=\'), CORE IDs (\'?coreids=\'), and Microsoft Academic IDs (\'?magids=\'), as per the following worked example:',br(),
                                     tags$a(href="https://estech.shinyapps.io/citationchaser/?dois=10.1038/s41559-020-01295-x,10.1371/journal.pone.0138237", "https://estech.shinyapps.io/citationchaser/?dois=10.1038/s41559-020-01295-x,10.1371/journal.pone.0138237"),br(),
                                     hr(),
                                     tags$img(height = 30, src = "https://static.lens.org/lens/7.2.3/img/home-page/Lens-logo-tagline.png"),
                                     br(),
                                     'The Lens is a service provided by the not-for-profit organisation Cambia. This application and its developers have no financial relationship with The Lens or with Cambia.',
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
                                     'You must complete this step before retrieving references and citations.',
                                     br(),
                                     br(),
                                     splitLayout(textAreaInput("article_ids","Article identifiers (comma separated)", placeholder = "separate identifiers with a comma", width = '90%'),
                                                 selectInput("type", "Identifier type:",
                                                             c("DOI" = "doi",
                                                               "PubMed ID" = "pmid",
                                                               "PubMed Central ID" = "pmcid",
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
                                     actionButton("find_inputs", "Load my input articles", class = "btn-info"),
                                     actionButton("reset", "Reset", class = "btn-warning")
                              ),
                              
                              # Show table of results
                              column(12,
                                     conditionalPanel(
                                         condition='input.find_inputs!=null && input.find_inputs!=""',
                                         h3('Your input articles'),
                                         br(),
                                         textOutput('article_report'),
                                         br(),
                                         uiOutput('download_art'),
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
                                     br(),
                                     'Once you have loaded your input articles, you can search for all referenced articles across them.',
                                     br(),
                                     br(),
                                     br(),
                                     conditionalPanel(
                                         condition='input.find_inputs!=null && input.find_inputs!=""',
                                         actionButton("find_refs", "Search for all referenced articles in Lens.org", class = "btn-info"),
                                         actionButton("reset2", "Reset", class = "btn-warning"),
                                         br(),br(),
                                         textOutput('refs_report'),
                                         br(),
                                         conditionalPanel(
                                             condition='input.find_refs!=null && input.find_refs!=""',
                                             downloadButton('refs_ris', 'Download an RIS file of referenced articles (including abstracts)')),
                                         br(),
                                         add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                                         dataTableOutput('references'))
                              )
                          )
                 ),
                 tabPanel("Citations",
                          fluidRow(
                              column(12,
                                     h3('Citations of your articles (forward citation chasing)'),
                                     br(),
                                     'Once you have loaded your input articles, you can search for all articles that cite them.',
                                     br(),
                                     br(),
                                     br(),
                                     conditionalPanel(
                                         condition='input.find_inputs!=null && input.find_inputs!=""',
                                         actionButton("find_cits", "Search for all citing articles in Lens.org", class = "btn-info"),
                                         actionButton("reset3", "Reset", class = "btn-warning"),
                                         br(),br(),
                                         textOutput('cits_report'),
                                         br(),
                                         conditionalPanel(
                                             condition='input.find_cits!=null && input.find_cits!=""',
                                             downloadButton('cits_ris', 'Download an RIS file of citing articles (including abstracts)')),
                                         br(),
                                         add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                                         dataTableOutput('citations'))
                              )
                          )
                 ),
                 tabPanel("Network",
                          fluidRow(
                              column(12,
                                     h3('Visualise the citation network'),
                                     br(),
                                     'Once you have loaded your input articles, you can visualise your network.',
                                     br(),
                                     br(),
                                     br(),
                                     conditionalPanel(
                                         condition='input.find_inputs!=null && input.find_inputs!=""',
                                         actionButton("get_network", "Visualise"), tags$img(height = 80, src = "legendnew.png"),
                                         br(),
                                         br(),
                                         'The network visualisation may take a few moments to generate. Zoom in and out using your mouse wheel or two fingers on a trackpad. Move around the network by clicking and dragging. Click on a node to see the record on Lens.org.',
                                         br(),
                                         add_busy_spinner(spin = "fading-circle", color = "#19d0fc", margins = c(70, 20)),
                                         conditionalPanel(
                                             condition='input.get_network!=null && input.get_network!=""',
                                             forceNetworkOutput("force", height = '1100px')))
                              )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    rv <- reactiveValues()
    
    # detect redirect PMIDs in Shiny URL
    observe({
        x <- parseQueryString(session$clientData$url_search)
        if (is.null(x$pmids) == FALSE){
            updateTextInput(session, "article_ids", value = x$pmids)
            updateTextInput(session, "type", value = 'pmid')
        } else if (is.null(x$dois) == FALSE){
            updateTextInput(session, "article_ids", value = x$dois)
            updateTextInput(session, "type", value = 'doi')
        } else if (is.null(x$pmcid) == FALSE){
            updateTextInput(session, "article_ids", value = x$pmcids)
            updateTextInput(session, "type", value = 'pmcid')
        } else if (is.null(x$magid) == FALSE){
            updateTextInput(session, "article_ids", value = x$magids)
            updateTextInput(session, "type", value = 'magid')
        } else if (is.null(x$coreid) == FALSE){
            updateTextInput(session, "article_ids", value = x$coreids)
            updateTextInput(session, "type", value = 'coreid')
        } else {
            updateTextInput(session, "type", value = 'doi')
        }
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
        rv$article_number <- 1 + str_count(input$article_ids,",")
        if (rv$article_number >100){
            article_ref <- ''
            rv$articles <- ''
            rv$articles_ris <- ''
            rv$articles_df <- ''
        } else {
            article_ref <- get_citation(input$article_ids, 
                                        type = input$type, 
                                        token = input$token)
            
            if (substr(article_ref[1], start = 1, stop = 7) == 'Warning'){
                rv$articles <- data.frame(Error = article_ref)
                rv$articles_ris <- NULL
                rv$articles_df <- NULL
            } else {
                rv$articles <- article_ref$display
                rv$articles_ris <- article_ref$ris
                rv$articles_df <- article_ref$df
            }
        }
    })
    # render article report text
    output$article_report <- renderText({
        if (rv$article_number > 100){
            'The maximum number of starting articles is 100. Please revise your search and try again.'
        } else {
            paste0('You provided ', rv$article_number, ' starting articles.')
        }
    })
    # render number of articles
    output$download_art <- renderUI({
        if (rv$article_number < 101){
            downloadButton('input_ris', 'Download an RIS file of your articles (including abstracts)')
        } else {
        }
    })
    # render articles table
    output$article_ref <- renderDataTable({
        if ((1 + str_count(input$article_ids,",")) > 100){
        } else {
            rv$articles
        }
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
        
        if (substr(references[1], start = 1, stop = 7) == 'Warning'){
            rv$refs_display <- data.frame(Error = references)
            rv$refs_report <- NULL
            rv$refs_ris <- NULL
            rv$refs_df <- NULL
        } else {
            rv$refs_display <- references$display
            rv$refs_report <- references$report
            rv$refs_ris <- references$ris
            rv$refs_df <- references$df
        }
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
        if (substr(citations[1], start = 1, stop = 7) == 'Warning'){
            rv$cits_display <- data.frame(Error = citations)
            rv$cits_report <- NULL
            rv$cits_ris <- NULL
            rv$cits_df <- NULL
        } else {
            rv$cits_display <- citations$display
            rv$cits_report <- citations$report
            rv$cits_ris <- citations$ris
            rv$cits_df <- citations$df
        }
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
        #if (substr(article_ref[1], start = 1, stop = 7) == 'Warning'){
        #    rv$network <- 'Warning: Your article search did not finish successfully. Please check for errors in previous steps.'
        #} else {
        input_refs <- unnest(rv$articles_df, data.references)
        input_refs <- data.frame(input_lensID = input_refs$data.lens_id, reference_lensID = input_refs$lens_id, type = 'reference')
        
        input_cits <- unnest(rv$articles_df, data.scholarly_citations)
        input_cits <- data.frame(input_lensID = input_cits$data.lens_id, reference_lensID = input_cits$data.scholarly_citations, type = 'citation')
        
        rv$network <- rbind(input_refs, input_cits)
        #}
    })
    #network viz
    output$force <- renderForceNetwork({
        if (substr(rv$network[1], start = 1, stop = 7) == 'Warning'){
            n_net <- rv$network
        } else {
            network=rv$network
            inputarticles=network$input_lensID
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
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
