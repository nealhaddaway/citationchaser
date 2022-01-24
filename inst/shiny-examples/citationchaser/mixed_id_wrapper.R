#' Wrapper for multiple input ID types
#' 
#' @description A wrapper function allowing the searching of multiple different 
#' identifiers at once.
#' @param input a dataframe with one column containing the record identifiers, 
#' and a second column detailing the type of ID (must be one of the following: 
#' 'doi', 'pmid', 'opmcid', 'coreid', 'magid', or 'lens_id'.)
#' @param search The type of search to be run, whether looking up citations 
#' to populate the input table ('input'), or references for backward 
#' citation chasing ('references'), citing articles for forward citation 
#' chasing ('citations') or both references and citations ('both').
#' @param token The lens.org API access token.
#' @return A list of three outputs: a data-table for presentation in the Shiny 
#' app, an RIS (text) file containing all citations, and a dataframe with the 
#' full JSON output from the lens.org API query.
#' @examples 
#' \dontrun{
#' ids <- c('10.1007/978-3-642-37048-9_13', 
#'          '10.1111/sum.12030', 
#'          '10.5194/bg-13-3619-2016', 
#'          '10.1016/j.agee.2012.09.006',
#'          '32652585',
#'          '32706299')
#' type <- c('doi', 
#'           'doi', 
#'           'doi', 
#'           'doi', 
#'           'pmid', 
#'           'pmid')
#' input <- data.frame(ids, type)
#' test1 <- get_refs_mixed(input, search = 'input', token = token)
#' test2 <- get_refs_mixed(input, search = 'references', token = token)
#' }
get_refs_mixed <- function(input,
                           search,
                           token){
  
  #split input data by ID type
  #doi <- split(input, input$type)$doi
  #pmid <- split(input, input$type)$pmid
  #pmcid <- split(input, input$type)$pmcid
  #core <- split(input, input$type)$core
  #magid <- split(input, input$type)$magid
  #lens <- split(input, input$type)$lens
  
  #for each ID type, run a lookup
  #for input
  if(search == 'input'){
    #create blank outputs
    display <- data.table::data.table(NULL)
    ris <- ''
    df <- data.frame()
    
    #dois
    if(('doi' %in% input$type) == TRUE){
      data_doi <- split(input, input$type)$doi
      id_list_doi <- as.vector(data_doi[,1])
      intermediate_doi <- get_citation(id_list_doi, type = 'doi', token = token)
      display_doi <- tryCatch(intermediate_doi$display, error = function(cond){return(NULL)})
      ris_doi <- tryCatch(intermediate_doi$ris, error = function(cond){return(NULL)})
      df_doi <- tryCatch(intermediate_doi$df, error = function(cond){return(NULL)})
      print('DOIs searched')
    } else {
      display_doi <- NULL
      ris_doi <- NULL
      df_doi <- NULL
    }
    
    #pmid
    if(('pmid' %in% input$type) == TRUE){
      data_pmid <- split(input, input$type)$pmid
      id_list_pmid <- as.vector(data_pmid[,1])
      intermediate_pmid <- get_citation(id_list_pmid, type = 'pmid', token = token)
      display_pmid <- tryCatch(intermediate_pmid$display, error = function(cond){return(NULL)})
      ris_pmid <- tryCatch(intermediate_pmid$ris, error = function(cond){return(NULL)})
      df_pmid <- tryCatch(intermediate_pmid$df, error = function(cond){return(NULL)})
      print('PMIDs searched')
    } else {
      display_pmid <- NULL
      ris_pmid <- NULL
      df_pmid <- NULL
    }
    
    #pmcid
    if(('pmcid' %in% input$type) == TRUE){
      data_pmcid <- split(input, input$type)$pmcid
      id_list_pmcid <- as.vector(data_pmcid[,1])
      intermediate_pmcid <- get_citation(id_list_pmcid, type = 'pmcid', token = token)
      display_pmcid <- tryCatch(intermediate_pmcid$display, error = function(cond){return(NULL)})
      ris_pmcid <- tryCatch(intermediate_pmcid$ris, error = function(cond){return(NULL)})
      df_pmcid <- tryCatch(intermediate_pmcid$df, error = function(cond){return(NULL)})
      print('PMIDs searched')
    } else {
      display_pmcid <- NULL
      ris_pmcid <- NULL
      df_pmcid <- NULL
    }
    
    #magid
    if(('magid' %in% input$type) == TRUE){
      data_magid <- split(input, input$type)$magid
      id_list_magid <- as.vector(data_magid[,1])
      intermediate_magid <- get_citation(id_list_magid, type = 'magid', token = token)
      display_magid <- tryCatch(intermediate_magid$display, error = function(cond){return(NULL)})
      ris_magid <- tryCatch(intermediate_magid$ris, error = function(cond){return(NULL)})
      df_magid <- tryCatch(intermediate_magid$df, error = function(cond){return(NULL)})
      print('MAGIDs searched')
    } else {
      display_magid <- NULL
      ris_magid <- NULL
      df_magid <- NULL
    }
    
    #coreid
    if(('coreid' %in% input$type) == TRUE){
      data_coreid <- split(input, input$type)$coreid
      id_list_coreid <- as.vector(data_coreid[,1])
      intermediate_coreid <- get_citation(id_list_coreid, type = 'coreid', token = token)
      display_coreid <- tryCatch(intermediate_coreid$display, error = function(cond){return(NULL)})
      ris_coreid <- tryCatch(intermediate_coreid$ris, error = function(cond){return(NULL)})
      df_coreid <- tryCatch(intermediate_coreid$df, error = function(cond){return(NULL)})
      print('COREIDs searched')
    } else {
      display_coreid <- NULL
      ris_coreid <- NULL
      df_coreid <- NULL
    }
    
    #lensid
    if(('lens_id' %in% input$type) == TRUE){
      data_lens_id <- split(input, input$type)$lens_id
      id_list_lens_id <- as.vector(data_lens_id[,1])
      intermediate_lens_id <- get_citation(id_list_lens_id, type = 'lens_id', token = token)
      display_lens_id <- tryCatch(intermediate_lens_id$display, error = function(cond){return(NULL)})
      ris_lens_id <- tryCatch(intermediate_lens_id$ris, error = function(cond){return(NULL)})
      df_lens_id <- tryCatch(intermediate_lens_id$df, error = function(cond){return(NULL)})
      print('LENSIDs searched')
    } else {
      display_lens_id <- NULL
      ris_lens_id <- NULL
      df_lens_id <- NULL
    }
    
    #combine list outputs
    display <- dplyr::bind_rows(display_doi, 
                                display_pmid, 
                                display_pmcid, 
                                display_magid, 
                                display_coreid, 
                                display_lens_id)
    ris <- paste(ris_doi, 
                 ris_pmid, 
                 ris_pmcid, 
                 ris_magid, 
                 ris_coreid, 
                 ris_lens_id, 
                 sep = '\n\n')
    df <- dplyr::bind_rows(df_doi, 
                           df_pmid, 
                           df_pmcid, 
                           df_magid, 
                           df_coreid, 
                           df_lens_id)
    
    return(list(display = display, ris = ris, df = df))
  } else {
    
    #create blank outputs
    display <- data.table::data.table(NULL)
    ris <- ''
    df <- data.frame()
    
    #dois
    if(('doi' %in% input$type) == TRUE){
      data_doi <- split(input, input$type)$doi
      id_list_doi <- as.vector(data_doi[,1])
      intermediate_doi <- get_refs(id_list_doi, type = 'doi', get_records = search, token = token)
      display_doi <- tryCatch(intermediate_doi$display, error = function(cond){return(NULL)})
      ris_doi <- tryCatch(intermediate_doi$ris, error = function(cond){return(NULL)})
      df_doi <- tryCatch(intermediate_doi$df, error = function(cond){return(NULL)})
    } else {
      display_doi <- NULL
      ris_doi <- NULL
      df_doi <- NULL
    }
    
    #pmid
    if(('pmid' %in% input$type) == TRUE){
      data_pmid <- split(input, input$type)$pmid
      id_list_pmid <- as.vector(data_pmid[,1])
      intermediate_pmid <- get_refs(id_list_pmid, type = 'pmid', get_records = search, token = token)
      display_pmid <- tryCatch(intermediate_pmid$display, error = function(cond){return(NULL)})
      ris_pmid <- tryCatch(intermediate_pmid$ris, error = function(cond){return(NULL)})
      df_pmid <- tryCatch(intermediate_pmid$df, error = function(cond){return(NULL)})
    } else {
      display_pmid <- NULL
      ris_pmid <- NULL
      df_pmid <- NULL
    }
    
    #pmcid
    if(('pmcid' %in% input$type) == TRUE){
      data_pmcid <- split(input, input$type)$pmcid
      id_list_pmcid <- as.vector(data_pmcid[,1])
      intermediate_pmcid <- get_refs(id_list_pmcid, type = 'pmcid', get_records = search, token = token)
      display_pmcid <- tryCatch(intermediate_pmcid$display, error = function(cond){return(NULL)})
      ris_pmcid <- tryCatch(intermediate_pmcid$ris, error = function(cond){return(NULL)})
      df_pmcid <- tryCatch(intermediate_pmcid$df, error = function(cond){return(NULL)})
    } else {
      display_pmcid <- NULL
      ris_pmcid <- NULL
      df_pmcid <- NULL
    }
    
    #magid
    if(('magid' %in% input$type) == TRUE){
      data_magid <- split(input, input$type)$magid
      id_list_magid <- as.vector(data_magid[,1])
      intermediate_magid <- get_refs(id_list_magid, type = 'magid', get_records = search, token = token)
      display_magid <- tryCatch(intermediate_magid$display, error = function(cond){return(NULL)})
      ris_magid <- tryCatch(intermediate_magid$ris, error = function(cond){return(NULL)})
      df_magid <- tryCatch(intermediate_magid$df, error = function(cond){return(NULL)})
    } else {
      display_magid <- NULL
      ris_magid <- NULL
      df_magid <- NULL
    }
    
    #coreid
    if(('coreid' %in% input$type) == TRUE){
      data_coreid <- split(input, input$type)$coreid
      id_list_coreid <- as.vector(data_coreid[,1])
      intermediate_coreid <- get_refs(id_list_coreid, type = 'coreid', get_records = search, token = token)
      display_coreid <- tryCatch(intermediate_coreid$display, error = function(cond){return(NULL)})
      ris_coreid <- tryCatch(intermediate_coreid$ris, error = function(cond){return(NULL)})
      df_coreid <- tryCatch(intermediate_coreid$df, error = function(cond){return(NULL)})
    } else {
      display_coreid <- NULL
      ris_coreid <- NULL
      df_coreid <- NULL
    }
    
    #lensid
    if(('lens_id' %in% input$type) == TRUE){
      data_lens_id <- split(input, input$type)$lens_id
      id_list_lens_id <- as.vector(data_lens_id[,1])
      intermediate_lens_id <- get_refs(id_list_lens_id, type = 'lens_id', get_records = search, token = token)
      display_lens_id <- tryCatch(intermediate_lens_id$display, error = function(cond){return(NULL)})
      ris_lens_id <- tryCatch(intermediate_lens_id$ris, error = function(cond){return(NULL)})
      df_lens_id <- tryCatch(intermediate_lens_id$df, error = function(cond){return(NULL)})
    } else {
      display_lens_id <- NULL
      ris_lens_id <- NULL
      df_lens_id <- NULL
    }
    
    #combine list outputs
    display <- dplyr::bind_rows(display_doi, 
                                display_pmid, 
                                display_pmcid, 
                                display_magid, 
                                display_coreid, 
                                display_lens_id)
    ris <- paste(ris_doi, 
                 ris_pmid, 
                 ris_pmcid, 
                 ris_magid, 
                 ris_coreid, 
                 ris_lens_id, 
                 sep = '\n\n')
    df <- dplyr::bind_rows(df_doi, 
                           df_pmid, 
                           df_pmcid, 
                           df_magid, 
                           df_coreid, 
                           df_lens_id)
    
    return(list(display = display, ris = ris, df = df))
    
  }
  
}

