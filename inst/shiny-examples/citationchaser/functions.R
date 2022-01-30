#' Automated citation chasing in systematic reviews
#' 
#' @description This function takes a list of articles in the form of established 
#'   identifiers (e.g. digital object identifiers) and sends a request to the 
#'   lens.org API to firstly identify all cited references in all articles (in the
#'   form of lists of lens IDs), and then query these lens IDs to bring back full 
#'   citation information for all listed records. Deduplicates references to the 
#'   same records across articles, resulting in an RIS file and a summary report 
#'   in the console.
#' @param article_list List of article identifiers for which the reference 
#'   lists will be returned. Must be a list/vector of identifiers, e.g. 
#'   '"10.1186/s13750-018-0126-2" "10.1002/jrsm.1378"'.
#' @param type Specification of the type of input provided. The default is 
#'   'doi' (digital object identifier), but any of the following are accepted: 
#'   "pmid" (PubMed ID), "pmcid" (PubMed Central ID), "magid" (Microsoft 
#'   Academic ID), "coreid" (CORE identifier), lens_id" (The Lens.org ID), 
#'   "title" (article title; much lower specificity).
#' @param get_records Specification of whether to look for records referenced 
#'   within the input articles ('references'), records citing the input articles 
#'   ('citations'), or both ('both'). 
#' @param save_object Option to save the resultant ris file as an object in 
#'   the Global Environment. The default is FALSE.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#'   applying for scholarly API access and creating a token once approved. See 
#'   'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @return An RIS file is saved to the working directory. A report is printed 
#'   to the console. If 'save_object=TRUE', the RIS file is returned as an 
#'   object
#' @importFrom maditr vlookup
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#' @importFrom tibble tibble
#' @importFrom dplyr mutate group_split bind_rows
#' @importFrom MESS cumsumbinning
#' @export
#' @examples
#' \dontrun{
#' article_list <- c("10.1007/978-3-642-37048-9_13", 
#'                   "10.1111/sum.12030", 
#'                   "10.5194/bg-13-3619-2016", 
#'                   "10.1016/j.agee.2012.09.006")
#'   token <- 'token'
#'   refs <- get_refs(article_list, get_records = 'references', token = token)
#'   refs
#'   }
get_refs <- function(article_list,
                     type = 'doi',
                     get_records,
                     save_object = FALSE,
                     token) {
  
  # set the maximum number of results returned for each query
  max_results <- 500
  
  # if the number of input articles is 1, then split the input list where there are commas (and strip out whitespace)
  if(length(article_list) == 1){
    article_list <- trimws(unlist(strsplit(article_list, '[,]')))
  }
  print('Input record list:')
  print(article_list)
  
  ## input article search
  # build query for input article search
  request1 <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"',type,'": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":500\n}')
  print('Input article request:')
  print(request1)
  
  # perform article search and extract text results
  data <- getLENSData(token, request1)
  print('Input request executed.')
  
  # report requests remaining within the limit (currently 50/min)
  requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
  print(paste0('Remaining requests = ', requests_remaining))
  
  # extract the JSON content of the response
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  print('Results converted to df from JSON output:')
  print(record_list$results)
  
  # error messages
  if (data$status_code == 404){
    print('Error 404: no matched records')
    return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
  }
  if (data$status_code == 429){
    print('Error 429: system overloaded')
    return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
  }
  if (record_list$total == 0){
    print('Error 1: no matched records')
    return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
  }
  
  # report number of input articles returned
  input_number <- record_list[["total"]]
  
  # list input article lens IDs (for later use)
  articles_id <- record_list[["data"]][["lens_id"]]
  print('Returned records from input request:')
  print(articles_id)
  
  ### search for citations of input articles
  if (get_records == 'citations') {
    print('Seeking citations...')
    
    # citations per article
    citation_count <- record_list[["data"]][["scholarly_citations_count"]]
    citation_count[is.na(citation_count)] <- 0
    print('Citations per record sought:')
    print(citation_count)
    # sum of all citations
    all_citations <- sum(citation_count, na.rm = TRUE) 
    print('Total citations sought:')
    print(all_citations)
    
    # return error message if input article(s) have no citations
    #if (record_list[["data"]][["scholarly_citations_count"]] == 0 || is.null(record_list[["data"]][["scholarly_citations_count"]]) == TRUE){
    if (is.null(all_citations) == TRUE || identical(all_citations, 0) == TRUE){
      print('Error 2: no matched citations')
      return('Warning: Your input articles have no recorded citations in the Lens.org database')
    }
    
    ## group articles into chunks based on max number of results per search
    # list citing articles per input article
    cit_by_art <- record_list[["data"]][["scholarly_citations"]]
    citations <- unlist(record_list[["data"]][["scholarly_citations"]])
    print('Citations record list: ')
    print(citations)
    # remove duplicates across articles
    citations_unique <- unique(citations)
    print('Deduplicated citations record list:')
    print(citations_unique)
    # set up dataframe for groups of articles to search
    cit_counts_df <- tibble::tibble(articles_id,
                                    citation_count,
                                    cit_by_art)
    # remove records with no citations
    cit_counts_df[is.na(cit_counts_df)] <- 0
    cit_counts_df <- cit_counts_df[cit_counts_df$citation_count!=0,]
    # subset of records that indidivually have more citations than the max number of results per query
    single <- subset(cit_counts_df, citation_count >= max_results)
    single$export_group <- 0
    # subset of records that have fewer citations than the max number of results per query
    rest <- subset(cit_counts_df, citation_count < max_results)
    
    #the following code mistakenly bins around 500 (>500 records in some groups)
    #rest <- mutate(rest,
    #               # divide by max citations allowed
    #               export_group = ceiling(cumsum(citation_count) / max_results))
    #fix:
    rest <- dplyr::mutate(rest, export_group = MESS::cumsumbinning(citation_count, 500))
    
    # bind both dataframes back together
    cit_counts_df <- rbind(single, rest)
    print(cit_counts_df)
    
    # get a list of vectors of the ids for searches within the export limits
    citgroups <- dplyr::group_split(cit_counts_df, export_group) 
    citgroups_single <- dplyr::group_split(single, export_group) 
    citgroups_rest <- dplyr::group_split(rest, export_group) 
    print(citgroups_rest)
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 500 hits from lens.org
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":500,
	"scroll": "1m",
	"include": ["lens_id", "authors", "publication_type", "title", "external_ids", "start_page", "end_page", "volume", "issue", "references", "scholarly_citations", "source_urls", "abstract", "date_published", "year_published", "references_count", "scholarly_citations_count", "source"]
}')
      # perform search and extract results
      results <- getLENSData(token, request)
      return(results)
    }
    
    cit_results <- data.frame()
    
    # run the query function for each cluster of records, looping through and recording the timing
    if(length(citgroups_rest) > 0){
      
      tStart_cit <- Sys.time()
      for (i in 1:length(citgroups_rest)){
        print(paste0('Running group ', i, ' request...'))
        print(unlist(citgroups_rest[[i]]$cit_by_art))
        data_cit <- run_request(unlist(citgroups_rest[[i]]$cit_by_art))
        requests_remaining <- data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
        # print the requests remaining to the Shinyapps log
        print(paste0('Remaining requests = ', requests_remaining))
        # extract and convert the results to a JSON
        record_json_cit <- httr::content(data_cit, "text")
        record_list_cit <- jsonlite::fromJSON(record_json_cit)
        
        # error messages
        if (data_cit$status_code == 404){
          return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
        }
        if (data_cit$status_code == 429){
          return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
        }
        if (record_list_cit$total == 0){
          return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
        }
        
        # convert the results to a dataframe
        record_list_cit_df <- as.data.frame(record_list_cit)
        cit_results <- dplyr::bind_rows(cit_results, record_list_cit_df)
        tEnd_cit <- Sys.time()
        
        # back off if the requests per minute is close to the limit (currently 50/min)
        if (data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
          t_cit <- tEnd_cit - tStart_cit
          Sys.sleep(60 - t)
        }
      }
    }
    
    ## cursor-based pagination for any single-record query with more than 500 citations
    # only run if there are single-record queries
    if (length(citgroups_single) != 0){
      runrequest <- function(input){
        request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size": ',max_results,',
	"scroll": "1m",
	"include": ["lens_id", "authors", "publication_type", "title", "external_ids", "start_page", "end_page", "volume", "issue", "references", "scholarly_citations", "source_urls", "abstract", "date_published", "year_published", "references_count", "scholarly_citations_count", "source"]
}')
        
        # perform article search and extract text results
        data <- getLENSData(token, request)
        requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
        # print the requests remaining to the Shinyapps log
        print(paste0('Remaining requests = ', requests_remaining))
        # extract and convert the results to a JSON
        record_json <- httr::content(data, "text")
        record_list <- jsonlite::fromJSON(record_json)
        
        # error messages
        if (data$status_code == 404){
          return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
        }
        if (data$status_code == 429){
          return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
        }
        if (record_list$total == 0){
          return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
        }
        
        # convert to a dataframe
        record_df <- data.frame(record_list) 
        
        # if a result contains more than the max number of records per request, use cursor-based pagination
        if(record_list[["total"]] > max_results) {
          
          sets <- ceiling(record_list[["total"]] / max_results) # calculate the number of queries needed for those with more than the max number of results
          
          scroll_id <- record_list[["scroll_id"]] # extract the scroll id from the query to go back to the same search
          
          for (i in 2:sets){ # loop through the sets of results needed to bring back all records into a dataframe
            scroll_id <- record_list[["scroll_id"]] #extract the latest scroll_id from the last query
            
            request <- paste0('{"scroll_id": "', # new query based on scroll_id and including 'include' for efficiency
                              scroll_id,
                              '","include": ["lens_id", "authors", "publication_type", "title", "external_ids", "start_page", "end_page", "volume", "issue", "references", "scholarly_citations", "source_urls", "abstract", "date_published", "year_published", "references_count", "scholarly_citations_count", "source"]}')
            
            # perform article search and extract text results
            data <- getLENSData(token, request)
            requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
            print(paste0('Remaining requests = ', requests_remaining))
            record_json <- httr::content(data, "text")
            record_list <- jsonlite::fromJSON(record_json) # convert json output from article search to list
            
            # error messages
            if (data$status_code == 404){
              return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
            }
            if (data$status_code == 429){
              return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
            }
            if (record_list$total == 0){
              return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
            }
            
            new_df <- data.frame(record_list)
            # output
            record_df <- dplyr::bind_rows(record_df,new_df) # bind the latest search dataframe to the previous dataframe
            
          }
        }
        return(record_df)
      }
      
      # loop through single-record queries that are less than the maximum allowed results per query
      for (i in 1:length(citgroups_single)){
        data_cit <- runrequest(unlist(citgroups_single[[i]]$cit_by_art))
        requests_remaining <- data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
        print(paste0('Remaining requests = ', requests_remaining))
        
        # error messages
        if (data$status_code == 404){
          return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
        }
        if (data$status_code == 429){
          return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
        }
        
        cit_results <- dplyr::bind_rows(cit_results, data_cit)
        #tEnd_cit <- Sys.time()
        #if (data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
        #  t_cit <- tEnd_cit - tStart_cit
        #  Sys.sleep(60 - t) # pause to limit requests below 50 requests/min 
        #}
      }
    }
    
    # remove duplicate records
    cit_results <- cit_results[!duplicated(cit_results$data.lens_id),]
    all_results_cit <- cit_results$data.lens_id
    
    # convert json to ris style
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_cit <- cit_results$data.publication_type
    authors_cit <- list()
    for (i in 1:length(cit_results$data.authors)) {
      authors_cit <- unlist(c(authors_cit, paste0(cit_results$data.authors[[i]]$last_name, ', ', 
                                                  cit_results$data.authors[[i]]$first_name, collapse = '; ')))
    }
    title_cit <- cit_results$data.title
    year_cit <- cit_results$data.year_published
    abstract_cit <- cit_results$data.abstract
    start_page_cit <- cit_results$data.start_page
    end_page_cit <- cit_results$data.end_page
    source_title_cit <- list()
    for (i in 1:length(cit_results[,1])) {
      source_title_cit <- unlist(c(source_title_cit, cit_results$data.source[[1]][i]))
    }
    volume_cit <- cit_results$data.volume
    issue_cit <- cit_results$data.issue
    publisher_cit <- cit_results$data.source.publisher
    issn_cit <- cit_results$data.source.issn
    doi_cit <- unlist(lapply(cit_results$data.external_ids, function(ch) maditr::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    # generate data table for Shiny UI
    level1_table_cit <- data.table::data.table(authors = authors_cit,
                                               year = year_cit,
                                               title = title_cit,
                                               source_title = source_title_cit,
                                               publisher = publisher_cit,
                                               volume = volume_cit,
                                               issue = issue_cit,
                                               start_page = start_page_cit,
                                               end_page = end_page_cit,
                                               doi = doi_cit)
    
    # generate RIS file
    level1_ris_cit <- paste(paste0('\n',
                                   'TY  - ', maditr::vlookup(publication_type_cit, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
                                   'AU  - ', authors_cit, '\n',
                                   'TI  - ', title_cit, '\n',
                                   'PY  - ', year_cit, '\n',
                                   'AB  - ', abstract_cit, '\n',
                                   'SP  - ', start_page_cit, '\n',
                                   'EP  - ', end_page_cit, '\n',
                                   'JF  - ', source_title_cit, '\n',
                                   'VL  - ', volume_cit, '\n',
                                   'IS  - ', issue_cit, '\n',
                                   'PB  - ', publisher_cit, '\n',
                                   # 'SN  - ', issn_cit, '\n',
                                   'DO  - ', doi_cit, '\n',
                                   'ER  - '),
                            collapse = '\n')
    
    # generate ris build report
    ris_records_cit <- lengths(regmatches(level1_ris_cit, gregexpr("TY  - ", level1_ris_cit)))
    
    stage1_report_cit <- paste0('Your ', scales::comma(input_number), ' articles were cited a total of ', scales::comma(all_citations), ' times. This corresponds to ', 
                                scales::comma(length(citations_unique)), ' unique article IDs. Your RIS file is ready for download and contains ', 
                                scales::comma(ris_records_cit), ' records exported from Lens.org.')
    
    report_cit <- stage1_report_cit
    
    return(list(display = level1_table_cit, ris = level1_ris_cit, report = report_cit, df = cit_results))
    
    ### search for references articles
  } else if (get_records == 'references') {
    
    if (is.null(record_list[["data"]][["references_count"]]) == TRUE || identical(record_list[["data"]][["references_count"]], 0) == TRUE){
      return('Warning: Your input articles contained no references in the Lens.org database')
    }
    
    # obtain reference lists from article search
    reference_count <- record_list[["data"]][["references_count"]]
    reference_count[is.na(reference_count)] <- 0
    all_references <- sum(reference_count, na.rm = TRUE)
    ref_by_art <- record_list[["data"]][["references"]]
    references <- unlist(record_list[["data"]][["references"]])
    references_unique <- unique(references)
    deduped_references <- length(references_unique)
    
    ref_counts_df <- tibble::tibble(articles_id,
                                    reference_count,
                                    ref_by_art)
    ref_counts_df[is.na(ref_counts_df)] <- 0
    #below code not working because it was tagging excess records from one group onto the next (of 500)
    #ref_counts_df <- dplyr::mutate(ref_counts_df,
    #                        # cumulatively add up citation count
    #                        cumulative_n = cumsum(reference_count),
    #                        # divide by max citations allowed
    #                        export_group = floor(cumsum(reference_count) / 500))
    ref_counts_df <- dplyr::mutate(ref_counts_df, export_group = MESS::cumsumbinning(reference_count, 500))
    
    # get a list of vectors of the ids
    refgroups <- dplyr::group_split(ref_counts_df, export_group) 
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":500,
	"include": ["lens_id", "authors", "publication_type", "title", "external_ids", "start_page", "end_page", "volume", "issue", "references", "scholarly_citations", "source_urls", "abstract", "date_published", "year_published", "references_count", "scholarly_citations_count", "source"]
}')
      #perform references search and extract text results
      results <- getLENSData(token, request)
      return(results)
    }
    
    # run the query function for each tibble, adding to a final dataset
    ref_results <- data.frame()
    tStart_ref <- Sys.time()
    for (i in 1:length(refgroups)){
      data_ref <- run_request(unique(unlist(refgroups[[i]]$ref_by_art)))
      requests_remaining <- data_ref[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
      print(paste0('Remaining requests = ', requests_remaining))
      record_json_ref <- httr::content(data_ref, "text")
      record_list_ref <- jsonlite::fromJSON(record_json_ref)
      
      # error messages
      if (data_ref$status_code == 404){
        return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
      }
      if (data_ref$status_code == 429){
        return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
      }
      if (record_list_ref$total == 0){
        return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
      }
      
      record_list_ref_df <- as.data.frame(record_list_ref)
      ref_results <- dplyr::bind_rows(ref_results, record_list_ref_df)
      tEnd_ref <- Sys.time()
      if (data_ref[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
        t_ref <- tEnd_ref - tStart_ref
        Sys.sleep(60 - t) # pause to limit requests below 50 requests/min 
      }
    }
    ref_results <- ref_results[!duplicated(ref_results$data.lens_id),]
    all_results_ref <- ref_results$data.lens_id
    
    # convert json to ris style
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_ref <- ref_results$data.publication_type
    authors_ref <- list()
    for (i in 1:length(ref_results$data.authors)) {
      authors_ref <- unlist(c(authors_ref, paste0(ref_results$data.authors[[i]]$last_name, ', ', 
                                                  ref_results$data.authors[[i]]$first_name, collapse = '; ')))
    }
    title_ref <- ref_results$data.title
    year_ref <- ref_results$data.year_published
    abstract_ref <- ref_results$data.abstract
    start_page_ref <- ref_results$data.start_page
    end_page_ref <- ref_results$data.end_page
    source_title_ref <- list()
    for (i in 1:length(ref_results[,1])) {
      source_title_ref <- unlist(c(source_title_ref, ref_results$data.source[[1]][i]))
    }
    volume_ref <- ref_results$data.volume
    issue_ref <- ref_results$data.issue
    publisher_ref <- ref_results$data.source.publisher
    issn_ref <- ref_results$data.source.issn
    doi_ref <- unlist(lapply(ref_results$data.external_ids, function(ch) maditr::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_table_ref <- data.table::data.table(authors = authors_ref,
                                               year = year_ref,
                                               title = title_ref,
                                               source_title = source_title_ref,
                                               publisher = publisher_ref,
                                               volume = volume_ref,
                                               issue = issue_ref,
                                               start_page = start_page_ref,
                                               end_page = end_page_ref,
                                               doi = doi_ref)
    
    level1_ris_ref <- paste(paste0('\n',
                                   'TY  - ', maditr::vlookup(publication_type_ref, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
                                   'AU  - ', authors_ref, '\n',
                                   'TI  - ', title_ref, '\n',
                                   'PY  - ', year_ref, '\n',
                                   'AB  - ', abstract_ref, '\n',
                                   'SP  - ', start_page_ref, '\n',
                                   'EP  - ', end_page_ref, '\n',
                                   'JF  - ', source_title_ref, '\n',
                                   'VL  - ', volume_ref, '\n',
                                   'IS  - ', issue_ref, '\n',
                                   'PB  - ', publisher_ref, '\n',
                                   # 'SN  - ', issn_ref, '\n',
                                   'DO  - ', doi_ref, '\n',
                                   'ER  - '),
                            collapse = '\n')
    
    # ris build report
    ris_records_ref <- lengths(regmatches(level1_ris_ref, gregexpr("TY  - ", level1_ris_ref)))
    
    stage1_report_ref <- paste0('Your ', scales::comma(input_number), ' articles contained a total of ', scales::comma(all_references), ' references. This corresponds to ', 
                                scales::comma(deduped_references), ' unique IDs. Your RIS file is ready for download and contains ', scales::comma(ris_records_ref), ' records exported from Lens.org.')
    
    report_ref <- stage1_report_ref
    
    return(list(display = level1_table_ref, ris = level1_ris_ref, report = report_ref, df = ref_results))
    
  } 
}


#' Function to query Lens.org
#' 
#' @description Function written by lens.org for use of their API.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#'   applying for scholarly API access and creating a token once approved. See 
#'   'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @param query A search string formulated according to the Lens.org API 
#'   documentation: 'https://docs.api.lens.org/request-scholar.html'.
#' @return A summary response. The results are viewable using 
#'   'content(data, "text")'. Other details regarding the request (e.g. repsonse 
#'   times) can be accessed through the main output.
#' @importFrom httr add_headers POST
getLENSData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, httr::add_headers(.headers=headers), body = query)
}


#' Find citation based on identifier
#' 
#' @description 
#' @param article_list List of article identifiers for which the reference 
#'   lists will be returned. Must be a list/vector of identifiers, e.g. 
#'   '"10.1186/s13750-018-0126-2" "10.1002/jrsm.1378"'.
#' @param type Specification of the type of input provided. The default is 
#'   'doi' (digital object identifier), but any of the following are accepted: 
#'   "pmid" (PubMed ID), "pmcid" (PubMed Central ID), "magid" (Microsoft 
#'   Academic ID), "coreid" (CORE identifier), lens_id" (The Lens.org ID), 
#'   "title" (article title; much lower specificity).
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#'   applying for scholarly API access and creating a token once approved. See 
#'   'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @return A dataframe containing the matching citation from Lens.org.
#' @importFrom expss vlookup
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @examples 
#' \dontrun{
#' article_list <- c("10.1007/978-3-642-37048-9_13", "10.1111/sum.12030", "10.5194/bg-13-3619-2016", "10.1016/j.agee.2012.09.006")
#' results <- get_citation(article_list)
#' articles <- results$display
#' }
#' @export
get_citation <- function(article_list, 
                         type = 'doi',
                         token = 'WCFlpCtuJXYI1sDhZcZ8y7hHpri0SEmTnLNkeU4OEM5JTQRNXB9w'){
  
  if(length(article_list) == 1){
    article_list <- trimws(unlist(strsplit(article_list, '[,]')))
  }
  
  request <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"', type, '": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":500\n}')
  
  # perform article search and extract text results
  data <- getLENSData(token, request)
  requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
  print(paste0('Remaining requests = ', requests_remaining))
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  
  # error messages
  if (data$status_code == 404){
    return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
  }
  if (data$status_code == 429){
    return('Warning: Right now there are too many people using citationchaser. This has been logged and we will endeavour to increase the bandwith as soon as possible. Please try again later.')
  }
  if (record_list$total == 0){
    return('Warning: Your search returned no matched results. Please double check your input article identifiers and try again.')
  }
  
  inputs_df <- as.data.frame(record_list)
  
  # citations and references
  citation_count <- record_list[["data"]][["scholarly_citations_count"]]
  reference_count <- record_list[["data"]][["references_count"]]
  
  # convert json to ris style
  type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                          description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                          publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
  publication_type <- record_list[["data"]][["publication_type"]]
  authors <- list()
  for (i in 1:length(record_list[["data"]][["authors"]])) {
    authors <- unlist(c(authors, paste0(record_list[["data"]][["authors"]][[i]]$last_name, ', ', 
                                        record_list[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
  }
  title <- record_list[["data"]][["title"]]
  year <- record_list[["data"]][["year_published"]]
  abstract <- record_list[["data"]][["abstract"]]
  start_page <- record_list[["data"]][["start_page"]]
  end_page <- record_list[["data"]][["end_page"]]
  source_title <- record_list[["data"]][["source"]][["title"]]
  volume <- record_list[["data"]][["volume"]]
  issue <- record_list[["data"]][["issue"]]
  publisher <- record_list[["data"]][["source"]][["publisher"]]
  issn <- record_list[["data"]][["source"]][["issn"]]
  doi <- unlist(lapply(record_list[["data"]][["external_ids"]], function(ch) maditr::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
  
  article_table <- data.table(authors = authors,
                              year = year,
                              title = title,
                              source_title = source_title,
                              publisher = publisher,
                              volume = volume,
                              issue = issue,
                              start_page = start_page,
                              end_page = end_page,
                              doi = doi,
                              Lens_refs = reference_count,
                              Lens_cited = citation_count)
  
  article_ris <- paste(paste0('\n',
                              'TY  - ', maditr::vlookup(publication_type, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
                              'AU  - ', authors, '\n',
                              'TI  - ', title, '\n',
                              'PY  - ', year, '\n',
                              'AB  - ', abstract, '\n',
                              'SP  - ', start_page, '\n',
                              'EP  - ', end_page, '\n',
                              'JF  - ', source_title, '\n',
                              'VL  - ', volume, '\n',
                              'IS  - ', issue, '\n',
                              'PB  - ', publisher, '\n',
                              # 'SN  - ', issn, '\n',
                              'DO  - ', doi, '\n',
                              'ER  - '),
                       collapse = '\n')
  
  return(list(display = article_table, ris = article_ris, df = inputs_df))
  
}
