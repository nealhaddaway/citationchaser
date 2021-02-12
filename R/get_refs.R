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
#' @importFrom expss vlookup
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#' @importFrom tibble tibble
#' @importFrom dplyr mutate group_split
#' @export
#' @examples
#' \dontrun{
#' article_list <- c("10.1007/978-3-642-37048-9_13", 
#'                   "10.1111/sum.12030", 
#'                   "10.5194/bg-13-3619-2016", 
#'                   "10.1016/j.agee.2012.09.006")
#'   token <- 'token'
#'   refs <- get_refs(article_list, get_records = 'both', token = token)
#'   refs
#'   }
get_refs <- function(article_list,
                     type = 'doi',
                     get_records,
                     save_object = FALSE,
                     token) {
  
  # process doi formats of input article list
  if (type == 'doi') { 
    if (any(grep('(http|doi)', article_list)) == TRUE) {
      stop('Please ensure dois (digital object identifiers) are provided alone and not as URLS (i.e. do not include "doi.org" or "http...")')
    }
  } else if (type != ('pmid|pmcid|coreid|magid|lens_id|title')) {
    stop('Please provide articles in one of the following formats: "doi" (digital object identifier, in this format: "10.1186/s13750-018-0126-2"), "pmid" (PubMed ID), "pmcid" (PubMed Central ID), "magid" (Microsoft Academic ID), "coreid" (CORE identifier), lens_id" (The Lens.org ID), "title" (article title; much lower specificity)')
  } 
  
  # build query for article search
  request1 <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"doi": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":1000\n}')
  
  # perform article search and extract text results
  data <- getLENSData(token, request1)
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  input_number <- record_list[["total"]]
  
  # list lens IDs
  articles_id <- record_list[["data"]][["lens_id"]]
  
  if (get_records == 'citations') {
    
    # citations per article
    citation_count <- record_list[["data"]][["scholarly_citations_count"]]
    all_citations <- sum(citation_count, na.rm = TRUE)
    cit_by_art <- record_list[["data"]][["scholarly_citations"]]
    citations <- unlist(record_list[["data"]][["scholarly_citations"]])
    citations_unique <- unique(citations)
    
    cit_counts_df <- tibble(articles_id,
           citation_count,
           cit_by_art)
    cit_counts_df[is.na(cit_counts_df)] <- 0
    cit_counts_df <- mutate(cit_counts_df,
        # cumulatively add up citation count
        cumulative_n = cumsum(citation_count),
        # divide by max citations allowed
        export_group = floor(cumsum(citation_count) / 1000)
      )
    
    
    # get a list of vectors of the ids
    citgroups <- dplyr::group_split(cit_counts_df, export_group) 
    
    stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', all_citations, ' times. The following number of citations for each input article were found on lens.org:\n\n',
                                paste(paste0('doi: ', article_list, ', citations: ', record_list[["data"]][["scholarly_citations_count"]]), collapse = '\n'),
                                '\nIn total, this corresponds to a total of ', length(citations_unique), ' records on lens.org.\n\n')
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      #perform references search and extract text results
      results <- getLENSData(token, request)
      return(results)
    }
    
    # run the query function for each tibble, adding to a final dataset
    cit_results <- data.frame()
    tStart_cit <- Sys.time()
    for (i in 1:length(citgroups)){
      data_cit <- run_request(unlist(citgroups[[i]]$cit_by_art))
      record_json_cit <- httr::content(data_cit, "text")
      record_list_cit <- jsonlite::fromJSON(record_json_cit)
      record_list_cit_df <- as.data.frame(record_list_cit)
      cit_results <- bind_rows(cit_results, record_list_cit_df)
      tEnd_cit <- Sys.time()
      if (data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
        t_cit <- tEnd_cit - tStart_cit
        Sys.sleep(60 - t) # pause to limit requests below 10/min 
      }
    }
    all_results_cit <- cit_results$data.lens_id
    
    download_report_cit <- paste0('Your query returned ', nrow(cit_results), ' records from lens.org.\n\n')
    
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
    source_title_cit <- cit_results$data.source.title
    volume_cit <- cit_results$data.volume
    issue_cit <- cit_results$data.issue
    publisher_cit <- cit_results$data.source.publisher
    issn_cit <- cit_results$data.source.issn
    doi_cit <- unlist(lapply(cit_results$data.external_ids, function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_ris_cit <- paste(paste0('\n',
                                   'TY  - ', expss::vlookup(publication_type_cit, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
    
    # ris build report
    ris_records_cit <- lengths(regmatches(level1_ris_cit, gregexpr("TY  - ", level1_ris_cit)))
    risbuild_report_cit <- paste0('Your output report contains ', ris_records_cit, ' records and has been saved as "citations.ris" in your working directory.\n\n')
    
    citations_ris <- level1_ris_cit
    
    write.table(citations_ris, file = "citations.ris", sep = "")
    cat(paste0('#Citations summary\n', stage1_report_cit, '#Download\n', download_report_cit, '#RIS file build\n', risbuild_report_cit))
    if (save_object == TRUE) {
      return(citations_ris)
    }
    
    
  } else if (get_records == 'references') {
    
    # obtain reference lists from article search
    reference_count <- record_list[["data"]][["references_count"]]
    all_references <- sum(reference_count, na.rm = TRUE)
    ref_by_art <- record_list[["data"]][["references"]]
    references <- unlist(record_list[["data"]][["references"]])
    references_unique <- unique(references)
    deduped_references <- length(references_unique)
    
    ref_counts_df <- tibble(articles_id,
                            reference_count,
                            ref_by_art)
    ref_counts_df[is.na(ref_counts_df)] <- 0
    ref_counts_df <- mutate(ref_counts_df,
                            # cumulatively add up citation count
                            cumulative_n = cumsum(reference_count),
                            # divide by max citations allowed
                            export_group = floor(cumsum(reference_count) / 1000)
    )
    
    
    # get a list of vectors of the ids
    refgroups <- dplyr::group_split(ref_counts_df, export_group) 
    
    stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_references, ' references. The input articles contained the following identifiable references on lens.org: \n',
                                paste(paste0('doi: ', article_list, ', references: ', record_list[["data"]][["references_count"]]), collapse = '\n'),
                                '\n\nThis corresponds to ', deduped_references, ' unique records.\n\n')
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      #perform references search and extract text results
      results <- getLENSData(token, request)
      return(results)
    }
    
      # run the query function for each tibble, adding to a final dataset
      ref_results <- data.frame()
      tStart_ref <- Sys.time()
      for (i in 1:length(refgroups)){
        data_ref <- run_request(unlist(refgroups[[i]]$ref_by_art))
        record_json_ref <- httr::content(data_ref, "text")
        record_list_ref <- jsonlite::fromJSON(record_json_ref)
        record_list_ref_df <- as.data.frame(record_list_ref)
        ref_results <- bind_rows(ref_results, record_list_ref_df)
        tEnd_ref <- Sys.time()
        if (data_ref[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
          t_ref <- tEnd_ref - tStart_ref
          Sys.sleep(60 - t) # pause to limit requests below 10/min 
        }
      }
      all_results_ref <- ref_results$data.lens_id
      
    download_report_ref <- paste0('Your query returned ', nrow(ref_results), ' records from lens.org.\n\n')
    
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
    source_title_ref <- ref_results$data.source.title
    volume_ref <- ref_results$data.volume
    issue_ref <- ref_results$data.issue
    publisher_ref <- ref_results$data.source.publisher
    issn_ref <- ref_results$data.source.issn
    doi_ref <- unlist(lapply(ref_results$data.external_ids, function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_ris_ref <- paste(paste0('\n',
                                   'TY  - ', expss::vlookup(publication_type_ref, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
    risbuild_report_ref <- paste0('Your output report contains ', ris_records_ref, ' records and has been saved as "references.ris" in your working directory.\n\n')
    
    references_ris <- level1_ris_ref
    
    write.table(references_ris, file = "references.ris", sep = "")
    cat(paste0('#References summary\n', stage1_report_ref, '#Download\n', download_report_ref, '#RIS file build\n', risbuild_report_ref))
    if (save_object == TRUE) {
      return(references_ris)
    }
  } else if (get_records == 'both') {
    
    # obtain reference lists from article search
    reference_count <- record_list[["data"]][["references_count"]]
    all_references <- sum(reference_count, na.rm = TRUE)
    ref_by_art <- record_list[["data"]][["references"]]
    references <- unlist(record_list[["data"]][["references"]])
    references_unique <- unique(references)
    deduped_references <- length(references_unique)
    
    ref_counts_df <- tibble(articles_id,
                            reference_count,
                            ref_by_art)
    ref_counts_df[is.na(ref_counts_df)] <- 0
    ref_counts_df <- mutate(ref_counts_df,
                            # cumulatively add up citation count
                            cumulative_n = cumsum(reference_count),
                            # divide by max citations allowed
                            export_group = floor(cumsum(reference_count) / 1000)
    )
    
    
    # get a list of vectors of the ids
    refgroups <- dplyr::group_split(ref_counts_df, export_group) 
    
    stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_references, ' references. The input articles contained the following identifiable references on lens.org: \n',
                                paste(paste0('doi: ', article_list, ', references: ', record_list[["data"]][["references_count"]]), collapse = '\n'),
                                '\n\nThis corresponds to ', deduped_references, ' unique records.\n\n')
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      #perform references search and extract text results
      results <- getLENSData(token, request)
      return(results)
    }
    
    # run the query function for each tibble, adding to a final dataset
    ref_results <- data.frame()
    tStart_ref <- Sys.time()
    for (i in 1:length(refgroups)){
      data_ref <- run_request(unlist(refgroups[[i]]$ref_by_art))
      record_json_ref <- httr::content(data_ref, "text")
      record_list_ref <- jsonlite::fromJSON(record_json_ref)
      record_list_ref_df <- as.data.frame(record_list_ref)
      ref_results <- bind_rows(ref_results, record_list_ref_df)
      tEnd_ref <- Sys.time()
      if (data_ref[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
        t_ref <- tEnd_ref - tStart_ref
        Sys.sleep(60 - t) # pause to limit requests below 10/min 
      }
    }
    all_results_ref <- ref_results$data.lens_id
    
    download_report_ref <- paste0('Your query returned ', nrow(ref_results), ' records from lens.org.\n\n')
    
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
    source_title_ref <- ref_results$data.source.title
    volume_ref <- ref_results$data.volume
    issue_ref <- ref_results$data.issue
    publisher_ref <- ref_results$data.source.publisher
    issn_ref <- ref_results$data.source.issn
    doi_ref <- unlist(lapply(ref_results$data.external_ids, function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_ris_ref <- paste(paste0('\n',
                                   'TY  - ', expss::vlookup(publication_type_ref, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
    risbuild_report_ref <- paste0('Your output report contains ', ris_records_ref, ' records and has been saved as "references.ris" in your working directory.\n\n')
    
    references_ris <- level1_ris_ref
    
    write.table(references_ris, file = "references.ris", sep = "")
    cat(paste0('#References summary\n', stage1_report_ref, '#Download\n', download_report_ref, '#RIS file build\n', risbuild_report_ref))
    
    
    # citations per article
    citation_count <- record_list[["data"]][["scholarly_citations_count"]]
    all_citations <- sum(citation_count, na.rm = TRUE)
    cit_by_art <- record_list[["data"]][["scholarly_citations"]]
    citations <- unlist(record_list[["data"]][["scholarly_citations"]])
    citations_unique <- unique(citations)
    
    cit_counts_df <- tibble(articles_id,
                            citation_count,
                            cit_by_art)
    cit_counts_df[is.na(cit_counts_df)] <- 0
    cit_counts_df <- mutate(cit_counts_df,
                            # cumulatively add up citation count
                            cumulative_n = cumsum(citation_count),
                            # divide by max citations allowed
                            export_group = floor(cumsum(citation_count) / 1000)
    )
    
    
    # get a list of vectors of the ids
    citgroups <- dplyr::group_split(cit_counts_df, export_group) 
    
    stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', all_citations, ' times. The following number of citations for each input article were found on lens.org:\n\n',
                                paste(paste0('doi: ', article_list, ', citations: ', record_list[["data"]][["scholarly_citations_count"]]), collapse = '\n'),
                                '\nIn total, this corresponds to a total of ', length(citations_unique), ' records on lens.org.\n\n')
    
    # define query function
    run_request <- function(input){
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(input, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      #perform references search and extract text results
      results <- getLENSData(token, request)
      return(results)
    }
    
    # run the query function for each tibble, adding to a final dataset
    cit_results <- data.frame()
    tStart_cit <- Sys.time()
    for (i in 1:length(citgroups)){
      data_cit <- run_request(unlist(citgroups[[i]]$cit_by_art))
      record_json_cit <- httr::content(data_cit, "text")
      record_list_cit <- jsonlite::fromJSON(record_json_cit)
      record_list_cit_df <- as.data.frame(record_list_cit)
      cit_results <- bind_rows(cit_results, record_list_cit_df)
      tEnd_cit <- Sys.time()
      if (data_cit[["headers"]][["x-rate-limit-remaining-request-per-minute"]] < 1){
        t_cit <- tEnd_cit - tStart_cit
        Sys.sleep(60 - t) # pause to limit requests below 10/min 
      }
    }
    all_results_cit <- cit_results$data.lens_id
    
    download_report_cit <- paste0('Your query returned ', nrow(cit_results), ' records from lens.org.\n\n')
    
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
    source_title_cit <- cit_results$data.source.title
    volume_cit <- cit_results$data.volume
    issue_cit <- cit_results$data.issue
    publisher_cit <- cit_results$data.source.publisher
    issn_cit <- cit_results$data.source.issn
    doi_cit <- unlist(lapply(cit_results$data.external_ids, function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_ris_cit <- paste(paste0('\n',
                                   'TY  - ', expss::vlookup(publication_type_cit, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
    
    # ris build report
    ris_records_cit <- lengths(regmatches(level1_ris_cit, gregexpr("TY  - ", level1_ris_cit)))
    risbuild_report_cit <- paste0('Your output report contains ', ris_records_cit, ' records and has been saved as "citations.ris" in your working directory.\n\n')
    
    citations_ris <- level1_ris_cit
    
    write.table(citations_ris, file = "citations.ris", sep = "")
    cat(paste0('\n**********\n#Citations summary\n', stage1_report_cit, '#Download\n', download_report_cit, '#RIS file build\n', risbuild_report_cit))
    
    if (save_object == TRUE) {
      return(list(citations = citations_ris, references = references_ris))
    }
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
