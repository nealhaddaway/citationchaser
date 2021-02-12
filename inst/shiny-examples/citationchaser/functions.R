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
#' @export
#' @examples
#' \dontrun{
#' article_list <- c("10.1007/978-3-642-37048-9_13", 
#'                   "10.1111/sum.12030", 
#'                   "10.5194/bg-13-3619-2016", 
#'                   "10.1016/j.agee.2012.09.006")
#'   refs <- get_refs(article_list, token = 'xxx', get_records = 'both')
#'   citations <- refs$display_cit
#'   references <- refs$display_ref
#'   }
get_refs <- function(article_list,
                     type = 'doi',
                     get_records,
                     save_object = FALSE,
                     token) {
  
  if(length(article_list) == 1){
    article_list <- trimws(unlist(strsplit(article_list, '[,]')))
  }
  
  # build query for article search
  request1 <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"doi": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":1000\n}')
  
  # perform article search and extract text results
  data <- getLENSData(token, request1)
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  record_listdb <- as.data.frame(jsonlite::fromJSON(record_json))
  input_number <- record_list[["total"]]
  
  if (get_records == 'citations') {
    
    # citations per article
    citation_count <- record_list[["data"]][["scholarly_citations_count"]]
    all_citations <- sum(citation_count, na.rm = TRUE)
    citations <- unlist(record_list[["data"]][["scholarly_citations"]])
    citations_unique <- unique(citations)
    
    stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', 
                                all_citations, ' times. In total, this corresponds to a total of ', 
                                length(citations_unique), ' records on lens.org.')
    
    # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
    request2_cit <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(citations_unique, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
    
    #perform references search and extract text results
    data2_cit <- getLENSData(token, request2_cit)
    record_json2_cit <- httr::content(data2_cit, "text")
    
    #convert json output from references search to list and build download report
    record_list2_cit <- jsonlite::fromJSON(record_json2_cit) 
    
    # convert json to ris style
    
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_cit <- record_list2_cit[["data"]][["publication_type"]]
    authors_cit <- list()
    for (i in 1:length(record_list2_cit[["data"]][["authors"]])) {
      authors_cit <- unlist(c(authors_cit, paste0(record_list2_cit[["data"]][["authors"]][[i]]$last_name, ', ', 
                                                  record_list2_cit[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
    }
    title_cit <- record_list2_cit[["data"]][["title"]]
    year_cit <- record_list2_cit[["data"]][["year_published"]]
    abstract_cit <- record_list2_cit[["data"]][["abstract"]]
    start_page_cit <- record_list2_cit[["data"]][["start_page"]]
    end_page_cit <- record_list2_cit[["data"]][["end_page"]]
    source_title_cit <- record_list2_cit[["data"]][["source"]][["title"]]
    volume_cit <- record_list2_cit[["data"]][["volume"]]
    issue_cit <- record_list2_cit[["data"]][["issue"]]
    publisher_cit <- record_list2_cit[["data"]][["source"]][["publisher"]]
    issn_cit <- record_list2_cit[["data"]][["source"]][["issn"]]
    doi_cit <- unlist(lapply(record_list2_cit[["data"]][["external_ids"]], function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_table_cit <- data.table(authors = authors_cit,
                                 year = year_cit,
                                 title = title_cit,
                                 source_title = source_title_cit,
                                 publisher = publisher_cit,
                                 volume = volume_cit,
                                 issue = issue_cit,
                                 start_page = start_page_cit,
                                 end_page = end_page_cit,
                                 doi = doi_cit)
    
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
    
    report_cit <- stage1_report_cit
    
    return(list(display = level1_table_cit, ris = level1_ris_cit, report = report_cit))
    
  } else if (get_records == 'references') {
    
    # obtain reference lists from article search
    references <- unlist(record_list[["data"]][["references"]])
    all_refs <- length(references)
    
    # deduplicate shared references and build deduplication report
    references <- unique(references)
    deduped_citations <- length(references)
    duplicates <- all_refs - deduped_citations
    stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_refs, ' references. This corresponds to ', deduped_citations, ' unique records.')
    
    # build query for article references - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
    request2_ref <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(references, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
    
    #perform references search and extract text results
    data2_ref <- getLENSData(token, request2_ref)
    record_json2_ref <- httr::content(data2_ref, "text")
    
    #convert json output from references search to list and build download report
    record_list2_ref <- jsonlite::fromJSON(record_json2_ref) 
    
    # convert json to ris style
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_ref <- record_list2_ref[["data"]][["publication_type"]]
    authors_ref <- list()
    for (i in 1:length(record_list2_ref[["data"]][["authors"]])) {
      authors_ref <- unlist(c(authors_ref, paste0(record_list2_ref[["data"]][["authors"]][[i]]$last_name, ', ', 
                                                  record_list2_ref[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
    }
    title_ref <- record_list2_ref[["data"]][["title"]]
    year_ref <- record_list2_ref[["data"]][["year_published"]]
    abstract_ref <- record_list2_ref[["data"]][["abstract"]]
    start_page_ref <- record_list2_ref[["data"]][["start_page"]]
    end_page_ref <- record_list2_ref[["data"]][["end_page"]]
    source_title_ref <- record_list2_ref[["data"]][["source"]][["title"]]
    volume_ref <- record_list2_ref[["data"]][["volume"]]
    issue_ref <- record_list2_ref[["data"]][["issue"]]
    publisher_ref <- record_list2_ref[["data"]][["source"]][["publisher"]]
    issn_ref <- record_list2_ref[["data"]][["source"]][["issn"]]
    doi_ref <- unlist(lapply(record_list2_ref[["data"]][["external_ids"]], function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_table_ref <- data.table(authors = authors_ref,
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
    
    report_ref <- stage1_report_ref
    
    return(list(display = level1_table_ref, ris = level1_ris_ref, report = report_ref))
    
  } else if (get_records == 'both') {
    
    # obtain reference lists from article search
    references <- unlist(record_list[["data"]][["references"]])
    all_refs <- length(references)
    
    # deduplicate shared references and build deduplication report
    references <- unique(references)
    input_number <- record_list[["total"]]
    deduped_citations <- length(references)
    duplicates <- all_refs - deduped_citations
    stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_refs, ' references. This corresponds to ', deduped_citations, ' unique records.')
    
    # build query for article references - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
    request2_ref <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(references, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
    
    #perform references search and extract text results
    data2_ref <- getLENSData(token, request2_ref)
    record_json2_ref <- httr::content(data2_ref, "text")
    
    #convert json output from references search to list and build download report
    record_list2_ref <- jsonlite::fromJSON(record_json2_ref) 
    
    # convert json to ris style
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_ref <- record_list2_ref[["data"]][["publication_type"]]
    authors_ref <- list()
    for (i in 1:length(record_list2_ref[["data"]][["authors"]])) {
      authors_ref <- unlist(c(authors_ref, paste0(record_list2_ref[["data"]][["authors"]][[i]]$last_name, ', ', 
                                                  record_list2_ref[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
    }
    title_ref <- record_list2_ref[["data"]][["title"]]
    year_ref <- record_list2_ref[["data"]][["year_published"]]
    abstract_ref <- record_list2_ref[["data"]][["abstract"]]
    start_page_ref <- record_list2_ref[["data"]][["start_page"]]
    end_page_ref <- record_list2_ref[["data"]][["end_page"]]
    source_title_ref <- record_list2_ref[["data"]][["source"]][["title"]]
    volume_ref <- record_list2_ref[["data"]][["volume"]]
    issue_ref <- record_list2_ref[["data"]][["issue"]]
    publisher_ref <- record_list2_ref[["data"]][["source"]][["publisher"]]
    issn_ref <- record_list2_ref[["data"]][["source"]][["issn"]]
    doi_ref <- unlist(lapply(record_list2_ref[["data"]][["external_ids"]], function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_table_ref <- data.table(authors = authors_ref,
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
    
    report_ref <- stage1_report_ref
    
    
    # citations per article
    citation_count <- record_list[["data"]][["scholarly_citations_count"]]
    all_citations <- sum(citation_count, na.rm = TRUE)
    citations <- unlist(record_list[["data"]][["scholarly_citations"]])
    citations_unique <- unique(citations)
    
    stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', 
                                all_citations, ' times. In total, this corresponds to a total of ', 
                                length(citations_unique), ' records on lens.org.')
    
    # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
    request2_cit <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(citations_unique, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
    
    #perform references search and extract text results
    data2_cit <- getLENSData(token, request2_cit)
    record_json2_cit <- httr::content(data2_cit, "text")
    
    #convert json output from references search to list and build download report
    record_list2_cit <- jsonlite::fromJSON(record_json2_cit) 
    
    # convert json to ris style
    type_list <- data.frame(type = c("ABST", "BOOK", "CHAP", "COMP", "CONF", "DATA", "JOUR"), 
                            description = c("abstract reference", "whole book reference", "book chapter reference", "computer program", "conference proceeding", "data file", "journal/periodical reference"), 
                            publication_type = c("reference entry", "book", "book chapter", "component", "conference proceedings", "dataset", "journal article"))
    publication_type_cit <- record_list2_cit[["data"]][["publication_type"]]
    authors_cit <- list()
    for (i in 1:length(record_list2_cit[["data"]][["authors"]])) {
      authors_cit <- unlist(c(authors_cit, paste0(record_list2_cit[["data"]][["authors"]][[i]]$last_name, ', ', 
                                                  record_list2_cit[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
    }
    title_cit <- record_list2_cit[["data"]][["title"]]
    year_cit <- record_list2_cit[["data"]][["year_published"]]
    abstract_cit <- record_list2_cit[["data"]][["abstract"]]
    start_page_cit <- record_list2_cit[["data"]][["start_page"]]
    end_page_cit <- record_list2_cit[["data"]][["end_page"]]
    source_title_cit <- record_list2_cit[["data"]][["source"]][["title"]]
    volume_cit <- record_list2_cit[["data"]][["volume"]]
    issue_cit <- record_list2_cit[["data"]][["issue"]]
    publisher_cit <- record_list2_cit[["data"]][["source"]][["publisher"]]
    issn_cit <- record_list2_cit[["data"]][["source"]][["issn"]]
    doi_cit <- unlist(lapply(record_list2_cit[["data"]][["external_ids"]], function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    level1_table_cit <- data.table(authors = authors_cit,
                                 year = year_cit,
                                 title = title_cit,
                                 source_title = source_title_cit,
                                 publisher = publisher_cit,
                                 volume = volume_cit,
                                 issue = issue_cit,
                                 start_page = start_page_cit,
                                 end_page = end_page_cit,
                                 doi = doi_cit)
    
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
    
    report_cit <- stage1_report_cit

    return(list(display_cit = level1_table_cit, ris_cit = level1_ris_cit, report_cit = report_cit,
                display_ref = level1_table_ref, ris_ref = level1_ris_ref, report_ref = report_ref))
    
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
                         token = 'xxx'){
  
  if(length(article_list) == 1){
    article_list <- trimws(unlist(strsplit(article_list, '[,]')))
  }
  
  request <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"', type, '": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":1000\n}')
  
  # perform article search and extract text results
  data <- getLENSData(token, request)
  record_json <- httr::content(data, "text")
  
  # convert json output from article search to list
  record_list <- jsonlite::fromJSON(record_json) 
  
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
  doi <- unlist(lapply(record_list[["data"]][["external_ids"]], function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
  
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
                              'TY  - ', expss::vlookup(publication_type, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
  
  return(list(display = article_table, ris = article_ris))
  
}


