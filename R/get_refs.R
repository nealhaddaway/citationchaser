#' Automated citation chasing in systematic reviews
#' 
#' @description This function takes a list of articles in the form of established 
#' identifiers (e.g. digital object identifiers) and sends a request to the 
#' lens.org API to firstly identify all cited references in all articles (in the
#' form of lists of lens IDs), and then query these lens IDs to bring back full 
#' citation information for all listed records. Deduplicates references to the 
#' same records across articles, resulting in an RIS file and a summary report 
#' in the console.
#' @param article_list List of article identifiers for which the reference 
#' lists will be returned. Must be a list/vector of identifiers, e.g. 
#' '"10.1186/s13750-018-0126-2" "10.1002/jrsm.1378"'.
#' @param type Specification of the type of input provided. The default is 
#' 'doi' (digital object identifier), but any of the following are accepted: 
#' "pmid" (PubMed ID), "pmcid" (PubMed Central ID), "magid" (Microsoft 
#' Academic ID), "coreid" (CORE identifier), lens_id" (The Lens.org ID), 
#' "title" (article title; much lower specificity).
#' @param get_records Specification of whether to look for records referenced 
#' within the input articles ('references'), records citing the input articles 
#' ('citations'), or both ('both'). 
#' @param save_object Option to save the resultant ris file as an object in 
#' the Global Environment. The default is FALSE.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#' applying for scholarly API access and creating a token once approved. See 
#' 'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @return An RIS file is saved to the working directory. A report is printed 
#' to the console. If 'save_object=TRUE', the RIS file is returned as an 
#' object.
#' @export
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
  request1 <- paste0('{
	"query": {
		"terms": {
			"doi": ["', paste0('', paste(article_list, collapse = '", "'), '"'),']
		}
	},
	"size":1000
}')
  
  # perform article search and extract text results
  data <- getLENSData(token, request1)
  record_json <- content(data, "text")
  
  # convert json output from article search to list
  record_list <- fromJSON(record_json) 
  record_listdb <- fromJSON(record_json) %>% as.data.frame
  

    if (get_records == 'citations') {
      
      # citations per article
      citation_count <- record_list[["data"]][["scholarly_citations_count"]]
      all_citations <- sum(citation_count, na.rm = TRUE)
      citations <- unlist(record_list[["data"]][["scholarly_citations"]])
      
      stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', all_citations, ' times. The following number of citations for each input article were found on lens.org:\n\n',
                                  paste(paste0('doi: ', article_list, ', citations: ', record_list[["data"]][["scholarly_citations_count"]]), collapse = '\n'))
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request2_cit <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(citations, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      
      #perform references search and extract text results
      data2_cit <- getLENSData(token, request2_cit)
      record_json2_cit <- content(data2_cit, "text")
      
      #convert json output from references search to list and build download report
      record_list2_cit <- fromJSON(record_json2_cit) 
      download_report_cit <- paste0('Your query returned ', record_list2_cit[["total"]], ' records from lens.org.\n\n')
      
      # convert json to ris style
      type_list <- read.csv("inst/extdata/type.csv")
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
      doi_cit <- unlist(lapply(record_list2_cit[["data"]][["external_ids"]], function(ch) vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
      
      level1_ris_cit <- paste(paste0('\n',
                                     'TY  - ', vlookup(publication_type_cit, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
                                     #'SN  - ', issn_cit, '\n',
                                     'DO  - ', doi_cit, '\n',
                                     'ER  - '),
                              collapse = '\n')
      
      # ris build report
      ris_records_cit <- lengths(regmatches(level1_ris_cit, gregexpr("TY  - ", level1_ris_cit)))
      risbuild_report_cit <- paste0('Your output report contains ', ris_records_cit, ' records and has been saved as "citations.ris" in your working directory.\n\n')
      
      citations_ris <- level1_ris_cit
      
      write.table(citations_ris, file = "citations.ris", sep = "")
      cat(paste0('#Summary\n', stage1_report_cit, '#Download\n', download_report_cit, '#RIS file build\n', risbuild_report_cit))
      if (save_object == TRUE) {
        return(citations_ris)
      }
    } else if (get_records == 'references') {
      
      # obtain reference lists from article search
      references <- unlist(record_list[["data"]][["references"]])
      all_refs <- length(references)
      
      # deduplicate shared references and build deduplication report
      references <- unique(references)
      input_number <- record_list[["total"]]
      deduped_citations <- length(references)
      duplicates <- all_refs - deduped_citations
      stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_refs, ' references. The input articles contained the following identifiable references on lens.org: \n',
                                  paste(paste0('doi: ', article_list, ', references: ', record_list[["data"]][["references_count"]]), collapse = '\n'),
                                  '\n\nThis corresponds to ', deduped_citations, ' unique records.\n\n')
      
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
      record_json2_ref <- content(data2_ref, "text")
      
      #convert json output from references search to list and build download report
      record_list2_ref <- fromJSON(record_json2_ref) 
      download_report_ref <- paste0('Your query returned ', record_list2_ref[["total"]], ' records from lens.org.\n\n')
      
      # convert json to ris style
      type_list <- read.csv("inst/extdata/type.csv")
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
      doi_ref <- unlist(lapply(record_list2_ref[["data"]][["external_ids"]], function(ch) vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
      
      level1_ris_ref <- paste(paste0('\n',
                                     'TY  - ', vlookup(publication_type_ref, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
                                     #'SN  - ', issn_ref, '\n',
                                     'DO  - ', doi_ref, '\n',
                                     'ER  - '),
                              collapse = '\n')
      
      # ris build report
      ris_records_ref <- lengths(regmatches(level1_ris_ref, gregexpr("TY  - ", level1_ris_ref)))
      risbuild_report_ref <- paste0('Your output report contains ', ris_records_ref, ' records and has been saved as "references.ris" in your working directory.\n\n')
      
      references_ris <- level1_ris_ref
      
      write.table(references_ris, file = "references.ris", sep = "")
      cat(paste0('#Summary\n', stage1_report_ref, '#Download\n', download_report_ref, '#RIS file build\n', risbuild_report_ref))
      if (save_object == TRUE) {
        return(references_ris)
      }
    } else if (get_records == 'both') {
      
      # obtain reference lists from article search
      references <- unlist(record_list[["data"]][["references"]])
      all_refs <- length(references)
      
      # deduplicate shared references and build deduplication report
      references <- unique(references)
      input_number <- record_list[["total"]]
      deduped_citations <- length(references)
      duplicates <- all_refs - deduped_citations
      stage1_report_ref <- paste0('Your ', input_number, ' articles contained a total of ', all_refs, ' references. The input articles contained the following identifiable references on lens.org: \n',
                                  paste(paste0('doi: ', article_list, ', references: ', record_list[["data"]][["references_count"]]), collapse = '\n'),
                                  '\n\nThis corresponds to ', deduped_citations, ' unique records.\n\n')
      
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
      record_json2_ref <- content(data2_ref, "text")
      
      #convert json output from references search to list and build download report
      record_list2_ref <- fromJSON(record_json2_ref) 
      download_report_ref <- paste0('Your query returned ', record_list2_ref[["total"]], ' records from lens.org.\n\n')
      
      # convert json to ris style
      type_list <- read.csv("inst/extdata/type.csv")
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
      doi_ref <- unlist(lapply(record_list2_ref[["data"]][["external_ids"]], function(ch) vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
      
      level1_ris_ref <- paste(paste0('\n',
                                     'TY  - ', vlookup(publication_type_ref, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
                                     #'SN  - ', issn_ref, '\n',
                                     'DO  - ', doi_ref, '\n',
                                     'ER  - '),
                              collapse = '\n')
      
      # ris build report
      ris_records_ref <- lengths(regmatches(level1_ris_ref, gregexpr("TY  - ", level1_ris_ref)))
      risbuild_report_ref <- paste0('Your output report contains ', ris_records_ref, ' records and has been saved as "references.ris" in your working directory.\n\n')
      
      references_ris <- level1_ris_ref
      
      write.table(references_ris, file = "references.ris", sep = "")
      cat(paste0('#Summary\n', stage1_report_ref, '#Download\n', download_report_ref, '#RIS file build\n', risbuild_report_ref))
      
      
      # citations per article
      citation_count <- record_list[["data"]][["scholarly_citations_count"]]
      all_citations <- sum(citation_count, na.rm = TRUE)
      citations <- unlist(record_list[["data"]][["scholarly_citations"]])
      
      stage1_report_cit <- paste0('Your ', input_number, ' articles were cited a total of ', all_citations, ' times. The following number of citations for each input article were found on lens.org:\n\n',
                                  paste(paste0('doi: ', article_list, ', citations: ', record_list[["data"]][["scholarly_citations_count"]]), collapse = '\n'))
      
      # build query for article citations - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
      request2_cit <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(citations, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
      
      #perform references search and extract text results
      data2_cit <- getLENSData(token, request2_cit)
      record_json2_cit <- content(data2_cit, "text")
      
      #convert json output from references search to list and build download report
      record_list2_cit <- fromJSON(record_json2_cit) 
      download_report_cit <- paste0('Your query returned ', record_list2_cit[["total"]], ' records from lens.org.\n\n')
      
      # convert json to ris style
      type_list <- read.csv("inst/extdata/type.csv")
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
      doi_cit <- unlist(lapply(record_list2_cit[["data"]][["external_ids"]], function(ch) vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
      
      level1_ris_cit <- paste(paste0('\n',
                                     'TY  - ', vlookup(publication_type_cit, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
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
                                     #'SN  - ', issn_cit, '\n',
                                     'DO  - ', doi_cit, '\n',
                                     'ER  - '),
                              collapse = '\n')
      
      # ris build report
      ris_records_cit <- lengths(regmatches(level1_ris_cit, gregexpr("TY  - ", level1_ris_cit)))
      risbuild_report_cit <- paste0('Your output report contains ', ris_records_cit, ' records and has been saved as "citations.ris" in your working directory.\n\n')
      
      citations_ris <- level1_ris_cit
      
      write.table(citations_ris, file = "citations.ris", sep = "")
      cat(paste0('#Summary\n', stage1_report_cit, '#Download\n', download_report_cit, '#RIS file build\n', risbuild_report_cit))
      if (save_object == TRUE) {
        return(list(citations = citations_ris, references = references_ris))
      }
    } 
}


#' Function to query Lens.org
#' 
#' @description Function written by lens.org for use of their API.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#' applying for scholarly API access and creating a token once approved. See 
#' 'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @param query A search string formulated according to the Lens.org API 
#' documentation: 'https://docs.api.lens.org/request-scholar.html'.
#' @return A summary response. The results are viewable using 
#' 'content(data, "text")'. Other details regarding the request (e.g. repsonse 
#' times) can be accessed through the main output.
#' @export
getLENSData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  POST(url = url, add_headers(.headers=headers), body = query)
}
