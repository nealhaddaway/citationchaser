#' get_refs function for citation chasing in systematic reviews
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
#' @param save_object Option to save the resultant ris file as an object in 
#' the Global Environment. The default is FALSE.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#' applying for scholarly API access and creating a token once approved. See 
#' 'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @return An RIS file is saved to the workign directory. A report is printed 
#' to the console. If 'save_object=TRUE', the RIS file is returned as an 
#' object.
#' @examples 
#' \notrun {
#' article_list <- c("10.1186/s13750-018-0126-2", "10.1002/jrsm.1378");
#' refs <- get_refs(article_list);
#' refs
#' }
#' @export
get_refs <- function(article_list,
                     type = 'doi',
                     save_object = FALSE,
                     #dedup = FALSE,
                     #layers = 1,
                     token = 'K4FaRccEYJJgd0MaSUVy1MXNcgzvB1Y25PWfyEeNicqxmndHKk0v'){
  
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
  
  # obtain reference lists from article search
  references <- unlist(record_list[["data"]][["references"]])
  all_citations <- length(references)
  
  # deduplicate shared references and build deduplication report
  references <- unique(references)
  input_number <- record_list[["total"]]
  deduped_citations <- length(references)
  duplicates <- all_citations - deduped_citations
  deduplication_report <- paste0('Your ', input_number, ' articles contained a total of ', all_citations, ' references.\nThis corresponds to ', deduped_citations, ' unique records in lens.org.\n\n')
  
  # build query for article references - this will pull back a maximum of 1,000 hits from lens.org, so not great if lots of refs
  request2 <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(references, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
  
  #perform references search and extract text results
  data2 <- getLENSData(token, request2)
  record_json2 <- content(data2, "text")
  
  #convert json output from references search to list and build download report
  record_list2 <- fromJSON(record_json2) 
  
  download_report <- paste0('Your query returned ', record_list2[["total"]], ' records from lens.org.\n\n')
  
  # convert json to ris style
  type_list <- read.csv("inst/extdata/type.csv")
  publication_type <- record_list2[["data"]][["publication_type"]]
  authors <- list()
  for (i in 1:length(record_list2[["data"]][["authors"]])) {
    authors <- unlist(c(authors, paste0(record_list2[["data"]][["authors"]][[i]]$last_name, ', ', 
                                        record_list2[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
  }
  title <- record_list2[["data"]][["title"]]
  year <- record_list2[["data"]][["year_published"]]
  abstract <- record_list2[["data"]][["abstract"]]
  start_page <- record_list2[["data"]][["start_page"]]
  end_page <- record_list2[["data"]][["end_page"]]
  source_title <- record_list2[["data"]][["source"]][["title"]]
  volume <- record_list2[["data"]][["volume"]]
  issue <- record_list2[["data"]][["issue"]]
  publisher <- record_list2[["data"]][["source"]][["publisher"]]
  issn <- record_list2[["data"]][["source"]][["issn"]]
  options(warn=-1)
  ext_ids <- expss::vlookup('doi', record_list2[["data"]][["external_ids"]], result_column = 'value', lookup_column = 'type')
  options(warn=0)
  external_ids <- list()
  for (i in 1:length(record_list2[["data"]][["external_ids"]])) {
    external_ids <- unlist(c(external_ids, paste0(record_list2[["data"]][["external_ids"]][[i]]$type, ', ', 
                                                  record_list2[["data"]][["external_ids"]][[i]]$value, collapse = '; ')))
  }
  doi <- sub('.*doi, ', '', external_ids)
  
  level1_ris <- paste(paste0('\n',
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
                             #'SN  - ', issn, '\n',
                             'DO  - ', doi, '\n',
                             'ER  - '),
                      collapse = '\n')
  
  # ris build report
  ris_records <- lengths(regmatches(level1_ris, gregexpr("TY  - ", level1_ris)))
  risbuild_report <- paste0('Your output report contains ', ris_records, ' records and has been saved as "output.ris" in your working directory.\n\n')
  
  articles_ris <- level1_ris
  
  # deduplicate
  #if (dedup == TRUE) {
  #  finalrefs <- record_list2 
  #} else {
  #  finalrefs <- record_list2
  #}
  
  if (layers == 2) {
    
    # build query for article references
    level2refs <- unlist(record_list2[["data"]][["references"]])
    
    request3 <- paste0('{
	"query": {
		"terms": {
			"lens_id": [', paste0('"', paste(level2refs, collapse = '", "'), '"'), ']
		}
	},
	"size":1000
}')
    
    #perform references search and extract text results
    ###max results set to 1,000 (can loop through building requests and calls for more than 1,000 records)
    data3 <- getLENSData(token, request3)
    record_json3 <- content(data3, "text")
    
    #convert json output from references search to list
    record_list3 <- fromJSON(record_json3) 
    
    # convert json to ris style
    publication_type <- record_list3[["data"]][["publication_type"]]
    authors <- list()
    for (i in 1:length(record_list3[["data"]][["authors"]])) {
      authors <- unlist(c(authors, paste0(record_list3[["data"]][["authors"]][[i]]$last_name, ', ', 
                                          record_list3[["data"]][["authors"]][[i]]$first_name, collapse = '; ')))
    }
    title <- record_list3[["data"]][["title"]]
    year <- record_list3[["data"]][["year_published"]]
    abstract <- record_list3[["data"]][["abstract"]]
    start_page <- record_list3[["data"]][["start_page"]]
    end_page <- record_list3[["data"]][["end_page"]]
    source_title <- record_list3[["data"]][["source"]][["title"]]
    volume <- record_list3[["data"]][["volume"]]
    issue <- record_list3[["data"]][["issue"]]
    publisher <- record_list3[["data"]][["source"]][["publisher"]]
    issn <- unlist(record_list3[["data"]][["source"]][["issn"]])
    ext_ids <- expss::vlookup('doi', record_list3[["data"]][["external_ids"]], result_column = 'value', lookup_column = 'type')
    external_ids <- list()
    for (i in 1:length(record_list3[["data"]][["external_ids"]])) {
      external_ids <- unlist(c(external_ids, paste0(record_list3[["data"]][["external_ids"]][[i]]$type, ', ', 
                                                    record_list3[["data"]][["external_ids"]][[i]]$value, collapse = '; ')))
    }
    doi <- sub('.*doi, ', '', external_ids)
    
    level2_ris <- paste(paste0('\n',
                               'TY  - ', expss::vlookup(publication_type, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
                               'AU  - ', authors, '\n',
                               'TI  - ', title, '\n',
                               'PY  - ', year, '\n',
                               'AB  - ', abstract, '\n',
                               'SP  - ', start_page, '\n',
                               'EP  - ', end_page, '\n',
                               'JF  - ', source_title, '\n',
                               'VL  - ',  volume, '\n',
                               'IS  - ', issue, '\n',
                               'PB  - ', publisher, '\n',
                               #'SN  - ', issn, '\n',
                               'DO  - ', doi, '\n',
                               'ER  - '),
                        collapse = '\n')
  
    articles_ris <- unlist(c(articles_ris, level2_ris))
      
  }
  
  write.table(articles_ris, file = "output.ris", sep = "")
  
  cat(paste0('#Duplication\n', deduplication_report, '#Download\n', download_report, '#RIS file build\n', risbuild_report))
  
  if (save_object == TRUE) {
    return(articles_ris)
  }
}


#' Function to query Lens.org
#' 
#' @description Function written by lens.org for use of their API.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#' applying for scholarly API access and creating a token once approved. See 
#' 'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @return A summary response. The results are viewable using 
#' 'content(data, "text")'. Other details regarding the request (e.g. repsonse 
#' times) can be accessed through the main output.
#' @export
getLENSData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, add_headers(.headers=headers), body = query)
}
