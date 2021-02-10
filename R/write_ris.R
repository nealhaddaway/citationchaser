

# translate publication_type to RIS format
type_list <- read.csv("inst/extdata/type.csv")
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
issn <- unlist(record_list[["data"]][["source"]][["issn"]])
ext_ids <- expss::vlookup('doi', record_list[["data"]][["external_ids"]], result_column = 'value', lookup_column = 'type')
external_ids <- list()
for (i in 1:length(record_list[["data"]][["external_ids"]])) {
  external_ids <- unlist(c(external_ids, paste0(record_list[["data"]][["external_ids"]][[i]]$type, ', ', 
                                                record_list[["data"]][["external_ids"]][[i]]$value, collapse = '; ')))
}
doi <- sub('.*doi, ', '', external_ids)

articles_ris <- paste(paste0('\n',
                             'TY  - ', expss::vlookup(publication_type, type_list, result_column = 'type', lookup_column = 'publication_type'), '\n',
                             'AU  - ', authors, '\n',
                             'TI  - ', title, '\n',
                             'PY  - ', year, '\\\\n',
                             'AB  - ', abstract, '\n',
                             'SP  - ', start_page, '\n',
                             'EP  - ', end_page, '\n',
                             'JF  - ', source_title, '\n',
                             'VL  - ',  volume, '\n',
                             'IS  - ', issue, '\n',
                             'PB  - ', publisher, '\n',
                             'SN  - ', issn, '\n',
                             'DO  - ', doi, '\n',
                             'ER  - '),
                      collapse = '\n')

write.table(articles_ris, file = "my_data.ris", sep = "")
