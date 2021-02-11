## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE---------------------------------------------------------------
article_list <- c("10.1007/978-3-642-37048-9_13", "10.1111/sum.12030", "10.5194/bg-13-3619-2016", "10.1016/j.agee.2012.09.006")

## ---- echo=TRUE---------------------------------------------------------------
get_refs(article_list, 
         get_records = 'references', 
         token = '&#39;K4FaRccEYJ---REDACTED---ndHKk0v&#39')

