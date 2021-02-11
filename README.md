# citationchaser <img src="inst/extdata/citationchaser.png" align="right" width="15%"/>

In searching for research articles, we often want to obtain lists of references from across studies, and also obtain lists of articles that cite a particular study. In systematic reviews, this supplementary search technique is known as 'citation chasing': forward citation chasing looks for all records citing one or more articles of known relevance; backward ciation chasing looks for all records referenced in one or more articles. 

Traditionally, this process would be done manually, and the resulting records would need to be checked one-by-one against included studies in a review to identify potentially relevant records that should be included in a review. 

This package contains functions to automate this process by making use of the Lens.org API. An input article list can be used to return a list of all referenced records, and/or all citing records in the Lens.org database (consisting of PubMed, PubMed Central, CrossRef, Microsoft Academic Graph and CORE; 'https://www.lens.org').

Please cite as:<br>
Neal R Haddaway. 2021. citationchaser: an R package for forward and backward citations chasing in academic searching. doi: <a href="https://doi.org/10.5281/zenodo.4287835" target="_blank">10.5281/zenodo.4287835</a><br>
<a id="raw-url" href="https://raw.githubusercontent.com/nealhaddaway/PRISMA2020/master/inst/extdata/citation.ris">Citation in .ris format (right click 'Save Link As')</a>

<!-- badges: start -->
![GitHub all releases](https://img.shields.io/github/downloads/nealhaddaway/citationchaser/total)
![GitHub Release Date](https://img.shields.io/github/release-date/nealhaddaway/citationchaser)
![GitHub R package version](https://img.shields.io/github/r-package/v/nealhaddaway/citationchaser)

![GitHub Repo stars](https://img.shields.io/github/stars/nealhaddaway/citationchaser?style=social)
![Twitter URL](https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Fwww.twitter.com%2Fnealhaddaway)
<!-- badges: end -->
