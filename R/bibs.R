

#' @title bibs in grid.tzh package
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @name grid_bib
#' @export
.schulz10 <- \(key = 'Schulz10', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = c(
      person(given = c('Kenneth', 'F.'), family = 'Schulz'), 
      person(given = c('Douglas', 'G.'), family = 'Altman'), 
      person(given = 'David', family = 'Moher')
    ),
    title = 'CONSORT 2010 Statement: updated guidelines for reporting parallel group randomised trials',
    volume = '340',
    year = '2010',
    doi = '10.1136/bmj.c332',
    journal = 'British Medical Journal (The BMJ)'
  )
}


#' @rdname grid_bib
#' @export
.venn <- \(key = 'Venn1880', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = 'John', family = 'Venn'),
    title = 'I. On the diagrammatic and mechanical representation of propositions and reasonings',
    journal = 'The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science',
    volume = '10',
    number = '59',
    pages = '1--18',
    year = '1880',
    publisher = 'Taylor & Francis',
    doi = '10.1080/14786448008626877'
  )
}