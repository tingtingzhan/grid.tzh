

#' @title Finetune Function \link[consort]{consort_plot}
#' 
#' @description
#' Some minor touch-ups of function \link[consort]{consort_plot}.
#' 
#' @param data \link[base]{data.frame}, see function \link[consort]{consort_plot}
#' 
#' @param orders \link[base]{character} \link[base]{vector}, see function \link[consort]{consort_plot}
#' 
#' @param sidebox_pattern \link[base]{regex}
#' 
#' @param ... additional parameters of \link[consort]{consort_plot}
#' 
#' 
#' @details
#' Function [consort_rx()]
#' \itemize{
#' \item{finds the argument `side_box` of function \link[consort]{consort_plot}, from the argument of `orders`.}
#' }
#' 
#' @returns
#' Function [consort_rx()] returns a 
#' \link[consort]{consort_plot} object.
#' 
#' @keywords internal
#' @importFrom consort consort_plot
#' @export
consort_rx <- function(
    data, 
    orders, 
    sidebox_pattern = '^sidebox_', 
    ...
) {
  
  if (!is.character(orders) || anyNA(orders)) stop('currently only supports `orders` of character vector')
  nm <- names(orders)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('illegal `orders` names')
  
  consort_plot(
    data = data, orders = orders, 
    side_box = grepv(pattern = sidebox_pattern, x = nm),
    ...
  )
  
}



#' @title Markdown Lines for \link[consort]{consort_plot}
#' 
#' @param x a \link[consort]{consort_plot}
#' 
#' @param ... additional parameters of function \link[rmd.tzh]{md_.default}
#' 
#' @examples
#' # example from \pkg{consort} vignette
#' library(consort)
#' data(dispos.data)
#' out = consort_plot(data = dispos.data, orders = c(
#'   trialno = 'Population',
#'   exclusion = 'Excluded',
#'   trialno = 'Allocated',
#'   subjid_notdosed = 'Not dosed',
#'   followup = 'Followup'
#' ), side_box = c('exclusion', 'subjid_notdosed'), cex = 0.9)
#' 
#' list(consort = out) |> 
#'   rmd.tzh::render_(file = 'consort')
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export md_.consort
#' @export
md_.consort <- function(x, ...) {
  
  attr(x, which = 'text') <- 'CONSORT [Consolidated Standards of Reporting Trials, @Schulz10] diagram is created by <u>**`R`**</u> package <u>**`consort`**</u>.' |>
    new(Class = 'md_lines', package = 'consort', bibentry = bibentry(
      bibtype = 'article', key = 'Schulz10',
      author = 'Kenneth F. Schulz and Douglas G. Altman and David Moher',
      title = 'CONSORT 2010 Statement: updated guidelines for reporting parallel group randomised trials',
      volume = '340',
      year = '2010',
      doi = '10.1136/bmj.c332',
      journal = 'BMJ'
    ))
  
  # NextMethod(generic = 'md_') 
  # does NOT work! 
  # \link[consort]{consort_plot} returns an object of class "consort" "list"
  # dispatch to \link[rmd.tzh]{md_.list} 
  # instead of \link[rmd.tzh]{md_.default} 
  
  md_.default(x, ...)
  
}









#' @title sidebox
#' 
#' @param pattern \link[base]{regex} of *ineligibility* criterion
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [sidebox()] converts ineligibility criterion in *side box* into 
#' a \link[base]{logical} \link[base]{matrix} of **eligibility** criterion.
#' 
#' @returns 
#' Function [sidebox()] returns a \link[base]{matrix}
#' 
#' @keywords internal
#' @export
sidebox <- function(pattern = '^sidebox_', data, ...) {
  # do not want to @importFrom ThomasJeffersonUniv select_rx
  nm <- names(data)
  x <- data[grepl(pattern = pattern, x = nm)] |>
    is.na() # missing indicates eligibility in \CRANpkg{consort}
  # stopifnot(is.matrix(x))
  colnames(x) <- gsub(pattern = pattern, replacement = '', x = colnames(x))
  return(x)
}





#' @title Element-Wise Concatenation of Non-Missing Entries 
#' 
#' @description
#' Element-wise concatenation of non-\link[base]{missing} entries.
#' 
#' @param ... two or more \link[base]{character} \link[base]{vector}s
#' 
#' @param sep \link[base]{character} scalar
#' 
#' @returns
#' Function [paste_nna_()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @details
#' Function [paste_nna_()] can be used to combine two or more reasons in a \CRANpkg{consort} diagram.
#' 
#' @examples
#' x = c(NA_character_, 'x1', 'x2')
#' y = c(NA_character_, 'y1', NA_character_)
#' paste(x, y, sep = '; ')
#' paste_nna_(x, y, sep = '; ')
#' @keywords internal
#' @export
paste_nna_ <- function(..., sep = '; ') {
  
  (function(..., collapse) {
    x <- unique(c(...))
    if (identical(x, NA_character_)) return(NA_character_)
    x0 <- x[!is.na(x)]
    paste(x0, collapse = collapse)
  }) |>
    .mapply(dots = list(...), MoreArgs = list(collapse = sep)) |>
    unlist()

}



