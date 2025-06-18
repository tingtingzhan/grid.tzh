

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
#' \item{returns a \link[grid]{grob} object.}
#' }
#' 
#' @returns
#' Function [consort_rx()] returns a 
#' \link[grid]{grob} object.
#' 
#' @keywords internal
#' @importFrom consort consort_plot build_grid
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
  
  ret <- consort_plot(
    data = data, orders = orders, 
    side_box = grepv(pattern = sidebox_pattern, x = nm),
    ...
  ) |>
    build_grid()
  
  attr(ret, which = 'text') <- 'Consort diagram is created by <u>**`R`**</u> package <u>**`consort`**</u>.'

  return(ret)
  # returned object class c('gtable', 'gTree', 'grob', 'gDesc')
  
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



