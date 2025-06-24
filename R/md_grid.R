

md_grid <- function(x, xnm, ...) {
  
  fig.cap <- attr(x, which = 'fig.cap', exact = TRUE)
  txt <- attr(x, which = 'text', exact = TRUE)
  
  #label <- attr(x, which = 'label', exact = TRUE)
  # https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html
  # rmarkdown does *not* provide cross-referencing 
  
  ret <- c(
    txt,
    '\n',
    '```{r}', 
    # let \pkg{grid} does not figure out the width and height very perfectly
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    fig.cap |> sprintf(fmt = '#| fig-cap: %s'), # len-0 compatible
    # 'grid::grid.newpage()', # no need!!
    sprintf(fmt = '%s |> grid::grid.draw()', xnm), 
    # ?grid:::grid.draw.gList
    # ?grid:::grid.draw.grob
    # etc.
    '```'
  )
  bib <- txt |>
    attr(which = 'bibentry', exact = TRUE)
  if (length(bib)) attr(ret, which = 'bibentry') <- bib
  return(ret)
  
}


#' @title Markdown Lines for `gList`
#' 
#' @param x `gList`
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' # see ?venn
#' @keywords internal
#' @importFrom rmd.tzh md_
#' @export md_.gList
#' @export
md_.gList <- md_grid



#' @title Markdown Lines for `gDesc`
#' 
#' @param x `gDesc`
#' 
#' @param xnm ..
#' 
#' @param ... ..
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
#' p = out |> build_grid()
#' attr(p, which = 'text') <- Sprintf_consort()
#' library(rmd.tzh); list(
#'   '`consort`' = p
#' ) |> render_(file = 'gDesc')
#' @keywords internal
#' @importFrom rmd.tzh md_
#' @export md_.gDesc
#' @export
md_.gDesc <- md_grid




if (FALSE) {
  list(
    figure = const_all # Rupsa's study
  ) |> render_(file = 'gDesc')
}
