

#' @title Markdown Lines for \link[grid]{grob}
#' 
#' @param x a \link[grid]{grob}
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @importFrom methods new
#' @export md_.grob
#' @export
md_.grob <- function(x, xnm, ...) {
  
  # a simplified version from 
  # ?fastmd::md_.default
  
  z1 <- (attr(x, which = 'text', exact = TRUE) %||% character()) |>
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}',
    '#| echo: false', 
    
    # len-0 compatible
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    x |>
      attr(which = 'fig-cap', exact = TRUE) |> 
      sprintf(fmt = '#| fig-cap: \"%s\"'),
    # end of len-0 compatible
    
    xnm |> sprintf(fmt = 'grid::grid.draw(%s)'), 

    '```'
  ) |> new(Class = 'md_lines')
  
  return(c(z1, z2)) # [c.md_lines()]
  
}
