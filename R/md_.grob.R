

#' @title Markdown Lines for \link[grid]{grob}
#' 
#' @param x a \link[grid]{grob}
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @param fig.height,fig.width ..
#' 
#' @param fig.cap ..
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @export md_.grob
#' @export
md_.grob <- function(
    x, xnm, ...,
    fig.height = attr(x, which = 'fig-height', exact = TRUE),
    fig.width = attr(x, which = 'fig-width', exact = TRUE),
    fig.cap = attr(x, which = 'fig-cap', exact = TRUE)
) {
  
  # a simplified version from 
  # ?fastmd::md_.default
  
  z1 <- (attr(x, which = 'text', exact = TRUE) %||% character()) |>
    new(Class = 'md_lines')
  
  z2 <- c(
    # len-0 compatible
    fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    fig.cap |> 
      sprintf(fmt = '#| fig-cap: \"%s\"'),
    # end of len-0 compatible
    
    xnm |> 
      sprintf(fmt = 'grid::grid.draw(%s)')
  ) |> new(Class = 'md_lines', chunk.r = TRUE)
  
  return(c(z1, z2)) # [c.md_lines()]
  
}
