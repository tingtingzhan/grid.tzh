

#' @title \link[grid]{unit}
#' 
#' @param x,e1,e2 \link[grid]{unit}
#' 
#' @param value ..
#' 
#' @note
#' 
#' Accepted units for function \link[grid]{unit} can be found in
#' `R source/src/library/grid/src/unit.c` file.
#' 
#' Only **length** units for plot are accepted.
#' Weight units (such as kg or lb) are not accepted!
#' 
#' @examples
#' library(grid)
#' (x = unit(10, units = 'cm'))
#' units(x) = 'inches'
#' x
#' 
#' (x1 = unit(rnorm(10), units = 'inches'))
#' (x2 = unit(rnorm(10), units = 'cm'))
#' x1 > x2
#' 
#' 
#' @name grid_unit
#' @importFrom grid convertUnit
#' @export `units<-.unit`
#' @export
`units<-.unit` <- function(x, value) {
  # S3 generic ?base::`units<-`
  convertUnit(x, unitTo = value) # why not in \pkg{grid}??
}

#' @rdname grid_unit
#' @export units.unit
#' @export
units.unit <- function(x) {
  # S3 generic ?base::units
  # ?grid::unit
  # ?grid:::print.unit
  # ?grid:::as.character.unit
  # ?grid:::unitDesc
  u <- c('npc', 'cm', 'inches', 'mm', 'points', 'picas', 'bigpts', 'dida', 'cicero', 
         'scaledpts', 'lines', 'char', 'native', 'snpc', 'strwidth', 'strheight', 'grobwidth', 'grobheight')
  u[attr(x, which = 'unit', exact = TRUE) + 1L] # C-style index, starts with 0L instead of 1L
}



#' @rdname grid_unit
#' @export `>.unit`
#' @export
`>.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) > unclass(e2)
}


#' @rdname grid_unit
#' @export `>=.unit`
#' @export
`>=.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) >= unclass(e2)
}

#' @rdname grid_unit
#' @export `<.unit`
#' @export
`<.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) < unclass(e2)
}
  
#' @rdname grid_unit
#' @export `<=.unit`
#' @export
`<=.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) <= unclass(e2)
}

#' @rdname grid_unit
#' @export `!=.unit`
#' @export
`!=.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) != unclass(e2)
}

#' @rdname grid_unit
#' @export `==.unit`
#' @export
`==.unit` <- function(e1, e2) {
  units(e2) <- units(e1)
  unclass(e1) == unclass(e2)
}


