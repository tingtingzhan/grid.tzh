
#' @title Modify `$label` of a `'text'` \link[grid]{grob}
#' 
#' @param pattern,replacement,... see function \link[base]{gsub}
#' 
#' @param x see **Usage**
#' 
#' @keywords internal
#' @name gsub_text_label
#' @export
gsub_text_label <- function(pattern, replacement, x, ...) UseMethod(generic = 'gsub_text_label', object = x)

#' @rdname gsub_text_label
#' @export gsub_text_label.default
#' @export
gsub_text_label.default <- function(pattern, replacement, x, ...) return(x) # exception handling

#' @rdname gsub_text_label
#' @export gsub_text_label.grob
#' @export
gsub_text_label.grob <- function(pattern, replacement, x, ...) {
  # we expect class(x) to be c('text', 'grob', 'gDesc')
  if (!inherits(x, what = 'text')) return(x) # e.g., c('polygon', 'grob', 'gDesc')
  x$label <- gsub(pattern = pattern, replacement = replacement, x = x$label, ...)
  return(x)
}

#' @rdname gsub_text_label
#' @export gsub_text_label.gList
#' @export
gsub_text_label.gList <- function(pattern, replacement, x, ...) {
  x[] <- x |> 
    lapply(FUN = gsub_text_label, pattern = pattern, replacement = replacement, ...)
  return(x)
}





