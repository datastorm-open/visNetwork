mergeLists <- function (base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- mergeLists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
}

toArrayList <- function(obj, names = TRUE){
  value = lapply(1:nrow(obj), function(i) {
    res <- as.list(obj[i, , drop = FALSE])
    if (!names) names(res) <- NULL
    res
  })
  names(value) <- NULL;
  value
}

#' Export magrittr function
#' 
#' Export magrittr function
#' 
#' @importFrom magrittr %>%
#' @name %>%
#' @export
#' @rdname visNetwork-exports
NULL