parse_groupings <- function(x, nrow, ncol, names = x){
    l <- lapply(x, parse_grouping, nrow = nrow, ncol = ncol)
    l
}

parse_grouping <- function(x, nrow, ncol, name = x){
    stopifnot(length(x) == 1)
    if (x == '')
        return(list())
    plate_split <- strsplit(x, '!', fixed = TRUE)[[1]]

    if (length(plate_split) == 2) {
        x <- plate_split[2]
        output <- list(plate = plate_split[1])
    } else {
        output <- list()
    }
    l <- strsplit(x, '->', fixed = TRUE)[[1]]
    if (length(l) == 1) {
        return(c(output, list(group = name, well = x, is_ref = TRUE)))
    }
    l <- lapply(l, strsplit, split = ',', fixed = TRUE)
    from <- unlist(lapply(l[[1]][[1]], expand_range, nrow = nrow, ncol = ncol))
    to <- unlist(lapply(l[[2]][[1]], expand_range, nrow = nrow, ncol = ncol))
    set <- unique(c(from, to))
    is_ref <- set %in% from
    c(output, list(group = name, well = set, is_ref = is_ref))
}

expand_range <- function(x, nrow, ncol) {
    query <- strsplit(x, split = ':', fixed = TRUE)[[1]]
    if (length(query) == 1)
        return(x)
    wells <- mtputils::well_labels(nrow = nrow, ncol = ncol)
    m <- matrix(wells, nrow = nrow, ncol = ncol, byrow = TRUE)

    fr <- which(m == query[1], arr.ind = TRUE)
    to <- which(m == query[2], arr.ind = TRUE)
    row_range <- range(c(fr[1], to[1]))
    col_range <- range(c(fr[2], to[2]))
    rows <- seq.int(row_range[1], row_range[2])
    cols <- seq.int(col_range[1], col_range[2])
    output <- m[rows, cols]
    dim(output) <- NULL
    output
}


# relatively --------------------------------------------------------------

#' @export
relatively <- function(values, is_ref, agg_fxn = mean) {
    values / agg_fxn(values[is_ref])
}



# add_grouping ------------------------------------------------------------

#' @export
add_grouping <- function(data, x, nrow, ncol) {
    group_tbl <- tibble::as_tibble(parse_grouping(x, nrow, ncol))
    by = "well"
    if ("plate" %in% names(group_tbl))
        by <- c("plate", "well")
    dplyr::left_join(data, group_tbl, by = by)
}

#' @export
add_groupings <- function(data, x, nrow, ncol) {
    l <- parse_groupings(x, nrow, ncol)
    group_tbl <- purrr::map_df(l, tibble::as_tibble)
    by = "well"
    if ("plate" %in% names(group_tbl))
        by <- c("plate", by)
    dplyr::left_join(data, group_tbl, by = by)
}
