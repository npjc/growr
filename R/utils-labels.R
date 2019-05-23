well_labels <- function(lbl_row, lbl_col, nchar_row = max(nchar(lbl_row)),
                        nchar_col = max(nchar(lbl_col)),
                        order = c("rowmajor", "colmajor")) {
    fmt <- paste0("%", nchar_row,"s", "%0", nchar_col, "s")
    order <- match.arg(order)
    nrow <- length(lbl_row)
    ncol <- length(lbl_col)
    switch(order,
           rowmajor = sprintf(fmt, rep(lbl_row[seq_len(nrow)], each = ncol), rep.int(lbl_col, nrow)),
           colmajor = sprintf(fmt, rep.int(lbl_row[seq_len(nrow)], ncol), rep(lbl_col, each = nrow))
    )
}

row_labels <- function(row) {
    # c(LETTERS, paste0(LETTERS[1], LETTERS), paste0(LETTERS[2], LETTERS))
    lbls <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
              "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y",
              "Z", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ",
              "AK", "AL", "AM", "AN", "AO", "AP", "AQ", "AR", "AS", "AT", "AU",
              "AV", "AW", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BF",
              "BG", "BH", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BP", "BQ",
              "BR", "BS", "BT", "BU", "BV", "BW", "BX", "BY", "BZ")
    lbls[row]
}

col_labels <- function(col) {
    as.character(col)
}

.well_labels <- list(
    `12` = well_labels(row_labels(1:3), col_labels(1:4)),
    `24` = well_labels(row_labels(1:4), col_labels(1:6)),
    `48` = well_labels(row_labels(1:6), col_labels(1:8)),
    `96` = well_labels(row_labels(1:8), col_labels(1:12)),
    `384` = well_labels(row_labels(1:16), col_labels(1:24)),
    `1536` = well_labels(row_labels(1:32), col_labels(1:48)),
    `6144` = well_labels(row_labels(1:64), col_labels(1:96))
)

complete_wells <- function(x) {
    x <- unique(x)
    l <- lapply(.well_labels, function(y) all(x %in% y))
    .well_labels[[which.max(l)]]
}
