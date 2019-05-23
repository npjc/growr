# expand_range ------------------------------------------------------------

test_that("single well returns itself", {
    res <- expand_range('A01', 8, 12)
    expect_equal(res, 'A01')
    res <- expand_range('A01:A01', 8, 12)
    expect_equal(res, 'A01')
})

test_that("row and column expansion work both ways", {

    one_row <- expand_range("A01:A04", nrow = 8, ncol = 12)
    expect_equal(one_row, c("A01", "A02", "A03", "A04"))

    one_row_rev <- expand_range("A04:A01", nrow = 8, ncol = 12)
    expect_equal(one_row, c("A01", "A02", "A03", "A04"))

    one_col <- expand_range("A01:E01", nrow = 8, ncol = 12)
    expect_equal(one_col, c("A01", "B01", "C01", "D01", "E01"))

    one_col_rev <- expand_range("E01:A01", nrow = 8, ncol = 12)
    expect_equal(one_col_rev, c("A01", "B01", "C01", "D01", "E01"))
})

test_that("rectangular expansions works both ways", {
    expected <- c("A01", "B01", "C01", "D01", "A02", "B02", "C02", "D02", "A03",
                  "B03", "C03", "D03")
    expect_equal(expand_range("A01:D03", 8, 12), expected)
    expect_equal(expand_range("D03:A01", 8, 12), expected)
})



# parse_grouping ----------------------------------------------------------

test_that("groups are parsed correctly", {
    res <- parse_grouping("A01->A02", 8, 12)
    expected <- list(group = "A01->A02",
                     well = c("A01", "A02"),
                     is_ref = c(TRUE, FALSE))
    expect_equal(res, expected)
    res <- parse_grouping("p1!A01->A02", 8, 12)
    expected <- list(plate = "p1",
                     group = "A01->A02",
                     well = c("A01", "A02"),
                     is_ref = c(TRUE, FALSE))
    expect_equal(res, expected)

})

test_that("parse grouping with commas", {

    expected <- list(group = "A01,B03->D04,H12",
                     well = c("A01", "B03", "D04", "H12"),
                     is_ref = c(TRUE, TRUE, FALSE, FALSE))
    res <- parse_grouping("A01,B03->D04,H12", 8, 12)
    expect_equal(res, expected)
    expected <- list(plate = "p1",
                     group = "A01,B03->D04,H12",
                     well = c("A01", "B03", "D04", "H12"),
                     is_ref = c(TRUE, TRUE, FALSE, FALSE))
    res <- parse_grouping("p1!A01,B03->D04,H12", 8, 12)
    expect_equal(res, expected)
})

test_that("parse_grouping() supports 1-well groups +/- plate delims", {
    expect_is(parse_grouping('A01', 8, 12), 'list')
    expect_length(parse_grouping('A01', 8, 12), 3)
    expect_is(parse_grouping('p1!A01', 8, 12), 'list')
    expect_length(parse_grouping('p1!A01', 8, 12), 4)
})

test_that("can join groupings onto example 1", {
    skip('not yet tested.')
    y <- tibble::as_tibble(parse_grouping("A01->A02:A12", 8, 12))
    library(dplyr)
    mtp_example1 %>%
        left_join(y, by = 'well')

})

test_that("'A01:H01,A12:H12->B01:H11', parsed as expected", {
    grouping <- "A01:H01,A12:H12->A02:H11"
    res <- parse_grouping(grouping, 8, 12)
    expected <- list(group = grouping,
                     well = c("A01", "B01",
                              "C01", "D01", "E01", "F01", "G01", "H01", "A12", "B12", "C12",
                              "D12", "E12", "F12", "G12", "H12", "A02", "B02", "C02", "D02",
                              "E02", "F02", "G02", "H02", "A03", "B03", "C03", "D03", "E03",
                              "F03", "G03", "H03", "A04", "B04", "C04", "D04", "E04", "F04",
                              "G04", "H04", "A05", "B05", "C05", "D05", "E05", "F05", "G05",
                              "H05", "A06", "B06", "C06", "D06", "E06", "F06", "G06", "H06",
                              "A07", "B07", "C07", "D07", "E07", "F07", "G07", "H07", "A08",
                              "B08", "C08", "D08", "E08", "F08", "G08", "H08", "A09", "B09",
                              "C09", "D09", "E09", "F09", "G09", "H09", "A10", "B10", "C10",
                              "D10", "E10", "F10", "G10", "H10", "A11", "B11", "C11", "D11",
                              "E11", "F11", "G11", "H11"),
                     is_ref = c(rep(TRUE, 16), rep(FALSE, 80)))
    expect_equal(res, expected)
})



test_that("can join groupings onto example 2", {
    skip('not yet tested.')
    library(dplyr)
    mtp_example2 %>%
        add_grouping("A01:H01,A12:H12->A02:H11", 8, 12) %>%
        distinct(well, group, is_ref)

})

test_that("reshaping as expected", {
    library(dplyr)
    library(grid)
    g <- parse_grouping("A01:H01,A12:H12->A02:H11", 8, 12)
    # g <- parse_grouping("A01->A02:H12", 8, 12)
    expanded <- expand.grid(from = g$well[g$is_ref], to = g$well[!g$is_ref], stringsAsFactors = F)
    data <- dplyr::transmute(mtp_example2, well, x = runtime, y = measure)
    data2 <- expanded %>%
        left_join(mtp_example2, by = c("from" = "well")) %>%
        transmute(well = to, id = from, x = runtime, y = measure) %>%
        as_tibble()
    coords <- compute_coords(spec_96well())
    grid.newpage()
    gr <- mtpGrob(coords)
    gt <- well_polyline_gTree(coords, data2, vp = 'vpPlate', gp = grid::gpar(alpha = 0.4))
    well_line_gTree(coords, data, vp = 'vpPlate', gp = grid::gpar(col = 'red'))

})

