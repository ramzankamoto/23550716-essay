# Convert data to return matrix
prepare_return_matrix <- function(J200) {
    return_mat <- J200 %>%
        select(date, Tickers, return) %>%
        spread(Tickers, return)

    colnames(return_mat) <- gsub("\\ SJ Equity", "", colnames(return_mat))

    return_mat
}