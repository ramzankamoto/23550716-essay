plot_rolling_risk_parity <- function(return_mat) {
    return_mat_xts <- return_mat["2010-01-04/2024-06-01"]
    rWindows <- rollingWindows(return_mat_xts, period = "12m", by = "3m")

    # Extract dates as a standard vector
    from_dates <- as.Date(rWindows$from@Data)
    to_dates <- as.Date(rWindows$to@Data)

    parity.weights <- map2(from_dates, to_dates,
                           ~ riskParityPortfolio(cov(return_mat_xts[paste0(.x, "/", .y)]))$w)
    names(parity.weights) <- to_dates
    parity.weights <- list.rbind(parity.weights)

    parity_df <- as.data.frame(parity.weights) %>%
        rownames_to_column(var = "Date") %>%
        pivot_longer(-Date, names_to = "Asset", values_to = "Weight") %>%
        mutate(Portfolio = "Risk Parity Portfolio")

    parity_df$Date <- as.Date(parity_df$Date)

    ggplot(parity_df, aes(x = Date, y = Weight, fill = Asset)) +
        geom_bar(stat = "identity") +
        labs(title = "Quarterly Risk Parity Portfolio Weights", x = "Date", y = "Weight", fill = "Asset") +
        theme_minimal()
}