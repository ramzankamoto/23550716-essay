# Generate Plot 2: Portfolio Weights Comparison
plot_portfolio_weights <- function(return_mat) {
    Sigma <- cov(return_mat)
    portfolio.parity <- riskParityPortfolio(Sigma)
    portfolio.tangency <- tangencyPortfolio(as.timeSeries(return_mat), constraints = "LongOnly")

    portfolio.weights <- rbind(portfolio.parity$w, getWeights(portfolio.tangency))
    row.names(portfolio.weights) <- c("Parity Portfolio", "Tangency Portfolio")

    portfolio.weights.df <- as.data.frame(t(portfolio.weights))
    portfolio.weights.df$Asset <- row.names(portfolio.weights.df)
    portfolio.weights.df <- reshape2::melt(portfolio.weights.df, id.vars = "Asset")
    colnames(portfolio.weights.df) <- c("Asset", "Portfolio", "Weight")

    ggplot(portfolio.weights.df, aes(x = Asset, y = Weight, fill = Portfolio)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_fill_manual(values = c("Parity Portfolio" = "red", "Tangency Portfolio" = "black")) +
        labs(title = "Portfolio Weights Comparison", x = "Assets", y = "Weights", fill = "Portfolio") +
        theme_minimal()
}