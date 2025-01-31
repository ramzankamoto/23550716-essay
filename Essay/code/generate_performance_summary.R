# Generate Plot 4: Performance summary
generate_performance_summary <- function(return_mat) {
    return_mat_xts <- return_mat["2010-01-04/2024-06-01"]
    parity.weights <- riskParityPortfolio(cov(return_mat_xts))$w
    parity.returns <- Return.portfolio(return_mat_xts, weights = parity.weights, verbose = TRUE)
    p.returns <- parity.returns$returns
    names(p.returns) <- "Top10 Parity Index"

    charts.PerformanceSummary(p.returns)
}