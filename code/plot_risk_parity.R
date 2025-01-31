# Generate Plot 1: Risk parity equivalence to equal risk contribution
plot_risk_parity <- function(return_mat) {
    Sigma <- cov(return_mat)
    rpp <- riskParityPortfolio(Sigma)
    barplotPortfolioRisk(rpp$w, Sigma, col = "#FF7F7F")
}