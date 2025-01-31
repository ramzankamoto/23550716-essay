# Generate Table 1: Summary statistics
generate_table1 <- function(return_mat) {
    sum_stats <- PerformanceAnalytics::table.Stats(return_mat)
    table1 <- xtable(sum_stats, caption = "Table 1: Descriptive statistics")
    print.xtable(table1, floating = TRUE, table.placement = 'H', comment = FALSE, caption.placement = 'bottom')
}