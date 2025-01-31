# Load and preprocess data
load_data <- function(start_date) {
    J200 <- fmxdat::J200 %>%
        filter(date > start_date) %>%
        mutate(return = Return, weight = J200) %>%
        select(-Return, -J200)

    top_10 <- J200 %>%
        filter(date == last(date)) %>%
        arrange(desc(weight)) %>%
        top_n(10, weight) %>%
        pull(Tickers)

    J200 %>% filter(Tickers %in% top_10)
}