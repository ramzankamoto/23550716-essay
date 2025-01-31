# Function to impute missing returns
impute_missing_returns <- function(return_mat, method = "NONE") {
    if (!"date" %in% colnames(return_mat)) stop("No 'date' column provided in return_mat.")

    if (method %in% c("NONE", "None", "none")) {
        if (any(is.na(return_mat))) warning("There are missing values in the return matrix.")
        return(return_mat)
    }

    if (method == "Drawn_Distribution_Collective") {
        NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))
        return_mat <- bind_cols(
            return_mat %>% gather(Stocks, Returns, -date),
            return_mat %>% gather(Stocks, Returns, -date) %>%
                mutate(Dens = list(density(Returns, na.rm = T))) %>%
                summarise(Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob = .$Dens[[1]]$y))) %>%
                unnest(Random_Draws)
        ) %>%
            mutate(Returns = coalesce(Returns, Random_Draws)) %>%
            select(-Random_Draws) %>%
            spread(Stocks, Returns)
        return(return_mat)
    }
    stop("Invalid impute_returns_method.")
}