av7fun <- function(x, date) {
  ifelse(date <= max(date) - 3,
         stats::filter(x,
                       filter = rep(1/7, 7),
                       method = "convolution",
                       sides = 2),
         stats::filter(x,
                       filter = rep(1/7, 7),
                       method = "convolution",
                       sides = 1)) %>%
    as.numeric()
}


av7fun_lag <- function(x, date) {
  stats::filter(x,
                filter = rep(1/7, 7),
                method = "convolution",
                sides = 1) %>%
    as.numeric()
}

av7fun_center <- function(x, date) {
  stats::filter(x,
                filter = rep(1/7, 7),
                method = "convolution",
                sides = 2) %>%
    as.numeric()
}

