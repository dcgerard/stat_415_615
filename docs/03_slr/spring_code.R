############
## All spring code taken from https://ggplot2-book.org/spring1.html
############
library(tidyverse)
library(animation)

create_spring <- function(x, y, xend, yend, diameter = 1, tension = 0.75, n = 50) {
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  # Calculate direct length of segment
  length <- sqrt((x - xend)^2 + (y - yend)^2)

  # Figure out how many revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions

  # Calculate sequence of radians and x and y offset
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)

  # Create the new data
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

StatSpring <- ggproto("StatSpring", Stat,
  setup_data = function(data, params) {
    if (anyDuplicated(data$group)) {
      data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
    }
    data
  },
  compute_panel = function(data, scales,
                           diameter = 1,
                           tension = 0.75,
                           n = 50) {
    cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
    springs <- lapply(seq_len(nrow(data)), function(i) {
      spring_path <- create_spring(
        data$x[i], data$y[i],
        data$xend[i], data$yend[i],
        diameter = diameter,
        tension = tension,
        n = n
      )
      cbind(spring_path, unclass(data[i, cols_to_keep]))
    })
    do.call(rbind, springs)
  },
  required_aes = c("x", "y", "xend", "yend")
)

geom_spring <- function(mapping = NULL, data = NULL, stat = "spring",
                        position = "identity", ..., n = 50, arrow = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

stat_spring <- function(mapping = NULL, data = NULL, geom = "path",
                        position = "identity", ..., diameter = 1, tension = 0.75,
                        n = 50, na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSpring,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diameter = diameter,
      tension = tension,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

StatSpring <- ggproto("StatSpring", Stat,
  setup_data = function(data, params) {
    if (anyDuplicated(data$group)) {
      data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
    }
    data
  },
  compute_panel = function(data, scales, n = 50) {
    cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
    springs <- lapply(seq_len(nrow(data)), function(i) {
      spring_path <- create_spring(data$x[i], data$y[i], data$xend[i],
                                   data$yend[i], data$diameter[i],
                                   data$tension[i], n)
      cbind(spring_path, unclass(data[i, cols_to_keep]))
    })
    do.call(rbind, springs)
  },
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = c("diameter", "tension")
)

geom_spring <- function(mapping = NULL, data = NULL, stat = "spring",
                        position = "identity", ..., n = 50, arrow = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

some_data <- tibble(
  x = runif(5, max = 10),
  y = runif(5, max = 10),
  xend = runif(5, max = 10),
  yend = runif(5, max = 10),
  class = sample(letters[1:2], 5, replace = TRUE),
  tension = runif(5),
  diameter = runif(5, 0.5, 1.5)
)

ggplot(some_data, aes(x, y, xend = xend, yend = yend)) +
  geom_spring(aes(tension = tension, diameter = diameter))

############
## Make a gif
############
hibbs <- read_csv("https://dcgerard.github.io/stat_415_615/data/hibbs.csv")

vote_mean <- mean(hibbs$vote)
growth_mean <- mean(hibbs$growth)
lmout <- lm(vote ~ growth, data = hibbs)
slope_max <- coef(lmout)[["growth"]]

slope_seq <- seq(0, slope_max, length.out = 25)
slope_seq <- c(slope_seq, rev(slope_seq))

pl_list <- list()
for (i in seq_along(slope_seq)) {
  beta1 <- slope_seq[[i]]
  beta0 <- vote_mean - beta1 * growth_mean

  hibbs %>%
    mutate(fits = beta0 + beta1 * growth,
           resids = vote - fits,
           resids2 = resids^2,
           tension = resids2 * 0.8 + 5 ,
           r2inv = 1 / resids2) ->
    df

  ggplot() +
    geom_point(data = df, mapping = aes(x = growth, y = vote)) +
    geom_abline(intercept = beta0, slope = beta1) +
    geom_spring(data = df, mapping = aes(x = growth,
                                         xend = growth,
                                         y = vote,
                                         yend = fits,
                                         tension = tension,
                                         diameter = I(0.05))) +
    theme_bw() +
    ggtitle(paste0("Sum of Squares: ", round(sum(df$resids2)))) ->
    pl

  pl_list[[i]] <- pl
}

saveGIF(expr = {
  for (i in seq_along(pl_list)) {
    print(pl_list[[i]])
  }
}, movie.name = "ols_springs.gif", interval = 0.1)
