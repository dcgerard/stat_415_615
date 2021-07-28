####################
## Gifs for 03_inference.Rmd
####################

## Confidence interval for mean

library(tidyverse)
library(animation)
library(broom)

hibbs <- read_csv("https://dcgerard.github.io/stat_415_615/data/hibbs.csv")
lmout <- lm(vote ~ growth, data = hibbs)
tout <- tidy(lmout)
gout <- glance(lmout)
beta0 <- tout$estimate[[1]]
beta1 <- tout$estimate[[2]]
sigma <- gout$sigma[[1]]
ymin <- min(hibbs$vote) - 3 * sigma
ymax <- max(hibbs$vote) + 3 * sigma
newx <- -0.2
truey <- beta0 + beta1 * newx
newdf <- data.frame(growth = newx)
hibbs %>%
  select(growth) ->
  simdf

set.seed(2)
pllist <- list()
totin <- 0
for (i in seq_len(100)) {
  simdf %>%
    mutate(vote = beta0 + beta1 * growth + rnorm(n = n(), mean = 0, sd = sigma)) ->
    simdf
  lmnew <- lm(vote ~ growth, data = simdf)
  pout <- predict(object = lmnew, newdata = newdf, interval = "confidence")
  segdf <- tibble(x = newx, ymin = pout[[2]], ymax = pout[[3]])
  isin <- truey < pout[[3]] & truey > pout[[2]]
  totin <- totin + isin
  ggplot() +
    geom_point(data = simdf, mapping = aes(x = growth, y = vote)) +
    geom_smooth(data = simdf, mapping = aes(x = growth, y = vote), method = "lm", se = FALSE, formula = y ~ x, lty = 2) +
    theme_bw() +
    ylim(ymin, ymax) +
    geom_abline(slope = beta1, intercept = beta0) +
    geom_errorbar(data = segdf, mapping = aes(x = x, ymin = ymin, ymax = ymax), width = 0.1, color = ifelse(isin, "black", "red"), lwd = 1.5) +
    xlab("Growth") +
    ylab("Vote") +
    ggtitle(paste0("# Samples: ", i, ", ", "Proportion Covered: ", round(totin / i, digits = 2)))->
    pl
  pllist[[i]] <- pl
}

saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "ci_mean.gif", interval = 1)

## Prediction interval
rm(list = ls())

library(tidyverse)
library(animation)
library(broom)

hibbs <- read_csv("https://dcgerard.github.io/stat_415_615/data/hibbs.csv")
lmout <- lm(vote ~ growth, data = hibbs)
tout <- tidy(lmout)
gout <- glance(lmout)
beta0 <- tout$estimate[[1]]
beta1 <- tout$estimate[[2]]
sigma <- gout$sigma[[1]]
ymin <- min(hibbs$vote) - 3 * sigma
ymax <- max(hibbs$vote) + 3 * sigma
newx <- 1
truey <- beta0 + beta1 * newx
newdf <- data.frame(growth = newx)
hibbs %>%
  select(growth) ->
  simdf

set.seed(3)
pllist <- list()
totin <- 0
for (i in seq_len(50)) {
  simdf %>%
    mutate(vote = beta0 + beta1 * growth + rnorm(n = n(), mean = 0, sd = sigma)) ->
    simdf
  lmnew <- lm(vote ~ growth, data = simdf)
  pout <- predict(object = lmnew, newdata = newdf, interval = "prediction")
  segdf <- tibble(x = newx, ymin = pout[[2]], ymax = pout[[3]])

  newy <- truey + rnorm(n = 1, mean = 0, sd = sigma)


  ggplot() +
    geom_point(data = simdf, mapping = aes(x = growth, y = vote)) +
    geom_smooth(data = simdf, mapping = aes(x = growth, y = vote), method = "lm", se = FALSE, formula = y ~ x, lty = 2) +
    theme_bw() +
    ylim(ymin, ymax) +
    geom_abline(slope = beta1, intercept = beta0) +
    xlab("Growth") +
    ylab("Vote") +
    ggtitle(paste0("# Samples: ", i)) +
    geom_errorbar(data = segdf, mapping = aes(x = x, ymin = ymin, ymax = ymax), width = 0.1, color = "black", lwd = 1.5) ->
    pl

  pllist[[2 * i - 1]] <- pl

  isin <- newy < pout[[3]] & newy > pout[[2]]
  totin <- totin + isin

  pl <- pl +
    geom_errorbar(data = segdf, mapping = aes(x = x, ymin = ymin, ymax = ymax), width = 0.1, color = ifelse(isin, "black", "red"), lwd = 1.5) +
    annotate(geom = "point", x = newx, y = newy, color = "orange", size = 3) +
    ggtitle(paste0("# Samples: ", i, ", ", "Proportion Covered: ", round(totin / i, digits = 2)))

  pllist[[2 * i]] <- pl
}

saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "pred_int.gif", interval = 1)

