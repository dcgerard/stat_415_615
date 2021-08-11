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
    annotate(geom = "point", x = newx, y = newy, color = "orange", size = 5) +
    ggtitle(paste0("# Samples: ", i, ", ", "Proportion Covered: ", round(totin / i, digits = 2)))

  pllist[[2 * i]] <- pl
}

saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "pred_int.gif", interval = 1)

## Confidence bands ----
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
newdf <- data.frame(growth = seq(min(hibbs$growth), max(hibbs$growth), length.out = 100))
multval <- sqrt(2 * qf(p = 0.95, df1 = 2, df2 = nrow(hibbs) - 2))
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
  pout <- predict(object = lmnew, newdata = newdf, se.fit = TRUE)
  data.frame(fit = pout$fit, se = pout$se.fit) %>%
    mutate(lower = fit - multval * se,
           upper = fit + multval * se,
           growth = newdf$growth,
           truevote = beta0 + beta1 * growth) ->
    band_df

  isin <- !any((band_df$lower > band_df$truevote) | (band_df$upper < band_df$truevote))
  totin <- totin + isin

  ggplot() +
    geom_point(data = simdf, mapping = aes(x = growth, y = vote)) +
    geom_smooth(data = simdf, mapping = aes(x = growth, y = vote), method = "lm", se = FALSE, formula = y ~ x, lty = 2) +
    theme_bw() +
    ylim(ymin, ymax) +
    geom_abline(slope = beta1, intercept = beta0) +
    xlab("Growth") +
    ylab("Vote") +
    ggtitle(paste0("# Samples: ", i, ", ", "Proportion Covered: ", round(totin / i, digits = 2))) +
    geom_line(data = band_df, mapping = aes(x = growth, y = lower), lwd = 1.5, color = ifelse(isin, "black", "red")) +
    geom_line(data = band_df, mapping = aes(x = growth, y = upper), lwd = 1.5, color = ifelse(isin, "black", "red")) ->
    pl
  pllist[[i]] <- pl
}

saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "cband.gif", interval = 1)


## Hypothesis testing strategy
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
ymin <- min(hibbs$vote) - 2 * sigma
ymax <- max(hibbs$vote) + 2 * sigma

ggplot(data = hibbs, mapping = aes(x = growth, y = vote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ylim(ymin, ymax) ->
  pl

ggsave(filename = "./hypothesis_testing/true_plot.pdf", plot = pl, height = 1, width = 1.5)

hibbs %>%
  select(growth) ->
  simdf

set.seed(3)
for (i in seq_len(5)) {
  simdf %>%
    mutate(vote = mean(hibbs$vote) + rnorm(n = n(), mean = 0, sd = sigma)) ->
    simdf

  print(summary(lm(vote ~ growth, data = simdf)))

  ggplot(data = simdf, mapping = aes(x = growth, y = vote)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    ylim(ymin, ymax) ->
    pl

  ggsave(filename = paste0("./hypothesis_testing/sim_plot_", i, ".pdf"), plot = pl, height = 1, width = 1.5)
}


tibble(x = seq(-4, 4, length.out = 500)) %>%
  mutate(y = dt(x = x, df = nrow(hibbs) - 2)) ->
  tdat
ggplot(tdat, aes(x = x, y = y)) +
  geom_line() +
  theme_classic() +
  theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
  geom_ribbon(data = filter(tdat, x > 2.5), aes(x = x, ymax = y), ymin = 0, fill = "blue") +
  geom_ribbon(data = filter(tdat, x < -2.5), aes(x = x, ymax = y), ymin = 0, fill = "blue") +
  geom_vline(xintercept = 2.5, lty = 2, col = 2) ->
  pl

ggsave(filename = "./hypothesis_testing/tdist.pdf", plot = pl, height = 1, width = 1.5)


## Confidence interval gif
library(ggthemes)
library(animation)
library(latex2exp)
set.seed(3)
ncoll <- 20
nind <- 15
meanval <- 3
sig <- 3.76
tibble(diff2 = rnorm(n = nind * ncoll, mean = meanval, sd = sig),
       sample = rep(1:ncoll, each = nind)) %>%
  group_by(sample) %>%
  nest_legacy() %>%
  mutate(ttest = map(data, ~t.test(.$diff2)),
         ttest = map(ttest, tidy)) %>%
  unnest_legacy(ttest, .drop = TRUE) %>%
  select(sample, conf.low, conf.high) %>%
  mutate(cover = factor(conf.low < meanval & conf.high > meanval, levels = c("TRUE", "FALSE"))) ->
  tdat

ymin <- min(tdat$conf.low)
ymax <- max(tdat$conf.high)

pllist <- list()
for (i in seq_len(nrow(tdat))) {
  tdat %>%
    filter(sample <= i) %>%
    ggplot(aes(x = sample, xend = sample, y = conf.low, yend = conf.high, color = cover)) +
    geom_segment(lwd = 2) +
    geom_hline(yintercept = meanval, lty = 2) +
    theme_classic() +
    theme(legend.position = "none", text = element_text(size = 30)) +
    xlab("Sample Number") +
    ylab(TeX("$\\beta_1$")) +
    scale_x_continuous(breaks = 1:ncoll, limits = c(1, ncoll)) +
    scale_color_colorblind() +
    ylim(ymin, ymax) ->
    pl
  pllist[[i]] <- pl
}


saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "ci_interp.gif", interval = 0.5, ani.width = 750)


## Construct a QQ-plot
set.seed(1)
library(tidyverse)
library(animation)
library(ggthemes)
library(patchwork)

data.frame(x = rnorm(1000)) %>%
  filter(x < 3, x > -3) ->
  edf
data.frame(x = seq(-3, 3, length.out = 500)) %>%
  mutate(y = dnorm(x)) ->
  tdf

qvec <- seq(0.05, 0.95, by = 0.05)

quantile_df <- data.frame(theoretical = qnorm(p = qvec), sample = quantile(x = edf$x, probs = qvec), quantile = qvec)

plist <- list()
for (i in seq_along(qvec)) {
    xq <- quantile(edf$x, probs = qvec[[i]])
    edf %>%
      mutate(islow = x < xq) %>%
      ggplot(aes(x = x, fill = islow)) +
      geom_histogram(bins = 25, color = "black") +
      scale_fill_manual(values = c("white", "blue")) +
      geom_vline(xintercept = xq, lty = 2, col = 2, lwd = 2) +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size = 25)) +
      xlim(-3, 3) +
      ggtitle(paste0("Sample ", qvec[[i]], "th Quantile")) ->
      pl1

    sdf <- filter(tdf, x < qnorm(qvec[[i]]))
    ggplot() +
      geom_line(data = tdf, mapping = aes(x = x, y = y)) +
      geom_ribbon(data = sdf, mapping = aes(x = x, ymin = 0, ymax = y), fill = "blue") +
      geom_vline(xintercept = qnorm(qvec[[i]]), lty = 2, col = 2, lwd = 2) +
      theme_bw() +
      theme(title = element_text(size = 25)) +
      ggtitle(paste0("Theoretical ", qvec[[i]], "th Quantile")) ->
      pl2

    quantile_df %>%
      mutate(isnow = quantile == qvec[[i]]) %>%
      ggplot(mapping = aes(x = theoretical, y = sample, color = isnow)) +
      geom_point(size = 7) +
      scale_color_colorblind() +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size = 25)) +
      ggtitle("QQ-plot") +
      xlab("Theoretical Quantile") +
      ylab("Sample Quantile") ->
      pl3

    plist[[i]] <- pl1 + pl2 + pl3
}

saveGIF(expr = {
  for (i in seq_along(plist)) {
    print(plist[[i]])
  }
}, movie.name = "qqplot.gif", interval = 0.5, ani.width = 1500)

## Bad qq

data.frame(x = rgamma(1000, shape = 2.5, rate = 2.5) - 3) %>%
  filter(x < 3, x > -3) ->
  edf
data.frame(x = seq(-3, 3, length.out = 500)) %>%
  mutate(y = dnorm(x)) ->
  tdf

qvec <- seq(0.05, 0.95, by = 0.05)

quantile_df <- data.frame(theoretical = qnorm(p = qvec), sample = quantile(x = edf$x, probs = qvec), quantile = qvec)

plist <- list()
for (i in seq_along(qvec)) {
    xq <- quantile(edf$x, probs = qvec[[i]])
    edf %>%
      mutate(islow = x < xq) %>%
      ggplot(aes(x = x, fill = islow)) +
      geom_histogram(bins = 25, color = "black") +
      scale_fill_manual(values = c("white", "blue")) +
      geom_vline(xintercept = xq, lty = 2, col = 2, lwd = 2) +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size = 25)) +
      xlim(-3, 3) +
      ggtitle(paste0("Sample ", qvec[[i]], "th Quantile")) ->
      pl1

    sdf <- filter(tdf, x < qnorm(qvec[[i]]))
    ggplot() +
      geom_line(data = tdf, mapping = aes(x = x, y = y)) +
      geom_ribbon(data = sdf, mapping = aes(x = x, ymin = 0, ymax = y), fill = "blue") +
      geom_vline(xintercept = qnorm(qvec[[i]]), lty = 2, col = 2, lwd = 2) +
      theme_bw() +
      theme(title = element_text(size = 25)) +
      ggtitle(paste0("Theoretical ", qvec[[i]], "th Quantile")) ->
      pl2

    quantile_df %>%
      mutate(isnow = quantile == qvec[[i]]) %>%
      ggplot(mapping = aes(x = theoretical, y = sample, color = isnow)) +
      geom_point(size = 7) +
      scale_color_colorblind() +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size = 25)) +
      ggtitle("QQ-plot") +
      xlab("Theoretical Quantile") +
      ylab("Sample Quantile") ->
      pl3

    plist[[i]] <- pl1 + pl2 + pl3
}

saveGIF(expr = {
  for (i in seq_along(plist)) {
    print(plist[[i]])
  }
}, movie.name = "bad_qqplot.gif", interval = 0.5, ani.width = 1500)


## Prediction interval
library(tidyverse)
library(Sleuth3)
data("case0702")
case0702 %>%
  mutate(logtime = log(Time)) ->
  case0702
lmc <- lm(pH ~ logtime, data = case0702)
dfpred <- tibble(logtime = seq(0, log(8), length.out = 200))
predict(object = lmc, newdata = dfpred, interval = "prediction") %>%
  cbind(dfpred) ->
  dfpred

ggplot() +
  geom_point(data = case0702, mapping = aes(x = logtime, y = pH)) +
  geom_smooth(data = case0702, mapping = aes(x = logtime, y = pH), method = "lm", se = FALSE) +
  geom_line(data = dfpred, mapping = aes(x = logtime, y = lwr), lty = 2) +
  geom_line(data = dfpred, mapping = aes(x = logtime, y = upr), lty = 2) +
  geom_hline(yintercept = 6) +
  xlab("log(Time)") ->
  pl
ggsave(filename = "./03_figs/inverse_pred.pdf", plot = pl, family = "Times")
