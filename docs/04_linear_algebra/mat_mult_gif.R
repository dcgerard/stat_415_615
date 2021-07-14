set.seed(1)
library(tidyverse)
library(animation)

A <- matrix(sample(x = -3:3, size = 9, replace = TRUE), nrow = 3)
B <- matrix(sample(x = -3:3, size = 6, replace = TRUE), nrow = 3)
C <- A %*% B

df <- tribble(~mat, ~i, ~j,
              "a", 3, 1,
              "a", 2, 1,
              "a", 1, 1,
              "a", 3, 2,
              "a", 2, 2,
              "a", 1, 2,
              "a", 3, 3,
              "a", 2, 3,
              "a", 1, 3,
              "b", 3, 1,
              "b", 2, 1,
              "b", 1, 1,
              "b", 3, 2,
              "b", 2, 2,
              "b", 1, 2,
              "c", 3, 1,
              "c", 2, 1,
              "c", 1, 1,
              "c", 3, 2,
              "c", 2, 2,
              "c", 1, 2)
df %>%
  mutate(val = c(c(A), c(B), c(C)),
         x = case_when(mat == "a" ~ 1,
                       mat == "b" ~ 5,
                       mat == "c" ~ 10) + j,
         y = i) ->
  df

parendf <- tibble(y = seq(-0.5, 0.5, length.out = 100))
parendf %>%
  mutate(x = y^2) ->
  parendf

parendf %>%
  mutate(x1 = x + 1.5,
         y1 = y * 3 + 2,
         x2 = -1 * x + 4.5,
         y2 = y1,
         x3 = x1 + 4,
         y3 = y1,
         x4 = x2 + 3,
         y4 = y1,
         x5 = x1 + 9,
         y5 = y1,
         x6 = x2 + 8,
         y6 = y1) ->
  parendf

ggplot() +
  geom_text(data = df, mapping = aes(x = x, y = y, label = val), size = 10) +
  theme_void() +
  geom_path(data = parendf, mapping = aes(x = x1, y = y1))  +
  geom_path(data = parendf, mapping = aes(x = x2, y = y2))  +
  geom_path(data = parendf, mapping = aes(x = x3, y = y3))  +
  geom_path(data = parendf, mapping = aes(x = x4, y = y4))  +
  geom_path(data = parendf, mapping = aes(x = x5, y = y5))  +
  geom_path(data = parendf, mapping = aes(x = x6, y = y6)) +
  annotate(geom = "text", x = 9, y = 2, label = "=", size = 10) ->
  pl


pllist <- list()
k <- 1
const <- 0.25
for (ival in 3:1) {
  for (jval in 1:2) {
    df %>%
      filter((mat == "a" & i == ival) |
               (mat == "b" & j == jval) |
               (mat == "c" & i == ival & j == jval)) ->
      segdf

    paste0(segdf$val[[1]], " * ", segdf$val[[4]], " + ",
           segdf$val[[2]], " * ", segdf$val[[5]], " + ",
           segdf$val[[3]], " * ", segdf$val[[6]], " = ",
           segdf$val[[7]]) ->
      gtitle

    tribble(~mat, ~x, ~xend, ~y, ~yend,
            "a", min(segdf$x[segdf$mat == "a"]) - const, max(segdf$x[segdf$mat == "a"]) + const, unique(segdf$y[segdf$mat == "a"]), unique(segdf$y[segdf$mat == "a"])
            ) ->
      segdf2

    tribble(~mat, ~x, ~xend, ~y, ~yend,
            "b", unique(segdf$x[segdf$mat == "b"]), unique(segdf$x[segdf$mat == "b"]), min(segdf$y[segdf$mat == "b"]) - const, max(segdf$y[segdf$mat == "b"]) + const
    ) ->
      segdf3

    pl +
      geom_segment(data = segdf2, mapping = aes(x = x, xend = xend, y = y, yend = yend), col = "blue", lwd = 12, lineend = "round", alpha = 1/4) +
      geom_segment(data = segdf3, mapping = aes(x = x, xend = xend, y = y, yend = yend), col = "red", lwd = 12, lineend = "round", alpha = 1/4) +
      annotate(geom = "point", x = segdf$x[segdf$mat == "c"], y = segdf$y[segdf$mat == "c"], size = 13, color = "purple", alpha = 1/4) +
      ggtitle(gtitle) ->
      pllist[[k]]
    k <- k + 1
  }
}


saveGIF(expr = {
  for (i in seq_along(pllist)) {
    print(pllist[[i]])
  }
}, movie.name = "matmult.gif", interval = 2, ani.height = 200)

