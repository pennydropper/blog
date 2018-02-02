
# Copied from https://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
# on 29 Jan 2018

set.seed(4)
wts.clean %>%
  ggplot(aes(x = date, y = wt)) +
  geom_point(aes(colour = wk.day)) +
  geom_smooth(span = 0.2, method = "loess") +
  labs(title = "Daily weight with line of best fit",
       subtitle = "Varying rates of decline to Christmas then a gain of 1.5kg",
       x = "Date in 2017",
       y = "Weight (kg)") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(breaks = c(84:96)) +
  coord_cartesian(ylim = c(84, 96))

wts.model <- wts.pred %>%
  filter(date < ymd("2017/12/25")) %>%
  left_join(wts.adj, by = c("period", "wk.day")) %>%
  mutate(wt.adj = wt - wk.day.eff)

span.seq <- seq(from = 0.1, to = 0.95, by = 0.05) #explores range of spans
k <- 10 #number of folds
set.seed(1) # replicate results
folds <- sample(x = 1:k, size = nrow(wts.model), replace = TRUE)
cv.error.mtrx <- matrix(rep(x = NA, times = k * length(span.seq)), 
                        nrow = length(span.seq), ncol = k)

for(i in 1:length(span.seq)) {
  for(j in 1:k) {
    loess.fit <- loess(wt.adj ~ date.nm, data = wts.model[folds != j, ], span = span.seq[i])
    preds <- predict(object = loess.fit, newdata = wts.model[folds == j, ])
    cv.error.mtrx[i, j] <- mean((wts.model$wt.adj[folds == j] - preds)^2, na.rm = TRUE)
    # some predictions result in `NA` because of the `x` ranges in each fold
  }
}

cv.errors <- rowMeans(cv.error.mtrx)
cv.errors

best.span.i <- which.min(cv.errors)
best.span.i
span.seq[best.span.i]


wts.model %>%
  ggplot(aes(x = date, y = wt)) +
  geom_point(aes(colour = wk.day)) +
  geom_smooth(span = 0.1, method = "loess") +
  geom_smooth(span = 0.2, method = "loess", colour = "red")
  labs(title = "Daily weight with line of best fit",
       subtitle = "Varying rates of decline to Christmas then a gain of 1.5kg",
       x = "Date in 2017",
       y = "Weight (kg)") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(breaks = c(84:96)) +
  coord_cartesian(ylim = c(84, 96))
