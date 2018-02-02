
# Try a seasonal plot -----------------------------------------------------


per.interv <- 7

wts.model %>%
  mutate(wk.comm = format(floor_date(date, "week"), "%d-%b"),
         wk.num = week(date)) %>%
  group_by(wk.comm) %>%
  mutate(wk.sat = max(if_else(wk.day == "Sat", wt, 0))) %>%
  ungroup() %>%
  filter(wk.num %% per.interv == 0) %>%
  ggplot(aes(x = wk.day, y = wt, linetype = fct_reorder(wk.comm, -wk.sat))) +
  geom_point() +
  geom_path(aes(group = wk.comm)) +
  geom_path(aes(y = pred, group = wk.comm), colour = "blue", alpha = 0.25) +
  labs(linetype = "Wk/Comm") +
  ggrepel::geom_label_repel(aes(label = format(date-6, "%d-%b")),
             data = wts.model %>%
               mutate(wk.comm = format(floor_date(date, "week"), "%d-%b"),
                      wk.num = week(date)) %>%
               group_by(wk.comm) %>%
               mutate(wk.sat = max(if_else(wk.day == "Sat", wt, 0))) %>%
               ungroup() %>%
               filter(wk.num %% per.interv == 0, wk.day == "Sat"),
             nudge_x = 5)



# Try a spline ------------------------------------------------------------


fit2 <- with(wts.model, smooth.spline(date, wt, cv = TRUE))
fit2

wts.spline <- predict(fit2, newdata = wts.model$date) %>%
  as_tibble() %>%
  rename(date = x, pred.spline = y) %>%
  mutate(date = as_date(date))

wts.model <- wts.model %>%
  select(-pred.spline)

wts.model.spline <- wts.model %>%
  left_join(wts.spline, by = "date")

wts.model.spline %>%
  ggplot(aes(x = date, y = wt)) +
  geom_point(aes(colour = wk.day)) +
  # geom_smooth(span = 0.1, method = "loess") +
  geom_line(aes(y = pred.spline), colour = "blue") +
  geom_smooth(span = 0.06, method = "loess", colour = "red") +
labs(title = "Daily weight with line of best fit",
     subtitle = "Varying rates of decline to Christmas then a gain of 1.5kg",
     x = "Date in 2017",
     y = "Weight (kg)") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(breaks = c(84:96)) +
  coord_cartesian(ylim = c(84, 96))

wts.pred
