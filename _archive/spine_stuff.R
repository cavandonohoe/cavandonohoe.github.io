
random_number = c(rep(.5, 30000),
                  rnorm(n=30000, mean = .42, sd = .05),
                  rnorm(n=30000, mean = .18, sd = .05),
                  rnorm(n=30000, mean = -.22, sd = .05),
                  rnorm(n=30000, mean = -.78, sd = .05),
                  rnorm(n=30000, mean = -1.5, sd = .05))

random_sample_tib %>% filter(sample %in% 1:5) %>% 
  ggplot(aes(x = position, y = random_number, color = sample)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ggtitle("Sumedh's Million Dollar Graph")

random_sample_tib = tibble(random_number) %>%
  mutate(sample = rep(seq(1, length(random_number) / 6), 6),
         # sample = (row_number() - 1) %/% 6 + 1,
         position = (row_number() - 1) %/% (length(random_number) / 6) + 1
         # position = rep(1:6, times = length(random_number) / 6)
         ) %>%
  arrange(sample, position) %>%
  group_by(sample) %>%
  mutate(sample_sum = sum(random_number)) %>% 
  ungroup() %>% 
  group_by(sample) %>%
  mutate(theta = REdaS::rad2deg(tan((random_number - lag(random_number)) /
                                      (position - lag(position))))) %>% 
  ungroup()

# average slope
random_sample_tib %>% group_by(position) %>% 
  summarise(ave_theta = mean(theta, na.rm = TRUE))

random_sample_tib %>% pull(sample_sum) %>% unique

# sumedh wants there to be an integral in here
# no need when each vertebrae is equal anyways right?
# plus across different spines of different lengths, the integral is going to be different
# for each spine

random_sample_tib %>% pull(sample_sum) %>% mean


