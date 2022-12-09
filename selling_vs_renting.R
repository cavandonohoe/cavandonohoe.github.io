
library(tidyverse)

##### ASSUMPTIONS #####
interest_rate = .1 # yearly interest rate
n_periods = 100000 # load up the vector
pmt = 5000
value_of_house = 900000

n_vector = seq(1, n_periods, by = 1)

##### CALCULATION #####
# annuity
rented = FinCal::fv(r = interest_rate / 12, n = n_vector, pmt = -1*pmt, pv = 0, type = 0)


# one time investment from getting house
selling = value_of_house * (1 + interest_rate)^(n_vector/12)

check = tibble(n = n_vector, rented_value = rented, selling_value = selling)

# let's do a fun thing where we check where the intersection is
check2 = check %>% mutate(rented_greater_than_selling = rented_value > selling_value)

intersection = check2 %>% filter(rented_greater_than_selling) %>% pull(n) %>% min

check3 = check2 %>% filter(n < intersection * 2) %>% select(-rented_greater_than_selling)

check4 = check3 %>% gather(key = rented_or_selling, value = amount, -n) %>%
  arrange(n)

check4 %>% ggplot(aes(x = n, y = amount, colour = rented_or_selling)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_dollar(), breaks = scales::pretty_breaks(n = 12)) +
  xlab("n (months)") +
  theme(axis.title.y = element_blank())

intersection
intersection / 12

