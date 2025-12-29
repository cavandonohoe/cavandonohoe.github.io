`%>%` <- magrittr::`%>%`
##### ASSUMPTIONS #####
interest_rate <- .1 # yearly interest rate
n_periods <- 100000 # load up the vector
pmt <- 5000
value_of_house <- 900000

n_vector <- seq(1, n_periods, by = 1)

##### CALCULATION #####
# annuity
rented <- FinCal::fv(r = interest_rate / 12, n = n_vector, pmt = -1*pmt, pv = 0, type = 0)


# one time investment from getting house
selling <- value_of_house * (1 + interest_rate)^(n_vector/12)

check <- tibble::tibble(n = n_vector, rented_value = rented, selling_value = selling)

# let's do a fun thing where we check where the intersection is
check2 <- check %>%
  dplyr::mutate(rented_greater_than_selling = rented_value > selling_value)

intersection <- check2 %>%
  dplyr::filter(rented_greater_than_selling) %>%
  dplyr::pull(n) %>%
  min

check3 <- check2 %>%
  dplyr::filter(n < intersection * 2) %>%
  dplyr::select(-rented_greater_than_selling)

check4 <- check3 %>%
  tidyr::gather(key = rented_or_selling, value = amount, -n) %>%
  
  dplyr::arrange(n)

check4 %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = amount, colour = rented_or_selling)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_continuous(labels = scales::label_dollar(),
    breaks = scales::pretty_breaks(n = 12)) +
  ggplot2::xlab("n(months)") +
  ggplot2::theme(axis.title.y = ggplot2::element_blank())

intersection
intersection / 12
