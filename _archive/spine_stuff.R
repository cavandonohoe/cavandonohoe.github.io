`%>%` <- magrittr::`%>%`
random_number <- c(rep(.5, 30000),
                  rnorm(n=30000, mean = .42, sd = .05),
                  rnorm(n=30000, mean = .18, sd = .05),
                  rnorm(n=30000, mean = -.22, sd = .05),
                  rnorm(n=30000, mean = -.78, sd = .05),
                  rnorm(n=30000, mean = -1.5, sd = .05))

random_sample_tib %>%
  dplyr::filter(sample %in% 1:5) %>%
  
  ggplot2::ggplot(ggplot2::aes(x = position, y = random_number, color = sample)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::ggtitle("Sumedh's Million Dollar Graph")

random_sample_tib <- tibble::tibble(random_number) %>%
  
  dplyr::mutate(sample = rep(seq(1, length(random_number) / 6), 6),
         # sample = (dplyr::row_number() - 1) %/% 6 + 1,
         position = (dplyr::row_number() - 1) %/% (length(random_number) / 6) + 1
         # position = rep(1:6, times = length(random_number) / 6)
         ) %>%
           
  dplyr::arrange(sample, position) %>%
    
  dplyr::group_by(sample) %>%
    
  dplyr::mutate(sample_sum = sum(random_number)) %>%
    
  dplyr::ungroup() %>%
    
  dplyr::group_by(sample) %>%
    
  dplyr::mutate(theta = REdaS::rad2deg(tan((random_number - dplyr::lag(random_number)) /
                                      (position - dplyr::lag(position))))) %>%
                                        
  dplyr::ungroup()

# average slope
random_sample_tib %>%
  dplyr::group_by(position) %>%
  
  dplyr::summarise(ave_theta = mean(theta, na.rm = TRUE))

random_sample_tib %>%
  dplyr::pull(sample_sum) %>%
  unique

# sumedh wants there to be an integral in here
# no need when each vertebrae is equal anyways right?
# plus across different spines of different lengths, the integral is going to be different
# for each spine

random_sample_tib %>%
  dplyr::pull(sample_sum) %>%
  mean


