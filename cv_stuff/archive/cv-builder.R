`%>%` <- magrittr::`%>%`
start_time = Sys.time()
# coding in the resume

resume = readxl::read_excel(here::here("data", "resume.xlsx")
# shell.exec("T:/Cavan/Test Folder/Resume.xlsx")
resume_v1.1 = resume %>% dplyr::mutate(start.date = as.Date(paste(start, "1", sep="-"), format="%B %Y-%d")) %>%
  dplyr::mutate(section_num = dplyr::row_number())

resume_v2 = resume_v1.1 %>% tidyr::pivot_longer(tidyselect::starts_with('description'),
                                         names_to = 'description_num',
                                         values_to = 'description',
                                         values_drop_na = TRUE)

resume_v3 = resume_v2 %>% dplyr::mutate(start = ifelse(is.na(start), "", start), end=ifelse(is.na(end), "", end)) %>% 
  dplyr::mutate(time_period = dplyr::case_when(start=="" ~ end, 
                                 end == "" & section=="awards" ~ start,
                                 TRUE ~ paste(start, end, sep=" - "))) %>% 
  dplyr::mutate(description_paste = ifelse(section!="About Me", paste("-", description), description)) %>% 
  dplyr::filter(in_resume)

positions_collapsed_bullets = resume_v3 %>%
  dplyr::mutate(group_num = dplyr::cur_group_id()) %>%
  dplyr::group_by(section, title, location, institution, time_period, section_num) %>% 
  dplyr::summarise(description_section = paste(description_paste, collapse = "\n")) %>% dplyr::arrange(section_num)

positions_collapsed_bullets_v1.1 = positions_collapsed_bullets %>% dplyr::ungroup() %>% 
  dplyr::mutate(description_section = dplyr::case_when(section == "skills" ~ gsub(x=description_section, pattern= "\n", replacement = "\n\n"), 
                                         section %in% c("awards", "credentials") ~ gsub(x=description_section, pattern= "- ", replacement = ""),
                                         TRUE ~ description_section)) %>% 
  replace(is.na(.), "N/A")

# dealing with hyperlinks
positions_collapsed_bullets_v1.2 = positions_collapsed_bullets_v1.1 %>% 
  dplyr::mutate(description_section = gsub(x=description_section, pattern = "Project Presentation", 
                                    replacement = 
                                      "[Project Presentation](http://www.math.ucla.edu/~actuary/caseCompetition/Case%20Comp%202018%20-%20Team%20UCI%20Presentation.pdf)")) %>% 
  dplyr::mutate(description_section = gsub(x=description_section, pattern = "Project Memorandum", 
                                    replacement = 
                                      "[Project Memorandum](http://www.math.ucla.edu/~actuary/caseCompetition/Case%20Comp%202018%20-%20Team%20UCI%20Executive%20Summary.pdf)"))

positions_collapsed_bullets_v1.3 = positions_collapsed_bullets_v1.2 %>% 
  dplyr::mutate(description_section = dplyr::case_when(grepl(x=description_section, pattern = "\\[keep\\]") ~ "N/A",
                                         TRUE~ description_section))

positions_collapsed_bullets_final = positions_collapsed_bullets_v1.3

positions_collapsed_bullets_final %>% dplyr::filter(section=="About Me") %>% glue::glue_data("{description_section}", "\n\n")

positions_collapsed_bullets %>% dplyr::filter(section=="Education") %>% glue::glue_data("### {title}", "\n\n",
                                                                           "{location}",
                                                                           "\n\n",
                                                                           "{institution}",
                                                                           "\n\n",
                                                                           "{time_period}",
                                                                           "\n\n",
                                                                           "{description_section}", 
                                                                           .na="")

positions_collapsed_bullets %>% dplyr::filter(section=="Experience") %>% glue::glue_data("### {title}", "\n\n",
                                                                           "{location}",
                                                                           "\n\n",
                                                                           "{institution}",
                                                                           "\n\n",
                                                                           "{time_period}",
                                                                           "\n\n",
                                                                           "{description_section}",
                                                                           "\n\n",
                                                                           .na="")

positions_collapsed_bullets_final %>% dplyr::filter(section=="Skills") %>% glue::glue_data("{description_section}",
                                                                        "\n\n",
                                                                        .na="")




rmarkdown::render(input = "cv-builder.Rmd", output_file = "cv-builder.html")
shell.exec("cv-builder.html")



#####################################x
end_time = Sys.time()
end_time - start_time
