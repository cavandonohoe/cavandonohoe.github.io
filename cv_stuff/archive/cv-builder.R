start_time = Sys.time()


library(glue)
library(tidyverse)
library(readxl)
library(rmarkdown)
library(readxl)
# coding in the resume

resume = read_excel("data/resume.xlsx")
# shell.exec("T:/Cavan/Test Folder/Resume.xlsx")
resume_v1.1 = resume %>% mutate(start.date = as.Date(paste(start, "1", sep="-"), format="%B %Y-%d")) %>%
  mutate(section_num = row_number())

resume_v2 = resume_v1.1 %>% pivot_longer(starts_with('description'),
                                         names_to = 'description_num',
                                         values_to = 'description',
                                         values_drop_na = TRUE)

resume_v3 = resume_v2 %>% mutate(start = ifelse(is.na(start), "", start), end=ifelse(is.na(end), "", end)) %>% 
  mutate(time_period = case_when(start=="" ~ end, 
                                 end == "" & section=="awards" ~ start,
                                 TRUE ~ paste(start, end, sep=" - "))) %>% 
  mutate(description_paste = ifelse(section!="About Me", paste("-", description), description)) %>% 
  filter(in_resume)

positions_collapsed_bullets = resume_v3 %>%
  mutate(group_num = cur_group_id()) %>%
  group_by(section, title, location, institution, time_period, section_num) %>% 
  summarise(description_section = paste(description_paste, collapse = "\n")) %>% arrange(section_num)

positions_collapsed_bullets_v1.1 = positions_collapsed_bullets %>% ungroup() %>% 
  mutate(description_section = case_when(section == "skills" ~ gsub(x=description_section, pattern= "\n", replacement = "\n\n"), 
                                         section %in% c("awards", "credentials") ~ gsub(x=description_section, pattern= "- ", replacement = ""),
                                         TRUE ~ description_section)) %>% 
  replace(is.na(.), "N/A")

# dealing with hyperlinks
positions_collapsed_bullets_v1.2 = positions_collapsed_bullets_v1.1 %>% 
  mutate(description_section = gsub(x=description_section, pattern = "Project Presentation", 
                                    replacement = 
                                      "[Project Presentation](http://www.math.ucla.edu/~actuary/caseCompetition/Case%20Comp%202018%20-%20Team%20UCI%20Presentation.pdf)")) %>% 
  mutate(description_section = gsub(x=description_section, pattern = "Project Memorandum", 
                                    replacement = 
                                      "[Project Memorandum](http://www.math.ucla.edu/~actuary/caseCompetition/Case%20Comp%202018%20-%20Team%20UCI%20Executive%20Summary.pdf)"))

positions_collapsed_bullets_v1.3 = positions_collapsed_bullets_v1.2 %>% 
  mutate(description_section = case_when(grepl(x=description_section, pattern = "\\[keep\\]") ~ "N/A",
                                         TRUE~ description_section))

positions_collapsed_bullets_final = positions_collapsed_bullets_v1.3

positions_collapsed_bullets_final %>% filter(section=="About Me") %>% glue_data("{description_section}", "\n\n")

positions_collapsed_bullets %>% filter(section=="Education") %>% glue_data("### {title}", "\n\n",
                                                                           "{location}",
                                                                           "\n\n",
                                                                           "{institution}",
                                                                           "\n\n",
                                                                           "{time_period}",
                                                                           "\n\n",
                                                                           "{description_section}", 
                                                                           .na="")

positions_collapsed_bullets %>% filter(section=="Experience") %>% glue_data("### {title}", "\n\n",
                                                                           "{location}",
                                                                           "\n\n",
                                                                           "{institution}",
                                                                           "\n\n",
                                                                           "{time_period}",
                                                                           "\n\n",
                                                                           "{description_section}",
                                                                           "\n\n",
                                                                           .na="")

positions_collapsed_bullets_final %>% filter(section=="Skills") %>% glue_data("{description_section}",
                                                                        "\n\n",
                                                                        .na="")




render(input = "cv-builder.Rmd", output_file = "cv-builder.html")
shell.exec("cv-builder.html")



#####################################x
end_time = Sys.time()
end_time - start_time