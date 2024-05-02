library(tidyverse)
library(cityforwardcollective)
library(wisconsink12)

these_schools <- c(
  "3619_0407", # Carmen hs south
  "3619_0451", # Carmen hs southeast
  "4613_0100", # Pulaski
  "3619_0162" # ALBA
)

get_name <- function(dti) {
  wisconsink12::schools |> 
    filter(dpi_true_id == dti) |> 
    filter(school_year == max(school_year)) |> 
    pull(school_name)
}

this_e <- enrollment |> 
  filter(dpi_true_id %in% these_schools &
           str_detect(group_by, regex("grade", ignore_case = TRUE))) |> 
  select(-group_by) |> 
  left_join(schools |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_name)) |> 
  mutate(group_by_value = factor(group_by_value,
                                 levels = c(
                                   "K3",
                                   "K4",
                                   "KG",
                                   seq(1:12)
                                 )))
  


map(these_schools, function(dti) {
  td <- this_e |> 
    filter(dpi_true_id == dti)
  
  td |> 
    ggplot(aes(school_year, student_count)) +
    geom_col(aes(fill = group_by_value)) +
    scale_fill_viridis_d() +
    labs(title = str_wrap(td$school_name, 60),
         fill = "Grade")
})
