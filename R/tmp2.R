p <-td |> 
  pivot_longer(cols = 4:5, names_to = "score") |> 
  ungroup() |> 
  mutate(id = row_number()) |> 
  ggplot(aes(school_year, 
             value, color = score,
             data_id = id)) +
  geom_point_interactive(size = 5, aes(data_id = id)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_viridis_d() +
  labs(title = str_wrap(td$school_name, 60),
       fill = "Grade", x = "", y = "Score") +
  theme(axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 16)) 

girafe(ggobj = p)
