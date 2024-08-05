# General ggplot theme
theme_general <- 
  theme_few(base_size = 15) +
  theme(      
    legend.position = 'none',
    axis.text.x = element_text(color = 'black'), 
    axis.text.y = element_text(color = 'black'), 
    axis.title.x = element_text(face='bold'),
    axis.title.y = element_text(face='bold'),
    axis.line = element_line(linewidth = 0),
    panel.border = element_rect(linewidth = 0.4, color = 'black'), 
    axis.ticks.length.x = unit(0.5, 'mm'),
    axis.ticks.length.y = unit(0.5, 'mm'))