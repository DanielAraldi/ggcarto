result <- lplot::l_north_rose(
  design = "eight_point",
  fill = "#197C80",
  fill_secondary = "#F4CD68",
  col = "#293438",
  angle = 0,
  fontsize = 14,
  name = "custom_rose"
)

lplot::l_render(lplot::l_place(
  result,
  x = "50%",
  y = "50%",
  anchor = "center",
  width = 160,
  height = 160
))
