first <- ggcarto::gc_rect(
  width = ggcarto::gc_unit(55, "mm"),
  height = ggcarto::gc_unit(35, "mm"),
  fill = "#95CEC0",
  col = NA
)
second <- ggcarto::gc_rect(
  width = ggcarto::gc_unit(55, "mm"),
  height = ggcarto::gc_unit(35, "mm"),
  fill = "#194E70",
  col = NA
)

result <- ggcarto::gc_join(
  list(first, second),
  flow = "row",
  gap = 16,
  padding = 16,
  background = "white"
)

ggcarto::gc_render(result)
