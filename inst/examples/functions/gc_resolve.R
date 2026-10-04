rectangle <- ggcarto::gc_rect(
  width = ggcarto::gc_unit(40, "mm"),
  height = ggcarto::gc_unit(20, "mm")
)

result <- ggcarto::gc_resolve(rectangle, width = 600, height = 400)
print(result$root$box)
print(result$root$children[[1]]$box)
