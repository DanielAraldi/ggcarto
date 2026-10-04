title <- ggcarto::gc_text("Area de estudo", fontsize = 18)
credits <- ggcarto::gc_text("Fonte: dados locais", fontsize = 10)

result <- ggcarto::gc_viewport(
  list(title, credits),
  padding = 24,
  flow = "column",
  gap = 16,
  background = "#EEF3CF"
)

ggcarto::gc_render(result)
