label <- ggcarto::gc_text("Titulo exportado", fontsize = 18)
directory <- tempfile("ggcarto-save-")

result <- ggcarto::gc_save(
  label,
  type = "svg",
  dir = directory,
  filename = "title",
  width = 600,
  height = 400
)

print(result)
