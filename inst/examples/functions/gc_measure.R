label <- ggcarto::gc_text("Titulo do mapa", fontsize = 18)

result <- ggcarto::gc_measure(label, viewport = list(width = 600, height = 400))
print(result)
