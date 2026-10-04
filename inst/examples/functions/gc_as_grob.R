label <- ggcarto::gc_text("Conteudo convertido em grob", fontsize = 18)

result <- ggcarto::gc_as_grob(label)

ggcarto::gc_render(ggcarto::gc_place(result, width = "100%", height = "100%"))
