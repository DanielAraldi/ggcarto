result <- ggcarto::gc_register_element(
  "map_note",
  can_extract = is.character,
  extract = function(source, ...) {
    ggcarto::gc_text(source, fontsize = 12, col = "#194E70")
  }
)

print(names(result))
