examples_root <- system.file("examples", package = "ggcarto")
sys.source(file.path(examples_root, "data", "maps.R"), envir = environment())
sys.source(file.path(examples_root, "utils.R"), envir = environment())
rm(examples_root)

map_template_scene <- function(
  field = "BIR74",
  title = "Nascimentos | 1974",
  accent = "#197C80",
  background = "#F0F6F5"
) {
  counties <- map_counties()
  base <- map_simple_scene(counties, field, title)
  label <- function(title, subtitle) {
    map_label_template(title, subtitle, accent, background)
  }
  ggcarto::gc_viewport(
    list(
      ggcarto::gc_place(
        label(title, "Carolina do Norte | Condados"),
        left = 0,
        right = 0,
        top = 0,
        height = 72,
        id = "header"
      ),
      ggcarto::gc_place(base$children[[2]], top = 88, bottom = 144),
      ggcarto::gc_place(base$children[[3]], bottom = 84),
      ggcarto::gc_place(
        label(
          format(
            sum(counties[[field]]),
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE,
            trim = TRUE
          ),
          "Nascimentos no periodo"
        ),
        left = 0,
        bottom = 0,
        width = "48%",
        height = 64,
        id = "births"
      ),
      ggcarto::gc_place(
        label(as.character(nrow(counties)), "Condados mapeados"),
        right = 0,
        bottom = 0,
        width = "48%",
        height = 64,
        id = "counties"
      )
    ),
    width = 900,
    height = 600,
    padding = 16,
    background = "white",
    metadata = base$metadata
  )
}
