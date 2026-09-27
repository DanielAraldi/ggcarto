designs <- c(
  "classic",
  "eight_point",
  "sixteen_point",
  "thirty_two_point",
  "stellar",
  "concentric",
  "compass",
  "nautical",
  "minimal",
  "geometric",
  "ornamental",
  "asymmetric"
)
children <- list()
for (index in seq_along(designs)) {
  column <- (index - 1) %% 4
  row <- (index - 1) %/% 4
  children <- c(
    children,
    list(
      lplot::l_north_rose(
        designs[[index]],
        name = designs[[index]],
        vp = grid::viewport(
          x = (column + 0.5) / 4,
          y = 1 - (row + 0.56) / 3,
          width = lplot::l_unit(0.23, "npc"),
          height = lplot::l_unit(0.27, "npc")
        )
      ),
      lplot::l_text(
        designs[[index]],
        x = (column + 0.5) / 4,
        y = 1 - (row + 0.08) / 3,
        fontsize = 11,
        col = "#203C43"
      )
    )
  )
}
result <- grid::grobTree(children = do.call(grid::gList, children))
lplot::l_render(lplot::l_place(result, width = "100%", height = "100%"))
