# lplot

A responsive scene graph for R graphics. Extract ggplot2 elements, place them
with logical constraints, and render nested compositions using native `grid`.
There is no patchwork or browser dependency.
Use `l_save()` to export scenes or plots to PNG, JPG/JPEG, SVG and WebP.

## Install and Try

From the package directory:

```sh
Rscript -e 'install.packages(c("ggplot2", "gtable", "testthat", "vdiffr", "svglite"))'
R CMD INSTALL .
```

```r
library(lplot)
library(ggplot2)

plot <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() + labs(title = "Vehicle efficiency", colour = "Cylinders") +
  theme_minimal()

title <- l_get_element(plot, "title", style = list(
  color = "#164E45", font_size = "clamp(10pt, 2.5vmin, 20pt)"
))
legend <- l_get_element(plot, "legend", style = list(
  background = "white", padding = "6px", border = "#DDDDDD"
))

scene <- l_viewport(list(
  l_place(l_without(plot, c("title", "legend")),
          left = 0, right = 0, top = 40, bottom = 0),
  l_place(title, x = "50%", top = 4, anchor = "top-center", z_index = 20),
  l_place(legend, right = 12, top = 60, z_index = 10)
), width = 800, height = 600, padding = 12, background = "white")

l_render(scene)
grid::grid.draw(scene)
l_resolve(scene, width = 1024, height = 768)$root$children[[2]]$box
```

An executable terrain composition is in [inst/examples/terrain.R](inst/examples/terrain.R):

```r
source(system.file("examples", "terrain.R", package = "lplot"))
scene <- terrain_scene()
l_save(scene, type = "png", dir = "exports", filename = "terrain",
  width = 1200, height = 700, dpi = 144)
```

Install the optional `ragg` dependency before using this PNG export.

## Customizable North Arrows

`l_north_arrow()` builds a native grid grob with twelve designs. Customize
colours, primary/secondary fills, line width, label position and typography,
rotation, or replace the symbol body with your own grid grobs.

```r
north <- l_north_arrow(
  design = "classic", fill = "#197C80", fill_secondary = "white",
  col = "#203C43", lwd = 1.2, fontsize = 11, angle = 12
)
l_render(l_place(north, right = 16, top = 16, width = 40, height = 68))
```

| Design         | Symbol                                             |
| -------------- | -------------------------------------------------- |
| `classic`      | Classic cartographic split diamond (default)       |
| `ornate`       | Traditional ornamented compass                     |
| `minimal`      | Modern minimalist shaft and configurable arrowhead |
| `fleur_de_lis` | Fleur-de-lis with curved side petals               |
| `bold`         | Bold geometric arrow                               |
| `fine_line`    | Technical open-tip arrow with reference ticks      |
| `circle`       | Split arrow with a circle                          |
| `double`       | Two opposing tips                                  |
| `triangle`     | Split triangle                                     |
| `cross`        | Four-point cross                                   |
| `pennant`      | Right triangular flag on a mast, without arrowhead |
| `art_deco`     | Stepped art deco arrow                             |

`angle = 0` points up; positive angles rotate counterclockwise. This is a
graphical constructor, not a CRS calculator. For true north, supply an angle
computed by `l_north_angle()` for the map projection and reference location.
The `map_north_arrow()` example helper uses this calculation and draws with the
`minimal` preset. No `sf` dependency is required by `l_north_arrow()` itself.

## Geographic North Orientation

`l_north_angle(map, at = NULL, step = 0.0001)` calculates local true north and
returns one angle in degrees for either `l_north_arrow()` or `l_north_rose()`.
Zero points up; positive values rotate counterclockwise. It requires optional
`sf`, not a new dependency. It does not calculate magnetic declination.

`map` accepts a coord_sf plot, an `l_frame()`/`l_inset()`, a bounding box with
CRS, or an explicit CRS. Without `at`, a map or bbox uses its displayed extent
center; a CRS alone requires a location. Numeric `at` is always longitude and
latitude in WGS84 degrees. A single sf POINT can instead supply its own CRS.

```r
angle <- l_north_angle(3413, at = c(0, 75))
arrow <- l_north_arrow("minimal", angle = angle)
rose <- l_north_rose("eight_point", angle = angle)
l_render(l_viewport(list(
  l_place(arrow, left = 20, top = 20, width = 80, height = 100),
  l_place(rose, left = 140, top = 20, width = 120, height = 120)
)))
```

This example returns 45 degrees on the northern polar stereographic map.
The angle is local, not valid everywhere on a large map, and must be recomputed
after changing the projection or reference. Unknown CRSs, poles and invalid
projection locations are rejected.

## Cartographic Frames, Scales and Insets

`l_frame()` extracts a single `coord_sf()` panel, retains its displayed extent
and CRS, and fits it without stretching. Titles, legends and external axes
remain separate elements. Overlays use the fitted map as their layout context,
not the surrounding page. Outer padding and borders do not alter map scale.

`l_scale_bar()` belongs directly in a frame's `overlays`. It recalculates its
width on every draw, with explicit or automatic distance, `m`/`km`/`ft`/`mi`
labels, alternating bars or ticks, and configurable subdivisions. It measures
**projected distance**, not geodesic ground distance. Metres, kilometres,
international feet and US survey feet are supported as projection units;
angular and unsupported units are rejected. Arbitrary width overrides and
collision shrinking are rejected so the distance label cannot become misleading.

`l_inset()` creates an independent secondary frame. In `mode = "locator"`, it
highlights the main extent inside the secondary map; in `mode = "detail"`, it
highlights the secondary extent on the main frame when added to its overlays.
Footprints are densified before CRS transformation. The explicit `reference`
must match the main frame, and each map owns its own scale.

These functions require optional **sf** for cartographic composition. The
generic layout engine does not require any geographic package.

```r
counties <- sf::st_transform(sf::st_read(
  system.file("shape/nc.shp", package = "sf"), quiet = TRUE
), 32119)
overview <- ggplot2::ggplot(counties) +
  ggplot2::geom_sf(fill = "#95CEC0", colour = "white", linewidth = 0.3) +
  ggplot2::coord_sf(expand = FALSE, datum = NA) + ggplot2::theme_void()
main <- overview
main$coordinates <- ggplot2::coord_sf(
  crs = 32119, xlim = c(580000, 820000), ylim = c(130000, 290000),
  expand = FALSE, datum = NA
)
locator <- l_inset(overview, reference = main, background = "white")
scene <- l_frame(main, overlays = list(
  l_place(locator, right = 10, top = 10),
  l_scale_bar(50, "km", segments = 2, left = 12, bottom = 8)
), padding = 16, background = "white")
l_render(scene)
```

## Independent Function Examples

There is one standalone script for each exported function in
[inst/examples/functions/](inst/examples/functions/). Each focuses on its named
function, using `l_text()` and `l_rect()` to prepare content, `l_unit()` for native
grid dimensions and `l_render()` to draw where applicable. Every script supplies
its own inputs without shared utilities, other example scripts or downloads.
The `l_frame`, `l_scale_bar` and `l_inset` examples require optional `sf` and use
its bundled county data. The `l_north_angle` example also requires sf and maps
Wake County, North Carolina, from those data. Scale, inset and north-angle examples use `l_frame()` as
their map context; the north-angle example also draws both north constructors.
All other function examples run without sf.

From the project root, load the development package and choose a script:

```r
pkgload::load_all(".")
source("inst/examples/functions/l_get_element.R")
```

With lplot installed, the equivalent is:

```r
source(system.file("examples", "functions", "l_get_element.R", package = "lplot"))
```

Replace the filename to try another function. Unlike the map scene constructors,
sourcing these scripts runs the example immediately. Graphics appear on the
current device; inspection examples print their results to the console. Each
script stores its main return value in `result`, even when a subsequent
`l_render()` call draws it. Run the scripts separately, in any order.

| Function               | Script                                                                                       | Demonstration                                                       |
| ---------------------- | -------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| `l_unit()`             | [inst/examples/functions/l_unit.R](inst/examples/functions/l_unit.R)                         | Declare and print a native grid unit in millimetres.                |
| `l_length()`           | [inst/examples/functions/l_length.R](inst/examples/functions/l_length.R)                     | Declare and print a percentage length.                              |
| `l_clamp()`            | [inst/examples/functions/l_clamp.R](inst/examples/functions/l_clamp.R)                       | Declare minimum, preferred and maximum lengths.                     |
| `l_text()`             | [inst/examples/functions/l_text.R](inst/examples/functions/l_text.R)                         | Draw text with explicit typography.                                 |
| `l_rect()`             | [inst/examples/functions/l_rect.R](inst/examples/functions/l_rect.R)                         | Draw a filled rectangle with an outline.                            |
| `l_template()`         | [inst/examples/functions/l_template.R](inst/examples/functions/l_template.R)                 | Combine a rectangle and text into one reusable object.              |
| `l_north_rose()`       | [inst/examples/functions/l_north_rose.R](inst/examples/functions/l_north_rose.R)             | Draw one customized compass rose, centered in the viewport.         |
| `l_north_arrow()`      | [inst/examples/functions/l_north_arrow.R](inst/examples/functions/l_north_arrow.R)           | Draw a customized, rotated fleur-de-lis north arrow.                |
| `l_north_angle()`      | [inst/examples/functions/l_north_angle.R](inst/examples/functions/l_north_angle.R)           | Calculate true north and apply the same angle to an arrow and rose. |
| `l_frame()`            | [inst/examples/functions/l_frame.R](inst/examples/functions/l_frame.R)                       | Fit a geographic panel while preserving its CRS and proportions.    |
| `l_scale_bar()`        | [inst/examples/functions/l_scale_bar.R](inst/examples/functions/l_scale_bar.R)               | Draw a map-bound 200 km scale with subdivisions.                    |
| `l_inset()`            | [inst/examples/functions/l_inset.R](inst/examples/functions/l_inset.R)                       | Add a locator highlighting the main map's displayed extent.         |
| `l_style()`            | [inst/examples/functions/l_style.R](inst/examples/functions/l_style.R)                       | Style a copy of a text grob.                                        |
| `l_registry()`         | [inst/examples/functions/l_registry.R](inst/examples/functions/l_registry.R)                 | List the built-in extraction adapters.                              |
| `l_register_element()` | [inst/examples/functions/l_register_element.R](inst/examples/functions/l_register_element.R) | Register a character-to-text adapter in an independent registry.    |
| `l_get_element()`      | [inst/examples/functions/l_get_element.R](inst/examples/functions/l_get_element.R)           | Extract and draw only a ggplot legend.                              |
| `l_without()`          | [inst/examples/functions/l_without.R](inst/examples/functions/l_without.R)                   | Draw a copy of a ggplot without its title and legend.               |
| `l_place()`            | [inst/examples/functions/l_place.R](inst/examples/functions/l_place.R)                       | Position and size a rectangle using percentages.                    |
| `l_viewport()`         | [inst/examples/functions/l_viewport.R](inst/examples/functions/l_viewport.R)                 | Arrange text grobs in a padded local context.                       |
| `l_join()`             | [inst/examples/functions/l_join.R](inst/examples/functions/l_join.R)                         | Join two rectangles with horizontal spacing.                        |
| `l_as_grob()`          | [inst/examples/functions/l_as_grob.R](inst/examples/functions/l_as_grob.R)                   | Create and draw a deferred grid-compatible wrapper.                 |
| `l_render()`           | [inst/examples/functions/l_render.R](inst/examples/functions/l_render.R)                     | Draw a text grob and retain the returned layout.                    |
| `l_measure()`          | [inst/examples/functions/l_measure.R](inst/examples/functions/l_measure.R)                   | Print the constrained and intrinsic dimensions of text.             |
| `l_resolve()`          | [inst/examples/functions/l_resolve.R](inst/examples/functions/l_resolve.R)                   | Inspect the root and child boxes in logical pixels.                 |
| `l_save()`             | [inst/examples/functions/l_save.R](inst/examples/functions/l_save.R)                         | Export a text grob and print the output path.                       |

The `l_as_grob()` and `l_template()` examples use `l_place()` to give graphical
trees an explicit rendering area rather than relying on automatic intrinsic
measurement. The `l_rect()` example also uses an explicit area to preserve its
relative geometry. `l_unit()` creates native grid units, including physical
dimensions; direct `grid::unit()` calls remain supported.

The export example requires the optional **svglite** package
(`install.packages("svglite")`). It creates a new temporary directory on every
run, writes an SVG there and prints its absolute path. Change `dir` to retain
the file outside R's temporary directory. Existing files are not overwritten.

To test all function examples in isolated environments from the project root:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "function-examples", reporter = "summary", stop_on_failure = TRUE)'
```

The four registered S3 methods support `print()`, `grid::grid.draw()` and
`grid::makeContent()`; they are invoked through those generics rather than
treated as additional exported lplot functions.

## Four Progressive Map Examples

[inst/examples/](inst/examples/) also provides four examples that progress from
a minimal map to a composed cartographic report, each in its own file. All use the
North Carolina counties bundled with the optional `sf` package; no external data
download is required. `ggplot2` draws the geographic layers and `lplot` composes
the elements. The third example additionally uses `sf` for reprojection and area
calculations.

From the project root, source each example file separately and run its rendering
command:

```r
pkgload::load_all(".")

source("inst/examples/simple.R")
lplot::l_render(map_simple_scene())

source("inst/examples/template.R")
lplot::l_render(map_template_scene())

source("inst/examples/sf.R")
lplot::l_render(map_sf_scene())

source("inst/examples/complex.R")
lplot::l_render(map_complex_scene())
```

Install `sf` first with `install.packages("sf")` if needed. For an installed
`lplot`, use `library(lplot)` and
`source(system.file("examples", "simple.R", package = "lplot"))` (and so on for
the other files) instead of the first two lines above. Sourcing a script only
defines functions; it does not draw. The simple example reads the bundled data
directly with `sf` and does not load utility scripts. The other examples load
shared data (`inst/examples/data/maps.R`) and rendering helpers
(`inst/examples/utils.R`). Source `simple.R` before `template.R` or `sf.R`,
which reuse `map_simple_scene()`.

1. **Simple map:** `map_simple_scene()` keeps the complete map in ggplot2, which
   controls its coordinates and proportions. Only the title and horizontal
   legend are extracted and positioned with lplot, with colour limits and breaks
   derived from the selected data. Change `field` or supply
   modified county data when constructing a new scene; no legend labels need to
   be maintained by hand. Its position is resolved again at each device size.
2. **Custom templates:** `map_template_scene()` reuses `map_label_template()` for
   the heading and two summary labels. This helper combines `l_template()`,
   `l_rect()` and `l_text()`. Content, accent colour and background change without
   duplicating graphical construction or map placement. Templates receive explicit
   outer dimensions, while their children use native grid coordinates.
3. **Spatial processing:** `map_sf_scene()` transforms counties to EPSG:32119,
   calculates projected area in square kilometres with `st_area()`, and maps
   1974 births per 100 square kilometres. This is an area-normalized count, not a
   population birth rate. An `sf` county object in another CRS can be supplied.
4. **Complex map:** `map_complex_scene()` combines a regional map, locator inset,
   georeferenced north arrow, 50 km projected scale, legend, reusable heading,
   summary labels, top-five bar chart and source note in nested viewports.
   Indicators and ranking include the **whole count** of every county intersecting
   the displayed extent; counts are not estimated for clipped polygon fragments.

Customize the data and template without changing the layout:

```r
lplot::l_render(map_simple_scene(field = "BIR79", title = "Nascimentos | 1979"))

scene <- map_template_scene(
  field = "BIR79", title = "Nascimentos | 1979",
  accent = "#A63748", background = "#FAF1F3"
)
lplot::l_render(scene)

counties <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
lplot::l_render(map_sf_scene(counties))
```

Use at least 600 x 400 logical pixels for the first three examples and 1000 x 700
for the complex composition. Export at an explicit size when the Plots pane is
smaller (PNG requires the optional `ragg` package):

```r
lplot::l_save(map_complex_scene(), type = "png", dir = "exports",
          filename = "complex-map", width = 1200, height = 800)
```

## Three Cartographic Examples

[inst/examples/](inst/examples/) also contains three independently callable
scenes, each in its own file (`scale.R`, `inset.R`, `join.R`). They use the North
Carolina county polygons bundled with the optional `sf` package, so no data
download, API key or network connection is needed after installation. Both map
and terrain examples use the graphical constructors described below.

In the Positron R console, with the project root as the working directory:

```r
install.packages("sf") # Only if not already installed
pkgload::load_all(".")

source("inst/examples/scale.R")
lplot::l_render(map_scale_scene())

source("inst/examples/inset.R")
lplot::l_render(map_inset_scene())

source("inst/examples/join.R")
lplot::l_render(map_join_scene())
```

Run the three rendering commands individually to inspect each plot:

1. `map_scale_scene()`: county map with a 200 km scale bar, a horizontal legend,
   title and subtitle, all positioned as separate lplot elements.
2. `map_inset_scene()`: enlarged central/eastern region with a 50 km scale bar,
   north arrow, title, legend and upper-left locator inset. The red rectangle in
   the inset is the exact projected extent displayed by the main map.
3. `map_join_scene()`: two independent map viewports joined side by side with
   `l_join()`. They display the `BIR74` and `BIR79` birth-count attributes from
   `sf::nc`, using identical extents and colour limits for comparison.

The four cartographic scenes (`scale`, `inset`, `join` and `complex`) use the
public map constructors through shared composition helpers. `map_frame()` calls
`l_frame()` and adds `l_scale_bar()` with endpoint labels and an intermediate
tick, keeping scales legible on compact joined maps. Extent and proportions come
from the actual plot; no separate frame extent or manual scale-width calculation
is maintained. `map_locator()` uses `l_inset(mode = "locator")` with the main plot
as its explicit reference, so the highlighted footprint stays geographically linked.

`map_sheet()` still places separately extracted titles, subtitles and legends.
The shared north arrow helper calculates orientation with `l_north_angle()` and uses
`l_north_arrow("minimal", angle = angle)` for its graphical composition.
These shared helpers live in `inst/examples/utils.R`; sourcing an example file only
defines functions and does not open a graphics device or automatically draw all
examples. The minimal `simple` example and the generic terrain composition remain
unchanged; `template` and `sf` continue building on the minimal example.

Coordinates use EPSG:32119 (NAD83 / North Carolina), in metres. Scale-bar width is
the requested projected distance divided by the displayed extent width, not a
fixed decorative width; it remains aligned when resized. These are projected grid
distances, not geodesic measurements. The north arrow follows geographic north
evaluated at the centre of the main map, accounting for the projection's local
orientation. The inset frame does not change the panel's coordinate proportions.

Use at least approximately 800 x 600 logical pixels for the two-map example, or
export to a device with an explicit size to avoid a narrow Plots panel:

```r
scene <- map_join_scene()
lplot::l_save(scene, type = "png", dir = "exports", filename = "joined-maps",
              width = 1200, height = 700)
```

For an installed package, replace the two loading commands with:

```r
library(lplot)
source(system.file("examples", "join.R", package = "lplot"))
```

## Templates and Graphical Primitives

Create reusable graphical content without wrapping every style in `grid::gpar()`:

```r
badge <- l_template(
  l_rect(fill = "white", col = "#203C43", lwd = 1),
  l_text("N", fontsize = 12, fontface = "bold"),
  gp = list(col = "#203C43")
)
scene <- l_viewport(list(
  l_place(badge, right = 12, top = 12, width = 40, height = 50)
), width = 240, height = 120)
l_render(scene)
```

`l_template()` returns a native `gTree`, accepting grobs in `...` or in
`children = list(...)`. Templates can be nested and mixed with native grid grobs.
`l_rect()` and `l_text()` return native rectangle and text grobs, preserving grid
arguments such as `x`, `y`, `just`, `hjust`, `vjust`, `default.units`, `name` and
`vp`, plus rectangle dimensions or text rotation and overlap control.

All three accept `gp` as a `gpar` or named list. Templates share it with their
children using grid's inheritance rules. Rectangles and text also accept named
graphical parameters directly, overriding `gp`: `col`, `fill`, `alpha`, `lwd`,
`lty`, `fontsize`, `fontfamily`, `fontface`, `lineheight` and other `gpar` settings.
Use `vp = grid::viewport(...)` for group rotation, clipping and local coordinates.

Inside these grobs, numbers default to grid's `npc` units, with (0, 0) at the
bottom-left; `l_unit()` and `grid::unit()` objects are also supported. These are not lplot's top-left
pixel coordinates or percentage strings. Use `l_place()` for outer placement
and explicit template dimensions, or `l_get_element()` for semantic metadata
and responsive styles.

## Compass Roses

`l_north_rose()` creates a native grid `gTree` with one of twelve designs:

| `design`           | Appearance                                                           |
| ------------------ | -------------------------------------------------------------------- |
| `classic`          | Classical four-point rose: north, east, south and west.              |
| `eight_point`      | Eight split tips, including the intercardinal directions.            |
| `sixteen_point`    | Sixteen tips with three levels of directional emphasis.              |
| `thirty_two_point` | Traditional 32-point rose with four levels of detail.                |
| `stellar`          | Long, narrow star tips; `points = 8`, `16` (default) or `32`.        |
| `concentric`       | Three concentric rings, divisions and eight directional tips.        |
| `compass`          | Compass dial with ticks, a two-ended needle and a central pivot.     |
| `nautical`         | Portolan-inspired rhumb lines, rings and sixteen tips.               |
| `minimal`          | Fine cardinal rays, a small north tip and a center circle.           |
| `geometric`        | Detached diamonds and a central geometric motif.                     |
| `ornamental`       | Decorative rings, beads, petals, star and central jewel.             |
| `asymmetric`       | Broad cardinal tips, smaller diagonal diamonds and emphasized north. |

```r
rose <- l_north_rose("eight_point",
  fill = "#197C80", fill_secondary = "white", angle = 15,
  labels = c("N", "NE", "L", "SE", "S", "SO", "O", "NO")
)
element <- l_get_element(rose, "north_rose", width = 120, height = 120)
scene <- l_viewport(list(l_place(element, right = 12, top = 12)),
  width = 240, height = 180, background = "white"
)
l_render(scene)
```

Use `col`, `fill`, `fill_secondary`, `lwd`, `fontsize`, `fontface`, `fontfamily`,
`gp` and `label_gp` to customize the symbol. `labels = FALSE` hides labels;
character vectors of length 4, 8, 16 or 32 place translated labels clockwise
from north. `angle` rotates the whole rose counterclockwise. It does not
calculate geographic north or magnetic declination.

The symbol stays square inside rectangular boxes. Always provide explicit
layout dimensions: generic gTrees do not have automatic content bounds. Font
size is fixed in points; use larger boxes or smaller text for dense labels.
For export, wrap the grob with `l_place()` as well:

```r
l_save(l_place(rose, width = "100%", height = "100%"),
  type = "svg", dir = "exports", filename = "compass-rose",
  width = 240, height = 240
)
source(system.file("examples", "functions", "l_north_rose.R", package = "lplot"))
```

SVG export requires optional `svglite`. The example draws one customized
eight-point rose in a centered 160 by 160 box. Tests cover every design, exact point
counts, labels, styling, rotation, validation, extraction, layout, export,
nonblank and distinct raster output, square proportions and visual snapshots.

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "north-rose|function-examples|elements", reporter = "summary", stop_on_failure = TRUE)'
```

## Native Grid Units

`l_unit(x, units, data = NULL)` delegates directly to `grid::unit()` and returns
the same native `unit` object. It accepts the same arguments and uses grid's
validation and recycling rules, without adding a dependency or a new unit class:

- `x`: numeric vector of values.
- `units`: grid unit names, such as `"mm"`, `"cm"`, `"inches"`, `"points"`,
  `"npc"`, `"native"`, `"lines"` or `"char"`. Unit names can also be a vector.
- `data`: optional text, expression, grob, grob path or list for units that
  require it, such as `"strwidth"`, `"strheight"`, `"grobwidth"` and `"grobheight"`.
  The default is `NULL`.

```r
spacing <- l_unit(5, "mm")
identical(spacing, grid::unit(5, "mm")) # TRUE
l_unit(c(0.25, 0.75), "npc")
l_unit(c(1, 5), c("cm", "mm"))
l_unit(1, "strwidth", data = "Survey area")

outline <- l_rect(
  width = l_unit(20, "mm"), height = l_unit(10, "mm"),
  fill = NA, col = "black"
)
label <- l_text("Survey", x = spacing, just = "left")
l_unit(1, "grobwidth", data = label)
l_unit(1, "npc") - spacing
```

Use these objects inside `l_rect()`, `l_text()`, template children and native
grid grobs or viewports. Native arithmetic, indexing and `grid::unit.c()` work
unchanged. Construction does not draw, open a device or convert to a fixed
numeric size. Grid evaluates relative units and font/grob measurements in the
applicable viewport and device; use `grid::convertUnit()`, `grid::convertWidth()`
or `grid::convertHeight()` for explicit conversions in that context.

`l_unit(0.5, "npc")` means half a native grid viewport; `l_length("50%")` is an
lplot layout length. Use `l_length()` or layout strings for `l_place()` and
`l_viewport()` constraints. Grid units do not accept CSS-like `"px"`, `"%"`,
`"vw"`, `"auto"` or `"clamp(...)"`. Invalid input produces grid errors.
The `"null"` unit has its relative-sizing meaning in `grid::grid.layout()`, not
in the lplot layout engine. Native `npc` coordinates retain a bottom-left origin.

## Save Images

Install the codecs for the formats you need:

```r
install.packages(c("ragg", "svglite", "webp"))
```

```r
path <- l_save(scene, type = "png", dir = "exports", filename = "map",
               width = 900, height = 500, dpi = 192)
print(path)
l_save(scene, type = "jpeg", dir = "exports", filename = "map", quality = 90)
l_save(scene, type = "svg", dir = "exports", filename = "map")
l_save(scene, type = "webp", dir = "exports", filename = "map", quality = 90)
```

`plot` is an explicit ggplot, grob, element or logical scene. `type` is
case-insensitive; `jpg` and `jpeg` are aliases. `dir` is a directory, created
recursively if needed, and `filename` is a basename with an optional matching
extension. With only the required plot, defaults are PNG, the working directory,
filename `plot.png`, 800 by 600 logical pixels, 96 dpi and white background.

The helper returns an absolute path invisibly. Files are not overwritten unless
`overwrite = TRUE`; the previous file survives a rendering failure. Export uses
a temporary file and its own graphics device, then restores the caller's device.
It never saves a screenshot of the current pane or relies on its dimensions.

Width/height are logical pixels (1/96 inch), not necessarily raster file pixels:
900 by 500 at 192 dpi produces 1800 by 1000 pixels. SVG dimensions do not depend
on DPI. Raster exports allow at most 100 million pixels, with a WebP limit of
16383 pixels per axis. `quality` (0-100) controls lossy JPEG/WebP only.
PNG/SVG/WebP accept `background = "transparent"`; JPEG requires an opaque
background. Opaque scene backgrounds still cover transparent device backgrounds.
SVG preserves vector sources; raster map layers remain raster within the SVG.

Optional runtime dependencies: `ragg` for PNG/JPEG, `svglite` for SVG, and
`ragg` plus `webp` for WebP. No installation occurs automatically. Layout warnings
are not suppressed. PDF remains available through `pdf()` plus `l_render()`,
not through `l_save(type = "pdf")`.

## Layout Contract

- Coordinates start at the top-left; positive y points down.
- Numbers are logical pixels. `1px = 1/96in`; `1pt = 1/72in`.
  DPI controls raster output density, not the physical definition of a logical pixel.
- `%` uses the parent's content box; `vw`, `vh`, `vmin`, `vmax` use the root.
  Lengths also accept `mm`, `cm`, `in`, `auto`, and nested `min()`, `max()`, `clamp()`.
- Width/height describe the border box. Padding/borders are inside it; margins are outside.
  One to four edge values follow CSS order, or use a named list of sides.
- `auto` measures elements intrinsically; plots, panels and viewports fill available space.
  Opposing insets derive an `auto` dimension. Inconsistent constraints raise typed errors.
- Nine anchors align an element to `x`/`y`, or dock it when coordinates are absent.
  Insets pin edges, independently of the anchor. `x` cannot be mixed with left/right,
  nor `y` with top/bottom. Min/max and aspect ratio are resolved before placement.
- Root width/height are reference dimensions for resolution without a graphics device.
  Rendering defaults to the actual current viewport; explicit `l_render(width, height)`
  uses physical logical-pixel dimensions. Nested viewport declarations remain constraints.
- `l_join()` creates a parent, preserving child-local coordinates. Use explicit placement
  or `flow = "row"` / `"column"` with `gap`. The default is absolute positioning;
  `flow = "stack"` overlays children. Root margins only take effect when nested.
- Children draw by increasing `z_index`, with input order breaking ties.
  `overflow = "visible"` is the default; use `"hidden"` or `"inherit"` explicitly.
- `l_measure()` and `l_resolve()` do not draw or leave devices open. They use an existing
  device's font metrics, or a temporary null PDF device when no device exists.
- `l_as_grob()` returns a lazy grob that resolves again on each draw. Resolved layouts
  are inspection results for one context, never replacements for the logical scene.
- `l_render()` also records a lazy scene: when the graphics device replays it at a
  different size, automatic dimensions are recalculated. Explicit `width`/`height`
  remain fixed on their respective axes. Its returned layout describes the initial
  draw only. Frozen IDE previews still need a redraw request.

## Elements and Styles

Native extraction supports title, subtitle, caption, tag, legend, axes, axis titles,
panel, strip, plot background and panel background. The original plot is unchanged.
Use `l_without()` to remove components explicitly; removing entire data panels is
intentionally rejected. Missing elements raise `lplot_missing_element`, or return
`NULL` with `missing = "null"`. Multiple matches preserve their gtable arrangement;
use `which = 1` to select a single match.

Typography inherits the captured source theme. Supported overrides include `color`
(`colour`), `font_family`, `font_size`, `font_face`, `line_height`, `alpha`, `opacity`,
`padding`, `margin`, `background` and `border`. Background elements and generic grobs
also support `fill`, `line_width`, `line_type`. Legends additionally accept
`legend.direction`, `legend.key_width`, `legend.key_height`. Unknown styles are errors,
not silently ignored. Whole plots/viewports accept presentation properties only:
background, border, margin and padding. Borders accept a color or a list containing
`color`, `width` and `line_type`.

Custom registries are explicit, immutable values, not global registrations:

```r
registry <- l_register_element(
  "badge", can_extract = is.character,
  extract = function(plot) grid::textGrob(plot),
  measure = function(element, context) c(width = 80, height = 24)
)
badge <- l_get_element("Survey area", "badge", registry = registry)
```

Callbacks are `extract(plot, ...)`, `can_extract(plot, ...)`,
`measure(element, context)` and `style(element, style, context)`.
Measurers receive prepared content and return width/height in logical pixels.
Style callbacks receive resolved style lengths and return a grob. `extractor` and
`measurer` are aliases. Pass an existing registry when adding another type.
Direct grobs support north_arrow, scale_bar, map_frame, credits, annotation, inset
and custom types. No cowplot adapter is required for core functionality.

## Automatic Placement

Set `collision = "avoid"`, `"shrink"` or `"avoid-and-shrink"` explicitly.
Higher priorities are placed first; ties use input order. Explicit coordinates/insets
are fixed unless `allow_move = TRUE`. Shrinking additionally requires `responsive = TRUE`
and an explicit `min_width` or `min_height`; it scales content uniformly with a 10%
lower safety bound, retaining padding and borders. Candidate anchors can be supplied
using `candidates`. The attempted boxes are available in each resolved node's
`collision_candidates`.

Safe areas constrain automatic positioning. Base plots and panel/background elements
are not obstacles; other sibling boxes are. Override using
`metadata = list(obstacle = TRUE)` or `FALSE`. This is rectangle-based placement,
not analysis of the data painted inside a map. Impossible placement keeps the
preferred box and emits `lplot_collision`; overflow emits `lplot_overflow`.

`l_render(scene, debug = TRUE)` shows boxes, margins, content, intrinsic bounds,
anchors, IDs, coordinates, z-order and collision candidates.

## Maintaining API Documentation

All 19 exported functions and four registered S3 methods have English roxygen2
documentation next to their definitions in [R/](R/). Each help topic includes
parameters, return values, usage details, related functions and runnable examples.

After editing these comments, regenerate the help files and namespace from the
package root (install `roxygen2` as a development tool first if needed):

```sh
Rscript -e 'roxygen2::roxygenise(".")'
```

The files in [man/](man/) and [NAMESPACE](NAMESPACE) are generated; edit the
source comments instead of modifying these outputs by hand, and include the
regenerated files when submitting changes. roxygen2 is not a runtime dependency.
Check the generated examples with `R CMD check` before publishing. Passing
documentation checks does not replace the remaining CRAN submission requirements.

## Validation

```sh
Rscript -e 'testthat::test_local(reporter = "summary")'
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "visual")'
R CMD build .
R CMD check --no-manual lplot_0.1.0.tar.gz
```

Tests cover lengths, extraction/theme inheritance, style isolation, constraints,
nesting/joining, four output sizes, collision policy, rendering and visual references.
The `templates` and `function-examples` tests also verify `l_unit()` against
`grid::unit()`, including vectors, auxiliary data, arithmetic, invalid inputs,
viewport-dependent conversion and identical rendering at two sizes.
Export tests decode JPEG/PNG/WebP, inspect SVG and check device restoration,
file safety and dimensions. Run `testthat::test_local(filter = "save")`;
full codec tests additionally use optional `png` and `jpeg` image readers.
Real map exports in PNG, JPG, JPEG, SVG and WebP are kept in
[tests/testthat/\_snaps/save](tests/testthat/_snaps/save). To compare fresh exports
against these references, run:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(filter = "save", reporter = "summary")'
```

These snapshots also require `sf`. Raster checks compare decoded pixels and
dimensions; SVG checks compare its textual representation. Changed snapshots
fail instead of replacing approved references.

Visual snapshots use optional test dependencies `vdiffr` and `svglite`. There is no persisted
cross-device measurement cache; prepared content is reused by the renderer within
one resolution. Optimize further only against measured workloads.

## Deliberate Boundaries

This release does not implement breakpoint/media-query syntax, automatic text wrapping,
a browser CSS engine, or geographic calculations for scale bars/north arrows.
Supply those symbols as grobs or extraction adapters. Content that cannot fit produces
diagnostics rather than silent clipping.
Package maintainer metadata currently uses a placeholder address; replace it before publication.
