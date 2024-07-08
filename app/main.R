box::use(
  shiny[...],
  mapgl,
  strayr,
  dplyr,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    sliderInput("slider", "min value:", value = 0, min = -3, max = 3),
    mapgl$mapboxglOutput(ns("map"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    map_shapes <- strayr$read_absmap("sa32021")
    map_shapes$measure <- stats::rnorm(n = nrow(map_shapes))
    
    ns <- NS(id)
    cat(ns('map'))
    
    output$map <- mapgl$renderMapboxgl({
      mapgl$mapboxgl(mapgl$mapbox_style("streets")) |>
        mapgl$fit_bounds(map_shapes, animate = FALSE) |>
        mapgl$add_fill_layer(id = "polygon_layer",
                             source = map_shapes,
                             fill_color = "blue",
                             fill_opacity = 0.5)
    })
    
    observe({
      mapgl$mapboxgl_proxy(ns("map")) |>
        mapgl$set_filter(
          "polygon_layer",
          list(">=", mapgl$get_column("measure"), input$slider)
        )
    })
    
  })
}
