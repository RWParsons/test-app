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
    sliderInput(ns("slider"), "min value:", value = 0, min = -3, max = 3),
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
      newProxy("map") |>
      # mapgl$mapboxgl_proxy(ns("map")) |> # also works if using the ns()
        mapgl$set_filter(
          "polygon_layer",
          list(">=", mapgl$get_column("measure"), input$slider)
        )
    })
    
  })
}



newProxy <- function(mapId, session = shiny::getDefaultReactiveDomain()){
  browser()
  if (is.null(session)) {
    stop("mapboxgl_proxy must be called from the server function of a Shiny app")
  }
  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(mapId, 1, nchar(session$ns(""))) != session$ns("")) {
    mapId <- session$ns(mapId)
  }
  structure(
    list(
      session = session, 
      id = mapId
    ), 
    class = "mapboxgl_proxy"
  )
}