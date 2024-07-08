library(shiny)

ui <- bootstrapPage(
    sliderInput("slider", "min value:", value = 0, min = -3, max = 3),
    mapgl::mapboxglOutput("map")
  )



server <- function(input, output, session) {
    map_shapes <- strayr::read_absmap("sa32021")
    map_shapes$measure <- stats::rnorm(n = nrow(map_shapes))
    
    output$map <- mapgl::renderMapboxgl({
      mapgl::mapboxgl(mapgl::mapbox_style("streets")) |>
        mapgl::fit_bounds(map_shapes, animate = FALSE) |>
        mapgl::add_fill_layer(id = "polygon_layer",
                             source = map_shapes,
                             fill_color = "blue",
                             fill_opacity = 0.5)
    })
    
    observe({
      mapgl::mapboxgl_proxy("map") |>
        mapgl::set_filter("polygon_layer",
                         list(">=", mapgl$get_column("measure"), input$slider))
    })
    
  }

shiny::shinyApp(ui, server)
