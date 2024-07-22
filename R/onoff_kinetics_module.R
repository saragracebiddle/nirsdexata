#' Plotting gadget for rawdata object
#'
#' @param rawdata rawdata object
#'
#' @return rawdata object
#' @export
interact_rawdata <- function(rawdata){

  ui <- miniPage(
    gadgetTitleBar("Plot",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done")),
    fillCol(
      miniContentPanel(
        plotOutput("ook_plot", height = "100%", brush = "brush",
                   dblclick = "dblclick")
      ),
      miniContentPanel(
        plotOutput("ook_plot_zoom", height = "100%", brush= "brush2")
      )
    )
    ,
    miniButtonBlock(
      actionButton("reset", "Reset"),
      actionButton("remove", "Remove")
    )
  )

  server <- function(input, output, session){

    vals <- reactiveValues(
      meas_start = rawdata$info$bounds$meas_start,
      meas_end = rawdata$info$bounds$meas_end,
      bads = rawdata$info$bads,
      removed = rawdata$info$bounds$removed,
      col_types = rawdata$info$cols$col_type,
      col_names = rawdata$info$cols$col_name,
      type = NULL,
    )


  ranges <- reactiveValues(x = NULL, y = NULL)

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
      ranges$xmin <- brush$xmin
      ranges$xmax <- brush$xmax
      ranges$ymin <- brush$ymin
      ranges$ymax <- brush$ymax

    } else {
      ranges$xmin <- NULL
      ranges$xmax <- NULL
      ranges$ymin <- NULL
      ranges$ymax <- NULL
    }
  })

    plotdata <- reactive({
        prepare_shiny_plot(
          rawdata$data,
          sfreq = rawdata$info$sfreq,
          meas_start = vals$meas_start,
          meas_end = vals$meas_end,
          bads = vals$bads,
          removed = vals$removed,
          col_types = vals$col_types,
          col_names = vals$col_names,
          type = vals$type
        )

    })

    plotdata_zoom <- reactive({
      if(is.null(ranges$xmin)){
        plotdata()
      }else{
      plotdata() |>
        dplyr::filter(
          ZeroedTime > ranges$xmin & ZeroedTime < ranges$xmax
        ) |>
        dplyr::filter(
          value > ranges$ymin & value < ranges$ymax
        )
      }

    })



    output$ook_plot <- renderPlot({
      ggook(plotdata(), NULL, NULL)
    })

    output$ook_plot_zoom <- renderPlot({
      ggook(plotdata_zoom(), NULL, NULL)
    })

    observeEvent(input$cancel, {
      # When `Cancel` actionButton is clicked,
      # return original rawdata object
      stopApp(rawdata)
    })

    observeEvent(input$done, {
      # Change the values contained in info to the reactive vals
      rawdata$info$bounds$meas_start <- vals$meas_start
      rawdata$info$bounds$meas_end <- vals$meas_end
      rawdata$info$bounds$removed <- vals$removed
      rawdata$info$bads <- vals$bads
      # Return the updated rawdata object
      stopApp(rawdata)

    })

    observeEvent(input$reset, {
      # change the reactive values back to the original values from rawdata$info
      vals$bads <- rawdata$info$bads
      vals$removed <- rawdata$info$bounds$removed
      vals$meas_start <- rawdata$info$bounds$meas_start
      vals$meas_end <- rawdata$info$bounds$meas_end
    })


    observeEvent(input$remove, {

      brsh <- brushedPoints(plotdata_zoom(),
                            input$brush2)

      new <- unique(brsh$samp_num)
      print(brsh)
      vals$removed = union(vals$removed, new)

    })



  }
  runGadget(ui, server, viewer =dialogViewer("plot_rawdata"))
}

