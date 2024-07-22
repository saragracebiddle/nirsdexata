#' UI for plotting gadget
#'
#' @noRd
ook_ui <- function(){


}

#' Server logic for plotting gadget
#'
#' @param input,output,session Internal `{shiny}` parameters
#' @noRd
ook_server <- function(){

}

#' Plotting gadget for rawdata object
#'
#' @param rawdata rawdata object
#'
#' @return TODO
#' @export
plot_rawdata <- function(rawdata){

  ui <- miniPage(
    gadgetTitleBar("Plot",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done")),
    miniContentPanel(
      plotOutput("ook_plot", height = "100%", brush = "brush",
                 dblclick = "dblclick")
    ),
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
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
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
          type = vals$type,
          xlim = ranges$x,
          ylim = ranges$y
        )

    })



    output$ook_plot <- renderPlot({
      ggook(plotdata(), ranges$x, ranges$y)
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
      brushed <- input$brush
      brsh <- brushedPoints(plotdata(),
                            input$brush,
                            xvar = "ZeroedTime",
                            yvar = "value")

      new <- unique(brsh$samp_num)
      print(new)
      vals$removed = union(vals$removed, new)

    })



  }
  runGadget(ui, server, viewer =dialogViewer("plot_rawdata"))
}

wrapPage <- function(title, content){
  miniTabPanel(
    title,
    miniContentPanel(content)
    )
}

ook_by_type_UI <- function(id, types){
  n <- length(types)

  wrapped <- vector("list", n)

  for(t in types){
    wrapped[[i]] <- wrapPage(t, plotOutput(type))
  }

  wrapped$id <- NS(id, "tabPanelByType")
  do.call("miniTabStripPanel", wrapped)
}

ook_by_type_Server <- function(id, types){
  moduleServer(id, function(input, output, session){


  })
}


