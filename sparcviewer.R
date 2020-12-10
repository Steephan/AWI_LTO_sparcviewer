#############################################################################
##
##   Shiny App
##
##   App to display the time series of a selection of variables from the selected station, dataset and year
##
##
##
##   written by:  stephan.lange@awi.de
##                christian.lehr@awi.de
##   last modified: 2020-01-15
##
##   last check: 2020-01-27
##   checked by: christian.lehr@awi.de
##
###############################################################################
##
## open issues:
##
##   -  modification of code to stop warnings:
##          "Warnung in structure(x, class = unique(c("AsIs", oldClass(x))))
##          Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
##          Consider 'structure(list(), *)' instead."
##
##  Maybe it is an Rbokeh-version issue:
##  https://github.com/bokeh/rbokeh/issues/216
##  https://github.com/rstudio/shiny/issues/1682
##
#############################################################################
##
## last modification:
##   - exclusion of calendar option to select date because of synchronization issues ==> the app was not running stable on the local server when switching between different years
##
##
##
#############################################################################
## comments:
##
#############################################################################

library(shiny)
library(ggplot2)
library(reshape2)
library(rbokeh)

today <- Sys.time()
year  <- as.POSIXlt(today)$year + 1900

running.system <- 1
#
# 1 - windows
# 2 - linux AWI

#### import data ####

## read paths and allowed variables
if (running.system == 1) {

    yearlyDatasetPaths <- read.csv("N:/sparc/LTO/R_database/flagger_sa/yearlyDataPath_auto.csv",
                                   stringsAsFactors = FALSE, strip.white = TRUE)
    allowedVariables <- read.csv("N:/sparc/LTO/R_database/flagger_sa/allowedVariables.csv",
                                 stringsAsFactors = FALSE, strip.white = TRUE)
    # read file for modification of style of shiny-app
    source("N:/sparc/LTO/R_database/flagger_sa/appCSS.R")
} else if (running.system == 2) {
    yearlyDatasetPaths <- read.csv("/sparc/LTO/R_database/flagger_sa/yearlyDataPath_AWI.csv", stringsAsFactors = FALSE,
                                   strip.white = TRUE)
    allowedVariables <- read.csv("/sparc/LTO/R_database/flagger_sa/allowedVariables.csv", stringsAsFactors = FALSE,
                                 strip.white = TRUE)
    # read file for modification of style of shiny-app
    source("/sparc/LTO/R_database/flagger_sa/appCSS.R")
}

allowedVariables$selected <- FALSE
allowedVariables$colour   <- FALSE

# figure width
fig.width <- 1200

###############################
#### server logic ####
server <- shinyServer(function(input, output, session) {
    # Initialize variable to save application state
    store <- reactiveValues(selectedDaterange = c(0, 1), variables = allowedVariables,
                            currentData = data.frame())

    observeEvent(input$station, {
        # update years list
        years <- sort(unique(yearlyDatasetPaths$year[yearlyDatasetPaths$station == input$station]), decreasing = TRUE)
        if (input$year %in% years) {
            # double update to trigger input$year invalidation
            updateSelectInput(session, "year", choices = years, selected = input$year)
        } else {
            updateSelectInput(session, "year", choices = years)
        }
    })

    observeEvent(list(input$year, input$station), {
        # Update dataset choice according to year or station change
        datasets <- with(yearlyDatasetPaths, dataset[year == input$year &
                                                     station == input$station])
        if (input$dataset %in% datasets) {
            # double update to trigger input$dataset invalidation
            updateSelectInput(session, 'dataset', choices = datasets, selected = input$dataset)
        } else {
            updateSelectInput(session, 'dataset', choices = datasets)
        }
        # Update date sliders
        minDate <- as.Date(paste0(input$year, "-01-01"))
        maxDate <- as.Date(paste0(as.numeric(as.character(input$year)) + 1, "-01-01")) # OLD: minDate + 365
        updateSliderInput(session, "dateSlider", min = minDate, max = maxDate,
                          value = c(minDate, maxDate))

        ##############################################
        ### excluded because of synchronization issues
        # updateDateRangeInput(session, "dateInput", min = minDate, max = maxDate,
        #                      start = minDate, end = maxDate)
        ##############################################
    })

    observeEvent(input$dataset, {
        # update variable checkbox choices on dataset change
        choiceVariables <- store$variables[store$variables$dataset == input$dataset,
                                           "variable"]
        selectedVariables <- store$variables[store$variables$dataset == input$dataset &
                                             store$variables$selected, "variable"]
        updateCheckboxGroupInput(session, "variables", choices = choiceVariables,
                                 selected = selectedVariables)
    })

    observeEvent(list(input$variables, input$year), ignoreInit = TRUE, {
        #### updates data on variable or year change
        timing <- proc.time()
        ##########
        # abort if dataset / year inconsistency
        # (eg. when year changed but dataset didn't update yet)
        availableDatasets <- yearlyDatasetPaths$dataset[yearlyDatasetPaths$year == input$year]
        if (!input$dataset %in% availableDatasets) return()
        ##########
        # update selected variables
        store$variables <- within(store$variables, {
            selected[dataset == input$dataset] <- (variable[dataset == input$dataset] %in% input$variables)
        })
        ##########
        # abort if no selected data available for current year
        selectedVars <- store$variables[store$variables$selected &
                                        store$variables$dataset %in% availableDatasets, ]
        if (nrow(selectedVars) == 0) {
            store$currentData <- NULL
            return()
        }
        ##########
        # get data
        newdata <- data.frame()
        for (dataset in unique(selectedVars$dataset)) {
            path <- yearlyDatasetPaths[yearlyDatasetPaths$dataset == dataset &
                                       yearlyDatasetPaths$year == input$year, "path"]
            # path will be empty if dataset changed and input$variable did not update
            #if (length(path) == 0) return()
            # get names of selected variable and associated flag
            variablenames <- selectedVars[selectedVars$dataset == dataset, "variable"]
            flagnames <- paste0(variablenames, "_fl")
            # read data
            if (!file.exists(path)) {
                warning(paste("File", path, "not found."))
                return()
            }
            d <- read.csv(path)[, c("UTC", variablenames, flagnames)]
            # reshape for plotting with rBokeh, ggplot2 etc...,
            # variable column indices: 2:(2 + length(variablenames) - 1)
            # flag column indices: (2 + length(variablenames)):ncol(d))
            d.m <- reshape(d, direction = "long",
                           idvar = "UTC",
                           varying = list(2:(2 + length(variablenames) - 1),
                                          (2 + length(variablenames)):ncol(d)),
                           timevar = "variable",
                           times = names(d)[2:(2 + length(variablenames) - 1)],
                           v.names = c("value", "flag"))
            d.m$dataset <- dataset
            newdata <- rbind(newdata, d.m)
        }
        newdata$UTC <- as.POSIXct(newdata$UTC, tz = 'UTC', origin = '1970-01-01' )
        store$currentData <- newdata

        ##########
        ## update dates
        minDate <- as.Date(min(newdata$UTC))# as.Date(newdata$UTC[1])
        maxDate <- as.Date(max(newdata$UTC))# as.Date(rev(newdata$UTC)[1])

      #  minDate <- as.Date(paste0(input$year, "-01-01"))
      #  maxDate <- as.Date(paste0(as.numeric(input$year) + 1, "-01-01")) # OLD: minDate + 365

        # if (the year stays the same ==> update startDate values with values of input$dateSlider
        # else (for new year ==> change startDate to minDate / maxDate of the selected year)
        if (input$dateSlider[1] >= minDate & input$dateSlider[1] <= maxDate) {
            startDate <- input$dateSlider[1]
        } else startDate <- minDate

        if (input$dateSlider[2] >= minDate & input$dateSlider[2] <= maxDate) {
            endDate <- input$dateSlider[2]
        } else endDate <- maxDate

        updateSliderInput(session, "dateSlider", min = minDate, max = maxDate,
                          value = c(startDate, endDate))

        ##############################################
        ### excluded because of synchronization issues
        # updateDateRangeInput(session, "dateInput", min = minDate, max = maxDate,
        #                      start = startDate, end = endDate)
        ##############################################

        store$selectedDaterange <- c(startDate, endDate)

        cat(file = stderr(), "\nTime spent reading & preparing data:",
            (proc.time() - timing)[3], "s\n")
    })

    #
    # ##############################################
    # ### excluded because of synchronization issues
    #
    observeEvent(input$dateSlider, ignoreInit = TRUE, {

        store$selectedDaterange <- input$dateSlider
        # # Update daterange on dateSlider change and sync dateInput
        # # Check if actually changed to prevent infinity loooop
        # #if (any(input$dateSlider != input$dateInput)) {
        # #if (any(as.character(input$dateSlider) != as.character(input$dateInput))) {
        #
        # # if there are not the same dates
        #  if (sum(as.character(input$dateSlider) %in% as.character(input$dateInput)) < 2) {
        #
        # # if ( (as.character(input$dateSlider[1]) != as.character(input$dateInput[1])) |
        # #      (as.character(input$dateSlider[2]) != as.character(input$dateInput[2])) ) {
        #     store$selectedDaterange <- input$dateSlider
        #     updateDateRangeInput(session, "dateInput",
        #                          start = input$dateSlider[1], end = input$dateSlider[2])
        # }
    })
    # ##############################################


    ##############################################
    ### excluded because of synchronization issues

    # observeEvent(input$dateInput, ignoreInit = TRUE, {
    #     # Update daterange on dateInput change and sync dateSlider
    #     # Check if actually changed to prevent infinity loooop
    #     #if (any(input$dateInput != input$dateSlider)) {
    #
    #     # same as above
    #     if (sum(as.character(input$dateInput) %in% as.character(input$dateSlider)) < 2) {
    #
    #     # if ( (as.character(input$dateSlider[1]) != as.character(input$dateInput[1])) |
    #     #      (as.character(input$dateSlider[2]) != as.character(input$dateInput[2])) ) {
    #         store$selectedDaterange <- input$dateInput
    #         updateSliderInput(session, "dateSlider",
    #                           value = c(input$dateInput[1], input$dateInput[2]))
    #     }
    # })
    ##############################################

    observeEvent(input$resetVariables, {
        # reset variable choices
        store$variables$selected <- FALSE
        updateCheckboxGroupInput(session, "variables", selected = character(0))
    })

    output$dataplot <- renderRbokeh({
        timing <- proc.time()
        validate(need(any(store$variables$selected), "Please select variables."))
        validate(need( !is.null(store$currentData),
                      # OLD: nrow(store$currentData) > 0,
                      paste("No data available for ",
                            apply(store$variables[store$variables$selected,
                                                  c("dataset", "variable")],
                                  1, function(x) paste(x, collapse = ":")))))
        # subset to selected date
        dm <- subset(store$currentData,
                     as.Date(UTC) >= store$selectedDaterange[1] &
                     as.Date(UTC) <= store$selectedDaterange[2])

        # remove short streaks of NA (< 12) in order to plot lines properly
        r <- rle(is.na(dm$value)) # run length encoding
        dm <- dm[!rep(r$values & r$lengths < 12, r$lengths), ]

        # Add dataset string to variable names where appropriate in order to show up in legend
        figs = list()
        dm <- split(dm, dm$dataset)

        test <<- dm

        figs <- lapply(dm, function(x) {
            #if (x$dataset[1] == "BaSnow2013") {height = 700} else {  height = 700    }
            figure(width = fig.width, height = 800, #responsive = TRUE, lod_threshold = 200,
                   webgl = F) %>%
                ly_lines(UTC, value, data = x, color = variable) %>% #variable) %>%
                ly_points(UTC, value, data = x, line_color = variable, fill_color = variable, #alpha = 0.5, #"lightgrey",#20",
                      hover = list(UTC, value, flag, variable), glyph = 16, size = 5,
                      legend = FALSE) %>%


                ### Can't use the data argument together with legend and color parameters,
                ### see issue bokeh/rbokeh/issues/132, adding everything manually
                ly_points(x[x$flag == 2, "UTC"], x[x$flag == 2, "value"], # system error
                          legend = "flag 2, system error", color = "red",
                          hover = subset(x, flag == 2)) %>%
                ly_points(x[x$flag == 3, "UTC"], x[x$flag == 3, "value"], # maintenance
                          legend = "flag 3, maintenance", color = "thistle",
                          hover = subset(x, flag == 3)) %>%
                ly_points(x[x$flag == 4, "UTC"], x[x$flag == 4, "value"], # physical limits
                          legend = "flag 4, physical limits", color = "skyblue",
                          hover = subset(x, flag == 4)) %>%
                ly_points(x[x$flag == 5, "UTC"], x[x$flag == 5, "value"], # gradient (peaks)
                          legend = "flag 5, gradient (peaks)", color = "darkblue",
                          hover = subset(x, flag == 5)) %>%
                ly_points(x[x$flag == 6, "UTC"], x[x$flag == 6, "value"], # plausibility
                          legend = "flag 6, plausibility", color = "palegreen",
                          hover = subset(x, flag == 6)) %>%
                ly_points(x[x$flag == 7, "UTC"], x[x$flag == 7, "value"], # shift
                          legend = "flag 7, shift", color = "orchid",
                          hover = subset(x, flag == 7)) %>%
                ly_points(x[x$flag == 8, "UTC"], x[x$flag == 8, "value"], # snow covered
                          legend = "flag 8, snow covered", color = "aquamarine",
                          hover = subset(x, flag == 8)) %>%
                ly_abline(h = 0, color = "darkred", type = 2) %>%
                tool_wheel_zoom(dimensions = c("height")) %>%
                tool_crosshair()
        })
        cat(file = stderr(), "Time spent on renderRbokeh", (proc.time() - timing)[3], "s\n")
        grid_plot(figs, ncol = 1, same_axes = c(TRUE, FALSE),  simplify_axes = FALSE)

    })
})

#### user interface ####
ui <- shinyUI(
    fluidPage(
        tags$style(appCSS),
        tags$head(tags$style(HTML(
            "#variables {-webkit-column-count: 2; /* Chrome, Safari, Opera */
            -moz-column-count: 3;    /* Firefox */
            column-count: 3;}
            #resetVariables {text-align: center; margin-bottom: 1.4em;}
            .col-sm-4 {width: 750px;}"))),
        p(" "),
        # Sidebar with a slider input for number of bins
        fluidRow(
            column(3, wellPanel(
                selectInput("station", "Choose a station:",
                            choices = sort(unique(yearlyDatasetPaths$station)),
                #             selected = "Samoylov"),
                # selectInput("year", "Choose a year:", choices = 2018, selected = 2018),
                # selectInput("dataset", "Choose a dataset:",
                #              choices = "BaSoil2002", selected = "BaSoil2002"),
                            selected = "Bayelva"),
                selectInput("year", "Choose a year:",
                            #choices = NULL, selected = NULL),
                            choices = sort(unique(yearlyDatasetPaths$year), decreasing = TRUE), selected = NULL),
                            #choices = 2016, selected = 2016),
                selectInput("dataset", "Choose a dataset:",
                            #choices = NULL, selected = NULL),
                            # sort(unique(yearlyDatasetPaths$year
                            choices = "BaMet2009", selected = "BaMet2009"),
                checkboxGroupInput("variables", "Select variables:", choices = NULL),
                actionButton("resetVariables", "Reset variables"),
                # reactive daterangeInput that syncs with dateSlider
                # dateRangeInput("dateInput", "From ... to:",
                #                #min = as.Date(today) - 365, max = as.Date(today),
                #                #start = as.Date(today) - 365, end = as.Date(today)),
                #                min = NULL, max = NULL,
                #                start = NULL, end = NULL),
                # # dateSlider that gets updated when store$dataset or dateInput changes
                sliderInput("dateSlider", "Choose date range:",
                            min = as.Date(today) - 365, max = as.Date(today),
                            value = c(as.Date(today) - 365, as.Date(today)))
            )),
            # Show a plot of the generated distribution
            column(9,
                   # Application title
                   # titlePanel("SPARC data viewer"),
                   rbokehOutput("dataplot", width = "100%", height = "100%")
            )
        )
    ))

#### run ####
#options(shiny.reactlog = FALSE)
# shinyApp(ui = ui, server = server)
runApp(shinyApp(ui = ui, server = server))
