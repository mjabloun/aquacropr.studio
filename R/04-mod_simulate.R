mod_simulate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "simulate-page",
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Run AquaCrop"),
        shiny::p(class = "step-hint",
          "Launches the plugin from the plugin folder. It reads ",
          shiny::tags$code("LIST/ListProjects.txt"),
          ". Tick ",
          shiny::strong("Use parameter defaults"), " to render templates first. Open ",
          shiny::strong("Plots"), " afterwards to read and chart ",
          shiny::tags$code(".OUT"), " files."
        ),
        shiny::uiOutput(ns("param_config_controls")),
        shiny::actionButton(ns("run"), "Run AquaCrop", class = "btn-primary")
      ),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Last run"),
        shiny::verbatimTextOutput(ns("run_summary"))
      )
    ),
    bslib::card(
      class = "simulate-log-card",
      fill = FALSE,
      bslib::card_header("Plugin folder / OUTP"),
      shiny::div(
        class = "d-flex justify-content-end mb-2",
        shiny::actionButton(ns("reload_log"), "Refresh listing", class = "btn-sm btn-outline-secondary")
      ),
      shiny::verbatimTextOutput(ns("run_log"))
    )
  )
}

mod_simulate_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    has_param_config <- function() {
      !is.null(project$config) && isTRUE(project$config_ok)
    }

    output$param_config_controls <- shiny::renderUI({
      if (!has_param_config()) return(NULL)
      shiny::checkboxInput(session$ns("use_defaults"), "Use parameter defaults", value = FALSE)
    })

    shiny::observe({
      tryCatch({
        use_tmpl <- has_param_config() && isTRUE(input$use_defaults)
        shiny::updateActionButton(
          session, "run",
          label = if (use_tmpl) "Render and run" else "Run AquaCrop"
        )
      }, error = function(e) NULL)
    })

    shiny::observeEvent(input$run, {
      tryCatch({
        need_project(project)
        use_tmpl <- has_param_config() && isTRUE(input$use_defaults)
        with_nav_lock(session, project, function() {
          tryCatch({
            values <- NULL
            shiny::withProgress(message = "Running AquaCrop", value = 0.2, {
              if (use_tmpl) {
                values <- parameter_defaults(project$config)
                shiny::incProgress(0.4, detail = "Executable")
              }
              run_project_aquacrop(project, values = values, show_log = TRUE)
            })
            log_append(project, "AquaCrop finished with status ", project$last_status)
            if (!identical(project$last_status, 0L))
              shiny::showNotification("AquaCrop returned a non-zero status. Check OUTP/.", type = "warning")
            else
              shiny::showNotification("Run finished. Use the Plots page to view output.", type = "message")
          }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
        })
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    output$run_summary <- shiny::renderText({
      if (is.null(project$last_status))
        return("No AquaCrop run in this session yet.")
      vals <- project$last_run_values
      val_txt <- if (is.null(vals)) "\n(plugin as-is; templates not rendered)"
      else paste0("\n", paste(sprintf("  %s = %s", names(vals), signif(vals, 5)), collapse = "\n"))
      paste0(
        "status = ", project$last_status,
        "\nplugin = ", plugin_dir_of(project),
        "\nLIST project = ", project$list_project %||% "",
        val_txt
      )
    })

    shiny::observeEvent(input$reload_log, {
      project$ac_log_text <- describe_last_run(project)
      project$ac_log_stamp <- as.numeric(Sys.time())
    })

    output$run_log <- shiny::renderText({
      project$ac_log_stamp
      txt <- project$ac_log_text %||% ""
      if (!nzchar(txt)) {
        return(paste(
          "No listing yet.",
          "Run AquaCrop, or click Refresh listing if OUTP/ already has files."
        ))
      }
      txt
    })
  })
}
