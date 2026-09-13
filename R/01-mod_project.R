mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("Project folder and AquaCrop"),
      shiny::p(class = "step-hint",
        "A project is a folder with templates, ",
        shiny::tags$code("parameters.yaml"),
        ", and observed data. The plugin folder is where ",
        shiny::tags$code("AquaCrop73.exe"),
        " lives together with ", shiny::tags$code("LIST/"), ", ",
        shiny::tags$code("SIMUL/"), ", and ", shiny::tags$code("OUTP/"),
        ". The executable and ", shiny::tags$code("SIMUL/"),
        " are taken from that folder."
      ),
      shiny::textInput(ns("plugin_dir"), "Plugin directory", width = "100%",
                       placeholder = "Folder that contains AquaCrop73.exe, SIMUL/, LIST/, OUTP/"),
      shiny::div(
        class = "daisy-radio-inline",
        shiny::radioButtons(
          ns("aquacrop_launch"),
          "How to run AquaCrop",
          choices = c("Local executable" = "exe", "Command template" = "cmd"),
          selected = "exe",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        condition = "input.aquacrop_launch == 'cmd'",
        ns = ns,
        shiny::textAreaInput(
          ns("aquacrop_cmd"),
          "Command template",
          width = "100%",
          rows = 3,
          placeholder = 'e.g. "{aquacrop_exe}"'
        ),
        shiny::p(class = "step-hint",
          "Must include ", shiny::tags$code("{aquacrop_exe}"),
          " and/or ", shiny::tags$code("{working_dir}"),
          ". ", shiny::tags$code("{aquacrop_exe}"),
          " is the executable found in the plugin folder."
        )
      ),
      shiny::textInput(ns("project_dir"), "Project directory", width = "100%",
                       placeholder = "e.g. C:/path/to/my_aquacrop_project"),
      shiny::textInput(ns("template_dir"), "Template directory", width = "100%",
                       placeholder = "e.g. C:/path/to/templates"),
      shiny::textInput(ns("output_dir"), "Rendered-file directory", width = "100%",
                       placeholder = "e.g. DATA (default: project directory)"),
      shiny::textInput(ns("list_project"), "LIST project (.PRM / .PRO)",
                       width = "100%", placeholder = "e.g. example_wheat.PRM"),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(ns("apply"), "Apply paths", class = "btn-primary"),
        shiny::actionButton(ns("load_example"), "Load wheat example", class = "btn-outline-secondary")
      )
    ),
    bslib::card(
      bslib::card_header("How to start"),
      shiny::tags$ol(
        shiny::tags$li("Point ", shiny::strong("Plugin directory"), " at the folder that contains ",
          shiny::tags$code("AquaCrop73.exe"), " and ", shiny::tags$code("SIMUL/"),
          " (AquaCrop 7.3 must be launched from that folder)."),
        shiny::tags$li("Optionally use a ", shiny::strong("Command template"),
          " for HPC/Singularity; otherwise the local executable in that folder is used."),
        shiny::tags$li("Choose a writable project folder, or load the bundled Tunis wheat example."),
        shiny::tags$li("Build or load parameters, run one simulation, then calibrate or analyse sensitivity.")
      ),
      shiny::hr(),
      shiny::p(shiny::strong("Bundled example")),
      shiny::p(class = "step-hint",
        "Copies ", shiny::tags$code("Example_aquacrop"),
        " files into ",
        shiny::tags$code("aquacropr-studio-workspace"),
        " under the current working directory and loads ",
        shiny::tags$code("CCx"), " / ", shiny::tags$code("WP"), " parameters."
      )
    )
  )
}

mod_project_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateRadioButtons(session, "aquacrop_launch", selected = project$aquacrop_launch %||% "exe")
      shiny::updateTextAreaInput(session, "aquacrop_cmd", value = project$aquacrop_cmd %||% "")
      shiny::updateTextInput(session, "plugin_dir", value = project$plugin_dir)
      shiny::updateTextInput(session, "project_dir", value = project$project_dir)
      shiny::updateTextInput(session, "template_dir", value = project$template_dir)
      shiny::updateTextInput(session, "output_dir", value = project$output_dir)
      shiny::updateTextInput(session, "list_project", value = project$list_project)
    })

    shiny::observeEvent(input$apply, {
      mode <- input$aquacrop_launch %||% "exe"
      cmd <- trimws(input$aquacrop_cmd %||% "")
      if (identical(mode, "cmd") &&
          !grepl("{aquacrop_exe}", cmd, fixed = TRUE) &&
          !grepl("{working_dir}", cmd, fixed = TRUE)) {
        shiny::showNotification(
          "Command template must include {aquacrop_exe} and/or {working_dir}.",
          type = "error",
          duration = NULL
        )
        log_append(project, "Command template rejected: missing placeholders.")
        return()
      }
      project$aquacrop_launch <- mode
      project$aquacrop_cmd <- cmd
      pd <- trimws(input$plugin_dir)
      project$plugin_dir <- if (nzchar(pd)) norm_dir(pd) else ""
      sync_plugin_paths(project)
      project$project_dir <- norm_dir(input$project_dir)
      td <- trimws(input$template_dir)
      project$template_dir <- if (nzchar(td)) norm_dir(td) else ""
      od <- trimws(input$output_dir)
      project$output_dir <- if (nzchar(od)) norm_dir(od) else ""
      project$list_project <- trimws(input$list_project)
      plugin <- plugin_dir_of(project)
      ok_plugin <- nzchar(plugin) && dir.exists(plugin)
      ok_exe <- aquacrop_launcher_ok(project)
      ok_simul <- nzchar(simul_dir_of(project)) && dir.exists(simul_dir_of(project))
      ok_dir <- dir.exists(project$project_dir)
      log_append(
        project,
        "Paths applied. Plugin: ", if (ok_plugin) plugin else "NOT SET",
        "; exe: ", if (ok_exe) basename(aquacrop_exe_of(project)) else "missing",
        "; SIMUL: ", if (ok_simul) "ok" else "missing",
        if (identical(mode, "cmd")) " (command template)" else "",
        "; project: ", if (ok_dir) "ok" else "missing",
        "; template: ", if (nzchar(project$template_dir)) project$template_dir else "(not set)"
      )
      if (!ok_plugin) {
        shiny::showNotification("Plugin directory does not exist.", type = "error")
      } else if (!ok_exe) {
        msg <- if (identical(mode, "cmd"))
          "Enter a command template that includes {aquacrop_exe} or {working_dir}, and put AquaCrop73.exe in the plugin folder."
        else
          "No AquaCrop executable found in the plugin directory."
        shiny::showNotification(msg, type = "error")
      } else if (!ok_dir) {
        shiny::showNotification("Project directory does not exist.", type = "error")
      } else {
        if (!ok_simul)
          shiny::showNotification("Plugin is ready, but SIMUL/ was not found under it.", type = "warning")
        else
          shiny::showNotification("Project paths saved.", type = "message")
      }
    })

    shiny::observeEvent(input$load_example, {
      src <- example_wheat_dir()
      if (!nzchar(src) || !dir.exists(src)) {
        shiny::showNotification("Could not find Example_aquacrop files.", type = "error")
        return()
      }
      dest <- normalizePath(file.path(getwd(), "aquacropr-studio-workspace"), winslash = "/", mustWork = FALSE)
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      for (sub in c("templates", "DATA")) {
        from <- file.path(src, sub)
        to <- file.path(dest, sub)
        if (dir.exists(from)) {
          dir.create(to, recursive = TRUE, showWarnings = FALSE)
          file.copy(list.files(from, full.names = TRUE), to, overwrite = TRUE, recursive = TRUE)
        }
      }
      obs <- file.path(src, "observed_yield.csv")
      if (file.exists(obs)) file.copy(obs, dest, overwrite = TRUE)
      repo <- dirname(src)
      plugin <- file.path(repo, "AquaCrop73_windows")
      project$project_dir <- dest
      project$template_dir <- file.path(dest, "templates")
      project$output_dir <- file.path(dest, "DATA")
      project$list_project <- "example_wheat.PRM"
      project$aquacrop_launch <- "exe"
      project$aquacrop_cmd <- ""
      if (dir.exists(plugin)) {
        project$plugin_dir <- normalizePath(plugin, winslash = "/", mustWork = FALSE)
      } else {
        project$plugin_dir <- default_plugin_dir()
      }
      sync_plugin_paths(project)
      yaml_path <- file.path(dest, "templates", "parameters.yaml")
      if (file.exists(yaml_path)) {
        yaml_text <- paste(readLines(yaml_path, warn = FALSE), collapse = "\n")
        tryCatch({
          apply_param_config(project, yaml_text, path = yaml_path)
        }, error = function(e) {
          project$config_yaml <- yaml_text
          project$config_path <- yaml_path
          project$config_ok <- FALSE
          project$config_msg <- err_text(e)
          log_append(project, "Example copied but parameter validation failed: ", err_text(e))
        })
      }
      shiny::updateRadioButtons(session, "aquacrop_launch", selected = "exe")
      shiny::updateTextAreaInput(session, "aquacrop_cmd", value = "")
      shiny::updateTextInput(session, "plugin_dir", value = project$plugin_dir)
      shiny::updateTextInput(session, "project_dir", value = project$project_dir)
      shiny::updateTextInput(session, "template_dir", value = project$template_dir)
      shiny::updateTextInput(session, "output_dir", value = project$output_dir)
      shiny::updateTextInput(session, "list_project", value = project$list_project)
      log_append(project, "Loaded wheat example into ", dest)
      shiny::showNotification("Wheat example loaded into aquacropr-studio-workspace.", type = "message")
    })
  })
}
