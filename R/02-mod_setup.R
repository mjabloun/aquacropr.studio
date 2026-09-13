mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Load an AquaCrop text file"),
      shiny::p(class = "step-hint",
        "Load a ", shiny::tags$code(".CRO"), ", ",
        shiny::tags$code(".SOL"), ", ", shiny::tags$code(".PRM"),
        ", ", shiny::tags$code(".PRO"), ", or other plugin file from the project folder. Edit and save on the right \u2014 useful for templates with ",
        shiny::tags$code("{placeholders}"), "."
      ),
      shiny::textInput(
        ns("ac_load"), "Existing file to load", width = "100%",
        placeholder = "e.g. templates/WheatGDD.CRO"
      ),
      shiny::actionButton(ns("ac_open"), "Load file", class = "btn-outline-primary")
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("File editor"),
      shiny::h6("Save this text"),
      shiny::p(class = "step-hint",
        "A relative path is written under the Project folder. This does not change ",
        shiny::strong("LIST project"),
        " on the Project page."
      ),
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shiny::div(
          class = "flex-grow-1",
          shiny::textInput(
            ns("ac_save"), "Save as", width = "100%",
            placeholder = "e.g. templates/WheatGDD.CRO"
          )
        ),
        shiny::actionButton(ns("ac_write"), "Save file", class = "btn-primary mb-3")
      ),
      shiny::hr(),
      shiny::p(class = "step-hint",
        "To mark a parameter, write the placeholder with braces, e.g. ",
        shiny::tags$code("{CCx}"),
        " \u2014 not the bare name ",
        shiny::tags$code("CCx"),
        "."
      ),
      ac_editor_ui(
        ns("ac_text"),
        placeholder = "e.g. load a .CRO / .PRM file, or paste AquaCrop text here"
      )
    )
  )
}

mod_setup_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(project$session_reset, {
      update_ac_editor(session, "ac_text", "")
      shiny::updateTextInput(session, "ac_load", value = "")
      shiny::updateTextInput(session, "ac_save", value = "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ac_open, {
      tryCatch({
        need_project(project)
        path <- resolve_project_file(project, input$ac_load, must_exist = TRUE)
        update_ac_editor(session, "ac_text", read_text_file(path))
        if (!nzchar(trimws(input$ac_save %||% "")))
          shiny::updateTextInput(session, "ac_save", value = trimws(input$ac_load))
        log_append(project, "Loaded ", path, " into the file editor.")
        shiny::showNotification(paste("Loaded", path), type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$ac_write, {
      tryCatch({
        need_project(project)
        out_path <- resolve_project_file(project, input$ac_save)
        write_text_file(out_path, input$ac_text %||% "")
        log_append(project, "Saved ", out_path)
        shiny::showNotification(paste("Saved", out_path), type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })
  })
}
