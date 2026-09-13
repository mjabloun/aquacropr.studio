test_that("studio session files round-trip project paths and config yaml", {
  st <- aquacropr.studio:::empty_project_state()
  st$project_dir <- "C:/tmp/ac_proj"
  st$list_project <- "example_wheat.PRM"
  st$config_yaml <- "name: CCx\n"
  st$config_ok <- TRUE
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  aquacropr.studio:::write_studio_session(tmp, st, label = "trial")
  got <- aquacropr.studio:::read_studio_session(tmp)
  expect_identical(got$project_dir, "C:/tmp/ac_proj")
  expect_identical(got$list_project, "example_wheat.PRM")
  expect_identical(got$config_yaml, "name: CCx\n")
  expect_true(got$config_ok)
  expect_null(got$last_sim)
})

test_that("session search includes the user home folder", {
  home <- aquacropr.studio:::studio_session_home_dir()
  expect_match(home, "aquacropr-studio-sessions$")
  dirs <- aquacropr.studio:::studio_session_search_dirs(NULL)
  expect_true(
    normalizePath(home, winslash = "/", mustWork = FALSE) %in% dirs
  )
})

test_that("apply_project_state fills missing fields from empty state", {
  proj <- list2env(aquacropr.studio:::empty_project_state(), parent = emptyenv())
  st <- list(project_dir = "C:/tmp/ac", config_ok = TRUE)
  aquacropr.studio:::apply_project_state(proj, st)
  expect_identical(proj$project_dir, "C:/tmp/ac")
  expect_true(proj$config_ok)
  expect_false(isTRUE(proj$run_busy))
})
