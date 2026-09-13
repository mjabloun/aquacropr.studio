# aquacropr.studio 0.0.0.9000

* Initial package: Shiny studio for aquacropr (plugin project setup, parameters,
  simulate, plots, objective function, sensitivity, calibration, and design).
* Setup page edits AquaCrop text files (`.CRO`, `.SOL`, `.PRM`, `.PRO`, …)
  without converting through YAML.
* Session files live in `~/aquacropr-studio-sessions` (user home) and are listed
  even before a project folder is set; project-folder copies are still found.
* Plugin directory is the only AquaCrop install path: the executable and
  `SIMUL/` are resolved from that folder (command template remains optional).
* Plots / Observe omit plugin status files `AllDone.OUT` and
  `ListProjectsLoaded.OUT`.
