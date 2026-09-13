# aquacropr.studio

Shiny studio for the **aquacropr** package: set up AquaCrop plugin
projects, edit parameters and AquaCrop text files, run simulations,
define objectives, and run calibration, sensitivity analysis, and
space-filling designs from a browser.

Requires a working **aquacropr** install and, for simulation, the FAO
AquaCrop stand-alone plugin (see the aquacropr README).

## Launch

```r
library(aquacropr.studio)
run_aquacropr_studio()
```

Sessions are stored in `~/aquacropr-studio-sessions` (your user home).
Project-folder copies are still found if you point the app at a folder.
The plugin directory is the AquaCrop install path: the executable and
`SIMUL/` are resolved from that folder.

## Status

Pages for project, setup (`.CRO`, `.SOL`, `.PRM`, `.PRO`, …), parameters,
simulate, plots, observe, calibrate, sensitivity, and design.
