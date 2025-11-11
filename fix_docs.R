#!/usr/bin/env Rscript

# Script to fix remaining documentation issues systematically

message("Regenerating documentation...")
devtools::document()

message("Checking documentation...")
devtools::check_man()

message("Documentation fixes applied. Run devtools::check() to verify.")
