## Creation du R compendium
file.create(".here")
## Ajout d'un README ---
utils::file.edit(here::here("README.md"))
## Conversion d'un Rmd en un md ----
rmarkdown::render("README.Rmd")
## Conversion d'un qmd en un md ----
quarto::quarto_render("README.qmd")

## Ajout d'un fichier DESCRIPTION ----
rcompendium::add_description()
## Ajout d'une licence ----
rcompendium::add_license(license = "GPL-2")

## Ajout de sous-répertoires ----
rcompendium::add_compendium(
  compendium = c("data", "analyses", "R", "figures", "outputs")
)

## Ajout des dépendances dans DESCRIPTION ----
usethis::use_package(package = "here")
usethis::use_package(package = "utils")
usethis::use_package(package = "data.table")
usethis::use_package(package = "purrr")
usethis::use_package(package = "stringr")
usethis::use_package(package = "lme4")

usethis::use_package(package = "dplyr")
usethis::use_package(package = "lubridate")
usethis::use_package(package = "readr")
usethis::use_package(package = "suncalc")

usethis::use_package(package = "geosphere")
usethis::use_package(package = "sf")
usethis::use_package(package = "adehabitatHR")
usethis::use_package(package = "mapview")
usethis::use_package(package = "leaflet")
usethis::use_package(package = "move2")
usethis::use_package(package = "move")
usethis::use_package(package = "raster")

usethis::use_package(package = "move2")
usethis::use_package(package = "webshot")
usethis::use_package(package = "htmlwidgets")

usethis::use_package(package = "ggplot2", type = "Depends")

## Ajout d'un makefile ----
utils::file.edit(here::here("make.R"))

## Ajout d'un .gitignore pour les projets R -----
rcompendium::add_to_gitignore()

usethis::use_github()
