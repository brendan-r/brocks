# Code required to resolve clashing/overlapping js & css dependencies in
# htmlwidgets.
#
# This code taken from the rmarkdown package v0.9.5.1
# (https://github.com/rstudio/rmarkdown/blob/013d36b13aac24d57fdcc33711abc70d1900d927/R/html_dependencies.R),
# copied here as the functions required are internal, not exported, and thus
# subject to change. Rstudio provide this code under the GPL-3 licence
# (https://www.gnu.org/licenses/gpl-3.0.en.html).
#
# Copyright (C) 2016 Rstudio inc.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.


# rmarkdown internal functions: html dependency resolution ----------------


# flattens an arbitrarily nested list and returns all of the html_dependency
# objects it contains
flatten_html_dependencies <- function(knit_meta) {

  all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)) && is.list(dep)) {
      inner_dependencies <- flatten_html_dependencies(dep)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    }
    else if (is_html_dependency(dep)) {
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  all_dependencies
}

# consolidate dependencies (use latest versions and remove duplicates). this
# routine is the default implementation for version dependency resolution;
# formats may specify their own.
html_dependency_resolver <- function(all_dependencies) {

  dependencies <- htmltools::resolveDependencies(all_dependencies)

  # validate each surviving dependency
  lapply(dependencies, validate_html_dependency)

  # return the consolidated dependencies
  dependencies
}

# validate that the passed list is a correctly formed html_dependency
validate_html_dependency <- function(list) {

  # ensure it's the right class
  if (!is_html_dependency(list))
    stop("passed object is not of class html_dependency", call. = FALSE)

  # validate required fields
  if (is.null(list$name))
    stop("name for html_dependency not provided", call. = FALSE)
  if (is.null(list$version))
    stop("version for html_dependency not provided", call. = FALSE)
  if (is.null(list$src$file))
    stop("path for html_dependency not provided", call. = FALSE)
  if (!file.exists(list$src$file))
    stop("path for html_dependency not found: ", list$src$file, call. = FALSE)

  list
}

# check class of passed list for 'html_dependency'
is_html_dependency <- function(list) {
  inherits(list, "html_dependency")
}
