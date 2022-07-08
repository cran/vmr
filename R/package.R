# This file is part of vmr.
# Copyright (c) 2021 Jean-François Rey <jf.rey.public@gmail.com>
#
# vmr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# vmr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <https://www.gnu.org/licenses/>.

.VagrantCloudOrganization <- "VMR"

# @title Print package information
# @name getInfo
# @description Displays some information about the package
# @importFrom utils packageVersion
getInfo <- function() {
  message("Package: vmr | Virtual Machines for R")
  message("Author: Jean-Fran\u00E7ois Rey <jf.rey.public@gmail.com>")
  message("Version: ", appendLF = FALSE)
  message(utils::packageVersion("vmr"))
  message("License: GPLV3")
  message("'vmr' package is under development")
  message("Be comprehensible and feedback are welcome.")
  message("Do not use virtual machine for production!")
  message("Enjoy!")
}

# @title Check package dependencies
# @name .checkDependencies
# @description Check if tools needed by 'vmr' package is installed
# @return check results
.checkDependencies <- function() {
  res <- vagrantIsInstalled()
  if (!is.null(res) && nzchar(res$vagrant_bin) && grepl("[0-9\\.]+", res$version)) {
    message(paste0("vagrant path: ", res$vagrant_bin))
    message(paste0("vagrant version: ", res$version))
  }
  ## TODO Virtualbox or providers check
}

# @title Things to do at package attach
# @name .onAttach
# @param libname a character string giving the library directory where
#  the package defining the namespace was found.
# @param pkgname a character string giving the name of the package.
# @description Print package information and check dependencies
.onAttach <- function(libname, pkgname) {
  getInfo()
  .checkDependencies()
}

# @title Things to do at package load
# @name .onLoad
# @param libname a character string giving the library directory where
#  the package defining the namespace was found.
# @param pkgname a character string giving the name of the package.
# @description Print package information and check dependencies
.onLoad <- function(libname, pkgname) {
  vmr_env$verbose_mode <- 1

  vagrant_exec <- Sys.which("vagrant")
  if (nzchar(vagrant_exec)) {
    vmr_env$vagrant_bin <- vagrant_exec
  } else {
    vmr_env$vagrant_bin <- "vagrant"
  }
}

# @title manage verbose message
# @description print message if verbose_mode is >= to verbose_val
# @param verbose_val
# @param ... messages to print
printVerbose <- function(verbose_val = 0, ...) {
  if (vmr_env$verbose_mode >= verbose_val) {
    cat(paste0(" vmr ==> ", paste(..., collapse = " "), "\n"))
  }
  return(invisible(NULL))
}
