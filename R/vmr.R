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


#' @encoding UTF-8
#' @title Virtual Machines for R
#' @description Manage, provision and use Virtual Machines pre-configured for R.
#' Develop, test and build package in a clean environment.
#' 'Vagrant' tool and a provider (such as 'Virtualbox') have to be installed.
#' @aliases vmr-package vmr
#'
#' @author Jean-François Rey \email{jf.rey.public@@gmail.com}
#'
#' Maintainer: Jean-François Rey \email{jf.rey.public@@gmail.com}
#' @docType package
#' @name vmr-package
#' @details \tabular{ll}{
#'          Package: \tab vmr\cr
#'          Type: \tab Package\cr
#'          Version: \tab 0.0.2\cr
#'          Date: \tab 2021-02-19\cr
#'          License: \tab GPL (>=3)\cr
#'          }
#' @details This package is a wrap of the [Vagrant](https://www.vagrantup.com/)
#'  tool and more.
#'  It allows to manage, provision and use Virtual Machines pre-configured for R.
#'  It currently only uses 'Virtualbox' (>= 6.1.14) as provider.
#'  Vagrant tool have to be installed too.
#'  Used VMs come from [https://app.vagrantup.com/VMR](https://app.vagrantup.com/VMR)
#'   repository and the sources use to generate them can be found at
#'  [https://gitlab.com/rstuff/vms](https://gitlab.com/rstuff/vms).
#'  See vignettes for the documentations `browseVignette("vmr")`.
#'
#' @keywords vagrant virtual machine provision provider virtualbox
#' @examples
#' \dontrun{
#' library("vmr")
#' }
#' @md
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite prettify parse_json
"_PACKAGE"

vmr_env <- new.env(parent = emptyenv())
vmr_env$verbose_mode <- 1
vmr_env$vagrant_bin <- "vagrant"
