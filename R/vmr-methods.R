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

#' @title Set verbose level
#' @name vmrSetVerbose
#' @description Set verbose level for vmr package functions
#' @details Three verboses mode is available:
#' * "None" : print nothings
#' * "Normal" : print essential
#' * "Full" : print all
#' @param verbose_mode "None", "Normal" or "Full"
#' @return invisible verbose value
#' @export
#' @md
vmrSetVerbose <- function(verbose_mode = "Normal") {
  switch(verbose_mode,
    "None" =  vmr_env$verbose_mode <- 0,
    "Normal" = vmr_env$verbose_mode <- 1,
    "Full" =  vmr_env$verbose_mode <- 2,
    stop('Unknow verbose mode, should be "None", "Normal" or "Full"')
  )

  return(invisible(vmr_env$verbose_mode))
}

#' @title List available boxes from VagrantCloud
#' @name vmrList
#' @description List of available boxes from a VagrantCloud organization account.
#' @details Default usage lists boxes preconfigurated with R
#'  from [VMR organization account](https://app.vagrantup.com/VMR).
#' @param org Vagrant Cloud organization name (default : 'VMR')
#' @return a data.frame with Name, Provider, Version and Description of available boxes
#' @export
#' @md
vmrList <- function(org = .VagrantCloudOrganization) {
  getAvailableBoxes(org)
}

#' @title List all available version of a box
#' @name vmrListBox
#' @description List all versions and providers available of a box.
#' @details List information of a box from VagrantCloud.
#'  Default usage list information of a box preconfigurated with R
#'  from [VMR organization account](https://app.vagrantup.com/VMR).
#' @param box_name the box name
#' @param org Vagrant Cloud organization name (default : 'VMR')
#' @examples
#' \dontrun{
#' # List Boxes
#' boxes <- vmrList()
#' # Box informaion
#' box_info <- vmrListBox(boxes$Name[1])
#' box_info
#' }
#' @return a data.frame with "Name, "Version", "Description", "Provider" and "Date" of the box
#' @export
#' @md
vmrListBox <- function(box_name, org = .VagrantCloudOrganization) {
  getBoxInfo(box_name, org)
}

#' @title Create a __vmr__ environment class
#' @name vmrCreate
#' @description Create a __vmr__ object.
#' @details Create a S3 __vmr__ object (a simple list).
#' The object contains all information needed to configure and manage a
#' __vmr__ environment (a vagrant environment).
#'
#' A __vmr__ environment need mostly a box _name_ and a _provider_.
#' The environment is attached to the current working directory.
#'
#' __vmr__ object main attributs:
#' * __path__: working directory
#' * __org__: Vagrant cloud user/organization name 'VMR'
#' * __box__: the box name
#' * __version__: the box version
#' * __provider__: the provider
#' * __provider_options__: the provider options (see [[getProviderOptions()]])
#' * __vagrantName__: Vagrant environment name
#' * __ID__ <- Vagrant environment ID
#' * __synced_folder__: a list with source and destination
#' * __ssh_user__: the ssh user
#' * __ssh_pwd__: the ssh user password
#' * __ssh_port__: the ssh port
#' * __ssh_private_key_path__: the private ssh key path
#' @param name a box name
#' @param provider the box provider (default: "virtualbox")
#' @param version the box version (default : "latest")
#' @param provider.options provider options (call [[getProviderOptions()]] to get values)
#' @examples
#' \dontrun{
#' # List boxes available
#' boxes <- vmrList()
#' # Create a vmr object
#' vmr <- vmrCreate(boxes$Name[1])
#'
#' # to customize the guest machine for virtualbox
#' virtualboxOpts <- getProviderOptions(provider = "virtualbox")
#' virtualboxOpts$modifyvm <- list(cpus = 4, memory = 4096)
#' virtualboxOpts$name <- "My VM Cool Name"
#' # To specify a provider and version
#' vmr <- vmrCreate(
#'   name = boxes$Name[1],
#'   provider = "virtualbox",
#'   version = boxes$Version[1],
#'   provider.options = virtualboxOpts
#' )
#' }
#' @return a __vmr__ object (see details)
#' @export
#' @md
vmrCreate <- function(name, provider = "virtualbox", version = "latest", provider.options = virtualboxOptions(FALSE)) {
  printVerbose(1, "Creating vmr environment...")
  vmr <- list()
  attr(vmr, "class") <- "vmr"
  vmr$path <- normalizePath(getwd())

  if (!identical(grep("/", name), integer(0))) {
    pos <- regexpr("/", name)
    vmr$org <- substr(name, 1, pos - 1)
    vmr$box <- substr(name, pos + 1, nchar(name))
  } else {
    vmr$org <- .VagrantCloudOrganization
    vmr$box <- name
  }
  vmr$version <- version
  vmr$provider <- provider
  vmr$provider_options <- provider.options

  vmr$vagrantName <- paste0("vmr-", sub("/", "-", name), "-", provider)
  vmr$ID <- vagrantGetID(vmr$vagrantName, vmr$path)

  vmr$synced_folder <- list()
  vmr$synced_folder$source <- ""
  vmr$synced_folder$destination <- ""

  vmr$ssh_user <- "vagrant"
  vmr$ssh_pwd <- "vagrant"
  vmr$ssh_port <- ""
  vmr$ssh_private_key_path <- ""

  return(vmr)
}

#' @title List provider options
#' @name getProviderOptions
#' @description List a provider available options.
#' @details It return a list of options name and value for a specific provider.
#' To get the help page do ```?<provider_name>Options()```, for example [[virtualboxOptions()]].
#' @param provider a provider name
#' @param details if TRUE print options, otherwise return default options
#' @return a list of options
#' @examples
#' vbOpts <- getProviderOptions(provider = "virtualbox")
#' print(vbOpts)
#' @export
#' @md
getProviderOptions <- function(provider = "virtualbox", details = FALSE) {
  get(paste0(provider, "Options"))(details)
}

#' @title Print __vmr__ object information
#' @name print.vmr
#' @description print information from a __vmr__ object
#' @param x a __vmr__ object
#' @param ... optional print arguments
#' @return the __vmr__ object (via invisible(x))
#' @export
#' @md
print.vmr <- function(x, ...) {
  cat("### vmr environment information ###\n")
  cat("Organization:", x$org, "\n")
  cat("Name:", x$box, "\n")
  cat("Version:", x$version, "\n")
  if (x$ID != "") cat("Machine ID:", x$ID, "\n")
  cat("Machine instance:", x$vagrantName, "\n")
  cat("Machine environment directory:", x$path, "\n")
  if (x$synced_folder$source != "" && x$synced_folder$destination != "") {
    cat(paste0("Synced folder: '", x$synced_folder$source, "' to Guest '", x$synced_folder$destination, "'\n"))
  } else {
    cat("Synced folder: Disable\n")
  }
  cat("ssh user:", x$ssh_user, "\n")
  cat("ssh password:", x$ssh_pwd, "\n")
  cat("ssh port:", x$ssh_port, "\n")
  if (x$ssh_private_key_path != "") cat("ssh private key path:", x$ssh_private_key_path, "\n")
  cat("Provider:", x$provider, "\n")
  if (x$provider == "virtualbox") {
    virtualboxPrintOptions(x$provider_options)
  } else {
    cat(x$provider, "unknow or not implemented provider\n")
  }

  return(invisible(x))
}

#' @title Summary __vmr__ object information
#' @name summary.vmr
#' @description print information from a __vmr__ object
#' @param object a __vmr__ object
#' @param ... optional print arguments
#' @return the __vmr__ object (via invisible(x))
#' @export
#' @md
summary.vmr <- function(object, ...) {
  print.vmr(object)
}

# @title check a vmr object "class"
# @description check minimal variables needed in a 'vmr' object
# @param vmr a vmr object
# @return TRUE if OK
.checkMinimalVMR <- function(vmr) {
  if (attr(vmr, "class") != "vmr") stop("arguments is not a vmr list object")
  if (is.null(vmr$org) || vmr$org == "") stop("vmr$org undefined")
  if (is.null(vmr$box) || vmr$box == "") stop("vmr$box undefined")
  if (is.null(vmr$provider) || vmr$provider == "") stop("vmr$provider undefined")
  TRUE
}

# .checkvmrEnv <- function(){
#  if (!file.exists("Vagrantfile")) stop("Wrong vmr environment\n",
#                                        "Vagrantfile template can't be find here\n",
#                                        getwd(),"\n")
#  global_status <- vagrantGlobalStatus()
#
# }

#' @title Load a __vmr__ environment containing a Vagrant file
#' @name vmrLoad
#' @description Load a __vmr__ environment containing a VagrantFile
#'  and create a __vmr__ object (see [[vmrCreate()]] for object details).
#' @details It read a Vagrant file template with __vmr__ compatible parameters.
#' It's an experimental Vagrant file reading, some parameters may not be loaded.
#' @param dir the __vmr__ environment directory (default: "./")
#' @param vagrantfileName a Vagrantfile name (default: "Vagrantfile")
#' @examples
#' \dontrun{
#' # load the Vagrantfile in the current directory
#' vmr <- vmrLoad(getwd())
#' }
#' @return a __vmr__ object
#' @export
#' @md
vmrLoad <- function(dir = "./", vagrantfileName = "Vagrantfile") {
  printVerbose(2, "Experimental Vagrantfile reading... some Options may not be loaded")
  path <- normalizePath(dir)
  printVerbose(2, paste0("move to ", path))
  oldpath <- getwd()
  setwd(path)
  on.exit(setwd(oldpath))

  vagrant_file <- normalizePath(file.path(path, vagrantfileName))

  if (!file.exists(vagrant_file)) stop("can't find ", vagrant_file)

  vmr <- list()
  attr(vmr, "class") <- "vmr"
  vmr$path <- path

  printVerbose(1, paste0("Reading ", vagrant_file))

  vagrant_data <- readLines(vagrant_file)

  extract <- function(text_vector, pattern) {
    text <- text_vector[grep(pattern, text_vector)]
    pos_value <- regexpr('\"[^\"]+\"', text)
    substr(text, pos_value + 1, pos_value + attr(pos_value, "match.length") - 2)
  }

  extractInt <- function(text_vector, pattern) {
    text <- text_vector[grep(pattern, text_vector)]
    pos_value <- regexpr("= [0-9]+", text)
    as.integer(substr(text, pos_value + 2, pos_value + attr(pos_value, "match.length")))
  }

  box <- strsplit(extract(vagrant_data, "config.vm.box "), split = "/")[[1]]
  vmr$org <- box[1]
  vmr$box <- box[2]
  vmr$version <- extract(vagrant_data, "config.vm.box_version")
  if (identical(character(0), vmr$version) || nchar(vmr$version) == 0) vmr$version <- "latest"


  vmr$vagrantName <- extract(vagrant_data, "config.vm.define")

  vmr$provider <- extract(vagrant_data, "config.vm.provider")
  if (identical(vmr$provider, character(0))) {
    vmr$provider <- ""
    vmr$provider_options <- NULL
  } else {
    vmr$provider_options <- get(paste0(vmr$provider, "ReadOptions"))(vagrant_data)
  }

  vmr$ID <- vagrantGetID(vmr$vagrantName, vmr$path)

  vmr$synced_folder <- list()
  vmr$synced_folder$source <- ""
  vmr$synced_folder$destination <- ""
  sync_f <- vagrant_data[grep("config.vm.synced_folder", vagrant_data)]
  if (length(sync_f) > 1) {
    temp <- strsplit(sync_f[1], split = " +")[[1]]
    vmr$synced_folder$source <- substr(temp[2], 2, nchar(temp[2]) - 2)
    vmr$synced_folder$destination <- substr(temp[3], 2, nchar(temp[3]) - 1)
  }

  vmr$ssh_user <- extract(vagrant_data, "config.ssh.username")
  if (identical(character(0), vmr$ssh_user)) vmr$ssh_user <- "vagrant"
  vmr$ssh_pwd <- extract(vagrant_data, "config.ssh.password")
  if (identical(character(0), vmr$ssh_pwd)) vmr$ssh_pwd <- "vagrant"
  vmr$ssh_port <- extractInt(vagrant_data, "config.ssh.port")
  if (identical(integer(0), vmr$ssh_port)) vmr$ssh_port <- ""
  vmr$ssh_private_key_path <- extract(vagrant_data, "config.ssh.private_key_path")
  if (identical(character(0), vmr$ssh_private_key_path)) vmr$ssh_private_key_path <- ""

  return(vmr)
}

#' @title Initialize the __vmr__ environment
#' @name vmrInitEnv
#' @description Create __vmr__ environment in the current directory.
#' Set configuration into a template file name "Vagrantfile"
#'  and download the box if needed.
#' @details The __vmr__ environment consist of a directory (the working directory)
#' and a template file name _Vagrantfile_.
#' If the box is not present in localhost it will be download.
#' @param vmr a __vmr__ object
#' @param force.vagrantfile if TRUE force to overwrite environment configuration (default FALSE)
#' @param force.download if TRUE force to download the box, otherwise do not (default FALSE).
#' @examples
#' \dontrun{
#' boxes <- vmrList()
#' vmr <- vmrCreate(boxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' }
#' @return the __vmr__ object
#' @export
vmrInitEnv <- function(vmr, force.vagrantfile = FALSE, force.download = FALSE) {
  printVerbose(1, "Initialize vmr environment")
  .checkMinimalVMR(vmr)

  oldpath <- getwd()
  setwd(vmr$path)
  on.exit(setwd(oldpath))

  if (vmrIsRunning()) stop("The virtual guest is running. run vmrStop() and recall this function")

  writeVagrantFile(vmr, force.vagrantfile)

  ## TODO may update box vagrantBoxUpdate
  if (identical(grep("/", vmr$box), integer(0))) {
    box <- paste0(vmr$org, "/", vmr$box)
  } else {
    box <- vmr$box
  }
  vagrantBoxAdd(box, vmr$version, vmr$provider, force.download)

  printVerbose(1, "Now run vmr<Function>() into", vmr$path, "directory")

  return(invisible(vmr))
}

# @title Create a Vagrant file from a __vmr__ object
# @description Create a Vagrant file template from a __vmr__ object.
# @param vmr a __vmr__ object
# @param force if TRUE force to overwrite the template file
# @return path to the Vagrantfile
# @md
writeVagrantFile <- function(vmr, force = FALSE) {
  if (file.exists("Vagrantfile") && isFALSE(force)) {
    stop("Vagrantfile already exists in ", getwd(), "\n use argument force.vagrantfile=TRUE to override it.")
  }

  printVerbose(2, "Creating Vagrantfile template in ", getwd())

  tryCatch(
    {
      sink("Vagrantfile")
      cat(paste0(
        # if (isTRUE(vmr$r$update_packages) || length(vmr$r$install_packages) != 0) .provisionRscript(vmr),
        'Vagrant.configure("2") do |config|\n',

        if (!identical(grep("/", vmr$box), integer(0))) {
          paste0('\tconfig.vm.box = "', vmr$box, '"\n')
        } else {
          paste0('\tconfig.vm.box = "', vmr$org, "/", vmr$box, '"\n')
        },

        if (vmr$version != "" && vmr$version != "latest") paste0('\tconfig.vm.box_version = "', vmr$version, '"\n'),

        if (vmr$synced_folder$source != "" && vmr$synced_folder$destination != "") {
          paste0('\tconfig.vm.synced_folder "', vmr$synced_folder$source, '", "', vmr$synced_folder$destination, '"\n')
        },
        '\tconfig.vm.synced_folder ".", "/vagrant", disabled: true\n',

        paste0('\tconfig.vm.define "', vmr$vagrantName, '" do |d|\n'),
        "\tend\n",

        # SSH configuration
        '\tconfig.vm.communicator = "ssh"\n',
        if (vmr$ssh_user != "vagrant" || vmr$ssh_pwd != "vagrant") {
          paste0(
            '\tconfig.ssh.username ="', vmr$ssh_user, '"\n',
            '\tconfig.ssh.password ="', vmr$ssh_pwd, '"\n',
            "\tconfig.ssh.keep_alive = true\n",
            "\tconfig.ssh.insert_key = true\n"
          )
        } else {
          paste0("\tconfig.ssh.insert_key = false\n")
        },

        if (vmr$ssh_port != "") paste0("\tconfig.ssh.port = ", vmr$ssh_port, "\n"),
        if (vmr$ssh_private_key_path != "") paste0('\tconfig.ssh.private_key_path = "', vmr$ssh_private_key_path, '"\n'),

        # Provisioning
        # if (isTRUE(vmr$r$update_packages) || length(vmr$r$install_packages) != 0) '\tconfig.vm.provision "shell", inline: $rscript\n',

        # Provider configuration (default virtualbox)
        # for providers options '\tconfig.vm.provider = "',vmr$provider,'"\n',
        if (!is.null(vmr$provider) && nchar(vmr$provider) > 0) {
          if (vmr$provider == "virtualbox" && !is.null(vmr$provider_options)) virtualBoxVagrantFile(vmr$provider_options)
        },
        "end\n"
      ))
      sink()
    },
    error = function(cond) {
      sink()
      warning("Error writing Vagrantfile template\n", cond, immediate. = TRUE)
    },
    warning = function(cond) {
      sink()
      warning("May have some error writing Vagrantfile template\n", cond, immediate. = TRUE)
    },
    finally = {
    }
  )

  return(normalizePath("Vagrantfile"))
}


#' @title Get guest machine information
#' @name vmrInfo
#' @description Get guest machine information.
#' Print OS, R, R-devel and R packages information.
#' Still in development.
#' @examples
#' \dontrun{
#' boxes <- vmrList()
#' vmr <- vmrCreate(boxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' vmrInfo()
#' }
#' @return \code{NULL}
#' @export
#' @md
vmrInfo <- function() {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")
  # TODO make a clean print

  cat("#### R Version ####\n")
  vagrantSSHCommand("R --version")
  cat("#### Rdevel Version ####\n")
  vagrantSSHCommand("if Rdevel --version &> /dev/null ; then Rdevel --version ; fi")
  cat("#### OS informations ####\n")
  vagrantSSHCommand("Rscript -e \"Sys.info()\"")
  # cat("#### R packages installed ####\n")
  # vagrantSSHCommand("Rscript -e \"installed.packages()\"")

  return(NULL)
}

#' @title Update a __vmr__ environment.
#' @name vmrUpdateEnvVersion
#' @description Force to use the latest box version of the current __vmr__ environment.
#' @details Put __vmr__ object version to latest and update the Vagrant File template.
#' Download the new box version if needed.
#' @param vmr a __vmr__ object
#' @examples
#' \dontrun{
#' boxes <- vmrList()
#' vmr <- vmrCreate(boxes$Name[1], version = "oldone")
#' vmr <- vmrInitEnv(vmr)
#'
#' # update to latest
#' vmr <- vmrUpdateEnvVersion(vmr)
#' vmrStart()
#' }
#' @return a __vmr__ object
#' @export
#' @md
vmrUpdateEnvVersion <- function(vmr) {
  if (vmrIsRunning()) vmrStop()

  printVerbose(1, "Set environment to use the latest box version")
  vmr$version <- "latest"
  vmr <- vmrInitEnv(vmr, force.vagrantfile = TRUE)

  vmrLocalBoxUpdate()

  return(vmr)
}

#' @title Remove all resources created in a __vmr__ environment
#' @name vmrDestroy
#' @description Remove all resources created by [vmrStart()]
#' @details Will by default remove all resources created from the current __vmr__ environment.
#'  By specifying the _id_ any environment with this _id_ will be remove.
#' @param id a __vmr__ environment id (default : "" id from the current environment)
#' @param force if TRUE force to remove
#' @return the vagrant environment id
#' @examples
#' \dontrun{
#' vmrStop()
#' vmrDestroy()
#' }
#' @export
#' @md
vmrDestroy <- function(id = "", force = FALSE) {
  printVerbose(1, "Checking configuration...")
  vms <- vagrantGlobalStatus()
  if (id != "") {
    vm <- vms[which(vms$id == id), ]
  }
  else {
    vm <- vms[which(vms$directory == normalizePath("./")), ]
  }

  x <- "y"
  if (!isTRUE(force)) {
    x <- readline(paste0("Are you sure you want to destroy the ", vm$name, " VM? [y/N] "))
  }

  if (x == "y" || x == "Y") {
    printVerbose(1, paste0("The VM ", vm$name, " will be destroyed"))
    args <- c("destroy", "--force", vm$id)
    vagrantExec(args)
  }
  else {
    printVerbose(1, paste0("The VM ", vm$name, " will not be destroyed"))
  }

  if (id == "") {
    x <- "y"
    if (!isTRUE(force)) {
      x <- readline(paste0(
        "Do you want to remove Vagrantfile template ",
        "and .vagrant directory ",
        "(clean vmr environment) ? [y/N] "
      ))
    }

    if (x == "y" || x == "Y") {
      file.remove(paste0(getwd(), "/Vagrantfile"))
      unlink(paste0(getwd(), "/.vagrant"), recursive = TRUE)
    }
  }

  return(invisible(vm$id))
}

#' @title Mount a host directory to guest
#' @name vmrMountDir
#' @description Mount a host directory to the guest machine.
#' @details If the option of mounting a directory is available
#' in the guest provider, it will mount _src_ to _destination_ directory.
#' Calling with no arguments will disable this option.
#' @param vmr a __vmr__ object
#' @param src a host directory
#' @param dest a destination guest directory
#' @examples
#' \dontrun{
#' boxes <- vmrList()
#' vmr <- vmrCreate(boxes$Name[1])
#' vmr <- vmrMountDir(vmr, src = getwd(), dest = "/vmr")
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' }
#' @return a __vmr__ object
#' @export
#' @md
vmrMountDir <- function(vmr, src = "", dest = "") {
  printVerbose(1, "Mount ", src, " to", dest)
  printVerbose(1, "This option may not be available to all boxes")
  printVerbose(1, "Call vmrInitEnv() and restart the environment after that to take effect")

  vmr$synced_folder <- list()
  vmr$synced_folder$source <- src
  vmr$synced_folder$destination <- dest

  return(vmr)
}

#' @title List downloaded boxes
#' @name vmrLocalBoxList
#' @description List all boxes downloaded in localhost
#' @examples
#' \dontrun{
#' localBoxes <- vmrLocalBoxList()
#' print(localBoxes)
#' }
#' @return a data.frame with boxes Name, Providers and Version
#' @export
#' @md
vmrLocalBoxList <- function() {
  box_list <- vagrantBoxList()
  printVerbose(2, "This is all boxes available in your system: ")
  # printVerbose(2, box_list)
  return(box_list)
}

#' @title Update local box version
#' @name vmrLocalBoxUpdate
#' @description Download the latest version of the box use in the current __vmr__ environment.
#' @return execution code or message
#' @export
#' @md
vmrLocalBoxUpdate <- function() {
  printVerbose(2, "Download latest version of the current environment box")
  invisible(vagrantBoxUpdate())
}

#' @title Remove a box from localhost
#' @name vmrLocalBoxRemove
#' @description Remove a specific box from localhost.
#' @param name the box name
#' @param provider the box provider (default: first provider found)
#' @param version the box version (default: version available)
#' @param force if TRUE force to remove
#' @examples
#' \dontrun{
#' lboxes <- vmrLocalBoxList()
#' vmrLocalBoxRemove(lboxes$Name[[1]])
#' # if multiple providers and versions
#' vmrLocalBoxRemove(lboxes$Name[[1]], lboxes$Provider[[1]], lboxes$Version[[1]])
#' }
#' @return execution code or message
#' @export
#' @md
vmrLocalBoxRemove <- function(name, provider = "", version = "", force = FALSE) {
  printVerbose(2, paste0("Remove the box: ", name))
  if (provider != "") printVerbose(1, "Provider: ", provider)
  if (version != "") printVerbose(1, "Version: ", version)
  invisible(vagrantBoxRemove(name, provider, version, force))
}

#' @title Remove old installed boxes
#' @name vmrLocalBoxPrune
#' @description Removes old versions of installed boxes.
#' @return a data.frame of still installed boxes (Name, Poviders and Version)
#' @examples
#' \dontrun{
#' vmrLocalBoxPrune()
#' }
#' @export
#' @md
vmrLocalBoxPrune <- function() {
  printVerbose(2, "Removes old boxes version")
  vagrantBoxPrune()
}

# @export
# vmrPackage <- function() {
#  print("soon")
# }

#' @title Take a snapshot of the guest machine
#' @name vmrTakeSnapshot
#' @description Take a snapshot of the guest machine.
#' @param snap_name the name given to the snapshot
#' @return the snapshot name (invisible)
#' @examples
#' \dontrun{
#' vmrTakeSnapshot("my snapshot")
#' }
#' @export
#' @md
vmrTakeSnapshot <- function(snap_name) {
  printVerbose(1, "Taking a snapshot ", snap_name)
  vagrantSnapshot("save", snap_name)

  return(invisible(snap_name))
}

#' @title Restore a snapshot of the guest machine
#' @name vmrRestoreSnapshot
#' @description Restore a snapshot of the guest machine.
#' @param snap_name the snapshot name
#' @return the snapshot name
#' @examples
#' \dontrun{
#' vmrRestoreSnapshot("my snapshot")
#' }
#' @export
#' @md
vmrRestoreSnapshot <- function(snap_name) {
  printVerbose(1, "Restore snapshot: ", snap_name)
  vagrantSnapshot("restore", snap_name)

  return(invisible(snap_name))
}

#' @title List snapshot of the guest machine
#' @name vmrListSnapshot
#' @description Print all snapshot name of the guest machine
#' @return \code{NULL}
#' @export
#' @md
vmrListSnapshot <- function() {
  printVerbose(1, "Listing snapshot")
  vagrantSnapshot("list")
  ## TODO return the list name
  return(NULL)
}

#' @title remove a snapshot of the guest machine
#' @name vmrRemoveSnapshot
#' @description remove a snapshot of the guest machine
#' @param snap_name the snapshot name
#' @return \code{NULL}
#' @export
#' @md
vmrRemoveSnapshot <- function(snap_name) {
  printVerbose(1, "Delete a snapshot: ", snap_name)
  vagrantSnapshot("delete", snap_name)
  ## TODO return the list name
  return(NULL)
}


#' @title Save state and stop guest machine
#' @name vmrSuspend
#' @description Save the guest machine and stop it.
#' @details In the current __vmr__ environment, save the state of the guest machine
#'  and stop it.
#' @return \code{NULL}
#' @export
#' @md
vmrSuspend <- function() {
  printVerbose(1, "Suspend environment")
  args <- "suspend"
  invisible(vagrantExec(args))
}

#' @title Resume a stopped guest machine
#' @name vmrResume
#' @description Resume a stopped guest machine.
#' @details In the current __vmr__ environment, start a stopped ([[vmrSuspend()]]) guest machine.
#' @return \code{NULL}
#' @export
#' @md
vmrResume <- function() {
  printVerbose(1, "Resume Environment")
  args <- "resume"
  invisible(vagrantExec(args))
  return(NULL)
}

#' @title Get the state of the guest machine
#' @name vmrStatus
#' @description Print guest machine state in the current __vmr__ environment.
#' @return a data.frame with Name, Provider and state
#' @export
#' @md
vmrStatus <- function() {
  printVerbose(1, "Getting status")
  st <- vagrantStatus()

  printVerbose(1, paste0(
    "The machine ", st$vagrantName, " provided by:\n",
    st$provider, " is ", st$state
  ))
  return(st)
}

#' @title Download a Box
#' @name vmrBoxDownload
#' @description Download a box from a __vmr__ object.
#' @param vmr a __vmr__ object
#' @return a __vmr__ object
#' @export
#' @md
vmrBoxDownload <- function(vmr) {
  .checkMinimalVMR(vmr)

  printVerbose(
    1,
    paste0(
      "Will download the box ", paste0(vmr$org, "/", vmr$box),
      " ", vmr$version,
      " for", vmr$provider, "provider."
    )
  )
  res <- vagrantBoxAdd(name = paste0(vmr$org, "/", vmr$box), version = vmr$version, provider = vmr$provider)

  return(invisible(vmr))
}

#' @title Start a __vmr__ environment
#' @name vmrStart
#' @description Start a guest virtual machine using the current __vmr__ environment
#'  (directory and Vagrantfile template)
#' @examples
#' \dontrun{
#' lboxes <- vmrList()
#' vmr <- vmrCreate(lboxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' vmrStop()
#' }
#' @return the vmr environment unique id
#' @export
#' @md
vmrStart <- function() {
  if (vmrIsRunning()) stop("Virtual machine already running")
  printVerbose(1, "Starting virtual machine")

  vagrantUp()

  id <- vagrantGetID(vagrantStatus()$vagrantName, normalizePath(getwd()))

  return(id)
}

#' @title Stop a __vmr__ environement
#' @name vmrStop
#' @description Stop a guest virtual machine in the current __vmr__ environment.
#' @param force if TRUE force to stop (powerOff), otherwise FALSE clean shutdown
#' @return \code{NULL}
#' @examples
#' \dontrun{
#' lboxes <- vmrList()
#' vmr <- vmrCreate(lboxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' vmrStop()
#' }
#' @export
#' @md
vmrStop <- function(force = FALSE) {
  if (vmrIsRunning()) {
    printVerbose(1, "Stoping virtual machine")
    vagrantHalt(force)
  } else {
    stop("virtual machine is not running")
  }

  return(NULL)
}

#' @title Is __vmr__ environment running
#' @name vmrIsRunning
#' @description Check if a guest machine in a __vmr__ environment is running
#' @examples
#' \dontrun{
#' lboxes <- vmrList()
#' vmr <- vmrCreate(lboxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' vmrIsRunning()
#' vmrStop()
#' vmrIsRunning()
#' }
#' @return TRUE if running, otherwise FALSE
#' @export
#' @md
vmrIsRunning <- function() {
  if (file.exists("Vagrantfile")) {
    st <- vagrantStatus()
    if (length(st) != 0) {
      return(st$state == "running")
    }
  }
  return(FALSE)
}

#' @title Update R packages installed
#' @name vmrUpdatePackages
#' @description Updates R packages installed in the guest machine.
#' @details Will perform a [update.packages()] in the guest machine
#'  of the current __vmr__ environment.
#' @return NULL
#' @examples
#' \dontrun{
#' lboxes <- vmrList()
#' vmr <- vmrCreate(lboxes$Name[1])
#' vmr <- vmrInitEnv(vmr)
#' vmrStart()
#' vmrUpdatePackages()
#' }
#' @export
#' @md
vmrUpdatePackages <- function() {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  Rcmd <- "Rscript -e \"update.packages(lib.loc=.libPaths()[1],
                      repo='https://cloud.r-project.org',
                      ask=FALSE)\""

  vagrantSSHCommand(Rcmd)
  return(NULL)
}

#' @title Install R packages into guest machine
#' @name vmrInstallPackages
#' @description Install a list of R packages into the guest machine
#'  of the current __vmr__ environment.
#' @param pkgs list of R packages
#' @examples
#' \dontrun{
#' vmrInstallPackages(c("vmr"))
#' }
#' @return installed packages vector
#' @export
#' @md
vmrInstallPackages <- function(pkgs = c()) {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  pkgs_escape <- unlist(lapply(pkgs, FUN = function(p) {
    paste0("'", p, "'")
  }))
  Rcmd <- paste0("Rscript -e \"install.packages(c(", paste(pkgs_escape, collapse = ","), "),
                      repo='https://cloud.r-project.org',
                      ask=FALSE)\"")
  cat("Installing R packages :\n")
  cat(paste0("c(", paste(pkgs_escape, collapse = ","), ")\n"))
  vagrantSSHCommand(Rcmd)
  return(invisible(pkgs))
}

#' @title Send files and/or directories to guest machine
#' @name vmrSend
#' @description Send files and/or directories to the guest machine
#'  in the current __vmr__ environment.
#' They are upload into ~/vmr/ directory.
#' @param elt list of files and directories
#' @return 0 if OK, message otherwise
#' @examples
#' \dontrun{
#' vmrSend(c("myfile"))
#' }
#' @export
#' @md
vmrSend <- function(elt = c()) {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  res <- lapply(elt, FUN = vagrantUpload)
  lapply(seq_along(res), FUN = function(i) {
    if (res[[i]] == 0) {
      message(elt[i], " uploaded")
    } else {
      message(res[[i]])
    }
  })

  return(invisible(res))
}

# TODO implement vmrGet to download form guest files/directories

#' @title Execute R methods into guest machine
#' @name vmrExec
#' @description Run R method into guest machine.
#' @details call Rscript -e "cmd" into the guest machine from
#'  the current __vmr__ environment.
#'  Command are independents and do not keep memory of past commands.
#' @param cmd list of R command
#' @examples
#' \dontrun{
#' cmd <- c("Sys.info()", 'print("Hello World!")')
#' vmrExec(cmd)
#' }
#' @return \code{NULL}
#' @export
#' @md
vmrExec <- function(cmd = c()) {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  Rcmds <- lapply(cmd, FUN = function(c) {
    paste0("Rscript -e \"", c, "\"")
  })

  res <- lapply(Rcmds, vagrantSSHCommand)
  # return(invisible(res))
  return(NULL)
}

#' @title Configure ssh
#' @name vmrConfigSSH
#' @description Configure ssh credential.
#' @details by default __vmr__ use vagrant as user/password and insecure key
#' for ssh connection. This behavior can be change here, by setting an another
#' user and/or ssh keys. Calling with no arguments will disable this option.
#' Be careful, ssh using only password may result of _vmr_ functions bugs.
#' @param vmr a __vmr__ object
#' @param ssh_user the ssh user (default 'vagrant')
#' @param ssh_pwd the ssh pwd if any (default 'vagrant')
#' @param port the ssh port (default empty)
#' @param ssh_private_key_path path to the private ssh key to use (default empty, use insecure vagrant key)
#' @examples
#' \dontrun{
#' vmr <- vmrConfigSSH(ssh_user = "John", ssh_pwd = "d0e", port = "22")
#' vmr <- vmrConfigSSH(ssh_user = "John", private_key_path = "/path/to/private/key/")
#' }
#' @return an updated __vmr__ object
#' @export
#' @md
vmrConfigSSH <- function(vmr, ssh_user = "vagrant", ssh_pwd = "vagrant", port = "", ssh_private_key_path = "") {
  vmr$ssh_user <- ssh_user
  vmr$ssh_pwd <- ssh_pwd
  vmr$ssh_port <- port
  vmr$ssh_private_key_path <- ssh_private_key_path

  return(vmr)
}

#' @title Open a ssh connection to guest machine
#' @name vmrConnect
#' @description Open a ssh connection to guest machine
#' @details To open a ssh connection 'ssh' package have to be installed.
#' @param vmr a __vmr__ object
#' @return a __vmr__ object
# @import ssh
#' @export
#' @md
vmrConnect <- function(vmr) {
  if (!requireNamespace("ssh", quietly = TRUE)) {
    stop("Package \"ssh\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  ssh_conf <- vagrantSSHConfig()
  printVerbose(
    2, "Try to connect...\n",
    "User: ", vmr$ssh_user, "\n",
    "Passwd: ", vmr$ssh_pwd, "\n",
    "Host: ", ssh_conf$hostname, "\n",
    "Port: ", ssh_conf$port, "\n",
    "Keyfile: ", ssh_conf$keyfile, "\n"
  )
  session <- ssh::ssh_connect(
    host = paste0(vmr$ssh_user, "@", ssh_conf$hostname, ":", ssh_conf$port),
    keyfile = ssh_conf$keyfile,
    passwd = vmr$ssh_pwd
  )
  ssh::ssh_exec_wait(session, command = c("whoami", "R --version"))

  vmr$ssh_session <- session

  return(vmr)
}

#' @title Disconnect ssh connection to guest machine
#' @name vmrDisconnect
#' @description Close a ssh connection to the guest machine
#' @details 'ssh' package need to be installed.
#' @param vmr a __vmr__ object
#' @return a __vmr__ object
#' @export
#' @md
vmrDisconnect <- function(vmr) {
  if (!requireNamespace("ssh", quietly = TRUE)) {
    stop("Package \"ssh\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  printVerbose(2, "Disconnect ssh...")
  ssh::ssh_disconnect(vmr$ssh_session)
  vmr$ssh_session <- NULL

  return(vmr)
}

#' @title Provision a __vmr__ environment
#' @name vmrProvision
#' @description Provision a __vmr__ environment.
#' @details Upload 'elts' files and/or directories to the guest machine 'dest'
#' from the current __vmr__ environment.
#' And finaly run shell commands 'cmd' in the guest machine.
#' @param cmd list of shell commands
#' @param elts list of files and/or directories
#' @param dest destination of elts (default HOME/vmr)
#' @return \code{NULL}
#' @export
vmrProvision <- function(cmd = c(), elts = c(), dest = "") {
  if (!vmrIsRunning()) stop("Virtual Machine is not running.\n
                           use vmrStart() or vmrResume()\n")

  lapply(elts, FUN = function(fd) {
    vagrantUpload(fd, dest)
  })

  lapply(cmd, FUN = function(c) {
    vagrantSSHCommand(cmd)
  })

  return(invisible(""))
}

#' @title Perform a package check on guest
#' @name vmrPackageCheck
#' @description Perform a package check into the guest
#' @details upload the package and run [devtools::check()]
#'  into the guest machine. (check available in $HOME/vmr/package/pkg).  
#'  Checking a directory with multiple files may slower upload, prefer tar.gz file
#' @param pkg a package directory or a tar.gz file
#' @return \code{NULL}
#' @examples
#' \dontrun{
#' vmrPackageCheck("vmr_package.tar.gz")
#' }
#' @export
#' @md
vmrPackageCheck <- function(pkg = "./") {
  printVerbose(1, "Will perform package check")

  if (dir.exists(normalizePath(pkg))) {
    to_dir <- basename(normalizePath(pkg))
    to_pkg <- ""
    Rcmds <- paste0(
      "Rscript -e \"library(devtools);check(normalizePath('vmr/package/",
      to_dir, "/", to_pkg, "'), check_dir=normalizePath('vmr/package/", to_dir, "'))\""
    )
  }
  else {
    to_dir <- basename(dirname(normalizePath(pkg)))
    to_pkg <- basename(pkg)
    Rcmds <- paste0(
      "Rscript -e \"library(devtools);check_built(normalizePath('vmr/package/",
      to_dir, "/", to_pkg, "'), check_dir=normalizePath('vmr/package/", to_dir, "'))\""
    )
  }

  printVerbose(2, paste0("Cleaning Guest \"vmr/package/", to_dir, "\""))
  vagrantSSHCommand(paste0("rm -rf vmr/package/", to_dir, "/* 2>&1 > /dev/null"))
  vagrantSSHCommand(paste0("mkdir -p vmr/package/", to_dir, " 2>&1 > /dev/null"))

  vmrProvision(cmd = Rcmds, elts = pkg, dest = paste0("vmr/package/", to_dir, "/"))
  return(NULL)
}

#' @title Build a package in the guest machine
#' @name vmrPackageBuild
#' @description Build a package bundle or binary into the guest machine.
#' @details upload the package and run [devtools::build()]
#' (build available in $HOME/vmr/package/pkg) in the current __vmr__ environment.
#' @param pkg a package directory or a tar.gz file
#' @param binary if TRUE build binary package otherwise FALSE
#' @return \code{NULL}
#' @export
#' @md
vmrPackageBuild <- function(pkg = "./", binary = FALSE) {
  printVerbose(1, "Will perform package build")

  if (dir.exists(normalizePath(pkg))) {
    to_dir <- basename(normalizePath(pkg))
    to_pkg <- ""
  }
  else {
    to_dir <- basename(dirname(normalizePath(pkg)))
    to_pkg <- basename(pkg)
  }

  printVerbose(2, paste0("Cleaning Guest \"vmr/package/", to_dir, "\""))
  vagrantSSHCommand(paste0("rm -rf vmr/package/", to_dir, "/* 2>&1 > /dev/null"))
  vagrantSSHCommand(paste0("mkdir -p vmr/package/", to_dir, " 2>&1 > /dev/null"))
  Rcmds <- paste0(
    "Rscript -e \"library(devtools);build(normalizePath('vmr/package/",
    to_dir, "/", to_pkg, "'), binary=", binary, ")\""
  )
  vmrProvision(cmd = Rcmds, elts = pkg, dest = paste0("vmr/package/", to_dir, "/"))
  return(NULL)
  # TODO return path to package binary in guest
}

#' @title Test a package into a guest machine
#' @name vmrPackageTest
#' @description Test a package into a guest machine
#' @details Perform a package check into the guest machine
#'  of the current __vmr__ environment using [devtools::test()].
#'  (tests are available in $HOME/vmr/package/pkg)
#' @param pkg a package directory or tar.gz
#' @return \code{NULL}
#' @export
vmrPackageTest <- function(pkg = "./") {
  printVerbose(1, "Will perform package test")

  if (dir.exists(normalizePath(pkg))) {
    to_dir <- basename(normalizePath(pkg))
    to_pkg <- ""
  }
  else {
    to_dir <- basename(dirname(normalizePath(pkg)))
    to_pkg <- basename(pkg)
  }

  printVerbose(2, paste0("Cleaning Guest \"vmr/package/", to_dir, "\""))
  vagrantSSHCommand(paste0("rm -rf vmr/package/", to_dir, "/* 2>&1 > /dev/null"))
  vagrantSSHCommand(paste0("mkdir -p vmr/package/", to_dir, " 2>&1 > /dev/null"))
  Rcmds <- paste0(
    "Rscript -e \"library(devtools);test(normalizePath('vmr/package/",
    to_dir, "/", to_pkg, "'))\""
  )
  vmrProvision(cmd = Rcmds, elts = pkg, dest = paste0("vmr/package/", to_dir, "/"))
  return(NULL)
}
