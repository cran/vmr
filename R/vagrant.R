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

# @title System call to vagrant (old version)
# @name vagrantExec2
# @description execute vagrant command with parameters
# @param args character vector of arguments to vagrant command
# @param stdout if "" print output, if TRUE capture and return it and FALSE discard ouput
# @param stderr if "" print stderr, if TRUE capture and return it and FALSE discard stderr
# @return execution code or error output
vagrantExec2 <- function(args = character(), stdout = "", stderr = "") {
  out <- -1

  tc <- tryCatch(
    {
      printVerbose(2, "Execute:", "vagrant ", paste(args))
      out <- system2("vagrant", args, stdout = stdout, stderr = stderr)
    },
    error = function(cond) {
      message(cond)
      warning("vagrant command went wrong!\n", immediate. = TRUE)
    },
    warning = function(cond) {
      message(cond)
      warning("vagrant command may went wrong!\n")
    },
    finally = {
    }
  )

  return(invisible(out))
}

# @title System call to vagrant
# @name vagrantExec
# @description execute vagrant command with parameters
# @param args character vector of arguments to vagrant command
# @param stdout if "" print output, if TRUE capture and return it and FALSE discard ouput
# @param stderr if "" print stderr, if TRUE capture and return it and FALSE discard stderr
# @return execution code (0 if OK) otherwise cmd output if stdout and/or stderr is TRUE
vagrantExec <- function(args = character(), stdout = "", stderr = "") {
  out <- -1
  printVerbose(2, "Execute: '", vmr_env$vagrant_bin, " ", paste(args, collapse = " "), "'")
  if (vmr_env$verbose_mode == 0 && stdout != TRUE) stdout <- FALSE
  if (stderr != TRUE && (vmr_env$verbose_mode == 0 || vmr_env$verbose_mode == 1)) {
    stderr <- FALSE
  }

  out <- system2(vmr_env$vagrant_bin, args, stdout = stdout, stderr = stderr)

  if (!is.null(attr(out, "status"))) {
    warning("vmr ==> vagrant command may went wrong!\n vmr ==> output : \n", out)
  }

  return(invisible(out))
}

# @title List vagrant boxes installed
# @name vagrantBoxList
# @description execute vagrant command 'box list'
# @return a data.frame with boxes Name, Poviders and Version
vagrantBoxList <- function() {
  args <- c("box", "list")
  out <- vagrantExec(args, stdout = TRUE)

  df_box <- data.frame(Name = character(0), Provider = character(0), Version = character(0))
  
  if( length(grep("There are no installed boxes!",out)) == 0L ) {
    l_box <- lapply(out, FUN = function(l) {
      lp <- strsplit(l, "[ ]+")
      df_box <<- rbind(
        df_box,
        c(
          lp[[1]][1],
          substr(lp[[1]][2], start = 2, stop = nchar(lp[[1]][2]) - 1),
          substr(lp[[1]][3], start = 1, stop = nchar(lp[[1]][3]) - 1)
        )
      )
    })
  }
  colnames(df_box) <- c("Name", "Provider", "Version")

  return(df_box)
}

# @title Update a vagrant box
# @name vagrantBoxUpdate
# @description execute vagrant command 'box update'.
# Will download the latest version of the current directory box
#  or a specific box using arguments.
# @param box a box name to update
# @param provider a provider of the box to update
# @return execution code
vagrantBoxUpdate <- function(box = "", provider = "") {
  args <- c("box", "update")
  if (nchar(box) > 0) args <- c(args, "--box", box)
  if (nchar(provider) > 0) args <- c(args, "--provider", provider)
  vagrantExec(args)
}

# @title Remove a vagrant box
# @name vagrantBoxRemove
# @description Execute vagrant command 'box remove'.
# It remove a specific box identify by it name. It provider and version can be specify.
# @param name the box name to remove
# @param provider the box provider (default: the default one)
# @param version the box version (default: the latest)
# @param force force box remove if TRUE (default: FALSE)
# @return execution code or error output
vagrantBoxRemove <- function(name, provider = "", version = "", force = FALSE) {
  args <- c("box", "remove")
  if (nchar(provider) > 0) args <- c(args, "--provider", provider)
  if (nchar(version) > 0) args <- c(args, "--box-version", version)
  if (force) args <- c(args, "--force")
  args <- c(args, name)
  vagrantExec(args)
}

# @title Download a box from VagrantCloud
# @name vagrantBoxAdd
# @description Download a box from VagrantCloud. Command 'box add'.
# @param name the box name to download
# @param version the box version (default : latest)
# @param provider the box provider (default : virtualbox)
# @param force if TRUE force box update otherwise don't
# @return a data.frame with box information (name, provider and version)
vagrantBoxAdd <- function(name, version = "latest", provider = "virtualbox", force = FALSE) {

  ## TODO check box size and harddrive space

  printVerbose(1, "The download can be long depending on the size of the box and your bandwidth")

  args <- "box add"
  args <- c(args, paste0("--provider ", provider))
  if (version != "latest" && version != "") args <- c(args, paste0(" --box-version ", version))
  if (isTRUE(force)) args <- c(args, " --force ")
  args <- c(args, name)

  vagrantExec(args)

  return(invisible(data.frame(name = name, version = version, provider = provider)))
}


# @title Remove old versions of installed boxes
# @name vagrantBoxPrune
# @description Run vagrant command 'box prune'.
# Removes old versions of installed boxes.
# @return a data.frame with boxes information (name, provider and version)
vagrantBoxPrune <- function() {
  printVerbose(2, "Boxes before pruning\n", vagrantBoxList())

  args <- c("box", "prune", "--force", "--keep-active-boxes")
  vagrantExec(args)
  printVerbose(2, "Boxes keeped after pruning\n")
  boxes <- vagrantBoxList()
  printVerbose(2, boxes)
  return(boxes)
}

# @title Initialize a Vagrantfile
# @name vagrantInit
# @description Run vagrant command 'init'.
# Creates a Vagrantfile for a box, and force version if specified
#  in the current directory.
# @param name the box name
# @param version the box version (default : "latest")
# @param force default to FALSE, if TRUE force to rewrite file
# @return Vagrantfile path
vagrantInit <- function(name, version = "latest", force = FALSE) {
  if (file.exists("Vagrantfile") && isFALSE(force)) {
    stop("Vagrantfile already exists in ", getwd(), "\n use argument force=TRUE to override it.")
  }

  args <- "init "
  if (version != "latest") args <- c(args, paste0(" --box-version ", version))
  args <- c(args, " --minimal ")
  if (isTRUE(force)) args <- c(args, " --force ")
  args <- c(args, name)

  vagrantExec(args)

  return(normalizePath(paste0(getwd(), "/Vagrantfile")))
}

# @title Creates and configure a guest machine
# @name vagrantInit
# @description Run vagrant command 'up --no-provision'.
# Creates and configure a guest machine according to the Vagrantfile
# in current directory.
# @return execution code or error
vagrantUp <- function() {
  if (!file.exists("Vagrantfile")) {
    stop("Vagrantfile does not exists in ", getwd(), "\n use vmrInitEnv()")
  }

  args <- " up "
  args <- c(args, "--no-provision")

  printVerbose(2, "Start guest machine using VagrantFile, please wait...")
  vagrantExec(args)
}

# @title Halt a guest machine
# @name vagrantHalt
# @description Run vagrant command 'halt'.
# Shuts down the running machine in the vagrant current directory.
# @param force if TRUE force poweroff otherwise normal shut down.
# @return execution code or error
vagrantHalt <- function(force = FALSE) {
  if (!file.exists("Vagrantfile")) {
    stop("Vagrantfile does not exists in ", getwd(), "\n use startVMR")
  }

  args <- " halt "
  if (isTRUE(force)) args <- c(args, " --force ")

  printVerbose(2, "Stop box using VagrantFile in ", getwd(), "\n please wait...")
  vagrantExec(args)
}

# @title List machines states
# @name vagrantStatus
# @description Run vagrant command 'status'.
# Tell the state of machines.
# @return a data.frame with name, provider and state or a empty list
vagrantStatus <- function() {
  args <- "status"
  args <- c(args, "--machine-readable")
  res <- vagrantExec(args, stdout = TRUE)

  if (is.character(res)) {
    vagrantName <- strsplit(res[1], split = ",")[[1]][2]
    provider <- strsplit(res[2], split = ",")[[1]][4]
    state <- strsplit(res[4], split = ",")[[1]][4]
    return(list(vagrantName = vagrantName, provider = provider, state = state))
  }
  else {
    return(list())
  }
}

# @title get global-status of Vagrant environment
# @name vagrantGlobalStatus
# @description Run vagrant command 'global-status --prune'.
# Its get Vagrant global-status.
# @return a data.frame with id, name, provider, path and state or empty data.frame
vagrantGlobalStatus <- function() {
  args <- c("global-status", "--prune") # , "--machine-readable")
  res <- vagrantExec(args, stdout = TRUE)

  if ((is.null(attr(res, "status")) || attr(res, "status") == 0) && is.character(res)) {
    col_name <- strsplit(res[1], split = "[ ]+")[[1]]

    global_status <- data.frame(matrix(ncol = length(col_name), nrow = 0))

    if (!identical(res, integer(0)) && !identical(which(res == " "), integer(0))) {
      trash <- lapply(res[3:(which(res == " ") - 1)], FUN = function(vm) {
        global_status <<- rbind(global_status, strsplit(vm, split = "[ ]+")[[1]])
      })
    }
    colnames(global_status) <- col_name
    return(global_status)
  }

  return(data.frame())
}

# @title Get id of a vagrant environment
# @name vagrantGetID
# @description Get id of a vagrant environment from the global-status
# environment is identify by name and/or directory
# @param name vagrant environment name
# @param path vagrant environment directory
# @return the id or ""
vagrantGetID <- function(name = "", path = "") {
  res <- vagrantGlobalStatus()
  if (length(res) == 0) {
    return("")
  }

  if (name != "" && path != "") {
    ind <- which(res$name == name & res$directory == path)
    if (length(ind) == 1) {
      return(res[ind, "id"])
    }
  }

  if (name == "" && path != "") {
    ind <- which(res$directory == path)
    if (length(ind) == 1) {
      return(res[ind, "id"])
    }
  }

  if (name != "" && path == "") {
    ind <- which(res$name == name)
    if (length(ind) == 1) {
      return(res[ind, "id"])
    }
  }

  # warning("Can't determine environment ID")
  return("")
}

# @title Run VagrantFile provision
# @name vagrantProvision
# @description This method is no more used.
# Run vagrant command 'provision'.
# Its run vagrantFile provision section.
vagrantProvision <- function() {
  if (!file.exists("Vagrantfile")) {
    stop("Vagrantfile does not exists in ", getwd(), "\n use vmrInitEnv")
  }

  args <- " provision "

  printVerbose(2, "Provision box using VagrantFile in ", getwd(), "\n please wait...")
  vagrantExec(args)
}

# @title Check if vagrant is installed and up to date.
# @name vagrantIsInstalled
# @description try to run vagrant and get version
# @return vagrant binary path and version or empty characters
vagrantIsInstalled <- function() {
  where_is_vagrant <- Sys.which("vagrant")
  version <- ""
  out <- ""

  # if can't find vagrant  try 'which' sys call
  if (!nzchar(where_is_vagrant)) {
    out <- tryCatch(
      {
        # if (.Platform$OS.type == "windows") {
        #   system2("where", args = c("vagrant"), stdout = TRUE, stderr = FALSE)
        # }else {}
        system2("which", args = c("vagrant"), stdout = TRUE, stderr = FALSE)
      },
      error = function(cond) {},
      warning = function(cond) {},
      finally = {}
    )

    # 'which' return vagrant path and binary exists ?
    if (!is.null(out) && grepl("vagrant", out[1]) && file.exists(out[1])) where_is_vagrant <- out[1]
  }

  if (nzchar(where_is_vagrant)) {
    names(where_is_vagrant) <- NULL
    out <- suppressWarnings(try(system2(where_is_vagrant, args = c("--version"), stdout = TRUE, stderr = TRUE)))
    if (grepl("Vagrant", out[1])) {
      pos_value <- regexpr("[0-9\\.]+", out[1])
      version <- substr(out[1], pos_value, pos_value + attr(pos_value, "match.length"))
    }
  } else {
    packageStartupMessage(
      "Vagrant seems not to be installed.\n",
      "Please visit the page below and download and install vagrant: \n",
      "https://www.vagrantup.com/downloads.html\n"
    )
  }

  # vagrant version
  # if ( !identical(pos <- grep("Installed Version: ", out), integer(0)) ) message("# Vagrant ", out[pos])
  # if ( !identical(pos <- grep("Latest Version: ", out), integer(0)) ) message("# Please upgrade vagrant to ", out[pos])

  return(list("vagrant_bin" = where_is_vagrant, "version" = version))
}

# @title Run a command via ssh on guest machine
# @name vagrantSHHCommand
# @description Run vagrant command 'ssh'.
# Executes a command via ssh on the guest machine.
# @param cmd a command
# @return execution code
vagrantSSHCommand <- function(cmd) {
  # cmd_escape <- paste0('\"', gsub('\\\"', '\\\\\"', cmd), ' 1>&2\"')
  cmd_escape <- paste0('\"', gsub('\\\"', '\\\\\"', cmd), '\"')

  args <- c(" ssh ", " -c ")
  args <- c(args, cmd_escape)

  res <- vagrantExec(args)
  return(res)
}

# @title Get vagrant ssh configuration
# @name vagrantSSHConfig
# @return a list with hostname, port and (identity) keyfile
vagrantSSHConfig <- function() {
  out <- vagrantExec(c("--machine-readable", "ssh-config"), stdout = TRUE)

  res <- list()

  ind <- regexpr("HostName ([0-9.]+)", out[2], useBytes = FALSE)
  res$hostname <- substr(out[2], ind[1] + 9, ind[1] + 9 + attr(ind, "match.length") - 10)


  ind <- regexpr("Port ([0-9]+)", out[2], useBytes = FALSE)
  res$port <- substr(out[2], ind[1] + 5, ind[1] + 5 + attr(ind, "match.length") - 6)

  ind <- regexpr("IdentityFile [a-zA-Z0-9/:._]+", out[2], useBytes = FALSE)
  res$keyfile <- substr(out[2], ind[1] + 13, ind[1] + 13 + attr(ind, "match.length") - 14)

  return(res)
}

# @title Upload a file or directory from host to guest machine
# @name vagrantUpload
# @description Run vagrant command 'upload'.
# Uploads a file or a directory from the host to the guest machine.
# @param elt a file or directory name
# @param dest destination path (default "~/vmr")
# @return code (0 if OK) or message
vagrantUpload <- function(elt, dest = "") {
  if (!file.exists(elt) && !dir.exists(elt)) {
    stop(paste0(elt, " does not exists. Can't upload it."))
  }

  args <- c("upload", elt, dest)
  res <- vagrantExec(args = args)

  return(res)
}

# @title Manage snapshot virtual machine
# @description Run vagrant command 'snapshot'.
# Its list, create or restore snapshots.
# @param cmd "list", "save" or "restore"
# @param name snapshot name to save or restore
# @return execution code or message
vagrantSnapshot <- function(cmd, name = "") {
  args <- c("snapshot", cmd, name)
  res <- vagrantExec(args)
  return(invisible(res))
}

# @title create a R script for provisioning via vagrantFile
# @name .provisionRscript
# @description create a RScript with cmd to declare it into the VagrantFile.
# This is not the best way to do so (Deprecated for now). Use vmrExec() instead.
# @param cmd list of R command
# @return characters to be cat into a Vagrantfile
.provisionRscript <- function(cmd) {
  paste0(
    "$rscript = <<-'SCRIPT'\n",
    "#!/usr/bin/env Rscript\n",
    "r_lib <- .libPaths()[1]\n",
    paste0(cmd, collapse = "\n"),
    "SCRIPT\n"
  )
}
