vmrSetVerbose("None")
vmr_env <- list()
attr(vmr_env, "class") <- "vmr"
vmr_env$path <- getwd()
vmr_env$org <- .VagrantCloudOrganization
vmr_env$box <- "FakeBox"
vmr_env$version <- "latest"
vmr_env$vagrantName <- "vmr-FakeBox-virtualbox"
vmr_env$provider <- ""
vmr_env$provider_options <- NULL
vmr_env$ID <- ""
vmr_env$synced_folder <- list()
vmr_env$synced_folder$source <- ""
vmr_env$synced_folder$destination <- ""
vmr_env$ssh_user <- "vmr"
vmr_env$ssh_pwd <- "vmr"

test_that("VagrantFile write", {
  path <- writeVagrantFile(vmr_env, force = TRUE)

  expect_snapshot_file(path,
    name = paste0(basename(path), ".default"),
    binary = FALSE, cran = FALSE
  )


  vmr_vb <- vmr_env
  vmr_vb$synced_folder$source <- "./"
  vmr_vb$synced_folder$destination <- "/vmr"

  path <- writeVagrantFile(vmr_vb, force = TRUE)

  expect_snapshot_file(path,
    name = paste0(basename(path), ".mount"),
    binary = FALSE, cran = FALSE
  )

  vmr_vb$provider <- "virtualbox"
  vmr_vb$provider_options <- virtualboxOptions(details = FALSE)
  vmr_vb$ID <- "42"

  path <- writeVagrantFile(vmr_vb, force = TRUE)

  expect_snapshot_file(path,
    name = paste0(basename(path), ".virtualbox"),
    binary = FALSE, cran = FALSE
  )
})

test_that("VagrantFile read", {
  skip_on_cran()
  skip_if(TRUE)
  skip_on_ci()
  skip_if_not(file.exists("Vagrantfile.default"), message = paste("bad wd ",getwd()))
  skip_if(vagrantIsInstalled() == "", message = paste("vagrant not installed",getwd()))

  vmr_env_read <- vmrLoad(getwd(), "Vagrantfile.default")

  expect_equal(vmr_env, vmr_env_read)

  vmr_vb <- vmr_env
  vmr_vb$synced_folder$source <- "./"
  vmr_vb$synced_folder$destination <- "/vmr"
  vmr_vb$provider <- "virtualbox"
  vmr_vb$provider_options <- virtualboxOptions(details = FALSE)

  vmr_env_read <- vmrLoad(getwd(), "Vagrantfile.virtualbox")

  expect_equal(vmr_vb$provider, vmr_env_read$provider)
  expect_equal(vmr_vb$source, vmr_env_read$source)
  expect_equal(vmr_vb$destination, vmr_env_read$destination)
  expect_equal(vmr_vb$provider_options$gui, vmr_env_read$provider_options$gui)
  expect_equal(vmr_vb$provider_options$modifyvm$memory, vmr_env_read$provider_options$modifyvm$memory)
})
