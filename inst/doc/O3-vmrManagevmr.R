## ----eval=FALSE---------------------------------------------------------------
#  vmr_env <- vmrCreate(<boxname>)

## ---- eval=FALSE--------------------------------------------------------------
#  setwd("path/to/my/vmr/environment/")
#  vmr_env <- vmrLoad()
#  vmr_env

## ----eval=FALSE---------------------------------------------------------------
#  vmr_env # created or loaded object
#  # force.vagrantfile will override existing Vagrantfile template
#  vmr_env <- vmrInitEnv(vmr_env, force.vagrantfile=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  vmr_env <- vmrLoad()
#  # provider cleaning
#  vmrDestroy(vmr_env$id)
#  # box cleaning
#  vmrLocalBoxRemove(vmr_env$box, provider = vmr_env$provider, version = vmr_env$version)
#  # remove the working directory

## ----eval=FALSE---------------------------------------------------------------
#  vmr_env <- vmrUpdateEnvVersion(vmr_env)

## ----eval=FALSE---------------------------------------------------------------
#  vmr_env <- vmrMountDir(vmr_env, src = "/" , dest = "/" )

## ----eval=FALSE---------------------------------------------------------------
#  # Get environment status
#  vmrStatus()
#  # Start a provider instance
#  vmrStart()
#  # Save state and stop provider instance
#  vmrSuspend()
#  # Resume a saved provider instance
#  vmrResume()
#  # Stop a provider instance
#  vmrStop()
#  # Remove a provider instance
#  vmrDestroy()

## ----eval=FALSE---------------------------------------------------------------
#  # Take a snapshot
#  vmrTakeSnapshot("my snapshot")
#  # resume a snapshot
#  vmrRestoreSnapshot("my snapshot")
#  # list snapshots
#  vmrListSnapshot()

