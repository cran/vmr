## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  #install.packages(c('vmr'))
#  library(vmr)
#  vmrSetVerbose("Full")

## ----eval=FALSE---------------------------------------------------------------
#  list_boxes <- vmrList()
#  print(list_boxes)

## ----eval=FALSE---------------------------------------------------------------
#  index <- which(list_boxes$Name == "LinuxMint20-R")[1]
#  vmr_env <- vmrCreate(name = list_boxes$Name[index], provider = list_boxes$Provider[1])
#  vmr_env

## ----eval=FALSE---------------------------------------------------------------
#  vmr_env <- vmrInitEnv(vmr_env)

## ----eval=FALSE---------------------------------------------------------------
#  vmrStart()

## ----eval=FALSE---------------------------------------------------------------
#  vmrStatus()

## ----eval=FALSE---------------------------------------------------------------
#  vmrSuspend()

## ----eval=FALSE---------------------------------------------------------------
#  vmrResume()

## ----eval=FALSE---------------------------------------------------------------
#  vmrStop()

