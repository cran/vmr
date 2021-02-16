## ----eval=FALSE---------------------------------------------------------------
#  vmrInfo()

## ----eval=FALSE---------------------------------------------------------------
#  vmrSend(c("myfile1","myfile2"))

## ----eval=FALSE---------------------------------------------------------------
#  vmrProvision(cmd = c("./myscript.sh"), elts = c("myscript.sh"), dest = "/home/vmr/")

## ----eval=FALSE---------------------------------------------------------------
#  vmrExec(c('print("HelloWorld")'))

## ----eval=FALSE---------------------------------------------------------------
#  vmrUpdatePackages()
#  vmrInstallPackages(pkg = c("vmr"))

## ----eval=FALSE---------------------------------------------------------------
#  # check a local package
#  vmrPackageCheck()
#  # Test it
#  vmrPackageTest()
#  # and create archive and package binary
#  vmrPackageBuild()

