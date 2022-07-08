## ----eval=FALSE---------------------------------------------------------------
#  vmrStart()
#  # do what you want
#  vmrTakeSnapshot("cicdversionR")

## ----eval=FALSE---------------------------------------------------------------
#  virtualboxGitlabRunner(vmr_env,
#                         gitlab_url = "gitlab.com",
#                         gt_token = "<mytoken>",
#                         snapshot_name = "cicdversionR",
#                         vm_name = <VirtualBox VM Name>)

## ----eval=FALSE---------------------------------------------------------------
#  vmrTakeSnapshot("cicdversionR")
#  vmrStop()

