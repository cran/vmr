## ----eval=FALSE---------------------------------------------------------------
#  boxes_list <- vmrList()
#  boxes_list

## ----eval=FALSE---------------------------------------------------------------
#  vmrListBox(boxes_list$Name[1])

## ----eval=FALSE---------------------------------------------------------------
#  vmrBoxDownload(vmr_env)

## ----eval=FALSE---------------------------------------------------------------
#  # List downloaded boxes
#  vmrLocalBoxList()
#  # Remove old boxes (not up to date)
#  vmrLocalBoxPrune()
#  # Remove a specific box
#  vmrLocalBoxRemove(<box name>)
#  # Download the last box version (use in a __vmr__ environment)
#  vmrLocalBoxUpdate()

