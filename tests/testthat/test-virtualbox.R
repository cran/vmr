vb.opts <- list(
  gui = TRUE,
  name = "",
  nic_type = "",
  linked_clone = FALSE,
  check_guest_additions = TRUE,
  modifyvm = list(cpus = "2", memory = "4096")
)

test_that("Virtuabox default options", {
  opts <- virtualboxOptions(details=FALSE)
  
  expect_equal(vb.opts, opts)
  
})

vb.vagrantfile <- c('config.vm.provider "virtualbox" do |vb|',
                  'vb.gui = true',
                  'vb.linked_clone = false',
                  'vb.check_guest_additions = true',
                  'vb.customize ["modifyvm", :id, "--cpus", "2"]',
                  'vb.customize ["modifyvm", :id, "--memory", "4096"]',
                  'end')

test_that("Virtuabox read options", {
  opts <- virtualboxReadOptions(vb.vagrantfile)
  expect_equal(vb.opts, opts)
  
})
