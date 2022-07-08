# {vmr} - Virtual Machines for R <img src="inst/logos/vmr-logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
![CRAN Downloads Badge](https://cranlogs.r-pkg.org/badges/vmr)
[![CRAN status](https://www.r-pkg.org/badges/version/vmr)](https://cran.r-project.org/package=vmr)
[![pipeline status](https://gitlab.com/rstuff/vmr/badges/main/pipeline.svg)](https://gitlab.com/rstuff/vmr/-/commits/main)
<!-- badges: end -->

**A R package to manage Virtual Machines using _Vagrant_ tool.**  

## Purpose

This package is a wrap of the [Vagrant](https://www.vagrantup.com/) tool and more.  
It allows to manage, provision and use Virtual Machines preconfigured for R.    

It currently only use VirtualBox (>= 6.1.14) as provider. 
Vagrant tool have to be installed too. (See [Dependencies](#dependencies))

Used VMs come from [https://app.vagrantup.com/VMR](https://app.vagrantup.com/VMR) repository
and the sources use to generate them can be found at 
[https://gitlab.com/rstuff/vms](https://gitlab.com/rstuff/vms).

## Dependencies

### Tools

* [Vagrant](https://www.vagrantup.com/) (>= 2.2.10)

### Providers

* [VirtualBox](https://www.virtualbox.org/) (>= 6.1.14)

## Download last packages

The CI/CD generate the packages for each versions and OS. Choose your OS for the last version (dev) :  
* [Source](https://gitlab.com/rstuff/vmr/-/jobs/artifacts/main/download?job=r-release)
* [Windows](https://gitlab.com/rstuff/vmr/-/jobs/artifacts/main/download?job=Windows-r-package)
* [MacOS](https://gitlab.com/rstuff/vmr/-/jobs/artifacts/main/download?job=MacOS-r-package)

Unzip the downloaded file to get the package.

## Install

```R
# From CRAN
install.packages(c('vmr'))
# or from the development repository (latest version)
remotes::install_git('https://gitlab.com/rstuff/vmr.git')
```

## Documentation

Online : [https://rstuff.gitlab.io/vmr/](https://rstuff.gitlab.io/vmr/)  

```r
browseVignettes(package = "vmr")
```

## Authors

* Jean-François Rey \<jf.rey.public[at]gmail.com\>

## LICENSE

GPLv3

