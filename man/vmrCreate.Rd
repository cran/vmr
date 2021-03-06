% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vmr-methods.R
\name{vmrCreate}
\alias{vmrCreate}
\title{Create a \strong{vmr} environment class}
\usage{
vmrCreate(
  name,
  provider = "virtualbox",
  version = "latest",
  provider.options = virtualboxOptions(FALSE)
)
}
\arguments{
\item{name}{a box name}

\item{provider}{the box provider (default: "virtualbox")}

\item{version}{the box version (default : "latest")}

\item{provider.options}{provider options (call [\code{\link[=getProviderOptions]{getProviderOptions()}}] to get values)}
}
\value{
a \strong{vmr} object (see details)
}
\description{
Create a \strong{vmr} object.
}
\details{
Create a S3 \strong{vmr} object (a simple list).
The object contains all information needed to configure and manage a
\strong{vmr} environment (a vagrant environment).

A \strong{vmr} environment need mostly a box \emph{name} and a \emph{provider}.
The environment is attached to the current working directory.

\strong{vmr} object main attributs:
\itemize{
\item \strong{path}: working directory
\item \strong{org}: Vagrant cloud user/organization name 'VMR'
\item \strong{box}: the box name
\item \strong{version}: the box version
\item \strong{provider}: the provider
\item \strong{provider_options}: the provider options (see [\code{\link[=getProviderOptions]{getProviderOptions()}}])
\item \strong{vagrantName}: Vagrant environment name
\item \strong{ID} <- Vagrant environment ID
\item \strong{synced_folder}: a list with source and destination
\item \strong{ssh_user}: the ssh user
\item \strong{ssh_pwd}: the ssh user password
\item \strong{ssh_port}: the ssh port
\item \strong{ssh_private_key_path}: the private ssh key path
}
}
\examples{
\dontrun{
# List boxes available
boxes <- vmrList()
# Create a vmr object
vmr <- vmrCreate(boxes$Name[1])

# to customize the guest machine for virtualbox
virtualboxOpts <- getProviderOptions(provider = "virtualbox")
virtualboxOpts$modifyvm <- list(cpus = 4, memory = 4096)
virtualboxOpts$name <- "My VM Cool Name"
# To specify a provider and version
vmr <- vmrCreate(
  name = boxes$Name[1],
  provider = "virtualbox",
  version = boxes$Version[1],
  provider.options = virtualboxOpts
)
}
}
