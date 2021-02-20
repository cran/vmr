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

.VagrantCloudURL <- "https://app.vagrantup.com"

# @title List Boxes Available in VagrantCloud
# @name getAvailableBoxes
# @description List boxes and providers available.
# @param org organization name account (default: "VMR")
# @return a data.frame
# @import curl
# @import jsonlite
# @return a data.frame with "Name", "Version", "Description" and "Provider" boxes
getAvailableBoxes <- function(org = .VagrantCloudOrganization) {
  req <- .requestVagrantAPI(api_uri = paste0("/user/", org))

  # json_res <- jsonlite::prettify(rawToChar(req$content))
  # print(json_res)
  printVerbose(2, "parsing response : \n ", paste0(" \n", rawToChar(req$content)))
  json_res <- jsonlite::parse_json(paste0(" \n", rawToChar(req$content)))


  list_boxes <- lapply(json_res$boxes, FUN = function(l) {
    list_providers <- lapply(l$current_version$providers, FUN = function(p) {
      return(c(l$name, l$current_version$version, l$short_description, p$name))
    })
    return(list_providers)
  })

  vms <- data.frame(matrix(unlist(list_boxes), ncol = 4, byrow = TRUE))
  colnames(vms) <- c("Name", "Version", "Description", "Provider")

  return(vms)
}

# @title Get a box information from VagrantCloud
# @name getBoxInfo
# @description Get information about a box
# @param name the box name
# @param org organization name account (default: "VMR")
# @return a data.frame
# @import curl
# @import jsonlite
# @return a data.frame with "Name, "Version", "Description", "Provider" and "Date"
getBoxInfo <- function(name, org = .VagrantCloudOrganization) {
  req <- .requestVagrantAPI(api_uri = paste0("/box/", org, "/", name))

  # json_res <- jsonlite::prettify(rawToChar(req$content))
  # print(json_res)
  printVerbose(2, "parsing response : \n ", paste0(" \n", rawToChar(req$content)))
  json_res <- jsonlite::parse_json(paste0(" \n", rawToChar(req$content)))


  # last_box <- lapply(json_res$current_version$providers,
  #                      FUN = function(p) {
  #                        return(c(json_res$name,
  #                                 json_res$current_version$version,
  #                                 json_res$short_description,
  #                                 p$name,
  #                                 p$created_at))
  #   })
  #
  # vm <- data.frame(matrix(unlist(last_box), ncol = 5, byrow = TRUE))

  list_boxes <- lapply(json_res$versions, FUN = function(l) {
    list_providers <- lapply(l$providers, FUN = function(p) {
      return(c(
        json_res$name,
        l$version,
        l$description_markdown,
        p$name,
        p$created_at
      ))
    })
    return(list_providers)
  })

  vms <- data.frame(matrix(unlist(list_boxes), ncol = 5, byrow = TRUE))

  # boxes <- rbind(vm,vms)
  boxes <- vms
  colnames(boxes) <- c("Name", "Version", "Description", "Provider", "Date")

  return(boxes)
}

# @title Query Vagrant Cloud API
# @name requestVagrantAPI
# @description Query Vagrant Cloud API using api_uri
# @param api_uri API uri to use
# @return API answer
.requestVagrantAPI <- function(api_uri = "/user/VMR") {
  url <- paste0(.VagrantCloudURL, "/api/v1", api_uri)
  printVerbose(2, "Fetching information from ", url)
  res <- curl::curl_fetch_memory(url)

  printVerbose(2, "Response status: ", res$status_code)
  printVerbose(2, "Response type: ", res$type)

  if (res$status_code != 200) {
    warning("VagrantCloud API query return ", res$status_code, "code => seems somethings got wrong")
  }
  return(res)
}
