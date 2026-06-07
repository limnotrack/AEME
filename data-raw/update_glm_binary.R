owner = "AquaticEcoDynamics"
repo = "glm-aed"
path = "binaries/windows/glm_latest"
dest = "inst/extbin/glm_aed/windows"
fils <- list.files(dest, full.names = TRUE)

# Delete old files
if (length(fils) > 0) {
  file.remove(fils)
}

download_github_folder <- function(owner, repo, path, dest = ".") {
  
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s",
    owner, repo, path
  )
  
  resp <- httr2::request(api_url) |>
    httr2::req_perform()
  
  items <- resp |> httr2::resp_body_json()
  
  # Ensure destination directory exists
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  
  for (item in items) {
    if (item$type == "file") {
      message("Downloading file: ", item$path)
      dest_file <- file.path(dest, basename(item$path))
      download.file(item$download_url, dest_file, mode = "wb")
    }
    
    if (item$type == "dir") {
      message("Recursing into: ", item$path)
      sub_dest <- file.path(dest, basename(item$path))
      download_github_folder(owner, repo, item$path, sub_dest)
    }
  }
}

download_github_folder(
  owner = "AquaticEcoDynamics",
  repo = "glm-aed",
  path = "binaries/windows/glm_latest",
  dest = "inst/extbin/glm_aed/windows"
)

download_github_folder(
  owner = "AquaticEcoDynamics",
  repo = "glm-aed",
  path = "binaries/ubuntu/24.04/glm_latest",
  dest = "inst/extbin/glm_aed/linux"
)

download_github_folder(
  owner = "AquaticEcoDynamics",
  repo = "glm-aed",
  path = "binaries/macos/Big_Sur/glm_latest",
  dest = "inst/extbin/glm_aed/macos"
)



