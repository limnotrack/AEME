# Install a GLM executable for AEME

Downloads a pre-compiled GLM binary for the current platform, verifies
it against its published SHA256 checksum, and installs it into a
persistent user cache directory. Binaries are attached as assets to
AEME's GitHub releases (tagged with the AEME package version, e.g.
`"0.4.0"`), with the GLM version encoded in the asset filename (e.g.
`glm-windows-3.9.108.zip`). Because a given GLM version isn't tied to
any one AEME release, this function searches across all releases of
`repo` to find the release that has the requested version's asset
attached.

## Usage

``` r
install_glm_aed(
  version = "latest",
  os = NULL,
  repo = "limnotrack/AEME",
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- version:

  Character. The GLM version to install, e.g. `"3.9.108"`. Use
  [`list_glm_versions()`](https://limnotrack.com/reference/list_glm_versions.md)
  to see what's available. Defaults to `"latest"`, which resolves to the
  highest version number available for the current platform.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on; you shouldn't normally need to
  set this.

- repo:

  Character. The `"owner/repo"` GitHub repository that GLM binaries are
  attached to. Defaults to `"limnotrack/AEME"`.

- force:

  Logical. If `FALSE` (the default) and this version is already
  installed for this platform, the download/verification steps are
  skipped and the existing path is returned. Set to `TRUE` to
  re-download and reinstall anyway.

- quiet:

  Logical. If `TRUE`, suppresses progress messages (download errors and
  checksum failures still raise, and are never silenced).

## Value

Invisibly, the file path to the installed GLM executable.

## Details

Binaries are cached under
[`tools::R_user_dir("AEME", "data")`](https://rdrr.io/r/tools/userdir.html),
in a `<os>/<version>/` subdirectory - by default this resolves to
`~/.local/share/R/AEME` on Linux,
`~/Library/Application Support/org.R-project.R/R/AEME` on macOS, and
`%APPDATA%\R\data\R\AEME` on Windows (overridable via the
`R_USER_DATA_DIR`/`XDG_DATA_HOME` environment variables - see
[tools::R_user_dir](https://rdrr.io/r/tools/userdir.html) for details).
Multiple GLM versions can be installed side by side and switched between
via the `version` argument to
[`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md)
without re-downloading.

Every binary is published alongside a `.sha256` checksum file in the
same release. This function downloads both, recomputes the SHA256 of the
downloaded zip locally, and compares it to the published value before
extracting anything. If the checksums don't match, or if no checksum
file is found at all, installation is aborted and nothing is extracted -
this function will not install an unverified binary under any
circumstances.

## See also

[`list_glm_versions()`](https://limnotrack.com/reference/list_glm_versions.md)
to discover available versions,
[`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md) to
locate an already-installed executable.

## Examples

``` r
if (FALSE) { # \dontrun{
install_glm_aed(version = "3.9.108")

# Force re-download and reinstall
install_glm_aed(version = "3.9.108", force = TRUE)
} # }
```
