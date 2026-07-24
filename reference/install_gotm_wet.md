# Install a GOTM-WET executable for AEME

Downloads a pre-compiled GOTM (coupled with the WET biogeochemical
library) binary for the current platform, verifies it against its
published SHA256 checksum, and installs it into a persistent user cache
directory. Binaries are attached as assets to AEME's GitHub releases,
with the GOTM-WET version encoded in the asset filename (e.g.
`gotm-windows-2023.2.0.zip`), mirroring
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md).
Because a given GOTM-WET version isn't tied to any one AEME release,
this function searches across all releases of `repo` to find the release
that has the requested version's asset attached.

## Usage

``` r
install_gotm_wet(
  version = "latest",
  os = NULL,
  repo = "limnotrack/AEME",
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- version:

  Character. The GOTM-WET version to install, e.g. `"2023.2.0"`. Use
  [`list_gotm_wet_versions()`](https://limnotrack.com/reference/list_gotm_wet_versions.md)
  to see what's available. Defaults to `"latest"`, which resolves to the
  highest version number available for the current platform.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on; you shouldn't normally need to
  set this. Note that only a Windows build currently exists (no source
  is compiled for other platforms), so `macos`/`linux` will report no
  versions available.

- repo:

  Character. The `"owner/repo"` GitHub repository that GOTM-WET binaries
  are attached to. Defaults to `"limnotrack/AEME"`.

- force:

  Logical. If `FALSE` (the default) and this version is already
  installed for this platform, the download/verification steps are
  skipped and the existing path is returned. Set to `TRUE` to
  re-download and reinstall anyway.

- quiet:

  Logical. If `TRUE`, suppresses progress messages (download errors and
  checksum failures still raise, and are never silenced).

## Value

Invisibly, the file path to the installed GOTM-WET executable.

## Details

Binaries are cached under
[`tools::R_user_dir("AEME", "data")`](https://rdrr.io/r/tools/userdir.html),
in an `<os>/<version>/` subdirectory (the same root
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
and
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
use – these coexist there because the installed files themselves are
named differently, `gotm`/`gotm.exe` vs `glm`/`glm.exe` vs
`simstrat`/`simstrat.exe`).

Every binary is published alongside a `.sha256` checksum file in the
same release. This function downloads both, recomputes the SHA256 of the
downloaded zip locally, and compares it to the published value before
extracting anything. If the checksums don't match, or if no checksum
file is found at all, installation is aborted and nothing is extracted –
this function will not install an unverified binary under any
circumstances.

## See also

[`list_gotm_wet_versions()`](https://limnotrack.com/reference/list_gotm_wet_versions.md)
to discover available versions,
[`gotm_wet_exe_path()`](https://limnotrack.com/reference/gotm_wet_exe_path.md)
to locate an already-installed executable,
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
for the equivalent GLM-AED installer this mirrors.

## Examples

``` r
if (FALSE) { # \dontrun{
install_gotm_wet(version = "2023.2.0")

# Force re-download and reinstall
install_gotm_wet(version = "2023.2.0", force = TRUE)
} # }
```
