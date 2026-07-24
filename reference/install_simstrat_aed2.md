# Install a Simstrat-AED2 executable for AEME

Downloads a pre-compiled Simstrat (coupled with AED2) binary for the
current platform, verifies it against its published SHA256 checksum, and
installs it into a persistent user cache directory. Binaries are
attached as assets to AEME's GitHub releases, with the Simstrat version
encoded in the asset filename (e.g. `simstrat-windows-3.0.4.zip`),
mirroring
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md).
Because a given Simstrat version isn't tied to any one AEME release,
this function searches across all releases of `repo` to find the release
that has the requested version's asset attached.

## Usage

``` r
install_simstrat_aed2(
  version = "latest",
  os = NULL,
  repo = "limnotrack/AEME",
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- version:

  Character. The Simstrat version to install, e.g. `"3.0.4"`. Use
  [`list_simstrat_aed2_versions()`](https://limnotrack.com/reference/list_simstrat_aed2_versions.md)
  to see what's available. Defaults to `"latest"`, which resolves to the
  highest version number available for the current platform.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on; you shouldn't normally need to
  set this. Note that only a Windows build is currently bundled with the
  package (`inst/extbin/simstrat_aed2/simstrat.exe`) – `macos`/`linux`
  release assets don't exist yet, so those platforms will report no
  versions available until binaries are published.

- repo:

  Character. The `"owner/repo"` GitHub repository that Simstrat binaries
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

Invisibly, the file path to the installed Simstrat-AED2 executable.

## Details

Binaries are cached under
[`tools::R_user_dir("AEME", "data")`](https://rdrr.io/r/tools/userdir.html),
in an `<os>/<version>/` subdirectory (the same root
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
uses – the two coexist there because the installed files themselves are
named differently, `simstrat`/`simstrat.exe` vs `glm`/`glm.exe`).
Multiple Simstrat versions can be installed side by side and switched
between via the `version` argument to
[`simstrat_aed2_exe_path()`](https://limnotrack.com/reference/simstrat_aed2_exe_path.md)
without re-downloading.

Every binary is published alongside a `.sha256` checksum file in the
same release. This function downloads both, recomputes the SHA256 of the
downloaded zip locally, and compares it to the published value before
extracting anything. If the checksums don't match, or if no checksum
file is found at all, installation is aborted and nothing is extracted –
this function will not install an unverified binary under any
circumstances.

## See also

[`list_simstrat_aed2_versions()`](https://limnotrack.com/reference/list_simstrat_aed2_versions.md)
to discover available versions,
[`simstrat_aed2_exe_path()`](https://limnotrack.com/reference/simstrat_aed2_exe_path.md)
to locate an already-installed executable,
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
for the equivalent GLM-AED installer this mirrors.

## Examples

``` r
if (FALSE) { # \dontrun{
install_simstrat_aed2(version = "3.0.4")

# Force re-download and reinstall
install_simstrat_aed2(version = "3.0.4", force = TRUE)
} # }
```
