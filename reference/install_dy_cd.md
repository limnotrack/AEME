# Install DYRESM-CAEDYM executables for AEME

Downloads the pre-compiled DYRESM-CAEDYM executable suite (`dycd.exe`
plus its three companion tools, `createDYref.exe`, `createDYsim.exe`,
and `extractDYinfo.exe`) for the current platform, verifies it against
its published SHA256 checksum, and installs it into a persistent user
cache directory. Binaries are attached as assets to AEME's GitHub
releases, with the version encoded in the asset filename (e.g.
`dycd-windows-5.0.0.zip`), mirroring
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md).

## Usage

``` r
install_dy_cd(
  version = "latest",
  os = NULL,
  repo = "limnotrack/AEME",
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- version:

  Character. The DYRESM-CAEDYM version to install, e.g. `"5.0.0"`. Use
  [`list_dy_cd_versions()`](https://limnotrack.com/reference/list_dy_cd_versions.md)
  to see what's available. Defaults to `"latest"`, which resolves to the
  highest version number available for the current platform.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on; you shouldn't normally need to
  set this. Note that only a Windows build exists (no source is
  available to build for other platforms), so `macos`/`linux` will
  report no versions available.

- repo:

  Character. The `"owner/repo"` GitHub repository that DYRESM-CAEDYM
  binaries are attached to. Defaults to `"limnotrack/AEME"`.

- force:

  Logical. If `FALSE` (the default) and this version is already
  installed for this platform, the download/verification steps are
  skipped and the existing path is returned. Set to `TRUE` to
  re-download and reinstall anyway.

- quiet:

  Logical. If `TRUE`, suppresses progress messages (download errors and
  checksum failures still raise, and are never silenced).

## Value

Invisibly, the file path to the installed `dycd.exe` (its three
companion tools sit alongside it in the same directory).

## Details

DYRESM-CAEDYM is closed-source (no public repository to build from), so
unlike GLM there is no possibility of compiling it in a CI pipeline –
this only ever distributes whatever binary was previously obtained and
vetted, exactly as the binaries bundled directly in earlier AEME
versions were.

Binaries are cached under
[`tools::R_user_dir("AEME", "data")`](https://rdrr.io/r/tools/userdir.html),
in an `<os>/<version>/` subdirectory (the same root
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md),
[`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md),
and
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
use). Every binary is published alongside a `.sha256` checksum file in
the same release. This function downloads both, recomputes the SHA256 of
the downloaded zip locally, and compares it to the published value
before extracting anything. If the checksums don't match, or if no
checksum file is found at all, installation is aborted and nothing is
extracted – this function will not install an unverified binary under
any circumstances.

## See also

[`list_dy_cd_versions()`](https://limnotrack.com/reference/list_dy_cd_versions.md)
to discover available versions,
[`dy_cd_exe_path()`](https://limnotrack.com/reference/dy_cd_exe_path.md)
to locate an already-installed executable,
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
for the equivalent GLM-AED installer this mirrors.

## Examples

``` r
if (FALSE) { # \dontrun{
install_dy_cd(version = "5.0.0")

# Force re-download and reinstall
install_dy_cd(version = "5.0.0", force = TRUE)
} # }
```
