# Install a Simstrat-AED executable for AEME

Downloads a pre-compiled Simstrat (coupled with AED, not AED2) binary
for the current platform, verifies it against its published SHA256
checksum, and installs it into a persistent user cache directory.
Mirrors
[`install_simstrat_aed2`](https://limnotrack.com/reference/install_simstrat_aed2.md)
exactly, except for the release-asset filename prefix (`simstrat_aed-*`
here vs `simstrat_aed2-*` for AED2, as built by
`.github/workflows/build-simstrat.yaml`) and the cache subdirectory it
installs into, so an AED2 and an AED build can be installed side by side
without colliding.

## Usage

``` r
install_simstrat_aed(
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
  [`list_simstrat_aed_versions()`](https://limnotrack.com/reference/list_simstrat_aed_versions.md)
  to see what's available. Defaults to `"latest"`, which resolves to the
  highest version number available for the current platform.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on; you shouldn't normally need to
  set this.

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

Invisibly, the file path to the installed Simstrat-AED executable.

## Details

Binaries are cached under
[`tools::R_user_dir("AEME", "data")`](https://rdrr.io/r/tools/userdir.html),
in an `<os>/simstrat_aed/<version>/` subdirectory – distinct from
Simstrat-AED2's `<os>/<version>/` root so the two coexist without the
installed `simstrat`/`simstrat.exe` filename colliding for the same
version string.

Every binary is published alongside a `.sha256` checksum file in the
same release. This function downloads both, recomputes the SHA256 of the
downloaded zip locally, and compares it to the published value before
extracting anything. If the checksums don't match, or if no checksum
file is found at all, installation is aborted and nothing is extracted –
this function will not install an unverified binary under any
circumstances.

## See also

[`list_simstrat_aed_versions()`](https://limnotrack.com/reference/list_simstrat_aed_versions.md)
to discover available versions,
[`simstrat_aed_exe_path()`](https://limnotrack.com/reference/simstrat_aed_exe_path.md)
to locate an already-installed executable,
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
for the AED2 installer this mirrors.

## Examples

``` r
if (FALSE) { # \dontrun{
install_simstrat_aed(version = "3.0.4")

# Force re-download and reinstall
install_simstrat_aed(version = "3.0.4", force = TRUE)
} # }
```
