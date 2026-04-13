#' Visualise GLM-AED model configuration
#'
#' Reads the parsed GLM and AED configuration lists from an AEME object and
#' produces an interactive HTML visualisation showing the lake hypsograph,
#' sediment zones, module wiring, and key parameter values.
#'
#' @param aeme An AEME object.
#' @param path Character. Path to AEME project.
#' @param output Character or NULL. Path for the output HTML file. If NULL, a
#'   temporary file is created and opened in the browser. Defaults to NULL.
#'
#' @importFrom jsonlite toJSON
#' @importFrom rstudioapi viewer
#' @return Invisibly returns the path to the generated HTML file.
#' @export

plot_glm_config <- function(aeme, path, output = NULL) {
  
  data("glm_aed_parameter_library", package = "AEME", envir = environment())
  
  cfg <- configuration(aeme)
  glm_nml <- cfg[["glm_aed"]][["hydrodynamic"]]
  aed_nml <- cfg[["glm_aed"]][["bgc"]]
  
  # Convenience: safe extraction with default
  safe <- function(lst, ..., default = NA) {
    keys <- c(...)
    val <- lst
    for (k in keys) {
      if (is.null(val) || !k %in% names(val)) return(default)
      val <- val[[k]]
    }
    val
  }
  
  # ---- 1. Extract GLM morphometry ----------------------------------------
  morph   <- glm_nml$morphometry
  H       <- morph$H
  A       <- morph$A
  lake_name  <- safe(morph, "lake_name", default = "unknown")
  latitude   <- safe(morph, "latitude", default = NA)
  longitude  <- safe(morph, "longitude", default = NA)
  base_elev  <- safe(morph, "base_elev", default = min(H))
  crest_elev <- safe(morph, "crest_elev", default = max(H))
  max_depth  <- crest_elev - base_elev
  
  init_profiles <- safe(glm_nml, "init_profiles", default = list())
  init_depth <- safe(init_profiles, "lake_depth", default = max_depth)
  
  # ---- 2. Sediment zones -------------------------------------------------
  sed          <- glm_nml$sediment
  zone_heights <- safe(sed, "zone_heights", default = numeric(0))
  n_zones      <- safe(sed, "n_zones", default = length(zone_heights))
  
  # ---- 3. GLM parameters -------------------------------------------------
  glm_params <- list(
    dt              = safe(glm_nml, "time", "dt"),
    nsave           = safe(glm_nml, "output", "nsave"),
    Kw              = safe(glm_nml, "light", "Kw"),
    ce              = safe(glm_nml, "meteorology", "ce"),
    ch              = safe(glm_nml, "meteorology", "ch"),
    cd              = safe(glm_nml, "meteorology", "cd"),
    coef_wind_stir  = safe(glm_nml, "mixing", "coef_wind_stir"),
    coef_mix_shear  = safe(glm_nml, "mixing", "coef_mix_shear"),
    coef_mix_conv   = safe(glm_nml, "mixing", "coef_mix_conv"),
    coef_mix_turb   = safe(glm_nml, "mixing", "coef_mix_turb"),
    coef_mix_KH     = safe(glm_nml, "mixing", "coef_mix_KH"),
    min_layer_thick = safe(glm_nml, "glm_setup", "min_layer_thick"),
    max_layer_thick = safe(glm_nml, "glm_setup", "max_layer_thick"),
    max_layers      = safe(glm_nml, "glm_setup", "max_layers"),
    start           = safe(glm_nml, "time", "start"),
    stop            = safe(glm_nml, "time", "stop"),
    num_inflows     = safe(glm_nml, "inflow", "num_inflows"),
    num_outlet      = safe(glm_nml, "outflow", "num_outlet"),
    light_mode      = safe(glm_nml, "light", "light_mode")
  )
  
  # ---- 4. AED module parameters ------------------------------------------
  aed <- aed_nml$aed  # the inner aed block
  
  aed_models <- safe(aed, "aed_models", "models", default = character(0))
  
  # Sediment flux (from aed_sed_const2d)
  sed_const <- safe(aed, "aed_sed_const2d", default = list())
  sed_cfg <- list(
    model        = safe(aed, "aed_sedflux", "sedflux_model", default = "unknown"),
    n_zones      = safe(sed_const, "n_zones", default = NA),
    active_zones = safe(sed_const, "active_zones", default = numeric(0)),
    fsed_oxy     = paste(safe(sed_const, "fsed_oxy", default = NA), collapse = ", "),
    fsed_amm     = paste(safe(sed_const, "fsed_amm", default = NA), collapse = ", "),
    fsed_nit     = paste(safe(sed_const, "fsed_nit", default = NA), collapse = ", "),
    fsed_frp     = paste(safe(sed_const, "fsed_frp", default = NA), collapse = ", ")
  )
  
  # Oxygen
  oxy <- safe(aed, "aed_oxygen", default = list())
  oxy_cfg <- list(
    oxy_initial   = safe(oxy, "oxy_initial"),
    oxy_min       = safe(oxy, "oxy_min"),
    oxy_max       = safe(oxy, "oxy_max"),
    ksed_oxy      = safe(oxy, "ksed_oxy"),
    theta_sed_oxy = safe(oxy, "theta_sed_oxy")
  )
  
  # Nitrogen
  nit <- safe(aed, "aed_nitrogen", default = list())
  nit_cfg <- list(
    amm_initial = safe(nit, "amm_initial"),
    nit_initial = safe(nit, "nit_initial"),
    rnitrif     = safe(nit, "rnitrif"),
    rdenit      = safe(nit, "rdenit")
  )
  
  # Phosphorus
  phs <- safe(aed, "aed_phosphorus", default = list())
  phs_cfg <- list(
    frp_initial = safe(phs, "frp_initial")
  )
  
  # Organic matter
  ogm <- safe(aed, "aed_organic_matter", default = list())
  ogm_cfg <- list(
    doc_initial = safe(ogm, "doc_initial"),
    poc_initial = safe(ogm, "poc_initial"),
    rdom_minerl = safe(ogm, "rdom_minerl"),
    f_an        = safe(ogm, "f_an"),
    w_pom       = safe(ogm, "w_pom")
  )
  
  # Phytoplankton
  phy <- safe(aed, "aed_phytoplankton", default = list())
  num_phytos <- safe(phy, "num_phytos", default = 0)
  
  # Phytoplankton parameters table (separate tibble in aed_nml)
  phyto_pars <- aed_nml$aed_phyto_pars
  phyto_groups <- character(0)
  phyto_key_params <- list()
  if (!is.null(phyto_pars) && nrow(phyto_pars) > 0) {
    # Column names are the group names (except the first col which is p_name)
    name_col <- names(phyto_pars)[1]
    group_cols <- setdiff(names(phyto_pars), name_col)
    phyto_groups <- group_cols
    
    # Extract a few key parameters per group for display
    key_rows <- c("R_growth", "Xcc", "theta_growth", "Pmax",
                  "w_p", "R_resp", "R_mort")
    for (g in group_cols) {
      vals <- list()
      for (pr in key_rows) {
        idx <- which(phyto_pars[[name_col]] == pr)
        if (length(idx) > 0) {
          vals[[pr]] <- phyto_pars[[g]][idx[1]]
        }
      }
      phyto_key_params[[g]] <- vals
    }
  }
  
  # Zooplankton
  zoo <- safe(aed, "aed_zooplankton", default = list())
  num_zoops <- safe(zoo, "num_zoops", default = 0)
  zoop_pars <- aed_nml$aed_zoop_pars
  zoop_groups <- character(0)
  if (!is.null(zoop_pars) && nrow(zoop_pars) > 0) {
    zoop_groups <- setdiff(names(zoop_pars), names(zoop_pars)[1])
  }
  
  # Macrophytes
  mac <- safe(aed, "aed_macrophyte", default = list())
  num_mphy <- safe(mac, "num_mphy", default = 0)
  mac_pars <- aed_nml$aed_macrophyte_pars
  mac_groups <- character(0)
  if (!is.null(mac_pars) && nrow(mac_pars) > 0) {
    mac_groups <- setdiff(names(mac_pars), names(mac_pars)[1])
  }
  
  # ---- 5. Build JSON data ------------------------------------------------
  lib_lookup <- as.list(glm_aed_parameter_library$label)
  names(lib_lookup) <- glm_aed_parameter_library$parameter
  
  config_data <- list(
    parameter_library = lib_lookup, # Add this line
    lake = list(
      name       = lake_name,
      latitude   = latitude,
      longitude  = longitude,
      max_depth  = max_depth,
      base_elev  = base_elev,
      crest_elev = crest_elev,
      H          = H,
      A          = A,
      zone_heights = zone_heights,
      n_zones      = n_zones,
      init_depth   = init_depth
    ),
    glm        = glm_params,
    aed_models = aed_models,
    sediment   = sed_cfg,
    oxygen     = oxy_cfg,
    nitrogen   = nit_cfg,
    phosphorus = phs_cfg,
    organic_matter = ogm_cfg,
    phytoplankton = list(
      num_groups  = num_phytos,
      groups      = phyto_groups,
      key_params  = phyto_key_params
    ),
    zooplankton = list(
      num_groups = num_zoops,
      groups     = zoop_groups
    ),
    macrophyte = list(
      num_groups    = num_mphy,
      groups        = mac_groups,
      active_zones  = safe(mac, "active_zones", default = numeric(0))
    )
  )
  
  config_json <- jsonlite::toJSON(config_data, auto_unbox = TRUE, 
                                  pretty = FALSE)
  
  # ---- 6. Generate HTML --------------------------------------------------
  html <- .generate_config_html(config_json)
  
  # ---- 7. Write and open -------------------------------------------------
  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
  }
  writeLines(html, output)
  
  if (interactive()) {
    rstudioapi::viewer(output)
  }
  cli::cli_alert_success("Config visualisation saved to {.file {output}}")
  invisible(output)
}


#' Generate the HTML visualisation
#' @keywords internal
#' @noRd
.generate_config_html <- function(config_json) {
  
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>GLM-AED Configuration</title>
<style>
  @import url("https://fonts.googleapis.com/css2?family=DM+Sans:ital,wght@0,400;0,500;0,600&family=JetBrains+Mono:wght@400;500&display=swap");

  :root {
    --bg: #fafaf8; --bg2: #f0efeb; --bg3: #e6e5e0;
    --tx: #2c2c2a; --tx2: #5f5e5a; --tx3: #888780;
    --bd: rgba(0,0,0,0.1);
    --blue50: #E6F1FB; --blue200: #85B7EB; --blue600: #185FA5; --blue800: #0C447C;
    --teal50: #E1F5EE; --teal200: #5DCAA5; --teal600: #0F6E56; --teal800: #085041;
    --coral50: #FAECE7; --coral200: #F0997B; --coral600: #993C1D; --coral800: #712B13;
    --amber50: #FAEEDA; --amber200: #EF9F27; --amber600: #854F0B; --amber800: #633806;
    --green50: #EAF3DE; --green200: #97C459; --green600: #3B6D11; --green800: #27500A;
    --purple50: #EEEDFE; --purple200: #AFA9EC; --purple600: #534AB7; --purple800: #3C3489;
    --gray50: #F1EFE8; --gray200: #B4B2A9; --gray600: #5F5E5A; --gray800: #444441;
    --pink50: #FBEAF0; --pink200: #ED93B1; --pink600: #993556; --pink800: #72243E;
    --red50: #FCEBEB; --red600: #A32D2D;
    --font: "DM Sans", sans-serif;
    --mono: "JetBrains Mono", monospace;
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --bg: #1a1a18; --bg2: #2a2a27; --bg3: #3a3a36;
      --tx: #e8e6dc; --tx2: #b4b2a9; --tx3: #888780;
      --bd: rgba(255,255,255,0.1);
      --blue50: #0C447C; --blue200: #85B7EB; --blue600: #85B7EB; --blue800: #E6F1FB;
      --teal50: #085041; --teal200: #5DCAA5; --teal600: #5DCAA5; --teal800: #E1F5EE;
      --coral50: #712B13; --coral200: #F0997B; --coral600: #F0997B; --coral800: #FAECE7;
      --amber50: #633806; --amber200: #EF9F27; --amber600: #EF9F27; --amber800: #FAEEDA;
      --green50: #27500A; --green200: #97C459; --green600: #97C459; --green800: #EAF3DE;
      --purple50: #3C3489; --purple200: #AFA9EC; --purple600: #AFA9EC; --purple800: #EEEDFE;
      --gray50: #444441; --gray200: #B4B2A9; --gray600: #B4B2A9; --gray800: #F1EFE8;
      --pink50: #72243E; --pink200: #ED93B1; --pink600: #ED93B1; --pink800: #FBEAF0;
    }
  }

  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: var(--font); background: var(--bg); color: var(--tx);
    line-height: 1.5; padding: 24px; max-width: 1100px; margin: 0 auto;
  }

  .header { margin-bottom: 28px; }
  .header h1 { font-size: 22px; font-weight: 600; margin-bottom: 4px; }
  .header p { font-size: 13px; color: var(--tx2); }

  .grid { display: grid; grid-template-columns: 340px 1fr; gap: 24px; }
  @media (max-width: 780px) { .grid { grid-template-columns: 1fr; } }

  .card {
    background: var(--bg2); border: 1px solid var(--bd);
    border-radius: 12px; padding: 20px; margin-bottom: 16px;
  }
  .card h2 { font-size: 14px; font-weight: 600; margin-bottom: 12px; }
  .card h3 {
    font-size: 11px; font-weight: 500; color: var(--tx3);
    margin: 14px 0 6px; text-transform: uppercase; letter-spacing: 0.6px;
  }

  .param-row {
    display: flex; justify-content: space-between; align-items: center;
    padding: 3px 0; font-size: 13px; border-bottom: 1px solid var(--bd);
  }
  .param-row:last-child { border-bottom: none; }
  .param-key { color: var(--tx2); }
  .param-val { font-family: var(--mono); font-size: 12px; font-weight: 500; text-align: right; }

  .module-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
  @media (max-width: 780px) { .module-grid { grid-template-columns: 1fr; } }
  .module-full { grid-column: 1 / -1; }

  .module {
    border-radius: 10px; padding: 14px; border: 1px solid;
    transition: transform 0.15s, box-shadow 0.15s;
  }
  .module:hover { transform: translateY(-1px); box-shadow: 0 4px 12px rgba(0,0,0,0.06); }
  .module h3 {
    font-size: 13px; font-weight: 600; margin: 0 0 8px;
    text-transform: none; letter-spacing: 0;
  }
  .module .param-row { font-size: 12px; }
  .module .param-val { font-size: 11px; }

  .m-oxygen    { background: var(--coral50); border-color: var(--coral200); }
  .m-oxygen h3, .m-oxygen .param-val { color: var(--coral800); }
  .m-oxygen .param-key { color: var(--coral600); }

  .m-sedflux   { background: var(--coral50); border-color: var(--coral200); }
  .m-sedflux h3, .m-sedflux .param-val { color: var(--coral800); }
  .m-sedflux .param-key { color: var(--coral600); }

  .m-nitrogen  { background: var(--teal50); border-color: var(--teal200); }
  .m-nitrogen h3, .m-nitrogen .param-val { color: var(--teal800); }
  .m-nitrogen .param-key { color: var(--teal600); }

  .m-phosphorus { background: var(--teal50); border-color: var(--teal200); }
  .m-phosphorus h3, .m-phosphorus .param-val { color: var(--teal800); }
  .m-phosphorus .param-key { color: var(--teal600); }

  .m-orgmatter { background: var(--amber50); border-color: var(--amber200); }
  .m-orgmatter h3, .m-orgmatter .param-val { color: var(--amber800); }
  .m-orgmatter .param-key { color: var(--amber600); }

  .m-phyto     { background: var(--green50); border-color: var(--green200); }
  .m-phyto h3, .m-phyto .param-val { color: var(--green800); }
  .m-phyto .param-key { color: var(--green600); }

  .m-zoop      { background: var(--purple50); border-color: var(--purple200); }
  .m-zoop h3, .m-zoop .param-val { color: var(--purple800); }
  .m-zoop .param-key { color: var(--purple600); }

  .m-macro     { background: var(--pink50); border-color: var(--pink200); }
  .m-macro h3, .m-macro .param-val { color: var(--pink800); }
  .m-macro .param-key { color: var(--pink600); }

  .m-silica, .m-totals { background: var(--gray50); border-color: var(--gray200); }
  .m-silica h3, .m-silica .param-val, .m-totals h3, .m-totals .param-val { color: var(--gray800); }
  .m-silica .param-key, .m-totals .param-key { color: var(--gray600); }

  .chips { display: flex; gap: 6px; flex-wrap: wrap; margin-top: 8px; }
  .chip {
    font-size: 11px; font-weight: 500; padding: 3px 10px;
    border-radius: 20px; border: 1px solid;
  }
  .chip-cyano    { background: var(--coral50); color: var(--coral800); border-color: var(--coral200); }
  .chip-green    { background: var(--teal50); color: var(--teal800); border-color: var(--teal200); }
  .chip-diatom   { background: var(--blue50); color: var(--blue800); border-color: var(--blue200); }
  .chip-default  { background: var(--gray50); color: var(--gray800); border-color: var(--gray200); }
  .chip-zoop     { background: var(--purple50); color: var(--purple800); border-color: var(--purple200); }
  .chip-macro    { background: var(--pink50); color: var(--pink800); border-color: var(--pink200); }

  .phyto-table { width: 100%; margin-top: 10px; border-collapse: collapse; font-size: 11px; }
  .phyto-table th {
    text-align: left; font-weight: 500; color: var(--green600);
    border-bottom: 1px solid var(--green200); padding: 3px 6px;
    font-family: var(--mono); font-size: 10px;
  }
  .phyto-table td {
    padding: 2px 6px; font-family: var(--mono); color: var(--green800);
    border-bottom: 1px solid rgba(0,0,0,0.04);
  }
  .phyto-table td:first-child { color: var(--green600); font-weight: 400; }

  .hyps-svg { width: 100%; height: auto; display: block; }
  .zone-label { font-family: var(--mono); font-size: 10px; font-weight: 500; }
  .zone-fill { opacity: 0.15; }
  .zone-line { stroke-dasharray: 4 3; stroke-width: 0.8; }
  .hyps-path { fill: none; stroke-width: 1.8; stroke-linecap: round; }
  .hyps-area { opacity: 0.12; }
  .axis-text { font-family: var(--mono); font-size: 10px; fill: var(--tx3); }
  .axis-label { font-family: var(--font); font-size: 11px; fill: var(--tx2); font-weight: 500; }
  .axis-line { stroke: var(--bd); stroke-width: 0.5; }
  .grid-line { stroke: var(--bd); stroke-width: 0.3; }

  .warn-banner {
    background: var(--amber50); border: 1px solid var(--amber200);
    border-radius: 10px; padding: 14px 16px; margin-top: 16px;
  }
  .warn-banner p {
    font-size: 12px; color: var(--amber800); margin-bottom: 6px;
  }
  .warn-banner p:last-child { margin-bottom: 0; }
  .warn-banner strong { font-weight: 600; }
</style>
</head>
<body>

<div class="header">
  <h1 id="title"></h1>
  <p id="subtitle"></p>
</div>

<div class="grid">
  <div>
    <div class="card">
      <h2>Hypsograph &amp; sediment zones</h2>
      <div id="hyps-container"></div>
    </div>
    <div class="card" id="glm-params"></div>
  </div>
  <div>
    <div class="card">
      <h2>AED biogeochemical modules</h2>
      <div class="module-grid" id="aed-modules"></div>
    </div>
    <div id="warnings"></div>
  </div>
</div>

<script>
const CFG = ', config_json, ';

// ===== Header =====
document.getElementById("title").textContent =
  "GLM-AED configuration \\u2014 " + (CFG.lake.name || "unnamed lake");
document.getElementById("subtitle").textContent =
  [
    CFG.lake.latitude ? CFG.lake.latitude.toFixed(3) + "\\u00b0, " +
      CFG.lake.longitude.toFixed(3) + "\\u00b0" : null,
    "Max depth: " + CFG.lake.max_depth.toFixed(1) + " m",
    CFG.glm.start + " to " + CFG.glm.stop,
  ].filter(Boolean).join("  \\u00b7  ");

// ===== Hypsograph SVG =====
(function() {
  const H = CFG.lake.H;
  const A = CFG.lake.A;
  const base = CFG.lake.base_elev;
  const crest = CFG.lake.crest_elev;
  const initDepth = CFG.lake.init_depth;
  const zones = CFG.lake.zone_heights || [];

  // Surface elevation = base_elev + init_depth
  // Depth convention: 0 at water surface, NEGATIVE below, POSITIVE above
  // depth[i] = H[i] - surfElev
  //   e.g. lake bottom: H = base_elev -> depth = base - (base + initDepth) = -initDepth
  //   e.g. crest: H = crest_elev -> depth = crest - surfElev (positive if crest > surfElev)
  const surfElev = base + initDepth;
  const depthVals = H.map(h => h - surfElev);  // negative below, positive above
  const areas = A.map(a => a / 1e6);
  const maxA = Math.max(...areas);

  // Y-axis range
  const minVal = Math.min(...depthVals);  // most negative (deepest point)
  const maxVal = Math.max(...depthVals);  // most positive (crest)
  const valRange = maxVal - minVal;

  const W = 310, HH = 380;
  const ml = 42, mr = 48, mt = 25, mb = 35;
  const pw = W - ml - mr, ph = HH - mt - mb;

  // sx: area -> x pixel
  const sx = v => ml + (v / (maxA || 1)) * pw;
  // sy: depth value -> y pixel (positive/crest at top, negative/deep at bottom)
  const sy = v => mt + ((maxVal - v) / (valRange || 1)) * ph;

  let svg = \'<svg class="hyps-svg" viewBox="0 0 \' + W + " " + HH +
    \'" xmlns="http://www.w3.org/2000/svg">\';

  // Grid lines at nice intervals
  const ngy = 6;
  const rawStep = valRange / ngy;
  const step = rawStep >= 5 ? Math.round(rawStep / 5) * 5
             : rawStep >= 1 ? Math.round(rawStep)
             : Math.round(rawStep * 10) / 10 || 1;
  const gridStart = Math.floor(minVal / step) * step;
  const gridEnd = Math.ceil(maxVal / step) * step;

  for (let v = gridStart; v <= gridEnd; v += step) {
    if (v < minVal - step * 0.3 || v > maxVal + step * 0.3) continue;
    const y = sy(v);
    svg += \'<line x1="\' + ml + \'" y1="\' + y + \'" x2="\' + (W - mr) +
      \'" y2="\' + y + \'" class="grid-line"/>\';

    // Left axis: depth (0 at surface, negative below, positive above)
    const depthLabel = v > 0 ? "+" + v.toFixed(0) : v.toFixed(0);
    svg += \'<text x="\' + (ml - 4) + \'" y="\' + (y + 3) +
      \'" text-anchor="end" class="axis-text">\' + depthLabel + "</text>";

    // Right axis: elevation H (raw elevation = surfElev + depth_value)
    const elev = surfElev + v;
    svg += \'<text x="\' + (W - mr + 4) + \'" y="\' + (y + 3) +
      \'" text-anchor="start" class="axis-text">\' + elev.toFixed(1) + "</text>";
  }

  // Water surface line at depth = 0
  const y0 = sy(0);
  svg += \'<line x1="\' + ml + \'" y1="\' + y0 + \'" x2="\' + (W - mr) +
    \'" y2="\' + y0 + \'" stroke="#185FA5" stroke-width="1.2" stroke-dasharray="6 3" opacity="0.6"/>\';
  svg += \'<text x="\' + (ml + pw / 2) + \'" y="\' + (y0 - 5) +
    \'" text-anchor="middle" style="font-family:var(--mono);font-size:9px;fill:#185FA5;font-weight:500">water surface (0 m)</text>\';

  // Sediment zone fills — zone_heights are heights above lake bottom (base_elev)
  // Convert to depth value: depthVal = zone_height - initDepth (negative if below surface)
  const zoneColors = ["#85B7EB", "#5DCAA5", "#EF9F27", "#AFA9EC", "#F0997B"];
  for (let i = zones.length - 1; i >= 0; i--) {
    const topVal = zones[i] - initDepth;   // depth value at top of zone
    const botVal = (i === 0) ? minVal : zones[i - 1] - initDepth;  // depth value at bottom
    const y1 = sy(topVal);
    const y2 = sy(botVal);
    const col = zoneColors[i % zoneColors.length];
    svg += \'<rect x="\' + ml + \'" y="\' + Math.min(y1, y2) + \'" width="\' + pw +
      \'" height="\' + Math.abs(y2 - y1) +
      \'" fill="\' + col + \'" class="zone-fill"/>\';
    svg += \'<line x1="\' + ml + \'" y1="\' + y1 + \'" x2="\' + (W - mr) +
      \'" y2="\' + y1 + \'" stroke="\' + col + \'" class="zone-line"/>\';
    svg += \'<text x="\' + (ml + 4) + \'" y="\' + (y1 + 12) +
      \'" text-anchor="start" class="zone-label" fill="\' + col +
      \'">zone \' + (i + 1) + " (" + topVal.toFixed(1) + " m)</text>";
  }

  // Hypsograph area fill — trace the full profile including crest
  let areaPath = "M" + sx(0) + " " + sy(depthVals[0]);
  for (let i = 0; i < depthVals.length; i++) {
    areaPath += " L" + sx(areas[i]) + " " + sy(depthVals[i]);
  }
  areaPath += " L" + sx(0) + " " + sy(depthVals[depthVals.length - 1]) + " Z";
  svg += \'<path d="\' + areaPath + \'" fill="#185FA5" class="hyps-area"/>\';

  // Hypsograph line — full profile
  let linePath = "M" + sx(areas[0]) + " " + sy(depthVals[0]);
  for (let i = 1; i < depthVals.length; i++) {
    linePath += " L" + sx(areas[i]) + " " + sy(depthVals[i]);
  }
  svg += \'<path d="\' + linePath + \'" stroke="#185FA5" class="hyps-path"/>\';

  // Left axis line
  svg += \'<line x1="\' + ml + \'" y1="\' + mt + \'" x2="\' + ml +
    \'" y2="\' + (HH - mb) + \'" class="axis-line"/>\';
  // Right axis line
  svg += \'<line x1="\' + (W - mr) + \'" y1="\' + mt + \'" x2="\' + (W - mr) +
    \'" y2="\' + (HH - mb) + \'" class="axis-line"/>\';
  // Bottom axis line
  svg += \'<line x1="\' + ml + \'" y1="\' + (HH - mb) + \'" x2="\' + (W - mr) +
    \'" y2="\' + (HH - mb) + \'" class="axis-line"/>\';

  // Axis labels
  svg += \'<text x="\' + (ml + pw / 2) + \'" y="\' + (HH - 4) +
    \'" text-anchor="middle" class="axis-label">Area (km\\u00b2)</text>\';
  svg += \'<text x="10" y="\' + (mt + ph / 2) +
    \'" text-anchor="middle" class="axis-label" transform="rotate(-90,10,\' +
    (mt + ph / 2) + \')">Depth (m)</text>\';
  svg += \'<text x="\' + (W - 8) + \'" y="\' + (mt + ph / 2) +
    \'" text-anchor="middle" class="axis-label" transform="rotate(90,\' +
    (W - 8) + "," + (mt + ph / 2) + \')">Elevation (m)</text>\';

  // X axis ticks
  const ngx = 4;
  for (let i = 0; i <= ngx; i++) {
    const a = (maxA / ngx) * i;
    svg += \'<text x="\' + sx(a) + \'" y="\' + (HH - mb + 14) +
      \'" text-anchor="middle" class="axis-text">\' +
      (a < 1 ? a.toFixed(2) : a.toFixed(1)) + "</text>";
  }

  svg += "</svg>";
  document.getElementById("hyps-container").innerHTML = svg;
})();

// ===== GLM parameters =====
(function() {
  const g = CFG.glm;
  const sections = [
    ["Simulation", [
      ["Period", g.start + " to " + g.stop],
      ["Timestep", g.dt + " s"],
      ["Output interval", "every " + g.nsave + " timesteps"],
    ]],
    ["Vertical structure", [
      ["Max layers", g.max_layers],
      ["Layer thickness", g.min_layer_thick + " \\u2013 " + g.max_layer_thick + " m"],
    ]],
    ["Mixing", [
      ["wind_stir", g.coef_wind_stir],
      ["mix_shear", g.coef_mix_shear],
      ["mix_conv", g.coef_mix_conv],
      ["mix_turb", g.coef_mix_turb],
      ["mix_KH", g.coef_mix_KH],
    ]],
    ["Meteorology", [
      ["ce / ch / cd", g.ce + " / " + g.ch + " / " + g.cd],
    ]],
    ["Light", [
      ["Kw", g.Kw + " m\\u207b\\u00b9"],
      ["Light mode", g.light_mode],
    ]],
    ["Flows", [
      ["Inflows", g.num_inflows],
      ["Outlets", g.num_outlet],
    ]],
  ];

  let html = "<h2>GLM physical setup</h2>";
  for (const [section, rows] of sections) {
    html += "<h3>" + section + "</h3>";
    for (const [k, v] of rows) {
      html += \'<div class="param-row"><span class="param-key">\' + k +
        \'</span><span class="param-val">\' + v + "</span></div>";
    }
  }
  document.getElementById("glm-params").innerHTML = html;
})();

// ===== AED modules =====
(function() {
  const el = document.getElementById("aed-modules");
  let html = "";

  function row(k, v) {
    return \'<div class="param-row"><span class="param-key">\' + k +
      \'</span><span class="param-val">\' + (v != null ? v : "\\u2014") +
      "</span></div>";
  }

  // Sediment flux
  html += \'<div class="module m-sedflux"><h3>Sediment flux</h3>\';
  html += row("Model", CFG.sediment.model);
  html += row("n_zones", CFG.sediment.n_zones);
  const az = CFG.sediment.active_zones;
  html += row("active_zones", az && az.length ? az.join(", ") : "\\u2014");
  html += row("fsed_oxy", CFG.sediment.fsed_oxy);
  html += row("fsed_amm", CFG.sediment.fsed_amm);
  html += row("fsed_nit", CFG.sediment.fsed_nit);
  html += row("fsed_frp", CFG.sediment.fsed_frp);
  html += "</div>";

  // Oxygen
  html += \'<div class="module m-oxygen"><h3>Oxygen</h3>\';
  html += row("oxy_initial", CFG.oxygen.oxy_initial);
  html += row("oxy_min / max", CFG.oxygen.oxy_min + " / " + CFG.oxygen.oxy_max);
  html += row("ksed_oxy", CFG.oxygen.ksed_oxy);
  html += row("\\u03b8_sed_oxy", CFG.oxygen.theta_sed_oxy);
  html += "</div>";

  // Nitrogen
  html += \'<div class="module m-nitrogen"><h3>Nitrogen</h3>\';
  html += row("amm_initial", CFG.nitrogen.amm_initial);
  html += row("nit_initial", CFG.nitrogen.nit_initial);
  html += row("rnitrif", CFG.nitrogen.rnitrif);
  html += row("rdenit", CFG.nitrogen.rdenit);
  html += "</div>";

  // Phosphorus
  html += \'<div class="module m-phosphorus"><h3>Phosphorus</h3>\';
  html += row("frp_initial", CFG.phosphorus.frp_initial);
  html += "</div>";

  // Organic matter
  html += \'<div class="module m-orgmatter"><h3>Organic matter</h3>\';
  html += row("doc_initial", CFG.organic_matter.doc_initial);
  html += row("poc_initial", CFG.organic_matter.poc_initial);
  html += row("rdom_minerl", CFG.organic_matter.rdom_minerl);
  html += row("f_an", CFG.organic_matter.f_an);
  html += row("w_pom", CFG.organic_matter.w_pom + " m/d");
  html += "</div>";

  // Silica
  if (CFG.aed_models.some(m => m.includes("silica"))) {
    html += \'<div class="module m-silica"><h3>Silica</h3>\';
    html += row("Included", "yes");
    html += "</div>";
  }

  // Phytoplankton (full width)
  html += \'<div class="module m-phyto module-full"><h3>Phytoplankton (\' +
    CFG.phytoplankton.num_groups + \' groups)</h3>\';
  html += \'<div class="chips">\';
  const phytoChipClass = { cyano: "chip-cyano", green: "chip-green", diatom: "chip-diatom" };
  for (const g of (CFG.phytoplankton.groups || [])) {
    const cls = phytoChipClass[g] || "chip-default";
    html += \'<span class="chip \' + cls + \'">\' + g + "</span>";
  }
  html += "</div>";

  // Key params table
  const kp = CFG.phytoplankton.key_params;
  const groups = CFG.phytoplankton.groups || [];
  if (kp && groups.length > 0) {
    const paramNames = Object.keys(kp[groups[0]] || {});
    if (paramNames.length > 0) {
      html += \'<table class="phyto-table"><tr><th>parameter</th>\';
      for (const g of groups) html += "<th>" + g + "</th>";
      html += "</tr>";
      for (const p of paramNames) {
        html += "<tr><td>" + p + "</td>";
        for (const g of groups) {
          const v = kp[g] && kp[g][p] != null ? kp[g][p] : "\\u2014";
          html += "<td>" + v + "</td>";
        }
        html += "</tr>";
      }
      html += "</table>";
    }
  }
  html += "</div>";

  // Zooplankton
  if (CFG.zooplankton.num_groups > 0) {
    html += \'<div class="module m-zoop"><h3>Zooplankton (\' +
      CFG.zooplankton.num_groups + \' groups)</h3>\';
    html += \'<div class="chips">\';
    for (const g of (CFG.zooplankton.groups || [])) {
      html += \'<span class="chip chip-zoop">\' + g + "</span>";
    }
    html += "</div></div>";
  }

  // Macrophytes
  if (CFG.macrophyte.num_groups > 0) {
    html += \'<div class="module m-macro"><h3>Macrophytes (\' +
      CFG.macrophyte.num_groups + \' groups)</h3>\';
    html += \'<div class="chips">\';
    for (const g of (CFG.macrophyte.groups || [])) {
      html += \'<span class="chip chip-macro">\' + g + "</span>";
    }
    html += "</div>";
    const az = CFG.macrophyte.active_zones;
    if (az && az.length > 0) {
      html += row("Active zones", az.join(", "));
    }
    html += "</div>";
  }

  // Totals
  if (CFG.aed_models.some(m => m.includes("totals"))) {
    html += \'<div class="module m-totals"><h3>Totals</h3>\';
    html += row("Computed", "TN, TP, TOC");
    html += "</div>";
  }

  el.innerHTML = html;
})();

// ===== Warnings =====
(function() {
  const warns = [];

  const fsedStr = CFG.sediment.fsed_oxy;
  if (fsedStr) {
    const vals = String(fsedStr).split(",").map(s => parseFloat(s.trim())).filter(v => !isNaN(v));
    const allSame = vals.length > 1 && vals.every(v => v === vals[0]);
    if (allSame) {
      warns.push("<strong>fsed_oxy is uniform</strong> across all " +
        CFG.lake.n_zones + " sediment zones (" + vals[0] +
        " mmol/m\\u00b2/d). Consider varying by zone depth.");
    }
    if (vals.length > 0 && Math.abs(vals[0]) >= 20) {
      warns.push("<strong>|fsed_oxy| = " + Math.abs(vals[0]) +
        "</strong> is a strong sediment oxygen demand. Typical range is " +
        "5\\u201320 mmol/m\\u00b2/d for mesotrophic lakes.");
    }
  }

  const ce = parseFloat(CFG.glm.ce);
  const ch = parseFloat(CFG.glm.ch);
  const cd = parseFloat(CFG.glm.cd);
  if (!isNaN(ce) && ce === ch && ch === cd) {
    warns.push("<strong>ce = ch = cd = " + ce + "</strong> \\u2014 fixed and " +
      "identical. Atmospheric exchange has no stability dependence.");
  }

  if (warns.length) {
    document.getElementById("warnings").innerHTML =
      \'<div class="warn-banner">\' +
      warns.map(w => "<p>\\u26a0\\ufe0f " + w + "</p>").join("") + "</div>";
  }
})();
</script>
</body>
</html>')
}