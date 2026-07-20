
# ------------------------------------------------------------------------------
# Internal colour palettes (hex values)
# ------------------------------------------------------------------------------

#' @keywords internal
.pal_spectral <- c(
  "#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B",
  "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD",
  "#5E4FA2"
)

#' @keywords internal
.pal_plasma <- c(
  "#0D0887FF", "#42049EFF", "#6A00A8FF", "#900DA4FF", "#B12A90FF",
  "#CC4678FF", "#E16462FF", "#F1844BFF", "#FCA636FF", "#FCCE25FF",
  "#F0F921FF"
)

#' @keywords internal
.pal_viridis <- c(
  "#440154FF", "#482576FF", "#414487FF", "#35608DFF", "#2A788EFF",
  "#21908CFF", "#22A884FF", "#43BF71FF", "#7AD151FF", "#BBDF27FF",
  "#FDE725FF"
)

#' @keywords internal
.pal_inferno <- c(
  "#000004FF", "#170C3AFF", "#420A68FF", "#6B186EFF", "#932667FF",
  "#BB3754FF", "#DD513AFF", "#F3771AFF", "#FCA50AFF", "#F6D645FF",
  "#FCFFA4FF"
)

#' @keywords internal
.pal_thermal <- c(
  "#042333FF", "#0F326BFF", "#40349FFF", "#684396FF", "#8B538DFF",
  "#B05F82FF", "#D66C6BFF", "#F2814DFF", "#FCA63CFF", "#F7CF45FF",
  "#E8FA5BFF"
)

#' @keywords internal
.pal_oxy <- c(
  "grey40", "indianred2", "#F2814DFF", "#F7CF45FF", "lightyellow1",
  "#BBDBE5FF", "palegreen2", "#a7db5b", "pink"
)

#' @keywords internal
.pal_deep <- c(
  "#92D8A4FF", "#65C2A4FF", "#52A8A3FF", "#488E9EFF", "#407498FF",
  "#3E5A92FF", "#41407BFF", "#382D51FF", "#281A2CFF"
)

#' @keywords internal
.pal_dense <- c(
  "#E6F1F1FF", "#BBDBE5FF", "#96C5E2FF", "#7BACE4FF", "#7390E3FF",
  "#7771D5FF", "#7953BAFF", "#743A98FF", "#682471FF", "#531547FF",
  "#360E24FF"
)

#' @keywords internal
.pal_haline <- c(
  "#2A186CFF", "#2828A2FF", "#0D4E96FF", "#18668CFF", "#2D7C89FF",
  "#3B9287FF", "#4AAA81FF", "#64C072FF", "#94D35DFF", "#CFE06CFF",
  "#FDEF9AFF"
)

#' @keywords internal
.pal_solar <- c(
  "#331418FF", "#531E22FF", "#732724FF", "#8F341EFF", "#A54A17FF",
  "#B66313FF", "#C47F15FF", "#CF9B1DFF", "#D8BA2AFF", "#DEDA39FF",
  "#E1FD4BFF"
)

#' @keywords internal
.pal_turbid <- c(
  "#E9F6ABFF", "#DBD886FF", "#CFBC66FF", "#C3A04DFF", "#B58740FF",
  "#A1703BFF", "#895D3AFF", "#704D37FF", "#563E30FF", "#3B2F27FF",
  "#221F1BFF"
)

#' @keywords internal
.pal_chla <- c(
  "#D7F9D0FF", "#B7E2ABFF", "#96CD8AFF", "#71BA6BFF", "#44A855FF",
  "#129450FF", "#097C4AFF", "#156641FF", "#1A5034FF", "#183A25FF",
  "#122414FF"
)

#' @keywords internal
.pal_ice <- c(
  "#040613FF", "#1B1B37FF", "#302F5FFF", "#3D4389FF", "#3E5EA9FF",
  "#427AB7FF", "#5296C1FF", "#6AB0CBFF", "#8CCBD6FF", "#BBE3E6FF",
  "#EAFDFDFF"
)

#' @keywords internal
.pal_cyano <- c(
  "#3D4389FF", "#427AB7FF", "#6AB0CBFF", "#65C2A4FF",
  "#00dddd", "#00eeee", "#00ffff", "#fcffd4"
)

#' @keywords internal
.pal_chlor <- c(
  "#3D4389FF", "#427AB7FF", "#6AB0CBFF", "#65C2A4FF",
  "#71BA6BFF", "#94D35DFF", "#7AD151FF", "chartreuse"
)

#' @keywords internal
.pal_turbid <- c(
  "#3E5A92FF", "#488E9EFF", "#65C2A4FF", "#C9EAB1FF", "#E9F6ABFF",
  "#DBD886FF", "#CFBC66FF", "#B58740FF", "#895D3AFF", "#563E30FF"
)

#' @keywords internal
.pal_tdiff <- c(
  "#ADD8E6", "#BDDBCD", "#CDDFB5", "#DDE29D", "#EEE685",
  "#EEC57C", "#EEA474", "#EE836B", "#EE6363"
)

#' Default heatmap colour palettes
#'
#' @keywords internal
#' @noRd
.hm_palettes <- list(
  HYD_temp = .pal_thermal,
  HYD_tmpdif = .pal_tdiff,
  CHM_oxysat = .pal_oxy,
  CHM_oxy = .pal_oxy,
  CHM_ph     = rev(.pal_plasma),
  CHM_orp    = .pal_ice,
  Cond   = .pal_viridis,
  SpCond = .pal_viridis,
  TurbRT = .pal_turbid,
  PHY_tchla = .pal_chla,
  PHY_green = .pal_chla,
  PHY_cyano = .pal_cyano,
  CHM_pc02   = .pal_inferno,
  BIO_trtsts = rev(.pal_inferno),
  
  # Additional palettes
  spectral = .pal_spectral,
  plasma   = .pal_plasma,
  viridis  = .pal_viridis,
  inferno  = .pal_inferno,
  
  default = .pal_viridis
)
