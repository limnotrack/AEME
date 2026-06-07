#' Calculate TLI chlorophyll component
#' @param chla Chlorophyll-a concentration in µg/L
#' @return TLI chlorophyll component
calc_tli_chla <- function(chla) {
  2.22 + 2.54 * log10(chla)
}

#' Calculate TLI nitrogen component
#' @param tn Total nitrogen concentration in mg/L
#' @return TLI nitrogen component
calc_tli_n <- function(tn) {
  -3.61 + 3.01 * log10(tn * 1000)
}

#' Calculate TLI phosphorus component
#' @param tp Total phosphorus concentration in mg/L
#' @return TLI phosphorus component
calc_tli_p <- function(tp) {
  0.218 + 2.92 * log10(tp * 1000)
}

#' Calculate TLI secchi component
#' @param secchi Secchi depth in meters
#' @return TLI secchi component
calc_tli_secchi <- function(secchi) {
  if (length(secchi) < 1) return(NA)
  5.56 + 2.6 * log10(1/secchi - 1/40)
}

#' Calculate TLI 3
#' @param chla Chlorophyll-a concentration in µg/L
#' @param tn Total nitrogen concentration in mg/L
#' @param tp Total phosphorus concentration in mg/L
#' @return TLI 3
#' @export
calc_tli3 <- function(chla, tn, tp) {
  lke_chla <- calc_tli_chla(chla)
  lke_n <- calc_tli_n(tn)
  lke_p <- calc_tli_p(tp)
  (lke_chla + lke_n + lke_p) / 3
}

#' Calculate TLI 4
#' @param chla Chlorophyll-a concentration in µg/L
#' @param tn Total nitrogen concentration in mg/L
#' @param tp Total phosphorus concentration in mg/L
#' @param secchi Secchi depth in meters
#' @return TLI 4
#' @export
calc_tli4 <- function(chla, tn, tp, secchi) {
  lke_chla <- calc_tli_chla(chla)
  lke_n <- calc_tli_n(tn)
  lke_p <- calc_tli_p(tp)
  lke_secchi <- calc_tli_secchi(secchi)
  (lke_chla + lke_n + lke_p + lke_secchi) / 4
}