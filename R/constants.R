#' Floor applied before log10() in the TLI component functions below. A
#' simulated concentration can legitimately hit exactly zero (or a rounding
#' artefact can push it just negative) at a given timestep - e.g. near-total
#' nutrient depletion - and log10() of that is -Inf/NaN, which propagates
#' into a non-finite TLI value and aborts an entire PEST++ forward run over
#' one timestep. 1e-6 is far below any physically meaningful concentration
#' in these units (µg/L chla, mg/L*1000 = µg/L for N/P), so it only guards
#' the log10() singularity and does not distort real values.
#' @noRd
tli_conc_floor <- 1e-6
