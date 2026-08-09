# One-off script: add a `simstrat_aed2` column to key_naming.csv.
# AED2 biogeochemical variable names are copied from the existing `glm_aed2`
# column (AED2's module/variable naming is shared regardless of the host
# physical model). Physical/flux variables are mapped to Simstrat's native
# output variable names confirmed from strat_outputfile.f90 and the
# TestCase_LakeZurich reference output file names (T, S, HA, HK, HV, Rad0).
# Everything else is left blank, matching the existing convention for
# variables not available from a given model.

kn <- read.csv("data-raw/key_naming.csv", stringsAsFactors = FALSE,
                colClasses = "character", na.strings = NULL)

kn$simstrat_aed2 <- ""

# AED2 biogeochemical variables: reuse glm_aed2 naming
kn$simstrat_aed2[kn$glm_aed2 != ""] <- kn$glm_aed2[kn$glm_aed2 != ""]

# Physical / flux variables specific to Simstrat's own output. Confirmed
# against the canonical output_var_names list + case-statement descriptions
# in strat_inputfile.f90 (lines ~178-303):
#   V, U, T, S, num, nuh, NN, k, eps, P, B, Ps, HA, HW, HK, HV, Rad0,
#   TotalIceH, BlackIceH, WhiteIceH, SnowH, WaterH, Qvert, Eseiche
# HA = incoming atmospheric longwave only (not net longwave), HW = outgoing
# longwave from the water surface, HK = sensible heat flux, HV = latent
# heat flux, Rad0 = shortwave at surface. LKE_Qlw (net longwave) has no
# single matching native variable (net = HA - HW), so it is left blank
# rather than mapped incorrectly. HYD_dens has no direct Simstrat output
# variable (not in the list above), so any glm_aed2-inherited value is
# explicitly cleared below.
phys_map <- c(
  HYD_temp   = "T",
  HYD_surft  = "T",
  CHM_salt   = "S",
  LKE_Qh     = "HK",
  LKE_Qe     = "HV",
  LKE_Qsw    = "Rad0",
  RAD_par    = "Rad0"
)
idx <- match(names(phys_map), kn$name)
kn$simstrat_aed2[idx[!is.na(idx)]] <- phys_map[!is.na(idx)]

# Clear incorrect glm_aed2-inherited values for physical vars Simstrat does
# not natively output, or where the single native variable would be
# misleading (LKE_Qlw is a *net* longwave flux; Simstrat only exposes the
# incoming (HA) and outgoing (HW) components separately, not the net)
no_native_output <- c("HYD_dens", "LKE_Qlw")
kn$simstrat_aed2[match(no_native_output, kn$name)] <- ""

# Insert simstrat_aed2 column right after glm_aed2
cols <- names(kn)
cols <- cols[cols != "simstrat_aed2"]
insert_at <- which(cols == "glm_aed2")
new_cols <- append(cols, "simstrat_aed2", after = insert_at)
kn <- kn[, new_cols]

write.csv(kn, "data-raw/key_naming.csv", row.names = FALSE, na = "")

cat("Rows with simstrat_aed2 populated:", sum(kn$simstrat_aed2 != ""), "of", nrow(kn), "\n")
