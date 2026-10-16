test_that("tli_year uses a July-June hydrological year", {
  d <- as.Date(c("2020-06-30", "2020-07-01", "2021-01-15"))
  expect_equal(tli_year(d), c(2020L, 2021L, 2021L))
  expect_equal(tli_year(as.POSIXct(d, tz = "UTC")), c(2020L, 2021L, 2021L))
  expect_equal(tli_year(d, year_start_month = 1L), c(2020L, 2020L, 2021L))
})

test_that("annual_mean_by_year holds one mean per year", {
  d <- seq(as.Date("2020-07-01"), as.Date("2022-06-30"), by = "day")
  x <- c(rep(1:2, length.out = sum(tli_year(d) == 2021)),
         rep(10, sum(tli_year(d) == 2022)))
  out <- annual_mean_by_year(x, d)
  expect_length(out, length(d))
  expect_equal(unique(out[tli_year(d) == 2022]), 10)
  expect_equal(length(unique(out[tli_year(d) == 2021])), 1)
  expect_true(all(is.na(annual_mean_by_year(rep(NA_real_, 5), d[1:5]))))
})

test_that("Secchi TLI follows the protocol equation", {
  expect_equal(calc_tli_secchi(2), 5.10 + 2.27 * log10(1/2 - 1/40))
  expect_true(is.finite(calc_tli_secchi(50)))
})

test_that("derived TLI is calculated from annual means", {
  d <- seq(as.Date("2020-07-01"), as.Date("2022-06-30"), by = "day")
  n <- length(d)
  season <- sin(2 * pi * as.numeric(d - d[1]) / 365)
  depths <- matrix(rep(c(0.5, 1.5, 30), n), nrow = 3)
  mk <- function(base) {
    matrix(rep(base * (1.5 + season), each = 3), nrow = 3)
  }
  out_list <- list(
    Date = d,
    LKE_depths = depths,
    HYD_epidep = rep(5, n),
    PHY_tchla = mk(5), NIT_tn = mk(0.5), PHS_tp = mk(0.02),
    LKE_photic = 3 * (1.5 + season)
  )
  tli3 <- calc_LKE_tli3(out_list, NULL)
  tli4 <- calc_LKE_tli4(out_list, NULL)
  yr <- tli_year(d)

  # Constant within each hydrological year
  for (y in unique(yr)) {
    expect_equal(length(unique(round(tli3[yr == y], 10))), 1)
  }
  # Equal to the index of the annual means
  y1 <- yr == min(yr)
  expect_equal(tli3[y1][1],
               calc_tli3(mean(out_list$PHY_tchla[1, y1]),
                         mean(out_list$NIT_tn[1, y1]),
                         mean(out_list$PHS_tp[1, y1])))
  expect_equal(tli4[y1][1],
               calc_tli4(mean(out_list$PHY_tchla[1, y1]),
                         mean(out_list$NIT_tn[1, y1]),
                         mean(out_list$PHS_tp[1, y1]),
                         mean(out_list$LKE_photic[y1])))
  # Differs from averaging the daily indices
  daily <- calc_tli3(out_list$PHY_tchla[1, ], out_list$NIT_tn[1, ],
                     out_list$PHS_tp[1, ])
  expect_false(isTRUE(all.equal(tli3[y1][1], mean(daily[y1]))))
})
