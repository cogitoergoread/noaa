context("Data")

test_that("eq_read_data reads the appropriate dataset", {
  eq <- eq_read_data(filename="../../data-raw/signif.txt.bz2")

  expect_equal(nrow(eq), 5962)
  expect_equal(ncol(eq), 47)

})

test_that("data transformation is done", {
    eqt <-  data.frame( YEAR =2017, MONTH=10, DAY=15, HOUR=12, MINUTE=22, SECOND=0)
    eqr <- eq_clean_data(eqt)

    expect_equal(eqr$date[1], lubridate::ydm_hm("201715101222"))

})

test_that("location is properly converted", {
  eqt <- data.frame( LOCATION_NAME = c("JORDAN:  BAB-A-DARAA,AL-KARAK", "TURKMENISTAN:  W")  )
  eqr <- eq_location_clean(eqt)

  expect_equal(eqr$LOCATION_NAME[1],  "Bab-A-Daraa,Al-Karak")
  expect_equal(eqr$LOCATION_NAME[2],  "W")
  })
