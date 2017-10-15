context("geom")

# Using vdiffr
# Source: https://cran.r-project.org/web/packages/vdiffr/README.html
# Samples at ggplot2/tests/testthat/test-geom-path.R
# To run tests: devtools::test(filter = "geom")
# To validate figs: vdiffr::manage_cases()

test_that("test timeline geom", {
  eqdta <- data.frame ( date = c(lubridate::ymd("20150315"), lubridate::ymd("20160501"),lubridate::ymd("20171023")),
                        COUNTRY=c("USA","USA","CHINA"),
                        FOCAL_DEPTH=c(10,5,15),
                        LOCATION_NAME=c("JORDAN:  BAB-A-DARAA,AL-KARAK", "TURKMENISTAN:  W", "USA: WASHINGTON"),
                        EQ_PRIMARY = c(5.5, 5.9, 7.0)
                        )
  eqdta <- eq_location_clean(eqdta)
  # Draw the geom
  gg <- ggplot (data = eqdta,
          aes(
            x = date,
            y = COUNTRY,
            size = FOCAL_DEPTH,
            label = LOCATION_NAME,
            colour = EQ_PRIMARY
          )) +
    geom_timeline()

  # Test geom_timeline()
  vdiffr::expect_doppelganger("timeline",
                              gg
  )

  # Test geom_timeline() and geom_timeline_label()
  vdiffr::expect_doppelganger("timeline and label",
                              gg + geom_timeline_label()
  )

})
