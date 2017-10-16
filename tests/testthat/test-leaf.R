context("leaf")


test_that("test eq leaflet", {
  eqdta <- data.frame ( date = c(lubridate::ymd("20150315"), lubridate::ymd("20160501"),lubridate::ymd("20171023")),
                        COUNTRY=c("USA","USA","CHINA"),
                        FOCAL_DEPTH=c(10,5,15),
                        LOCATION_NAME=c("JORDAN:  BAB-A-DARAA,AL-KARAK", "TURKMENISTAN:  W", "USA: WASHINGTON"),
                        EQ_PRIMARY = c(5.5, 5.9, 7.0),
                        LATITUDE = c(46.0778307, 47.1258945, 47.1946298),
                        LONGITUDE = c(18.180542, 17.8372091, 18.3712126 )
  )
  eqdta <- eq_location_clean(eqdta)
  # Draw the geom
  res <- eq_map(frame = eqdta, annot_col = "LOCATION_NAME")

  # Test result class whether is a leaflet object
  expect_equal(class(res), c( "leaflet" ,   "htmlwidget" )  )
  # Unfortunately vdiffr is not suitable for widgets...
})


test_that("test eq popup text", {
  eqdta <- data.frame ( TOTAL_DEATHS=c(10,5,15),
                        LOCATION_NAME=c("JORDAN:  BAB-A-DARAA,AL-KARAK", "TURKMENISTAN:  W", "USA: WASHINGTON"),
                        EQ_PRIMARY = c(5.5, 5.9, 7.0)
  )
  eqdta <- eq_location_clean(eqdta)

  res <- eq_create_label(eqdta)

  expect_equal(res[1], "<b>Location</b>: Bab-A-Daraa,Al-Karak <br /> <b>Total deaths</b>: 10 <br /> <b>Magnitude</b>: 5.5 <br />")
  expect_equal(res[2], "<b>Location</b>: W <br /> <b>Total deaths</b>: 5 <br /> <b>Magnitude</b>: 5.9 <br />")
  expect_equal(res[3], "<b>Location</b>: Washington <br /> <b>Total deaths</b>: 15 <br /> <b>Magnitude</b>: 7 <br />")
})
