library(readr)
earthquakes <- read_tsv(file="data-raw/signif.txt",
                        col_names = TRUE,
                        col_types =
                          list (
                            I_D = "i",
                            FLAG_TSUNAMI = "c",
                            YEAR = "i",
                            MONTH = "i",
                            DAY = "i",
                            HOUR = "i",
                            MINUTE = "i",
                            SECOND = "d",
                            FOCAL_DEPTH = "i",
                            EQ_PRIMARY = "d",
                            EQ_MAG_MW = "d",
                            EQ_MAG_MS = "d",
                            EQ_MAG_MB = "d",
                            EQ_MAG_ML = "d",
                            EQ_MAG_MFA = "d",
                            EQ_MAG_UNK = "d",
                            INTENSITY = "i",
                            COUNTRY = "c",
                            STATE = "c",
                            LOCATION_NAME = "c",
                            LATITUDE = "d",
                            LONGITUDE = "d",
                            REGION_CODE = "i",
                            DEATHS = "i",
                            DEATHS_DESCRIPTION = "i",
                            MISSING = "i",
                            MISSING_DESCRIPTION = "i",
                            INJURIES = "i",
                            INJURIES_DESCRIPTION = "i",
                            DAMAGE_MILLIONS_DOLLARS = "d",
                            DAMAGE_DESCRIPTION = "i",
                            HOUSES_DESTROYED = "i",
                            HOUSES_DESTROYED_DESCRIPTION = "i",
                            HOUSES_DAMAGED = "i",
                            HOUSES_DAMAGED_DESCRIPTION = "i",
                            TOTAL_DEATHS = "i",
                            TOTAL_DEATHS_DESCRIPTION = "i",
                            TOTAL_MISSING = "i",
                            TOTAL_MISSING_DESCRIPTION = "i",
                            TOTAL_INJURIES = "i",
                            TOTAL_INJURIES_DESCRIPTION = "i",
                            TOTAL_DAMAGE_MILLIONS_DOLLARS = "d",
                            TOTAL_DAMAGE_DESCRIPTION = "i",
                            TOTAL_HOUSES_DESTROYED = "i",
                            TOTAL_HOUSES_DESTROYED_DESCRIPTION = "i",
                            TOTAL_HOUSES_DAMAGED = "i",
                            TOTAL_HOUSES_DAMAGED_DESCRIPTION = "i"
                          )
                        )

devtools::use_data(earthquakes, overwrite = TRUE)