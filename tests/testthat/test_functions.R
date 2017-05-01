test_that("filename is valid",{
  expect_that(eq_data_read("file_not_here"),throws_error())
})


test_that("required column present in data",{
  filename <- system.file("extdata","earthquakes_data.txt.gz",package="earthquakesViz")
  eq_raw2 <- eq_data_read(filename) %>% dplyr::select(- LOCATION_NAME)

  expect_that(eq_clean_data(eq_raw2),throws_error())
  expect_that(eq_location_clean(eq_raw2),throws_error())

})

test_that("values for timeline arguments are correct",{
  filename <- system.file("extdata","earthquakes_data.txt.gz",package="earthquakesViz")
  sample_USA <- eq_data_read(filename) %>% eq_clean_data() %>%
      dplyr::filter((COUNTRY=="USA") & lubridate::year(DATE) >= 2000)

  #call to eq_time
  expect_that(eq_time(eq_clean=sample_USA,y="LOCATION_NAME"),throws_error())
  expect_that(eq_time(eq_clean=sample_USA,color="LOCATION_NAME"),throws_error())
  expect_that(eq_time(eq_clean=sample_USA,size="LOCATION_NAME"),throws_error())
  expect_that(eq_time(eq_clean=sample_USA,timeline_label="MAYBE"),gives_warning())
  expect_that(eq_time(eq_clean=sample_USA,timeline_label=TRUE,n_max="no"),gives_warning())
  expect_that(eq_time(eq_clean=sample_USA,alpha=10),gives_warning())
  expect_that(eq_time(eq_clean=sample_USA,alpha="no"),gives_warning())

  #no DATE
  sample_USA2 <- sample_USA %>% dplyr::select(- DATE)
  expect_that(eq_time(sample_USA2),throws_error())

  })

test_that("eq_map columns and annot_col are present",{
  filename <- system.file("extdata","earthquakes_data.txt.gz",package="earthquakesViz")
  sample_MEXICO <- eq_data_read(filename) %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)

  expect_that(eq_map(eq_clean=sample_MEXICO,annot_col="hello"),gives_warning())

  sample_MEXICO2 <- sample_MEXICO %>% dplyr::select(- LATITUDE)
  expect_that(eq_map(eq_clean=sample_MEXICO2,annot_col="DATE"),throws_error())

  sample_MEXICO2 <- sample_MEXICO %>% dplyr::select(- LOCATION_NAME)
  expect_that(eq_create_label(sample_MEXICO2),throws_error())

})



