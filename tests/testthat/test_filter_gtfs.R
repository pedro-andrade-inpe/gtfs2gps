
test_that("filter_valid_stop_times", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  expect_equal(dim(poa$stop_times)[1], 23040)
  expect_equal(dim(poa$stops)[1], 212)
  expect_equal(dim(poa$routes)[1], 4)
  
  subset <- filter_valid_stop_times(poa)
  expect_equal(dim(subset$stop_times)[1], 774)
  expect_equal(dim(subset$stops)[1], 8)
  expect_equal(dim(subset$routes)[1], 4)
})


test_that("remove_invalid", {
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

  sp$shapes <- sp$shapes[-(1:30000),]
  sp$agency$agency_id <- 'test'
  
  sp2 <- remove_invalid(sp, prompt_invalid = TRUE)
  
  expect_equal(length(sp$stops$stop_id), 3039)
  expect_equal(length(sp2$stops$stop_id), 602)
  
  sp3 <- remove_invalid(sp, only_essential = FALSE)
  
  expect_equal(length(sp3$stops$stop_id), 0)
})

test_that("filter_valid_stop_times drops malformed times and keeps strings verbatim", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

  poa$stop_times[1, `:=`(arrival_time = "05:21", departure_time = "05:21:00")]
  poa$stop_times[2, `:=`(arrival_time = "abc", departure_time = "05:21:00")]
  poa$stop_times[3, `:=`(arrival_time = " 5:21:00", departure_time = "5:21:00")]
  keys <- poa$stop_times[1:3, .(trip_id, stop_sequence)]
  before <- data.table::copy(poa)

  subset <- filter_valid_stop_times(poa)

  # input data is not modified
  expect_identical(poa, before)

  # rows 1 and 2 dropped, row 3 kept with its strings untouched
  kept <- subset$stop_times[keys, on = c("trip_id", "stop_sequence"), nomatch = NULL]
  expect_equal(nrow(kept), 1)
  expect_equal(kept$arrival_time, " 5:21:00")
  expect_equal(kept$departure_time, "5:21:00")
})
