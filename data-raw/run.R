filepath <- system.file("extdata/tcx", "2013-06-08-090442.TCX", package = "trackeR")
run <- read_container(filepath,
                      type = "tcx",
                      timezone = "GMT",
                      session_threshold = 2,
                      country = NULL,
                      mask = TRUE,
                      from_distances = NULL,
                      speedunit = NULL,
                      distanceunit = NULL,
                      lgap = 30,
                      lskip = 5,
                      m = 11)
save(run, file = "data/run.rda", compress = "xz")

## all of June = 33 files, 27 sessions
filedir <- system.file("extdata/tcx", package = "trackeR") ## needs to point to git folder
runs <- readDirectory(filedir,
                      aggregate = TRUE,
                      table = "gps_data",
                      timezone = "",
                      session_threshold = 1.5, ## not the default, splits session 20
                      country = NULL,
                      mask = TRUE,
                      from_distances = NULL,
                      speedunit = list(tcx = "m_per_s", db3 = "km_per_h", json = "km_per_h"),
                      distanceunit = list(tcx = "m", db3 = "km", json = "km"),
                      lgap = 30,
                      lskip = 5,
                      m = 11,
                      verbose = TRUE,
                      silent = FALSE)
save(runs, file = "data/runs.rda", compress = "xz")


run <- readContainer(filepath,
                     type = "tcx",
                     timezone = "GMT",
                     sessionThreshold = 2,
                     country = NULL,
                     mask = TRUE,
                     fromDistances = NULL,
                     speedunit = NULL,
                     distanceunit = NULL,
                     lgap = 30,
                     lskip = 5,
                     m = 11)
