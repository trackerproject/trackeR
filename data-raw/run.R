filepath <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")
run <- readContainer(filepath,
                     type = "tcx",
                     table = "gps_data",
                     timezone = "GMT",
                     sessionThreshold = 2,
                     country = NULL,
                     mask = TRUE,
                     fromDistances = NULL,
                     speedunit = NULL,
                     distanceunit = NULL,
                     cycling = FALSE,
                     lgap = 30,
                     lskip = 5,
                     m = 11,
                     mc.cores = getOption("mc.cores", 2L))
save(run, file = "data/run.rda", compress = "xz")

## all of June = 33 files, 26 sessions
filedir <- system.file("extdata", package = "trackeR")
runs <- readDirectory(filedir,
                     aggregate = TRUE,
                     table = "gps_data",
                     timezone = "",
                     sessionThreshold = 2,
                     country = NULL,
                     mask = TRUE,
                     fromDistances = NULL,
                     speedunit = list(tcx = "m_per_s", db3 = "km_per_h"),
                     distanceunit = list(tcx = "m", db3 = "km"),
                     cycling = FALSE,
                     lgap = 30, lskip = 5, m = 11,
                     mc.cores = getOption("mc.cores", 2L),
                     verbose = TRUE)
save(runs, file = "data/runs.rda", compress = "xz")

if(FALSE){
## ATI = all of June + 1st of July = 34 files, 27 sessions
filedir <- system.file("extdata", package = "trackeR")
runs <- readDirectory(filedir,
                     aggregate = TRUE,
                     table = "gps_data",
                     timezone = "",
                     sessionThreshold = 2,
                     country = NULL,
                     mask = TRUE,
                     fromDistances = NULL,
                     speedunit = list(tcx = "m_per_s", db3 = "km_per_h"),
                     distanceunit = list(tcx = "m", db3 = "km"),
                     cycling = FALSE,
                     lgap = 30, lskip = 5, m = 11,
                     mc.cores = getOption("mc.cores", 2L),
                     verbose = TRUE)
save(runs, file = "data/runs_ATI.rda")
}
