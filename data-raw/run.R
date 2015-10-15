filepath <- system.file("extdata", "2013-06-04-174137.TCX", package = "trackeR")
run <- readContainer(filepath, type = "tcx",
                     table = "gps_data",
                     timezone = "GMT", sessionThreshold = 2,
                     fromDistances = NULL, speedunit = switch(type, "tcx" = "m_per_s", "db3" = "km_per_h"),
                     distanceunit = switch(type, "tcx" = "m", "db3" = "km"), cycling = FALSE,
                     lgap = 30, lskip = 5, m = 11,
                     mc.cores = getOption("mc.cores", 2L))
save(run, file = "data/run.rda")
