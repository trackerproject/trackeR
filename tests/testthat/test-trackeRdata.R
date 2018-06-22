context("implementation [trackeRdata, read_container]")

tcxfile <- system.file("extdata/tcx/", "2013-06-30-070511.TCX", package = "trackeR")
gpxfile <- system.file("extdata/gpx/", "20170709-151453-Ride.gpx", package = "trackeR")


tcx <- readTCX(tcxfile)
gpx <- readGPX(gpxfile)

## read_container
tcx1 <- read_container(tcxfile, cores = 2)
