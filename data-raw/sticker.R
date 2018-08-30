library("hexSticker")
library("png")
library("grid")
library("ggplot2")





"#FF94B7"
"#FFB32F"
"#B6D000"
"#00E380"
"#00E6E6"
"#28CFFF"
"#F3A2FF"


g2 <- readPNG(system.file("icons", "man-running.png", package = "trackeRapp"))
g2 <- matrix(rgb(g2[,,1], g2[,,2], g2[,,3], g2[,,4] * 1), nrow = dim(g2)[1])
g2 <- rasterGrob(g2, interpolate = TRUE)

gg <- qplot(c(2, 3), c(2, 3.2), geom = "blank") +
    annotation_custom(g2, xmin = 2.05, xmax = 2.95, ymin = 2.05, ymax = 2.95) +
    geom_text(aes(x = 2.61, y = 3.07, label = "app", vjust = 0, hjust = 0), color = "white", size = 4.7) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = NULL)

sticker(gg,
        package="tracker",
        p_size=8, s_x = 1, s_y = .8, s_width = 1.2, s_height = 1,
        p_color = "white",
        h_color = "#009ACD",
        h_fill = "#009ACD",
        filename="~/Downloads/baseplot.png")


img1 <- readPNG(system.file("inst/icons", "cycling.png", package = "trackeRapp"))
img3 <- readPNG(system.file("inst/icons", "swimming-man.png", package = "trackeRapp"))

g1 <- rasterGrob(img1, interpolate = TRUE)
g3 <- rasterGrob(img3, interpolate = TRUE)

