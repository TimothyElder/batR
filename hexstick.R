library(hexSticker)

setwd("/Users/timothyelder/Documents/batR")

library(showtext)

font_add(family = "Bookerly",
         regular = "/Users/timothyelder/Library/Fonts/Bookerly.ttf")

s <- sticker("inst/figures/batcat.png", package="batR", h_fill = "#656d66",
             h_color = "#000000", h_size = 2, p_family = "Bookerly", p_size = 20,
             s_x = 1, s_y = .8, s_width = .7, s_height = .7,
             filename="inst/figures/imgfile.png")

s