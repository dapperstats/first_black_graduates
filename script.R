#install.packages(c("maps", "png", "rnaturalearth", "sf", "sp"))
#devtools::install_github("ropensci/rnaturalearthdata")
#devtools::install_github("ropensci/rnaturalearthhires")

library(maps)
library(png)
library(rnaturalearth)
library(sp)
library(sf)

slater96 <- read.csv("data/slater_1996.csv")

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_names <- states$ID
state_names <- state_names[state_names != "district of columbia"]

#windows(12,8)
tiff("map.tiff", height = 8, width = 12, units = "in", res = 200, 
     compression = "lzw")
par(mar = c(1, 1, 10, 2), fig = c(0, 1, 0, 1))
states_plot <- ne_states(country = "United States of America")
states_plot_names <- tolower(states_plot[[9]])
state_match <- match(states_plot_names, 
                     tolower(as.character((slater96$state))))
states_plot_years <- slater96$year[state_match]

states_col <- 1 - (1975 - states_plot_years) / (1975 - 1775)
states_col[is.na(states_col)] <- 1

plot(states_plot, col = grey(states_col, 0.5),
     xlim = c(-124, -68), ylim = c(24, 49), lwd = 2, border = grey(0.1))

for(i in 1:length(state_names)){
  row_in <- tolower(as.character(slater96$state)) == state_names[i]
  x <- slater96$text_lon[row_in]
  y <- slater96$text_lat[row_in]

  uni <- as.character(slater96$university[row_in])
  nc_uni <- nchar(uni)
  grads <- strsplit(as.character(slater96$graduates[row_in]), " and ")[[1]]
  ngrads <- length(grads)
  nc_grads <- unlist(lapply(grads, nchar))  

  if(is.na(slater96$year[row_in])){
    xo <- nc_uni/9
    rect(x - xo, y - 1, x + xo, y + 0.5, border = NA, col = grey(0.9, 0.8),
         xpd = TRUE)

    text(x, y, uni, cex = 0.4, font = 2, xpd = TRUE)
    text(x, y - 0.5, "Not Known", cex = 0.5, font = 2, xpd = TRUE)     
  }else{
    xo <- max(c(nc_uni/9, max(nc_grads)/8))

    yo <- 0.8 + ngrads * 0.5
    rect(x - xo, y - yo, x + xo, y + 0.5, border = NA, col = grey(0.9, 0.8),
         xpd = TRUE)

    text(x, y, uni, cex = 0.4, font = 2, xpd = TRUE)
    text(x, y - 0.5, slater96$year[row_in], cex = 0.4, font = 2, xpd = TRUE)

    for(j in 1:ngrads){
      text(x, y - 1 - (j - 1) * 0.5, grads[j], cex = 0.4, font = 2,
           xpd = TRUE)
    }
  }
}
points(slater96$lon, slater96$lat, pch = 16, col = grey(0.9, 0.6), cex = 1.5,
       lwd = 2)
points(slater96$lon, slater96$lat, pch = 1, col = grey(0.1, 0.8), cex = 1.5,
       lwd = 2)




par(mar = c(0, 0, 0, 0), fig = c(0.02, 0.3, 0.04, 0.23), new = TRUE)
plot(states_plot, col = grey(states_col, 0.5),
     xlim = c(-190, -120), ylim = c(51, 75), lwd = 2, border = grey(0.1))
points(slater96$lon, slater96$lat, pch = 16, col = grey(0.9, 0.6), cex = 1.5,
       lwd = 2)
points(slater96$lon, slater96$lat, pch = 1, col = grey(0.1, 0.8), cex = 1.5,
       lwd = 2)

row_in <- tolower(as.character(slater96$state)) == "alaska"
x <- slater96$text_lon[row_in]
y <- slater96$text_lat[row_in]

uni <- as.character(slater96$university[row_in])
nc_uni <- nchar(uni)
grads <- strsplit(as.character(slater96$graduates[row_in]), " and ")[[1]]
ngrads <- length(grads)
nc_grads <- unlist(lapply(grads, nchar))  

xo <- nc_uni/1.5
rect(x - xo, y - 3, x + xo, y + 2.5, border = NA, col = grey(0.9, 0.8),
     xpd = TRUE)
text(x, y + 1, uni, cex = 0.4, font = 2, xpd = TRUE)
text(x, y - 1, "Not Known", cex = 0.5, font = 2, xpd = TRUE)     

par(mar = c(0, 0, 0, 0), fig = c(0.3, 0.4, 0.04, 0.18), new = TRUE)
plot(states_plot, col = grey(states_col, 0.5),
     xlim = c(-160, -150), ylim = c(15, 24.5), lwd = 2, border = grey(0.1))
points(slater96$lon, slater96$lat, pch = 16, col = grey(0.9, 0.6), cex = 1.5,
       lwd = 2)
points(slater96$lon, slater96$lat, pch = 1, col = grey(0.1, 0.8), cex = 1.5,
       lwd = 2)

row_in <- tolower(as.character(slater96$state)) == "hawaii"
x <- slater96$text_lon[row_in]
y <- slater96$text_lat[row_in]

uni <- as.character(slater96$university[row_in])
nc_uni <- nchar(uni)
grads <- strsplit(as.character(slater96$graduates[row_in]), " and ")[[1]]
ngrads <- length(grads)
nc_grads <- unlist(lapply(grads, nchar))  

xo <- nc_uni/6
rect(x - xo, y - 1.25, x + xo, y + 1.5, border = NA, col = grey(0.9, 0.8),
     xpd = TRUE)
text(x, y + 0.25, uni, cex = 0.4, font = 2, xpd = TRUE)
text(x, y - 0.5, "Not Known", cex = 0.5, font = 2, xpd = TRUE)     
  

par(mar = c(1, 1, 1, 1), fig = c(0, 1, 0, 1), new = TRUE)
plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", 
     xlab = "", xlim = c(-124, -68), ylim = c(24, 49), )
text(-125, 23, "#Strike4BlackLives #ShutdownSTEM #ShutdownAcademia",
     cex = 0.8, font = 2, adj = 0, xpd = NA)
text(-66, 23.8, "Content from Slater 1996",
     cex = 0.8, font = 2, adj = 1, xpd = NA)
text(-66, 23, "The Journal of Blacks in Higher Education",
     cex = 0.8, font = 4, adj = 1, xpd = NA)
text(-95, 50, xpd = NA, cex = 1.8, font = 2,
     "The First Black Graduates from Each State's Flagship University")
text(-95, 49, xpd = NA, cex = 1.2,
     "A visualization of the work of Robert B. Slater")
text(-88, 23, "*Enrollment year   **Graduate student",
     cex = 0.8, font = 2, adj = 1, xpd = NA)


par(mar = c(1.5, 2.5, 1, 1), fig = c(0, 0.3, 0.77, 0.95), new = TRUE)
y <- table(states_plot_years)
x <- as.numeric(names(table(states_plot_years)))
x2 <- 1825:1975
y2 <- rep(0, length(x2))
xmatch <- match(x, x2)
y2[xmatch] <- y
hist_col <- 1 - (1975 - x2) / (1975 - 1775)

plot(x2, y2, type = "h", xlim = c(1825, 1975), ylim = c(0, 4), lwd = 3,
     col = grey(hist_col), bty = "L", xlab = "", ylab = "", xaxt = "n",
     yaxt = "n")
box(bty = "L", lwd = 2)
axis(1, at = seq(1820, 1975, 10), labels = FALSE, tck = -0.03, lwd = 2)
axis(1, at = seq(1850, 1950, 50), cex.axis = 0.7, col = NA, line = -0.7,
     font = 2)
axis(1, at = seq(1850, 1950, 50), labels = FALSE, tck = -0.06, lwd = 2)
axis(2, at = 0:4, labels = FALSE, tck = -0.05, lwd = 2)
axis(2, at = seq(0, 4, 2), col = NA, line = -0.5, las = 1, cex.axis = 0.7,
     font = 2)
mtext(side = 1, "Year of First Black Graduate", line = 1.25, cex = 0.7,
      font = 2)
mtext(side = 2, "Universities", line = 1.25, cex = 0.7, font = 2)


par(mar = c(0, 0, 0, 0), fig = c(0.935, 0.995, 0.46, 0.59), new = TRUE)
img <- readPNG("images/alan_thacker_busby.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)


par(mar = c(0, 0, 0, 0), fig = c(0.39, 0.44, 0.39, 0.52), new = TRUE)
img <- readPNG("images/blanche_bruce.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.75, 0.81, 0.48, 0.58), new = TRUE)
img <- readPNG("images/calvin_waller.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.64, 0.69, 0.35, 0.45), new = TRUE)
img <- readPNG("images/doris_wilkinson.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.40, 0.45, 0.22, 0.32), new = TRUE)
img <- readPNG("images/edna_rhambo.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.93, 0.97, 0.68, 0.76), new = TRUE)
img <- readPNG("images/elizabeth_virgil.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.37, 0.42, 0.45, 0.61), new = TRUE)
img <- readPNG("images/george_flippin.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.76, 0.8, 0.63, 0.73), new = TRUE)
img <- readPNG("images/george_henderson.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.78, 0.82, 0.38, 0.45), new = TRUE)
img <- readPNG("images/jack_hodges.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.855, 0.895, 0.44, 0.52), new = TRUE)
img <- readPNG("images/james_carr.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.33, 0.37, 0.62, 0.69), new = TRUE)
img <- readPNG("images/james_dorsey.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.625, 0.66, 0.42, 0.47), new = TRUE)
img <- readPNG("images/marcellus_neal.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.18, 0.22, 0.40, 0.47), new = TRUE)
img <- readPNG("images/theodore_miller.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.62, 0.66, 0.15, 0.22), new = TRUE)
img <- readPNG("images/vivian_malone.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.57, 0.62, 0.61, 0.670), new = TRUE)
img <- readPNG("images/william_noland.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

par(mar = c(0, 0, 0, 0), fig = c(0.56, 0.6, 0.46, 0.52), new = TRUE)
img <- readPNG("images/william_walter_smith.png")
plot(1:2, type = "n", bty = "n", xaxt = "n", yaxt = "n")
transparent <- img[,,4] == 0
img <- as.raster(img[,,1:3])
img[transparent] <- NA
rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate = FALSE)

dev.off()




