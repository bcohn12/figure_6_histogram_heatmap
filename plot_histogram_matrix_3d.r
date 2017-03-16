view_params <- function(){
     return(structure(list(FOV = 114.430473327637, ignoreExtent = FALSE, 
    listeners = 76L, mouseMode = structure(c("polar", "fov", 
    "zoom", "pull"), .Names = c("left", "right", "middle", "wheel"
    )), skipRedraw = FALSE, userMatrix = structure(c(0.256301522254944, 
    -0.0408177487552166, 0.965734720230103, 0, 0, 0.999108016490936, 
    0.0422283038496971, 0, -0.966596901416779, -0.0108231781050563, 
    0.256072908639908, 0, 0, 0, 0, 1), .Dim = c(4L, 4L)), scale = c(1, 
    1, 1), viewport = structure(c(0L, 0L, 1538L, 906L), .Names = c("x", 
    "y", "width", "height")), zoom = 1, windowRect = c(4L, 44L, 
    1542L, 950L), family = "sans", font = 1L, cex = 1, useFreeType = TRUE), .Names = c("FOV", 
    "ignoreExtent", "listeners", "mouseMode", "skipRedraw", "userMatrix", 
    "scale", "viewport", "zoom", "windowRect", "family", "font", 
    "cex", "useFreeType")))
}

plot_histogram_matrix_3d <- function(histogram_matrix, y_scale) {
     require(rgl)
     y <- y_scale * histogram_matrix      # Exaggerate the relief
     x <- 100 * (1:nrow(y))   # 10 meter spacing (S to N)
     z <- 500 * (1:ncol(y))   # 10 meter spacing (E to W)
     ylim <- range(y)
     ylen <- ylim[2] - ylim[1] + 1
     colorlut <- heat.colors(ylen) # height color lookup table
     col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point    
     # rgl.open()
     # par3d(view_params())
     # bg3d("white")
     rgl.surface(x, z, y, color = "cyan", back = "lines")
}