#' Make Kobe plot
#' 
#' \code{make_kobeplot} This function makes kobe plot
#' 
#' @export

make_kobeplot <- function(Kobe.Out, Slim = c(0, 6), Flim = c(0, 1.6)) {
    # Define the panel margins
    windows(9, 10)
    par(mfrow = c(2, 1), mar = c(5, 2, 1, 1), omi = c(0.5, 0.5, 0.2, 0))
    
    ######################## Plot 1 - SSB
    
    # Get the quantities to plot
    x <- Kobe.Out$SoverSmsy
    y <- Kobe.Out$FmultInv
    
    # Margins
    if (length(Slim) == 1) {
        xlim <- range(pretty(x))
    }
    if (length(Slim) == 2) {
        xlim <- Slim
    }
    if (length(Flim) == 1) {
        ylim <- range(pretty(y))
    }
    if (length(Flim) == 2) {
        ylim <- Flim
    }
    
    plot(0, 0, xlim = xlim, ylim = ylim, axes = T, xaxs = "i", yaxs = "i", type = "o", col = "black", ylab = "", 
        xlab = "", lwd = 1.25, pch = 19, cex = 0.75, las = 1)
    
    # Polygons for quadrants
    polygon(c(0, 1, 1, 0), c(1, 1, 1.6, 1.6), col = "red", border = NA)  # GIVE ACTUAL DIMENSIONS
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = "gold", border = NA)
    polygon(c(1, 6, 6, 1), c(0, 0, 1, 1), col = "yellowgreen", border = NA)
    polygon(c(1, 6, 6, 1), c(1, 1, 1.6, 1.6), col = "lemonchiffon", border = NA)
    polygon(c(0, 6, 6, 0), c(0, 0, 1.6, 1.6), col = NA, border = "black")
    
    # Grid
    lines(c(1, 1), c(ylim[1], ylim[2]), lty = 2)
    lines(c(xlim[1], xlim[2]), c(1, 1), lty = 2)
    
    # Plot the trajactory
    lines(x, y, type = "o", col = "black", lwd = 1.25, pch = 19, cex = 0.75)
    points(x[length(x)], y[length(x)], pch = 19, cex = 1.5, col = "steelblue1")  # Plot the terminal point
    points(x[1], y[1], pch = 25, cex = 1.5, col = "blue", bg = "blue")  # Plot the initial point
    
    title(xlab = "Spawning stock size relative to MSY", cex.lab = 1.25, line = 2.5)
    title(xlab = "Tamano de la poblacion reproductora relativo al RMS", cex.lab = 1.25, line = 4)
    
    
    ######################## Plot 2 - Bsmr
    
    # Define plot margins windows(8,5) par(mar=c(6,6,2,1))
    
    # Get the quantities to plot
    x <- Kobe.Out$BoverBmsy
    y <- Kobe.Out$FmultInv
    
    # Margins
    if (length(Slim) == 1) {
        xlim <- range(pretty(x))
    }
    if (length(Slim) == 2) {
        xlim <- Slim
    }
    if (length(Flim) == 1) {
        ylim <- range(pretty(y))
    }
    if (length(Flim) == 2) {
        ylim <- Flim
    }
    
    plot(0, 0, xlim = xlim, ylim = ylim, axes = T, xaxs = "i", yaxs = "i", type = "o", col = "black", ylab = "", 
        xlab = "", lwd = 1.25, pch = 19, cex = 0.75, las = 1)
    
    # Polygons for quadrants
    polygon(c(0, 1, 1, 0), c(1, 1, 1.6, 1.6), col = "red", border = NA)  # GIVE ACTUAL DIMENSIONS
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = "gold", border = NA)
    polygon(c(1, 6, 6, 1), c(0, 0, 1, 1), col = "yellowgreen", border = NA)
    polygon(c(1, 6, 6, 1), c(1, 1, 1.6, 1.6), col = "lemonchiffon", border = NA)
    polygon(c(0, 6, 6, 0), c(0, 0, 1.6, 1.6), col = NA, border = "black")
    
    # Grid
    lines(c(1, 1), c(ylim[1], ylim[2]), lty = 2)
    lines(c(xlim[1], xlim[2]), c(1, 1), lty = 2)
    
    # Plot the trajactory
    lines(x, y, type = "o", col = "black", lwd = 1.25, pch = 19, cex = 0.75)
    points(x[length(x)], y[length(x)], pch = 19, cex = 1.5, col = "steelblue1")  # Plot the terminal point
    points(x[1], y[1], pch = 25, cex = 1.5, col = "blue", bg = "blue")  # Plot the terminal point
    
    title(xlab = "Total stock size relative to MSY", cex.lab = 1.25, line = 2.5)
    title(xlab = "Tamano total de la poblacion relativo al RMS", cex.lab = 1.25, line = 4)
    
    mtext(side = 2, outer = T, "F relative to MSY - F relativa al RMS", line = 1, cex = 1.5)
    
}
