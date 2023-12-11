hextriGrob <- function(x, y, seg, height, width, gp = grid::gpar()) {
  
  gp <- lapply(seq_along(x), function(i) structure(gp[i], class = "gpar"))
  xl  <- x - width
  xr  <- x + width
  y1  <- y + 2 * height
  y2  <- y + height
  y3  <- y - height
  y4  <- y - 2 * height
  pg  <- grid::polygonGrob
  
  do.call(grid::gList, 
          Map(function(x, y, xl, xr, y1, y2, y3, y4, seg, gp) {
            if(seg == 1) return(pg(x = c(x, x, xr, x),  y = c(y, y1, y2, y), gp = gp))
            if(seg == 2) return(pg(x = c(x, xr, xr, x), y = c(y, y2, y3, y), gp = gp))
            if(seg == 3) return(pg(x = c(x, xr, x, x),  y = c(y, y3, y4, y), gp = gp))
            if(seg == 4) return(pg(x = c(x, x, xl, x),  y = c(y, y4, y3, y), gp = gp))
            if(seg == 5) return(pg(x = c(x, xl, xl, x), y = c(y, y3, y2, y), gp = gp))
            if(seg == 6) return(pg(x = c(x, xl, x, x),  y = c(y, y2, y1, y), gp = gp))
          }, x = x, y = y, xl = xl, xr = xr, y1 = y1, 
          y2 = y2, y3 = y3, y4 = y4, seg = seg, gp = gp))
}

GeomHextri <- ggproto("GeomHextri", GeomHex,
                      draw_group = function (self, data, panel_params, coord, lineend = "butt",
                                             linejoin = "mitre", linemitre = 10) {
                        table_six <- function(vec) {
                          if(!is.factor(vec)) vec <- factor(vec)
                          tab <- round(6 * table(vec, useNA = "always")/length(vec))
                          n <- diff(c(0, findInterval(cumsum(tab) / sum(tab), 1:6/6)))
                          rep(names(tab), times = n)
                        }
                        num_cols <- sapply(data, is.numeric)
                        non_num_cols <- names(data)[!num_cols]
                        num_cols <- names(data)[num_cols]
                        datasplit <- split(data, interaction(data$x, data$y, drop = TRUE))
                        data <- do.call("rbind", lapply(seq_along(datasplit), function(i) {
                          num_list <- lapply(datasplit[[i]][num_cols], function(x) rep(mean(x), 6))
                          non_num_list <- lapply(datasplit[[i]][non_num_cols], function(x) {
                            table_six(rep(x, times = datasplit[[i]]$count))})
                          d <- datasplit[[i]][rep(1, 6),]
                          d[num_cols] <- num_list
                          d[non_num_cols] <- non_num_list
                          d$tri <- 1:6
                          d$group <- i
                          d}))
                        data <- ggplot2:::check_linewidth(data, snake_class(self))
                        if (ggplot2:::empty(data))  return(zeroGrob())
                        coords <- coord$transform(data, panel_params)
                        hw <- c(min(diff(unique(sort(coords$x)))), 
                                min(diff(unique(sort(coords$y))))/3)
                        hextriGrob(coords$x, coords$y, data$tri, hw[2], hw[1],
                                   gp = grid::gpar(col = data$colour, fill = alpha(data$fill, data$alpha),
                                                   lwd = data$linewidth * .pt, lty = data$linetype,
                                                   lineend = lineend, linejoin = linejoin,
                                                   linemitre = linemitre))})


hexify <- function (x, y, z, xbnds, ybnds, xbins, ybins, binwidth,
                    fun = mean, fun.args = list(),
                    drop = TRUE) {
  
  hb <- hexbin::hexbin(x, xbnds = xbnds, xbins = xbins, y,
                       ybnds = ybnds, shape = ybins/xbins, IDs = TRUE)
  value <- rlang::inject(tapply(z, hb@cID, fun, !!!fun.args))
  out <- hexbin::hcell2xy(hb)
  out <- ggplot2:::data_frame0(!!!out)
  out$value <- as.vector(value)
  out$width <- binwidth[1]
  out$height <- binwidth[2]
  if (drop) out <- stats::na.omit(out)
  out
}

StatHextri <- ggproto("StatBinhex", StatBinhex,
                      default_aes = aes(weight = 1, alpha = after_stat(count)),
                      compute_panel = function (self, data, scales, ...) {
                        if (ggplot2:::empty(data)) return(ggplot2:::data_frame0())
                        data$group <- 1
                        self$compute_group(data = data, scales = scales, ...)},
                      compute_group = function (data, scales, binwidth = NULL, bins = 30,
                                                na.rm = FALSE){
                        `%||%` <- rlang::`%||%`
                        rlang::check_installed("hexbin", reason = "for `stat_binhex()`")
                        binwidth <- binwidth %||% ggplot2:::hex_binwidth(bins, scales)
                        if (length(binwidth) == 1) binwidth <- rep(binwidth, 2)
                        wt <- data$weight %||% rep(1L, nrow(data))
                        non_pos <- !names(data) %in% c("x", "y", "PANEL", "group")
                        is_num  <- sapply(data, is.numeric)
                        aes_vars <- names(data)[non_pos & !is_num]
                        grps <- do.call("interaction", c(as.list(data[aes_vars]), drop = TRUE))
                        xbnds <- ggplot2:::hex_bounds(data$x, binwidth[1])
                        xbins <- diff(xbnds)/binwidth[1]
                        ybnds <- ggplot2:::hex_bounds(data$y, binwidth[2])
                        ybins <- diff(ybnds)/binwidth[2]
                        do.call("rbind", Map(function(data, wt) {
                          out <- hexify(data$x, data$y, wt, xbnds, ybnds, xbins,
                                        ybins, binwidth, sum)
                          for(var in aes_vars) out[[var]] <- data[[var]][1]
                          out$density <- as.vector(out$value/sum(out$value, na.rm = TRUE))
                          out$ndensity <- out$density/max(out$density, na.rm = TRUE)
                          out$count <- out$value
                          out$ncount <- out$count/max(out$count, na.rm = TRUE)
                          out$value <- NULL
                          out$group <- 1
                          out}, split(data, grps), split(wt, grps)))})

geom_hextri <- function(
    mapping     = aes(),
    data        = NULL,
    stat        = "hextri",
    position    = "identity",
    na.rm       = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    bins        = 10,
    ...) {
  
  ggplot2::layer(
    geom        = GeomHextri,
    data        = data,
    mapping     = mapping,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, bins = bins, ...)
  )
}