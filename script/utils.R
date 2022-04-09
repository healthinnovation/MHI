# 1. gg_barplot -----------------------------------------------------------
gg_barplot <- function(x,nrow = 1){
  g0 <- x %>% 
    ggplot(
      aes(
        x = bi_class,
        y = total,
        fill = bi_class
      )
    ) + 
    geom_bar(stat = 'identity') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    labs(x = "", y = "")
  
  if(nrow == 1){
    g1 <- g0 + 
      scale_fill_manual(values = c("#AE384C","#BA7A8F","#CADECF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  else if(nrow == 2){ 
    g1 <- g0 + 
      scale_fill_manual(values = c("#75304A","#7F688A","#87A1C7")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  } else {
    g1 <- g0 + 
      scale_fill_manual(values = c("#3D2847","#425785","#4785BF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  return(g1)
} 

# 2. Biscale color palette - customized -----------------------------------

bi_pal2 <- function (pal, dim = 3, preview = TRUE) 
{
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (pal %in% c("Brown", "DkBlue", "DkCyan", "DkViolet", "GrPink") == 
      FALSE) {
    stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (is.logical(preview) == FALSE) {
    stop("A logical scalar must be supplied for 'preview'. Please provide either 'TRUE' or 'FALSE'.")
  }
  if (preview == TRUE) {
    out <- bi_legend(pal = pal, dim = dim, size = 16)
  }
  else if (preview == FALSE) {
    if (pal == "DkViolet") {
      out <- pal_dkviolet2(n = dim)
    }
    else if (pal == "GrPink") {
      out <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      out <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      out <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      out <- pal_brown(n = dim)
    }
  }
  return(out)
}

pal_dkviolet <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#3F2949", # high x, high y
      "1-2" = "#4885C1", # low x, high y
      "2-1" = "#AE3A4E", # high x, low y
      "1-1" = "#CABED0" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#CADECF", # high x, high y
      "2-3" = "#BA7A8F",
      "1-3" = "#AE384C", # low x, high y
      "3-2" = "#87A1C7",
      "2-2" = "#7F688A", # medium x, medium y
      "1-2" = "#75304A",
      "3-1" = "#4785BF", # high x, low y
      "2-1" = "#425785",
      "1-1" = "#3D2847" # low x, low y
    )
    
  }
  return(out)
}

# gray pink palette
pal_grpink <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#574249", # high x, high y
      "1-2" = "#64ACBE", # low x, high y
      "2-1" = "#C85A5A", # high x, low y
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#574249", # high x, high y
      "2-3" = "#627F8C",
      "1-3" = "#64ACBE", # low x, high y
      "3-2" = "#985356",
      "2-2" = "#AD9EA5", # medium x, medium y
      "1-2" = "#B0D5DF",
      "3-1" = "#C85A5A", # high x, low y
      "2-1" = "#E4ACAC",
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  }
  
  # return output
  return(out)
  
}


bi_legend <- function(pal, dim = 3, xlab, ylab, size = 10, flip_axes = FALSE, rotate_pal = FALSE, pad_width = NA, pad_color = '#ffffff'){
  
  # global binding
  bi_class = bi_fill = x = y = NULL
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (missing(xlab) == TRUE){
    xlab <- "x var "
  }
  
  if (is.character(xlab) == FALSE){
    stop("The 'xlab' argument must be a character string.")
  }
  
  if (missing(ylab) == TRUE){
    ylab <- "y var "
  }
  
  if (is.character(ylab) == FALSE){
    stop("The 'ylab' argument must be a character string.")
  }
  
  if (is.numeric(size) == FALSE){
    stop("The 'size' argument must be a numeric value.")
  }
  
  # nse
  xQN <- rlang::quo_name(rlang::enquo(xlab))
  yQN <- rlang::quo_name(rlang::enquo(ylab))
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # create tibble for plotting
  x <- dplyr::tibble(
    bi_class = names(x),
    bi_fill = x
  )
  
  # reformat
  leg <- tidyr::separate(x, bi_class, into = c("x", "y"), sep = "-")
  leg <- dplyr::mutate(leg, x = as.integer(x), y = as.integer(y))
  
  # create ggplot2 legend object
  legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = leg, mapping = ggplot2::aes(x = x, y = y, fill = bi_fill), lwd = pad_width, col = pad_color) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = substitute(paste(xQN, ""%->%"")), y = substitute(paste(yQN, ""%->%""))) +
    bi_theme() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = size)) +
    ggplot2::coord_fixed()
  
  # return output
  return(legend)
  
}
bi_theme <- function(base_family = "sans", base_size = 24, bg_color = "#ffffff", font_color = "#000000", ...) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      
      # text defaults
      text = ggplot2::element_text(color = font_color),
      
      # remove all axes
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      
      # add a grid that blends into plot background
      panel.grid.major = ggplot2::element_line(color = bg_color, size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # background colors
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      
      # borders and margins
      plot.margin = ggplot2::unit(c(.5, .5, .2, .5), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(-.1, 0.2, .2, 0.2), "cm"),
      
      # titles
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, color = font_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = font_color,
                                            margin = ggplot2::margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                            face = "bold", debug = FALSE),
      legend.title = ggplot2::element_text(color = font_color),
      legend.text = ggplot2::element_text(hjust = 0, color = font_color),
      
      # captions
      plot.caption = ggplot2::element_text(size = ggplot2::rel(.6), hjust = .5,
                                           margin = ggplot2::margin(t = 0.2, b = 0, unit = "cm"),
                                           color = font_color),
      ...
    )
  
}

bi_scale_fill2 <- function(pal, dim = 3, flip_axes = FALSE, rotate_pal = FALSE, ...){
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis' or supply a custom palette created with 'bi_pal_custom()'.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # apply to ggplot object
  ggplot2::scale_fill_manual(values = x, ...)
  
}

# 3. Maps biscale ---------------------------------------------------------
gg_bimap <- function(data, xlim, ylim){
  g0 <- data %>% 
    ggplot() + 
    geom_sf(
      data = dep,
      lwd = 0.5,
      fill = "#d9d9d9",
      show.legend = FALSE,
      color = "white"
    ) +
    geom_sf(
      data = data,
      lwd = 0.0,
      aes(fill = bi_class),
      show.legend = FALSE
    ) +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) +
    bi_scale_fill2(pal = "DkViolet", dim = 3) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    theme_bw()  +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) + 
    facet_grid(.~title) + 
    theme(
      strip.background = element_rect(
        colour = "black",
        fill = "white")
    )
}