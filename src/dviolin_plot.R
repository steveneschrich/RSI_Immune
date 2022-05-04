#
#
# hlm_violin_plot
# Based on the code from plotrix v3.6-5
# Modified to include
# horizontal and vertical orientation
# color by density of histogram
#
# Input matrix X should be a data frame with
#  X$category
#  X$values
#  X$labels (optional, if color_by is "label")
# TODO: Have a formula (value~category) interface.
#
#
#
# The defaults are changed:
# bw=0.35 changed to "SJ", a recommended density bandwidth selector
# other options: bw= c("nrd0","nrd","ucv","bcv","SJ")
#
dviolin_plot<-function (X = rnorm(50), na.rm = TRUE, bw = "SJ", 
    violin_width = 0.8, violin_end_width = 0.005, equal_width = TRUE, 
    box_width = 0.03, box_col = "black", show_outliers = FALSE, 
    pch = 1, range = 1.5, value_limit,  value_label = "", 
    
    # Title options
    title = main,
    show_title = show_main,
    main = "Violin Plot", 
    show_main=TRUE, 
    title_gp = grid::gpar(fontface="bold"),
    
    #-------
    # Categories: How the y axis is split up.
    #-------
    category_gp = grid::gpar(cex=cex), # Category label parameters
    category_limit, 
    category_label = "",
    category_names, 
    show_n=TRUE, # Show the N= tag
    n_prefix="N=", # What is the prefix
    n_gp = grid::gpar(cex=cex),
    
    
    median_col = "white", 
    plot_mean = FALSE, mean_pch = 19, mean_pch_col = "yellow", 
    summary.function=median, 
    max.colors=50,
    # color_by: 
    #   density is just coloring the density of the distribution
    #   label is coloring by the label in X$labels
    color_by=c("density","label"),
    density.color.high="yellow",
    density.color.mid="green",
    density.color.low="blue",
    label_colors=NULL,
    label.color.high="red",
    label.color.low="blue",
 
    newpage=FALSE,
    
    # On the violin plot, a grand median (of all data) can be plotted.
    grand_median_line=FALSE,
    grand_median_gp = grid::gpar(lty=2),
    cex=0.75,
    ...) 
{
    
    # Libraries
    require(gplots)
    require(grid)
  
  # Current hack to give a unique id for the graph.
  id<-as.numeric(Sys.time()) * runif(n=1)
  
    # Order the categories (factors) by a given summary measure
    category.summary<-aggregate(x=X$values,by=list(category=X$category),summary.function)
    category.ordering<-order(category.summary$x)
    X$category<-factor(X$category,levels=category.summary$category[category.ordering])
  
    # at is the position of the violin in normal graph order (1,2,etc)
    at <- sequence(nlevels(X$category))
    
    # Calculate and display N= sample counts.
    category_counts<-aggregate(x=X$values,by=list(category=X$category),length)
    category_count_labels<-paste(n_prefix,category_counts$x,sep="")
      
      
    # stringr::str_pad(
    #string = category_count_labels,
    #width = max(stringr::str_length(category_count_labels)),
    #side="left"),


    # Fill in missing values with intelligent defaults.
    if (missing(category_limit)) 
        category_limit <- c(min(at) - violin_width/2, max(at) + violin_width/2)
    if (missing(value_limit)) 
        value_limit <- c(min(X$values, na.rm = TRUE), max(X$values, na.rm = TRUE))
    if (missing(category_names)) {
       category_names <- levels(X$category)
    }
    color_by=match.arg(color_by)
    
    if ( color_by == "label" && is.null(label_colors)) {
      label_levels<-levels(factor(X$labels))
      label_colors<-colorpanel(length(label_levels),
                               high=label.color.high,
                               low=label.color.low)
      names(label_colors)<-label_levels
      print(label_colors)                           
    }
    
    # Setup the color panel. Since R does 1..n indexing, create 101 colors so we have
    # indices 0..100. Then min value=0 has a color as does max value.
    density_colors<-colorpanel(max.colors+1,high=density.color.high, mid=density.color.mid,
                    low=density.color.low)

    
# TODO: Fix the fact that we pass in a col. This should be the basis
    # for colors.
    #    if (length(col) != length(at)) {
#        col = rep(col, length.out = length(at))
#    }


    
    ############################################################################
    # Margins, plotting area setup
    #
    # Currently, we use grid to manage the graph, then use primitives (lines, polygons)
    # to do the drawing.
    #
    # The plot (dviolin) is separated into the following areas:
    # |           |top_margin|            |
    # |left_margin|          |right_margin|
    # |           |bottom_margin|      
    #
    # Note that null units means fraction (percentage) of total. So for instance,
    # in the height, the top row is 1 line and the bottom row is 4 lines. The
    # middle is 1 null, or 100% (less the top/bottom allocation).
    #############################################################################
    grid::pushViewport(grid::viewport(
      layout=grid::grid.layout(3, 3,
                 widths=grid::unit(c(20, 50, 10), c("null", "null", "null")),
                 heights=grid::unit(c(1, 1, 4), c("lines", "null", "lines"))
            ),
      name=paste0("dviolin",id))
    )
 
    #########################################################################################
    # Draw the graph layout/axes/etc
    #########################################################################################
    if (newpage) grid::grid.newpage()
    
   
    
    # Draw the triangle figure at the bottom. It should be increasing to the right. This is
    # done via a polygon (triangle) with coordinates relative to the grid. The polygon
    # starts at the lower left corner, then traces to the right, followed back to the left
    # for the final join.
    grid::pushViewport(
      grid::viewport(
        layout.pos.col = 2, 
        layout.pos.row = 3,
        name = paste0("bottom_margin",id),
        xscale=extendrange(value_limit)
    ))
    
    # Draw the triangle.
    grid::grid.polygon(x=c(0,1,1,0),y=c(.3,.3,.5,.3), gp=grid::gpar(fill="black"))
    grid::grid.text(y=0.2,label=value_label)
    
    # Return to top of figure stack.
    grid::upViewport()
    
    
    # Next add the title across the top (if desired)
    if ( show_title) {
      grid::pushViewport(
        grid::viewport(
          layout.pos.col = 2, 
          layout.pos.row = 1,
          name = paste0("top_margin",id)
        )
      )
      grid::grid.text(label=title, gp=title_gp)
      grid::upViewport()   
    }
    
    # Add in N underline at top (bold): col=3,row=1
    # Create right y axis 
    # main=FALSE draws it at the left of the window
    # Note: we start at index 1 rather than 0, for buffer
    if (show_n) {
      grid::pushViewport(
        grid::viewport(
          layout.pos.col = 3, 
          layout.pos.row = 2,
          yscale = c(-0.5,length(category_count_labels)+1.5),
          name = paste0("right_margin",id)
        )
      )
      #grid::grid.yaxis(
      #  at=1:length(category_count_labels), 
      #  label = category_count_labels,
      #  gp=n_gp
      #
      grid::grid.rect(gp = gpar(lty = 0))
      #grid::grid.xaxis() 
      #grid::grid.yaxis()
      grid::grid.text(
        label = category_count_labels,
        y = 1:length(category_count_labels),
        default.units = "native",
        x = 0.5,
        just = "right",
        gp = n_gp
      )
      grid::grid.text(
        label = "N",
        y = length(category_count_labels)+1,
        default.units = "native",
        x=0.5,
        just = "right",
        gp = grid::gpar(fontface = "bold", cex=cex)
      )
      grid::upViewport()
    }
    
    
    # The violin plot. Most of the code in the function is for the violin plot area.
    grid::pushViewport(
      grid::viewport(
        layout.pos.col = 2, 
        layout.pos.row = 2,
        name = paste0("plot",id), 
        xscale = extendrange(value_limit), 
        yscale = c(-0.5,length(category_count_labels)+1.5),
        clip="off"
      )
    )
    
    # Draw plot area.
    grid::grid.rect()
    # Create x axis
    grid::grid.xaxis()
    # Create left y axis
    grid::grid.yaxis(
      at=1:length(category_names),
      label=category_names,
      gp=category_gp
    )
    
    # Create right y axis 
    # main=FALSE draws it at the left of the window
    # Note: we start at index 1 rather than 0, for buffer
#    if (show_n) {
#      grid::grid.yaxis(
#        at=1:length(category_count_labels), 
#        main=FALSE, 
##        label = stringr::str_pad(
#          string = category_count_labels,
#          width = max(stringr::str_length(category_count_labels)),
#          side="left"),
#        gp=n_gp
#      )
#    }
    
    category.list<-levels(X$category)
    for (i in at) {
        values<-X$values[which(X$category==category.list[i])]
        if (color_by == "label") {
          local.labels<-factor(as.character(X$labels[which(X$category==category.list[i])]),levels=label_levels)
        }
        if (na.rm == TRUE) {
            values <- values[!is.na(values)]
            if (color_by=="label") {
              local.labels <- local.labels[!is.na(values)]
            }
        }
        # The below calculates the density for the overall distribution,
        # We extend the range a bit to make it look nicer (hopefully going to 0)
        # The max/min ensures it stops at the builtin range (value_limit).
        # Note: the default kernel is bw.nrd0, the default value of cut is 3. These are hard-coded here
        # based on existing R density defaults.
        xrange<-range(values)-bw.nrd0(values)*3
        value_min<-max(min(values)-bw.nrd0(values)*3, value_limit[1])
        value_max<-min(max(values)+bw.nrd0(values)*3, value_limit[2])
        d <- density(values, bw = bw, from=value_min, to=value_max)
       
        # if required by colorby, will find the density of subsets
        # of the overall distribution based on the variables colors.
        if ( color_by == "label") {
            d.subset<-data.frame(row.names=d$x)
            for (lab in label_levels) {
              d.subset[,lab]<-density(values[which(local.labels==lab)],from=min(d$x),to=max(d$x),bw=bw)$y
            }
            
        }
        b <- boxplot.stats(values, coef = range)
        violin_value<-d$x
        # Adjust the width of the violin by the maximum density value observed
        if (equal_width == FALSE) 
            width_2 <- violin_width * max(d$y)
        else 
          width_2 <- violin_width
        
        violin_density <- 0 + (d$y - min(d$y)) * ((0.5 - violin_end_width) * 
            width_2)/diff(range(d$y))
      
        
        #
        #
        # Draw the actual violin. This uses the polygon method with vertices taken
        # from the density function.
        #
        plot.density<-c(at[i] + violin_end_width * violin_width, 
                          at[i] - violin_end_width * violin_width,
                          at[i] - violin_density[1] - violin_end_width * violin_width,
                          at[i] + rev(violin_density)[i] + violin_end_width * violin_width
                        )
        
        # First y polygon is minimum value to first coordinate calculated
        plot.value<-c(min(d$x), min(d$x), d$x[1],d$x[1])

        # First polygon color is lowest value
        if (color_by=="density") {
          plot.color<-c(density_colors[1])
        } else {
          # with color_by, pick the density with the largest value at
          # the evaluated point and color by that.
          # Find the max, then assign the appropriate color
          plot.color<-label_colors[which.max(d.subset[1,])]
        }
        for (j in 1:length(violin_density)) {
          plot.density<-c(plot.density, 
                    at[i] + violin_density[j] + violin_end_width * violin_width,
                    at[i] - violin_density[j] - violin_end_width * violin_width,
                    at[i] - violin_density[j+1] - violin_end_width * violin_width,
                    at[i] + violin_density[j+1] + violin_end_width * violin_width
          )
          plot.value<-c(plot.value, 
                    violin_value[j],violin_value[j],
                    violin_value[j+1],violin_value[j+1]
                    )
          # Default plot color is the density at the point, scaled to
          # a proportion.
          if (color_by=="density") {
            plot.color<-c(plot.color, density_colors[(max.colors*d$y[j]/max(d$y))+1])
          } else if ( color_by == "label") {
            # with color_by, pick the density with the largest value at
            # the evaluated point and color by that.
            # Find the max, then assign the appropriate color
            max.subset<-which.max(d.subset[j,])
            plot.color<-c(plot.color, label_colors[max.subset])
          }
        }
       
        #####################################################################################
        # Draw the violins and boxplots.
        ####################################################################################
        # grid.polygon is the major component of this. The data structure was built consisting
        # of a bunch of small polygons with individual colors. Each polygon has four points 
        # representing the four edges (we are actually doing a rectangle).
        
        
        grid.polygon(x=plot.value, y=plot.density,
                # fill polygons with plot color (a vector).
                # col=0 turns off borders
                gp=gpar(fill=plot.color, col=0),
                default.units="native",
                id.lengths=rep(4,length(violin_density)+1))
        
       grid.lines(y = c(at[i], at[i]), x = b$stats[1:2], 
                  gp=gpar(lineend="butt", lwd = 1),
                  default.units="native"
       )
        grid.lines(y = c(at[i], at[i]), x = b$stats[4:5], 
                   gp=gpar(lineend="butt", lwd = 1),
                   default.units="native"
        ) 
        grid.polygon(y = c(at[i] - box_width * violin_width, at[i] + 
                        box_width * violin_width, at[i] + box_width * violin_width, 
                      at[i] - box_width * violin_width), x = c(b$stats[2], 
                                                               b$stats[2], b$stats[4], b$stats[4]), 
                     gp=gpar(col = box_col),
        default.units="native")
        
        grid.lines(y = c(at[i] - box_width * violin_width, at[i] + 
                      box_width * violin_width), x = c(b$stats[3], b$stats[3]), 
              gp=gpar(lineend="butt", lwd = 4, col=median_col),
              default.units="native"
        )
        
        if (plot_mean) 
          grid.points(y = at[i], x = mean(x), pch = mean_pch, gp=gpar(col = mean_pch_col),
                      default.units="native")
        if (show_outliers) 
          points(y = rep(at[i], length(b$out)), x = b$out, pch = pch,
                 default.units="native")
    }
    
    # Add a grand median line across all violins.
    if ( grand_median_line) 
      grid::grid.lines( # plot a line x = median, y = top to bottom.
        x=grid::unit( c(median( X$values), median(X$values)),"native"), 
        y=grid::unit( c(-0.5,length(category_count_labels)+1.5),"native"), 
        gp=grand_median_gp
    )
    
    # End the violin plot area plotting.
    grid::upViewport()

    # Final result of function is the full figure (current viewport)
    popViewport()
}
