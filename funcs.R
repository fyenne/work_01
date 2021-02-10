kbt <- function(...){
  knitr::kable(..., format.args = list(big.mark = ',', scientific = F)) %>%
    kableExtra::kable_styling(c("striped", "hover", "condensed"),
                              full_width = F,
                              position = "center",
                              row_label_position = "c") %>%
    row_spec(0, bold = T, color = "white", background = "#004d1f")
}

gpt <- function(...){
  ggplot2::ggplot(...) + 
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5), 
          axis.text.x = element_text(angle = 0, hjust = 0.5))
}

stg <- function(...){
  stargazer::stargazer(..., 
                       type = "text",
                       # style = "qje",
                       # title = "daily_kwh ~ post:encouraged|customer_id + no.bill|0|customer_id",
                       keep.stat = c("n", "ser"),
                       digits = 5)
}
#




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#--------------------------------------------

# rmse_p <- function(pred_y, real_y, ){
#   ggplot(final, aes(x = pred_y,y = real_y)) + 
#   geom_point(alpha=0.65, col ="grey") + 
#   scale_x_continuous(limits=c(0,1)) +
#   scale_y_continuous(limits=c(0,1)) +
#   geom_abline(intercept=0,slope=1) +
#   geom_smooth(se=0, smethod = "lm") +
#   coord_fixed(1) +
#   ggtitle("RF")
# }