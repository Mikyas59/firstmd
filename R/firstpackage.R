#My First Package.
##dependencies: gt, gtsummary, kableextra, ggplot2, modelsummary
library(gt)
library(gtsummary)
library(tidyverse)
library(modelsummary)
library(kableExtra)


# use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))

# Data transformation functions
##Appropriately create factor variables when using case_when. Otherwise, variable levels will not be as intended.

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}


# Table Functions

##Convert a gt table to pdf friendly.
##Supply the table (x) and caption (y).

gt_to_pdf <- function (x, y, ...) {

  pdf_t <- x %>%
    as_kable_extra(
      booktabs= TRUE,
      # longtable = TRUE,
      linesep = "",
      caption = y
    )%>%
    kableExtra::kable_styling(
      position = "center",
      latex_options = c("striped"),
      stripe_color = "gray!15"

    )

}


## Refined Table-one from a gtsummary table with only the relevant variables
##Supply gt table and caption (y).
custom_table_one <- function(gt_tbl, y, ...) {

my_tbl_pdf <- gt_tbl%>%
  as_kable_extra(
    booktabs= TRUE,

    linesep = "",
    caption = y

  )%>%

  kableExtra::kable_styling(

    # longtable = T,
    full_width = T,
    position = "center",
    latex_options = c("striped", "full_width"),
    stripe_color = "gray!15"
  )%>%
  column_spec(1,2, width_min = "10cm")

my_tbl_pdf

}


##Refined table for producing a table with multiple models.
## supply a list of models, a title and a footnote.

models_table <- function(mod, xtitle, ftnote, ...) {

  my_model_table2 <-  function(mod, xtitle, ...) {
    modelsummary(mod,
                 shape = term ~ model:statistic,
                 coef_rename = T, stars = T,
                 title = xtitle, ...

    )
  }


  my_model_table2(mod, "Summary of Multivariate Mixed Effects Models", gof_omit = 'RMSE|AIC|BIC|Log.Lik.',
                  coef_omit = 'Intercept', output = "kableExtra", booktabs = TRUE, longtable = TRUE)%>%
  kableExtra::kable_styling(
    full_width = T,
    position = "center",
    stripe_color = "gray!15")%>%
  kableExtra::kable_styling(
    stripe_color = "grey!15")%>%
  column_spec(1, width_max = "20cm")%>%
  add_footnote(gt::md(ftnote), notation = "alphabet")

}







# Plot Functions

##Modified bbplot functions.
##Add custom theme to ggplot chart.

custom_style <- function() {
  font <- "Roboto"

  ggplot2::theme(

    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=20,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    # plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function

    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    # legend.title = ggplot2::element_blank(),
    # legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="#222222"),

    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    # axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 12)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),

    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),

    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

# Model checking functions
##Model checking functions for checking ols and mixed effects model.
##check linear regression models.
check_ols <- function(x, ...) {

  par(mfrow = c(2,2))

  residual_plots <- plot(x)
  residual_plots

  z<- car::vif(x)

  y <- summary(x)

  adjr2 <- y$adj.r.squared

  print(c(z, adjr2))
  print(residual_plots)

}



##check linear mixed effects model. Simply supply the model.
check_lmem.1 <- function(m, ...){
  ##A working function to check the assumptions in a linear mixed effects model.
  ##To be improved with HLMDiag package functionality at a future time.

  #m = Model

  a <- plot(m,type=c("p","smooth"), main = "Fitted v residual")

  b <- plot(m,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"), main = "Scale-location")

  c <- lattice::qqmath(m,id=0.05, main = "QQplot") ## quantile-quantile

  d <- cowplot::plot_grid(a,b,c, nrow = 2)

  #Calculate leverage
  lev<-hat(model.matrix(m))

  #Plot leverage against standardised residuals
  e <- plot(resid(m,type="pearson")~lev,las=1,ylab="Standardised residuals",xlab="Leverage")


  #Calculate Cook's Distance
  cd<-cooks.distance(m)
  #N.B. If Cook's distance is greater than 1 this highlights problematic datapoints

  #Plot leverage and Cook's distance together
  par(mfrow=c(1,1))
  f <- (plot(lev,pch=16,col="red",ylim=c(0,2.5),las=1,ylab="Leverage/Cook's distance value",sub = ">1 cd indicates problematic datapoints")+
          points(cd,pch=17,col="blue")+
          points(x=150,y=1.1,pch=16,col="red")+
          points(x=150,y=0.9,pch=17,col="blue")+
          text(x=155,y=1.1,"Leverage",adj=c(0,0.5))+
          text(x=155,y=0.9,"Cook's distance",adj=c(0,0.5)))

  d


}







