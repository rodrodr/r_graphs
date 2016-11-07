
# Multivariate Violin Plot in R


The function mvioplot is a modified version of the original vioplot function of the package "vioplot" created by Daniel Adler, and available at CRAN.

Violin plots are a combination of a box plot and a probability density function plot. Together, these two graphs allow the analysis of the distribution of the data, its degree of dispersion/concentration, as well as the type and shape of the probability distribution in a given set of cases.

The purpose of this graph is to include new functionalities and allow the graphical representation of multiple density functions (sharing the same scale) in one graph. It also aims at simplifying the use of groups of cases. 
 
The function extends the functionalities of the original package and allows the customization of some graphical parameters not available in the original code. The two main advancements are the parameters "by", allowing the automatic division of cases into groups, and "side", that allows to plot density functions on just one side of the graph. 

For a clearer look and design, it also replaces polygons by lines in order to avoid visual overload when multiple density functions are represented.  

We also added some style parameters to change the color of each graphical element (line color, width, type, boxplot line color,type, width, boxplot rectangle color, etc.).

The parameters used are the following:

**x -** nummeric vector containing the values to be represented.

**by -** optional -categorical variable containing the values used to group cases.

**range -** the size of the interquartile range to represent in the boxplot lines - default 1.5 times the IQR.

**h -** this parameter establishes the height of the density estimator for the sm.density function. 

**ylim -** establishes the limits for the scale to be represented in the graph.

**names -** allows users to define the names of the groups to be represented.

**horizontal -** changes the orientation of the violin plot from vertical to horizontal.

**col -** the color of the line. The default is "black".

**lty -** the type of the line. For each value of this parameter, see help("par").

**lwd -** the width of the line. For each value of this parameter, see help("par").

**rectCol -** the color of the rectangle composing the boxplot. The default is "gray50".

**rectBord -** the color of the border of the boxplot rectangle. The default is "black".

**colMed -** the color of the median. The default is "black".

**colMean -** the color for the mean. The default is "red".

**pchMed -** the symbol employed to represent the median. The default is 19 - circle.

**pchMean -** the symbol employed to represent the mean. The default is 19 - circle.

**at -** defines the position in the axis to place each group.

**add -** enables users to add a new violin plot to a previously existing one. The default value is FALSE.

**wex -** relative expansion of the violin. The default value is 1.

**drawRect -** defines if the boxplot will be ploted or not. This is useful when users want to draw more than one density function at the  same side of the violin plot. This allows the comparison of multiple density functions in one graph.

**side -** the side of the plot to draw the density function lines. The possible values are "both","hboth" - horizontal, left, right, up, down. When users define "hboth", "up", or "down", the function automatically changes the orientation to horizontal. The default value for this parameter is "both".

**box -** designs a box around the violin plot. The default is false.

**lag -** defines the space from the middle of the violin plot. It is useful to avoid the juxtaposition of boxplots when users are employing two different variables in each side of the violin plot. The default value is 0.03.

**plot.median -** defines if the median value will be plotted. The default value is TRUE.

**plot.mean -** defines if the mean value will be plotted. The default value is FALSE.

**lwd.line -** the line width for the boxplot whisker. The default is 1.

**lty.line -** the line type for the boxplot whisker. The default is 1.

**col.line -** the line color for the boxplot whisker. The default is "black".

**inv.y -** defines the scale to be decreasing. The default is FALSE. 



# Examples

Firstly, I will create some data based on the Brazilian Atlas of Human development of 2013.

Variables: year, Municipal Human Development Index, Gini Coefficient, and Urbanization Rate:

```{r}
a <- c(rep(1991,5570),rep(2000,5570),rep(2010,5570)) 
b <- c(rnorm(5570, 0.3813723, 0.10309697),rnorm(5570, 0.5234809, 0.10439613),rnorm(5570, 0.6591574, 0.07199728))
c <- c(rnorm(5570, 0.5254573, 0.07199823),rnorm(5570, 0.5470584, 0.06867278),rnorm(5570, 0.4943810, 0.06607458))
u <- c(rnorm(5570, 0.48491, 0.2680103),rnorm(5570, 0.5849109, 0.2366299),rnorm(5570, 0.6382640, 0.2204113))
u[u<0] <- 0
u[u>1] <- 1
d <- data.frame("year"=a, "HDI.M"=b, "GINI"=c, "urb_rate"=u)


```



Makes a simple violinplot of the urbanization rate
```{r echo=TRUE}
par(mar=c(3,3,1,0))
mvioplot(d$urb_rate)
```

The "by" parameter allows the division of the data into n categories or groups
```{r echo=TRUE}
mvioplot(d$HDI.M, by = d$year)
```


Changes the style of the plot
```{r echo=TRUE}
mvioplot(d$urb_rate, col = "orange", lwd = 2, lty = 2, colMed = "red", rectCol = "#F7BE81", col.line = "blue", rectBord = "blue")
```

The parameter "side", allows to plot the density function in just one side of the graph
```{r echo=TRUE}
mvioplot(d$urb_rate, side="left")
mvioplot(d$urb_rate, side="right")
mvioplot(d$urb_rate, side="up")
mvioplot(d$urb_rate, side="down")
```

With the "add" parameter, it is possible to plot more than one variable in the same graph
```{r echo=TRUE}
mvioplot(d$urb_rate, side="left")
mvioplot(d$HDI.M, side="left", col="orange", lty=2, drawRect = F,  add=T)
mvioplot(d$GINI, side="left", col="steelblue", drawRect = F, add=T)
```

With the "lag" parameter, it is possible to add space in between the two sides of the violin plot
```{r echo=TRUE}
mvioplot(d$urb_rate, side="left", lag=0.04)
mvioplot(d$HDI.M, side="right", col="orange", add=T, lag=0.04)
```

Styles the violin plot in order to clearly differentiate variables
```{r echo=TRUE}
mvioplot(d$HDI.M , d$year, side="left", lag=0.04, ylim=c(0,1), lwd=2)
mvioplot(d$GINI , d$year, side="right", lag=0.04, col="orange", rectCol = "#F7BE81", col.line = "orange", rectBord = "orange",colMed = "orange", add=T, lwd=2)
legend("bottomright", fill=rep("white",2), border = c("black","orange"), legend = c("Human Development Index", "Gini Index"), bty = "n", horiz = F, x.intersp = 0.2)
```


```{r echo=TRUE}

```



