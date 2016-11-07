
# Multivariate Violin Plot in R


The function mvioplot is a modified version of the original vioplot function of the package "vioplot" created by Daniel Adler, and available at CRAN

Violin plots are a combination of a box plot and a probability density function plot. Together, these two graphs allow the analysis of the distribution of the data, its degree of dispersion/concentration, as well as the type and shape of the probability distribution in a given set of cases.
 
The function extends the functionalities of the original package and allows the customization of some graphical parameters not available in the original code. The two main advancements are the parameters "by", allowing the automatic division of cases into groups, and "side", that allows to plot density functions on just one side of the graph 

For a clearer look and design, it also replaces polygons by lines in order to avoid visual overload.  

We also added some style parameters to change the color of each graphical element (line color, width, type, boxplot line color,type, width, boxplot rectangle color, etc.).





