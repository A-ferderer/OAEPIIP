# OAEPIIP statistical analysis example

First it is important to understand why phytoplankton time series data is unique and the difficulties associated with the appropriate analysis of such datasets. It is uncommon for phytoplankton associated time series data e.g. nutrient concentrations over time, to follow linear relationships. This prevents the analysis of such data using linear methods such as the linear regression as it violates one of the four major assumptions of linear models:
 
  1. There is a linear relationship between the dependent and independent variables

Because much of our data does not fit this assumption we must utilise other methods to appropriately assess our results. One such method involves the use of Generalised Additive Models or GAMs. Briefly, GAMs are a specific form of model whith the ability to incorporate non-linear distributions.

To better understand this we can use the "mcycle" dataset available in the "VarReg" package

```{r, eval=TRUE,echo = FALSE}
library(ggplot2, quietly = TRUE)
library(mgcv, quietly = TRUE)
```
