Introduction

First it is important to understand why phytoplankton time series data is unique and the difficulties associated with the appropriate analysis of such datasets. It is uncommon for phytoplankton associated time series data e.g. nutrient concentrations over time, to follow linear relationships. This prevents the analysis of such data using linear methods such as the linear regression as it violates one of the four major assumptions of linear models:
 
  1. There is a linear relationship between the dependent and independent variables

Because much of our data does not fit this assumption we must utilise other methods to appropriately assess our results. One such method involves the use of Generalised Additive Models or GAMs. Briefly, GAMs are a specific form of model whith the ability to incorporate non-linear distributions.

To better understand this we can use the "mcycle" dataset available in the "VarReg" package. First we will need to load the "VarReg" and inspect the dataset "mcycle"

```{r, eval=TRUE,echo = FALSE}
library(VarReg)
head(mcycle)
str(mcycle)
plot(mcycle)
```
The mcycle dataset has two numerical/continuous variables, times and accel. You will notice from the simple plot that the data is not linearly correlated

We can now look at fitting a simple linear regression using this dataset which will illustrate the primary issue with using linear models on non-linear datasets.

```{r, eval=TRUE,echo = FALSE}
lm_mod = lm(accel ~ times, data = mcycle)
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
```
You will notice that our linear model is doing a poor job of fitting our actual data points. In some cases you may be able to transform data however this will not work with all datasets and this is where GAMs are useful.

To fit a GAM we first need to load the mgcv package

```{r, eval=TRUE,echo = FALSE}
library(mgcv)
```
Now we can fit our model following the same simple inputs used for our linear model above.

```{r, eval=TRUE,echo = FALSE}
gam_mod <- gam(accel ~ s(times), data = mcycle)
```
This is the same as a model above with the exception that accel has now been fitted as a smooth function of the covariate "times". It is this smooth function which enables us to model non-linear relationships between two variables.

A simple plot for visualising our model can be obtained using:
```{r, eval=TRUE,echo = FALSE}
plot(gam_mod, residuals = TRUE, pch = 1)
```
In this plot you will see that we now have a non-linear relatioship and you can see from the plot that this model does a much better job of explaining raw our data.

The datasets that you will develop during the OAEPIIP experiment is potentially more complex than this simple dataset shown here. At the conclusion of your experiment you will have measured several variables (y) for each treatment (n=3) with three microcosms in each treatment and each of these microcosms being measured on serveral occasions over the experimental period.
This is an example of a hierarchical dataset with temporal pseudo replication. In order to deal with such a dataset you would normally use linear mixed effects models, however due to the non-linear nature of much of the data collected in phytoplankton time series datasets it is unlikely that these will be appropriate, hence the need for GAMs.

