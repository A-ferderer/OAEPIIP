Here we demonstrate one way to analyse the data which is likely to be collected during your OAEPIIP experiment. We have previously used this method to assess similar dataset and therefore recommend the use of Generalised additive mixed models (GAMMs) here as well (see [Ferderer et al., 2022](https://doi.org/10.5194/bg-19-5375-2022)). We highlight that this is not a tutorial and as such will not have in depth explanations or background information regarding the use of GAMMs. If the reader desires such explanations we recommend reading [S. N. Wood (2006)](https://doi.org/10.1201/9781315370279).

First we need to load the necessary packages (note if you have not installed these packages you will need to install them first using "install.packages()).


```{r, eval=TRUE,echo = FALSE}
library(mgcv)
library(mgcViz)
library(itsadug)
library(ggplot)

```

You will also need to download the csv file from the repository "GAMM_example.csv".
Now you can import this dataset and inspect the data 

```{r, eval=TRUE, echo = FLASE}
data = read.csv("GAMM_example.csv",fileEncoding = "UTF-8-BOM")
head(data)
str(data)
```

You will notice that microcosm and treatment are specified as character variables in our dataset. The mgcv package does not recognise character variables and thus we must change these to factor variables.

```{r, eval=TRUE, echo = FLASE}
data$Microcosm = as.factor(data$Microcosm)
data$Treatment = as.factor(data$Treatment)
str(data)
```

For the OAEPIIP there are four potential scenarios we would like to assess with our significance testing
1. The treatment has no significant effect on the dependent variable
2. The Treatment significantly effects the time at which changes in the dependent variable occur
3. The treatment has an effect on the absolute values of the dependent variable
4. The treatment has a significant effect on the timing and absolute values of the dependent variable 

![OAEPIIP_example](https://github.com/OAEPIIP/OAEPIIP-Statistics-example/assets/113956826/2b85b854-e4f1-4339-a1a0-fa69fe1fd635)

adapted from Ferderer et al. (2022) (https://doi.org/10.5194/bg-19-5375-2022)

In order to assess these we need to vary how the independent variable is included into our GAMM.

1. gamm_1 assumes Treatment is not significant and therefore we don't need to include it in the model.
2. gamm_2 assumes the Treatment has a significant influence on the timing of the dependent variable, thus we let Day vary by Treatment.
3. gamm_3 assumes the Treatment has a significant influence on the absolute value of the dependent variable, thus we add treatment as an additive variable.
4. gamm_4 assumes the Treatment has a significant influence on the absolute value and timing of the dependent variable, thus we add treatment as an additive variable and let day vary by treatment.

```{r, eval=TRUE, echo = FLASE}
gamm_1 <- gam((Y) ~ s(Day, k =12) + s(Day,Microcosm, bs = "fs"), family = gaussian (), method = "REML", data = data)#GAMM model 1
plot(gamm_1, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", pages = 1, all.terms = TRUE, seWithMean = TRUE)#plot type 1
plot_smooth(gamm_1, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")#plot type 2
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output
# you will notice an error here saying that the two smooth terms are essentially repeating the same information or are highly correlated. This is okay as we know this is likely the case


gamm_2 <- gam((Y) ~ s(Day, by = Treatment, k =12) + s(Day,Microcosm, bs = "fs"),family = gaussian (), method = "REML", data = data)#GAMM model 2
plot(gamm_2, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", pages = 1, all.terms = TRUE, seWithMean = TRUE)#plot type 1
plot_smooth(gamm_2, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")#plot type 2
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output


gamm_3 <- gam((Y) ~ s(Day, k =12) + s(Day,Microcosm, bs = "fs") + Treatment, family = gaussian (), method = "REML", data = data)#GAMM model 3
plot(gamm_3, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", pages = 1, all.terms = TRUE, seWithMean = TRUE)#plot type 1
plot_smooth(gamm_3, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")#plot type 2
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output


gamm_4 <- gam((Y) ~ s(Day,by =Treatment, k =12) + s(Day,Microcosm, bs = "fs") + Treatment, family = gaussian (), method = "REML", data = data)#GAMM model 4
plot(gamm_4, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", pages = 1, all.terms = TRUE, seWithMean = TRUE)#plot type 1
plot_smooth(gamm_4, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")#plot type 2
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output
```
Building on the [Introduction]( https://github.com/OAEPIIP/OAEPIIP-Statistics-example/blob/main/OAEPIIP%20statistical%20analysis%20example/Introduction.md) you will notice the addition of "k" and "s(Day,Microcosm, bs = "fs"" in all models. "K" is the number of basis functions or "knots" (see [S. N. Wood (2006)]( https://www.taylorfrancis.com/books/mono/10.1201/9781315370279/generalized-additive-models-simon-wood) for more details). For our OAEPIIP dataset this will be the number of days our y-variable (e.g. nutrients) is measured. The other section "s(Day,Microcosm, bs = "fs"" specifies our random effect which controls for temporal pseudoreplication (repeated measurements from the same microcosm). Here, we fit Microcosm so that each level of the random effect (or each microcosm) has its own smooth.

Now we can compare our models using AIC and R square values. This will help us decide which of our four models explains our data the best.
```{r, eval=TRUE, echo = FLASE}
# Create a data frame to store the results
model_results <- data.frame(Model = character(),
                            AIC = numeric(),
                            R_squared = numeric(),
                            stringsAsFactors = FALSE)

# Function to extract AIC and R-squared values from a model
extract_model_info <- function(model) {
  AIC_value <- round(AIC(model),3)
  R_squared <- round(summary(model)$r.sq, 3)
  return(c(AIC = AIC_value, R_squared = R_squared))
}

# Apply the function to each model and populate the data frame
model_results[1, ] <- c("gamm_1", extract_model_info(gamm_1))
model_results[2, ] <- c("gamm_2", extract_model_info(gamm_2))
model_results[3, ] <- c("gamm_3", extract_model_info(gamm_3))
model_results[4, ] <- c("gamm_4", extract_model_info(gamm_4))

# Print the table
print(model_results)
```

You will notice that gamm_2 has the lowest AIC value but has the same R squared value as gamm_4. Furthermore, the difference in AIC values is < 2 which is a general threshold for determining significant differences between models. Thus, you could safely pick either model 2 or 4 in this case, but first we should inspect the plots.
```{r, eval=TRUE, echo = FLASE}
par(mfrow = c(1, 2))#change plot view
plot_smooth(gamm_2, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output
plot_smooth(gamm_4, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
with(data, points(Day, Y, col = "grey50", pch = 16))#overlaying raw data on GAMM model output
```

There is a limitation to our model comparison which becomes apparent when visualising the data. You will notice that gamm_2 underestimates the difference in the slope of the relationship between Y and Days, particularly for the equilibrated treatment. This data is an example of dissolved inorganic nutrient data thus we would expect there to be no difference in y (the amounts/concentrations) between treatments. When we compare the start and end values given for "y" this is true. However, our model comparison shows that although this is true gamm_4 provides a better fit to the actual data. This is because in gamm_2 the exclusion of treatment as an additive effect forces the treatments to have identical absolute values over the experimental treatment which alters the fit of the smoother, in particular the start and end value, contorting our data. Therefore, under this scenario we should go with gamm_4. It is important to note that this is not always the case e.g. Chla can vary by absolute values between treatments as to can abundance etc. However, if you have a good understanding of the parameter and follow this tutorial you should be able to appropriately select the best model and infer significance from this and visual inspection of the model.









Although we have already decided upon our best model above it is important to also check that the assumptions of GAMMs are being adequatley met for. We will do this as an example for Gamm_4 however, it is good practice to do this for all models while keeping in mind that in some cases models will not meet the assumptions. This is only accetable if the model in question is not being used to describe the statistical significance of a variable (i.e. it is not your final model). It is also important that the random effects structure (s(Day,Microcosm, bs = "fs") and number of knots (k) is consistent across the models.

First we will check our model using gam.check
```{r, eval=TRUE, echo = FLASE}
par(mfrow = c(2, 2))
gam.check(gamm_4)
```

First looking at the four plots on the right. We ideally want to see that for plot 1) all points fall on the red line, 2) points are randomly scattered around 0, 3) a normal distribution/bell curve shape and 4) a straight line. The plots shown here fit the desired outcomes relatively well and in this scenario we would accept that the assumptions of our model are being met. In scenarios where this is not the case (see: [example]( https://r.qcbs.ca/workshop08/book-en/gam-model-checking.html)) it is important to consider; 1) the number of basis functions (k) which should be the same number of sampling days for the given parameter, 2) the "family" argument, what data type is your y variable, and 3) consider transforming the data.

You will also notice some text in the console output. The main thing to check here is that the p-values are >0.05. If they are <0.05 it may mean that there are not enough basis functions (k). Basis functions are the functions which make up our smooth terms. Too many basis functions will result in "overfitting", while too little results in important data being excluded.



We must also check for concurvity. This checks to see if one of our smooth terms is the same as another smooth term. This is similar to collinearity in linear models. Note, we will need to specify "full=FALSE" to inspect matrices of pairwise concurvities. These show the degree to which each variable is predetermined by each other variable, rather than all other variables.

```{r, eval=TRUE, echo = FLASE}
concurvity(gamm_4, full = FALSE)
```

This produces a large table in the console. When looking at this table you should look at the "worst" table and ensure that comaprisons between different treatments i.e. control vs equilibrated are less than 1 while comparisons between the same treatment will give a value of or close to 1. It is safe to ignore the column "Microcosm" here as the specification of Microcosm as a random effect prevents any meaningful interpretation here.

Finally we can have a look at the model summary now
```{r, eval=TRUE, echo = FLASE}
summary(gamm_4)
```

There is a lot to look at here but essentially the parametric coefficients explain our linear terms, in our case the additive term Treatment. But the Approximate significance of smooth terms is what we are interested in. "edf" is the effective degrees of freedom with 1 = a straight line and the higher the number the more wiggly the smooth function is. "ref.df" and "f" are test statistics used in anova but these are only approximate. Finally our p value is showing statistical significance of each term, however this is approximate only and it is recomended to a) visually check this and b) compare several models via AIC values to establish the significance of variables (which we do above).


