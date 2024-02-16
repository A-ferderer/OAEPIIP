GAM example

Now that we understand why GAMs can be so useful, let's have a look at a dataset which is similar to what can be expected from OAEPIIP.
First we need to load the necesarry packages (note if you have not installed these packages you will need to install them first using "install.packages()).

```{r, eval=TRUE,echo = FALSE}
library(mgcv)
library(mgcViz)
library(itsadug)

```
You will also need to download the csv file from the repository "GAM_example.csv".
Now you can import this dataset and inspect the data 

```{r, eval=TRUE, echo = FLASE}
data = read.csv("GAM_example.csv",fileEncoding = "UTF-8-BOM")
head(data)
str(data)
```
You will notice that microcosm and treatment are specified as character variables in our dataset. The mgcv package does not recognise character variables and thus we must change these to factor variables.

```{r, eval=TRUE, echo = FLASE}
data$Microcosm = as.factor(data$Microcosm)
data$Treatment = as.factor(data$Treatment)
str(data)
```
First we will fit a simple GAM model and explore possible plotting functions. There are many ways to visualise GAMs e.g. using R's default graphics package, the mgcViz package and itsadug package.
It is very importat to visualise GAMs as we will find out later, so it is good to understand the different ways to plot your results. You will also notice all the plots look relatively similar but as our model gets more complex you will undertsand why we use various plotting packages.

```{r, eval=TRUE, echo = FLASE}
gam_mod <- gam(Y ~ s(Day), 
               family = gaussian (), method = "REML", data = data)

plot(gam_mod, residuals = TRUE, pch = 1)

plot(gam_mod, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", 
     pages = 2, all.terms = TRUE, seWithMean = TRUE)

plot_smooth(gam_mod, view = "Day", main = "intercept + s(Day)")
```

There are two issues that we need to adress before this model is applicable to the data collected during OAEPIIP. The first inolves accounting for temporal pseudoreplication caused by repeated measurements from each microcosm.

To account for temporal pseudo replication we will add "Microcosm" as a random effect. For those with experience in linear mixed effects models you will know that there are several types of random effects random intercepts, random slopes and in gams random smooths as well. Here we will fit Microcosm so that each level of the random effect (or each microcosm) will have its own smooth. If you want to see other ways a random effect can be fitted see the "Random effetcs.md"

Note when we add a random variable to a GAM it becomes a Generalised Additive Mixed Model or GAMM.

```{r, eval=TRUE, echo = FLASE}
gam_mod1 <- gam(Y ~ s(Day) + s(Day,Microcosm, bs = "fs"),
                family = gaussian (), method = "REML", data = data)

plot_smooth(gam_mod1, view="Day", plot_all="Microcosm", rm.ranef=F, xlab = "Day", ylab = "Y")
```
Now that we have accounted for the temporal pseudoreplication we can add our "Treatment". There are several ways to add Treatment to the model (which we will explore later) but for now we will add it so that it accounts for the most variability possible.

You will notice "Treatment" appears twice in the model. The first instance "s(Day, by = Treatment)" allows the smooth function to vary by each level of "Treatment". The second instance "+ Treatment" is specifying Treatment as an additive variable and allows the smoothers to vary by intercept for each level of Treatment.
```{r, eval=TRUE, echo = FLASE}
gam_mod2 <- gam(Y ~ s(Day, by = Treatment) + Treatment + s(Day,Microcosm, bs = "fs"),
                family = gaussian (), method = "REML", data = data)
plot_smooth(gam_mod2, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
```

Now we need to explore our model fit. First we can compare our model to our actual data or averages for each treatment
```{r, eval=TRUE, echo = FLASE}
avg_Y <- aggregate(Y ~ Treatment + Day, data = data, FUN = mean)

ggplot(data = avg_Y, aes(x = Day, y = Y, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Y by Treatment",
       x = "Day",
       y = "Average Y")
```

It looks like our GAMM may be modelling some noise, see the control after day 5. Lets check our model and some of the assumptions of GAMs/GAMMS, we can do this using gam.check
```{r, eval=TRUE, echo = FLASE}
par(mfrow = c(2, 2))
gam.check(gam_mod2)
```
Looking at the plots you'll notice a few things, our model struggles to fit assumptions of gaussian data, mainly a normal dsitribution (top right plot).
In the text you'll see significant p values, in this space small p values indicate non-random distribution and suggests the model is not using enough basis functions. Basis functions are the functions which make up our smooth terms, to many results in "overfitting", while too little results in important data being excluded.
There are two things that can help with this the first is transforming your data to improve the fit and the second is specifying the number of basis functions or knots. In our case we should have as many knots as we have days/measurements. Knots are specified using "k=" in your smooth term for x
```{r, eval=TRUE, echo = FLASE}
gam_mod3 <- gam((Y) ~ s(Day, by = Treatment, k =12) + Treatment + s(Day,Microcosm, bs = "fs"),
                family = gaussian (), method = "REML", data = data)

par(mfrow = c(2, 2))
gam.check(gam_mod3)
```
Looking again at our gam.check you will see the fit is relatively good for all except the top left plot. You may chose to transform this data however for this example you'll notice see there is no visual improvement in the model fit, so we will leave it as is. The other option is to change the "family" argument in your model to something that is better equipped to handle non-gaussian data e.g. "scat".


We must also check for concurvity. This checks to see if one of our smooth terms is the same as another smooth term. This is similar to colinearity in linear models.
Ideally we want values less than 0.8 in the worst row. Note we will need to specifiy "full=FALSE" to inspect matrices of pairwise concurvities. These show the degree to which each variable is predetermined by each other variable, rather than all  other variables.
You will notice that our our random effect was causing issues with the previous model but you can see that by specifying "full=FALSE" our assumptions for each treatment are now met
```{r, eval=TRUE, echo = FLASE}
concurvity(gam_mod3, full = TRUE)
concurvity(gam_mod3, full = FALSE)
```

Finally we can have a look at the model summary now
```{r, eval=TRUE, echo = FLASE}
summary(gam_mod3)
```

There is a lot to look at here but essentially the parametric coefficients explain our linear terms, in our case the additive term Treatment. But the Approximate significance of smooth terms is what we are really interested. edf is the effective degrees of freedom with 1 = straight line and the higher the number the more wiggly the smooth function is ref.df and f are test statistics used in anova but these are only approximate and it is best to check visually for significance. Finally our p value is showing statistical significance of each term, however this is approximate only and it is recomended to a) visually check this and b) compare several models via AIC values to establish the significance of variables


There are four main scenarios we would like to assess with our significance testing
1. The treatment has no significant effect on the dependent variable
2. The Treatment significantly effects the time at which changes in the dependent variable occur
3. The treatment has an effect on the absolute values of the dependent variable
4. The treatment has a significant effect on the timing and absolute values of the dependent variable 

![OAEPIIP_example](https://github.com/OAEPIIP/OAEPIIP-Statistics-example/assets/113956826/2b85b854-e4f1-4339-a1a0-fa69fe1fd635)
adapted from Ferderer et al. (2022)

In order to assess these we need to vary how the independent variable is entered into our model this was discussed above but we will go through it again here;

1. gam_1 assumes Treatment is not significant and therefore we don't need to include it in the model
2. gam_2 assumes the Treatment has a significant influence on the timing of the dependent variable, thus we let Day vary by Treatment
3. gam_3 assumes the Treatment has a significant influence on the absolute value of the dependent variable, thus we add treatment as an additive variable
4. gam_4 assumes the Treatment has a significant influence on the absolute value and timing of the dependent variable, thus we add treatment as an additive variable and let day vary by treatment

```{r, eval=TRUE, echo = FLASE}
gam_1 <- gam((Y) ~ s(Day, k =12) + s(Day,Microcosm, bs = "fs"),
                family = gaussian (), method = "REML", data = data)
par(mfrow = c(1, 1))
plot(gam_1, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", 
     pages = 1, all.terms = TRUE, seWithMean = TRUE)
plot_smooth(gam_1, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
# you will notice an error here saying that the two smooth terms are essentially repeating the same information or are highly correlated. this is okay as we know this is likely the case

gam_2 <- gam((Y) ~ s(Day, by = Treatment, k =12) + s(Day,Microcosm, bs = "fs"),
             family = gaussian (), method = "REML", data = data)
plot(gam_2, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", 
     pages = 1, all.terms = TRUE, seWithMean = TRUE)
plot_smooth(gam_2, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")

gam_3 <- gam((Y) ~ s(Day, k =12) + s(Day,Microcosm, bs = "fs") + Treatment,
             family = gaussian (), method = "REML", data = data)
plot(gam_3, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", 
     pages = 1, all.terms = TRUE, seWithMean = TRUE)
plot_smooth(gam_3, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")

gam_4 <- gam((Y) ~ s(Day,by =Treatment, k =12) + s(Day,Microcosm, bs = "fs") + Treatment,
             family = gaussian (), method = "REML", data = data)
plot(gam_4, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue", 
     pages = 1, all.terms = TRUE, seWithMean = TRUE)
plot_smooth(gam_4, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
```

Now we can compare our models using AIC and R square values.
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
model_results[1, ] <- c("gam_1", extract_model_info(gam_1))
model_results[2, ] <- c("gam_2", extract_model_info(gam_2))
model_results[3, ] <- c("gam_3", extract_model_info(gam_3))
model_results[4, ] <- c("gam_4", extract_model_info(gam_4))

# Print the table
print(model_results)
```

You will notice that gam_2 has the lowest AIC value but has the same R squared value as gam_4. Furthermore the difference in AIC values is < 2 which is a general threshold for determining significant differences between models. Thus you could safely pick either model 2 or 4 in this case, but first we should inspect the plots
```{r, eval=TRUE, echo = FLASE}
par(mfrow = c(1, 1))
plot_smooth(gam_2, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
plot_smooth(gam_4, view="Day", plot_all="Treatment", rm.ranef=F, xlab = "Day", ylab = "Y")
ggplot(data = avg_Y, aes(x = Day, y = Y, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Y by Treatment",
       x = "Day",
       y = "Average Y")
```

There is a limitation to our model comparison which becomes aparent when visualising the data. You will notice that gam_2 underestimates the difference in the slope of the relationship between Y and Days, particularly for the equilibrated treatment. This data is an example of dissolved inorganic nutrient data thus we would expect there to be no difference in y (the amounts/concentrations) between treatments. When we comapre the start and end values given for "y" this is true. However our model comparison shows that although this is true gam_4 provides a better fit to the actual data. This is because in gam_2 the exclusion of treatment as an additive effect forces the treatments to have identical absolute values over the experimental treatment which alters the fit of the smoother, in particular the start and end value, contorting our data. Therefore under this scenario we should go with gam_4. It is important to note that this is not always the case e.g. Chla can vary by absolute values between treatments as to can abundance etc. However if you have a good understanding of the parameter and follow this tutorial you should be able to appropriately select the best model and infer significance from this and visual inspection of the model.


