GAM example

Now that we understand why GAMs are such a useful tool in our statistical toolbelt lets have a look at a more relevant dataset.
First we need to load the mgcv package.

```{r, eval=TRUE,echo = FALSE}
library(mgcv)
```
If you haven't already you will also need to download the csv file from the repository "GAM_example.csv".
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

Similar to before we will first fit a simple linear model and inspect a plot of this overlaid on the actual data
```{r, eval=TRUE, echo = FLASE}
lm_mod = lm(concentration ~ Day, data = data)
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
```

No surprises here, our linear model does a poor job of explaining the evolution of the 
