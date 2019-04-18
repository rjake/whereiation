# datascanR

The purpose of these tools is a pre-analysis screen of your data. Before digging into your analysis and guessing which fields you should look at, these charts are designed to help you understand the fields of your data. There are 2 main plots:
* `field_scan()`: this looks at the frequency of all categorical and numeric data. Numeric data is cut into a specified number of quantiles

![field_scan](https://github.com/rjake/images/mpg_field_scan.png)

* `variation_plot()`: this looks at the variation in the dependent variable across all factors in a field. 

![variation_plot](https://github.com/rjake/images/mpg_variation_plot.png)

It can be read like this

![variation_plot_reading](https://github.com/rjake/images/mpg_variation_plot_reading.png)


An example of the differences can be called using `variation_plot_single_obs()`

![variation_plot_single_obs](https://github.com/rjake/images/mpg_variation_plot_single_obs.png)

## Running the script
Here is how to you can run it on your own. The script will create objects in your environment and so it works best with an empty environment.
There are just 3 variables to set (example with mpg from `ggplot2`):

This is then followed by the following 4 lines of code. For this to run, **it is best to clone the repo and use the `.Rproj` file for relative paths**

```r
library(tidyverse)

df <- mpg
set_dv <- "cty"
set_ignore <- c("drv", "year")


source("R/run_analysis.R") # this must be run after you set your varaibles above

field_scan(n_cat = 10, n_quantile = 10) 

variation_plot()
variation_plot_single_obs(labels = TRUE)
```

Let's look at each step:

* `df` this can be any data frame, `read_csv()`, another built-in data set, etc.
* `set_dv` this is the dependent varaible. you can also pass it a test such as `"cty > 25"`
* `set_ignore` this is a vector of column names to drop from the analysis, can also be set to blank `""`

And the functions:

* `source("R/run_analysis.R")` this will create new objects in your environment in order to create your plots
* `field_scan()` generates bar chart frequencies. `n_cat` will adjust the # of factors shown for discrete fields, lumping the rest into "other", `n_quantile` will set the # of quantiles for numeric data
* `variation_plot()` this will create that colorful chart above
* `variation_plot_single_obs()` this will show a demo of an observation. a vertical line will be shown where the dependent variable is for that observation. The line will then link to all of the factors/identies to which it belongs

You can run a demo file [here](https://github.com/rjake/datascanr/blob/master/R/demo_analysis.Rmd). At the bottom are a lot of `df <-...; set_dv <-...; set_ignore...;` for examples that can be run within R. You will have to install some of the packages in order to use them. For example, the `AER::Fatalities` data set is `Fatalities` out of the `AER` package. To see what is on your machine, you can use the top of [this script](https://github.com/rjake/datascanr/blob/master/R/find_datasets.R).

please reach out with any questions

rjake@sas.upenn.edu

twitter: [@yake_84](https://twitter.com/yake_84)