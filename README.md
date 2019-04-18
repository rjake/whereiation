# datascanr

The purpose of this code is to run a pre-analysis screen of your data. Before guessing which fields you should look at, these charts are designed to help you understand the fields of your data. The script takes just 3 user inputs and returns 2 main plots:
* `field_scan()`: this looks at the frequency of all categorical and numeric data. Numeric data is cut into a specified number of quantiles
<img src="images/mpg_field_scan.png" alt="field_scan" width="500"/>

* `variation_plot()`: this looks at the variation in the dependent variable across all factors in a field. 

<img src="images/mpg_variation_plot.png" alt="mpg_variation_plot" width="500"/>



The variation plot is likely to be unfamiliar. This image should help explain what's going on (click to see a bigger pic):

<img src="images/mpg_variation_plot_reading.png" alt="mpg_variation_plot_reading" width="500"/>


Sometimes it's easier to see an example of where one observation may fall. For this you can use `variation_plot_single_obs()`

<img src="images/mpg_variation_plot_single_obs.png" alt="mpg_variation_plot_single_obs" width="500"/>

The idea is that categories matter. I like to think of it as a tug-of-war between different identities a single observation holds. In te example above,  being a dodge linked it to having a lower city mpg as the group mean for `manufacturer == "dodge"` has a group mean well below the grand mean (across all manufacturers/observations). The 8 cyl (V8) engine has a similar contribution. As you work your way from highest to lowest, the expectation is that the impact of these different factors have less and less pull.

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

Please reach out with any questions rjake@sas.upenn.edu, [@yake_84](https://twitter.com/yake_84)
