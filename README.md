# kirkegaard
This is my personal R package. The primary purpose of this package is to be a transportable collection of code that I often use. As such, it may change without much warning and break your code. Generally, it doesn't because this would break my code too! :) When it does, I usually leave a placeholder function behind so that one knows what the new function name is.

There are functions for areas including:

* `df_`: Common operations on data.frames, including merging data.frames with overlapping data, standardizing, adding missing data, residualizing and class conversion. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/dataframe.html).
* `MAT_`: Elementary matrix operations such as getting a half and restoring a symmetrical matrix from a half.
* `fa_`: Factor analysis, including new methods for checking for method variance, finding odd cases, using Jensen's method and plotting loadings.
* `GG_`: Convenience functions for plotting with ggplot2. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/ggplot2.html).
* `pu_`: A function that deals with names of political units by converting to and from standardized abbreviations. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/political_units_names.html).
* Various other functions that I often use.

Some old and deprecated spatial data functions were split off to **spatialstatstools** package (which is not updated). The newer ones are much faster.

## Installing from scratch
for whatever reason, it is sometimes hard to get it to install the dependencies automatically
so here's a small call to manually install them. This installs all dependencies, including those rarely used.

```
#CRAN packages
#determine which packages we don't have but need
pkgs_we_need = c("tidyverse", "assertthat", "plyr", "psych", "gtools", "polycor", "weights", "devtools", "VIM", "lsr", "compute.es", "Hmisc", "stringdist", "glmnet", "metafor", "binom", "GPArotation", "ggrepel", "qgam", "caTools", "furrr", "terra", "rms", "missForest", "psychTools")
install.packages(setdiff(pkgs_we_need, installed.packages()[, 1]))

#finally
devtools::install_github("deleetdk/kirkegaard")
```
