# kirkegaard
This is my personal R package. The primary purpose of this package is to be a transportable collection of code that I often use. As such, it may change without much warning and break your code. Generally, it doesn't because this would break my code too! :)

There are functions for areas including:

* `df_`: Common operations on data.frames, including merging data.frames with overlapping data, standardizing, adding missing data, residualizing and class conversion. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/dataframe.html).
* `MOD_`: Convenience Functions for statistical modeling such as best subset and penalized (LASSO) regression.
* `SAC_`: Spatial statistics, including new methods for measuring and controlling for autocorrelation.
* `MAT_`: Elementary matrix operations such as getting a half and restoring a symmetrical matrix from a half.
* `fa_`: Factor analysis, including new methods for checking for method variance, finding odd cases, using Jensen's method and plotting loadings.
* `GG_`: Convenience functions for plotting with ggplot2. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/ggplot2.html).
* `pu_`: A function that deals with names of political units by converting to and from standardized abbreviations. [Examples](https://cdn.rawgit.com/Deleetdk/kirkegaard/master/knitr/political_units_names.html).
* Various other functions that I often use.

It's probably that when some of these collections get large enough, they will be moved to their own packages.
