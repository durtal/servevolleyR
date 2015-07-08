servevolleyR 0.0.1
==================

### Overview

Collection of functions to simulate a tennis point, game, set, tiebreak and match.  

### Installation

```R
devtools::install_github("durtal/servevolleyR")
```

### Help

```R
??servevolleyR
```

Help pages are available [here](http://durtal.github.io/servevolleyR/), vignettes will be added in time

### Shiny

I have added a shiny app, but this being my first shiny app I think it might be a bit buggy, so it's not included in the master branch, if you want to install the package with the shiny app:

```R
devtools::install_github("durtal/servevolleyR", ref = "shiny")
```

To run the shiny app:

```R
library(servevolleyR)
svRshiny()
```

The shiny app allows users to simulate 1000 service games for two players, 500 sets and 500 matches, a screenshot of the app is shown below:

![](shinyApp.jpg)


### Other

Comments/suggestions to improve code, and speed up some of the functions would be welcomed, if possible turning `plyr` code that uses `rlply` and `ldply` into speedier `dplyr` code.
