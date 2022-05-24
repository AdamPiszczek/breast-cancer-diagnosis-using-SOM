## Authors
- [Kacper Kilianek](https://github.com/Kkilianek)
- [Adam Piszczek](https://github.com/AdamPiszczek)

## Shiny
- [ ] shiny app preview

## How to Run

```sh
> shiny::runApp('location/to/folder/of/downloaded/repo)
```

## Setup

```sh
> install.packages(dplyr)
> install.packages(ggplot2)
> install.packages(kohonen)
> install.packages(shiny)
```

## Dependiencies
- [dplyr](https://dplyr.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [kohonen](https://cran.r-project.org/web/packages/kohonen/index.html)
- [shiny](https://shiny.rstudio.com/)

## Design

The application allows selection of network parameters and visualization of the results obtained after the learning process. The user is able to choose parameters such as: network topology, SOM mesh size, neighborhood size.

## About

Breast cancer diagnosis in mammography using the SOM network based on the [Mammographic Mass_MLR dataset](http://archive.ics.uci.edu/ml/datasets/mammographic+mass).
