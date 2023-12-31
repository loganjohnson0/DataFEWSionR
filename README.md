
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataFEWSionR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Who are we?

We are an interdisciplinary group of graduate students from Iowa State
University. Part of our mission is to form collaborative environments to
engage in cross-cutting research projects.

We have developed a shiny application using a variety of data relevant
to Iowans broadly and incorporated all of the data in a user friendly
interface. We created this as we felt there was a need to have better
access to these data and in a way that is clear for the user.

Iowa has approximately \_\_\_\_\_ of people, and they are dispursed
throughout the state, with heavy population centers in Des Moines, Cedar
Rapids, and Iowa City. The remaining population exists in rural areas
where a significant amount of the agricultural crops are grown.

As climate change makes weather events more uncertain and the severity
of droughts and extreme weather events forecast to increase in the near
and long term, we incorporated the most recent climate change models
with the perception of individuals on climate change. This shows that
the regions that are most likely to be impacted by climate change are
those in rural areas.

## How can you use our Shiny Application?

First, you will need to install R and RStudio. See the links to download
R and RStudio, respectively.

You will then need a few packages install and loaded into RStudio.

``` r
library(devtools)
library(leaflet)
library(shiny)
library(sf)
library(tidyverse)
```

Now you are ready to download our package, `DataFEWSionR`.

``` r
devtools::install_github("loganjohnson0/DataFEWSionR")

library(DataFEWSionR)
```

Now if there are no errors or warnings, you should be ready to go! <b>
GREAT! </b>

To explore our data, simply run the following code:

``` r
DataFEWSionR::runExample()
```

This will launch our application and bring up our interface.
