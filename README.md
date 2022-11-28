
# nordicscir

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/nordicscir?sort=semver)](https://github.com/rapporteket/nordicscir/releases)
[![R build status](https://github.com/rapporteket/nordicscir/workflows/R-CMD-check/badge.svg)](https://github.com/rapporteket/nordicscir/actions)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/nordicscir/branch/master/graph/badge.svg)](https://codecov.io/gh/Rapporteket/nordicscir?branch=master)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub open issues](https://img.shields.io/github/issues/rapporteket/nordicscir.svg)](https://github.com/rapporteket/nordicscir/issues)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/nordicscir/)
<!-- badges: end -->


## Installation

You can install the released version of nordicscir from [GitHub](https://github.com/Rapporteket/nordicscir) with:

```r
remotes::install_github("Rapporteket/nordicscir@*release")
```
The latest development version can be installed with
```r
remotes::install_github("Rapporteket/nordicscir")
```

## Usage
Start the shiny application from the R console:
```r
command_tbd()
```

## Issues
Please provide any comments (_e.g._ on proposed enhancements, shortcomings, errors) through the [issue tracker](https://github.com/Rapporteket/nordicscir/issues).


## Develop
Contributors submit their code by branching from the _master_ branch and issuing a pull request. After acceptance by peer review the pull request may be merged to the master branch. Changes that are accepted in TEST and/or QA environments may be tagged as a new release of the package.

A development environment is provided as a _docker-compose.yml_ file found in the root of this repository. The container can be run from a system command prompt, _e.g._
```bash
docker-compose up
```
and the development environment will then be served by localhost through a web browser. By default, RStudio will accessible at [localhost:8787](http://localhost:8787), the database server at [localhost:8888](http://localhost:8888) and Shiny-Server at [localhost:3838](http://localhost:3838).
