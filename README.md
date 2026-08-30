# MassWateRdash

Materials for a MassWateR Shiny dashboard

## Using the Dashboard

There are three ways to run the dashboard, depending on whether you want to develop the app locally, install it as a package, or access an already-deployed instance.

### Local development (git clone + devtools)

Clone the repo and run the app from source with [devtools](https://devtools.r-lib.org/):

```r
# clone the repo, then from within the MassWateRdash directory:
devtools::load_all()
shiny::runApp()
```

This is the workflow to use if you're making changes to the app itself.

### Package install (remotes::install_github)

To use the dashboard without cloning the repo, install it directly as a package:

```r
remotes::install_github("massbays-tech/MassWateRdash")
library(MassWateRdash)
run_app()
```

### Docker

A pre-built Docker image is also available for server deployment. See [`docker/DEPLOY.md`](docker/DEPLOY.md) for full build and deployment instructions. Once running, the app is available at:

```
http://<server-ip>:3838/
```
