# MassWateRdash

Materials for a MassWateR Shiny dashboard

## Using the Dashboard

There are three ways to run the dashboard, depending on whether you want to simply access it from a web browser, use (or develop) the app locally, or install it as a package.

### Web browser

The app is currently deployed at <https://masswater.app>. Simply navigate to the URL to begin using the app. The online version uses a pre-built Docker image. See [`docker/DEPLOY.md`](docker/DEPLOY.md) for details on how the app was deployed.

### Local development (git clone + devtools)

Clone the repo and run the app from source with [devtools](https://devtools.r-lib.org/):

```r
# clone the repo, then from within the MassWateRdash directory:
devtools::load_all()
shiny::runApp()
```

This is the workflow to use if you're running the app within an IDE (e.g., RStudio).

### Package install (remotes::install_github)

To use the dashboard without cloning the repo, install it directly as a package.  This is also done locally within an IDE (e.g., RStudio):

```r
remotes::install_github("massbays-tech/MassWateRdash")
library(MassWateRdash)
run_app()
```

## Workflow

This diagram shows which input files are needed to populate each tab of the dashboard. All files are uploaded on the **Upload & Validate** tab, where they are checked by MassWateR. Each tab appears in the navigation bar once its required files have been uploaded and validated. Files can also be converted from another format before upload, or the example files can be loaded with test mode.

```mermaid
flowchart LR
    files["<b>Input files (.xlsx)</b><br/>Results<br/>DQO Accuracy<br/>DQO Frequency & Completeness<br/>Sites<br/>WQX Meta<br/><i>Censored (optional)</i>"]

    conv(["Convert from another format"]) -.-> files
    test(["Test mode (example files)"]) -.-> upload

    files --> upload["1 Upload & Validate<br/>MassWateR checks each file"]

    upload --> outlier["<b>2 Outlier assessment</b><br/>Results + Accuracy"]
    upload --> qc["<b>3 QC reporting</b><br/>Results + Accuracy +<br/>Frequency & Completeness<br/><i>Censored optional</i>"]
    upload --> wqxtab["<b>4 WQX output</b><br/>Results + Accuracy +<br/>Sites + WQX Meta"]
    upload --> viz["<b>5 Visualize</b><br/>Results + Accuracy + Sites"]

    classDef file fill:#E6F6FB,stroke:#00A4CF,color:#212529
    classDef opt fill:#FFFFFF,stroke:#00A4CF,stroke-dasharray:4 3,color:#212529
    classDef tab fill:#EDF8E8,stroke:#64C147,color:#212529
    class files file
    class conv,test opt
    class upload,outlier,qc,wqxtab,viz tab
```

Each tab box lists the files it needs. Dashed boxes and arrows are optional. The DQO Accuracy file is needed for every tab because MassWateR uses its quantitation limits to fill in results reported below detection or above quantitation. The Sites file provides the site coordinates and location groups used by the WQX output and the Visualize plots and map.
