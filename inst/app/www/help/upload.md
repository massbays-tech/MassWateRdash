This tab is used to upload and format data used for the rest of the dashboard. Five types of data (six files) are used with the MassWateR package.  See the MassWateR <a href="https://massbays-tech.github.io/MassWateR/articles/inputs.html" target="_blank">inputs vignette</a> for detailed information on these files.

1. Water quality **results** organized by sample location and date.
2. Summary of data quality objectives that describe quality control **accuracy**, **frequency**, and **completeness** measures for data in the results file. These are separate files, one for accuracy and frequency and another for completeness.
3. A **site metadata** file, including location names, latitude, longitude, and additional grouping factors for sites.
4. A **WQX metadata** file required for generating output to facilitate data upload to WQX. 
5. Optional information on the number of **censored** or missing observations by parameter, used only in the quality control report. 

The dashboard can be run in **test mode** by flipping the switch in the top left.  This loads pre-existing files to use with the package. 

Choosing the option to **convert from another format** opens up a box to convert existing data into the format required by MassWateR.  This option makes use of the <a href="https://github.com/massbays-tech/wqformat" target="_blank">wqformat</a> package. 

Uploading data files will run the standard suite of checks used by MassWateR that ensure the data are the correct format.  Please see the <a href="https://massbays-tech.github.io/MassWateR/articles/inputs.html" target="_blank">inputs vignette</a> for more information on these formats and the checks that are applied. Templates are available on the package's <a href="https://massbays-tech.github.io/MassWateR/RESOURCES.html" target="_blank">resources</a> page. An interactive popup will appear if the data require correction.  Follow the on-screen prompts to correct the data, then click "Try upload again" to load the corrected data from within the app.

Input data can be downloaded in a zipped folder once uploaded by clicking the **Download data** button.  The button is only visible after data are uploaded.