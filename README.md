### iNaturalist-PlantNet Workflow: Step-by-Step Guide and Troubleshooting
Nadja Pernat, 2024-06-06


##### Introduction

This very brief step-by-step manual explains how to use the automatic data exchange workflow between iNaturalist and PlantNet. When using this workflow with *Isodontia mexicana*, results may vary slightly due to changes on iNaturalist. Observations made within the investigated time period might have been added, removed, had their license changed, or lost their research-grade status. Note that PlantNet may also perform differently over time as its algorithm is continuously trained with new images, potentially resulting in different species suggestions or scores. To reproduce our results, we also provided our data as excel sheets.


##### Step 1: Running the adapted `get_rinat_obs()`

The original `get_rinat_obs()` command retrieves observations for only one year. We adapted the function slightly so that it can loop over multiple years without stopping. Please run this code first ("get_inat_obs_nostop").


##### Step 2: iNaturalist-PlantNet workflow  

Run the `get_rinat_obs_nostop()` function with *Isodontia mexicana* or any other `taxon_name` from the data preparation script. The looping over years creates a list that needs to be converted into a dataframe. You can also select the variables you want to keep.
To facilitate testing and reproducing results without retrieving observations from iNaturalist again, we have provided our data table after downloading the image URLs ("isomex_experts.xlsx" - we called it experts, because it was also used by the experts to identify the plant species on the given images). 

To access the PlantNet API via the `PlantNet` package, you need an API key, which you can obtain at [PlantNet Usage](https://my.PlantNet.org/usage). Without a key, it is not possible to run the code. We also adapted the `identify()` command to loop over all image URLs. The result is a list of lists that needs to be converted into a dataframe. At this step, it is advisable to write the dataframe as a backup.

 
 
##### Step 3: Preparing the data

Actually, you are now done to let PlantNet identify the plants on any citizens science image. If you want to work with our data, use the iso_new.xls table. However, you might have generated a huge dataset including all plant species candidate suggestions. There can be over 30 suggestions for one image. In our study, at this point we then filtered the dataframe for only the FIRST (best) plant species candidate suggestion, retaining only the plant suggestion with the highest score for each iNaturalist image. 

As a last step you would need to create genus and family names in case you plan to work on these taxonomic levels. PlantNet only provides species names, so we needed to extract the genus name using the `stringr` package's `word()` function and the family names using the `plantminer()` function from the `taxize` package. If `plantminer()` does not smoothly identify the families, check the Latin names of the species provided by PlantNet. Sometimes, the name of hybrids or import errors in the CSV table create unusual letter combinations. These errors are typically exceptions and not very frequent; we recommend manually substituting these letters using `gsub()` or `stringr::str_replace()`.

 
##### Step 4: Do your own research or reproduce our results 

This completes the iNaturalist-PlantNet workflow. Further code describes how we merged the data with the expert dataset, cleaned it, analyzed the matches, and produced the plots and tables. The code is commented, but if you encounter any issues, please do not hesitate to contact the corresponding author.

