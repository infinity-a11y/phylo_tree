![PhyloTree](https://user-images.githubusercontent.com/115372379/194724253-723baff7-d091-45d3-acdc-8330f91e428b.jpg)
# PhyloTree Version A.1.2

Shiny-Interface for inferring a customizable phylogenetic tree from either an integrated NGS pipeline (not available yet) or other output formats (BEAST, MrBayes, etc.).
Generate a customizable report containing the generated tree plot and several additional parameters. 


Using the app in the browser window is recommended.
To view the Shiny-Interface in external change following settings in RStudio.

![alt text](https://github.com/infinity-a11y/phylo_tree/blob/main/readme.png)



# 1 Upload data
  
  1.1 Import NGS data
  
  A pipeline for NGS data is still in work and will be available in the future. 
  
  
  1.2 Import tree dataset
  
  There are countless different tree formats that are generated by a transformative software respectively. Unfortunately they are not compatible with each other.
  Therefore researchers are often unable to compare phylogenetic data (just with high effort). The package 'treedataverse' contains a parsing function for all 
  significant tree formats. 
  In order to import tree data got to the tab 'Upload Data' and press the file input button. Choose the respective file and press 'Parsing Data'. Receiving the
  message 'Parsing successfull!' means that the translation to 'treedata' and 'phylo' objects was successfull. Receiving an error message could mean that you
  chose a wrong tree format (crosscheck the format of your dataset again).  
  The folder 'extdata', which is included in the .zip folder, contains examplary datasets in different tree formats that you can use for testing purposes. 
  Alternatively you can skip uploading data and proceed to the tab 'Visualization' where you can generate a random tree with a modifiable number of branches
  (increasing complexity).
  
  
# 2 Visualization
   
  2.1 Tree formatting
   
   
  2.2 Explorative formatting
   
   
# 3 Download
To generate a report containing the tree plot and several analysis and pipeline parameters, first choose the info to be contained in the report. Select a file format
that fits your purposes best and press the 'Download' button. Choose the directory the files should be saved in and confirm. 
