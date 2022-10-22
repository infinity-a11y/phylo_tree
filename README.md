# PhyloTree ***Still in work***
  **Version A.1.2**

  ![PhyloTree](https://user-images.githubusercontent.com/115372379/194724253-723baff7-d091-45d3-acdc-8330f91e428b.jpg)

  Shiny-Interface for inferring a customizable phylogenetic tree from either an integrated NGS pipeline (not available yet) or other output formats (BEAST, MrBayes,     etc.).
  Generate a customizable report containing the generated tree plot and several additional parameters. 


# 1 How to run PhyloTree
  
  Download *phylo_tree_github.zip* and extract it to any location on the computer (*important: unzip the zip file before starting the app!*). 
  Open *PhyloTree___.R*.
  To start the app from RStudio click the small arrow right next to *Run App* and change the settings as shown below.

  ![alt text](https://github.com/infinity-a11y/phylo_tree/blob/main/readme.png)

  Finally click on *Run App*.
  If some required packages are not installed, the respective downloads will first be executed before the app starts.
  It may be that while installing the neccessary packages you'll need to react to various update messages in the console (depends on what is already installed and       whether basic packages like ggplot2 are frequently updated on your computer).
  If every package is loaded without errors, a tab on the default browser will open.
  The Shiny interface should load almost immediately and is ready to go.


# 2 Upload data
  
  **2.1 Import NGS data**
  
  A pipeline for NGS data is still in work and will be available in the future. 
  
  
  **2.2 Import tree dataset**
  
  There are countless different tree formats that are generated by a transformative software respectively. Unfortunately they are not compatible with each other.
  Therefore researchers are often unable to compare phylogenetic data (just with high effort). The package 'treedataverse' contains a parsing function for all 
  significant tree formats. 
  In order to import tree data got to the tab 'Upload Data' and press the file input button. Choose the respective file and press 'Parsing Data'. Receiving the
  message 'Parsing successfull!' means that the translation to 'treedata' and 'phylo' objects was successfull. Receiving an error message could mean that you
  chose a wrong tree format (crosscheck the format of your dataset again).  
  The folder 'extdata', which is included in the .zip folder, contains examplary datasets in different tree formats that you can use for testing purposes. 
  Alternatively you can skip uploading data and proceed to the tab 'Visualization' where you can generate a random tree with a modifiable number of branches
  (increasing complexity).
  
  
# 3 Visualization
   
  **3.1 Tree formatting**
   
   
  **3.2 Explorative formatting**
   
   
# 4 Download
To generate a report containing the tree plot and several analysis and pipeline parameters, first choose the info to be contained in the report. Select a file format
that fits your purposes best and press the 'Download' button. Choose the directory the files should be saved in and confirm. 
