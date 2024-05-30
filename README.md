# BC_RecurrenceMetastasisML
This repository uses machine learning to predict breast cancer recurrence and metastasis. Models are trained on clinical data and validated with real patient datasets, ensuring accuracy and reliability. 

It contain xz directories, below is the description of each :
1.Notebooks :

   1.a Recurrence(Yes-No) : this directory contains all files related to the first approach that predicts whether there is reccurrence or not.

   It contains the below files:
     1.a.1 Model Weights Pickles : contains the optimum weights of each model.
     1.a.2 Final_Recurrence1_val(nobalance).ipynp : Jupyter notebook containing the code implementatin.

   1.b Recurrence Type (Local-Distance): this directory contains all files related to the second approach related to the recurrence type whether it is local recurrence or distant recurrence.

   It contains the below files:
     1.b.1 SVGs:  This directory contains SVG files related to combined classification metrics and visualizations for location and distance analysis.
     1.b.2 Rec_Type_prediction.ipynp:Jupyter notebook containing the code implementation.

   1.c  Metastasis (Location) : this directory contains all files related to the third approach which predict the location of the metastasis whether t is brain/bone/liver/lung.

   It contains the below files:
     1.c.1 Model Weights Pickles : contains the optimum weights of each model.
     1.c.2 Metastasis_loc.ipynp : Jupyter notebook containing the code implementation.


2.Datasets:
 2.a  Recurrence(Yes-No) : contains merged_data_withImp.csv.
   
3.Preprocessing: this cdirectory contains all details related the preprocessing: the steps, the datasets and the code implementation.
 
 3.a Training Datasets: 
   3.a.1  Breast Cancer METABRIC.xlsx 
   3.a.2 Clinical_and_Other_Features_Final.xlsx
   3.a.3 datasetmsk.csv
   3.a.4 SEER.csv
 3.b Final_general_merged_Preprocessed: contains the above four training datasets merged and preprocessed 
 3.c final-data-preprocessing.ipynp: Jupyter notebook containing the code implementation.