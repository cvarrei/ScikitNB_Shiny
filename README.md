# ScikitNB_Shiny

The RShiny application is available on: [https://scikitnb-sise.shinyapps.io/Scikit_Naive_Bayes/](https://scikitnb-sise.shinyapps.io/Scikit_Naive_Bayes/).It can be run locally using the R file 'r-shiny.R'. The package is available [here](https://github.com/cvarrei/ScikitNB). 

We deployed an application based on the ScikitNB R package we created. Our three Naïve Bayes Model are available for the user. 

## Welcome Page

The application's first page is a Welcome page, featuring a brief description of the package and a list of its authors.

<img src="https://github.com/cvarrei/ScikitNB_Shiny/blob/main/images/readme_package/capture1.PNG" width="950" height="300">

## Import Data

The second page enables users to import their CSV (only with ",") or XLSX files into the application. Please note, RShiny does not support files larger than 5 MB. Additionally, the dataset should not contain any missing data. 

<img src="https://github.com/cvarrei/ScikitNB_Shiny/blob/main/images/readme_package/Capture2.PNG" width="950" height="400">

Once the dataset is uploaded, you must select the target feature (the variable you wish to predict). The application also allows you to choose any numerical columns that you want to convert into categories, as indicated by the option 'Select Categorical Columns of Integers'.

## Parameter Model

The third page is for configuring the model. First, you need to specify whether the file contains text data or not. Then, select the model you want to use. Upon selection, parameters specific to the chosen model will be displayed.

Afterwards, you should 'Instantiate the model!'. The next step involves splitting the dataset into training and testing subsets. You will need to decide the proportion of the test dataset, and then click on 'Split the dataset!'. Finally, conclude the process on this page by clicking 'Start the processing!'.

<img src="https://github.com/cvarrei/ScikitNB_Shiny/blob/main/images/readme_package/Capture3.PNG" width="950" height="400">


## Training Performance

The fourth page displays the model's performance on the training dataset, including a confusion matrix and a summary of the results. The summary is specific to the model chosen.

<img src="https://github.com/cvarrei/ScikitNB_Shiny/blob/main/images/readme_package/Capture4b.PNG" width="950" height="400">

## Test Scores
Finally, the fifth page presents the results on the test dataset, featuring the score (including the confusion matrix, error rate, recall, and precision), a plotted confusion matrix, and the ROC curve.
<img src="https://github.com/cvarrei/ScikitNB_Shiny/blob/main/images/readme_package/Capture5.PNG" width="950" height="400">
