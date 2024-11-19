# Sample Size Calculator

Using whole populations for a study is not practical nor feasible. For this reason, when performing a clinical study, a 'sample' is selected from the population and, from the obtained results, we  make inferences about the population. The sample size, or number of participants, has to be defined before the start of a clinical study to avoid bias during the interpretation of the results. 

- If we include very few subjects, the sample won't truly represent the population and it may not be possible to detect the difference between study groups.
- If we include too many subjects, more individuals are exposed to the risk of the intervention and unnecessary resources will be wasted.
  
Therefore, the correct calculation of the sample size is crucial in any clinical study. Aims to calculate the optimum number of participants required to be able to achieve a scientifically valid result. It generally depends on the:

- Level of significance
- Power of the study
- Expected effect size
- Event rate in the population
- Standard deviation in the population

## About the Sample Size Calculator App

This R Shiny mini-app aids you with study design by taking the results of your pilot study and showing the required number of participants per group to detect the observed difference between group with the desired power, as well as helping to acquire a deeper understanding of sample size calculations by interactive visualization.

Moreover, you can calculate sample sizes for clustered studies and studies with binary dependent variables.

### Checkout and run

You can clone this repository by using the command:

```{bash}
git clone https://github.com/Aridhia-Open-Source/shiny-demo-sample-size-calculator
```

Open the .Rproj file in RStudio and use `runApp()` to run the app.

### Deploying to the workspace

1. Download this GitHub repo as a .zip file.
2. Create a new blank Shiny app in your workspace called "sample-size-calculator".
3. Navigate to the `sample-size-calculator` folder under "files".
4. Delete the `app.R` file from the `sample-size-calculator` folder. Make sure you keep the `.version` file!
5. Upload the .zip file to the `sample-size-calculator` folder.
6. Extract the .zip file. Make sure "Folder name" is blank and "Remove compressed file after extracting" is ticked.
7. Navigate into the unzipped folder.
8. Select all content of the unzipped folder, and move it to the `sample-size-calculator` folder (so, one level up).
9. Delete the now empty unzipped folder.
10. Start the R console and run the `dependencies.R` script to install all R packages that the app requires.
11. Run the app in your workspace.

For more information visit https://knowledgebase.aridhia.io/article/how-to-upload-your-mini-app/
