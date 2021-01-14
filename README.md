# Sample Size Calculator

Using the whoel population for a study is not practical nor feasible, for this reason a 'sample' is selected from the population so we can make inferences about the population from the obtained results. The sample size, or number of participants, has to be defined before the start of a clinical study to avoid bias during the interpretation of the results. 

- If we include very few subjects, the sample won't truly represent the population and it may not be possible to detect the difference between study groups.
- If we include too many subjects, more individuals are exposed to the risk of the intervention and unnecessary resources will be wasted.
  
Therefore, the correct calculation of the sample size is crucial in any clinical study. Aims to calculate the optiomum number of participants required to be able to achieve a scientifically valid reslt. It generally depends on the:

- Level of significace
- Power of the study
- Expected effect size
- Event rate in the population
- Standard deviation in the population

## About the Sample Size Calculator App

This R Shiny mini-app aids you with study design by taking the results of your pilot study and showing the required number of participants per group to detect the observed difference between group with the desired power, as well as helping to adquire a deeper understanding of sample size calculations by interactive visualization.

Moreover, you can calculate sample sizes for clustered studies and studies with binary dependen variables.

### Checkout and run

You can clone this repository by using the command:

```{bash}
git clone https://github.com/aridhia/demo-sample-size-calculator
```

Open the .Rproj file in RStudio and use `runApp()` to run the app.

### Deploying to the workspace

1. Create a new mini-app in the workspace called "sample-size-calculator" and delete the folder created for it
2. Download this GitHub repository as a ZIP file, or zip all the files in you computer
3. Upload the ZIP file to the workspace and upzip it inside a folder called "sample-size-calculator"
4. Run the app in your workspace