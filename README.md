# Screening-Tool-Chronic-Kidney-Disease

## ABSTRACT  

Chronic kidney disease (CKD) is a type of kidney disease in which there is gradual loss of kidney function over a period of months to years. Initially there are generally no symptoms; later, symptoms may include leg swelling, feeling tired, vomiting, loss of appetite, and confusion. Complications include an increased risk of heart disease, high blood pressure, bone disease, and anemia. CKD can affect almost every body system. Early recognition and intervention are essential to slowing disease progression, maintaining quality of life, and improving outcomes. 

 

Our study implements logistic regression and develops a model to identify whether a person has CKD. We cited various research papers and consulted various Doctors and implemented logistic regression to measure the model’s accuracy. We conducted (a) Correlation Analysis, (b) Lasso Regression and Scientific citation for feature Selection. Logistic regression gave a training accuracy of 80.3 % with a validation accuracy of 79.46%. The study also consists of a simple screening tool which most likely indicates the presence of CKD. This study concludes by using predictive model and the screening tool to predict the risk of CKD in 2819 people.  

 

## METHODOLOGY  

DATA DESCRPTION: The dataset for the case study consists of responses for specifically designed questionnaire from 8819 individuals, aged 20 years or older taken between 1999-2000 and 2001-2002 during a survey across various states in USA.  The dataset is divided into two sets 1. Training set with 6000 observations in which 33 variables along with the status of CKD is provided. 2. Testing set consisting of 2819 observations with same set of variables in which the CKD has to be predicted. Table1 has all the 33 variables given in our dataset. 

## DATA DESCRIPTION

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture1.png">


## MISSING DATA 

Missing data can occur because of nonresponse: no information is provided for one or more items or for a whole unit ("subject"). Some items are more likely to generate a nonresponse than others: for example, items about private subjects such as income. 

Our dataset consists of 8819 responses against 33 attributes (8819 x 33) 291027 individual responses are to be recorded. But only 283285 are recorded and 7742 records are missing (which is about 2.6 % of the data set). Four dummy variables have been created for Race group (Black, White, Hispanic and others). 

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture2.png">

The above picture shows the number of missing values in each of the variables. Further the missing values are analysed to observe any patterns or combinations occur within the data as shown below.
 
<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture3.png">


## IMPUTATION 

Missing data reduces the representativeness of the sample and can therefore distort inferences about the population. The choice of method to impute missing values, largely influences the model’s predictive ability. If the data is missing completely at random then deletion does not add any bias, but it might decrease the power of the analysis by decreasing the sample size. To deal with the missing data here, MICE package has been used with mean imputation so that the overall mean will not be affected.

## VARIABLE SELECTION  

Attribute selection methods are used to identify and remove unneeded, irrelevant and redundant attributes from data that do not contribute to the accuracy of a predictive model or may in fact decrease the accuracy of the model. We have used correlation analysis and cited many research papers online and eliminated following variables: Income, Unmarried, CareSource, Insured, Education, Height, Weight, LDL, Total Cholesterol for the initial selection. Then to further filter out the insignificant variables we have used several approaches.  

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture4.png">

### ANALYTICAL APPROACH 

We used Lasso regression for feature selection in remaining 24 variables. Based on the lasso model, following 13 variables have higher significance: Age, DBP, HDL, PVD, Activity, Hypertension, Diabetes, Stroke, CVD, Anemia, Racegroup Hispanic.  

### SCIENTIFIC APPROACH  

Considering real life scenario, we cross validated the variables with Nephrologist and modified the above variables and finalized the following 13 variables for our predictive model:  Age, Female, BMI, Dyslipidemia, PVD, Hypertension, Diabetes, Family Diabetes, Stroke, Family CVD, CHF, Anemia and Race Group (Black, White, Hispanic and others). 

### CRITERION BASED APPROACH  

The Akaike Information Criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data. Given a collection of models for the data, AIC estimates the quality of each model, relative to each of the other models. We want to minimize AIC. Larger models will fit better and so have smaller RSS but use more parameters. Thus, the best choice of model will balance fit with model size.  

Different Logit models were run on the different combination of variable selected. We used AIC as an estimator on the all the models and picked out the best one with the lowest AIC value.   

## PREDICTION MODEL 

This logit model gave an AIC value of 1478.8. Then to train and test the model we split the 6000 training set data into three sets 1) Main Training Set (4000) 2) Testing (1000) Set 3) Validation Set (1000).  

### FRAMEWORK OF THE APPROACH

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture5.png">


### ACCURACY OF THE MODEL 

After training the model, we tested it with the Testing set consisting of 1000 responses to check the accuracy of the model and select the threshold for prediction. We generated a for loop on this set to predict CKD for this testing set for various thresholds and compared with the actual CKD values to get confusion matrix, accuracies and corresponding costs.  

### THRESHOLD SELECTION  

We can convert the probabilities to predictions using what’s called a threshold value, t. If the probability of CKD is greater than this threshold value, t, we predict that person has CKD. But if the probability of CKD is less than the threshold value, t, then we predict that the person does not have CKD. 

How to select the value for t: The threshold value, t, is often selected based on which errors are better. This would imply that t would be best for no errors but it’s rare to have a model that predicts perfectly. In this model we have selected based on money as well. For taking test it takes 100 dollars for FP and TP we would lose 100$ and for TP the hospital will gain 1000$.  

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture6.jpg">

**AUC VALUE:** We selected the threshold based on the ROC curve. AUC - ROC curve is a performance measurement for classification problem at various thresholds settings. ROC is a probability curve and AUC represent degree or measure of separability. It tells how much model is capable of distinguishing between classes. Higher the AUC, better the model is at predicting 0s as 0s and 1s as 1s. By analogy, higher the AUC, better the model is at distinguishing between patients with CKD and without CKD. The ROC curve is plotted with TPR against the FPR where TPR is on y-axis and FPR is on the x-axis. The AUC value for the above ROC curve 87%. 

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture7.png">

Our main aim was to reduce the FP(False Positive), so we went with a threshold which gave us less FP at the same time more profit. That threshold came out to be 0.07 with an accuracy of 77.2%. 

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture8.jpg">

For further validation this logit model was validated with the validation set of 1000. The model gave out an accuracy of 78.2%.  

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture9.png">

With the above threshold we got a cost of $74,200. Running the model on the Validation set further confirmed our model’s accuracy and AUC were almost similar for both the Testing and Validation sets. So, we went with the threshold of 0.07 and finally ran the model to predict whether a person has CKD on the 2819 Prediction Set.  

## LIMITATIONS OF THE MODEL 

- The data set doesn’t talk about the severity of CKD (Symptoms like CVD develop only at the final stage. So, if a person who is prone to CKD might not have CVD at the initial stage). 

- Data about main causes of CKD are missing (Protein Urea, IHD, eGFR). Two simple tests can detect CKD urine, albumin and serum, creatinine. We don’t have these in the data set.

- The variables selected might not be the best combination as more research work is needed on each variable.  

- Under-representation of certain race may lead bias in the prediction.  

- Imbalance in the data set -We have less people with CKD. This will lead to bias again.  

- The data set is focused only for people of US state and during a certain time period. So, putting a general Prediction model is very tough.

- The model was focused on reducing FP and maximizing the profit but the most dangerous one is FN (False Negative).  

## SCREENING TOOL

<img src = "https://github.com/pinkesh-nayak/Screening_Tool_Chronic_Kidney_Disease/blob/master/Images/Picture10.png">


## CONCLUSION  

The model can accurately identify patients receiving low-quality care with test set accuracy being equal to 78 % with 13 attributes. In practice, the probabilities returned by the logistic regression model can be used to prioritize patients for intervention.  Any individual’s risk can be estimated as the probability of that individual with the questions used in the simple screening tool. Further research is needed to simultaneously assess the role of multiple risk factors which were not provided in the case study (as mentioned in the limitation section) to validate this model in other population. 

## REFERENCES 

1. Chronic Kidney Disease: Early Education Intervention by Judy Kauffman, MSN, RN, CNN Charlottesville, Virginia ( A DNP Scholarly Project presented to the Graduate Faculty of the University of Virginia in Candidacy for the Degree of Doctor of Nursing Practice School of Nursing University of Virginia May 2017). 

2. Detection of Chronic Kidney Disease and Selecting Important Predictive Attributes by Asif Salekin and John Stankovic (Department of Computer Science University of Virginia Charlottesville, Virginia). 

3. An Introduction to Statistical Learning with Applications in R by Gareth James and Daniela Witten. 

4. Centre for Disease Control and Prevention ( https://www.cdc.gov/kidneydisease/publications-resources/2019-national-facts.html ) 

5. African Health Sciences https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4915439/) 

 
