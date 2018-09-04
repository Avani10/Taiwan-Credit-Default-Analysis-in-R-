# Taiwan-Credit-Default-Analysis (in R)

### Introduction:  (Check "Team 14_Project Presentation -  Taiwan Credit Defaults v1.0.pdf" for summarised results)

The Taiwanese economy experienced tremendous growth during the 1990’s, almost doubling in value along with the other countries known as the Asian Tigers. The country’s financial sector was heavily involved in the growth of real estate during this period. However, in the early 2000’s, this growth slowed and banks in Taiwan turned towards consumer lending to continue the expansion. As a result, credit requirements were loosened and consumers were encouraged to spend by borrowing capital. 
 
We will be analyzing data on Taiwanese credit card holders from mid-2005, as the flood of debt was reaching its peak. The following algorithms will be tested for accuracy in predicting if an individual will miss their next payment (predictor variable). 
* Logistic Regression
* Random forest  
* Naïve Bayes  
 
The banking data is skewed with only 22.1% defaulters which could potentially lead to biased predictions. Thus, we compared the models in terms of their specificity and sensitivity - the proportion of correct positive predictions (Non-defaulters) and proportion of correct negative predictions (Defaulters). Each method was compared on the basis of Receiver Operating Characteristic curve. 
 
