# Loan-Repayment

Introduction to Data

Loans - a loan is the lending of money by one or more individuals, organizations
Credit risk- the borrowers ability to repay the loan with in a certain period
The interest rate is the amount charged by a lender to a borrower for the use of assets.
The dataset is extracted from Kaggel about prediction of Loan Repayment
We predict if the borrower will repay the loan by its mature date or not.


Data has 9578 observations and 14 variables 
Target variable is not.fully.paid (dependent variable)
Plots of variables
Date sets are the combination of categorical and numerical variables

# Data Modelling

Split the data into training set (70%), and test set (30%). Training set will be used to fit the model, and test set will be to evaluate the best model
Built a simple logistic regression model with P value shows significance
Combining good performing models the were trained independently will capture more of the truth than a single model
check the prediction variable by pred function, and  predict the probability of loans not being paid back full

Accuracy is 84%

The model has poor accuracy at the threshold 0.5.

# ROCR
