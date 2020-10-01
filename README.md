# Multivariate-Analysis-Project

## Classification & Prediction of Loan Defaulters

### Using a dataset that contains information about loan applicants and their default flag 

#### Team no.8:  
* Adarsh Lalchandani    &nbsp; &nbsp; &nbsp; &nbsp;  &nbsp;      https://github.com/AdarshRL2109
* Karthik Grandhi    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &ensp;           https://github.com/karthii24
* Richard Britto      &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &ensp;          https://github.com/richardbritto97

#### Goal: 
There exists some co-relation between the information we get from a user and the probability of that person to be defaulting with an outcome being either a defaulter or not a defaulter. We will perform analysis and develop a model by which we can classify & predict the defaulters and not defaulters

#### Hypothesis:

Research Question | Hypothesis | Null Hypothesis
------------ | ------------- |  -------------
How does the interest rates affect loan default rates? | Borrowers who have high interest rate are more likely to default than the borrowers with low interest rate | There is no relationship between the interest rate and probability to default
How is the borrower's debt to income ratio affecting their chances of defaulting? | Borrowers who have high debt to income ratio have high chances of defaulting thatn the borrowers with low debt to income ratio | There is no relationship between the borrower's debt to income ratio and the probability to default
Can the loan ammount adjustments improve loan default rates? | Borrowers who have high loan amount are more likely to be charged off than the borrowers with low loan amount | There is no relationship between loan amount and probability to default
How does the borrower's annual income affect their chances of defaulting? | Borrower's having high annual income have less chances to default thant the borrowers having low or average income | There is no relationship between the borrower's annual income and the probability to default



#### Data Dictionary:

Variable | Description
------------ | -------------
member_id | Unique identifier
loan_status | Current status of the loan
int_rate | Interest Rate on the loan
Bin_int | int_rate bins for categorical use
dti | A ratio calculated using the borrower’s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower’s self-reported monthly income.
Bin_dti | dti bins for categorical use
Default_flag | A Boolean value where 0 means no default & 1 means default
No_of_Enquiry | The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
enq_buckets | bucket or groups of enquiry for categorical use
annual_inc | The self-reported annual income provided by the borrower during registration.
Income_bins | bins of income for categorical use and to map outliers easily
Purpose | A category provided by the borrower for the loan request. 
home_ownership | status of the ownership of the borrower's property, takes categorical value
purpose | states the purpose for which the loan was taken
open_acc | The number of open credit lines in the borrower's credit file.
emp_length | The job title supplied by the Borrower when applying for the loan.
verification_status | the status of verification stating whether source verified, verified, or not verified
delinq_2yrs | The past-due amount owed for the accounts on which the borrower is now delinquent.
loan_amnt | The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
Bins_loan_amt | Bins for loan_amnt for categorical use


#### SPECIFIC QUESTIONS:

* What is the effect of loan amount to the percentage of loan defaults?

* What effect does the borrowers loan enquiry in the last 6 months have on percentage of defaults?

* How does the number of active credit lines of the borrower affect the percentage of defaults?

* How does the DTI affect the number of loan defaults?

* What is the effect of delinquency by the borrower in the last 12 months affect the loan default 	          percentage

#### SPECIFIC ANALYSIS & GRAPHS:

* Univariate Analysis of specific variables vs number of defaults

* Bivariate Analysis on continuous variables vs default percentages

* Bivariate Analysis on categorical variables vs default percentages

* Multivariate Analysis to create the co-relation plot and find significant of each feature 
	
#### KEY PERFORMANCE INDICATORS: 

Key Performance Indicators (KPI) can be achievable by determining what is your objectives, how you plan on achieving them, and who can act on this information. The following are the KPI’s which will help us to evaluate and determine the final analysis. 

Key Performance Indicators | 
------------ | 
Default Percentage |
Interest Rate |
Annual Income |
Loan Amount |
Loan inquiry in lat 6 months |
Delinquency in last 12 months |
Purpose for loan |
Debt to Income ratio |
Number of active credit lines |

#### KPIs need to be defined according to critical or core business objectives.
Following steps helps to define a KPI: 

#### 1.   What is your desired outcome? 
-> Classify and predict loan defaulters based on information given at the time of request.

#### 2.   Why does this outcome matter? 
-> There is a particular business interest and risk prevention by the outcome of ouranalysis.

#### 3.   How are you going to measure progress? 
-> Progress will be measured by the accuracy and predictive power of the different models which 	   we will be training and testing.

#### 4.   How can you influence the outcome? 
-> To find out the most affecting parameter that drives default rates, we shall achieve model 		   parsimony so as to have specific influencing variables.
	

#### 5.    Who is responsible for the business outcome? 
-> The lenders are responsible for the business outcome.

#### 6.   How are borrowers benefiting from this? 
-> Borrowers will get a value assigned to their profile.

#### 7.   How often will you review progress towards the outcome? 
-> Will be reviewing on a quarterly basis to see the effects. 
