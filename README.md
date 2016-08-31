# R-Statistical-Analysis-Scripts
R Scripts to conduct statistical anaylsis with example datasets.

Statistical Analysis Includes
- Confidence Interval Calculation
- Hypothesis Testing
- Linear Modelling
- Robust Linear Modelling
- Sample Size Calculation

### Example Statistical Anaylsis covered in each file
- Confidence Interval Calculation
  1. Confidence Interval from Bootstrapped Data
  2. Confidence Interval from Population mean (assuming normally distributed data)
  3. Confidence Interval Calculations 
  4. Tolerance Interval
  5. Population Proportion Confidence Interval
  6. Population Variance Confidence INterval
  7. Median Confidence Interval
  8. Variance and Correlatino Testing
  9. Wilcox Test paired
  
- Hypothesis Testing
  1. Hypothesis Testing of Mean ( Small Sample N < 40), Test statistic - T distribution (if small sample)
  2. Hypothesis Testing Variance ( Small Sample N < 40), Test statistic - Chi distribution (if small sample)
  3. Hypothesis Testing Independence of two groups, Test statistic - Chi distribution 
  4. Hypothesis Testing for Comparing two Variances, Use F statisitc = s1^2/s2^2
  5. Hypothesis of Delta between Two Data Sets, Test statistic - T distribution (if small sample)
  6. Hypothesis Testing of two Proportions, Use Z statistics proportion of children in either groups is statisiically different
  7. Hypothesis Test of Median, Test statistics is the observed number of plus differences r+ = (n/2)
  8. Non Parametric Median Hypothesis (Wicox Rank Sum, Wilcox Sign Rank, Sign Test)

- Linear Modelling
  1.  BIC
  2.  AIC
  3.  Error/Residual Diagnostics
  4.  Model Diagnostics plot the histogram of the residuals and add the pdf. This is to see how good the residuals fit a normal distribution. You can do a KS test on this, or a qqplot..)
  5.  Skill Evaluation
  6.  Gamma Function Fitting
  7.  Log Normal Fit

- Robust Linear Modelling
  1.  Perason Correlation Calculation
  2.  Kedall's Tau Correlation Caluclation
  3.  Spearman's Rank Correlation Calculation
  4.  Kendall Thiel Slope Estimate

- Sample Size Calculation
  1.  Simple sample size calculation based on desired alpha

### Build
Scripts created using RStudio V.99.491

## License
MIT License

Copyright (c) 2016 Justin Chin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Acknowledgments
Sample datasets used in the scripts are from CU Boulder Professor Balaji's 
PROBABILITY & STATISTICAL METHODS FOR NATURAL AND ENGINEERED SYSTEMS course website
http://ceae.colorado.edu/~balajir/CVEN5454/
