Car Price Prediction - Analysis of Fabia Dataset
Project Overview
This project explores the relationship between various car characteristics (such as age, mileage, engine power, and cubic capacity) and their selling price using the Fabia dataset. The goal is to build a model to predict the price of a car based on these factors.

Data Description
The dataset consists of the following columns:

date: The year of manufacture.

km: The mileage of the car in kilometers.

kW: The engine power of the car in kilowatts.

Price: The price of the car in euros.

engine_cc: The cubic capacity of the engine.

Steps in the Analysis
Data Preprocessing:

The car's age is calculated as 2023 - date.

The engine cubic capacity (engine_cc) is assigned based on the car's power (kW).

Descriptive Analysis:

Histogram plots are generated to visualize the distribution of variables (age, mileage, engine power, engine cubic capacity, and price).

Summary statistics are computed to get a deeper understanding of each variable.

Price by Age:

Histograms and comparisons of mean and variance of prices for different car ages.

Price by Mileage:

The effect of mileage on car prices is analyzed by splitting mileage into different intervals.

Both mean and variance of price are visualized for these intervals.

Regression Analysis:

A multiple regression model is built using the car's age, mileage, engine power, and engine cubic capacity to predict the price.

The model's goodness of fit is evaluated using R-squared, and assumptions are checked using diagnostic plots.

Model Refinement:

Stepwise selection (using AIC) is applied to refine the model by excluding variables that do not contribute significantly to the model.

Prediction:

The model is used to predict car prices for new data, and 95% confidence intervals for residuals are computed.

Key Functions and Visualizations
Histograms: To visualize the distribution of variables (e.g., price, age, mileage).

Regression Models: Multiple regression analysis to predict car price based on various factors.

Stepwise Selection: AIC-based model refinement.

Residual Analysis: Calculation of residuals, and confidence intervals to assess model accuracy.

Key Insights from the Analysis
Age and Price: Younger cars tend to have higher prices, but the variance in prices also increases with age.

Mileage and Price: Cars with lower mileage tend to have higher prices, but the price variance decreases as mileage increases.

Engine Power and Price: More powerful engines (higher kW) correlate with higher prices, but the relationship is not perfectly linear.

Model Interpretation
Regression Model: The final model uses age and mileage as predictors for the car's price, showing a strong relationship between these variables and the price.

Adjusted R-squared: The model explains 83.19% of the variance in car prices.

Stepwise Selection: By using AIC, a more optimized model is created by excluding insignificant variables.

Usage
Running the Analysis:

Install necessary packages (tidyverse, MASS).

Load the dataset and execute the analysis by running the R script.

Prediction:

Use the final regression model to predict the price of new cars by providing values for age and mileage.

Conclusion
This analysis demonstrates the use of multiple regression techniques to understand the factors affecting car prices. The model's predictions can be used for estimating car prices based on age and mileage, helping potential buyers and sellers make informed decisions.
