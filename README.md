# R-Shiny-App-for-California-Housing
House Price prediction 

# Project Overview
This project is an interactive R Shiny web application that predicts house prices in California using Machine Learning models trained on the California Housing Dataset. The app provides an intuitive interface for data exploration, model training, and predictions, allowing users to analyze housing trends and forecast property values based on key real estate factors.

# Features
Data Exploration: Visualize the distribution of different housing attributes.
Interactive Map: View house prices across California using Leaflet maps.
Model Training: Choose between Random Forest and Linear Regression models.
Model Insights: Analyze model performance, accuracy, and key feature importance.
Price Prediction: Input custom housing attributes to predict home values.

# Technologies Used

R Shiny: Web-based interactive UI.
tidyverse: Data processing and manipulation.
caret: Machine learning model training.
randomForest: For building robust predictive models.
mice: Data imputation to handle missing values.
ggplot2: Data visualization.
leaflet: Geospatial visualization of house prices.
shinydashboard: Enhancing UI with a structured dashboard layout.

# How to Run the App

1. Clone the Repository
   git clone https://github.com/your-username/house-price-prediction.git
cd house-price-prediction

2. Install Dependencies in R
   install.packages(c("shiny", "tidyverse", "caret", "randomForest", "ggplot2", "mice", "leaflet", "shinydashboard"))
   
3. Run the Application
   shiny::runApp("app.R")

# Dataset Information 
The dataset used is California Housing Data from Kaggle, containing housing attributes, demographics, and location-based information. The data is cleaned and preprocessed before being used for model training.

# Application Flow
Data Exploration: Select features and generate visualizations.
Map Visualization: View house prices based on geographical distribution.
Train Models: Choose Random Forest or Linear Regression and train models.
Analyze Model Performance: Check model accuracy and variable importance.
Predict House Prices: Input housing details and get real-time price predictions.

# Future Enhancements
Include more ML models like Gradient Boosting, XGBoost.
Add feature selection techniques for improved model performance.
Expand to include real-world housing datasets.





