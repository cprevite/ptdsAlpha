# Group Alpha   

M Redwan Hasan, Ana-Maria Casian, Claudio Previte, Jeremy Choppe, Pierre-Emmanuel Got

## Our project

This github repository has been created as part of the course *Programming Tools in Data Science* 2019 given by S.Orso and I.Rudnytskyi. 
This course enabled us to create the package *ptdsAlpha* containing our functions which we show in a Shiny App.

The main goal of our project is to display economic and pollution indicators of European countries in the last four decades, check specific trends, interesting correlations, rankings and perform future forecasting of these indicators.

## Our Shiny App

We created different functions that we stored in our project package. Below is the list of the main contents of our App:

- Continuous animation of the top 10 ranked countries in various economic and pollution indicators. This gives us an overview of how European countries have changed relative to these important variables over the years.
- Animated GIFs showing the overall evolution of the European countries through a period of 40 years in regards to the pollution and economic indicators in our data, giving us a visual overview of the performance of the countries.
- Interactive map of Europe in order to provide a visual and informative comparison between the pollution and economic indicator between two years. This allows us to see the interaction between the indicators and even how each countries performed in them.
- Bubble graph which shows the movement of countries across the indicators over the past 40 years. This can be done for a single country or a collection of countries.
- Various maps, graphs, barcharts and tables in order to get a simple yet effective view of the data, figures and trends.
- Forecast each variable's future values with specific time series models, in order anticipate how things might be in times to come.

We then used the interactive web app creation platform [Shiny](https://shiny.rstudio.com/). The Shiny App displays the output of our functions.

### Prerequisites

You should install the *ptdsAlpha* package in your R console.
You may also need to download the software [ImageMagick](https://imagemagick.org/script/download.php). 


## Main references

Here our some sources we used in order to create this project:  

* [Euromonitor](https://www.euromonitor.com/sign-in)
* M. Beckman, S. Guerrier, J. Lee, R. Molinari & S. Orso : An Introduction to Statistical Programming Methods with R
* Wickham, Hadley. 2015. *R Packages*. O’Reilly.

## Built With

* [Shiny](https://shiny.rstudio.com/) - The interactive web app platform used

