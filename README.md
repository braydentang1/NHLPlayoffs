# NHLPlayoffs
A model that aims to predict the outcome of a NHL playoff series before the first game is played. Currently, the model is an ensemble of five bagged elastic net models, trained on data from the 2006-2019 NHL playoffs.

Sits at 0.65742 log loss using data from 2006 and onwards.

# Dependecies:

- glmnet
- caret
- tidyverse
- recipes
- moments
- ParBayesianOptimization
- parallel
- fastknn
- RSelenium and a proper configuration of Selenium installed on your local machine
- rvest

Folder Descriptions:
-----
- data: Contains all raw, processed, and external data sets that the model is fit on.
- src: Contains all modelling, prediction, and scraping scripts in R. Also contains templates used for building the dataset.

How To Run:
-----
For validation of the model:

1. Download/clone the repository.
2. Set your current working directory to the directory of the downloaded repository on your local computer.
3. Run "src/modelling/main/bagged-elastic-net-bayesian.R."

For prediction of new data:

1. Download/clone the repository.
2. Set your current directory to the directory of the downloaded repository on your local computer.
3. Run "src/prediction/R/final-model.R". The vector new_data must contain observations with missing "result_factor" values.

TODO:
-----
- [x] 1. Finish the scraping automation.
- [x] 2. Redo the modelling script. 
- [x] 3. Add 2013 data. Improve the model below 0.67 log loss.
- [ ] 4/5. Paired t-tests? 
- [ ] 4/5. Document how to run the scripts from scratch.
- [ ] 4/5. Docker?

Credits:
-----
Data Pulled From:

https://www.corsicahockey.com/ <br>
http://www.puckon.net/ <br>
https://evolving-hockey.com/ <br>
http://www.espn.com/ <br>
https://www.nhl.com/ <br>
https://www.oddsportal.com/ <br>
https://www.naturalstattrick.com/ <br>
https://www.hockey-reference.com/

Credit for the ELO calculator formulas goes to the owner of HockeyAnalytics. The source page can be found here:

http://hockeyanalytics.com/2016/07/elo-ratings-for-the-nhl/

A big thanks to my good friend Mr. Riley Peters for spotting the logic flaws in some of the pre processing as well as supplying large amounts of suggestions on the game of hockey. 
