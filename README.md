# NHLPlayoffs
A model that aims to predict the outcome of a NHL playoff series before the first game is played. 

Currently sits at 0.675 AUROC.

Folder Descriptions:
-----
1. **All Team Stats:** .csv files pulled from corsica.hockey. Used in the .R file CorsicaAllTeamStats, located in the Scraping Scripts and Template.
2. **Game Score:** .csv files pulled from corsica.hockey. Used in the .R file CorsicaGameScore. 
3. **Hockey Reference:** .html renders of various web pages from hockey-reference. This is used to get around webpages that require JavaScript to be run upon opening the webpage in a browser which, to my knowledge, rvest cannot deal with. Used for scraping.
4. **Modelling:** .R files where the actual modelling scripts can be found. 
5. **NHL HTML Renders:** Similar to the folder "Hockey Reference", this contains .html renders of webpages from NHL.com, again to get around the use of JavaScript when scraping these pages.
6. **Notebooks:** Jupyter Notebooks with more comments on the modelling code. Also shows variable importance and the final AUROC score after nested cross validation. Otherwise, this is exactly the same thing as the folder "Modelling".
7. **Odds HTML Renders:** Similar to the folder "Hockey Reference", this contains .html renders of webpages from OddsPortal, again to get around the use of JavaScript when scraping these pages. Used to get Vegas opening game odds (ie. opening odds for the very first playoff game of a series) for each series.
8. **Required Data Sets:** All datasets needed to run the model (the .R file in 4) or the Jupyter Notebook in 6)). These .csv files are fully processed files for all 195 NHL playoff series since 2006 using data scraped from various webpages. These .csv files can be generated by running each .R file in folder 9.
9. **Scraping Scripts and Template:** All .R files used to scrape the webpages and to proceess the data in a format that allows for making predictions. <br> The dataset itself is processed so that one row/observation is the difference between each variable, from the higher seeds perspective. Note that this decision has no theoretical basis and that a ratio, sum, product, or other transformation could have been used instead. I just thought that a difference made the most intuitive sense. Another note: feature engineering becomes harder when doing this process since the data is preprocessed as a difference. <br> There is an Excel template file that requires one to fill out a few columns; namely the two teams involved in the series, the year, the round, the highest seed betweeen the two teams (for processing), and the conference. Nothing is actually done in Excel, this is purely to get the data in a usable format and to help in function calls/html links. Every .R scraping file pulls this template in and builds off it. Any spreadsheet editor can be used to fill this out, and one can easily add rows in R if need be rather than using a spreadsheet.

How To Run:
-----
Using the .csv files already processed in the folder "Required Data Sets":

1. Download the repository.
2. Open up the .R file in the Modelling folder, called "Bagged Elastic Net.r".  
3. Change the working directory (the first line in the script) to some arbitrary folder (it doesn't matter what folder; this is purely for status checks on the process when running in parallel).
4. In the part of the script with title "Read Data In", change each file path to each respective file in the folder "Required Data Sets" located in the repository.

Creating the .csv files yourself (and changing the preprocessing):

1. Download the repository.
2. Run every .R file in Scraping Scripts and Template. Note that you will have to change the file path to link to the correct file in the repository. In general, the file path that pulls in the template will need to be changed for all of the scraping scripts. Also, the .R files that pull in external .csv/.html files from the repository (Corsica All Team Stats/Game Score, Hockey Reference Aggregated Stats, NHL Official, and OddsPortal) will need to have their file paths changed to the relevant folders in this repository. <br> Each processed .csv file is output to a folder that needs to be changed to match your own local computer. 
3. Each scraping script has a similar function called "processData". This is where the differencing between stats occurs (from the highest seeds perspective). This choice is arbitrary and could be changed to ratios, products, sums, etc.
4. Open up the .R file in the Modelling folder, called "Bagged Elastic Net.r". 
5. Change the working directory (the first line in the script) to some arbitrary folder (it doesn't matter what folder; this is purely for status checks on the process when running in parallel).
6. In the part of the script with title "Read Data In", change each file path to each respective file in the folder "Required Data Sets" located in the repository.


TODO:
-----
- [x] 1. Finish the scraping automation.
- [x] 2. Redo the modelling script. In particular, the for loops are awful and there is a lot of code that can be improved.
- [x] 3. Try to improve the model; ideally 0.63 consistently should be possible. Add 2013 data.
- [ ] 4/5. Document how to run the scripts from scratch + other critical decisions.
- [ ] 4/5. Shiny app.

Data Pulled From:

http://corsica.hockey/ <br>
http://www.puckon.net/ <br>
http://www.espn.com/ <br>
https://www.nhl.com/ <br>
https://www.oddsportal.com/ <br>
https://www.naturalstattrick.com/ <br>
https://www.hockey-reference.com/

Credit for the ELO calculator formulas goes to the owner of HockeyAnalytics. The source page can be found here:

http://hockeyanalytics.com/2016/07/elo-ratings-for-the-nhl/
