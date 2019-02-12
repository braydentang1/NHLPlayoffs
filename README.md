# NHLPlayoffs
An attempt to predict the NHL playoffs. Currently sits at 0.62 AUROC.

TODO:

- [x] 1. Finish the scraping automation.
- [ ] 2. Redo the modelling script. In particular, the for loops are awful and there is a lot of old code that could be substantially              improved. 
- [ ] 3. Try to improve the model; ideally 0.63 consistently should be possible.
- [ ] 4/5. Document how to run the scripts for scratch + other critical decisions.
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
