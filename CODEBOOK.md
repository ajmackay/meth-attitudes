# Codebook & Data Dictionary

The following codebook contains information on scripts, individual variable names as well as calculations of specific assessments...

## Script & Analysis Information

While the analyses scripts (descriptives, multigroup-analyses and survey-analyses) can be run in any order, if running all code for the first time (or if an update has occurred in load-data or data-processing), load.data must be run before data-processing.

### load-packages.R

Loads and attaches all packages used in subsequent scripts and analyses.

Note: The package "Librarian" is used to load and attaches the relevant packages (as it will automatically install packages that are not currently installed). If Librarian is not already installed, running "load-packages" will automatically install Librarian before any other necessary packages.

### load-data.R

Does one of two things:

1.  If data has been updated and there are multiple survey files in the "data/" directory, it will take the most recent file and perform basic cleaning (adjusting column names) so that the data is in a data frame format. Once it has the raw data in a data frame, it moves the old file to an archive and saves the updated data as an object in `objects/survey-raw.RData`.
2.  If there has been no updates to the data since the last run of `load-data.R`, it will load the current data located in `objects/survey.raw.RData`

### data-processing.R

### descriptives.R

-   Runs descriptive analysis on data.

-   Descriptive summaries are saved as 'summ.'

### output-prep.R

-   Prepares the output in relevant format (primary ggplots and flextables) to either then be exported or used in quarto document

### 

## Assessments

### Attitudes Toward DUID

#### Variables

| Variable Name       | Questionnaire Item                                                    |
|------------------|------------------------------------------------------|
| duid.att.friends    | Most of my friends think its OK to take drugs and drive               |
| duid.att.high       | It's OK to drive after taking drugs as long as you're not too high    |
| duid.att.jail       | People who take drugs and drive should go to jail                     |
| duid.att.scrict     | My community needs stricter laws against drug driving                 |
| duid.att.police     | The police aren't tough enough on drug drivers                        |
| duid.att.caught     | It's OK to drug drive as long as you don't get caught                 |
| duid.att.once.while | Everybody who takes drugs, drives under the influence once in a while |
| duid.att.dumb       | My friends would think I was dumb if I drove after taking drugs       |
| duid.att.overrated  | The dangers of drug driving are overrated                             |
| duid.att.lose       | People who take drugs and drive should lose their driver's license    |

#### Caulculation

### Instances of DUI (only shows to those who have drunk alcohol)

| Variable         | Questionnaire Item                                                                                                                                                  |
|-------------|----------------------------------------------------------|
| dui.inst.revoked | Have you ever had your driver's license or learner's permit suspended or revoked for moving violations related to alcohol use?                                      |
| dui.inst.hurt    | Have you ever done things that could have easily hurt you or someone else - like speeding or dirivin or using heavy machinery while under the influence of alcohol? |

### Attitudes toward DUI

| Variable           | Questionnaire Item                                                       |
|-----------------|------------------------------------------------------|
| dui.att.friends    | Most of my friends think its OK to drink and drive                       |
| dui.att.drunk      | It's OK to drive after drinking as long as you're not too drunk          |
| dui.att.jail       | People who drink and drive should go to jail                             |
| dui.att.strict     | My community needs stricter laws against drink driving                   |
| dui.att.police     | The police aren't tough enough on drink drivers                          |
| dui.att.caught     | It's OK to drink and drive as long as you don't get caught               |
| dui.att.once.while | Everybody who drinks alcohol, drives under the influence once in a while |
| dui.att.dumb       | My friends would think I was dumb if I drove after drinking alcohol      |
| dui.att.overrated  | The dangers of drink driving are overrated                               |
| dui.att.lose       | People who drink and drive should lose their driver's license            |

### Strategies against DUI

| Variable            | Questionnaire Item                                                                                                |
|---------------|--------------------------------------------------------|
| dui.strat.leave     | Imagine you are out and know you have had too much to drink. Would you leave your car where it was and not drive? |
| dui.strat.light     | How likely would you be to drink light or zero alcohol drink (beer/cider/wine) if driving?                        |
| dui.strat.driver    | How likely would you be to plan ahead that the driver will not drink?                                             |
| dui.strat.use       | How likely would you be to plan ahead not to drink if driving?                                                    |
| dui.strat.taxi      | How likely would you be to take a taxi/Uber if you had been drinking?                                             |
| dui.strat.track     | How likely would you be to keep track of your drinks and stay under the limit if driving?                         |
| dui.strat.overnight | How likely would you be to stay away overnight if you have been drinking?                                         |
|                     |                                                                                                                   |
