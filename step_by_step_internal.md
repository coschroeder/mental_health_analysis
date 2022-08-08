# How to work with the data

### You need:
  - access to github repository
  - the preprocessed data file: `preprocessed_coarsed_data_v1.csv`

    (depending on your analysis you might also need the file `preprocessed_data_v1.csv`, which is the preprocessed, but not coarsed data)

    You can find the data in the MS One drive folder, `DATA/data_v1.zip`, password protected.
  - a working R/R studio environment


### Start   
  0. Open the file `mental_health_analysis\data_analysis\data_analysis.R `
  1. Load the data by changing the path to the folder in which you have downloaded the data. Best: just comment out the line, copy it and insert it. So that the others don't lose their path when you push your changes.
  2. Pick a Todo#Nr, write down your name next to it and: GO.

All the information for the `preprocessed_coarsed_data_v1.csv` *should* be in the `notebooks` folder, in the data_view.nb.html. If not, please note it down under *Todos* and state what is missing. It could also be worth to look back into the questionnaire...

If you work with `preprocessed_data_v1.csv` you might have to look into `preprocessing\data_preprocessing.R` for more detailed info. But please include this info also to the data_analysis.R file.

**Important**: to avoid merge conflicts you may want to work on a copy of the file and just copy your bits and pieces to the main file later.

## Todos:
#### General:
- Todo#1: Which one is the "Perceived Stress Scale" (Cohen) and the mentioned questions on "satisfaction life, job" (Hellgren)? Note this down in the `data_analysis.R` **@Julian**

#### Descriptive stats:
- Todo#2: EV: EV06, EV07, EV08: Job satisfaction (Hellgren et al.), Scale @Julian
- Todo#3: WG
- Todo#4: GH: GH01, GH02, GH03, GH04: Perceived Stress Scale (Cohen) -> GH02, GH03 recode! Scale 
- Todo#5: OR
- Todo#6: ST: ST13, ST14, ST15: Job insecurity (Hellgren et al.) @Julian
- Todo#7: SH
- Todo#8: MH

#### Correlation analysis
- Todo#9

#### Analysis of open questions
- Todo#10: prepare already for the data to come
