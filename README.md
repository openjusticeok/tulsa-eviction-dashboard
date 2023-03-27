# Data Request: Hyperobjekt/CPAL, Tulsa Eviction Dashboard Data

Provides data on individual eviction cases in Tulsa county (mostly after 2018)
and places those cases within different geographic levels. The levels available are:
- County as Whole
- Cities/Towns
- Zip Codes (ZCTA)
- Census Tract
- Federal House Legislative Districts
- State House & Senate Legislative Districts
- Voting Precincts
- Judicial Districts
- Tulsa Count Public School Districts
- Tribal Lands
- ~~TBD: City Council Districts~~

## Instructions:

**ALWAYS: Run renv::restore() if first-time running analysis.**

#### (Manual)

The following describes the order to run the analysis from start to finish.
1) /src/data-management/ojodb_tulsa_evictionData.R
   Output: /out/data-management/tulsaEvictionData.csv
2) /src/analysis/coordinatesToGeographies.R
   Output: /out/analysis/tulsaEvictionGeographies.csv

## Directory Guide:

-  ./ the Root Folder: The root directory.

-  ./src folder: This is the ‘source’ folder. It contains all of your code files you develop
   during your analysis and the original datasets you begin your analysis with.

-  ./out folder: This is the output directory. We will put anything that we create by
   running a R script. For example, it can contain new datasets we create by cleaning
   and merging our original data, saved regression results and saved figures or summary
   tables. The main point is that anything we can recreate by running R scripts will get
   saved here - whether they be ‘temporary’ or ‘intermediate’ outputs that will get used
   by another R script later in our project, or ‘final’ outputs that we will want to
   insert into a paper, report or slide deck.

- ./sandbox folder: As we work through our project, we will want to explore new ideas
  and test out how to code them. While we play with these bits and pieces of code, we
  save them in the sandbox. Separating them from src means we know that R scripts in
  here are ‘under development’. When they are finalized, we can move them into src.

## Subfolders Guide:

- data/ contains all of the project’s original/raw data.

- data-management/ contains all R scripts to clean and merge datasets together

- data-specs/ contains any special parameterizations used in cleaning or analysis.

- analysis/ contains all R scripts that are our main analysis. For example, our
     regression scripts

- lib/ contains R scripts that contain functions that can be used more generally.
      For example helper functions that can be used in both data cleaning and analysis
      could be put here. So can scripts that contain functions that can be portable
      across multiple projects.

- figures/ contains R scripts that produce figures. One script per figure.

- tables/ contains R scripts that produce summary tables and regression tables.
     One script per table.

- slides/ contains the Rmarkdown files to write up project results as a slide deck, i.e. the text of the slides.
