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
- City Council Districts

## Instructions:

**ALWAYS: Run renv::restore() if first-time running analysis.**

The following describes the order to run the analysis from start to finish.
1) R/ojodb_tulsa_evictionData.R
   Output: data/tulsaEvictionData.csv
2) R/coordinatesToGeographies.R
   Output: data/tulsa_eviction_cases.csv
