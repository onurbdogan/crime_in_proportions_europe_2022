## Introduction

This repository contains materials related to the research paper titled *"Crime in Proportions: Applying Compositional Data Analysis to European Crime Trends for 2022"* by Onur Batın Doğan and Fatma Sevinç Kurnaz. The study explores patterns in crime rates across European countries by employing Compositional Data Analysis (CoDa) techniques.

### Description of Files and Folders

[data_cts_corruption_and_economic_crime.xlsx](./data/data_cts_corruption_and_economic_crime.xlsx): regarding corruption and economic crime present national statistics on offenses related to specific economic and environmental crimes documented by the police or other law enforcement agencies. See [UNODC metadata](https://dataunodc.un.org/sites/dataunodc.un.org/files/metadata_corruption_and_economic_crime.pdf) for details.

[data_cts_intentional_homicide.xlsx](./data/data_cts_intentional_homicide.xlsx): related to intentional homicide contain statistics on victims at various levels—city, national, regional, and global—as well as information on individuals who have been arrested, suspected, or convicted of intentional homicide. See [UNODC metada](https://dataunodc.un.org/sites/dataunodc.un.org/files/metadata_intentional_homicide.pdf) for details.

[data_cts_violent_and_sexual_crime.xlsx](./data/data_cts_violent_and_sexual_crime.xlsx): regarding violent and sexual crimes present national statistics on offenses and victims of specific crimes documented by the police or other law enforcement agencies.
See [UNODC metada](https://dataunodc.un.org/sites/dataunodc.un.org/files/metadata_violent_and_sexual_crime.pdf) for details.

[europe_crime_rate_2022.xlsx](./data/europe_crime_rate_2022.xlsx): is the manually filtered data from the above excel files.

[interpolated_na_values.xlsx](./data/interpolated_na_values.xlsx): shows the interpolation results of some missing values.

[raw_data.csv](./data/raw_data.csv): is the input data for all methods, and created after the prepocessing and imputation steps.

[figures](./figures/): folder containing vector images. 

[europe_crime_coda](.europe_crime_coda.R/): main script that includes all the code. 



