# placing-prosperity-USALEEP

This repo provides R scripts to replicate the analysis in [Placing Prosperity: Neighborhoods and Life Expectancy in the New Orleans Metro](https://www.datacenterresearch.org/reports_analysis/placing-prosperity/). A brief description of the project and instructions for replicating the analysis are below. This repo is chiefly provided to inform those wishing to use USALEEP data on life expectancy at the census tract level or those wishing to adapt portions of The Data Center's analysis to other settings. 

### About Placing Prosperity

Using a national source of data on life expectancy at the neighborhood level as a starting point, this report examines the role of place in shaping shared prosperity in the New Orleans region. Taking data on life expectancy at the neighborhood level as a "snapshot" of pre-pandemic health inequality, *Placing Prosperity* is three-part series of data briefs that explores the causes and consequences of neighborhood differences in health and well-being over the life course. The analysis reveals how an uneven landscape of investment and socioeconomic opportunity shapes life expectancy, and shows that neighborhood inequality remains inextricable from racial inequality. Please read the [three-part interactive report] to learn more about the project and to see the findings.

Here is a description of the life expectancy data that motivated the analysis:
> The United States Small-Area Life Expectancy Estimates Project (USALEEP) is the first public health outcome measure available nationwide at the census tract level—measuring life expectancy at birth for nearly every census tract in the country. A joint effort of The Robert Wood Johnson Foundation, National Association for Public Health Statistics and Information Systems (NAPHSIS), and the National Center for Health Statistics (NCHS) at the Centers for Disease Control (CDC), USALEEP data provide unparalleled insights into
community health and demonstrate that not everyone has the same opportunity to be healthy where they live.

For more information about the USALEEP data, please visit https://www.naphsis.org/usaleep.

This project is based upon work supported by the Urban Institute through funds provided by the Robert Wood Johnson Foundation. We thank them for their support but acknowledge that the findings and conclusions presented in this report are those of the author(s) alone, and do not necessarily reflect the opinions of the Urban Institute or the Robert Wood Johnson Foundation.

We provide R code to replicate the analysis below in the hopes that it might be useful for analysts addressing similar issues, using the USALEEP data, or highlighting place-based inequality in other regions of the US. 

### Instructions for replication

*Placing Prosperity* focuses on New Orleans and its 8-parish metro area. Anyone wishing to replicate the analysis for another region would have to use the relevant FIPS codes. 

These R scripts are used to import, clean, and combine data sets. They should be run in order before conducting the analysis.

- 01_libraries and load data.R -- Loads R packages and imports data, either locally from the inputs folder or via API.
- 02_graphics themes.R -- Loads a set of colors, fonts, and ggplot templates used at The Data Center. DELETE?
- 03_cleaning and analysis.R -- Performs additional cleaning of data sets

With the data loaded, the following R scripts are used to conduct the analysis. The scripts generally use ggplot to make charts and to export CSV files to the **outputs** folder. Each script corresponds with a chapter in the final report. These CSV files are then used to generate interactive graphics for the final project web site. 

- chapter 1 graphics.R
- chapter 2 graphics.R
- chapter 3 graphics.R

The final interactive graphics on the website were produced with separate charting libraries.

#### Data sets

Each of the data sets used in this project is described on the project [methdology page](https://www.datacenterresearch.org/placing-prosperity/methodology.html), as well as detail on how the data was analyzed. 

Briefly, the following main sources are used:

- [USALEEP](https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html) for life expectancy by census tract
- [Opportunity Atlas](https://opportunityinsights.org/data/?geographic_level=0&topic=0&paper_id=1652#resource-listing) for income mobility by census tract
- American Community Survey for tract demographics, education, and income and poverty, with additional historical census tract-level estimates from Geolytics.
- CDC WONDER for parish-level mortality trends

Source data sets can be found in the **inputs** folder.

In order to replicate chapter 1's mortality trends for another location, data will have to be sourced from CDC WONDER. In order to replicate the historical analysis of neighborhood change in chapter 2 for another location, neighborhood boundaries and geographically harmonized census estimates would have to be sourced.
