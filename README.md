# CBW-CSNAPNI

The **Commodity-Specific Net Anthropogenic Nitrogen Inputs for the Chesapeake Bay Watershed (CBW-CSNAPNI)** model is an R-based material flow analysis (MFA) framework that tracks and quantifies reactive nitrogen (N) flows through agricultural systems across the Chesapeake Bay Watershed (CBW).
The model operates at both the **county** (197 counties) and **Land River Segment** (1,925 LRS) spatial scales, covering six Census years from **1997 to 2022**.

CBW-CSNAPNI is adapted from the original [CSNAPNI model](https://github.com/malgren/CSNAPNI) and reoriented to the CBW region, incorporating spatially explicit concordance matrices derived from satellite-based cropland data (USDA Cropland Data Layer) to allocate county-level agricultural data to individual LRS.

------------------------------------------------------------------------

## Study Area

The model covers the Chesapeake Bay Watershed across six states:

-   Delaware, Maryland, New York, Pennsylvania, Virginia, West Virginia

------------------------------------------------------------------------

## Spatial Scales

| Scale | Units | Description |
|------------------------|------------------------|------------------------|
| County | 197 | USDA NASS Census counties within the CBW |
| Land River Segment (LRS) | 1,925 | Phase 6 Beta CBW model hydrologic units from [Chesapeake Bay Assessment Tool (CAST)](cast.chesapeakebay.net) |

------------------------------------------------------------------------

## Temporal Coverage

Six USDA Census of Agriculture years: **1997, 2002, 2007, 2012, 2017, 2022**

------------------------------------------------------------------------

## Model Structure

The model is composed of multiple modular R scripts housed in the `Config/`, `CreateInputsSubs_CBW/` and `ModelSubs/` directory and orchestrated by a main script CSNAPNIv2.R.
`Config/` contains the configuration settings `CreateInputsSubs_CBW/` contains scripts that process raw input data (NASS, CDL, shapefiles) into model-ready formats.
`ModelSubs/` contains scripts that implement the core NANI and CS-NANI accounting framework and scenario analyses.

Key components include:

| Script | Description |
|------------------------------------|------------------------------------|
| `CSNAPNIv2.R` | Main model entry point; sources all submodules |
| `cropdata.R` | Crop production and area harvested processing (NASS data) |
| `animalpop.R` | Livestock population dynamics across animal categories |
| `fertilizer.R` / `Cprodfertfix.R` | Synthetic and organic fertilizer N allocation to crops |
| `manure.R` | Manure N generation, recovery, and N plant available |
| `biogas.R` | Biogas/anaerobic digestion scenario — digestate N and CH₄ |
| `nani.R` / `OutputN.R` | NANI component calculation and output |
| `scenarios.R` | Scenario framework (baseline + 9 BMP scenarios) |

------------------------------------------------------------------------

## N Accounting Framework

CBW-CSNAPNI estimates four components of **Net Anthropogenic Nitrogen Inputs (NANI)**:

1.  **Atmospheric N deposition** — From CMAQ/EQUATES gridded deposition data
2.  **Biological N fixation** — Crop-specific symbiotic fixation (soybeans, alfalfa, pasture legumes)
3.  **Synthetic fertilizer N** — Inorganic N applied to crops, informed by NASS Chemical Use Surveys and PSU Agronomy Guide
4.  **Net Food & Feed N** — Net import/export of N embodied in food and feed commodities

In addition to watershed-scale NANI, the model computes **commodity-specific N inputs** (kg N / kg protein) for:

-   **Crops**: corn grain, corn silage, wheat, oats, barley, sorghum, potatoes, rye, alfalfa hay, other hay, soybeans, pasture, and corn-derived feeds (CGF, CGM, DGS)
-   **Livestock products**: beef, milk, pork, eggs, broiler chicken, turkey

------------------------------------------------------------------------

## Scenarios

The model supports ten scenarios:

| \# | Scenario | Description |
|------------------------|------------------------|------------------------|
| 1 | Baseline | No BMP intervention |
| 2 | Grass | 10% of corn area converted to perennial grass |
| 3 | Winter rye | 50% of corn area planted with winter rye as double crop |
| 4 | Grass + Winter rye | Combined scenario (2+3) |
| 5–10 | Biogas variants | Combinations of grass/winter rye with anaerobic digestion of manure and biomass for renewable natural gas (RNG) production |

------------------------------------------------------------------------

## Key Inputs

-   **Crop production and area**: USDA NASS Census of Agriculture (county level)
-   **Livestock populations**: USDA NASS Census of Agriculture (county level)
-   **Cropland geolocation**: USDA Cropland Data Layer (CDL/CropScape), processed in ArcGIS
-   **Atmospheric N deposition**: CMAQ/EQUATES gridded TN deposition rasters
-   **Manure management**: secondary data from literature state-level recovery factors (Kellog, 2014)
-   **Animal feed composition**: AFIA regional feed consumption data (Northeast states)
-   **Fertilizer application rates**: NASS Chemical Use Surveys + PSU Agronomy Guide + USDA NASS Census of Agriculture (county level)
-   **Spatial concordance**: LRS-county overlap areas derived from Phase 6 Beta CBW shapefile from CAST

------------------------------------------------------------------------

## Key Outputs

| Output | Unit | Scale |
|------------------------|------------------------|------------------------|
| NANI components (atm. deposition, fixation, fertilizer, net food & feed) | kg N / yr | LRS, county, watershed |
| Commodity-specific N inputs | kg N / Animal Unit | Watershed |
| Crop N inputs (fertilizer + fixation) | kg N / km² | LRS, county |
| Livestock manure N (total, recoverable, plant-available) | kg N / yr | LRS |
| Biogas / RNG potential | m³ CH₄ / yr | LRS |
| N riverine export estimates | kg N / yr | Major basin |

Select output matrices are exported to `.txt` files in the `OutputFiles/` folder during a model run.

------------------------------------------------------------------------

## Running the Model

1.  Clone or download the repository and unzip.
2.  Create a project in RStudio within the folder unzipped. Functions getwd() and setwd() available in the main code, if needed.
3.  Set file paths using the `adjust_file_path()` utility (handles Windows/Mac differences).
4.  Source the main script:

``` r
source("CSNAPNIv2.R")
```

5.  To run a specific BMP scenario, set the rscenarios flag before running the main script (`CSNAPNIv2.R`):

``` r
rscenarios <- c(
  'baseline', 'grass_pct', 'grass_profitability',
  'cover_crop', 'double_crop',
  'grass_cover_combo', 'grass_double_combo',
  'biogas_grass_pct_double_combo',
  'biogas_grass_profit_double_combo',
  'biogas_grass_profit_cover_combo'
)
CURRENT_SCENARIO <- scenarios[1]  # Change index to switch scenarios
```

------------------------------------------------------------------------

## Repository Structure

```         
CBW-CSNAPNI/
├── Config/                  # Host settings file from the mode
├── RawData/                 # Source data (NASS, CDL, shapefiles)
├── InputFileKeys/           # File keys for interpreting processed input matrix files
├── InputFiles_CBW/          # Processed input data files
├── OutputFilesKeys/         # File keys for interpreting model output matrix files
├── OutputFiles_CBW/         # Model output text/Excel files
└── README.md
```

------------------------------------------------------------------------

## Dependencies

The model requires the following R packages:

``` r
install.packages(c(
  "readxl", "writexl", "openxlsx", "rnassqs",
  "ggplot2", "sf", "dplyr", "tidyr", "purrr",
  "networkD3", "terra", "viridis", "ggpubr",
  "corrplot", "classInt", "gt", "ggsci", "lm.beta", "EnvStats", "fitdistrplus",
  "knitr", "patchwork", "plotly", "stringr", "scales", "tidyverse", "data.table", DT
))
```

------------------------------------------------------------------------

## Citation

> Algren, M, Costello, C, Landis, A E, and Z.
> U. M. Chowhury.
> “Commodity-Specific Net Anthropogenic Phosphorus and Nitrogen Inputs (CSNAPNI) GitHub Repository.” May 18, 2022.
> <https://github.com/malgren/CSNAPNI>.
>
> Costello, Christine, Xiaobo Xue, and Robert W. Howarth.
> “Comparison of Production-Phase Environmental Impact Metrics Derived at the Farm- and National-Scale for United States Agricultural Commodities.” Environmental Research Letters 10, no. 11 (2015).
> <https://doi.org/10.1088/1748-9326/10/11/114004>.
>
> Chesapeake Bay Assessment Tool.
> “CAST - Map Tools & Spatial Data.” 2024.
> <https://cast.chesapeakebay.net/Documentation/MapToolSpatialData>.
>
> Hong, Bongghi, Dennis P. Swaney, and Robert W. Howarth.
> “A Toolbox for Calculating Net Anthropogenic Nitrogen Inputs (NANI).” Environmental Modelling and Software 26, no. 5 (2011): 623–33.
> <https://doi.org/10.1016/j.envsoft.2010.11.012>.
>
> USDA.
> “CroplandCROS.” 2022.
> <https://croplandcros.scinet.usda.gov/>.
>
> USDA National Agricultural Statistics Service.
> “NASS Quick Stats.” 2024.
> <https://quickstats.nass.usda.gov/>.

------------------------------------------------------------------------

## Contact

Lucas de Lima Casseres dos Santos — [lds5498\@psu.edu](mailto:lds5498@psu.edu){.email}\
Christine Costello — [chriscostello\@psu.edu](mailto:chriscostello@psu.edu){.email} Department of Agricultural and Biological Engineering, Penn State University

