## Introduction

This repository corresponds to the paper titled "TITLE", available as a pre-print at "LINK". **link to website**

### Abstract

Abstract goes here

## File Structure

### Directory Tree

```         
.
├── data
│   ├── codebook.xlsx <--- codebook for VOC species to variable names
│   ├── site_info.csv <--- monitoring sites information (created in R/01_clean_data.R)
│   ├── raw
│   │   ├── rawdata.xlsx <--- raw, unprocessed data
│   │   ├── sitenames.xlsx <--- clean site names
│   │   ├── landuse_100mbuffer.csv <--- land use buffers from ArcGIS Pro
│   │   ├── landuse_assignments.csv <--- land use assignments by site (created in R/01_clean_data.R)
│   │   ├── coords.rds <--- monitoring site coordinates (created in R/01_clean_data.R)
│   │   ├── landuse_key.xlsx <--- key to land use types
│   │   └── site_traffic.xlsx <--- traffic designations from ArcGIS Pro
│   ├── clean (created in R/01_clean_data.R)
│   │   ├── dat_ppb.csv <--- clean, processed data in parts per billion
│   │   ├── dat_mgm3.csv <--- clean, processed data in micrograms per meter cubed
│   │   ├── colos.csv <--- co-located sites for reliability measurements
│   └── shp <--- shapefiles for mapping
│       ├── landuse_sw.shp <--- land use from Open Data Philly, clipped to study area
│       └── refinery.shp <--- former Philadelphia Energy Solutions (PES) refinery site tax parcels
├── R (see details table below)
│   ├── 00_plot_theme.R
│   ├── 01_clean_data.R
│   ├── 02_summarize_data.R
│   ├── 03_btex_ratios.R
│   ├── 04_run_pca.R
│   └── 05_summarize_pca_results.R
├── results
│   ├── tables <--- tables that appear in the main manuscript text
│   ├── figures <--- figures that appear in the main manuscript text
│   ├── supplemental 
│       ├── tables <--- tables that appear in the supplement
│       └── figures <--- figures that appear in the supplement
│   └── interim_results <--- interim and supporting results
│       ├── figures <--- histogram distributions for each VOC
│       ├── tables
│           ├── btex_ratio_lmer.csv <--- model results
│           ├── flag_summary.csv <--- detection flags summarized
│           └── voc_by_site_type_summary.csv 
│       └── pca_results <--- result objects from R/04_run_pca.R
├── thriveair_analysis.Rproj
└── README.md
```

### Code Details

| Folder | File Name | Details |
|-------|----------------|--------------------------------------------------|
| R | 00_plot_theme | Create basemap and ggplot theme to be sourced in all other code dealing with mapping and/or visualization. |
| R | 01_clean_data | Clean raw data, link data to site information and source classes. |
| R | 02_summarize_data | Summary statistics, distribution plots, and reliability calculations |
| R | 03_btex_ratios | BTEX ratio analysis an visualization |
| R | 04_run_pca | Principal component analysis (PCA), primary analysis and sensitivity analyses. Saves interim results. |
| R | 05_summarize_pca_results | Tabulate, plot, and map PCA results created in 04_run_pca. |

## Contact & Links

[Contact:]{.underline}\
Lisa Frueh \| [lfrueh.com](#0) \| lf649 at drexel.edu

[Project link:]{.underline}\
[https://github/com/lfrueh/thriveair_analysis](#0)

[More about THRIVEair:]{.underline}\
[https://thriveairphilly.com/](#0){.uri}

[Interactive data dashboard:]{.underline}\
[https://lisa-frueh.shinyapps.io/THRIVEair_results/](#0){.uri}
