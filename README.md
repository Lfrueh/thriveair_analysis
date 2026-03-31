## Introduction

This repository corresponds to the paper titled “Volatile organic
compound source apportionment in a fenceline community following
THRIVEair, a community-responsive air monitoring network.” You can learn
more about THRIVEair at: <https://thriveairphilly.com/>

### Abstract

**Background:** Urban exposure to volatile organic compounds (VOCs) is
associated with several adverse health outcomes, including cancers and
respiratory outcomes. We previously partnered with Philly Thrive, a
Philadelphia-based environmental justice organization, to co-design a
one-year VOC monitoring campaign in South Philadelphia. We aimed to
quantify air quality impacts of traffic and redevelopment of a former
oil refinery site. Though our community-focused research dissemination
focused on benzene, a primary pollutant of concern, we also
characterized additional VOCs. Here, we present source apportionment
analyses based on the extended suite of VOC measurements.

**Methods:** Using passive thermal desorption tubes, we took one-week
integrated samples of 38 VOCs across 20 monitoring sites from July 2023
– June 2024. We summarized concentrations of VOCs across land use types
and traffic density. To identify potential sources, we calculated the
toluene:benzene (T/B) ratio and (*m,p*)-xylenes:ethylbenzene (X/E)
ratios. We employed PMF to identify and apportion potential VOC sources
and principal component analysis (PCA) as a secondary analysis.

**Results:** Generally, VOC concentrations were higher in industrial,
vs. non-industrial, and high-traffic, vs. low-traffic areas. Benzene, of
community concern, was found in low concentrations (mean 1.34 µg/m³;
range: 0.31-9.05 µg/m³). We identified (*m,p*)-xylenes as a potential
pollutant of concern in industrial areas. T/B and X/E ratios were higher
among industrial and high-traffic sites, suggesting fresh emissions of
benzene derivatives. PMF revealed five VOC sources: (1) vehicular
exhaust, (2) petroleum/gasoline evaporation, (3) industrial solvents,
(4) background pollution, and (5) mixed industry suggestive of auto
repair.

**Significance:** Using a community-responsive study design, we
characterized ambient VOCs in a pollution-burdened area of Philadelphia.
Source apportionment confirmed traffic as a primary VOC source in the
area and identified the former refinery area as a source of gasoline and
petroleum evaporative emissions. We further identified industrial
solvent use as a contributor to VOC pollution.

## File Structure

### Directory Tree

    .
    ├── data
    │   ├── codebook.xlsx 
    │   ├── site_info.csv 
    │   ├── raw
    │   ├── clean 
    │   └── shp <--- shapefiles for mapping parcels
    ├── R (see details table below)
    │   ├── 00_plot_theme.R
    │   ├── 01_prepare_site_info.R
    │   ├── 02_prepare_data.R
    │   ├── 03_summarize_data.R
    │   ├── 04_btex_ratios.R
    │   ├── 05_run_pca.R
    │   └── 05_summarize_pca_pmf_results.R
    ├── results
    │   ├── tables 
    │   ├── figures 
    │   ├── supplemental 
    │       ├── tables 
    │       └── figures 
    │   └── interim_results 
    │       ├── figures 
    │       ├── tables
    │       ├── pmf_results 
    │       └── pca_results 
    ├── thriveair_analysis.Rproj
    └── README.md

### Code Details

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Folder</th>
<th>File Name</th>
<th>Details</th>
</tr>
</thead>
<tbody>
<tr>
<td>R</td>
<td>00_plot_theme</td>
<td>Create basemap and ggplot theme to be sourced in all other code
dealing with mapping and/or visualization.</td>
</tr>
<tr>
<td>R</td>
<td>01_prepare_site_info</td>
<td>Clean and prepare site information.</td>
</tr>
<tr>
<td>R</td>
<td>02_clean_data</td>
<td>Clean raw data, link data to site information and source
classes.</td>
</tr>
<tr>
<td>R</td>
<td>03_summarize_data</td>
<td>Summary statistics, distribution plots, and reliability
calculations</td>
</tr>
<tr>
<td>R</td>
<td>04_btex_ratios</td>
<td>BTEX ratio analysis an visualization</td>
</tr>
<tr>
<td>R</td>
<td>05_run_pca</td>
<td>Principal component analysis (PCA) sensitivity analysis. Saves
interim results.</td>
</tr>
<tr>
<td>R</td>
<td>06_summarize_pca_pmf_results</td>
<td>Tabulate, plot, and map PCA and PMF results.</td>
</tr>
</tbody>
</table>

## Contact & Links

**Contact:**  
Lisa Frueh | [lfrueh.com](https://lfrueh.com) | lf649 at drexel.edu Lisa
Frueh | [lfrueh.com](https://lfrueh.com) | <lf649@drexel.edu>

**Project Link:**  
<https://github.com/lfrueh/thriveair_analysis>

**More About THRIVEair:**  
<https://thriveairphilly.com/>

**Interactive data dashboard:**  
<https://lisa-frueh.shinyapps.io/THRIVEair_results/>
