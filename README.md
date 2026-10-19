# Renewable Energy vs. CO₂ Emissions Across the World

An interactive R Shiny dashboard for the Advanced Data Visualization project.

**Research question:** Do countries with higher renewable energy shares tend to have lower CO₂ emissions per person?

> The dashboard shows association, not causation.

---

## Overview

The dashboard explores country-level data on renewable energy share and per-capita CO₂ emissions. It combines coordinated interactive views with reactive filters, statistics calculated from the exact observations on screen, and notes about data quality and method.

Everything runs in memory inside R. The app reads one CSV file and writes no output files.

## Dataset

| Column | Description |
|---|---|
| `country` | Country name |
| `year` | Observation year |
| `iso_code` | ISO-3 country code (used to join map geometry) |
| `renewables_energy_share_pct` | Renewable energy share (%) |
| `co2_per_capita` | CO₂ emissions per person (assumed tonnes per person) |

- Cleaned and merged file: `renewable_energy_co2_cleaned.csv`
- Source: [add your original data sources, e.g. Our World in Data]
- The app never modifies the dataset. Missing values are kept wherever at least one indicator is present, and each statistic uses only the observations it needs (paired values for correlation).

## Features

| Tab | What it does |
|---|---|
| **Overview** | Reactive KPI cards (countries, valid country-year observations, period, mean renewable share, median CO₂, Pearson r, data completeness), an automatic cautious summary, and a data-quality report |
| **World map** | Choropleth with real country boundaries joined by ISO-3 code, hover tooltips, selectable colour scale, and a note on unmatched or missing countries |
| **Scatter & regression** | Renewable share vs CO₂ per capita with OLS line, 95% confidence band, Pearson and Spearman correlations, p-values, slope, R², country search, and click-to-select |
| **Trends** | Aggregate, regional, or country-level yearly trends in aligned panels, an index (=100) view, optional labelled LOESS smoothing, and an annual correlation panel |
| **Hexbin map** | Equal-area hexagonal grid in a projected CRS. Each cell shows the area-weighted mean of the country values it intersects |
| **Rankings** | Top/bottom N countries for either indicator, a four-panel overview, and a searchable, sortable table |
| **Change analysis** | Within-country change in renewable share vs CO₂ between two chosen years (the additional visualization) |
| **Methodology** | Definitions, calculation rules, and limitations |

### Global controls

- Year range or single-year mode
- Shared country selection, which can highlight or filter
- Indicator selector and map colour scale
- Top/bottom ranking selector and number of countries
- Minimum paired years per country
- Log scale for CO₂ (non-positive values are excluded and counted)
- Reset-all button, active-filter summary, and light/dark theme toggle

## Requirements

- R 4.1 or later
- RStudio (recommended)

| Package | Purpose |
|---|---|
| `shiny`, `bslib` | App framework, layout, cards, theming |
| `plotly` | Interactive charts |
| `ggplot2` | Plot support |
| `dplyr` | Data handling |
| `DT` | Searchable, sortable tables |
| `sf` | Spatial data and hexagonal grid |
| `rnaturalearth`, `rnaturalearthdata` | Country boundaries (in memory) |
| `scales`, `htmltools` | Formatting and HTML helpers |
| `shinycssloaders` (optional) | Loading spinners |

Install them with:

```r
install.packages(c("shiny", "bslib", "plotly", "ggplot2", "dplyr", "DT", "sf",
                   "rnaturalearth", "rnaturalearthdata", "scales", "htmltools",
                   "shinycssloaders"))
```

## How to run

1. Clone or download this repository.
2. Place `renewable_energy_co2_cleaned.csv` where the script expects it, or edit the path near the top of the script:
```r
   DATA_PATH <- "D:/ADVT_PROJECT/renewable_energy_co2_cleaned.csv"
```
3. Open `Data_vis_1.R` in RStudio and click **Run App**, or run:
```r
   shiny::runApp("Data_vis_1.R")
```
4. Use **Open in Browser** in the Viewer for more space.

## Project structure

```
.
├── Data_vis_1.R                        # complete dashboard (single script)
├── renewable_energy_co2_cleaned.csv    # cleaned dataset (if you choose to include it)
└── README.md
```

## Methodology (short version)

- **Unit of observation:** one country-year (ISO-3 code + year). Duplicate keys are averaged and reported.
- **Correlation:** Pearson r (linear) and Spearman ρ (rank) with two-sided tests on paired observations. OLS slope with a 95% confidence band for the mean fit. Nothing is reported when n < 3 or a variable is constant.
- **Maps and rankings:** show one year at a time. Years are never mixed within a ranking or map.
- **Trends:** mean renewable share and median CO₂ per capita over the countries available in each year. The data are unbalanced, so composition can change over time.
- **Hexbin map:** equal-area hexagons in a projected coordinate system. The source data has one value per country, so each cell takes the area-weighted mean of intersecting country values. Country centroids are not used.
- **Causation:** correlation does not imply causation. Income, climate, industry, and data quality can influence both indicators.

## Limitations

- Data are country-level, so the hexbin map summarizes country values over land area. It does not show sub-national measurements, and cells may straddle borders.
- Natural Earth boundaries do not cover every small territory. Unmatched countries are listed in the map note.
- Regions are Natural Earth continents, not a custom grouping.
- P-values for pooled country-years are optimistic because repeated observations from one country are not independent.
- The CO₂ unit is assumed to be tonnes per person. Confirm this against your data source.

## Troubleshooting

- **"The dashboard could not start":** check `DATA_PATH`, make sure the file is not open in Excel, and confirm the five required column names.
- **Maps not displaying:** open the app in a browser instead of the RStudio Viewer, and check the last bullet of *Overview → Data quality & coverage* to see whether map geometry loaded. [Update this section once you have resolved the map issue.]
- **Hexbin map is slow the first time:** the grid is built once per cell size and cached in memory.

## Author

[Your name] · [Course / University] · [Year]

## License

[Choose a license, e.g. MIT, or remove this section]

## Acknowledgements

- Country boundaries: [Natural Earth](https://www.naturalearthdata.com/) via the `rnaturalearth` package
- [Add your data source citation]
