# Phyto-MAP data viewer

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
![Last commit](https://img.shields.io/github/last-commit/RayonVina/phytaxr?style=flat-square&color=b4befe)
<!-- badges: end -->

Shiny web application for exploring the Phyto-MAP database of marine phytoplankton abundance records. 

> Status: in development, not yet deployed.

The application provides a sampling coverage map, a filterable data catalogue, a metadata dictionary, and panels for inspecting selected locations and records. 

Planned deployment: Posit Connect Cloud.

## Installation

Clone the repository and download the database from Zenodo (see below). Required R packages:

```r
install.packages(c("shiny", "DBI", "RSQLite", "leaflet", "htmltools", "DiagrammeR", "DT"))
```

The application does not install packages automatically. At startup it reports any missing package and stops.

## Database

The database is not included in this repository. Download it from Zenodo:

- Record: https://zenodo.org/records/23885976
- DOI: https://doi.org/10.5281/zenodo.23885976

Place the SQLite file at `data/phytomap.sqlite3`. The application opens it in read-only mode.

Create the `data/` directory if it does not exist, then place the downloaded SQLite file at `data/phytomap.sqlite3`.

The database is required to run the application and is not downloaded automatically.

## Usage

From the project root:

```r
shiny::runApp()
```

## Features

- **Home**: database overview counts and notes on interpreting the records.
- **Map explorer**: sampling coverage by location, filtered by source and subsource, with rectangle selection, point details (events, samples, abundance records, taxonomy, sampling protocol) and PNG export.
- **Data catalogue**: browse and export joined database views to CSV, with filters.
- **Metadata dictionary**: table and field descriptions.
- **Selected records**: bounded table of records chosen on the map, with CSV export.

## Project structure

```
app.R                 # UI definition and entry point
R/
  connection.R        # Read-only database connection
  queries.R           # SQL queries and bounded data retrieval
  server.R            # Session setup and module registration
  server_home.R       # Home overview
  server_map.R        # Map explorer
  server_catalog.R    # Data catalogue and metadata dictionary
www/                  # Custom JavaScript for filters and interactions
data/                 # Local database (not versioned)
```

## Citation

If you use this viewer or the data, please cite the database:

Rayón-Viña, F., Finkel, Z. V., & Irwin, A. J. (2026). Phyto-MAP, a curated database of marine phytoplankton abundance records (Version 2.1.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.23885976

## License

MIT, © Fernando Rayón Viña. See [LICENSE](LICENSE). The database has its own license on Zenodo.
