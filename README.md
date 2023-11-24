# Energy Time Series Analysis

## Project Overview
Our project centers around the preprocessing of a time series dataset of energy blend prices and volumes, and calculating time series distances. The dataset includes UnixTime, Price, TimeInserted, EntryID, and Volume, from which we have extracted and regularized data for in-depth analysis.

## Time Series Preprocessing
We've employed the TSAT package's ReadTS function for data ingestion and performed a thorough cleaning using dplyr's suite of functions. By arranging, grouping, and summarizing, we processed UnixTime entries to select higher EntryID values, compute price averages, and handle NaNs as dictated by our project criteria.

### Regularizing Time Series
The data is grouped by date and summarized to establish daily opening and closing prices and volumes. We've converted UnixTime to UTC dates to regularize the time series data, facilitating comparison and analysis.

### Time Series Distances
A custom `msmDist` function was crafted to compute Move-Split-Merge (MSM) distances between time series data, which will be pivotal for the project's subsequent analytical phase.

## Analysis Pipeline
Our pipeline's key operations include:

- Conversion of Unix timestamps to UTC dates.
- Daily data aggregation for a detailed look at market movement.
- Processed data output into CSV and TS files, adhering to the project's specifications.

![tables](/images/tables.png)

## Visualization
Interactive visualizations of daily opening and closing prices are generated using `plot_ly`, enabling a dynamic exploration of price fluctuations over time.

![graphs](/images/graphs.png)

## Usage
To replicate our analysis:

1. Place the 110MB data file named "Energy_blend_rawdata2784724.ts" in the root project folder.
2. Set up the R environment with necessary packages by running "setup.R".
3. Execute "main.R" to start the analysis pipeline.

## Output
Processed datasets and visualizations are saved in the output directory, ready for any further analytical processes or reviews.

## Contributors
This project is a collective effort by:

- Elmo Primmer
- Eva Kvindt
- Hamidreza Khoshvaghti
- Yasmine Bettaieb
- Ugur Taysi
