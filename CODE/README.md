# CODE - Analysis Pipeline

This folder contains all the code necessary to reproduce the analysis for the paper **"ZRR Program and Populist Vote"**.

## Quick Start

1. **Set up R environment**
   ```r
   # Install pacman if not already installed
   install.packages("pacman")
   ```

2. **Set working directory** to the project root folder (parent of CODE/)
   ```r
   setwd("/path/to/THESIS_REPRO February 2026")
   ```

3. **Run the complete pipeline**
   ```r
   source("CODE/master.R")
   ```

## Folder Structure

```
CODE/
├── master.R                 # Main script - runs entire pipeline
├── configurations.R         # Paths, parameters, variable labels
├── README.md               # This file
│
├── Python code/            # Python scripts for geographic data processing
│   ├── master.py           # Master script - runs all Python preprocessing
│   ├── config.py           # Shared configuration (paths, parameters)
│   ├── requirements.txt    # Python dependencies
│   ├── defineBorders.py    # Creates dataGeoRDD1.xlsx (ZRR border distances)
│   ├── border_pair.py      # Creates border_pair.xlsx (matched pairs)
│   ├── distAgglo.py        # Creates distAgglo.xlsx (distance to agglomerations)
│   ├── defineBordersNotEpicenter1.py  # Creates dataGeoRDD_noEpicenter.xlsx
│   └── defineBorders_randomZRR.py     # Creates randomized treatment data (slow)
│
├── prepare data/           # R scripts for data preparation
│   ├── prepare_data.R      # Master data preparation script
│   ├── main.R              # Main data merge
│   ├── dataDes.R           # Descriptive statistics data
│   ├── script_sharp.R      # Sharp RDD data
│   ├── borders_pair.R      # Border pair analysis data
│   └── ...
│
├── prepare tables/         # R scripts for table generation
│   ├── prepare_tables.R    # Master table script
│   ├── descriptive_statistics.R
│   ├── DID_results.R
│   ├── main_results_diff_specifications.R
│   └── ... (19 table scripts total)
│
└── prepare figures/        # R scripts for figure generation
    ├── prepare_figures.R   # Master figure script
    ├── map_ZRR.R
    ├── RDD_outcomes.R
    └── ... (25 figure scripts total)
```

## Pipeline Execution Order

The analysis runs in three stages:

### Stage 1: Python Pre-processing (Run Once)
Run the Python master script from the project root:
```bash
cd "/path/to/THESIS_REPRO February 2026"
python "CODE/Python code/master.py"
```

This runs (in order):
1. `defineBorders.py` → Creates `dataGeoRDD1.xlsx`
2. `border_pair.py` → Creates `border_pair.xlsx`
3. `distAgglo.py` → Creates `distAgglo.xlsx`
4. `defineBordersNotEpicenter1.py` → Creates `dataGeoRDD_noEpicenter.xlsx`

Optional (slow, run separately):
- `defineBorders_randomZRR.py` → Creates randomization files

### Stage 2: R Data Preparation
Executed by `prepare_data.R`:
- Merges raw data sources
- Creates analysis datasets
- Saves `.RData` files to `DATA/processed data/`

### Stage 3: R Output Generation
- `prepare_tables.R` → Generates `.tex` files in `OUTPUT/tables/`
- `prepare_figures.R` → Generates `.png` files in `OUTPUT/figures/`

## Dependencies

### R Packages
All packages are loaded via `pacman::p_load()` in `master.R`:

**Core Data Manipulation:**
- `dplyr`, `tidyr`, `readr`, `readxl`, `magrittr`, `purrr`, `stringr`

**Visualization:**
- `ggplot2`, `viridis`, `ggrepel`, `gridExtra`, `cowplot`, `patchwork`, `grid`

**Spatial Analysis:**
- `sf`, `rnaturalearth`, `rnaturalearthdata`

**Econometrics:**
- `AER`, `fixest`, `sandwich`, `plm`, `estimatr`, `multiwayvcov`, `clubSandwich`

**RDD Analysis:**
- `rdd`, `rddtools`, `rdrobust`, `rddensity`

**Machine Learning:**
- `caret`, `htetree`, `glmnet`, `rpart`, `randomForest`, `SuperLearner`, `xgboost`

**Tables & Reporting:**
- `stargazer`, `knitr`, `kableExtra`, `modelsummary`, `broom`

**Other:**
- `zoo`, `ragg`, `forcats`, `reshape2`, `psych`, `MatchIt`, `devtools`, `progress`, `scales`, `here`

### Python Packages
Required for geographic data processing:
- `pandas`
- `numpy`
- `geopandas`
- `shapely`
- `matplotlib`
- `libpysal`
- `tqdm`

Install via:
```bash
pip install pandas numpy geopandas shapely matplotlib libpysal tqdm
```

## Configuration

All paths, parameters, and variable labels are defined in `configurations.R`:

- **Paths**: Automatically constructed from `main_path`
- **Bandwidths**: `c(20000, 10000, 5000)` meters for RDD analysis
- **Control variables**: 26 socioeconomic and demographic controls
- **Labels**: Human-readable variable names for tables/figures

## Output

After running `master.R`:

- **Tables**: 19 `.tex` files in `OUTPUT/tables/`
- **Figures**: 25 `.png` files in `OUTPUT/figures/`

## Troubleshooting

### "Could not find CODE folder" error
Make sure your working directory is set to the project root:
```r
setwd("/path/to/THESIS_REPRO February 2026")
```

### Package installation errors
Install packages manually if pacman fails:
```r
install.packages(c("dplyr", "ggplot2", "stargazer", ...))
```

### Memory issues
Some scripts (especially causal forest) require significant memory. Close other applications and consider running specific scripts individually rather than the full pipeline.

## Notes

- Python scripts contain hardcoded paths that may need adjustment
- Full pipeline takes approximately 15-30 minutes to run
- Intermediate `.RData` files are saved in `DATA/processed data/`
