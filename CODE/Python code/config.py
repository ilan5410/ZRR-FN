"""
Configuration file for Python data processing scripts.
All paths are relative to the project root.
"""
import os
from pathlib import Path

# Detect project root (parent of CODE folder)
SCRIPT_DIR = Path(__file__).resolve().parent
CODE_DIR = SCRIPT_DIR.parent
PROJECT_ROOT = CODE_DIR.parent

# Define paths
PATHS = {
    'raw_data': PROJECT_ROOT / "DATA" / "raw data",
    'processed_data': PROJECT_ROOT / "DATA" / "processed data",
    'shapefile': PROJECT_ROOT / "DATA" / "raw data" / "communes-20220101-shp" / "communes-20220101.shp",
    'zrr_file': PROJECT_ROOT / "DATA" / "raw data" / "ZRR.csv",
}

# Analysis parameters
PARAMS = {
    'year': 1995,
    'crs_epsg': 32633,
    'n_permutations': 100,
}

def get_path(key):
    """Get path as string for compatibility with pandas/geopandas."""
    return str(PATHS[key])

def standardize_commune_codes(series):
    """Return commune codes in the legacy unpadded format used by the R pipeline."""
    codes = series.astype(str).str.strip().str.replace(r"\.0$", "", regex=True)
    codes = codes.str.lstrip("0")
    return codes.where(codes != "", None)

def verify_paths():
    """Verify that required paths exist."""
    missing = []
    for key, path in PATHS.items():
        if not path.exists():
            missing.append(f"{key}: {path}")
    if missing:
        print("WARNING: Missing paths:")
        for m in missing:
            print(f"  - {m}")
        return False
    print("All paths verified.")
    return True

if __name__ == "__main__":
    print(f"Project root: {PROJECT_ROOT}")
    verify_paths()
