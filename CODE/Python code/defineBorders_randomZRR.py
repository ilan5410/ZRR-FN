import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.ops import unary_union
from shapely.strtree import STRtree
import os
import logging
import time
from joblib import Parallel, delayed
from tqdm import tqdm
from config import get_path, PARAMS, PROJECT_ROOT

# Configuration constants (using shared config)
CONFIG = {
    'main_path': str(PROJECT_ROOT) + "/",
    'raw_data_path': get_path('raw_data') + "/",
    'processed_data_path': get_path('processed_data') + "/",
    'shapefile_dir': "communes-20220101-shp",
    'shapefile_name': "communes-20220101.shp",
    'zrr_file': "ZRR.csv",
    'canton_file': "france1999.dbf",
    'year': PARAMS['year'],
    'output_subdir': "dataGeoRDD_canton_random/",
    'crs_epsg': PARAMS['crs_epsg'],
    'n_permutations': PARAMS['n_permutations'],
    'simplify_tolerance': 200  # Meters, adjust based on accuracy needs
}

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(os.path.join(CONFIG['processed_data_path'], "processing.log")),
        logging.StreamHandler()
    ]
)


def load_and_preprocess_data():
    """Load and preprocess shapefile, ZRR, and canton data."""
    logging.info('Loading shape file...')
    shp_path = os.path.join(CONFIG['raw_data_path'], CONFIG['shapefile_dir'], CONFIG['shapefile_name'])
    dfSHP = gpd.read_file(shp_path)
    dfSHP["codecommune"] = dfSHP["insee"].astype(str).str.lstrip('0')
    dfSHP.drop(["insee", "wikipedia", "surf_ha"], axis=1, inplace=True)

    logging.info('Loading ZRR data...')
    zrr_path = os.path.join(CONFIG['raw_data_path'], CONFIG['zrr_file'])
    dfZRR = pd.read_csv(zrr_path)
    dfZRR = dfZRR[dfZRR.year == CONFIG['year']]
    dfZRR["codecommune"] = dfZRR["codecommune"].astype(str).str.lstrip('0')
    dfZRR.drop(["nom", "treatmentLong"], axis=1, inplace=True)

    logging.info('Loading canton data...')
    canton_path = os.path.join(CONFIG['raw_data_path'], CONFIG['canton_file'])
    dfCanton = gpd.read_file(canton_path)
    dfCanton["codecommune"] = (dfCanton["DEP"].astype(str) + dfCanton["COM"].astype(str)).str.lstrip('0')
    dfCanton["codecanton"] = dfCanton["DEP"].astype(str) + dfCanton["CT"].astype(str)
    dfCanton["dep"] = dfCanton["DEP"].astype(str)
    dfCanton = dfCanton[['codecommune', 'codecanton', 'dep']].dropna(subset=['codecanton', 'codecommune'])

    pre_merge_len = len(dfZRR)
    dfZRR = dfZRR.merge(dfCanton[["codecommune", "codecanton"]], on=["codecommune"], how="inner")
    logging.info(f"Merged ZRR and Canton data. Dropped {pre_merge_len - len(dfZRR)} rows.")

    assert dfZRR['treatment'].isin([0, 1]).all(), "Treatment column must be binary."
    return dfSHP, dfZRR


def validate_input_data(dfSHP, dfZRR):
    """Validate input data before processing."""
    if not dfSHP.geometry.is_valid.all():
        raise ValueError("Invalid geometries found in shapefile.")
    if not dfZRR['codecommune'].notna().all():
        raise ValueError("Missing codecommune values in ZRR data.")


def compute_permutation(i, base_gdf, unique_cantons, output_dir):
    logging.info(f"Starting permutation {i}...")
    start_total = time.time()

    gdf = base_gdf.copy()
    rng = np.random.RandomState(i)  # Isolated random state
    cantons_to_switch = rng.choice(unique_cantons, size=len(unique_cantons) // 3, replace=False)
    switch_mask = gdf['codecanton'].isin(cantons_to_switch)
    gdf['treatment'] = np.where(switch_mask, 1 - gdf['treatment'], gdf['treatment'])

    # Log treatment distribution
    n_treated = gdf['treatment'].sum()
    n_total = len(gdf)
    logging.info(f"Permutation {i}: {n_treated} treated, {n_total - n_treated} untreated out of {n_total} units")
    if n_treated == 0 or n_treated == n_total:
        logging.warning(f"Permutation {i}: All or no units treated. Skipping.")
        return

    logging.info(f"Permutation {i}: Computing frontier...")
    start_frontier = time.time()
    treated_gdf = gdf[gdf['treatment'] == 1]
    untreated_gdf = gdf[gdf['treatment'] == 0]

    treated_geom = treated_gdf.dissolve().geometry.iloc[0] if not treated_gdf.empty else None
    untreated_geom = untreated_gdf.dissolve().geometry.iloc[0] if not untreated_gdf.empty else None

    if treated_geom is None or untreated_geom is None:
        logging.warning(f"Permutation {i}: Empty treated or untreated geometry. Skipping.")
        return

    frontier = treated_geom.boundary.intersection(untreated_geom.boundary)
    frontier_time = time.time() - start_frontier
    logging.info(f"Permutation {i}: Frontier computed in {frontier_time:.2f} seconds")

    if frontier.is_empty:
        logging.warning(f"Permutation {i}: Empty frontier. Skipping.")
        return

    from shapely.geometry import GeometryCollection, LineString, MultiLineString, Point
    frontier = unary_union(frontier) if isinstance(frontier, (list, tuple, GeometryCollection)) else frontier

    logging.info(f"Permutation {i}: Computing distances...")
    start_distance = time.time()
    if isinstance(frontier, (LineString, MultiLineString)):
        frontier_points = [Point(coord) for coord in frontier.coords] if frontier.geom_type == 'LineString' else []
        if frontier.geom_type == 'MultiLineString':
            for geom in frontier.geoms:
                frontier_points.extend([Point(coord) for coord in geom.coords])
        if frontier_points:
            tree = STRtree(frontier_points)
            gdf['distance_to_border'] = gdf['centroid'].apply(
                lambda x: min([x.distance(pt) for pt in tree.query(x, max_distance=100000)[1]])
                if tree.query(x, max_distance=100000)[1] else float('inf')
            )
        else:
            logging.warning(f"Permutation {i}: No valid points in frontier. Using direct distance.")
            gdf['distance_to_border'] = gdf['centroid'].distance(frontier)
    else:
        logging.info(f"Permutation {i}: Frontier type {frontier.geom_type}. Using direct distance.")
        gdf['distance_to_border'] = gdf['centroid'].distance(frontier)

    gdf.loc[gdf['treatment'] == 1, 'distance_to_border'] *= -1
    distance_time = time.time() - start_distance
    logging.info(f"Permutation {i}: Distances computed in {distance_time:.2f} seconds")

    df_export = gdf.drop(columns=["geometry", "centroid"])
    temp_output_path = os.path.join(output_dir, f"temp_random{i}.xlsx")
    final_output_path = os.path.join(output_dir, f"dataGeoRDD_canton_random{i}.xlsx")
    try:
        df_export.to_excel(temp_output_path, index=False)
        os.rename(temp_output_path, final_output_path)
        logging.info(f"Permutation {i}: Exported to {final_output_path} in {time.time() - start_total:.2f} seconds")
    except Exception as e:
        logging.error(f"Permutation {i}: Failed to write output: {str(e)}")


def main():
    dfSHP, dfZRR = load_and_preprocess_data()
    validate_input_data(dfSHP, dfZRR)

    base_gdf = dfZRR.merge(dfSHP, on="codecommune", how="inner").sort_values("codecommune").reset_index(drop=True)
    base_gdf = gpd.GeoDataFrame(base_gdf, geometry='geometry', crs=dfSHP.crs).to_crs(epsg=CONFIG['crs_epsg'])

    logging.info(f"Simplifying geometries with tolerance {CONFIG['simplify_tolerance']} meters...")
    start_simplify = time.time()
    base_gdf['geometry'] = base_gdf['geometry'].simplify(tolerance=CONFIG['simplify_tolerance'])
    base_gdf['centroid'] = base_gdf.geometry.centroid
    logging.info(f"Geometries simplified in {time.time() - start_simplify:.2f} seconds")

    # Create lightweight DataFrame
    base_gdf = base_gdf[['codecommune', 'codecanton', 'treatment', 'geometry', 'centroid']].copy()
    base_gdf['codecommune'] = base_gdf['codecommune'].astype('category')
    base_gdf['codecanton'] = base_gdf['codecanton'].astype('category')

    unique_cantons = dfZRR['codecanton'].unique()
    logging.info(f"There are {len(unique_cantons)} unique cantons.")

    output_dir = os.path.join(CONFIG['processed_data_path'], CONFIG['output_subdir'])
    os.makedirs(output_dir, exist_ok=True)

    # Parallelized permutations with error handling
    def safe_compute_permutation(i, base_gdf, unique_cantons, output_dir):
        try:
            compute_permutation(i, base_gdf, unique_cantons, output_dir)
        except Exception as e:
            logging.error(f"Permutation {i}: Failed with error: {str(e)}")

    logging.info("Starting parallel permutations...")
    start_time = time.time()
    Parallel(n_jobs=-1, backend='loky')(
        delayed(safe_compute_permutation)(i, base_gdf, unique_cantons, output_dir)
        for i in tqdm(range(CONFIG['n_permutations']), desc="Permutations")
    )
    total_time = time.time() - start_time
    logging.info(f"Total runtime for {CONFIG['n_permutations']} permutations: {total_time:.2f} seconds")
    logging.info("All permutations completed.")

    # Run single permutation
    # compute_permutation(0, base_gdf, unique_cantons, output_dir)

    logging.info("All permutations completed.")


if __name__ == '__main__':
    main()
