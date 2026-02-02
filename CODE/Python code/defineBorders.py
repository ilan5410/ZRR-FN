import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
from shapely.ops import unary_union
import shapely
from shapely.geometry import MultiPolygon, LineString
from shapely.ops import nearest_points
import matplotlib.pyplot as plt
from libpysal.weights import Queen
from config import get_path, PARAMS

tqdm.pandas()

if __name__ == '__main__':

    pathRawData = get_path('raw_data') + "/"
    pathProcessedData = get_path('processed_data') + "/"

    # load shp file (communes)
    pathSHP = get_path('shapefile')
    dfSHP = gpd.read_file(pathSHP)
    dfSHP["codecommune"] = dfSHP["insee"].astype(str)
    # dfSHP['codecommune'] = dfSHP['codecommune'].str.lstrip('0')
    dfSHP.drop(["insee", "wikipedia", "surf_ha"], axis=1, inplace=True)

    # load ZRR data (for now, only year 1995)
    pathZRR = pathRawData + "ZRR.csv"
    dfZRR = pd.read_csv(pathZRR)
    dfZRR = dfZRR[dfZRR.year == 1995]
    dfZRR["codecommune"] = dfZRR["codecommune"].astype(str)
    # dfZRR['codecommune'] = dfZRR['codecommune'].str.lstrip('0')
    dfZRR.drop(["nom", "treatmentLong"], axis=1, inplace=True)

    # merge both
    gdf = dfZRR.merge(dfSHP, on=["codecommune"], how="inner")
    print(gdf.shape)
    gdf = gdf.sort_values("codecommune")
    # Add an index column if not already present
    gdf = gdf.reset_index()

    # If 'gdf' is not recognized as a GeoDataFrame, explicitly convert it
    if not isinstance(gdf, gpd.GeoDataFrame):
        gdf = gpd.GeoDataFrame(gdf)

    gdf = gdf.to_crs(epsg=32633)

    # Separate treated and untreated communes
    treated = gdf[gdf['treatment'] == 1]
    untreated = gdf[gdf['treatment'] == 0]

    # Create a union of all treated geometries and all untreated geometries
    union_treated = treated.geometry.unary_union
    union_untreated = untreated.geometry.unary_union

    # Find the frontier by intersecting the boundaries of treated and untreated unions
    frontier = union_treated.boundary.intersection(union_untreated.boundary)

    # Ensure frontier is a single LineString for simplicity in distance calculations
    if isinstance(frontier, MultiPolygon):
        frontier = frontier.boundary

    gdf['centroid'] = gdf.geometry.centroid

    # Create a spatial index for the frontier
    frontier_gdf = gpd.GeoDataFrame(geometry=[frontier], crs=gdf.crs)
    frontier_sindex = frontier_gdf.sindex

    # Prepare an empty column for distances
    gdf['distance_to_border'] = np.nan



    def calculate_distances(row):
        # Find the nearest points between the commune's centroid and the frontier
        nearest_point = nearest_points(row['centroid'], frontier)[1]  # The second point is on the frontier

        # Calculate the distance from the centroid to this nearest point on the frontier
        min_distance = row['centroid'].distance(nearest_point)

        # Make the distance negative if the commune is treated
        return -min_distance if row['treatment'] == 1 else min_distance




    # Create 'border' dummy: 1 if commune boundary intersects buffered frontier
    print("Tagging border communes...")

    # Build spatial contiguity weights (Queen contiguity = shares border or vertex)
    w = Queen.from_dataframe(gdf)

    # Create border dummy
    def is_border(i, treatment_vec, neighbors_dict):
        own_treatment = treatment_vec[i]
        for neighbor in neighbors_dict[i]:
            if treatment_vec[neighbor] != own_treatment:
                return 1
        return 0

    # Ensure gdf index matches the weights matrix
    gdf = gdf.reset_index(drop=True)
    neighbors_dict = w.neighbors
    treatment_vec = gdf['treatment'].tolist()
    gdf['border'] = [is_border(i, treatment_vec, neighbors_dict) for i in range(len(gdf))]

    # Apply the function vectorized over the rows
    print("Let's compute distances")
    gdf['distance_to_border'] = gdf.progress_apply(calculate_distances, axis=1)

    # export
    dfExport = gdf.drop(["geometry", "centroid"], axis=1)
    dfExport.to_excel(pathProcessedData + "dataGeoRDD1.xlsx")
