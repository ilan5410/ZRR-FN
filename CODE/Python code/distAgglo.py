import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
from shapely.ops import unary_union
from pyproj import Proj, transform
import shapely
from scipy.spatial import cKDTree
from config import get_path, PARAMS

tqdm.pandas()


# Function to determine the UTM zone number for a given longitude
def utm_zone(longitude):
    return int(1 + (longitude + 180.0) / 6.0)


if __name__ == '__main__':

    pathRawData = get_path('raw_data') + "/"
    pathProcessedData = get_path('processed_data') + "/"

    # load shp file (communes)
    pathSHP = get_path('shapefile')
    dfSHP = gpd.read_file(pathSHP)
    dfSHP["codecommune"] = dfSHP["insee"].astype(str)
    dfSHP['codecommune'] = dfSHP['codecommune'].str.lstrip('0')
    dfSHP.drop(["insee", "wikipedia", "surf_ha"], axis=1, inplace=True)

    # load pop data
    path = pathRawData + "popcommunes.csv"
    dfPop = pd.read_csv(path)
    dfPop['codecommune'] = dfPop['codecommune'].astype(str)
    dfPop['codecommune'] = dfPop['codecommune'].str.lstrip('0')
    # popCols = [i for i in dfPop if (i.startswith("pop") and len(i) < 9)]
    var = "pop1995"
    dfPop = dfPop[["codecommune", "reg"] + [var]]

    # merge both
    gdf = dfPop.merge(dfSHP, on=["codecommune"], how="inner")
    print(gdf.shape)

    # Add an index column if not already present
    gdf = gdf.reset_index()

    # If 'gdf' is not recognized as a GeoDataFrame, explicitly convert it
    if not isinstance(gdf, gpd.GeoDataFrame):
        gdf = gpd.GeoDataFrame(gdf)

    # Determine the UTM zone for the centroid of your data
    gdf = gdf.to_crs(epsg=32633)
    centroid_longitude = gdf.geometry.centroid.x.mean()
    zone_number = utm_zone(centroid_longitude)
    hemisphere = '326' if gdf.geometry.centroid.y.mean() > 0 else '327'
    utm_epsg = f"{hemisphere}{zone_number}"

    # Debug: Print the constructed EPSG code
    print(f"Constructed EPSG Code: {utm_epsg}")

    # 1. Identify agglomerations

    # Calculate the 9th decile of var
    decile_9 = gdf[var].quantile(0.99)

    # Create the agglo column with value 1 if pop1995 >= 9th decile, else 0
    gdf['agglo'] = (gdf[var] >= decile_9).astype(int)
    gdf['agglo'].value_counts()

    # 2. Distance to agglo
    # Now that we identified the agglomerations, let's compute the distance to the closest agglo for each commune

    # Calculate the centroids of the geometries
    gdf['centroid'] = gdf.geometry.centroid

    # Extract the centroids of the agglomerations
    agglo_geoms = gdf[gdf['agglo'] == 1].centroid

    # Create a cKDTree for the agglomerations' centroids
    agglo_tree = cKDTree(np.array(list(agglo_geoms.apply(lambda geom: (geom.x, geom.y)))))

    # Define a function to compute the distance to the closest agglomeration
    def compute_min_distance(geom):
        point = np.array([geom.x, geom.y])
        distance, _ = agglo_tree.query(point, k=1)
        return distance

    # Apply the function to compute distances using the centroids
    gdf['min_distance_to_agglo'] = gdf['centroid'].apply(compute_min_distance)

    # export
    dfExport = gdf.drop(["geometry", "centroid"], axis=1)
    dfExport.to_excel(pathProcessedData + "distAgglo.xlsx")
