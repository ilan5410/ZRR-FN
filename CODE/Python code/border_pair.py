import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
from shapely.ops import unary_union
import shapely
from shapely.geometry import MultiPolygon, LineString
from shapely.ops import nearest_points
import matplotlib.pyplot as plt
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


    # load processed data
    pathZRR = pathProcessedData + "dataGeoRDD1.xlsx"
    dfZRR = pd.read_excel(pathZRR)
    dfZRR["codecommune"] = dfZRR["codecommune"].astype(str)
    # dfZRR['codecommune'] = dfZRR['codecommune'].str.lstrip('0')

    # merge both
    gdf = dfZRR.merge(dfSHP, on=["codecommune", "nom"], how="inner")
    print(gdf.shape)
    gdf = gdf.sort_values("codecommune")

    # Add an index column if not already present
    gdf = gdf.reset_index()


    # If 'gdf' is not recognized as a GeoDataFrame, explicitly convert it
    if not isinstance(gdf, gpd.GeoDataFrame):
        gdf = gpd.GeoDataFrame(gdf)

    # Identify neighboring localities and create border pairs
    spatial_index = gdf.sindex
    border_pairs = []

    for idx, commune in tqdm(gdf.iterrows(), total=gdf.shape[0], desc="Identifying border pairs"):
        possible_matches_index = list(spatial_index.intersection(commune.geometry.bounds))
        possible_matches = gdf.iloc[possible_matches_index]
        for _, neighbor in possible_matches.iterrows():
            if commune.codecommune != neighbor.codecommune and commune.geometry.touches(neighbor.geometry):
                pair = sorted([commune.codecommune, neighbor.codecommune])
                border_pairs.append(pair)

    border_pairs_df = pd.DataFrame(border_pairs, columns=["locality1", "locality2"]).drop_duplicates()
    border_pairs_df["border_pair"] = border_pairs_df.apply(lambda row: f"{row.locality1}_{row.locality2}", axis=1)


    # First merge: using locality1
    gdf_1 = gdf.merge(border_pairs_df, left_on="codecommune", right_on="locality1", how="left")
    # Second merge: using locality2
    gdf_2 = gdf.merge(border_pairs_df, left_on="codecommune", right_on="locality2", how="left", suffixes=('', '_rev'))

    # Concatenate the two DataFrames
    gdf_combined = pd.concat([gdf_1, gdf_2], ignore_index=True)

    gdf_combined["border_pair"] = gdf_combined["border_pair"].apply(
        lambda bp: "_".join(sorted(bp.split("_"))) if pd.notnull(bp) else bp)

    # Remove duplicates if necessary
    gdf_combined.drop_duplicates(subset=["codecommune", "border_pair"], inplace=True)
    gdf_combined = gdf_combined.sort_values("codecommune")


    # Filter out pairs where both localities share the same treatment status
    gdf_combined = gdf_combined[gdf_combined.groupby('border_pair')['treatment'].transform('nunique') == 2]


    gdf_combined = gdf_combined.dropna(subset="border_pair")


    # Add department
    gdf_combined["dep"] = gdf_combined["codecommune"].str[:2]

    # Merge the DataFrame with itself to get the department information for both localities in the pair
    gdf_combined = gdf_combined.merge(gdf_combined[["codecommune", "dep"]], left_on="locality1", right_on="codecommune",
                                      suffixes=('', '_locality1'))
    gdf_combined = gdf_combined.merge(gdf_combined[["codecommune", "dep"]], left_on="locality2", right_on="codecommune",
                                      suffixes=('', '_locality2'))

    # Filter to keep only pairs where both localities are in the same department
    # gdf_combined = gdf_combined[gdf_combined["dep_locality1"] == gdf_combined["dep_locality2"]]


    # Clean up the DataFrame
    gdf_combined.drop(["locality1", "locality2"], axis=1, inplace=True)
    gdf_combined.drop_duplicates(inplace=True)

    dfExport = gdf_combined.drop(["geometry", "level_0", "Unnamed: 0", "index"], axis=1)
    dfExport.to_excel(pathProcessedData + "border_pair.xlsx")


