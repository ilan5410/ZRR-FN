import pandas as pd
import numpy as np
import geopandas as gpd
from tqdm import tqdm
from shapely.ops import unary_union
import shapely
from shapely.geometry import MultiPolygon, LineString
from shapely.ops import nearest_points
import matplotlib.pyplot as plt

tqdm.pandas()

if __name__ == '__main__':

    # load shp file (communes)
    pathSHP = "/Users/ilanpargamin/Desktop/thesis/assoExploration/data/communes-20220101-shp/communes-20220101.shp"
    dfSHP = gpd.read_file(pathSHP)
    dfSHP["codecommune"] = dfSHP["insee"].astype(str)
    dfSHP['codecommune'] = dfSHP['codecommune'].str.lstrip('0')
    dfSHP.drop(["insee", "wikipedia", "surf_ha"], axis=1, inplace=True)

    # load ZRR data (for now, only year 1995)
    pathZRR = "/Users/ilanpargamin/Desktop/thesis/zonage/ZRR.csv"
    dfZRR = pd.read_csv(pathZRR)
    dfZRR = dfZRR[dfZRR.year == 1995]
    dfZRR["codecommune"] = dfZRR["codecommune"].astype(str)
    dfZRR['codecommune'] = dfZRR['codecommune'].str.lstrip('0')
    dfZRR.drop(["nom", "treatmentLong"], axis=1, inplace=True)

    # add canton
    dfCanton = pd.read_csv(
        "/Users/ilanpargamin/Desktop/elections_papers/DATA/socio_eco/Taille_agglo_commune_csv/codescommunescantons1999.csv")
    dfCanton = dfCanton[['codecommune', 'codecanton', 'dep']].dropna(subset=['codecanton', 'codecommune'])
    dfCanton['codecommune'] = dfCanton['codecommune'].str.lstrip('0')  # Remove leading zeros from codecommune
    dfZRR = dfZRR.merge(dfCanton[["codecommune", "codecanton"]], on=["codecommune"], how="inner")

    assert dfZRR['treatment'].isin([0, 1]).all(), "Treatment column must be binary (0 or 1)."
    unique_cantons = dfZRR['codecanton'].unique()

    def calculate_distances(row):
        # Find the nearest point on the frontier to the commune's centroid
        nearest_point = nearest_points(row['centroid'], frontier)[1]  # The second point is on the frontier
        min_distance = row['centroid'].distance(nearest_point)
        return -min_distance if row['treatment'] == 1 else min_distance

    # Permutations
    for i in range(5):
        dfZRR_permuted = dfZRR.copy()
        np.random.seed(i)  # For reproducibility
        cantons_to_switch = np.random.choice(unique_cantons, size=len(unique_cantons) // 3, replace=False)
        dfZRR_permuted.loc[dfZRR_permuted['codecanton'].isin(cantons_to_switch), 'treatment'] = 1 - dfZRR_permuted['treatment']
        gdf = dfZRR_permuted.merge(dfSHP, on=["codecommune"], how="inner")
        gdf = gdf.sort_values("codecommune").reset_index(drop=True)
        if not isinstance(gdf, gpd.GeoDataFrame):
            gdf = gpd.GeoDataFrame(gdf)
        gdf = gdf.to_crs(epsg=32633)
        treated = gdf[gdf['treatment'] == 1]
        untreated = gdf[gdf['treatment'] == 0]
        union_treated = treated.geometry.unary_union
        union_untreated = untreated.geometry.unary_union
        frontier = union_treated.boundary.intersection(union_untreated.boundary)
        if isinstance(frontier, MultiPolygon):
            frontier = frontier.boundary
        gdf['centroid'] = gdf.geometry.centroid
        frontier_gdf = gpd.GeoDataFrame(geometry=[frontier], crs=gdf.crs)
        frontier_sindex = frontier_gdf.sindex
        gdf['distance_to_border'] = np.nan
        print("Calculating distances to border")
        gdf['distance_to_border'] = gdf.progress_apply(calculate_distances, axis=1)
        dfExport = gdf.drop(["geometry", "centroid"], axis=1)
        dfExport.to_excel(f"dataGeoRDD_canton_random{i}.xlsx")
