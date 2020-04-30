import pandas as pd
import geopandas as gpd
import numpy as np
import sys

df = gpd.read_file(sys.argv[1])
#with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
#    print(df)

print(df)
df.CD_GEOCMU=df.CD_GEOCMU.astype(int)

df2 = pd.read_csv(sys.argv[2])
df2.CD_GEOCMU=df2.CD_GEOCMU.astype(int)

df=df.sort_values(by=['CD_GEOCMU'])
#df=df.set_index('CD_GEOCMU')
print(df)
df2=df2.sort_values(by=['CD_GEOCMU'])
#df2=df2.set_index('CD_GEOCMU')
print(df2)


df3 = pd.merge(df2, df, on='CD_GEOCMU')

df3.to_csv("maps/br_municipios/br_mun_with_uf.csv")
print("done csv")

df3 = gpd.GeoDataFrame(df3, geometry='geometry')
print(df3)
df3.to_file('maps/br_municipios/br_mun_with_uf.shp', driver='ESRI Shapefile')
print("done shp")



