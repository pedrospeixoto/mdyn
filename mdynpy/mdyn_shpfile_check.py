import pandas as pd
import geopandas as gpd
import numpy as np
import sys

df = gpd.read_file(sys.argv[1]) #Shape file municipios br with uf
print(df)
print(df.columns)

df.rename(columns={'nome':'regiao'}, inplace=True)

df3 = gpd.GeoDataFrame(df, geometry='geometry')
print(df3)
print(df3.columns)
df3.to_file('maps/regioes_2010/regioes_2010_label.shp', driver='ESRI Shapefile')

#br_municipios - convert to int codes
#df.CD_GEOCMU=df.CD_GEOCMU.astype(int)

sys.exit()


df2 = pd.read_csv(sys.argv[2]) #uf_vs_regions
#df2.CD_GEOCMU=df2.CD_GEOCMU.astype(int)
print(df2)



state_reg = dict(zip(df2.state, df2.regiao))
print(state_reg)

df['regiao']= df['Nome_UF'].map(state_reg)

print(df)

df3 = gpd.GeoDataFrame(df, geometry='geometry')
print(df3)
print(df3.columns)
df3.to_file('maps/br_municipios/br_mun_with_uf_region.shp', driver='ESRI Shapefile')
print("done shp")

sys.exit()


#df=df.sort_values(by=['CD_GEOCMU'])
#df=df.set_index('CD_GEOCMU')
#print(df)
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



