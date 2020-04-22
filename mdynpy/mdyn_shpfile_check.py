import pandas as pd
import geopandas as gpd
import sys

df = gpd.read_file(sys.argv[1])
#with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
#    print(df)
print(df.columns)
print(df.head())
print(df[df['CD_GEOCMI']=='43777'])

