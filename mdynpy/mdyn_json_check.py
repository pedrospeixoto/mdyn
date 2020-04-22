import pandas as pd
import geopandas as gpd
import sys
import json
import topojson
from pandas.io.json import json_normalize




#with open(sys.argv[1]) as json_file:
#    data = json.load(json_file)

df = pd.read_json(sys.argv[1])

df2=json_normalize(df['mesorregiao'])

df=df.join(df2, rsuffix='_meso')
print(df)
df.to_csv("maps/population/mesoregiao.csv")

