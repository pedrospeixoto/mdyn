#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import numpy as np
import pandas as pd
import math

import pyarrow.orc as orc
import pyarrow.parquet as pq

from pyspark.sql import SQLContext
from pyspark.sql import SparkSession
import pyspark.sql.functions as F

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

#Garbage collection
import gc

#Input parameters - dir name
#-----------------------------
data_dir = sys.argv[1]
out_dir = sys.argv[2]

#Ensure we have the "/"
if data_dir[-1]!="/":
    data_dir = data_dir+"/"

#Ensure we have the "/"
if out_dir[-1]!="/":
    out_dir = out_dir+"/"

print("Processing data from      :", data_dir )
print("Processed data will be in :", out_dir )

if not os.path.exists(data_dir):
    print( " Could not reach directory, stopping here.")
    sys.exit(0)

print()
for i, filename in enumerate(os.listdir(data_dir)):
    print(" Processing : ", filename )
    #fsize = os.stat(data_dir+filename).st_size
    #if fsize > 10e7:
    #    print("Big file!", data_dir+filename, fsize)
    pq_file = pq.ParquetFile(data_dir+filename)
        
    # initialise sparkContext
    spark = SparkSession.builder \
        .master('local') \
        .appName('mdyn') \
        .config('spark.executor.memory', '5gb') \
        .config("spark.cores.max", "6") \
        .getOrCreate()
    #print(spark)
    #sc = spark.sparkContext
    #print(sc)
    # using SQLContext to read parquet file
    #sqlContext = SQLContext(sc)
    # to read parquet file 
    #df = sqlContext.read.parquet(local_dir+filename)

    #Read parquet file
    parquetDF = spark.read.parquet(data_dir+filename)
    parquetDF.show()
    parquetDF.printSchema()
    #Rename/restruture dataframe
    parquetDF = parquetDF.withColumn("house_location", F.struct(
        F.col("house_location.lat").alias("lat0"), 
        F.col("house_location.lng").alias("lng0")))
    #parquetDF.printSchema()
    
    #Flatten data
    parquetDF = parquetDF.select("house_location.*", "day", 
            "left_home", "active_users_in_month")

    #parquetDF.printSchema()
    #parquetDF.show()

    #Restruture dates
    parquetDF.printSchema()
    parquetDF.show()
    
    #Write to files
    print("Writting new parquet files.")
    parquetDF.write.partitionBy("day").mode("append").parquet(out_dir)

    print("Writting new csv files.")
    parquetDF.write.partitionBy("day").mode("append").csv(out_dir)
