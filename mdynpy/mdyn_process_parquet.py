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
    parquetDF = parquetDF.withColumn("first_event", F.struct(
        F.col("first_event.lat").alias("lat0"), 
        F.col("first_event.lng").alias("lng0"),
        F.col("first_event.timestamp").alias("timestamp0"),
        F.col("first_event.state").alias("state0")))
    parquetDF = parquetDF.withColumn("second_event", F.struct(
        F.col("second_event.lat").alias("lat1"), 
        F.col("second_event.lng").alias("lng1"),
        F.col("second_event.timestamp").alias("timestamp1"),
        F.col("second_event.state").alias("state1")))
    parquetDF = parquetDF.drop(parquetDF.third_event)
    #parquetDF.printSchema()
    
    #Flatten data
    parquetDF = parquetDF.select("first_event.*", "second_event.*")
    #parquetDF.printSchema()
    #parquetDF.show()
    
    #Filter if required

    #Filter state
    #parquetDF = parquetDF.filter(parquetDF.state0 == mex.state_abrv2name.get(state_abrv))

    #Restruture dates
    parquetDF = parquetDF.withColumn('date0',F.from_unixtime(F.col('timestamp0')/1000,"yyyy-MM-dd") )
    parquetDF = parquetDF.withColumn('date1',F.from_unixtime(F.col('timestamp1')/1000,"yyyy-MM-dd") )
    parquetDF = parquetDF.withColumn('timestamp0',F.to_timestamp(F.from_unixtime(F.col('timestamp0')/1000) ))
    parquetDF = parquetDF.withColumn('timestamp1',F.to_timestamp(F.from_unixtime(F.col('timestamp1')/1000) ))
    parquetDF.printSchema()
    parquetDF.show()
    
    #Write to files
    print("Writting new parquet files.")
    parquetDF.write.partitionBy("state0", "date0").mode("append").parquet(out_dir)
    #print("Writting new csv files.")
    #parquetDF.write.partitionBy("state0", "date0").mode("append").csv(out_dir)