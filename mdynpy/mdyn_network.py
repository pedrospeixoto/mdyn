#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import math

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance
import geopandas as gpd

from shapely.geometry import Point, Polygon

import concurrent.futures

from mdynpy.mdyn_daydata import DayData
from mdynpy.mdyn_map import Map
from mdynpy.mdyn_extras import distance, distance_lat, distance_lon, daterange, matprint, round_down, round_up
from mdynpy.parhelper import parhelper


class Network:

    #cities = {}

    def __init__(self, ipar):
        print("")
        print("Mobile Dynamics Network Construction")
        print("-----------------------------------")

        domain = ipar.domain
        domain_abrv = ipar.domain_abrv
        domain_gran = ipar.domain_gran
        domain_shape = ipar.domain_shape
        domain_pop = ipar.domain_pop
        domain_pop_labels = ipar.domain_pop_labels
        subdomains = ipar.subdomains
        subdomains_gran = ipar.subdomains_gran
        subdomains_shape = ipar.subdomains_shape
        subdomains_pop = ipar.subdomains_pop
        subdomains_pop_labels = ipar.subdomains_pop_labels
        latlon_gran = ipar.latlon_gran
        load = ipar.load_network

        self.domain = domain
        self.domain_abrv = domain_abrv
        self.domain_gran= domain_gran
        self.domain_shape = domain_shape
        self.domain_pop = domain_pop
        self.domain_pop_labels = domain_pop_labels
        self.subdomains = subdomains
        self.subdomains_gran = subdomains_gran
        self.subdomains_shape = subdomains_shape
        self.subdomains_pop = subdomains_pop
        self.subdomains_pop_labels = subdomains_pop_labels
        self.latlon_gran = latlon_gran
        self.load = load

        self.parallelize = True

        self.max_workers = os.cpu_count()

        if 'MAX_WORKERS' in os.environ:
            self.max_workers = int(os.environ['MAX_WORKERS'])

            if self.max_workers <= 0:
                self.max_workers = None
                self.parallelize = False

        print("Parallization: "+str(self.parallelize))
        print("Number of workers for parallelization: "+str(self.max_workers))

        self.network_alg = 1
        if 'NETWORK_ALG' in os.environ:
            self.network_alg = int(os.environ['NETWORK_ALG'])
        print("Network algorithm: "+str(self.network_alg))

        self.safety_halo = 1
        if 'SAFETY_HALO' in os.environ:
            self.safety_halo = int(os.environ['SAFETY_HALO'])
        print("Safety halo: "+str(self.safety_halo))

        print(self.domain, self.subdomains)
        
        #Load main domain
        self.df_domain = self.load_domain()

        #Load subdomains
        self.df_subdomains = self.load_subdomains()

        #Create in/out region lists
        self.create_regions()

        #Build grid network
        self.build_grid_network()

        #Load regions' populations

        #self.load_pop()

        try:
            self.load_pop()
        except:
            self.reg_pop = np.zeros([self.nregions])
            print("Warning: No population data!!!")
            pass

    def load_domain(self):

        #Get map shapes files
        self.domain_shape_file = self.domain_shape+".shp"
        self.domain_shape_file_mdyn = self.domain_shape+"mdyn_dom.shp"

        #Load if exists and load flag set
        if os.path.exists(self.domain_shape_file_mdyn) and self.load:

            df = gpd.read_file(self.domain_shape_file_mdyn)
            print("  Domain shape file loaded.")

        else: #Build map structure with neighbours
            df = gpd.read_file(self.domain_shape_file)
            print("  Creating basic domain shape file...", end = '')

            df["NEIGHBORS"] = None  # add NEIGHBORS column
            for index, reg in df.iterrows():
                # get 'not disjoint' countries
                df_local = df[~df.geometry.disjoint(reg.geometry)]
                neighbors = df_local[self.domain_gran].tolist()

                # remove own name from the list
                neighbors = [ name for name in neighbors if reg[self.domain_gran] != name ]

                # add names of neighbors as NEIGHBORS value
                df.at[index, "NEIGHBORS"] = ",".join(neighbors)
            print(df)
            #Save modified shape file for future use
            print("Done. Saving domain shape structure for future use")
            df.to_file(self.domain_shape_file_mdyn)

        return df


    def load_subdomains(self):

        #Get map subdomains shapes files
        self.subdomains_shape_file = self.subdomains_shape+".shp"
        self.subdomains_shape_file_mdyn = self.subdomains_shape+"mdyn_subdom.shp"

        #Get map subdomains shapes
        if os.path.exists(self.subdomains_shape_file_mdyn) and self.load:
            df = gpd.read_file(self.subdomains_shape_file_mdyn)
            print("  Subdomains shape file loaded.")

        else: #Build map subdomains structure
            df = gpd.read_file(self.subdomains_shape_file)
            print("  Creating basic subdomains shape file...", end = '')

            df["lonc"] = None #long centroid
            df["latc"] = None #lat centroid
            for index, mun in df.iterrows():
                poly=mun.geometry
                if poly.geom_type == 'MultiPolygon':           
                    maxpoly=max(poly, key=lambda a: a.area)
                    df.at[index, "lonc"] = maxpoly.centroid.x
                    df.at[index, "latc"] = maxpoly.centroid.y
                else:
                    df.at[index, "lonc"] = poly.centroid.x
                    df.at[index, "latc"] = poly.centroid.y
            df["latc"]=df["latc"].astype(float)
            df["lonc"]=df["lonc"].astype(float)

            #Save modified shape file for future use
            print("Done. Saving subdomains shape structure for future use")
            df.to_file(self.subdomains_shape_file_mdyn)

        #print(df)
        #print(df.head)

        #Indexes columns
        idx = np.arange(len(df))
        df["idx"]=idx.astype(int)

        #print(df)
        #print(self.regions_in, self.regions_in_latlon)
        return df

    def create_regions(self):

        #Filter subdomains based on domain, if possible (reduces processing)
        dom_gran_tmp = self.domain_gran
        dom_gran_tmp=dom_gran_tmp.replace("NM_", "")

        filt_name = ""
        for lab in self.df_subdomains.columns.values:
            #print("Testing:", lab)
            if dom_gran_tmp in lab:
                filt_name = lab
                #print("found it:", filt_name )

        if len(filt_name) > 0:
            print("Filtering subdomain based on domain:", filt_name, self.domain)
            self.df_subdomains = self.df_subdomains[self.df_subdomains[filt_name]==self.domain]
            #Re-Indexes columns
            idx = np.arange(len(self.df_subdomains))
            self.df_subdomains["idx"]=idx.astype(int)
            self.df_subdomains = self.df_subdomains.set_index("idx")
            
        print(self.df_subdomains)

        #inner regions (subdomains)
        #--------------------------
        self.regions_in = self.df_subdomains[self.subdomains_gran].to_dict()
        filt = [self.subdomains_gran, "latc", "lonc", "idx"]
        self.regions_in_latlon = self.df_subdomains.filter(filt).set_index(self.subdomains_gran).T.to_dict('list')
        self.nreg_in = len(self.regions_in)

        if self.domain_abrv == "BRA":
            if self.subdomains == "Municip": #Save the names of cities for brasil (as the region is usually the code)
                self.regions_in_names = self.df_subdomains["NM_MUNICIP"].to_dict()
            elif self.subdomains == "microreg":
                self.regions_in_names = self.df_subdomains["NM_MICRO"].to_dict()
        else:
            if self.subdomains == "Municip": #Save the names of cities for brasil (as the region is usually the code)
                self.regions_in_codes = self.df_subdomains["CD_GEOCMU"].to_dict()
            elif self.subdomains == "microreg":
                self.regions_in_codes = self.df_subdomains["CD_GEOMI"].to_dict()
        
        #Outer regions (domain)
        #-------------------------
        try:
            df_domain_local = self.df_domain[self.df_domain[self.domain_gran] == self.domain]
        except:
            self.domain = self.domain.upper()
            df_domain_local = self.df_domain[self.df_domain[self.domain_gran] == self.domain]
        self.domain_geometry=df_domain_local.geometry.values[0]
        
        self.domain_neib = df_domain_local.NEIGHBORS.values[0]
        if self.domain_neib is not None:
            self.domain_neib = self.domain_neib.split(',')
            self.df_domain_nb=self.df_domain[self.df_domain[self.domain_gran].isin(self.domain_neib)]
            #print(self.df_domain_nb)
            self.df_domain_nb["idx"] = range(self.nreg_in, self.nreg_in+len(self.df_domain_nb), 1)
            self.df_domain_nb=self.df_domain_nb.set_index("idx")
            #print(self.df_domain_nb)
        domain_limit_coords=list(self.domain_geometry.envelope.exterior.coords)


        # Regular grid structure
        x = []
        y = []
        for xy in domain_limit_coords:
            x.append(xy[0])
            y.append(xy[1])
        minx = min(x)
        maxx = max(x)
        miny = min(y)
        maxy = max(y)
        #Create buffer on sides depending on size of domain
        if maxx-minx > 6.0:
            self.minlons = round_down(minx, 1) - 1.1
            self.maxlons = round_up(maxx, 1) + 1.1
        else:
            self.minlons = round_down(minx, 1) - 0.5
            self.maxlons = round_up(maxx, 1) + 0.5

        if maxy-miny > 6:
            self.minlats = round_down(miny, 1) - 1.1
            self.maxlats = round_up(maxy, 1) + 1.1
        else:
            self.minlats = round_down(miny, 1)- 0.5
            self.maxlats = round_up(maxy, 1) + 0.5

        self.minlons = np.round(self.minlons, 1)
        self.maxlons = np.round(self.maxlons, 1)
        self.minlats = np.round(self.minlats, 1)
        self.maxlats = np.round(self.maxlats, 1)

        #Latlon spacing
        self.dlon = self.latlon_gran
        self.dlat = self.latlon_gran

        # Number of cells within this region
        self.nlon = int(round((self.maxlons - (self.minlons))/self.dlon))
        self.nlat = int(round((self.maxlats - (self.minlats))/self.dlat))

        print("Domain Box:")
        print(" + lon (min/max): ", self.minlons, self.maxlons)
        print(" + lat (min/max): ", self.minlats, self.maxlats)
        print(" + dlon / dlat: ", self.dlon, self.dlat)
        print(" + nlon / nlat: ", self.nlon, self.nlat)

        #These are the datapoints
        self.lon_bins_c = np.linspace(self.minlons, self.maxlons, self.nlon+1, endpoint=True)
        self.lat_bins_c = np.linspace(self.minlats, self.maxlats, self.nlat+1, endpoint=True)

        #These are bins for datagrid (larger than datagrid) - Extended grid
        self.lon_bins_ext = np.linspace(self.minlons-self.dlon/2, self.maxlons+self.dlon/2, self.nlon+2, endpoint=True)
        self.lat_bins_ext = np.linspace(self.minlats-self.dlat/2, self.maxlats+self.dlat/2, self.nlat+2, endpoint=True)

        print(" + lon_bins_c", self.lon_bins_c.shape, ": ", str(self.lon_bins_c[:5]), "...", str(self.lon_bins_c[-5:]))
        print(" + lat_bins_c", self.lat_bins_c.shape, ": ", str(self.lat_bins_c[:5]), "...", str(self.lat_bins_c[-5:]))
        print(" + lon_bins_ext", self.lon_bins_ext.shape, ": ", str(self.lon_bins_ext[:5]), "...", str(self.lon_bins_ext[-5:]))
        print(" + lat_bins_ext", self.lat_bins_ext.shape, ": ", str(self.lat_bins_ext[:5]), "...", str(self.lat_bins_ext[-5:]))


        #2d grids
        self.lon_bins_c_2d, self.lat_bins_c_2d = np.meshgrid(self.lon_bins_c, self.lat_bins_c)
        self.lon_bins_ext_2d, self.lat_bins_ext_2d = np.meshgrid(self.lon_bins_ext, self.lat_bins_ext)

        #Region grid is composed of data points, cell centres
        self.region_grid = np.zeros((self.nlat+1, self.nlon+1), dtype=int)

        #Regions out are out of main domain
        if self.domain_neib is not None:
            regions_out = self.df_domain_nb[self.domain_gran].to_dict()
            reg_out = list(regions_out.values())
            self.nreg_out = len(reg_out)
        else:
            reg_out = []
            self.nreg_out = 0

        #Align indexing of out regions with in regions
        self.regions_out = {self.nreg_in+key:reg for key, reg in enumerate(reg_out)}
        #print(self.regions_out)

        #Join in and out regions
        self.regions = {**self.regions_in, **self.regions_out}
        self.nregions = len(self.regions)

        #Add extra region for all the rest (ocean or other places)
        #self.regions[str(self.nregions)] = 'NAN'
        #self.nregions = self.nregions + 1

        print("  Defined the following regions for the network:")
        if self.nregions > 50:
            print("  First regions: ", dict(list(self.regions.items())[0:20]))
            print("  Last regions:", dict(list(self.regions.items())[self.nregions-20:self.nregions]))
        else:
            print(self.regions)



    #Build regions grid
    def build_grid_network(self):

        print("Building grid network...")

        # First, create the output directory
        if not os.path.exists('maps/grids'):
            os.mkdir('maps/grids')

        self.gridname = 'maps/grids/regions_'+self.domain_abrv+"_"+\
            self.subdomains+\
            "_lats"+str(self.minlats)+"_"+str(self.maxlats)+\
            "_lons"+str(self.minlons)+"_"+str(self.maxlons)+\
            "_dlat"+str(self.dlat)+"_dlon"+str(self.dlon) #+"_alg"+str(self.network_alg)

        print(self.gridname,os.path.exists(self.gridname+".npy"), self.load, self.network_alg )
        #check if network pre built
        if os.path.exists(self.gridname+".npy") and self.load:
            self.region_grid=np.load(self.gridname+".npy")
            print("Regions loaded from file "+self.gridname)

        elif os.path.exists(self.gridname+"_alg"+str(self.network_alg)+".npy") and self.load:
            self.gridname = self.gridname+"_alg"+str(self.network_alg)
            self.region_grid=np.load(self.gridname+".npy")
            print("Regions loaded from file "+self.gridname)
        else:
            self.gridname = self.gridname+"_alg"+str(self.network_alg)

            # Initialize with -1
            self.region_grid = np.full((self.nlat+1, self.nlon+1), -1, dtype=int)


            if self.network_alg == 0:

                #
                # Original method (from Pedro)
                #

                def process_domains_by_pixel(i, j, lat, lon):
                    #print(lat, lon)
                    reg0 = self.get_closest_region(lat, lon) #This is the center of cell
                    if reg0 != -1:
                        self.region_grid[i,j]=reg0
                    else:
                        #Check if part of the cell is in a region, otherwise it is ocean
                        reg1 = self.get_closest_region(lat-self.dlat, lon-self.dlon)
                        if reg1 != -1:
                            ireg=reg1
                        else:
                            reg2 = self.get_closest_region(lat+self.dlat, lon-self.dlon)
                            if reg2 != -1:
                                ireg=reg2
                            else:
                                reg3 = self.get_closest_region(lat-self.dlat, lon+self.dlon)
                                if reg3 != -1:
                                    ireg=reg3
                                else:
                                    reg4 = self.get_closest_region(lat+self.dlat, lon+self.dlon)
                                    if reg4 != -1:
                                        ireg=reg4
                                    else:
                                        ireg = -1

                        self.region_grid[i,j] = ireg

                # Run for each latitude
                def par_exec(idx):

                    i = idx // len(self.lon_bins_c)
                    j = idx % len(self.lon_bins_c)

                    lat = self.lat_bins_c[i]
                    lon = self.lon_bins_c[j]

                    process_domains_by_pixel(i, j, lat, lon)


                from tqdm import tqdm
                if self.parallelize:
                    parhelper(len(self.lat_bins_c)*len(self.lon_bins_c), par_exec, max_workers=self.max_workers, use_tqdm=True)

                else:
                    # No progress bar if inner hasn't enough workload
                    iter_range = range(len(self.lat_bins_c)*len(self.lon_bins_c))
                    for index in tqdm(iter_range):
                        par_exec(index)

            elif self.network_alg == 1:

                #
                # Initialize with -1
                # Setup 2nd grid which is used in case that the cell centers didn't reveal a corresponding region
                #
                self.region_grid_2nd = np.full((self.nlat+1, self.nlon+1), -1, dtype=int)

                # Cache the distance of each lat/lon center point to the region for other passes

                #
                # Region-by-Region processing
                #
                def process_domains_by_regions(df_domains, par_outer=False, par_inner=False, region_dist_cache=None, center_offset=(0, 0)):

                    verbose = 0

                    # Convert to dict for nice and direct access
                    conv = {}
                    for index, reg in df_domains.iterrows():
                        conv[index] = reg

                    # Store the keys, since these are also the indices used for coloring
                    conv_keys = list(conv.keys())

                    def par_exec(i_region):
                        index = conv_keys[i_region]
                        reg = conv[index]

                        if verbose > 0:
                            if 'NM_ESTADO' in reg:
                                print("Processing NM_ESTADO region "+str(index)+": "+reg['NM_ESTADO'])
                            elif 'NM_MUNICIP' in reg:
                                print("Processing NM_MUNICIP region "+str(index)+": "+reg['NM_MUNICIP'])
                            elif 'NM_MICRO' in reg:
                                print("Processing NM_MICRO region "+str(index)+": "+reg['NM_MICRO'])
                            else:
                                print(reg)
                                print("Processing region "+str(index)+": [unknown type]")


                            print(" + boundaries (min_lon, min_lat, max_lon, max_lat): "+str(reg['geometry'].bounds))

                        # Find bounding rectangle
                        minx, miny, maxx, maxy = reg['geometry'].bounds

                        # lats
                        mini = int(np.floor((miny-self.lat_bins_c[0])/self.dlat))
                        maxi = int(np.ceil((maxy-self.lat_bins_c[0])/self.dlat))

                        # lons
                        minj = int(np.floor((minx-self.lon_bins_c[0])/self.dlon))
                        maxj = int(np.ceil((maxx-self.lon_bins_c[0])/self.dlon))

                        if verbose > 0:
                            print(" + domain res (lat,lon): "+str(len(self.lat_bins_c))+", "+str(len(self.lon_bins_c)))
                            print(" + lat idx min/max: "+str(mini)+", "+str(maxi))
                            print(" + lon idx min/max: "+str(minj)+", "+str(maxj))

                        mini -= self.safety_halo;
                        maxi += self.safety_halo;
                        minj -= self.safety_halo;
                        maxj += self.safety_halo;

                        mini = max(mini, 0)
                        minj = max(minj, 0)

                        maxi = min(maxi, len(self.lat_bins_c)-1)
                        maxj = min(maxj, len(self.lon_bins_c)-1)

                        if verbose > 0:
                            print(" + new lat idx min/max: "+str(mini)+", "+str(maxi))
                            print(" + new lon idx min/max: "+str(minj)+", "+str(maxj))

                        if region_dist_cache != None:
                            if i_region not in region_dist_cache:
                                region_dist_cache[i_region] = np.full((maxi+1-mini, maxj+1-minj), np.inf)

                        #
                        # Iterate over longitude
                        #
                        def par_exec_inner(i):
                            ci = i-mini
                            lat = self.lat_bins_c[i]

                            for j in range(minj, maxj+1):
                                cj = j-minj

                                # Skip region if it's already processed
                                if self.region_grid[i,j] != -1.0:
                                    continue

                                # Check for cached distance
                                if region_dist_cache != None:
                                    dist = region_dist_cache[i_region][ci,cj]
                                    if not np.isinf(dist):
                                        if dist > np.max(np.abs(center_offset))*2.0:
                                            continue


                                #
                                # Check for center, left, right, bottom and top points
                                #

                                lon = self.lon_bins_c[j]
                                p = Point(lon+center_offset[0], lat+center_offset[1])

                                if reg['geometry'].contains(p):
                                    # Set region index
                                    self.region_grid[i,j] = index

                                    # Set distance to 0
                                    if region_dist_cache != None:
                                        region_dist_cache[i_region][ci,cj] = 0
                                    continue

                                if region_dist_cache != None:
                                    # Update distance
                                    # WARNING: Distance computation is about 9x more expansive than geometry check
                                    dist = p.distance(reg['geometry'])
                                    region_dist_cache[i_region][ci,cj] = dist


                        if par_inner and self.parallelize:
                            f = lambda id: par_exec_inner(id + mini)
                            parhelper(maxi+1-mini, f, max_workers=self.max_workers, use_tqdm=False)

                        else:
                            # No progress bar since inner hasn't enough workload
                            #for index in tqdm(iter_range):
                            for i in range(mini, maxi+1):
                                par_exec_inner(i)


                    from tqdm import tqdm
                    if par_outer and self.parallelize:
                        parhelper(len(conv), par_exec, max_workers=self.max_workers, use_tqdm=True)

                    else:
                        for i in tqdm(range(len(conv))):
                            par_exec(i)


                    fill_rate = np.count_nonzero(self.region_grid+1.0) / self.region_grid.size
                    print("Fill rate with preprocessing: "+str(fill_rate))



                #
                # Pimped version
                #

                def task(i):
                    print(i)

                print("*"*80)
                print(" + len subdomains: "+str(len(self.df_subdomains,)))

                if self.domain_neib is not None:
                    print(" + len domain neighbors: "+str(len(self.df_domain_nb)))


                almost_one = 1.0-1e-10
                dhlon = 0.5*self.dlon*almost_one
                dhlat = 0.5*self.dlat*almost_one
                dlon = self.dlon*almost_one
                dlat = self.dlat*almost_one

                cell_offsets = [
                    # Center of cell
                    (0, 0),

                    # Edges of cell
                    (-dhlon, 0),
                    (+dhlon, 0),
                    (0, -dhlat),
                    (0, +dhlat),

                    # Corners of cell
                    (-dhlon, -dhlat),
                    (+dhlon, -dhlat),
                    (+dhlon, +dhlat),
                    (-dhlon, +dhlat),

                    # Go even further to adjacent cells
                    (-dlon, 0),
                    (+dlon, 0),
                    (0, -dlat),
                    (0, +dlat),

                    (-dlon, -dlat),
                    (+dlon, -dlat),
                    (+dlon, +dlat),
                    (-dlon, +dlat),
                ]


                region_dist_cache = {}
                region_dist_cache_nb = {}

                i = 0

                print("*"*80)
                print("Processing subdomains (first pass)")
                print("*"*80)
                process_domains_by_regions(self.df_subdomains, par_outer=True, region_dist_cache=region_dist_cache, center_offset=cell_offsets[i])

                if self.domain_neib is not None:
                    print("*"*80)
                    print("Processing domain neighbors (first pass)")
                    print("*"*80)
                    process_domains_by_regions(self.df_domain_nb, par_inner=True, region_dist_cache=region_dist_cache_nb, center_offset=cell_offsets[i])


                print("*"*80)
                print("Processing subdomains (other passes)")
                print("*"*80)
                for i in range(len(cell_offsets[1:])):
                    print("PASS ", i, " of ", len(cell_offsets), " using cell offset ", cell_offsets[i])
                    process_domains_by_regions(self.df_subdomains, par_outer=True, region_dist_cache=region_dist_cache, center_offset=cell_offsets[i])


                if self.domain_neib is not None:
                    print("*"*80)
                    print("Processing domain neighbors (other passes)")
                    print("*"*80)

                    for i in range(len(cell_offsets[1:])):
                        print("PASS ", i, " of ", len(cell_offsets))
                        process_domains_by_regions(self.df_domain_nb, par_inner=True, region_dist_cache=region_dist_cache_nb, center_offset=cell_offsets[i])



            else:
                raise Exception("This network algorithm is not implemented")

            #Save this region grid for furute use, as it takes time to build
            np.savetxt(self.gridname+".csv", self.region_grid, fmt='%i')
            np.save(self.gridname, self.region_grid)
            print("Regions saved in file "+self.gridname)


        imgfilename = self.gridname+".jpg"
        if not os.path.exists(imgfilename):
            print("Plotting regions to file "+imgfilename)
            #Map the regions
            map = Map(self)
            map.map_reg_data(self, self.gridname, filename=imgfilename)


    def get_closest_region(self, lat, lon):

        p = Point(lon, lat)
        inmaindomain = p.within(self.domain_geometry)
        #print(p, inmaindomain)
        #debug=False
        #if abs(lon+45.0160)<0.1:
        #    if abs(lat+23.46410256410256)<0.1:
        #        debug=True
        #        print(p, debug)
        ireg=-1
        if inmaindomain:

            lat_tmp=self.df_subdomains["latc"].to_numpy()
            lon_tmp=self.df_subdomains["lonc"].to_numpy()

            latv = np.full(len(lat_tmp), lat)
            lonv = np.full(len(lon_tmp), lon)

            dist_tmp = distance(lonv, latv, lon_tmp, lat_tmp)

            if self.nreg_in > 100 :
                k = 20 # k nearest neighbours
                inearreg = np.argpartition(dist_tmp, k) #[0:k]
            elif self.nreg_in > 10 :
                k = 10 # k nearest neighbours
                inearreg = np.argpartition(dist_tmp, k) #[0:k]
            elif self.nreg_in == 1 :
                return 0
            else:
                k = 2 # k nearest neighbours
                inearreg = np.argpartition(dist_tmp, k) #[0:k]

            #Use geometry to check if point in subregion
            for i in inearreg[0:k]:

                subreg_geometry = self.df_subdomains.loc[i, "geometry"]
                in_region=p.within(subreg_geometry)
                #if debug:
                #    print(i, in_region)
                if in_region:
                    ireg = i
                    return ireg

        else:
            #check if in neighbour states
            if self.domain_neib is not None:
                for index, nb in self.df_domain_nb.iterrows():
                    #nb_name=nb[self.domain_gran]
                    innbreg=p.within(nb.geometry)
                    #if debug:
                    #    print(index, nb, innbreg)
                    if innbreg:
                        ireg = index #self.regions_out.get(index, -1)
        #if debug:
        #    print(p)
        #    sys.exit(1)
        return ireg

    def add_reg_to_df(self, dom, data):

        for day in data:
            #Add column with region tag
            #for each event
            for i in range(2):
                s=str(i)
                lon=day.df['lng'+s].values
                lat=day.df['lat'+s].values
                lonnan=np.isnan(lon)
                latnan=np.isnan(lat)
                nan = lonnan*latnan
                ilon=((lon[~nan]-dom.minlons)/dom.dlon).astype(int)
                ilat=((lat[~nan]-dom.minlats)/dom.dlat).astype(int)
                #print(lon[~nan], lat[~nan], ilon, ilat)
                reg = np.zeros(day.n).astype(int)
                reg[nan] = -1
                reg[~nan]=self.region_grid[ilat, ilon]
                day.df['reg'+s]=reg

            #Add column with moved or not
            reg0 = day.df['reg0']
            reg1 = day.df['reg1']
            mov = reg0 != reg1
            day.df['mov_reg']=mov

            #day.df.to_csv("tmp.csv")

    def add_reg_to_daydf(self, daydata):

        #Add column with region tag
        #for each event
        for i in range(2):
            s=str(i)
            lon=daydata.df['lng'+s].values
            lat=daydata.df['lat'+s].values

            #Remove data that is outside of the box
            lon=np.where((lon>self.maxlons)|(lon<self.minlons), np.nan, lon)
            lat=np.where((lat>self.maxlats)|(lat<self.minlats), np.nan, lat)

            lonnan=np.isnan(lon)
            latnan=np.isnan(lat)
            nan = lonnan | latnan

            ilon=((lon[~nan]-self.minlons)/self.dlon).astype(int)
            ilat=((lat[~nan]-self.minlats)/self.dlat).astype(int)

            reg = np.zeros(daydata.n).astype(int)
            reg[nan] = -1
            reg[~nan]=self.region_grid[ilat, ilon]

            daydata.df['reg'+s]=reg

        #Add column with moved or not
        reg0 = daydata.df['reg0']
        reg1 = daydata.df['reg1']
        mov = reg0 != reg1
        daydata.df['mov_reg']=mov

        #day.df.to_csv("tmp.csv")

    def calc_transition_matrix(self, day):

        print()
        print("Generating transition matrices...", end="")

        df=day.df
        print(df)
        #df['origin']=df['reg0']
        df['destiny']=np.where(df['movedin24h'], df['reg1'], df['reg0'])

        print(len(df))
        if False: #filter dt < 24h        
            df = df[df['movedin24h']==1] 
        print(len(df))

        #df_tmp=df[df['movedin24h']==0]
        #df_tmp=df_tmp[df_tmp['mov_reg']==True]
        #print(df_tmp[['reg0', 'reg1', 'movedin24h', 'origin', 'destiny']])

        #df.to_csv("tmp.csv", header=True)
        table = df.pivot_table(values='dt1', index=['destiny'],\
                     columns=['reg0'], aggfunc=np.count_nonzero, fill_value=0, dropna=False)

        #print(table.columns)
        #Remove other outer regions from transition matrix
        #consider movements only data inside the domain
        #nb_regions = range(self.nreg_in, self.nregions+1, 1)
        #try:
        #    table = table.drop(columns=nb_regions)
        #    table = table.drop(nb_regions, axis=0)
        #except:
        #    pass

        #remove the problematic -1 regions
        #Columns
        try:
            table = table.drop(columns=[-1])
        except:
            pass
        #Rows
        try:
            table = table.drop([-1], axis=0)
        except:
            pass



        #debug tool only!!!!
        #table = table.drop(columns=[5])
        #table = table.drop(columns=[9])
        #table = table.drop([4], axis=0)
        #table = table.drop([9], axis=0)

        #Columns are regions at time 0
        #Rows are regions at time 1
        reg0 = table.columns
        #print(reg0)
        reg1 = table.index
        mat = table.as_matrix(columns=None)

        #print(list(reg0))
        #print(list(self.regions.keys()))
        #Check if we lost a region
        nreg0=len(list(reg0))
        nreg1=len(list(reg1))
        nreg=len(self.regions)
        reg = list(self.regions.keys())
        n , m = mat.shape

        if nreg0==nreg:
            print("Matrix check origin: ok!...", end="")
        elif nreg>nreg0:
            print("Missing columns..", end="")
            missing_col=list(set(reg)-set(reg0))
            missing_col.sort()
            print(missing_col, end="")

            for i in missing_col:
                col = np.zeros((n,1))
                mat = np.hstack((mat[:,:i], col, mat[:,i:]))
                #print(mat.shape)
                reg0 = np.hstack((reg0[:i], [i], reg0[i:]))
            print("..fixed!", end="")
        else:
            print("Something is wrong with your region data...and I don't know what it is...")
            sys.exit(1)

        #re-set mat sizes
        n , m = mat.shape
        #print(reg0)
        if nreg1==nreg:
            print("Matrix check destination: ok!...", end="")
        elif nreg>nreg1:
            print("..Missing row:", end="")
            missing_row=(list(set(reg)-set(reg1)))
            missing_row.sort()
            print(missing_row, end="")
            for i in missing_row:
                row = np.zeros((1,m))
                #print(mat[:i,:].shape, row.shape, mat[i:,:].shape)
                mat = np.vstack((mat[:i,:], row, mat[i:,:]))
                reg1 = np.hstack((reg1[:i], [i], reg1[i:]))
            print("..fixed!", end="")
        else:
            print("Something is wrong with your region data...and I don't know what it is...")
            sys.exit(1)

        n , m = mat.shape
        if n!=m:
            print("Matrix not square!!!")
            sys.exit(1)

        #Fix diagonal be one where zero, just to avoid null divisions
        for i in  range(n):
            if mat[i,i] == 0.0:
                mat[i,i] = 1.0

        #print(reg1)
        #matprint(mat)

        #The resulting table has the people that moved, we now need to include people that did not move
        # to be addressed as post-processing
        if self.nregions < 10:
            print("")
            print("Transition Matrix (number of people moving to/from regions)")
            print(table)
            print()

        #np.fill_diagonal(mat, mat.diagonal() + steady_users_per_reg)
        #print(mat)
        if self.nregions < 10:
            print("Transition matrix including steady users")
            matprint(mat)
            print()

        #Normalize
        mat_normed = mat / mat.sum(axis=0)
        if self.nregions<10:
            print("Normalized transition matrix (transition probability)")
            matprint(mat_normed)


        #check consistency
        mat_tmp=np.copy(mat_normed)
        for i in  range(n):
            mat_tmp[i,i] = 0.0
        #matprint(mat_tmp)
        moving = mat_tmp.max(axis=0)
        
        #print(moving)

        if self.nregions > 10:
            print("..done")

        return mat, mat_normed, reg0, reg1

    def collect_move_mat(self, local_dir):
        
        # Try to get the matriz from usual name
        name = "move_mat_"+self.domain+"_"+self.subdomains

        np_load_old = np.load
        np.load = lambda *a,**k: np_load_old(*a, allow_pickle=True, **k)

        matfile = local_dir+name+'.csv'
        if os.path.exists(matfile):    
            mat = np.genfromtxt(matfile)
            mat_normed = np.genfromtxt(local_dir+name+'_norm.csv')
                    
            #reg_names = np.load(local_dir+name+'_reg_names.npy')
            filename = local_dir+name+"_reg_names.txt"
            with open(filename) as f:
                reg_names = f.read().splitlines()            
        else:
            print("Could not find this domain matrix, searching for Brasil data")
            #Let check if we have a full brasil matriz available
            if self.subdomains == "Municip":
                name = "move_mat_Brasil_Municip"
            elif self.subdomains == "microreg":
                name = "move_mat_Brasil_microreg"
            elif self.subdomains == "states":
                name = "move_mat_Brasil_states"
            else: 
                print("Don't know how to collect this kind of subdomain", self.subdomains)
                sys.exit()

            matfile = local_dir+name+'.csv'
            if os.path.exists(matfile):    
                print("  Found a Brasil matrix! Calculating...this may take some time...")
                mat_br = np.genfromtxt(matfile)  
                filename = local_dir+name+"_reg_names.txt"
                with open(filename) as f:
                    reg_names = f.read().splitlines()
            else:
                print("Can't find Brasil matrix, please run mdyn_build_model.py first to generate the movement matrices")
                print(" (run with the same parameter file and option -o 0!)")
                sys.exit()

            # now we need to extract from the matrix only the desired columns/lines
            br_array = np.array(reg_names)
            local_array = np.array(list(self.regions_in_codes.values()))
            sorter = np.argsort(br_array)
            pos = sorter[np.searchsorted(br_array, local_array, sorter=sorter)]
            
            #filter matrix
            mat = mat_br[pos,:][: , pos]
            if mat.shape[0] != len(self.regions_in):
                print("Matriz build did not work propertly")
                sys.exit()

            mat_normed = mat / mat.sum(axis=0)

            #Save to avoid processing this again
            name = "move_mat_"+self.domain+"_"+self.subdomains
            np.savetxt( local_dir+name+".csv", mat)
            np.savetxt( local_dir+name+"_norm.csv", mat_normed)
            #np.savetxt( day_data.local_dir+name+"_reg0.csv", day_data.reg0)
            #np.savetxt( day_data.local_dir+name+"_reg1.csv", day_data.reg1)
            #np.save( day_data.local_dir+name+"_reg_names.npy", network.regions)
            reg=list(self.regions.values())
            with open(local_dir+name+"_reg_names.txt", "w") as output:
                for r in reg:
                    output.write("%s\n" % r)

            #print(len(pos), len(self.regions_in_codes), len(self.regions_in), len(br_array[pos]), mat_local.shape)
            #reg_collect = reg_names.index(list(self.regions_in_codes.values()))
            #print(reg_collect)

        return mat, mat_normed, self.regions_in

    def load_pop(self):
        print("Loading regions' populations....", end="")
        #Population - inner regions
        filename = self.subdomains_pop # "maps/population/population_sp_mun.csv"
        subdomains_pop_labels = self.subdomains_pop_labels # ["municipio", "populacao_estimada"]
        dom_label = subdomains_pop_labels[0]
        subdom_label = subdomains_pop_labels[1]
        pop_label = subdomains_pop_labels[2]

        df_pop = pd.read_csv(filename)
        #Filter to domain region
        df_pop_tmp=df_pop[df_pop[dom_label]==self.domain_abrv]
        if len(df_pop_tmp) < 1:
            print("Could not filter domain in population")
            print(df_pop_tmp)
        else:
            print("Filtered domain in population")
            df_pop = df_pop_tmp

        #Put to upper case
        #print(df_pop.dtypes)
        df_pop[subdom_label]=df_pop[subdom_label].apply(str)
        df_pop[subdom_label]=df_pop[subdom_label].astype(str)
        df_pop[subdom_label]=df_pop[subdom_label].str.upper()

        #print(df_pop.dtypes)
        self.reg_pop = np.zeros([self.nregions])

        for reg in range(self.nreg_in):
            region = self.regions.get(reg)
            region = str(region)
            
            try:
                pop = df_pop.loc[df_pop[subdom_label] == region, pop_label].values[0]
            except:
                print(df_pop)
                print("Cant find this region's population:", region )
                #pop = 0
                sys.exit(1)
            self.reg_pop[reg] = pop

        #Population - outer regions
        filename = self.domain_pop # "maps/population/population_sp_mun.csv"
        domain_pop_labels = self.domain_pop_labels # ["municipio", "populacao_estimada"]

        df_pop = pd.read_csv(filename, sep = ";")
        
        df_pop[domain_pop_labels[0]]=df_pop[domain_pop_labels[0]].str.upper()
        for reg in range(self.nreg_out):
            i=self.nreg_in+reg
            region = self.regions.get(i)
            try:
                pop = df_pop.loc[df_pop[domain_pop_labels[0]] == region, domain_pop_labels[1]].values[0]
            except:
                print("Cant find this region's population:", region )
                sys.exit(1)

            self.reg_pop[i] = pop

        print("Done.")
