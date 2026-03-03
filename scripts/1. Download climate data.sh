#!/usr/bin/env bash

# 1. Download climate data.sh 
#    Licence: GNU GPLv3 - see LICENSE.txt for more details

# Description 
 
#———————————————————————————————————————————————————————————————————————————————

# PURPOSE
# ------------------------------------------------------------------------------
 
# Shell script to download climate data from 
# the NASA Earth Exchange Global Daily Downscaled Projections archive (NEX-GDDP):

#   Thrasher, Bridget, Weile Wang, Andrew Michaelis, 
#   Forrest Melton, Tsengdar Lee, Ramakrishna Nemani. 
#   NASA global daily downscaled projections, CMIP6.
#   Scientific data 9, 262 (2022).
#   https://doi.org/10.1038/s41597-022-01393-4

# PRODUCT 
# ------------------------------------------------------------------------------
 
# 5 sets of netCDF time series spanning 2015-2100
# of daily mean air temperatures for 4 Great Lakes tributaries 
# under the SSP5:85 scenario (CMIP6)
# from 5 general circulation models selected using the GCMeval tool:
 
#   Kajsa M. Parding, Andreas Dobler, Carol F. McSweeney, 
#   Oskar A. Landgren, Rasmus Benestad, Helene B. Erlandsen,
#   Abdelkader Mezghani, Hilppa Gregow, Olle Räty, Elisabeth Viktor, 
#   Juliane El Zohbi, Ole B. Christensen, Harilaos Loukos.
#   GCMeval – An interactive tool for evaluation 
#   and selection of climate model ensembles,
#   Climate Services 18, 100167, (2020)
#   https://doi.org/10.1016/j.cliser.2020.100167.

# PROCESS
# ------------------------------------------------------------------------------

# Performance rankings for 101 models generated using GCMeval
# with settings prioritizing resolution for:
#   east north america region,
#   temperature variable (not precipitation),
#   winter and spring ranked very important
#   summer and autumn ranked regular importance

# 5 models were selected from this list by: 
#   taking the best performing models where possible,
#   and including lower ranking models where necessary
#   to capture as much of the range of the full ensemble 

#   Models selected                 |  Rank
#   --------------------------------+-------
#   CMIP6.MRI_ESM2_0.r1i1p1f1       |    2
#   CMIP6.HadGEM3_GC31_MM.r1i1p1f3  |   30
#   CMIP6.EC_Earth3_Veg.r1i1p1f     |   32
#   CMIP6.INM_CM5_0.r1i1p1f1        |   49
#   CMIP6.UKESM1_0_LL.r1i1p1f2      |   64

#   80% variation captured for scenario ssp5:8.5

# PROCEDURE
# ------------------------------------------------------------------------------

# WARNING: NASA's pages often seem to be down, malfunctioning,
#          or just interact poorly with certain networks (e.g. university wifi).
#          If things do not seem to work for you, 
#          try using a VPN, 
#          try again later from a different network,
#          or check if the repository has moved / URLs have changed
#          by navigating to the data from the root: 
#          ( https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6 )


# 1. retrieve download URLs - go to the NCCS THREDDS NEX-GDDP-CMIP6 portal:
#   ( https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html )
 
# 2. grab THEIR download link for each desired model/scenario/variant/variable
#   ( because they don't keep a consistent URL path structure across datasets,
#     and seemingly identical links typed by hand can fail to work )
 
#    these parameters should remain fixed as to retrieve data from the same series 
#    of projections across different years and in various locations, for example:
 
#    Static Parameters:
#          tas      -  mean air temperature
#          ssp585   -  shared socioeconomic pathway 5: 8.5 (scenario)
#          r1i1p1f1 -  model variant (1st Realization, Initialization, Physics, and Forcing)
 
# 3. edit links here to adjust the year / location parameters as needed,
#    and download data over 2015-2100 for 4 tributaries,
#    the Vermillion, St. Louis, Genesee, and Nipigon rivers,
#    respectively located at:
 
# V.       north=41.55&west=277.5&east=277.6&south=41.5
# S.       north=46.71&west=268.4&east=268.41&south=46.7
# G.       north=43.14&west=282.72&east=282.73&south=43.13
# N.       north=49.01&west=272.06&east=272.07&south=49


# Example download URL to show which parts of the link change:     |---- MODEL ---|                             |---- MODEL ---|                    YEAR            |---------------- LOCATION ----------------|                                               YEAR                              YEAR                                                     FILE NAME
# wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_2015.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&disableProjSubset=on&horizStride=1&time_start=2015-01-01T12%3A00%3A00Z&time_end=2015-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O v2015.nc

# ASSUMING you already have a data subdirectory within the project root directory
# make subdirectories for each model, then loop through years & download data
mkdir ../data/mriESM2 ../data/hadGEM3 ../data/ecEarth3 ../data/inmCM5 ../data/ukESM1


# MRI_ESM2_0 - Vermillion, Genesee, St. Louis, Nipigon
for n in {2015..2100}; do

    wget "https://ds.nccs.nasa.gov/thredds/ncss/AMES/NEX/GDDP-CMIP6/MRI-ESM2-0/ssp585/r1i1p1f1/tas/tas_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_$n.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/mriESM2/v$n.nc"

    wget "https://ds.nccs.nasa.gov/thredds/ncss/AMES/NEX/GDDP-CMIP6/MRI-ESM2-0/ssp585/r1i1p1f1/tas/tas_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_$n.nc?var=tas&north=46.71&west=268.4&east=268.41&south=46.7&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/mriESM2/s$n.nc"

    wget "https://ds.nccs.nasa.gov/thredds/ncss/AMES/NEX/GDDP-CMIP6/MRI-ESM2-0/ssp585/r1i1p1f1/tas/tas_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/mriESM2/g$n.nc"

    wget "https://ds.nccs.nasa.gov/thredds/ncss/AMES/NEX/GDDP-CMIP6/MRI-ESM2-0/ssp585/r1i1p1f1/tas/tas_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_$n.nc?var=tas&north=49.01&west=272.06&east=272.07&south=49&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/mriESM2/n$n.nc"

done


# HadGEM3_GC31_MM - Vermillion, Genesee, St. Louis, Nipigon
for n in {2015..2100}; do

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/HadGEM3-GC31-MM/ssp585/r1i1p1f3/tas/tas_day_HadGEM3-GC31-MM_ssp585_r1i1p1f3_gn_$n.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/hadGEM3/v$n.nc"
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/HadGEM3-GC31-MM/ssp585/r1i1p1f3/tas/tas_day_HadGEM3-GC31-MM_ssp585_r1i1p1f3_gn_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/hadGEM3/s$n.nc"
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/HadGEM3-GC31-MM/ssp585/r1i1p1f3/tas/tas_day_HadGEM3-GC31-MM_ssp585_r1i1p1f3_gn_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/hadGEM3/g$n.nc"
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/HadGEM3-GC31-MM/ssp585/r1i1p1f3/tas/tas_day_HadGEM3-GC31-MM_ssp585_r1i1p1f3_gn_$n.nc?var=tas&north=49.01&west=272.06&east=272.07&south=49&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/hadGEM3/n$n.nc"

done


# EC-Earth3-Veg-LR - Vermillion, Genesee, St. Louis, Nipigon
for n in {2015..2100}; do

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_$n.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ecEarth3/v$n.nc" 

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_$n.nc?var=tas&north=46.71&west=268.4&east=268.41&south=46.7disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ecEarth3/s$n.nc" 

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ecEarth3/g$n.nc" 

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_$n.nc?var=tas&north=49.01&west=272.06&east=272.07&south=49&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ecEarth3/n$n.nc" 

done


# INM_CM5_0 - Vermillion, Genesee, St. Louis, Nipigon
for n in {2015..2100}; do

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/INM-CM5-0/ssp585/r1i1p1f1/tas/tas_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_$n.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/inmCM5/v$n.nc" 
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/INM-CM5-0/ssp585/r1i1p1f1/tas/tas_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_$n.nc?var=tas&north=46.71&west=268.4&east=268.41&south=46.7&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/inmCM5/s$n.nc" 
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/INM-CM5-0/ssp585/r1i1p1f1/tas/tas_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/inmCM5/g$n.nc" 

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/INM-CM5-0/ssp585/r1i1p1f1/tas/tas_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_$n.nc?var=tas&north=49.01&west=272.06&east=272.07&south=49&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-31T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/inmCM5/n$n.nc" 

done


# UKESM1_0_LL - Vermillion, Genesee, St. Louis, Nipigon
for n in {2015..2100}; do

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/UKESM1-0-LL/ssp585/r1i1p1f2/tas/tas_day_UKESM1-0-LL_ssp585_r1i1p1f2_gn_$n.nc?var=tas&north=41.55&west=277.5&east=277.6&south=41.5&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ukESM1/v$n.nc"

    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/UKESM1-0-LL/ssp585/r1i1p1f2/tas/tas_day_UKESM1-0-LL_ssp585_r1i1p1f2_gn_$n.nc?var=tas&north=46.71&west=268.4&east=268.41&south=46.7&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ukESM1/s$n.nc"
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/UKESM1-0-LL/ssp585/r1i1p1f2/tas/tas_day_UKESM1-0-LL_ssp585_r1i1p1f2_gn_$n.nc?var=tas&north=43.14&west=282.72&east=282.73&south=43.13&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ukESM1/g$n.nc"
    
    wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/UKESM1-0-LL/ssp585/r1i1p1f2/tas/tas_day_UKESM1-0-LL_ssp585_r1i1p1f2_gn_$n.nc?var=tas&north=49.01&west=272.06&east=272.07&south=49&disableProjSubset=on&horizStride=1&time_start=$n-01-01T12%3A00%3A00Z&time_end=$n-12-30T12%3A00%3A00Z&timeStride=1&addLatLon=true" -O "data/ukESM1/n$n.nc"

done

# An example line for an alternative option, if interested:
#   a faster parallel download using GNU parallel on 8 cores (but requires escaping `&` and `?` characters in the URL)
#   parallel -a <(seq 2015 2100) -P 8 wget "https://ds.nccs.nasa.gov/thredds2/ncss/AMES/NEX/GDDP-CMIP6/EC-Earth3-Veg-LR/ssp585/r1i1p1f1/tas/tas_day_EC-Earth3-Veg-LR_ssp585_r1i1p1f1_gr_{1}.nc\?var=tas\&north=41.55\&west=277.5\&east=277.6\&south=41.5\&disableProjSubset=on\&horizStride=1\&time_start={1}-01-01T12%3A00%3A00Z\&time_end={1}-12-31T12%3A00%3A00Z\&timeStride=1\&addLatLon=true" -O "ecEarth3/v{1}.nc" 
