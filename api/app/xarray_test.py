import rioxarray
import xarray

xds = xarray.open_dataset(
    "/home/cbrust/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly//MRI-ESM2-0_ssp585_r1i1p1f1_tas.tif"
    )

print('a')