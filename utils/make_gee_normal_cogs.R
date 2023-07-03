library(rgee)

reticulate::use_python("/Users/Colin.Brust/.virtualenvs/rgee/bin/python")
reticulate::import("ee")
ee$Initialize()


get_qa_bits = function(img, from, to) {
  size = 1 + to - from
  msk = bitwShiftL(1, size) - 1
  img$rightShift(from)$bitwiseAnd(msk)
}

clean_mod16 <- function(img) {
  qa = img$select("ET_QC")
  good_quality = get_qa_bits(qa, 0, 0)$eq(0)
  no_clouds = get_qa_bits(qa, 3, 4)$eq(0)
  mask = good_quality$And(no_clouds)
  
  img$updateMask(mask)$
    select(c("ET", "PET"))$
    copyProperties(img, list("system:time_start"))
}

clean_mod17 <- function(img) {
  qa = img$select("Psn_QC")
  good_quality = get_qa_bits(qa, 0, 0)$eq(0)
  no_clouds = get_qa_bits(qa, 3, 4)$eq(0)
  mask = good_quality$And(no_clouds)
  
  img$updateMask(mask)$
    select("Gpp")$
    copyProperties(img, list("system:time_start"))
}

clean_mod13 <- function(img) {
  qa = img$select("DetailedQA")
  mask = get_qa_bits(qa, 0, 1)$eq(0)
  
  img$updateMask(mask)$
    select(c("NDVI", "EVI"))$
    copyProperties(img, list("system:time_start"))
}

year_mon_calc <- function(year, coll, func="sum") {
  
  months <- ee$List$sequence(1, 12)
  
  mon_calc <- function(m, func) {
    w <- coll$filter(ee$Filter$calendarRange(year, year, 'year'))$
      filter(ee$Filter$calendarRange(m, m, 'month'))
    
    if (func == "sum") {
      w <- w$sum()
    } else if (func == "mean") {
      w <- w$mean()
    } else {
      stop("func must be either 'sum' or 'mean'")
    }
    return(
      w$set('year', year)$
        set('month', m)$
        set('system:time_start', ee$Date$fromYMD(year, m, 1))
    )
  }
  return(months$map(rgee::ee_utils_pyfunc(function(x) {
    mon_calc(x, func=func)
  })))
}


process_normals <- function(coll, func, filter_start="2001-01-01") {
  years <- ee$List$sequence(1991, 2020)
  year_mons <- ee$ImageCollection$fromImages(
    years$map(rgee::ee_utils_pyfunc(
      function(x) {
        year_mon_calc(x, coll = coll, func = func)
      }
    ))$flatten()
  )
  
  year_mons <- year_mons$filterDate(filter_start, "2050-01-01")

  months <- ee$List$sequence(1, 12)
  monthly_mean <- function(m, year_mons) {
    w <- year_mons$filter(ee$Filter$eq('month', m))$mean()
  
    return(w$set('month', m))$set('system:time_start', ee$Date$fromYMD(1, m, 1))
  }

  out <- ee$ImageCollection$fromImages(
    months$map(rgee::ee_utils_pyfunc(function(x) {
      return(monthly_mean(x, year_mons))
    }))$flatten()
  )
  
  out = out$toBands()
  return(out)
}


shp <- urbnmapr::get_urbn_map(sf = T) %>% 
  dplyr::filter(state_name == "Montana") %>% 
  sf::st_transform(4326) %>%
  sf_as_ee()

mod16 = process_normals(
  ee$ImageCollection("MODIS/006/MOD16A2")$
    map(clean_mod16)$
    filterDate("1999-01-01", "2020-12-31"),
  "sum",
  "2001-01-01"
)


mod17 = process_normals(
  ee$ImageCollection("MODIS/006/MOD17A2H")$
    map(clean_mod17),
  "sum", 
  "2001-01-01"
) %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=500)

mod13 = process_normals(
  ee$ImageCollection("MODIS/061/MOD13A1")$
    map(clean_mod13),
  "mean", 
  "2001-01-01"
) %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=500) %>%
  normals::write_as_cog("./mod13.tif")

cover = ee$ImageCollection("projects/rangeland-analysis-platform/vegetation-cover-v3")$filterDate("1991-01-01", "2020-12-31")$mean() %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100)%>%
  normals::write_as_cog("./cover.tif")
npp = ee$ImageCollection("projects/rangeland-analysis-platform/npp-partitioned-v3")$filterDate("1991-01-01", "2020-12-31")$mean() %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100) %>%
  normals::write_as_cog("./npp.tif")

process_to_normal_cogs <- function(r, out_dir = "~/data/blm_project/gee_data", scale_factor=0.0001) {
  nms <- names(r) 
  
  var_locs <- stringr::str_split(nms, "_") %>% 
    purrr::map(magrittr::extract, 2) %>% 
    unlist()
  
  vars <- unique(var_locs)
  
  vars %>% 
    purrr::map(function(x) {

      write_dir = file.path(out_dir, tolower(x))
      if (!dir.exists(write_dir)) {
        dir.create(write_dir)
      }
      out_names <- file.path(write_dir, paste0(tolower(month.abb), "_mean.tif"))
      terra::subset(r, stringr::str_detect(var_locs, paste0("^",x,"$"))) %>%
        magrittr::set_names(tolower(month.abb)) %>%
        {. * scale_factor} %>%
        normals::write_as_cog(out_names)
        
    })
}

make_rapp_normals <- function(r, out_dir) {
  f_names <- file.path(out_dir, tolower(names(r)))
  purrr::map(f_names, dir.create)
  normals::write_as_cog(r, paste0(f_names, "/annual_mean.tif"))
}








