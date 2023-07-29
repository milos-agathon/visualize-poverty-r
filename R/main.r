#########################################
#                 Mapping poverty with R
#                 Milos Popovic
#                 2023/07/25
#########################################
install.packages("remotes")
remotes::github_install(
    "dickoa/rgeoboundaries"
)
libs <- c(
    "tidyverse", "rgeoboundaries",
    "sf", "terra", "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 1. GET INDIAN STATES

india_states <- rgeoboundaries::gb_adm1(
    "IND"
)

url <- "https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020/archive/refs/heads/master.zip"
download.file(
    url,
    basename(url),
    mode = "wb"
)

unzip("master.zip")

india_states <- sf::st_read(
    "India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp"
)

# 2. Global Gridded Relative Deprivation Index (GRDI), Version 1

unzip("povmap-grdi-v1-grdiv1-geotiff.zip")

grdi <- terra::rast("povmap-grdi-v1.tif")

# 3. CROP GRDI BY INDIAN SHAPEFILE

indian_states_4326 <- india_states |>
    sf::st_transform(
        4326
    )

india_grdi <- terra::crop(
    grdi,
    terra::vect(indian_states_4326),
    snap = "in",
    mask = T
)

# 4. AVERAGE GRDI WITH ZONAL STATISTICS

india_grdi_zs <- terra::zonal(
    india_grdi,
    terra::vect(
        indian_states_4326
    ),
    fun = "mean",
    na.rm = T
)

indian_states_grdi <- cbind(
    indian_states_4326,
    india_grdi_zs
)

names(indian_states_grdi)[2] <- "grdi"

# 5. MAP

p1 <- ggplot() +
    geom_sf(
        data = indian_states_grdi,
        aes(
            fill = grdi
        ),
        color = "grey10",
        size = .25,
        alpha = .75
    ) +
    geom_sf_label(
        data = indian_states_grdi,
        aes(
            label = round(grdi, 1)
        ),
        color = "grey10",
        size = 2,
        label.size = NA,
        alpha = .5
    ) +
    scale_fill_gradientn(
        name = "GRDI",
        colors = rev(hcl.colors(
            8, "Plasma",
            alpha =  .85
        ))
    ) +
    coord_sf(crs = 4326) +
    theme_void() +
    theme(
        legend.position = "right",
        plot.margin = unit(
            c(
                t = -3, r = 2,
                b = -3, l = .5
            ), "lines"
        )
    ) +
    labs(
        title = "Relative deprivation index (2010-2020)",
        caption = "Data: Global Gridded Relative Deprivation Index, v.1"
    )

ggsave(
    "india-grdi.png", p1,
    width = 7, height = 7,
    units = "in", bg = "white"
)

# 6. 3D MAP

india_grdi_mat <- rayshader::raster_to_matrix(
    india_grdi
)

cols <- rev(hcl.colors(
    8, "Plasma"
))

texture <- colorRampPalette(cols)(256)

india_grdi_mat |>
    rayshader::height_shade(
        texture = texture
    ) |>
    plot_3d(
        india_grdi_mat,
        zscale = 1,
        solid = F,
        shadow = T,
        shadow_darkness = .99,
        sunangle = 315,
        zoom = .4,
        phi = 30,
        theta = -30
    )
        
rayshader::render_camera(
    phi = 85,
    zoom = .6,
    theta = 0
)

rayshader::render_snapshot(
    "india_grdi-snapshot.png"
)
        
rayshader::render_highquality(
    filename = "india-grdi.png",
    preview = T,
    light = T,
    lightdirection = 315,
    lightintensity = 1100,
    lightaltitude = 60,
    interactive = F,
    parellel = T,
    width = 4000, height = 4000
)
