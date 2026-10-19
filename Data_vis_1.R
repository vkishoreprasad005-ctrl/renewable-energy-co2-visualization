# =====================================================================
# Renewable Energy vs. CO2 Emissions Across the World
# Interactive R Shiny dashboard (Advanced Data Visualization project)
# Input : D:/ADVT_PROJECT/renewable_energy_co2_cleaned.csv (read only)
# Output: none. Everything stays in memory; no files are written.
# =====================================================================

## ---- 0. PACKAGES (asks before installing anything) -----------------

library(shiny); library(bslib); library(plotly); library(ggplot2)
library(dplyr); library(DT); library(sf); library(scales); library(htmltools)

## ---- 1. SETTINGS, HELPERS ------------------------------------------
DATA_PATH <- "D:/ADVT_PROJECT/renewable_energy_co2_cleaned.csv"
COL_GREEN <- "#3DDC97"; COL_TEAL <- "#2AB7CA"; COL_NAVY <- "#0B1F3A"; COL_AMBER <- "#FFB703"
region_levels <- c("Africa", "Asia", "Europe", "North America", "South America",
                   "Oceania", "Other", "Unclassified")
region_pal <- c(Africa = "#E69F00", Asia = "#CC79A7", Europe = "#56B4E9",
                `North America` = "#009E73", `South America` = "#D55E00",
                Oceania = "#7F8CFF", Other = "#9AA5B1", Unclassified = "#9AA5B1")
`%||%` <- function(a, b) if (is.null(a)) b else a

metric_label <- function(m) if (m == "ren") "Renewable energy share (%)" else "CO\u2082 emissions per capita (t per person)"
metric_short <- function(m) if (m == "ren") "Renewable share" else "CO\u2082 per capita"
fmt_val <- function(m, v) if (m == "ren") sprintf("%.1f%%", v) else sprintf("%.2f t per person", v)
fmt_int <- function(x) format(as.integer(x), big.mark = ",", trim = TRUE)
fmt_p   <- function(p) if (is.na(p)) "n/a" else if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
stat_or_na <- function(f, x) { x <- x[is.finite(x)]; if (length(x)) f(x) else NA_real_ }
fill_val <- function(v, m, log10_on) if (m == "co2" && log10_on) ifelse(v > 0, log10(v), NA_real_) else v

strength_label <- function(r) {
  a <- abs(r)
  if (a < 0.1) return("negligible")
  s <- if (a < 0.3) "weak" else if (a < 0.5) "moderate" else if (a < 0.7) "strong" else "very strong"
  paste(s, if (r < 0) "negative" else "positive")
}

# Correlation + OLS on paired finite values. Never fabricates numbers.
corr_stats <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  out <- list(n = n, valid = FALSE, reason = NA_character_, r = NA_real_, r_p = NA_real_,
              rho = NA_real_, rho_p = NA_real_, slope = NA_real_, intercept = NA_real_,
              slope_p = NA_real_, r2 = NA_real_)
  if (n < 3) { out$reason <- "fewer than 3 paired observations"; return(out) }
  if (stats::sd(x) == 0 || stats::sd(y) == 0) { out$reason <- "one variable is constant"; return(out) }
  pe <- tryCatch(stats::cor.test(x, y, method = "pearson"), error = function(e) NULL)
  sp <- tryCatch(suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE)),
                 error = function(e) NULL)
  fit <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (is.null(pe) || is.null(sp) || is.null(fit)) { out$reason <- "statistics could not be computed"; return(out) }
  cf <- summary(fit)$coefficients
  out$valid <- TRUE; out$r <- unname(pe$estimate); out$r_p <- pe$p.value
  out$rho <- unname(sp$estimate); out$rho_p <- sp$p.value
  out$intercept <- cf[1, 1]; out$slope <- cf[2, 1]; out$slope_p <- cf[2, 4]
  out$r2 <- summary(fit)$r.squared
  out
}

# Friendly "empty state" errors inside plots
stop_msg <- function(msg) stop(structure(class = c("dash_msg", "error", "condition"),
                                         list(message = msg, call = NULL)))
empty_plot <- function(msg, dark = TRUE) {
  fg <- if (dark) "#D6E4F0" else "#1F2D3D"
  plot_ly() |>
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           annotations = list(list(text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                   showarrow = FALSE, font = list(size = 15, color = fg)))) |>
    config(displayModeBar = FALSE)
}
safe_plot <- function(expr, dark = TRUE) {
  tryCatch(expr,
           dash_msg = function(e) empty_plot(conditionMessage(e), dark),
           shiny.silent.error = function(e) empty_plot("Waiting for inputs\u2026", dark),
           error = function(e) empty_plot(paste("This chart could not be drawn:", conditionMessage(e)), dark))
}
pl_style <- function(p, dark) {
  fg <- if (dark) "#D6E4F0" else "#1F2D3D"
  grid <- if (dark) "rgba(255,255,255,0.10)" else "rgba(15,30,60,0.10)"
  p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(color = fg, family = "Segoe UI, system-ui, sans-serif", size = 13),
           xaxis = list(gridcolor = grid, zerolinecolor = grid),
           yaxis = list(gridcolor = grid, zerolinecolor = grid),
           hoverlabel = list(bgcolor = if (dark) "#0B1F3A" else "#FFFFFF", font = list(color = fg)),
           legend = list(bgcolor = "rgba(0,0,0,0)")) |>
    config(displaylogo = FALSE, responsive = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "sendDataToCloud"))
}
theme_dash <- function(dark) {
  fg <- if (dark) "#D6E4F0" else "#1F2D3D"
  theme_void(base_size = 12) +
    theme(text = element_text(colour = fg), legend.text = element_text(colour = fg),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA))
}
pal_scale <- function(pal, name, dark) {
  na_col <- if (dark) "#33465C" else "#CBD3DC"
  switch(pal,
         "Teal-green" = scale_fill_gradientn(colours = c("#F1FAEE", "#B7E4C7", "#52B788", "#1B7F79", "#0B3C49"),
                                             na.value = na_col, name = name),
         "Viridis" = scale_fill_viridis_c(option = "D", na.value = na_col, name = name),
         "Magma"   = scale_fill_viridis_c(option = "A", na.value = na_col, name = name),
         "Cividis" = scale_fill_viridis_c(option = "E", na.value = na_col, name = name),
         scale_fill_viridis_c(option = "D", na.value = na_col, name = name))
}
spin <- function(x) if (requireNamespace("shinycssloaders", quietly = TRUE))
  shinycssloaders::withSpinner(x, color = COL_TEAL, type = 6) else x

# Extract polygon rings from one sf geometry as x/y vectors separated by NA
ring_xy <- function(g) {
  m <- tryCatch(sf::st_coordinates(g), error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0 || ncol(m) < 4) return(NULL)
  k <- ncol(m)
  key <- apply(m[, 3:(k - 1), drop = FALSE], 1, paste, collapse = "_")
  idx <- split(seq_len(nrow(m)), factor(key, levels = unique(key)))
  list(x = unlist(lapply(idx, function(i) c(m[i, 1], NA)), use.names = FALSE),
       y = unlist(lapply(idx, function(i) c(m[i, 2], NA)), use.names = FALSE))
}

# sf polygons -> plotly, drawn as filled scatter shapes (one hoverable shape per feature)
make_sf_map <- function(sfdf, legend_title, pal, dark, outline_sf = NULL) {
  border <- if (dark) "#0B1F3A" else "#FFFFFF"
  na_col <- if (dark) "#33465C" else "#CBD3DC"
  cols <- switch(pal,
                 "Teal-green" = c("#F1FAEE", "#B7E4C7", "#52B788", "#1B7F79", "#0B3C49"),
                 "Viridis" = viridisLite::viridis(9, option = "D"),
                 "Magma"   = viridisLite::viridis(9, option = "A"),
                 "Cividis" = viridisLite::viridis(9, option = "E"),
                 viridisLite::viridis(9))
  sfdf <- sf::st_as_sf(sfdf)
  geom <- sf::st_geometry(sfdf)
  vals <- as.numeric(sfdf$fill)
  ok <- is.finite(vals)
  rng <- if (any(ok)) range(vals[ok]) else c(0, 1)
  ramp <- grDevices::colorRampPalette(cols)(100)
  fcol <- rep(na_col, length(vals))
  pos <- if (diff(rng) > 0) (vals[ok] - rng[1]) / diff(rng) else rep(0.5, sum(ok))
  fcol[ok] <- ramp[1 + round(99 * pos)]
  
  p <- plot_ly()
  x0 <- NA_real_; y0 <- NA_real_
  for (i in seq_along(geom)) {
    xy <- ring_xy(geom[[i]]); if (is.null(xy)) next
    if (is.na(x0)) { x0 <- xy$x[1]; y0 <- xy$y[1] }
    p <- add_trace(p, x = xy$x, y = xy$y, type = "scatter", mode = "lines",
                   fill = "toself", fillcolor = fcol[i],
                   line = list(color = border, width = 0.5),
                   text = sfdf$tip[i], hoverinfo = "text", hoveron = "fills",
                   showlegend = FALSE)
  }
  if (is.na(x0)) stop_msg("No drawable map polygons were found.")
  
  # Colour bar: an invisible two-point trace that carries the colour scale
  if (any(ok)) {
    cs <- Map(function(a, b) list(a, b), seq(0, 1, length.out = length(cols)), cols)
    p <- add_trace(p, x = c(x0, x0), y = c(y0, y0), type = "scatter", mode = "markers",
                   hoverinfo = "skip", showlegend = FALSE,
                   marker = list(size = 1, opacity = 0, color = rng, cmin = rng[1], cmax = rng[2],
                                 colorscale = cs, showscale = TRUE,
                                 colorbar = list(title = legend_title, thickness = 14, len = 0.6)))
  }
  # Amber outline for selected countries
  if (!is.null(outline_sf) && nrow(outline_sf)) {
    og <- sf::st_geometry(sf::st_as_sf(outline_sf))
    for (j in seq_along(og)) {
      xy <- ring_xy(og[[j]]); if (is.null(xy)) next
      p <- add_trace(p, x = xy$x, y = xy$y, type = "scatter", mode = "lines",
                     line = list(color = COL_AMBER, width = 2), hoverinfo = "skip", showlegend = FALSE)
    }
  }
  p |>
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "",
                        scaleanchor = "x", scaleratio = 1),
           margin = list(l = 0, r = 0, t = 10, b = 0)) |>
    pl_style(dark)
}
# Fallback choropleth that needs no sf geometry (plotly's built-in ISO-3 geography)
make_geo_fallback <- function(df, label, dark) {
  plot_geo(df) |>
    add_trace(z = ~fill, locations = ~iso, text = ~tip, hoverinfo = "text", locationmode = "ISO-3",
              colorscale = "Viridis", colorbar = list(title = label)) |>
    layout(geo = list(showframe = FALSE, showcoastlines = TRUE, bgcolor = "rgba(0,0,0,0)",
                      projection = list(type = "natural earth"))) |>
    pl_style(dark)
}

## ---- 2. DATA LOADING (read-only, in memory) ------------------------
load_dashboard_data <- function(path) {
  if (!file.exists(path)) stop("CSV file not found: ", path, call. = FALSE)
  req_cols <- c("country", "year", "iso_code", "renewables_energy_share_pct", "co2_per_capita")
  na_str <- c("", "NA", "NaN", "N/A", "null")
  
  # Try several safe ways of reading the CSV (read-only; nothing is written)
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = na_str),
    error = function(e) NULL)
  if (is.null(raw))
    raw <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = na_str,
                      fileEncoding = "latin1"),
      error = function(e) NULL)
  if (is.null(raw))
    raw <- tryCatch({
      lines <- readLines(path, warn = FALSE)
      utils::read.csv(text = lines, stringsAsFactors = FALSE, check.names = FALSE, na.strings = na_str)
    }, error = function(e) NULL)
  if (is.null(raw))
    stop("Could not read the CSV. Close it if it is open in Excel, check that it is a real CSV file ",
         "(not a OneDrive online-only placeholder), and confirm the path: ", path, call. = FALSE)
  
  # Clean column names, including a possible invisible BOM before the first name
  names(raw) <- trimws(names(raw))
  for (cn in req_cols) {
    hit <- which(endsWith(names(raw), cn))
    if (length(hit) >= 1 && !(cn %in% names(raw))) names(raw)[hit[1]] <- cn
  }
  miss <- setdiff(req_cols, names(raw))
  if (length(miss)) stop("Missing required column(s): ", paste(miss, collapse = ", "),
                         ". Columns found: ", paste(names(raw), collapse = ", "), call. = FALSE)
  rpt <- list(rows_raw = nrow(raw))
  d <- data.frame(country = trimws(as.character(raw$country)),
                  year = suppressWarnings(as.integer(round(as.numeric(raw$year)))),
                  iso = toupper(trimws(as.character(raw$iso_code))),
                  ren = suppressWarnings(as.numeric(raw$renewables_energy_share_pct)),
                  co2 = suppressWarnings(as.numeric(raw$co2_per_capita)),
                  stringsAsFactors = FALSE)
  d$ren[!is.finite(d$ren)] <- NA_real_; d$co2[!is.finite(d$co2)] <- NA_real_
  bad <- is.na(d$year) | is.na(d$country) | d$country == ""
  rpt$bad_rows <- sum(bad); d <- d[!bad, , drop = FALSE]
  ok_iso <- !is.na(d$iso) & grepl("^[A-Z]{3}$", d$iso)
  rpt$invalid_iso_rows <- sum(!ok_iso)
  rpt$invalid_iso_entities <- length(unique(d$country[!ok_iso]))
  d <- d[ok_iso, , drop = FALSE]
  oor <- !is.na(d$ren) & (d$ren < 0 | d$ren > 100); rpt$ren_out_of_range <- sum(oor); d$ren[oor] <- NA_real_
  neg <- !is.na(d$co2) & d$co2 < 0;                rpt$co2_negative <- sum(neg);    d$co2[neg] <- NA_real_
  both_na <- is.na(d$ren) & is.na(d$co2); rpt$rows_both_missing <- sum(both_na); d <- d[!both_na, , drop = FALSE]
  # One display name per ISO code (most frequent), then collapse duplicate iso-year keys by mean
  nm <- d |> count(iso, country) |> arrange(desc(n)) |> distinct(iso, .keep_all = TRUE)
  d$country <- nm$country[match(d$iso, nm$iso)]
  rpt$dup_keys <- sum(duplicated(paste(d$iso, d$year)))
  if (rpt$dup_keys > 0) {
    mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    d <- d |> group_by(iso, year) |>
      summarise(country = first(country), ren = mean_na(ren), co2 = mean_na(co2), .groups = "drop")
  }
  d <- as.data.frame(d)[, c("country", "year", "iso", "ren", "co2")]
  d$paired <- !is.na(d$ren) & !is.na(d$co2)
  d <- d[order(d$country, d$year), ]
  rpt$rows_final <- nrow(d); rpt$countries <- length(unique(d$iso))
  rpt$year_min <- min(d$year); rpt$year_max <- max(d$year)
  rpt$miss_ren <- sum(is.na(d$ren)); rpt$miss_co2 <- sum(is.na(d$co2))
  list(data = d, report = rpt)
}

load_world <- function() {
  old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  w <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  w <- sf::st_make_valid(w)
  pick <- function(nm) if (nm %in% names(w)) as.character(w[[nm]]) else rep(NA_character_, nrow(w))
  iso <- pick("iso_a3"); bad <- is.na(iso) | iso %in% c("-99", ""); iso[bad] <- pick("iso_a3_eh")[bad]   # Natural Earth special codes
  bad <- is.na(iso) | iso %in% c("-99", ""); iso[bad] <- pick("adm0_a3")[bad]
  nmv <- pick("name"); nmv[is.na(nmv)] <- pick("admin")[is.na(nmv)]
  w2 <- sf::st_sf(iso = iso, name = nmv, continent = pick("continent"), geometry = sf::st_geometry(w))
  w2 <- w2[!is.na(w2$iso) & !(w2$iso %in% c("-99", "")) & !(w2$name %in% "Antarctica"), ]
  if (anyDuplicated(w2$iso))   # avoid duplicate countries when matching
    w2 <- w2 |> group_by(iso) |> summarise(name = first(name), continent = first(continent), .groups = "drop")
  eq <- NULL; proj <- NA_character_
  for (cand in list(c("Equal Earth", "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"),
                    c("Mollweide",   "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))) {
    tmp <- tryCatch(sf::st_make_valid(sf::st_transform(w2, cand[2])), error = function(e) NULL)
    if (!is.null(tmp) && all(is.finite(sf::st_bbox(tmp)))) { eq <- tmp; proj <- cand[1]; break }
  }
  list(ll = w2, eq = eq, proj = proj)
}

data_error <- NULL; dat <- NULL; rpt <- NULL; world <- NULL; world_note <- NULL
load_res <- tryCatch(load_dashboard_data(DATA_PATH), error = function(e) e)
if (inherits(load_res, "error")) {
  data_error <- conditionMessage(load_res)
} else {
  dat <- load_res$data; rpt <- load_res$report
  wr <- tryCatch(load_world(), error = function(e) e)
  if (inherits(wr, "error")) world_note <- conditionMessage(wr) else world <- wr
  reg <- if (!is.null(world)) {
    r <- as.character(world$ll$continent[match(dat$iso, world$ll$iso)])
    r[r %in% "Seven seas (open ocean)"] <- "Other"; r[is.na(r) | r == ""] <- "Unclassified"
    r[!(r %in% region_levels)] <- "Other"; r
  } else rep("Unclassified", nrow(dat))
  dat$region <- factor(reg, levels = region_levels)
  years <- sort(unique(dat$year)); ymin <- min(years); ymax <- max(years)
  cov_by_year <- tapply(dat$paired, dat$year, sum)
  default_year <- max(as.integer(names(cov_by_year))[cov_by_year >= 0.6 * max(cov_by_year)])
  country_choices <- sort(unique(dat$country))
  ch_b_def <- default_year
  ch_a_def <- years[which.min(abs(years - (default_year - 20)))]
  if (ch_a_def >= ch_b_def) ch_a_def <- years[1]
  hex_cache <- new.env(parent = emptyenv())
}

# Hexagonal grid in an equal-area projection + land intersection (cached per cell size)
get_hex <- function(size_km) {
  key <- as.character(size_km)
  if (!is.null(hex_cache[[key]])) return(hex_cache[[key]])
  w <- world$eq
  grid <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(w)), cellsize = size_km * 1000, square = FALSE)
  grid <- sf::st_sf(hex_id = seq_along(grid), geometry = grid)
  inter <- suppressWarnings(sf::st_intersection(grid, w[, c("iso", "name")]))
  inter$area <- as.numeric(sf::st_area(inter))
  inter <- inter[inter$area > 1e6, ]                       # drop slivers < 1 km2
  tab <- sf::st_drop_geometry(inter)[, c("hex_id", "iso", "name", "area")]
  res <- list(hex = grid[grid$hex_id %in% unique(tab$hex_id), ], tab = tab)
  assign(key, res, envir = hex_cache); res
}

## ---- 3. USER INTERFACE ---------------------------------------------
css <- r"---(
body{font-size:.93rem}
[data-bs-theme="light"] body{background:#F2F7FA}
[data-bs-theme="dark"]{--bs-body-bg:#071426;--bs-card-bg:#0E2440;--bs-border-color:rgba(42,183,202,.25)}
[data-bs-theme="dark"] .sidebar{background-color:#0B1B33 !important}
.card{border-radius:14px;border:1px solid rgba(42,183,202,.25);box-shadow:0 2px 10px rgba(0,0,0,.08)}
.card-header{font-weight:600;letter-spacing:.01em}
.brand{display:flex;flex-direction:column;line-height:1.15}
.brand-t{font-weight:700;font-size:1.05rem;color:#3DDC97}
.brand-q{font-size:.72rem;opacity:.85;white-space:normal;max-width:620px}
@media(max-width:1100px){.brand-q{display:none}}
.side-h{font-size:.72rem;text-transform:uppercase;letter-spacing:.08em;color:#2AB7CA;font-weight:700;margin:4px 0 6px}
.kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(175px,1fr));gap:12px;margin-bottom:14px}
.kpi{border-radius:14px;padding:14px 16px;border:1px solid rgba(42,183,202,.30);
     background:linear-gradient(135deg,rgba(42,183,202,.12),rgba(61,220,151,.08))}
.kpi .lbl{font-size:.7rem;text-transform:uppercase;letter-spacing:.06em;opacity:.8}
.kpi .val{font-size:1.6rem;font-weight:700;line-height:1.2}
.kpi .sub{font-size:.74rem;opacity:.72}
.chip{display:inline-block;padding:2px 10px;margin:2px 4px 2px 0;border-radius:999px;font-size:.74rem;
      border:1px solid rgba(42,183,202,.5);background:rgba(42,183,202,.12)}
.note{font-size:.82rem;opacity:.85;padding:6px 2px}
.empty-state{padding:18px;border:1px dashed rgba(42,183,202,.5);border-radius:12px;text-align:center;opacity:.9}
.stat-table th{font-weight:600;width:48%}
.btn-teal{background:#1B9AAA;border-color:#1B9AAA;color:#fff}
.btn-teal:hover{background:#2AB7CA;color:#fff}
)---"

theme <- bs_theme(version = 5, primary = "#1B9AAA", success = "#3DDC97",
                  base_font = c("Segoe UI", "system-ui", "Helvetica Neue", "Arial", "sans-serif"))
dark_toggle <- if ("input_dark_mode" %in% getNamespaceExports("bslib"))
  bslib::input_dark_mode(id = "dark_mode", mode = "dark") else tags$span()

error_ui <- fluidPage(theme = theme, tags$head(tags$style(HTML(css))),
                      div(class = "empty-state", style = "margin:40px auto;max-width:760px;",
                          h3("The dashboard could not start"), p("Problem with the input data:"),
                          tags$pre(data_error %||% ""), p("Fix the path or columns at the top of the script and run it again.")))

if (is.null(data_error)) {
  sidebar_ui <- sidebar(
    id = "sidebar", width = 345, open = "desktop",
    div(class = "side-h", "Filters"),
    radioButtons("yr_mode", "Year selection", c("Year range" = "range", "Single year" = "single"), inline = TRUE),
    conditionalPanel("input.yr_mode == 'range'",
                     sliderInput("yr_range", "Period", min = ymin, max = ymax, value = c(ymin, ymax), step = 1, sep = "", ticks = FALSE),
                     selectInput("ref_year", "Year shown on maps, rankings (kept inside period)", choices = years, selected = default_year)),
    conditionalPanel("input.yr_mode == 'single'",
                     sliderInput("yr_single", "Year", min = ymin, max = ymax, value = default_year, step = 1, sep = "", ticks = FALSE)),
    selectizeInput("countries", "Countries (shared selection)", choices = country_choices, multiple = TRUE,
                   options = list(placeholder = "All countries \u2014 type to search", plugins = list("remove_button"))),
    radioButtons("sel_mode", "Country selection acts as", c("Highlight" = "highlight", "Filter data" = "filter"), inline = TRUE),
    radioButtons("metric", "Primary indicator (maps, rankings)",
                 c("Renewable energy share (%)" = "ren", "CO\u2082 per capita (t per person)" = "co2")),
    selectInput("map_pal", "Map colour scale", c("Teal-green", "Viridis", "Magma", "Cividis")),
    radioButtons("rank_dir", "Ranking direction", c("Top" = "top", "Bottom" = "bottom"), inline = TRUE),
    sliderInput("rank_n", "Countries in ranking", min = 5, max = 30, value = 15, step = 1, ticks = FALSE),
    sliderInput("min_obs", "Min. paired years per country (range mode)", min = 1,
                max = max(2, min(30, length(years))), value = 1, step = 1, ticks = FALSE),
    bslib::input_switch("log_co2", "Log scale for CO\u2082 (positive values only)", value = FALSE),
    actionButton("reset_all", "Reset all filters", icon = icon("rotate-left"), class = "btn-teal w-100 mt-2"),
    hr(), div(class = "side-h", "Active filters"), uiOutput("active_filters"))
  
  ui <- page_navbar(
    title = div(class = "brand", span(class = "brand-t", "Renewable Energy vs. CO\u2082 Emissions"),
                span(class = "brand-q", "Do countries with higher renewable energy shares tend to have lower CO\u2082 emissions per person?")),
    id = "main_nav", theme = theme, sidebar = sidebar_ui, bg = COL_NAVY, inverse = TRUE, fillable = FALSE,
    window_title = "Renewables vs CO2", header = tags$head(tags$style(HTML(css))),
    
    nav_panel("Overview", icon = icon("gauge-high"),
              uiOutput("kpis"),
              layout_columns(col_widths = c(7, 5),
                             card(card_header("Automated analytical summary"), uiOutput("auto_summary")),
                             card(card_header("Data quality & coverage"), uiOutput("dq_panel"))),
              card(card_header("What is being shown? (current filter logic)"), uiOutput("filter_logic"))),
    
    nav_panel("World map", icon = icon("earth-americas"),
              card(full_screen = TRUE,
                   card_header(class = "d-flex justify-content-between align-items-center",
                               textOutput("map_title", inline = TRUE),
                               actionButton("map_reset", "Reset view", class = "btn-sm btn-outline-info")),
                   spin(plotlyOutput("map", height = "560px")), uiOutput("map_note"))),
    
    nav_panel("Scatter & regression", icon = icon("chart-line"),
              card(layout_column_wrap(width = "210px",
                                      radioButtons("sc_method", "Headline correlation", c("Pearson" = "pearson", "Spearman" = "spearman"), inline = TRUE),
                                      bslib::input_switch("sc_fit", "Show OLS line + 95% CI band", value = TRUE),
                                      selectizeInput("sc_search", "Find a country", choices = c("None" = "", country_choices), selected = "",
                                                     options = list(placeholder = "Type to search\u2026")),
                                      div(style = "padding-top:24px", actionButton("sc_reset", "Reset zoom", class = "btn-sm btn-outline-info")))),
              layout_columns(col_widths = c(9, 3),
                             card(full_screen = TRUE, spin(plotlyOutput("scatter", height = "580px")), uiOutput("sc_note")),
                             card(card_header("Statistics (exact points shown)"), uiOutput("sc_stats")))),
    
    nav_panel("Trends", icon = icon("chart-simple"),
              card(layout_column_wrap(width = "200px",
                                      radioButtons("tr_view", "View", c("Aggregate" = "aggregate", "By region" = "region", "Selected countries" = "country")),
                                      radioButtons("tr_ind", "Indicator", c("Both (aligned panels)" = "both", "Renewable share" = "ren", "CO\u2082 per capita" = "co2")),
                                      div(bslib::input_switch("tr_index", "Compare relative change (index = 100)", value = FALSE),
                                          bslib::input_switch("tr_smooth", "LOESS smoothing (descriptive)", value = FALSE),
                                          bslib::input_switch("tr_corr", "Annual correlation panel", value = FALSE)),
                                      numericInput("tr_minn", "Min. observations per year", value = 5, min = 1, max = 100, step = 1))),
              card(full_screen = TRUE, spin(plotlyOutput("trend", height = "640px")), uiOutput("tr_note"))),
    
    nav_panel("Hexbin map", icon = icon("draw-polygon"),
              card(full_screen = TRUE,
                   card_header(class = "d-flex justify-content-between align-items-center gap-3",
                               textOutput("hex_title", inline = TRUE),
                               div(class = "d-flex gap-2 align-items-center",
                                   selectInput("hex_size", NULL, c("Fine (~500 km)" = 500, "Medium (~700 km)" = 700, "Coarse (~1000 km)" = 1000),
                                               selected = 700, width = "170px"),
                                   actionButton("hex_reset", "Reset view", class = "btn-sm btn-outline-info"))),
                   spin(plotlyOutput("hexmap", height = "560px")), uiOutput("hex_note"))),
    
    nav_panel("Rankings", icon = icon("ranking-star"),
              card(layout_column_wrap(width = "220px",
                                      radioButtons("rk_layout", "Layout", c("Selected ranking" = "single", "All four rankings" = "four"), inline = TRUE),
                                      div(style = "padding-top:24px", actionButton("rk_add", "Add displayed countries to selection", class = "btn-sm btn-teal")),
                                      div(style = "padding-top:24px", actionButton("rk_clear", "Clear selection", class = "btn-sm btn-outline-info")))),
              conditionalPanel("input.rk_layout == 'single'",
                               card(full_screen = TRUE, spin(plotlyOutput("rank_main", height = "560px")))),
              conditionalPanel("input.rk_layout == 'four'",
                               layout_columns(col_widths = c(6, 6),
                                              card(spin(plotlyOutput("rk_p1", height = "430px"))), card(spin(plotlyOutput("rk_p2", height = "430px"))),
                                              card(spin(plotlyOutput("rk_p3", height = "430px"))), card(spin(plotlyOutput("rk_p4", height = "430px"))))),
              card(card_header(class = "d-flex justify-content-between align-items-center",
                               "Country comparison table (follows year & selection filters)",
                               actionButton("tbl_add", "Add selected rows to country selection", class = "btn-sm btn-teal")),
                   DT::DTOutput("tbl"))),
    
    nav_panel("Change analysis", icon = icon("arrows-left-right"),
              card(layout_column_wrap(width = "200px",
                                      selectInput("ch_a", "Start year", choices = years, selected = ch_a_def),
                                      selectInput("ch_b", "End year", choices = years, selected = ch_b_def))),
              layout_columns(col_widths = c(9, 3),
                             card(full_screen = TRUE, spin(plotlyOutput("change", height = "580px"))),
                             card(card_header("Statistics of the changes"), uiOutput("ch_stats")))),
    
    nav_panel("Methodology", icon = icon("book"),
              card(card_header("Methodology, definitions and limitations"), uiOutput("method_panel"))),
    nav_spacer(), nav_item(dark_toggle)
  )
} else ui <- error_ui

## ---- 4. SERVER -----------------------------------------------------
server <- function(input, output, session) {
  if (!is.null(data_error)) return(invisible(NULL))
  
  is_dark <- reactive(identical(input$dark_mode, "dark"))
  log_on  <- reactive(isTRUE(input$log_co2))
  
  # ---- shared reactive data ------------------------------------------
  yr_sel <- reactive({
    req(input$yr_mode)
    if (input$yr_mode == "single") { y <- as.integer(input$yr_single); c(y, y) } else sort(as.integer(input$yr_range))
  })
  ref_year <- reactive({
    r <- yr_sel(); if (r[1] == r[2]) return(r[1])
    req(input$ref_year); min(max(as.integer(input$ref_year), r[1]), r[2])
  })
  period_label <- reactive({ r <- yr_sel(); if (r[1] == r[2]) as.character(r[1]) else paste0(r[1], "\u2013", r[2]) })
  
  period_dat <- reactive({
    r <- yr_sel(); d <- dat[dat$year >= r[1] & dat$year <= r[2], , drop = FALSE]
    if (r[1] != r[2] && isTRUE(input$min_obs > 1)) {
      keep <- d |> group_by(iso) |> summarise(np = sum(paired), .groups = "drop") |> filter(np >= input$min_obs)
      d <- d[d$iso %in% keep$iso, , drop = FALSE]
    }
    d
  })
  sel_dat <- reactive({
    d <- period_dat()
    if (identical(input$sel_mode, "filter") && length(input$countries)) d <- d[d$country %in% input$countries, , drop = FALSE]
    d
  })
  ref_dat <- reactive({ d <- period_dat(); d[d$year == ref_year(), , drop = FALSE] })
  
  add_countries <- function(cs) updateSelectizeInput(session, "countries", selected = union(input$countries, cs))
  toggle_country <- function(cn) {
    cur <- input$countries
    updateSelectizeInput(session, "countries", selected = if (cn %in% cur) setdiff(cur, cn) else c(cur, cn))
  }
  
  # ---- reset ----------------------------------------------------------
  observeEvent(input$reset_all, {
    updateRadioButtons(session, "yr_mode", selected = "range")
    updateSliderInput(session, "yr_range", value = c(ymin, ymax))
    updateSliderInput(session, "yr_single", value = default_year)
    updateSelectInput(session, "ref_year", selected = default_year)
    updateSelectizeInput(session, "countries", selected = character(0))
    updateRadioButtons(session, "sel_mode", selected = "highlight")
    updateRadioButtons(session, "metric", selected = "ren")
    updateSelectInput(session, "map_pal", selected = "Teal-green")
    updateRadioButtons(session, "rank_dir", selected = "top")
    updateSliderInput(session, "rank_n", value = 15)
    updateSliderInput(session, "min_obs", value = 1)
    bslib::update_switch("log_co2", value = FALSE, session = session)
    updateRadioButtons(session, "sc_method", selected = "pearson")
    bslib::update_switch("sc_fit", value = TRUE, session = session)
    updateSelectizeInput(session, "sc_search", selected = "")
    updateRadioButtons(session, "tr_view", selected = "aggregate")
    updateRadioButtons(session, "tr_ind", selected = "both")
    bslib::update_switch("tr_index", value = FALSE, session = session)
    bslib::update_switch("tr_smooth", value = FALSE, session = session)
    bslib::update_switch("tr_corr", value = FALSE, session = session)
    updateNumericInput(session, "tr_minn", value = 5)
    updateSelectInput(session, "hex_size", selected = 700)
    updateRadioButtons(session, "rk_layout", selected = "single")
    updateSelectInput(session, "ch_a", selected = ch_a_def)
    updateSelectInput(session, "ch_b", selected = ch_b_def)
  })
  
  output$active_filters <- renderUI({
    r <- yr_sel(); d <- sel_dat(); chip <- function(x) span(class = "chip", x)
    div(chip(paste("Period:", period_label())),
        if (r[1] != r[2]) chip(paste("Map/ranking year:", ref_year())),
        chip(if (length(input$countries)) paste0(length(input$countries), " countries ",
                                                 if (identical(input$sel_mode, "filter")) "(filter)" else "(highlight)") else "All countries"),
        chip(paste("Indicator:", metric_short(input$metric))),
        if (r[1] != r[2] && isTRUE(input$min_obs > 1)) chip(paste("\u2265", input$min_obs, "paired years")),
        if (log_on()) chip("CO\u2082 log scale"),
        chip(paste(fmt_int(nrow(d)), "rows in view")))
  })
  
  # ---- Overview -------------------------------------------------------
  output$kpis <- renderUI({
    d <- sel_dat()
    if (nrow(d) == 0) return(div(class = "empty-state", "No observations match the current filters. Widen the year range or clear the country filter."))
    p <- d[d$paired, , drop = FALSE]; st <- corr_stats(p$ren, p$co2)
    kpi <- function(l, v, s) div(class = "kpi", div(class = "lbl", l), div(class = "val", v), div(class = "sub", s))
    nyr <- length(unique(d$year))
    div(class = "kpi-grid",
        kpi("Countries", fmt_int(n_distinct(d$iso)), "with \u22651 indicator in view"),
        kpi("Valid country-year obs.", fmt_int(nrow(p)), paste0("both indicators present (of ", fmt_int(nrow(d)), " rows)")),
        kpi("Period", period_label(), if (nyr == 1) "single year" else paste(nyr, "years with data")),
        kpi("Avg renewable share", { v <- stat_or_na(mean, d$ren); if (is.na(v)) "n/a" else sprintf("%.1f%%", v) },
            paste0("mean of ", fmt_int(sum(!is.na(d$ren))), " available values")),
        kpi("Median CO\u2082 per capita", { v <- stat_or_na(stats::median, d$co2); if (is.na(v)) "n/a" else sprintf("%.2f t", v) },
            paste0("median of ", fmt_int(sum(!is.na(d$co2))), " available values")),
        kpi("Pearson r", if (st$valid) sprintf("%.2f", st$r) else "n/a",
            if (st$valid) paste0("p ", fmt_p(st$r_p), " \u00B7 n = ", fmt_int(st$n)) else st$reason %||% ""),
        kpi("Both indicators available", sprintf("%.1f%%", 100 * nrow(p) / nrow(d)), "share of rows in view"))
  })
  
  output$auto_summary <- renderUI({
    d <- sel_dat(); p <- d[d$paired, , drop = FALSE]; st <- corr_stats(p$ren, p$co2)
    if (!st$valid) return(div(class = "empty-state", paste0("No correlation can be reported: ", st$reason %||% "no data", ".")))
    dirw <- function(r) if (r < 0) "lower" else "higher"
    mixed <- sign(st$r) != sign(st$rho) || abs(abs(st$r) - abs(st$rho)) > 0.2
    tagList(
      p(sprintf("For %s, %s paired country-year observations from %s countries are available.",
                period_label(), fmt_int(st$n), fmt_int(n_distinct(p$iso)))),
      p(HTML(sprintf("Pearson <b>r = %.2f</b> (p %s) indicates a <b>%s</b> linear association; Spearman <b>\u03C1 = %.2f</b> (p %s) indicates a <b>%s</b> rank association. Observations with higher renewable shares tend to have <b>%s</b> CO\u2082 per capita in these data.",
                     st$r, fmt_p(st$r_p), strength_label(st$r), st$rho, fmt_p(st$rho_p), strength_label(st$rho), dirw(st$r)))),
      if (mixed) p("The two coefficients differ noticeably, which can signal a non-linear relationship or influential outliers."),
      p(class = "note", "This describes association only and does not show that renewable energy causes lower emissions. Country-years from the same country are not independent, so p-values for pooled periods are optimistic."))
  })
  
  output$dq_panel <- renderUI({
    li <- function(...) tags$li(...)
    tags$ul(
      li(sprintf("%s rows in CSV; %s rows kept (%s countries, %d\u2013%d).", fmt_int(rpt$rows_raw), fmt_int(rpt$rows_final), rpt$countries, rpt$year_min, rpt$year_max)),
      li(sprintf("%s rows with invalid/missing ISO-3 codes (%s entities, e.g. regional aggregates) excluded from country views.", fmt_int(rpt$invalid_iso_rows), rpt$invalid_iso_entities)),
      li(sprintf("%s rows with unusable year/country; %s rows missing both indicators dropped.", fmt_int(rpt$bad_rows), fmt_int(rpt$rows_both_missing))),
      li(sprintf("Duplicate country-year keys: %s%s.", fmt_int(rpt$dup_keys), if (rpt$dup_keys > 0) " (collapsed by mean)" else "")),
      li(sprintf("Set to missing: %s renewable shares outside 0\u2013100, %s negative CO\u2082 values.", fmt_int(rpt$ren_out_of_range), fmt_int(rpt$co2_negative))),
      li(sprintf("Remaining missing: %s renewable-share values, %s CO\u2082 values.", fmt_int(rpt$miss_ren), fmt_int(rpt$miss_co2))),
      li(if (is.null(world)) paste("Map geometry unavailable:", world_note %||% "") else
        paste0("Map geometry loaded (", nrow(world$ll), " polygons); hexagon projection: ", world$proj %||% "unavailable", ".")))
  })
  
  output$filter_logic <- renderUI({
    r <- yr_sel()
    tags$ul(
      tags$li(HTML(sprintf("<b>Period:</b> %s. %s", period_label(), if (r[1] == r[2]) "One year, so each country appears once." else sprintf("Charts using all years show country-year points; maps, rankings and the hexbin map show the single year %d.", ref_year())))),
      tags$li(HTML(sprintf("<b>Indicator for maps/rankings:</b> %s. Only countries with a value in the displayed year are ranked or coloured.", metric_label(input$metric)))),
      tags$li(HTML(sprintf("<b>Country selection</b> currently acts as a <b>%s</b> (%s).", if (identical(input$sel_mode, "filter")) "filter" else "highlight",
                           if (length(input$countries)) paste(length(input$countries), "selected") else "none selected"))),
      tags$li(HTML(if (r[1] != r[2]) sprintf("<b>Data quality:</b> countries need at least %d paired years in the period.", input$min_obs %||% 1) else "<b>Data quality filter:</b> only applies to year-range mode.")),
      tags$li("Click a point or bar in the scatterplot, change plot or rankings to add or remove that country from the shared selection."))
  })
  
  # ---- Map ------------------------------------------------------------
  map_world <- function() if (!is.null(world$eq)) world$eq else world$ll
  output$map_title <- renderText(paste0(metric_label(input$metric), " \u00B7 ", ref_year(), if (log_on() && input$metric == "co2") " \u00B7 log10 colour scale" else ""))
  
  output$map <- renderPlotly({
    dark <- is_dark()
    safe_plot({
      m <- input$metric; yr <- ref_year()
      rd <- ref_dat(); rd <- rd[!is.na(rd[[m]]), , drop = FALSE]
      if (!nrow(rd)) stop_msg(paste0("No ", metric_short(m), " values are available for ", yr, ". Try another year."))
      rd <- data.frame(iso = rd$iso, country = rd$country, value = rd[[m]], stringsAsFactors = FALSE)
      rd <- rd[!duplicated(rd$iso), ]
      leg <- if (m == "ren") "Renewable share (%)" else if (log_on()) "log10 CO\u2082 (t/person)" else "CO\u2082 (t/person)"
      if (is.null(world)) {
        rd$fill <- fill_val(rd$value, m, log_on())
        rd$tip <- paste0("<b>", rd$country, "</b><br>Year: ", yr, "<br>", metric_short(m), ": ", fmt_val(m, rd$value))
        make_geo_fallback(rd, leg, dark)
      } else {
        mw <- left_join(map_world(), rd, by = "iso")
        mw$fill <- fill_val(mw$value, m, log_on())
        if (!any(!is.na(mw$fill))) stop_msg("No positive values to show on a log scale for this year.")
        mw$tip <- ifelse(is.na(mw$value), paste0("<b>", mw$name, "</b><br>No ", metric_short(m), " data for ", yr),
                         paste0("<b>", mw$country, "</b><br>Year: ", yr, "<br>", metric_short(m), ": ", fmt_val(m, mw$value)))
        sel <- input$countries
        out <- if (length(sel)) mw[!is.na(mw$country) & mw$country %in% sel, ] else NULL
        make_sf_map(mw, leg, input$map_pal, dark, out)
      }
    }, dark)
  })
  observeEvent(input$map_reset, {
    plotlyProxy("map", session) |> plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
  })
  output$map_note <- renderUI({
    m <- input$metric; rd <- ref_dat(); rd <- rd[!is.na(rd[[m]]), , drop = FALSE]
    if (is.null(world)) return(div(class = "note", paste("Country polygons could not be loaded (", world_note %||% "", "). Showing plotly's built-in ISO-3 choropleth instead.")))
    unm <- rd[!(rd$iso %in% world$ll$iso), ]
    nod <- sum(!(world$ll$iso %in% rd$iso))
    div(class = "note", HTML(sprintf("%s countries coloured for %d; <b>grey</b> = no value (%s map polygons). Join key: ISO-3 (Natural Earth special codes such as France/Norway repaired). %s Amber outline = selected countries.",
                                     nrow(rd) - nrow(unm), ref_year(), nod,
                                     if (nrow(unm)) sprintf("%d countries with data have no polygon in the map and are not drawn: %s.", nrow(unm), paste(head(unm$country, 12), collapse = ", ")) else "All countries with data matched a polygon.")))
  })
  
  # ---- Scatter --------------------------------------------------------
  sc_prep <- reactive({
    d <- sel_dat(); d <- d[d$paired, , drop = FALSE]; dropped <- 0L
    if (log_on()) { dropped <- sum(d$co2 <= 0); d <- d[d$co2 > 0, , drop = FALSE] }
    d$y <- if (log_on()) log10(d$co2) else d$co2
    list(d = d, dropped = dropped, st = corr_stats(d$ren, d$y))
  })
  
  output$scatter <- renderPlotly({
    dark <- is_dark()
    safe_plot({
      s <- sc_prep(); d <- s$d; st <- s$st
      if (nrow(d) == 0) stop_msg("No paired observations (both indicators available) for the current filters.")
      ol <- if (dark) "rgba(255,255,255,0.55)" else "rgba(11,31,58,0.55)"
      d$hover <- paste0("<b>", d$country, "</b> (", d$year, ")<br>Renewable share: ", sprintf("%.1f%%", d$ren),
                        "<br>CO\u2082 per capita: ", sprintf("%.2f t", d$co2), "<br>Region: ", d$region)
      p <- plot_ly(colors = region_pal, source = "scatter")
      if (isTRUE(input$sc_fit) && st$valid) {
        fit <- stats::lm(y ~ ren, data = d)
        xs <- seq(min(d$ren), max(d$ren), length.out = 120)
        pr <- stats::predict(fit, newdata = data.frame(ren = xs), interval = "confidence", level = 0.95)
        tr <- if (log_on()) function(v) 10^v else identity
        p <- p |>
          add_ribbons(x = xs, ymin = tr(pr[, "lwr"]), ymax = tr(pr[, "upr"]), name = "95% CI (mean fit)",
                      fillcolor = "rgba(255,183,3,0.22)", line = list(color = "transparent"), hoverinfo = "skip") |>
          add_lines(x = xs, y = tr(pr[, "fit"]), name = "OLS fit", line = list(color = COL_AMBER, width = 3), hoverinfo = "skip")
      }
      if (length(unique(d$region)) > 1) {
        p <- p |> add_markers(data = d, x = ~ren, y = ~co2, color = ~region, key = ~country, text = ~hover, hoverinfo = "text",
                              marker = list(size = 8, opacity = 0.75, line = list(width = 0.6, color = ol)))
      } else {
        p <- p |> add_markers(data = d, x = ~ren, y = ~co2, key = ~country, text = ~hover, hoverinfo = "text", name = "Observations",
                              marker = list(size = 8, opacity = 0.75, color = COL_TEAL, line = list(width = 0.6, color = ol)))
      }
      sel <- input$countries
      if (length(sel)) {
        ds <- d[d$country %in% sel, , drop = FALSE]
        if (nrow(ds)) p <- p |> add_markers(data = ds, x = ~ren, y = ~co2, key = ~country, text = ~hover, hoverinfo = "text",
                                            name = "Selected countries",
                                            marker = list(size = 14, color = "rgba(0,0,0,0)", line = list(color = COL_AMBER, width = 2.5)))
      }
      q <- input$sc_search
      if (!is.null(q) && nzchar(q)) {
        dq <- d[d$country == q, , drop = FALSE]
        if (nrow(dq)) {
          dq <- dq[dq$year == max(dq$year), , drop = FALSE]
          p <- p |> add_trace(data = dq, x = ~ren, y = ~co2, type = "scatter", mode = "markers+text", text = ~country,
                              textposition = "top center", hoverinfo = "skip", name = q,
                              marker = list(symbol = "star", size = 17, color = COL_AMBER, line = list(color = "#FFFFFF", width = 1)))
        }
      }
      headline <- if (!st$valid) paste0("Statistics unavailable: ", st$reason) else if (identical(input$sc_method, "spearman"))
        sprintf("Spearman \u03C1 = %.2f (%s) \u00B7 n = %s", st$rho, strength_label(st$rho), fmt_int(st$n)) else
          sprintf("Pearson r = %.2f (%s) \u00B7 n = %s%s", st$r, strength_label(st$r), fmt_int(st$n), if (log_on()) " (on log10 CO\u2082)" else "")
      p |>
        layout(title = list(text = paste0("Renewable share vs CO\u2082 per capita \u00B7 ", period_label(),
                                          if (length(unique(yr_sel())) == 1) " (one point per country)" else " (one point per country-year)"),
                            x = 0.01, font = list(size = 15)),
               xaxis = list(title = "Renewable energy share (%)", rangemode = "tozero"),
               yaxis = list(title = paste0("CO\u2082 emissions per capita (t per person)", if (log_on()) " \u2014 log scale" else ""),
                            type = if (log_on()) "log" else "linear", rangemode = if (log_on()) "normal" else "tozero"),
               annotations = list(list(x = 0.99, y = 0.99, xref = "paper", yref = "paper", xanchor = "right", yanchor = "top",
                                       showarrow = FALSE, align = "right", font = list(size = 12),
                                       text = paste0("<b>", headline, "</b><br>Association only \u2014 not evidence of causation"))),
               legend = list(orientation = "h", y = -0.17), margin = list(t = 60)) |>
        event_register("plotly_click") |> pl_style(dark)
    }, dark)
  })
  observeEvent(input$sc_reset, {
    plotlyProxy("scatter", session) |> plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
  })
  observeEvent(suppressWarnings(event_data("plotly_click", source = "scatter")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "scatter"))
    message("SCATTER CLICK received. key = ", paste(ev$key, collapse = ", "))
    if (!is.null(ev$key) && length(ev$key)) toggle_country(as.character(ev$key[[1]]))
  })
  output$sc_stats <- renderUI({
    s <- sc_prep(); st <- s$st
    if (nrow(s$d) == 0) return(div(class = "empty-state", "No paired observations."))
    if (!st$valid) return(div(class = "empty-state", paste0("Statistics not shown: ", st$reason, " (n = ", st$n, ").")))
    row <- function(k, v) tags$tr(tags$th(k), tags$td(v))
    slope_txt <- if (log_on()) sprintf("%+.1f%% in CO\u2082 per +1 pp (b = %.4f on log10)", 100 * (10^st$slope - 1), st$slope) else sprintf("%+.3f t per +1 pp", st$slope)
    tags$table(class = "table table-sm stat-table", tags$tbody(
      row("Sample size (n)", fmt_int(st$n)),
      row(if (log_on()) "Pearson r (log10 CO\u2082)" else "Pearson r", sprintf("%.3f (p %s)", st$r, fmt_p(st$r_p))),
      row("Spearman \u03C1", sprintf("%.3f (p %s)", st$rho, fmt_p(st$rho_p))),
      row("OLS slope", slope_txt), row("Slope p-value", fmt_p(st$slope_p)),
      row("R\u00B2", sprintf("%.3f", st$r2)),
      row("Strength (headline)", if (identical(input$sc_method, "spearman")) strength_label(st$rho) else strength_label(st$r))))
  })
  output$sc_note <- renderUI({
    s <- sc_prep(); q <- input$sc_search
    msgs <- c(
      if (s$dropped > 0) sprintf("Log scale: %d observations with CO\u2082 \u2264 0 are omitted; statistics use log10(CO\u2082).", s$dropped),
      if (!is.null(q) && nzchar(q) && !(q %in% s$d$country)) paste0(q, " has no paired observation in the current view."),
      if (s$st$n > 0 && s$st$n < 10) "Small sample: interpret statistics with great caution.",
      if (length(unique(yr_sel())) > 1) "Pooled country-years are not independent; p-values are optimistic. Correlation does not imply causation.",
      if (all(s$d$region == "Unclassified")) "Region colours unavailable (map data not loaded); single colour used.")
    div(class = "note", paste(c(msgs, "Statistics are calculated from exactly the points plotted. Click a point to add/remove a country."), collapse = " "))
  })
  
  # ---- Trends ---------------------------------------------------------
  tr_prep <- reactive({
    r <- yr_sel()
    if (r[1] == r[2]) return(list(ok = FALSE, msg = "Trend analysis needs at least two years. Set 'Year selection' to 'Year range'."))
    view <- input$tr_view %||% "aggregate"; note <- NULL
    if (view == "country") {
      cs <- input$countries
      pdat <- period_dat()
      if (!length(cs)) {
        cnt <- pdat |> count(country, sort = TRUE); cs <- head(cnt$country, 6)
        note <- "No countries selected: showing the 6 countries with the most observations in the period."
      }
      d <- pdat[pdat$country %in% cs, , drop = FALSE]; d$grp <- d$country
    } else {
      d <- sel_dat(); d$grp <- if (view == "region") as.character(d$region) else "All countries in view"
    }
    if (!nrow(d)) return(list(ok = FALSE, msg = "No observations for this selection."))
    minn <- max(1L, as.integer(input$tr_minn %||% 5))
    agg <- d |> group_by(grp, year) |>
      summarise(n_ren = sum(!is.na(ren)), ren_v = if (n_ren > 0) mean(ren, na.rm = TRUE) else NA_real_,
                n_co2 = sum(!is.na(co2)), co2_v = if (n_co2 > 0) stats::median(co2, na.rm = TRUE) else NA_real_, .groups = "drop")
    agg <- as.data.frame(agg); hidden <- 0L
    if (view != "country") {
      hr <- agg$n_ren < minn; hc <- agg$n_co2 < minn
      hidden <- sum(hr | hc); agg$ren_v[hr] <- NA_real_; agg$co2_v[hc] <- NA_real_
    }
    list(ok = TRUE, agg = agg, d = d, view = view, note = note, hidden = hidden, minn = minn)
  })
  
  trend_cols <- function(groups) {
    base <- c("#2AB7CA", "#3DDC97", "#FFB703", "#CC79A7", "#56B4E9", "#D55E00", "#7F8CFF", "#E69F00", "#9AA5B1")
    cols <- setNames(rep_len(base, length(groups)), groups)
    hit <- intersect(groups, names(region_pal)); cols[hit] <- region_pal[hit]; cols
  }
  line_panel <- function(agg, vcol, ncol_, ylab, tiplab, cols, legend, smooth, ylog, fmt) {
    p <- plot_ly()
    for (g in unique(agg$grp)) {
      a <- agg[agg$grp == g & !is.na(agg[[vcol]]), , drop = FALSE]; a <- a[order(a$year), ]
      if (!nrow(a)) next
      tip <- paste0("<b>", g, "</b><br>Year: ", a$year, "<br>", tiplab, ": ", fmt(a[[vcol]]), "<br>Observations: ", a[[ncol_]])
      p <- p |> add_trace(x = a$year, y = a[[vcol]], type = "scatter", mode = "lines+markers", name = g, legendgroup = g,
                          showlegend = legend, text = tip, hoverinfo = "text",
                          line = list(color = cols[[g]], width = 2.2), marker = list(color = cols[[g]], size = 5))
      if (smooth && nrow(a) >= 8) {
        a$.y <- a[[vcol]]
        lo <- tryCatch(stats::loess(.y ~ year, data = a, span = 0.75), error = function(e) NULL)
        if (!is.null(lo)) p <- p |> add_trace(x = a$year, y = stats::predict(lo), type = "scatter", mode = "lines",
                                              line = list(color = cols[[g]], width = 1.6, dash = "dot"),
                                              name = paste(g, "(LOESS)"), legendgroup = g, showlegend = FALSE, hoverinfo = "skip")
      }
    }
    p |> layout(yaxis = list(title = ylab, type = if (ylog) "log" else "linear"))
  }
  
  output$trend <- renderPlotly({
    dark <- is_dark()
    safe_plot({
      tp <- tr_prep(); if (!tp$ok) stop_msg(tp$msg)
      agg <- tp$agg; cols <- trend_cols(sort(unique(agg$grp)))
      ind <- input$tr_ind %||% "both"; sm <- isTRUE(input$tr_smooth)
      lg <- log_on(); panels <- list()
      if (isTRUE(input$tr_index)) {
        p <- plot_ly()
        for (g in unique(agg$grp)) for (k in c("ren", "co2")) {
          if (ind != "both" && ind != k) next
          a <- agg[agg$grp == g & !is.na(agg[[paste0(k, "_v")]]), , drop = FALSE]; a <- a[order(a$year), ]
          if (!nrow(a)) next
          v <- a[[paste0(k, "_v")]]; base <- v[1]
          if (is.na(base) || base == 0) next
          p <- p |> add_trace(x = a$year, y = 100 * v / base, type = "scatter", mode = "lines+markers",
                              name = paste0(g, if (k == "ren") " \u2014 renewable share" else " \u2014 CO\u2082"),
                              text = paste0("<b>", g, "</b><br>Year: ", a$year, "<br>Index: ", sprintf("%.1f", 100 * v / base),
                                            "<br>Level: ", if (k == "ren") sprintf("%.1f%%", v) else sprintf("%.2f t", v), "<br>Base year: ", a$year[1]),
                              hoverinfo = "text", line = list(color = cols[[g]], width = 2.2, dash = if (k == "ren") "solid" else "dash"),
                              marker = list(color = cols[[g]], size = 5))
        }
        panels[[1]] <- p |> layout(yaxis = list(title = "Index (first available year in view = 100)"))
      } else {
        first <- TRUE
        if (ind %in% c("both", "ren")) {
          panels[[length(panels) + 1]] <- line_panel(agg, "ren_v", "n_ren", "Mean renewable share (%)", "Mean renewable share", cols, first, sm, FALSE, function(v) sprintf("%.1f%%", v)); first <- FALSE }
        if (ind %in% c("both", "co2")) {
          a2 <- agg; if (lg) a2$co2_v[!is.na(a2$co2_v) & a2$co2_v <= 0] <- NA_real_
          panels[[length(panels) + 1]] <- line_panel(a2, "co2_v", "n_co2", paste0("Median CO\u2082 per capita (t)", if (lg) " \u2014 log" else ""), "Median CO\u2082 per capita", cols, first, sm, lg, function(v) sprintf("%.2f t", v)) }
      }
      if (isTRUE(input$tr_corr)) {
        dd <- sel_dat(); dd <- dd[dd$paired, , drop = FALSE]
        cy <- do.call(rbind, lapply(sort(unique(dd$year)), function(y) {
          z <- dd[dd$year == y, ]; if (nrow(z) < max(tp$minn, 5)) return(NULL)
          s <- corr_stats(z$ren, z$co2); if (!s$valid) return(NULL)
          data.frame(year = y, n = s$n, pearson = s$r, spearman = s$rho) }))
        if (is.null(cy) || !nrow(cy)) {
          showNotification("Annual correlation: no year has enough paired observations.", type = "warning", duration = 4)
        } else {
          pc <- plot_ly() |>
            add_trace(x = range(cy$year), y = c(0, 0), type = "scatter", mode = "lines", hoverinfo = "skip", showlegend = FALSE, line = list(color = "gray", dash = "dot", width = 1)) |>
            add_trace(x = cy$year, y = cy$pearson, type = "scatter", mode = "lines+markers", name = "Pearson r", legendgroup = "corr",
                      text = paste0("Year ", cy$year, "<br>Pearson r = ", sprintf("%.2f", cy$pearson), "<br>n = ", cy$n), hoverinfo = "text",
                      line = list(color = COL_TEAL, width = 2), marker = list(color = COL_TEAL, size = 5)) |>
            add_trace(x = cy$year, y = cy$spearman, type = "scatter", mode = "lines+markers", name = "Spearman \u03C1", legendgroup = "corr",
                      text = paste0("Year ", cy$year, "<br>Spearman \u03C1 = ", sprintf("%.2f", cy$spearman), "<br>n = ", cy$n), hoverinfo = "text",
                      line = list(color = COL_AMBER, width = 2, dash = "dash"), marker = list(color = COL_AMBER, size = 5)) |>
            layout(yaxis = list(title = "Annual correlation", range = c(-1, 1)))
          panels[[length(panels) + 1]] <- pc
        }
      }
      if (!length(panels)) stop_msg("Nothing to display for the current trend options.")
      n <- length(panels)
      p <- if (n == 1) panels[[1]] else subplot(panels, nrows = n, shareX = TRUE, titleY = TRUE, margin = 0.04)
      axn <- paste0("xaxis", if (n > 1) n else "")
      p <- do.call(layout, c(list(p), setNames(list(list(title = "Year")), axn)))
      p |> layout(legend = list(orientation = "h", y = -0.1), hovermode = "closest",
                  title = list(text = paste("Year-by-year trends \u00B7", period_label()), x = 0.01, font = list(size = 15))) |> pl_style(dark)
    }, dark)
  })
  output$tr_note <- renderUI({
    tp <- tr_prep(); if (!tp$ok) return(NULL)
    div(class = "note", paste(c(tp$note,
                                "Aggregates use only the observations available in each year: renewable share = mean across countries, CO\u2082 per capita = median across countries. Countries entering or leaving the data change the composition, so changes in an aggregate are not purely within-country changes.",
                                if (tp$view != "country") sprintf("Year-group points with fewer than %d observations are hidden (%d hidden).", tp$minn, tp$hidden),
                                if (isTRUE(input$tr_index)) "Index view: each series is divided by its own first available value in the view (=100). It shows relative change only; percent changes from a small base can look large.",
                                if (isTRUE(input$tr_smooth)) "Dotted lines: LOESS (span 0.75), descriptive only, drawn only for series with \u2265 8 yearly points.",
                                "Separate aligned panels are used instead of a dual axis."), collapse = " "))
  })
  
  # ---- Hexbin map -----------------------------------------------------
  output$hex_title <- renderText(paste0("Hexagonal cell summary \u00B7 ", metric_short(input$metric), " \u00B7 ", ref_year()))
  hex_vals <- reactive({
    req(world$eq)
    m <- input$metric
    g <- withProgress(message = "Building hexagonal grid (first time only)\u2026", value = 0.4, get_hex(as.numeric(input$hex_size)))
    rd <- ref_dat(); rd <- rd[!is.na(rd[[m]]), , drop = FALSE]
    tab <- g$tab; tab$value <- rd[[m]][match(tab$iso, rd$iso)]
    st <- tab |> group_by(hex_id) |>
      summarise(n_land = n(), n_data = sum(!is.na(value)), cov = sum(area[!is.na(value)]) / sum(area),
                val = if (n_data > 0) stats::weighted.mean(value[!is.na(value)], area[!is.na(value)]) else NA_real_,
                labs = paste0(paste(head(name[order(-area)], 4), collapse = ", "), if (n() > 4) ", \u2026" else ""), .groups = "drop")
    list(hx = left_join(g$hex, st, by = "hex_id"), n_rd = nrow(rd))
  })
  
  output$hexmap <- renderPlotly({
    dark <- is_dark()
    safe_plot({
      m <- input$metric; yr <- ref_year()
      if (is.null(world$eq)) {
        rd <- ref_dat(); rd <- rd[!is.na(rd[[m]]), , drop = FALSE]
        if (!nrow(rd)) stop_msg("No data for this year.")
        df <- data.frame(iso = rd$iso, fill = fill_val(rd[[m]], m, log_on()),
                         tip = paste0("<b>", rd$country, "</b><br>", metric_short(m), ": ", fmt_val(m, rd[[m]])))
        make_geo_fallback(df, metric_short(m), dark)
      } else {
        hv <- hex_vals(); hx <- hv$hx
        if (hv$n_rd == 0) stop_msg(paste0("No ", metric_short(m), " values are available for ", yr, "."))
        hx$fill <- fill_val(hx$val, m, log_on())
        hx$tip <- ifelse(is.na(hx$val),
                         paste0("<b>Land cell \u2014 no data</b><br>No intersecting country has a ", metric_short(m), " value for ", yr, "<br>Countries touching cell: ", hx$labs),
                         paste0("<b>Hexagonal cell</b><br>Area-weighted mean ", metric_short(m), ": ", fmt_val(m, hx$val),
                                "<br>Countries with data / touching: ", hx$n_data, " / ", hx$n_land,
                                "<br>Land area with data: ", sprintf("%.0f%%", 100 * hx$cov), "<br>Countries: ", hx$labs))
        if (!any(!is.na(hx$fill))) stop_msg("No cell has a positive value to show on a log scale.")
        leg <- if (m == "ren") "Renewable share (%)" else if (log_on()) "log10 CO\u2082 (t/person)" else "CO\u2082 (t/person)"
        make_sf_map(hx, leg, input$map_pal, dark)
      }
    }, dark)
  })
  observeEvent(input$hex_reset, {
    plotlyProxy("hexmap", session) |> plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
  })
  output$hex_note <- renderUI({
    if (is.null(world$eq)) return(div(class = "note", "A projected hexagonal grid could not be built (no usable equal-area projection or map data). A country-level choropleth is shown instead."))
    hv <- tryCatch(hex_vals(), error = function(e) NULL); if (is.null(hv)) return(NULL)
    hx <- hv$hx
    div(class = "note", HTML(sprintf("Hexagons are equal-area cells (~%s km across) in the <b>%s</b> projection and summarise the <b>land they cover</b>. The source data has <b>one value per country, no sub-national locations</b>, so each cell takes the <b>area-weighted mean of the country values</b> that intersect it (country area inside the cell as weights); countries are not reduced to centroids. Cells may straddle borders and do not align with political boundaries; they do not show finer spatial precision than the country-level data. %d coloured cells; <b>grey</b> = land cell with no value; ocean-only cells are not drawn. Hover shows number of countries and share of cell land area with data. Antarctica excluded.",
                                     input$hex_size, world$proj, sum(!is.na(hx$val)))))
  })
  
  # ---- Rankings -------------------------------------------------------
  rank_data <- function(metric, dir) {
    d <- ref_dat(); d <- d[!is.na(d[[metric]]), , drop = FALSE]
    if (!nrow(d)) return(d)
    d$value <- d[[metric]]
    d <- if (dir == "top") d[order(-d$value, d$country), ] else d[order(d$value, d$country), ]
    head(d, input$rank_n %||% 15)
  }
  rank_plot <- function(metric, dir, src, dark) {
    yr <- ref_year(); d <- rank_data(metric, dir)
    if (!nrow(d)) stop_msg(paste0("No ", metric_short(metric), " values are available for ", yr, "."))
    n_avail <- sum(!is.na(ref_dat()[[metric]]))
    base <- if (metric == "ren") COL_GREEN else COL_TEAL
    d$col <- ifelse(d$country %in% input$countries, COL_AMBER, base)
    d$cf <- factor(d$country, levels = rev(d$country))
    d$lab <- if (metric == "ren") sprintf("%.1f%%", d$value) else sprintf("%.2f", d$value)
    d$hover <- paste0("<b>", d$country, "</b><br>Year: ", yr, "<br>", metric_short(metric), ": ", fmt_val(metric, d$value),
                      "<br>Rank ", seq_len(nrow(d)), " of ", n_avail, if (dir == "top") " (highest)" else " (lowest)")
    plot_ly(d, x = ~value, y = ~cf, type = "bar", orientation = "h", key = ~country, source = src,
            marker = list(color = d$col), text = ~lab, textposition = "outside", cliponaxis = FALSE,
            hovertext = ~hover, hoverinfo = "text") |>
      layout(title = list(text = sprintf("%s %d \u2014 %s (%d) \u00B7 %d countries with data", if (dir == "top") "Top" else "Bottom", nrow(d), metric_short(metric), yr, n_avail),
                          x = 0.01, font = list(size = 14)),
             xaxis = list(title = metric_label(metric), range = c(0, max(d$value, na.rm = TRUE) * 1.2)),
             yaxis = list(title = "", automargin = TRUE), margin = list(t = 50, r = 30)) |>
      event_register("plotly_click") |> pl_style(dark)
  }
  output$rank_main <- renderPlotly({ dark <- is_dark(); safe_plot(rank_plot(input$metric, input$rank_dir, "rank", dark), dark) })
  combos <- list(rk_p1 = c("ren", "top"), rk_p2 = c("ren", "bottom"), rk_p3 = c("co2", "top"), rk_p4 = c("co2", "bottom"))
  for (nm in names(combos)) local({
    id <- nm; cmb <- combos[[nm]]
    output[[id]] <- renderPlotly({ dark <- is_dark(); safe_plot(rank_plot(cmb[1], cmb[2], "rank_other", dark), dark) })
  })
  observeEvent(suppressWarnings(event_data("plotly_click", source = "rank")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "rank"))
    if (!is.null(ev$key) && length(ev$key)) toggle_country(as.character(ev$key[[1]]))
  })
  observeEvent(input$rk_add, {
    d <- rank_data(input$metric, input$rank_dir)
    if (nrow(d)) add_countries(d$country) else showNotification("No ranked countries to add.", type = "warning")
  })
  observeEvent(input$rk_clear, updateSelectizeInput(session, "countries", selected = character(0)))
  
  tbl_data <- reactive({
    d <- sel_dat(); d <- d[order(d$country, -d$year), ]
    data.frame(Country = d$country, ISO3 = d$iso, Region = as.character(d$region), Year = d$year,
               `Renewable share (%)` = d$ren, `CO2 per capita (t)` = d$co2, check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$tbl <- DT::renderDT({
    DT::datatable(tbl_data(), rownames = FALSE, selection = "multiple", filter = "top", class = "compact stripe hover",
                  options = list(pageLength = 12, order = list(list(3, "desc"), list(0, "asc")), dom = "lfrtip",
                                 language = list(emptyTable = "No observations match the current filters."))) |>
      DT::formatRound("Renewable share (%)", 1) |> DT::formatRound("CO2 per capita (t)", 2)
  })
  observeEvent(input$tbl_add, {
    idx <- input$tbl_rows_selected
    if (!length(idx)) { showNotification("Select one or more table rows first.", type = "warning"); return() }
    add_countries(unique(tbl_data()$Country[idx]))
  })
  
  # ---- Change analysis (6th visualization) ----------------------------
  ch_data <- reactive({
    a <- as.integer(input$ch_a); b <- as.integer(input$ch_b)
    if (is.na(a) || is.na(b) || a >= b) return(list(ok = FALSE, msg = "Choose a start year that is earlier than the end year."))
    da <- dat[dat$year == a & dat$paired, c("iso", "country", "region", "ren", "co2")]; names(da)[4:5] <- c("ren_a", "co2_a")
    db <- dat[dat$year == b & dat$paired, c("iso", "ren", "co2")]; names(db)[2:3] <- c("ren_b", "co2_b")
    m <- merge(da, db, by = "iso")          # one row per ISO in each table: no many-to-many join
    if (identical(input$sel_mode, "filter") && length(input$countries)) m <- m[m$country %in% input$countries, ]
    if (!nrow(m)) return(list(ok = FALSE, msg = sprintf("No country has both indicators in both %d and %d.", a, b)))
    m$d_ren <- m$ren_b - m$ren_a; m$d_co2 <- m$co2_b - m$co2_a
    m$quad <- ifelse(m$d_ren > 0 & m$d_co2 < 0, "Renewables up, CO\u2082 down",
                     ifelse(m$d_ren > 0, "Both up", ifelse(m$d_co2 < 0, "Both down", "Renewables down, CO\u2082 up")))
    list(ok = TRUE, d = m, a = a, b = b, st = corr_stats(m$d_ren, m$d_co2))
  })
  output$change <- renderPlotly({
    dark <- is_dark()
    safe_plot({
      cd <- ch_data(); if (!cd$ok) stop_msg(cd$msg)
      d <- cd$d; st <- cd$st; lc <- if (dark) "rgba(255,255,255,0.35)" else "rgba(11,31,58,0.35)"
      d$hover <- paste0("<b>", d$country, "</b><br>Renewable share: ", sprintf("%.1f%% \u2192 %.1f%%", d$ren_a, d$ren_b), " (", sprintf("%+.1f", d$d_ren), " pp)",
                        "<br>CO\u2082 per capita: ", sprintf("%.2f \u2192 %.2f t", d$co2_a, d$co2_b), " (", sprintf("%+.2f", d$d_co2), " t)")
      qpal <- c("Renewables up, CO\u2082 down" = COL_GREEN, "Both up" = COL_AMBER, "Both down" = COL_TEAL, "Renewables down, CO\u2082 up" = "#EF6F6C")
      p <- plot_ly(colors = qpal, source = "change")
      if (st$valid) {
        xs <- range(d$d_ren)
        p <- p |> add_lines(x = xs, y = st$intercept + st$slope * xs, name = "OLS fit", line = list(color = "#FFFFFF", width = 2, dash = "dash"), hoverinfo = "skip")
      }
      p <- p |> add_markers(data = d, x = ~d_ren, y = ~d_co2, color = ~quad, key = ~country, text = ~hover, hoverinfo = "text",
                            marker = list(size = 8, opacity = 0.8, line = list(width = 0.5, color = lc)))
      sel <- input$countries
      if (length(sel) && any(d$country %in% sel)) {
        ds <- d[d$country %in% sel, ]
        p <- p |> add_markers(data = ds, x = ~d_ren, y = ~d_co2, key = ~country, text = ~hover, hoverinfo = "text", name = "Selected countries",
                              marker = list(size = 14, color = "rgba(0,0,0,0)", line = list(color = COL_AMBER, width = 2.5)))
      }
      p |> layout(title = list(text = sprintf("Within-country change, %d \u2192 %d (n = %d countries)", cd$a, cd$b, nrow(d)), x = 0.01, font = list(size = 15)),
                  xaxis = list(title = "Change in renewable share (percentage points)"),
                  yaxis = list(title = "Change in CO\u2082 per capita (t per person)"),
                  shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, xref = "x", yref = "paper", line = list(color = lc, width = 1)),
                                list(type = "line", y0 = 0, y1 = 0, x0 = 0, x1 = 1, xref = "paper", yref = "y", line = list(color = lc, width = 1))),
                  legend = list(orientation = "h", y = -0.17), margin = list(t = 60)) |>
        event_register("plotly_click") |> pl_style(dark)
    }, dark)
  })
  observeEvent(suppressWarnings(event_data("plotly_click", source = "change")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "change"))
    if (!is.null(ev$key) && length(ev$key)) toggle_country(as.character(ev$key[[1]]))
  })
  output$ch_stats <- renderUI({
    cd <- ch_data(); if (!cd$ok) return(div(class = "empty-state", cd$msg))
    st <- cd$st; row <- function(k, v) tags$tr(tags$th(k), tags$td(v))
    tagList(
      if (st$valid) tags$table(class = "table table-sm stat-table", tags$tbody(
        row("Countries (n)", fmt_int(st$n)), row("Pearson r", sprintf("%.3f (p %s)", st$r, fmt_p(st$r_p))),
        row("Spearman \u03C1", sprintf("%.3f (p %s)", st$rho, fmt_p(st$rho_p))),
        row("OLS slope", sprintf("%+.3f t per +1 pp", st$slope)),
        row("Countries: renewables up & CO\u2082 down", fmt_int(sum(cd$d$quad == "Renewables up, CO\u2082 down"))))) else
          div(class = "empty-state", paste0("Statistics not shown: ", st$reason, ".")),
      div(class = "note", "Why this chart: it compares each country with itself over time, removing stable between-country differences (income, climate, industry) that dominate the cross-sectional scatterplot. It still cannot rule out time-varying confounders, so it is not causal evidence. Only countries with both indicators in both years are included; the sidebar period does not restrict the chosen years."))
  })
  
  # ---- Methodology ------------------------------------------------------
  output$method_panel <- renderUI({
    tags$ul(
      tags$li(HTML("<b>Variables:</b> <code>renewables_energy_share_pct</code> = renewable energy share (%); <code>co2_per_capita</code> = CO\u2082 emissions per person (assumed tonnes per person, confirm with your data source). Total national emissions are never used.")),
      tags$li("Observation unit: one country-year, identified by ISO-3 code + year. Duplicate keys (if any) are averaged and reported on the Overview tab. Rows with invalid ISO codes (e.g. regional aggregates) are excluded from all country views."),
      tags$li("Missing values: rows are kept while at least one indicator is present. Every statistic uses only the observations that have the values it needs (paired for correlation)."),
      tags$li("Correlation: Pearson r (linear) and Spearman \u03C1 (rank) with two-sided tests from paired observations; OLS line with 95% confidence band for the mean fit. Nothing is shown when n < 3 or a variable is constant. Strength labels: <0.1 negligible, <0.3 weak, <0.5 moderate, <0.7 strong, otherwise very strong."),
      tags$li("Log scale: when enabled, CO\u2082 \u2264 0 values are omitted (and counted) and scatter statistics use log10(CO\u2082); Spearman is unaffected apart from the omitted rows."),
      tags$li("Maps and rankings show a single year (the single-year selection, or the chosen year inside the range). Years are never mixed within a ranking or map."),
      tags$li("Trend aggregates: mean renewable share and median CO\u2082 per capita across available countries in each year; the panel is unbalanced, so composition changes over time."),
      tags$li("Hexbin map: equal-area hexagons in a projected CRS; each cell holds the area-weighted mean of intersecting country values. Source data is country-level, so this is a spatial summary of countries, not sub-national measurements."),
      tags$li("Regions: continents from Natural Earth, joined by ISO-3 code; unmatched countries are labelled Unclassified."),
      tags$li("Correlation is not causation. Wealth, climate, resources, industrial structure and data quality can all influence both indicators."))
  })
}

## ---- 5. LAUNCH ---------------------------------------------------------
shinyApp(ui = ui, server = server)
