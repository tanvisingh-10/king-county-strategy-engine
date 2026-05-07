# ==============================================================================
# PROJECT: King County OZ 2.0 Strategy Engine
# VERSION: 28.0 (FINAL AUTO-FILL & GEOGRAPHY REPAIR)
# DEVELOPER: Tanvi Singh
# PORTFOLIO: https://github.com/tanvisingh-10/king-county-strategy-engine
#
# DESCRIPTION:
# GIS-based decision support tool for OZ 2.0 community need analysis and
# rural prioritization across King County, WA. Built for King County ADO
# in partnership with Inclusive Data Solutions.
#
# KEY CHANGES IN 28.0:
#   - GEOCODING MOVED SERVER-SIDE (fixes "Failed to fetch").
#     JavaScript fetch() was blocked by browser CORS / shinyapps.io sandbox.
#     Now uses httr::GET() inside observeEvent(input$verify_btn) on the R
#     server. Result is pushed to the UI via updateTextInput(session,
#     "census_id") and leafletProxy flyTo(). Fallback triggers
#     showNotification() if the Census API is unreachable.
#   - Geocode trigger: textInput "geo_address_input" + actionButton
#     "verify_btn". observeEvent fires on the button click only, preventing
#     excessive API calls while the user types.
#   - All other v26.0 logic retained: dual Google Sheets CSV load, inner_join
#     on 11-digit "Census Tract" GEOID, st_transform(4326) at every stage,
#     TOTAL_POSSIBLE_POINTS = 185, verbatim Angela/Shaun rural definition,
#     privacy opt-in checkbox, RUCA description sandwiched between label and
#     slider, "Rural Prioritization Top 25%" throughout.
#
# ONE-TIME SETUP:
#   install.packages(c("shiny","leaflet","dplyr","tigris","readr",
#                      "stringr","DT","sf","googlesheets4","tibble","httr"))
# ==============================================================================

library(shiny)
library(leaflet)
library(dplyr)
library(tigris)
library(readr)
library(stringr)
library(DT)
library(sf)
library(googlesheets4)
library(tibble)
library(httr)

options(tigris_use_cache = TRUE)
suppressMessages(gs4_deauth())

# ------------------------------------------------------------------------------
# GLOBAL CONSTANTS
# ------------------------------------------------------------------------------
TOTAL_POSSIBLE_POINTS <- 185L

KC_LNG  <- -121.8
KC_LAT  <-   47.5
KC_ZOOM <-    9

URL_COUNTY_WIDE <- paste0(
  "https://docs.google.com/spreadsheets/d/e/",
  "2PACX-1vRkOT81BcKxby6fKYVbCZa9cGnfJnD3_FsyITgN1wqrU_A7VvGt5ROUJp61EZUtSg",
  "/pub?gid=130667350&single=true&output=csv"
)

URL_URBAN_RURAL <- paste0(
  "https://docs.google.com/spreadsheets/d/e/",
  "2PACX-1vRkOT81BcKxby6fKYVbCZa9cGnfJnD3_FsyITgN1wqrU_A7VvGt5ROUJp61EZUtSg",
  "/pub?gid=195975242&single=true&output=csv"
)

SUBMISSIONS_SHEET_ID <- "YOUR_SUBMISSIONS_SHEET_ID_HERE"

CENSUS_GEOCODER_URL <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"

# ------------------------------------------------------------------------------
# RUCA CODE LABELS
# ------------------------------------------------------------------------------
RUCA_LABELS <- c(
  "1"  = "Urban Core",              "2"  = "Urban High Commute",
  "3"  = "Urban Low Commute",       "4"  = "Small City Core",
  "5"  = "Small City High Commute", "6"  = "Small City Low Commute",
  "7"  = "Small Town Core",         "8"  = "Small Town High Commute",
  "9"  = "Small Town Low Commute",  "10" = "Rural"
)

# ------------------------------------------------------------------------------
# COLOUR PALETTE
# ------------------------------------------------------------------------------
PAL <- list(
  high   = "#C0392B",
  medium = "#D35400",
  watch  = "#B7950B",
  rural  = "#1A7A4A",
  top25p = "#6C3483",
  both   = "#1A5276",
  grey   = "#95A5A6",
  nodata = "#D5D8DC",
  gold   = "#F39C12"
)

# ------------------------------------------------------------------------------
# SHAPEFILE CACHE
# ------------------------------------------------------------------------------
SHAPE_CACHE <- "kc_shapes_cache.rds"

kc_shapes <- tryCatch({
  if (file.exists(SHAPE_CACHE)) {
    shapes      <- readRDS(SHAPE_CACHE)
    cached_epsg <- tryCatch(st_crs(shapes)$epsg, error = function(e) NA_integer_)
    if (is.na(cached_epsg) || cached_epsg != 4326L) {
      message("[SHAPES] Cache CRS is not WGS84 - re-projecting.")
      shapes <- st_transform(shapes, crs = 4326)
      saveRDS(shapes, SHAPE_CACHE)
    }
    shapes
  } else {
    message("[SHAPES] Downloading King County census tracts (one-time, approx 30s).")
    raw    <- tracts(state = "WA", county = "King", cb = FALSE, progress_bar = FALSE)
    shapes <- st_transform(raw, crs = 4326)
    saveRDS(shapes, SHAPE_CACHE)
    message("[SHAPES] Saved to cache.")
    shapes
  }
}, error = function(e) {
  message("[SHAPES] Could not load shapefile: ", conditionMessage(e))
  NULL
})

if (!is.null(kc_shapes)) {
  kc_shapes$GEOID <- str_pad(trimws(as.character(kc_shapes$GEOID)), 11, pad = "0")
}

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; }
      html, body {
        font-family: 'DM Sans', sans-serif;
        background: #F2F4F7;
        color: #1C2B3A;
        margin: 0; padding: 0;
      }
      #oz-overlay {
        display: none; position: fixed; top: 0; left: 0;
        width: 100%; height: 100%;
        background: rgba(10,31,53,0.85); z-index: 9999;
        align-items: center; justify-content: center;
        flex-direction: column; text-align: center;
      }
      #oz-overlay.visible { display: flex; }
      .oz-box {
        background: #fff; border-radius: 14px; padding: 32px 40px;
        max-width: 340px; box-shadow: 0 8px 40px rgba(0,0,0,0.3);
      }
      .oz-box h3 { margin: 0 0 8px; color: #12355B; font-size: 17px; font-weight: 700; }
      .oz-box p  { margin: 0 0 18px; font-size: 13px; color: #5A6A7A; }
      .spinner {
        width: 36px; height: 36px; margin: 0 auto 16px;
        border: 4px solid #E2EAF4; border-top-color: #12355B;
        border-radius: 50%; animation: spin .8s linear infinite;
      }
      @keyframes spin { to { transform: rotate(360deg); } }
      #map-loading {
        display: none; position: absolute; top: 14px; right: 14px; z-index: 1000;
        background: rgba(255,255,255,0.92); border-radius: 8px; padding: 8px 14px;
        font-size: 12px; color: #12355B; font-weight: 600;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1); border-left: 3px solid #F39C12;
      }
      .map-wrap { position: relative; }
      .topbar {
        background: linear-gradient(110deg, #0A1F35 0%, #12355B 60%, #1A4A7A 100%);
        padding: 18px 32px 16px; margin: -15px -15px 22px;
        border-bottom: 3px solid #F39C12;
      }
      .tb-badge {
        background: #F39C12; color: #0A1F35; font-size: 10px; font-weight: 700;
        letter-spacing: 1.5px; text-transform: uppercase; padding: 4px 10px;
        border-radius: 3px; display: inline-block; margin-bottom: 6px;
      }
      .topbar h2 { margin: 0; font-size: 20px; font-weight: 700; color: #fff; }
      .topbar p  { margin: 5px 0 0; font-size: 12px; color: rgba(255,255,255,0.55); }
      .topbar a  { color: #F39C12; font-weight: 600; text-decoration: none; }
      .topbar a:hover { text-decoration: underline; }
      .slbl {
        font-size: 10px; font-weight: 700; letter-spacing: 1.4px;
        text-transform: uppercase; color: #8A9BAC;
        margin: 16px 0 8px; padding-bottom: 5px;
        border-bottom: 1px solid #EBF0F5;
      }
      .slbl:first-child { margin-top: 0; }
      .irs-bar, .irs-bar-edge { background: #12355B !important; border-color: #12355B !important; }
      .irs-single, .irs-from, .irs-to {
        background: #12355B !important; border-radius: 4px;
        font-family: 'DM Mono', monospace; font-size: 11px;
      }
      .irs-handle { border-color: #12355B !important; }
      .sr { display: flex; gap: 8px; margin: 6px 0; }
      .sc {
        flex: 1; background: #F8FAFC; border: 1px solid #E2EAF4;
        border-radius: 10px; padding: 10px 8px; text-align: center;
      }
      .sc.hl { border-color: #1A7A4A; background: #F0FBF4; }
      .sv { font-size: 18px; font-weight: 700; color: #12355B; font-family: 'DM Mono', monospace; line-height: 1; }
      .sc.hl .sv { color: #1A7A4A; }
      .sl { font-size: 9px; font-weight: 600; color: #8A9BAC; letter-spacing: .8px; text-transform: uppercase; margin-top: 3px; line-height: 1.3; }
      .ok   { background: #E8F8F0; border-left: 4px solid #1A7A4A; color: #145A32; padding: 10px 12px; border-radius: 0 8px 8px 0; font-size: 12px; font-weight: 600; }
      .er   { background: #FDEDEC; border-left: 4px solid #C0392B; color: #922B21; padding: 10px 12px; border-radius: 0 8px 8px 0; font-size: 12px; font-weight: 600; }
      .er pre { font-size: 10px; margin: 4px 0 0; white-space: pre-wrap; color: #922B21; font-family: 'DM Mono', monospace; }
      .warn { background: #FEF9E7; border-left: 4px solid #B7950B; color: #7D6608; padding: 10px 12px; border-radius: 0 8px 8px 0; font-size: 12px; font-weight: 600; }
      .info-note {
        background: #EBF5FB; border-left: 4px solid #12355B; color: #1A3A5C;
        padding: 10px 12px; border-radius: 0 8px 8px 0; font-size: 11.5px;
        font-weight: 500; margin-bottom: 8px; line-height: 1.55;
      }
      .rtip {
        background: #E8F8F0; border-left: 4px solid #1A7A4A; color: #145A32;
        padding: 10px 12px; border-radius: 0 8px 8px 0; font-size: 11.5px;
        font-weight: 500; margin-top: 6px; line-height: 1.55;
      }
      .rtip b { font-weight: 700; }
      .dbg {
        background: #F0F7FF; border-left: 4px solid #12355B; color: #1A3A5C;
        padding: 8px 12px; border-radius: 0 8px 8px 0; font-size: 10.5px;
        font-family: 'DM Mono', monospace; margin-top: 6px; line-height: 1.7;
      }
      .optin-box {
        background: #F0F7FF; border: 1.5px solid #12355B; border-radius: 8px;
        padding: 10px 12px; margin: 10px 0; font-size: 12px; color: #1A3A5C;
      }
      .optin-box .shiny-input-container label {
        font-size: 12px !important; font-weight: 600 !important;
        color: #12355B !important; text-transform: none !important;
        letter-spacing: 0 !important;
      }
      .tli { display: flex; align-items: flex-start; gap: 10px; padding: 7px 10px; border-radius: 7px; background: #F4F8FF; margin: 4px 0; font-size: 11.5px; font-weight: 500; }
      .dot   { width: 9px; height: 9px; border-radius: 50%; background: #12355B; flex-shrink: 0; margin-top: 3px; }
      .dot.u { background: #C0392B; }
      .dot.g { background: #1A7A4A; }
      .tdate { color: #8A9BAC; font-size: 10px; font-weight: 600; font-family: 'DM Mono', monospace; }
      .nav-tabs { border-bottom: 2px solid #E2EAF4; margin-bottom: 0; }
      .nav-tabs > li > a { font-size: 13px; font-weight: 600; color: #8A9BAC; border: none !important; padding: 12px 14px; background: transparent !important; }
      .nav-tabs > li > a:hover { color: #12355B; }
      .nav-tabs > li.active > a { color: #12355B !important; border-bottom: 3px solid #F39C12 !important; background: transparent !important; }
      .tab-content { background: #fff; border: 2px solid #E2EAF4; border-top: none; border-radius: 0 0 14px 14px; box-shadow: 0 4px 16px rgba(0,0,0,0.06); padding: 18px; }
      .mn { font-size: 11px; color: #5A6A7A; margin-top: 10px; padding: 8px 14px; background: #F8FAFC; border-radius: 7px; border-left: 3px solid #CBD5E0; line-height: 1.8; }
      .mn b { color: #1C2B3A; }
      .ld { display: inline-block; width: 12px; height: 12px; border-radius: 3px; margin-right: 3px; vertical-align: middle; }
      .mcard { background: #F8FAFC; border-radius: 12px; border: 1px solid #E2EAF4; padding: 16px 20px; margin: 10px 0; }
      .mcard-head { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }
      .mcard-icon { width: 36px; height: 36px; border-radius: 8px; display: flex; align-items: center; justify-content: center; font-size: 13px; font-weight: 700; flex-shrink: 0; }
      .mcard-icon.navy  { background: #12355B; color: #fff; }
      .mcard-icon.gold  { background: #F39C12; color: #fff; }
      .mcard-icon.green { background: #1A7A4A; color: #fff; }
      .mcard h5 { margin: 0; font-size: 14px; font-weight: 700; color: #12355B; }
      .mcard p  { margin: 0; font-size: 13px; color: #4A5568; line-height: 1.65; }
      .mcard ul { margin: 6px 0 0; padding-left: 18px; font-size: 13px; color: #4A5568; line-height: 1.8; }
      .no-data-badge { display: inline-block; background: #EBF0F5; color: #5A6A7A; font-size: 10px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; padding: 2px 8px; border-radius: 4px; margin-left: 8px; vertical-align: middle; }
      .data-badge    { display: inline-block; background: #E8F8F0; color: #1A7A4A; font-size: 10px; font-weight: 700; letter-spacing: 1px; text-transform: uppercase; padding: 2px 8px; border-radius: 4px; margin-left: 8px; vertical-align: middle; }
      .step-row { display: grid; grid-template-columns: repeat(3,1fr); gap: 10px; margin: 12px 0; }
      .step { background: #fff; border: 1px solid #E2EAF4; border-radius: 10px; padding: 12px 14px; text-align: center; }
      .step-num { font-size: 22px; font-weight: 700; color: #F39C12; font-family: 'DM Mono', monospace; line-height: 1; }
      .step-lbl { font-size: 11.5px; color: #4A5568; margin-top: 4px; line-height: 1.4; }
      .ref-link { display: flex; align-items: flex-start; gap: 10px; background: #fff; border: 1px solid #E2EAF4; border-radius: 10px; padding: 12px 16px; margin: 6px 0; }
      .ref-dot  { width: 8px; height: 8px; border-radius: 50%; background: #F39C12; flex-shrink: 0; margin-top: 4px; }
      .ref-title { font-size: 13px; font-weight: 600; color: #12355B; margin-bottom: 2px; }
      .ref-desc  { font-size: 12px; color: #5A6A7A; line-height: 1.5; }
      .ic { background: #F8FAFC; border-radius: 10px; border-left: 4px solid #F39C12; padding: 14px 18px; margin: 10px 0; }
      .ic h5 { margin: 0 0 6px; color: #12355B; font-weight: 700; font-size: 14px; }
      .ic p, .ic ul { margin: 0; font-size: 13px; color: #4A5568; line-height: 1.7; }
      .ic.blue  { border-left-color: #12355B; }
      .ic.green { border-left-color: #1A7A4A; }
      .ic.gold  { border-left-color: #F39C12; }
      .blockquote-stat {
        background: #F8FAFC; border-left: 4px solid #12355B; border-radius: 0 8px 8px 0;
        padding: 14px 18px; margin: 12px 0; font-size: 13px; color: #2C3E50;
        font-style: italic; line-height: 1.7;
      }
      .blockquote-stat cite {
        display: block; margin-top: 8px; font-size: 11px; font-style: normal;
        font-weight: 700; color: #8A9BAC; text-transform: uppercase; letter-spacing: .8px;
      }
      .fact-row { display: grid; grid-template-columns: repeat(3,1fr); gap: 10px; margin: 12px 0; }
      .fact { background: #fff; border: 1px solid #E2EAF4; border-radius: 10px; padding: 12px; text-align: center; }
      .fact-num { font-size: 24px; font-weight: 700; color: #12355B; font-family: 'DM Mono', monospace; }
      .fact-lbl { font-size: 11px; color: #5A6A7A; margin-top: 4px; line-height: 1.4; }
      .fs { background: #F8FAFC; border-radius: 10px; border-left: 4px solid #12355B; padding: 14px 18px; margin: 12px 0; }
      .fs h5 { margin: 0 0 10px; color: #12355B; font-weight: 700; font-size: 12px; text-transform: uppercase; letter-spacing: .8px; }
      .fr     { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin: 8px 0; }
      .fr.one { grid-template-columns: 1fr; }
      .fg     { display: flex; flex-direction: column; gap: 4px; }
      .flbl   { font-size: 11px; font-weight: 600; color: #5A6A7A; text-transform: uppercase; letter-spacing: .8px; }
      .fctl   { background: #fff; border: 1.5px solid #D6E4F0; border-radius: 8px; padding: 9px 12px; font-size: 13px; color: #1C2B3A; width: 100%; }
      .fctl:focus { outline: none; border-color: #12355B; }
      .geo-row { display: flex; gap: 8px; margin-bottom: 6px; }
      .geo-inp { flex: 1; background: #fff; border: 1.5px solid #D6E4F0; border-radius: 8px; padding: 9px 12px; font-size: 13px; color: #1C2B3A; outline: none; }
      .geo-inp:focus { border-color: #12355B; }
      .collab-banner { background: #FFF3CD; border: 2px solid #F39C12; border-radius: 8px; padding: 12px 18px; margin-bottom: 16px; font-size: 13px; color: #5D4037; font-weight: 600; }
      .collab-banner b { color: #0A1F35; }
      .mbanner { background: #FEF3DC; border: 1.5px solid #F39C12; border-radius: 8px; padding: 10px 16px; margin-bottom: 16px; font-size: 12.5px; color: #7D5A10; }
      .sbtn { background: #12355B; color: #fff; border: none; border-radius: 8px; padding: 12px 28px; font-size: 14px; font-weight: 600; cursor: pointer; display: inline-block; margin-top: 8px; text-decoration: none; }
      .sbtn:hover { background: #1A4A7A; color: #fff; }
      .shiny-input-container { margin-bottom: 0 !important; }
      .shiny-input-container label { font-size: 11px; font-weight: 600; color: #5A6A7A; text-transform: uppercase; letter-spacing: .8px; margin-bottom: 4px; }
      .shiny-input-container input[type='text'],
      .shiny-input-container input[type='email'],
      .shiny-input-container input[type='tel'] {
        background: #fff; border: 1.5px solid #D6E4F0; border-radius: 8px;
        padding: 9px 12px; font-size: 13px; color: #1C2B3A; width: 100%;
        font-family: 'DM Sans', sans-serif;
      }
      .shiny-input-container input:focus { outline: none; border-color: #12355B; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select { border: 1px solid #CBD5E0; border-radius: 6px; padding: 4px 8px; font-family: 'DM Sans', sans-serif; }
      table.dataTable thead th { background: #F4F8FF; color: #12355B; font-size: 11px; font-weight: 700; letter-spacing: .5px; text-transform: uppercase; border-bottom: 2px solid #D6E4F0 !important; }
      table.dataTable tbody tr:hover td { background: #F0F7FF !important; }
      table.dataTable { font-size: 13px; }
      a { color: #12355B; } a:hover { color: #F39C12; }
      /* Verify button styled to match the design */
      #verify_btn {
        background: #12355B; color: #fff; border: none; border-radius: 8px;
        padding: 9px 14px; font-size: 13px; font-weight: 600; cursor: pointer;
        white-space: nowrap; height: 38px; line-height: 1;
      }
      #verify_btn:hover { background: #1A4A7A; }
      @media(max-width: 768px) {
        .topbar { padding: 14px 16px 12px; margin: -15px -15px 14px; }
        .topbar h2 { font-size: 15px; }
        .col-sm-3, .col-sm-9 { width: 100% !important; float: none !important; }
        .sv { font-size: 16px; }
        .fr { grid-template-columns: 1fr; }
        .fact-row, .step-row { grid-template-columns: 1fr; }
      }
    ")),
    
    tags$script(HTML("
      $(document).on('shiny:disconnected', function(){
        $('#oz-overlay').addClass('visible');
      });
      $(document).on('shiny:connected', function(){
        $('#oz-overlay').removeClass('visible');
      });
      setInterval(function(){
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue)
          Shiny.setInputValue('keepalive_ping', new Date().getTime(), {priority:'event'});
      }, 25000);
      $(document).on('shiny:busy', function(){ $('#map-loading').fadeIn(150); });
      $(document).on('shiny:idle', function(){ $('#map-loading').fadeOut(200); });
      // Allow Enter key on address input to trigger the Verify button
      $(document).on('keydown', '#geo_address_input', function(e){
        if (e.key === 'Enter'){ e.preventDefault(); $('#verify_btn').click(); }
      });
    "))
  ),
  
  div(id = "oz-overlay",
      div(class = "oz-box",
          div(class = "spinner"),
          h3("Reconnecting..."),
          p("Session timed out. Usually back within 5 seconds."),
          tags$small(style = "color:#B0BEC5;font-size:11px;",
                     "Your filters and settings will restore on reconnect.")
      )
  ),
  
  div(class = "topbar",
      div(class = "tb-badge", "King County ADO"),
      h2("Opportunity Zones 2.0 - Community Need Strategy Engine"),
      p("Decision Support Tool  |  Application Window: Apr 28 - May 27, 2026  |  Live Data  |  ",
        tags$a(href = "https://www.commerce.wa.gov/opportunity-zones/",
               target = "_blank", "Official WA Commerce OZ Page"))
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # ------------------------------------------------------------------
                 # ADDRESS LOOKUP - now a plain textInput + actionButton.
                 # Geocoding happens server-side in observeEvent(input$verify_btn).
                 # ------------------------------------------------------------------
                 div(class = "slbl", "Address Lookup"),
                 tags$small(
                   style = "color:#8A9BAC;font-size:10.5px;line-height:1.5;display:block;margin-bottom:8px;",
                   "Type any King County address and press Enter or click Verify. Auto-fills the Census Tract ID in the Submit Project form."
                 ),
                 div(class = "geo-row",
                     tags$input(id = "geo_address_input", type = "text", class = "geo-inp",
                                placeholder = "e.g. 400 Yesler Way, Seattle WA"),
                     actionButton("verify_btn", "Verify", class = "")
                 ),
                 uiOutput("geo_result_ui"),
                 br(),
                 
                 # Community Need Weights
                 div(class = "slbl", "Community Need Weights"),
                 tags$small(
                   style = "color:#8A9BAC;font-size:10.5px;line-height:1.5;display:block;margin-bottom:10px;",
                   paste0("Community need is 21 points out of ", TOTAL_POSSIBLE_POINTS,
                          " in the state scoring formula. Adjust weights to reflect your priorities.")
                 ),
                 sliderInput("w_poverty", "Poverty Rate",     0, 100, 50, post = "%", ticks = FALSE),
                 sliderInput("w_mfi",     "Income Gap (MFI)", 0, 100, 30, post = "%", ticks = FALSE),
                 sliderInput("w_rural",   "Rural Priority",   0, 100, 20, post = "%", ticks = FALSE),
                 uiOutput("weight_bar"),
                 
                 # RUCA threshold: label -> description block -> slider (sandwiched per spec)
                 div(class = "slbl", "Minimum RUCA Score for Rural Prioritization"),
                 div(class = "info-note",
                     "RUCA codes classify census tracts on a 1-10 scale using population density and commuting patterns:",
                     tags$br(),
                     tags$b("1-3: Urban"), "  |  ",
                     tags$b("4-6: Small City"), "  |  ",
                     tags$b("7-9: Small Town"), "  |  ",
                     tags$b("10: Rural"),
                     tags$br(),
                     "All 80 OZ 2.0 eligible King County tracts have RUCA = 1. Keep the slider at 1 to include them, or uncheck 'OZ 2.0 Eligible Only' to explore higher RUCA tracts."
                 ),
                 sliderInput("ruca_slider",
                             label = "Minimum RUCA for Rural Prioritization",
                             min = 1, max = 10, value = 1, step = 1, ticks = TRUE),
                 uiOutput("ruca_label"),
                 
                 # Score View Mode
                 div(class = "slbl", "Score View Mode"),
                 radioButtons("mode", label = NULL,
                              choices = c(
                                "Top 25% - High Community Need" = "top25",
                                "Top 25% - Custom Weights"      = "top25pct",
                                "Compare Both"                  = "both"
                              ),
                              selected = "top25"),
                 
                 # Filters
                 div(class = "slbl", "Filters (AND logic - each narrows results)"),
                 checkboxInput("elig_only",  "OZ 2.0 Eligible Tracts Only",          TRUE),
                 checkboxInput("hide_oz1",   "Exclude Current OZ 1.0 Tracts",        FALSE),
                 checkboxInput("rural_only", "Rural Tracts Only (RUCA >= threshold)", FALSE),
                 uiOutput("filter_note"),
                 
                 # Privacy Opt-In
                 hr(style = "border-color:#EBF0F5;margin:14px 0"),
                 div(class = "slbl", "Data Sharing"),
                 div(class = "optin-box",
                     checkboxInput(
                       "data_opt_in",
                       "I opt-in to share project-related data for collection and analysis.",
                       value = FALSE
                     ),
                     tags$small(
                       style = "color:#5A6A7A;font-size:10.5px;line-height:1.5;display:block;margin-top:4px;",
                       "When checked, submitted project data will be logged to the King County ADO tracking sheet. This does not affect map or ranking functionality."
                     )
                 ),
                 
                 hr(style = "border-color:#EBF0F5;margin:14px 0"),
                 uiOutput("status_box"),
                 br(),
                 uiOutput("stats"),
                 uiOutput("rural_tip"),
                 uiOutput("debug_panel"),
                 
                 div(class = "slbl", "Key Dates"),
                 div(class = "tli", div(class = "dot"),
                     div(div(class = "tdate", "Apr 6, 2026"),  "Treasury releases eligible tracts")),
                 div(class = "tli", div(class = "dot u"),
                     div(div(class = "tdate", "Apr 28, 2026"), "Applications officially OPEN")),
                 div(class = "tli", div(class = "dot"),
                     div(div(class = "tdate", "May 7, 2026"),  "Survey123 technical webinar")),
                 div(class = "tli", div(class = "dot u"),
                     div(div(class = "tdate", "May 27, 2026"), "Application DEADLINE")),
                 div(class = "tli", div(class = "dot g"),
                     div(div(class = "tdate", "Jan 1, 2027"),  "New OZ designations take effect")),
                 
                 hr(style = "border-color:#EBF0F5;margin:14px 0"),
                 div(style = "text-align:center;",
                     tags$a(href = "https://www.commerce.wa.gov/opportunity-zones/",
                            target = "_blank", class = "sbtn",
                            style = "font-size:12px;padding:9px 18px;",
                            "WA Commerce OZ 2.0 Official Site"))
    ),
    
    mainPanel(width = 9,
              tabsetPanel(id = "tabs",
                          
                          # ====================================================================
                          # Tab 1: Priority Map
                          # ====================================================================
                          tabPanel("Priority Map",
                                   br(),
                                   div(class = "collab-banner",
                                       tags$b("Collaboration Tool Only: "),
                                       "This dashboard is NOT an official state application. It is designed to identify community need and foster partnership conversations among cities, developers, and economic development partners. ",
                                       tags$a(href = "https://www.commerce.wa.gov/opportunity-zones/",
                                              target = "_blank", "Submit formal nominations via WA Commerce.")
                                   ),
                                   div(class = "map-wrap",
                                       leafletOutput("map", height = "510px"),
                                       div(id = "map-loading", "Updating map...")
                                   ),
                                   div(class = "mn",
                                       tags$b("Legend:  "),
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$high,";")),"High Need"),              "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$medium,";")),"Medium Need"),          "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$watch,";")),"Lower Need"),            "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$rural,";")),"Rural Prioritization"),  "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$top25p,";")),"Top 25% (Custom)"),     "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$both,";")),"In Both Views"),          "  |  ",
                                       tags$span(tags$span(class="ld",style=paste0("background:",PAL$grey,";")),"Not Selected"),
                                       tags$br(), tags$br(),
                                       tags$b("Important: "),
                                       paste0("This map scores Community Need only (21 of ", TOTAL_POSSIBLE_POINTS, " pts). "),
                                       tags$b("Market Readiness (104 pts)"), " and ", tags$b("Policy Alignment (42 pts)"),
                                       " require project-level data. Use the Submit Project tab to register a project.",
                                       tags$br(), tags$br(),
                                       tags$i(style = "color:#8A9BAC;font-size:10.5px;",
                                              "Use Address Lookup in the sidebar to find any address and fly the map to its census tract.")
                                   )
                          ),
                          
                          # ====================================================================
                          # Tab 2: Investment Rankings
                          # ====================================================================
                          tabPanel("Investment Rankings",
                                   br(),
                                   uiOutput("rankings_header"),
                                   DTOutput("tbl")
                          ),
                          
                          # ====================================================================
                          # Tab 3: Methodology
                          # ====================================================================
                          tabPanel("Methodology",
                                   br(),
                                   h4(style = "color:#12355B;font-weight:700;margin-bottom:4px;",
                                      "How OZ 2.0 Applications Are Scored by the State"),
                                   p(style = "font-size:13px;color:#5A6A7A;margin-bottom:18px;line-height:1.6;",
                                     paste0("WA Commerce evaluates OZ 2.0 applications on a ", TOTAL_POSSIBLE_POINTS,
                                            "-point scale across three categories. This dashboard calculates one from public data. The other two require project-level input.")),
                                   
                                   div(class = "mcard",
                                       div(class = "mcard-head",
                                           div(class = "mcard-icon navy", "104"),
                                           h5("Market and Investment Readiness"),
                                           tags$span(class = "no-data-badge", "No public dataset")),
                                       p("The heaviest category (104 of 185 pts). Evaluates whether specific development projects are ready to proceed in a tract. This information lives in developer conversations, city emails, and permit records."),
                                       tags$br(),
                                       p(tags$b("How to contribute: "), "Use the Submit Project tab to register a project in a specific census tract.")
                                   ),
                                   
                                   div(class = "mcard",
                                       div(class = "mcard-head",
                                           div(class = "mcard-icon gold", "42"),
                                           h5("Policy Alignment"),
                                           tags$span(class = "no-data-badge", "No public dataset")),
                                       p("42 of 185 pts. Evaluates whether a census tract is specifically referenced in formal planning documents such as a city comprehensive plan or economic development strategy.")
                                   ),
                                   
                                   div(class = "mcard",
                                       div(class = "mcard-head",
                                           div(class = "mcard-icon green", "21"),
                                           h5("Community Need and Diversity"),
                                           tags$span(class = "data-badge", "This dashboard")),
                                       p("21 of 185 pts. Calculated from public datasets:"),
                                       tags$ul(
                                         tags$li(tags$b("Poverty Rate"), " from the American Community Survey."),
                                         tags$li(tags$b("MFI Ratio"), " - Median Family Income as a percentage of AMI. Lower ratio = higher income distress."),
                                         tags$li(tags$b("Rural and Non-Urban tracts"), " receive a geographic diversity boost via RUCA Codes (USDA).")
                                       )
                                   ),
                                   
                                   # Verbatim statutory rural definition (Angela/Shaun)
                                   div(class = "blockquote-stat",
                                       "\"The rural definition under OZ 2.0 - which determines eligibility for the enhanced Qualified Rural Opportunity Fund (QROF) benefits - is set by federal statute and Treasury guidance, not by Commerce. To meet this definition, a tract must be located entirely within a city or town with a population under 50,000, or in an area contiguous and adjacent to such a city or town, to qualify as rural.\"",
                                       tags$cite("OZ 2.0 Statutory Rural Definition - Angela/Shaun, King County ADO Working Group")
                                   ),
                                   
                                   div(class = "ic gold",
                                       h5("RUCA vs. Statutory Rural: Important Distinction"),
                                       p("This dashboard uses RUCA codes (a USDA commuting-pattern metric) to approximate rural prioritization in the scoring model. The statutory OZ 2.0 rural definition above governs eligibility for QROF benefits and must be verified through the official WA Commerce tract eligibility list.")
                                   ),
                                   
                                   div(class = "mcard",
                                       div(class = "mcard-head",
                                           div(class = "mcard-icon navy", "!"),
                                           h5("Why Community Need Is Only Part of the Picture")),
                                       p("Market Readiness carries 104 of the 185 total points. A tract with 40% poverty and no identified project will score lower than a tract with 20% poverty and a permitted development ready to start."),
                                       tags$br(),
                                       p(tags$b("One Tract, One Winner: "),
                                         "If two cities apply for the same tract, only the highest-scoring application advances.")
                                   ),
                                   
                                   div(class = "mcard",
                                       div(class = "mcard-head",
                                           div(class = "mcard-icon navy", "?"),
                                           h5("How to Use This Tool")),
                                       div(class = "step-row",
                                           div(class = "step", div(class = "step-num", "1"),
                                               div(class = "step-lbl", "Adjust weight sliders and RUCA threshold for your context")),
                                           div(class = "step", div(class = "step-num", "2"),
                                               div(class = "step-lbl", "Identify high-need tracts on the map and check Rankings for the full sorted list")),
                                           div(class = "step", div(class = "step-num", "3"),
                                               div(class = "step-lbl", "Submit project details so partners see market readiness signals for specific tracts"))
                                       )
                                   ),
                                   
                                   br(),
                                   h4(style = "color:#12355B;font-weight:700;margin-bottom:12px;", "Official Sources"),
                                   div(class = "ref-link", div(class = "ref-dot"),
                                       div(div(class = "ref-title", tags$a(href="https://www.commerce.wa.gov/opportunity-zones/",target="_blank","WA Commerce OZ 2.0 Program Overview")),
                                           div(class = "ref-desc", "Official program page with scoring criteria, application guidance, and deadline information."))),
                                   div(class = "ref-link", div(class = "ref-dot"),
                                       div(div(class = "ref-title", tags$a(href="https://experience.arcgis.com/experience/035ad15299ae4a25ac7585d4d23ec309/page/All-Census-Tracts",target="_blank","WA Commerce OZ 2.0, All Census Tracts (ArcGIS)")),
                                           div(class = "ref-desc", "Official interactive map. Verify tract eligibility, poverty rate, and MFI ratio before any formal submission."))),
                                   div(class = "ref-link", div(class = "ref-dot"),
                                       div(div(class = "ref-title", tags$a(href="https://deptofcommerce.app.box.com/s/cvqzbn10ivvlmbe0z7zpe5h6y5nqcxw2",target="_blank","WA Commerce OZ 2.0 Scoring Criteria (PDF)")),
                                           div(class = "ref-desc", paste0("The official rubric with the full ", TOTAL_POSSIBLE_POINTS, "-point breakdown. Read before finalizing any application."))))
                          ),
                          
                          # ====================================================================
                          # Tab 4: Submit Project
                          # ====================================================================
                          tabPanel("Submit Project",
                                   br(),
                                   div(class = "collab-banner",
                                       tags$b("This is NOT an official state submission. "),
                                       "This form documents project ideas and shares them with economic development partners. A summary goes to your own email only. ",
                                       tags$a(href = "https://www.commerce.wa.gov/opportunity-zones/",
                                              target = "_blank", "For official OZ 2.0 nominations, visit WA Commerce.")
                                   ),
                                   div(class = "mbanner",
                                       tags$b("Why fill this out? "),
                                       "Market Readiness (104 pts) and Policy Alignment (42 pts) are not in any public dataset. This form documents those pieces and signals project activity to economic development partners."),
                                   h4(style = "color:#12355B;font-weight:700;margin-bottom:4px;",
                                      "Document an Investment Project"),
                                   p(style = "font-size:13px;color:#5A6A7A;margin-bottom:18px;line-height:1.6;",
                                     "Use the Address Lookup in the sidebar to auto-fill the Census Tract ID below. Check the opt-in box in the sidebar to enable data logging."),
                                   
                                   uiOutput("optin_warning"),
                                   
                                   div(class = "fs", h5("Project Basics"),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Project Name"),
                                               tags$input(id="sub_name",type="text",class="fctl",placeholder="e.g. Skyway Mixed-Use Development")),
                                           div(class = "fg",
                                               div(class = "flbl", "Project Type"),
                                               tags$select(id="sub_type",class="fctl",
                                                           tags$option("Housing"), tags$option("Commercial"),
                                                           tags$option("Industrial"), tags$option("Mixed-Use"), tags$option("Other")))
                                       ),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Estimated Investment ($)"),
                                               tags$input(id="sub_invest",type="text",class="fctl",placeholder="e.g. 2500000")),
                                           div(class = "fg",
                                               div(class = "flbl", "Expected Jobs Created"),
                                               tags$input(id="sub_jobs",type="number",class="fctl",placeholder="e.g. 45"))
                                       )
                                   ),
                                   
                                   div(class = "fs", h5("Location"),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Street Address"),
                                               tags$input(id="sub_addr",type="text",class="fctl",placeholder="e.g. 123 Main St, Kent WA 98032")),
                                           div(class = "fg",
                                               # Shiny textInput so updateTextInput(session, "census_id") can
                                               # write the geocoded 11-digit tract ID here when observer fires.
                                               textInput("census_id",
                                                         "Census Tract ID (auto-filled from Address Lookup)",
                                                         value = "", placeholder = "e.g. 53033027701"))
                                       ),
                                       div(class = "fr one",
                                           div(class = "fg",
                                               div(class = "flbl", "Project Description"),
                                               tags$textarea(id="sub_desc",class="fctl",
                                                             placeholder="Describe the project, community impact, development stage, and any planning documents referencing this tract.",
                                                             style="min-height:80px;")))
                                   ),
                                   
                                   div(class = "fs", h5("Market Readiness"),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Development Stage"),
                                               tags$select(id="sub_stage",class="fctl",
                                                           tags$option("Concept"), tags$option("Pre-Development"),
                                                           tags$option("Permitted"), tags$option("Under Construction"))),
                                           div(class = "fg",
                                               div(class = "flbl", "Target Start Date"),
                                               tags$input(id="sub_date",type="text",class="fctl",placeholder="e.g. Q3 2026"))
                                       ),
                                       div(class = "fr one",
                                           div(class = "fg",
                                               div(class = "flbl", "Policy Documents (optional)"),
                                               tags$input(id="sub_policy",type="text",class="fctl",
                                                          placeholder="Name any city comprehensive plan or economic strategy referencing this tract")))
                                   ),
                                   
                                   div(class = "fs", h5("Your Contact Information"),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Your Name"),
                                               tags$input(id="sub_contact",type="text",class="fctl",placeholder="e.g. Jane Smith")),
                                           div(class = "fg",
                                               div(class = "flbl", "Organization"),
                                               tags$input(id="sub_org",type="text",class="fctl",placeholder="e.g. City of Kent Economic Development"))
                                       ),
                                       div(class = "fr",
                                           div(class = "fg",
                                               div(class = "flbl", "Your Email (summary sent to you)"),
                                               tags$input(id="sub_email",type="email",class="fctl",placeholder="e.g. jane@cityofkent.gov")),
                                           div(class = "fg",
                                               div(class = "flbl", "Phone (optional)"),
                                               tags$input(id="sub_phone",type="tel",class="fctl",placeholder="e.g. (206) 555-0100"))
                                       )
                                   ),
                                   
                                   div(style = "margin-top:20px;",
                                       tags$button(
                                         id = "submit_btn", class = "sbtn",
                                         onclick = "
  var n    = document.getElementById('sub_name').value    || '(not provided)';
  var ty   = document.getElementById('sub_type').value;
  var inv  = document.getElementById('sub_invest').value  || '(not provided)';
  var jobs = document.getElementById('sub_jobs').value    || '(not provided)';
  var addr = document.getElementById('sub_addr').value    || '(not provided)';
  var tr   = document.getElementById('census_id').value   || '(not provided)';
  var desc = document.getElementById('sub_desc').value    || '(not provided)';
  var stg  = document.getElementById('sub_stage').value;
  var dt   = document.getElementById('sub_date').value    || '(not provided)';
  var pol  = document.getElementById('sub_policy').value  || '(not provided)';
  var con  = document.getElementById('sub_contact').value || '(not provided)';
  var org  = document.getElementById('sub_org').value     || '(not provided)';
  var em   = document.getElementById('sub_email').value;
  var ph   = document.getElementById('sub_phone').value   || '(not provided)';
  if (!em || em.indexOf('@') < 0) {
    alert('Please enter a valid email address. Your project summary will be sent there.');
    return;
  }
  var body = 'OZ 2.0 Project Documentation%0A'
    + 'King County ADO Collaboration Tool%0A'
    + 'NOT an official state application.%0A'
    + 'Official nominations: https://www.commerce.wa.gov/opportunity-zones/%0A%0A'
    + 'PROJECT%0A'
    + 'Name: '         + encodeURIComponent(n)    + '%0A'
    + 'Type: '         + encodeURIComponent(ty)   + '%0A'
    + 'Investment: $'  + encodeURIComponent(inv)  + '%0A'
    + 'Jobs: '         + encodeURIComponent(jobs) + '%0A%0A'
    + 'LOCATION%0A'
    + 'Address: '      + encodeURIComponent(addr) + '%0A'
    + 'Census Tract: ' + encodeURIComponent(tr)   + '%0A'
    + 'Description: '  + encodeURIComponent(desc) + '%0A%0A'
    + 'MARKET READINESS%0A'
    + 'Stage: '        + encodeURIComponent(stg)  + '%0A'
    + 'Start: '        + encodeURIComponent(dt)   + '%0A'
    + 'Policy Docs: '  + encodeURIComponent(pol)  + '%0A%0A'
    + 'CONTACT%0A'
    + 'Name: '         + encodeURIComponent(con)  + '%0A'
    + 'Org: '          + encodeURIComponent(org)  + '%0A'
    + 'Email: '        + encodeURIComponent(em)   + '%0A'
    + 'Phone: '        + encodeURIComponent(ph);
  window.location.href = 'mailto:' + encodeURIComponent(em)
    + '?subject=OZ+2.0+Project+-+Tract+' + encodeURIComponent(tr)
    + '+-+' + encodeURIComponent(n)
    + '&body=' + body;
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('log_submission_trigger', new Date().getTime(), {priority:'event'});
  }
",
                                         "Send Project Summary to My Email"
                                       ),
                                       p(style = "font-size:11px;color:#8A9BAC;margin-top:8px;",
                                         "Opens your email app with a pre-filled project summary addressed to you. If data sharing is opted-in, this also logs the submission to the King County ADO tracking sheet.")
                                   )
                          ),
                          
                          # ====================================================================
                          # Tab 5: About OZ 2.0
                          # ====================================================================
                          tabPanel("About OZ 2.0",
                                   br(),
                                   div(class = "collab-banner",
                                       tags$b("Collaboration Tool Only: "),
                                       "This dashboard is designed to signal project activity and foster partnerships. ",
                                       tags$a(href = "https://www.commerce.wa.gov/opportunity-zones/",
                                              target = "_blank", "Official nominations via WA Commerce.")
                                   ),
                                   h4(style = "color:#12355B;font-weight:700;", "What is Opportunity Zones 2.0?"),
                                   p(style = "font-size:13px;color:#4A5568;line-height:1.7;margin-bottom:14px;",
                                     "OZ 2.0 is a federal program designating specific low-income census tracts where private investors receive significant tax benefits for long-term capital investment. Washington State Department of Commerce is supporting Governor Ferguson in nominating up to 25% of eligible census tracts. King County ADO is coordinating partner submissions."),
                                   div(class = "fact-row",
                                       div(class = "fact", div(class = "fact-num", as.character(TOTAL_POSSIBLE_POINTS)),
                                           div(class = "fact-lbl", "Total points in the WA Commerce OZ 2.0 scoring formula")),
                                       div(class = "fact", div(class = "fact-num", "21"),
                                           div(class = "fact-lbl", "Points available for Community Need (this dashboard)")),
                                       div(class = "fact", div(class = "fact-num", "25%"),
                                           div(class = "fact-lbl", "Minimum rural tracts required in state nominations"))
                                   ),
                                   div(class = "ic",
                                       h5("Key Program Changes vs OZ 1.0"),
                                       tags$ul(
                                         tags$li("Program becomes ", tags$b("permanent"), " with 10-year designation cycles"),
                                         tags$li("Roughly ", tags$b("20% fewer eligible tracts"), " statewide vs OZ 1.0"),
                                         tags$li(tags$b("Geographic diversity required"), " - state scoring penalizes Seattle-only nominations"),
                                         tags$li("Rural areas: ", tags$b("30% basis reduction"), " plus ", tags$b("50% improvement threshold"), " vs 100% for urban"),
                                         tags$li("Eliminates the ", tags$b("5% contiguous tract exception"))
                                       )
                                   ),
                                   div(class = "ic blue",
                                       h5("King County ADO Strategy"),
                                       tags$ul(
                                         tags$li("Weekly working group meetings through May 27, 2026"),
                                         tags$li("Priority tract list built from Commerce distress criteria"),
                                         tags$li("Non-Urban tracts elevated to meet the geographic diversity requirement"),
                                         tags$li("Coordination with PSRC, Port of Seattle, Greater Seattle Partners"),
                                         tags$li("Tool built with Inclusive Data Solutions for city-level application support")
                                       )
                                   ),
                                   div(class = "ic green",
                                       h5("What This Dashboard Does and Does Not Do"),
                                       tags$ul(
                                         tags$li(tags$b("Does: "),
                                                 paste0("Score census tracts on Community Need (21 of ", TOTAL_POSSIBLE_POINTS,
                                                        " pts) using poverty rate, MFI ratio, and RUCA-based rurality.")),
                                         tags$li(tags$b("Does not: "), "Score Market Readiness (104 pts) or Policy Alignment (42 pts). Use the Submit Project tab for those."),
                                         tags$li(tags$b("Does not: "), "Replace the official WA Commerce nomination process.")
                                       )
                                   )
                          )
              )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  observeEvent(input$keepalive_ping, {}, ignoreNULL = TRUE)
  
  # Reactive value that holds the geocoding result for sidebar display
  geo_result_rv <- reactiveVal(list(status = "idle", message = "", tract = "", lat = NA, lng = NA))
  
  # --------------------------------------------------------------------------
  # SERVER-SIDE GEOCODING
  # Triggered by the Verify button (or Enter key via JS).
  # Uses httr::GET() to call the US Census Bureau Geocoding API directly
  # from the R process - completely bypasses browser CORS restrictions.
  #
  # On success: updateTextInput(session, "census_id") auto-fills the tract ID,
  #             and leafletProxy flyTo() pans the map to the matched location.
  # On failure: showNotification() displays a user-facing error message.
  # --------------------------------------------------------------------------
  observeEvent(input$verify_btn, {
    raw_addr <- trimws(input$geo_address_input)
    
    if (nchar(raw_addr) < 5) {
      geo_result_rv(list(status = "warn",
                         message = "Please enter a fuller address before clicking Verify.",
                         tract = "", lat = NA, lng = NA))
      return()
    }
    
    # Append WA if not already present to improve match rate for King County
    addr_query <- if (grepl("\\bwa\\b", raw_addr, ignore.case = TRUE)) raw_addr
    else paste0(raw_addr, ", WA")
    
    geo_result_rv(list(status = "loading",
                       message = "Looking up address...",
                       tract = "", lat = NA, lng = NA))
    
    result <- tryCatch({
      resp <- httr::GET(
        url   = CENSUS_GEOCODER_URL,
        query = list(
          address   = tolower(addr_query),
          benchmark = "Public_AR_Current",
          vintage   = "Current_Current",
          format    = "json"
        ),
        httr::timeout(12)
      )
      
      if (httr::http_error(resp)) {
        stop(paste0("Census API returned HTTP ", httr::status_code(resp)))
      }
      
      parsed  <- httr::content(resp, as = "parsed", simplifyVector = FALSE)
      matches <- parsed$result$addressMatches
      
      if (length(matches) == 0) {
        return(list(status = "error",
                    message = paste0("No match found for \"", raw_addr,
                                     "\". Try a fuller address: \"123 Main St, Kent, WA 98032\"."),
                    tract = "", lat = NA, lng = NA))
      }
      
      m      <- matches[[1]]
      tracts <- m$geographies[["Census Tracts"]]
      coords <- m$coordinates
      lat    <- as.numeric(coords$y)
      lng    <- as.numeric(coords$x)
      
      if (length(tracts) == 0 || is.null(tracts[[1]])) {
        return(list(status = "warn",
                    message = paste0("Matched: ", m$matchedAddress,
                                     " - but Census Tract ID could not be resolved. Try adding the ZIP code."),
                    tract = "", lat = lat, lng = lng))
      }
      
      geo    <- tracts[[1]]
      state  <- str_pad(as.character(geo$STATE),  2, pad = "0")
      county <- str_pad(as.character(geo$COUNTY), 3, pad = "0")
      tract  <- str_pad(as.character(geo$TRACT),  6, pad = "0")
      geoid  <- paste0(state, county, tract)
      
      list(status  = "ok",
           message = paste0("Matched: ", m$matchedAddress, " | Tract: ", geoid),
           tract   = geoid,
           lat     = lat,
           lng     = lng)
      
    }, error = function(e) {
      list(status  = "error",
           message = paste0("Address search currently unavailable: ", conditionMessage(e),
                            ". Please manually type the 11-digit Census Tract ID."),
           tract   = "", lat = NA, lng = NA)
    })
    
    geo_result_rv(result)
    
    # Auto-fill the Census Tract ID field in the Submit Project tab
    if (nzchar(result$tract)) {
      updateTextInput(session, "census_id", value = result$tract)
    }
    
    # Fly the map to the geocoded location
    if (!is.na(result$lat) && !is.na(result$lng)) {
      leafletProxy("map") %>%
        flyTo(lng = result$lng, lat = result$lat, zoom = 14)
    }
    
    # Fallback notification if geocoding failed
    if (result$status == "error") {
      showNotification(
        "Address search currently unavailable. Please manually type the 11-digit Census Tract ID.",
        type     = "error",
        duration = 8
      )
    }
  }, ignoreNULL = TRUE)
  
  # Render the geocoding result banner below the address row
  output$geo_result_ui <- renderUI({
    r <- geo_result_rv()
    if (r$status == "idle")    return(NULL)
    if (r$status == "loading") return(div(class = "warn", style = "margin-top:4px;font-size:11.5px;", r$message))
    if (r$status == "ok")      return(div(class = "ok",   style = "margin-top:4px;font-size:11.5px;", r$message))
    if (r$status == "warn")    return(div(class = "warn", style = "margin-top:4px;font-size:11.5px;", r$message))
    if (r$status == "error")   return(div(class = "er",   style = "margin-top:4px;font-size:11.5px;", r$message))
  })
  
  # --------------------------------------------------------------------------
  # GOOGLE SHEET LOGGING
  # --------------------------------------------------------------------------
  observeEvent(input$log_submission_trigger, {
    if (!isTRUE(input$data_opt_in)) {
      message("[LOG] Opt-in not checked - skipping Google Sheet log.")
      return()
    }
    if (SUBMISSIONS_SHEET_ID == "YOUR_SUBMISSIONS_SHEET_ID_HERE") {
      message("[LOG] Placeholder sheet ID - skipping log.")
      return()
    }
    tryCatch({
      row <- tibble::tibble(
        Timestamp    = as.character(Sys.time()),
        Census_Tract = isolate(input$census_id),
        Opted_In     = TRUE
      )
      googlesheets4::sheet_append(SUBMISSIONS_SHEET_ID, row)
      message("[LOG] Submission logged.")
    }, error = function(e) {
      message("[LOG] Google Sheet write failed: ", conditionMessage(e))
    })
  }, ignoreNULL = TRUE)
  
  # --------------------------------------------------------------------------
  # OPT-IN STATUS DISPLAY on Submit tab
  # --------------------------------------------------------------------------
  output$optin_warning <- renderUI({
    if (!isTRUE(input$data_opt_in)) {
      div(class = "warn", style = "margin-bottom:12px;",
          tags$b("Data logging is off. "),
          "Check the opt-in box in the sidebar to enable project data logging. Sending an email summary works regardless of this setting.")
    } else {
      div(class = "ok", style = "margin-bottom:12px;",
          tags$b("Data logging is active. "),
          "When you click the submit button, project data will be logged to the King County ADO tracking sheet.")
    }
  })
  
  # Debounced slider reactives
  wp_d   <- debounce(reactive(input$w_poverty),   500)
  wm_d   <- debounce(reactive(input$w_mfi),       500)
  wr_d   <- debounce(reactive(input$w_rural),     500)
  ruca_d <- debounce(reactive(input$ruca_slider), 500)
  
  # --------------------------------------------------------------------------
  # DATA LOAD
  # --------------------------------------------------------------------------
  load_error <- reactiveVal(NULL)
  
  raw_data <- reactive({
    invalidateLater(300000, session)
    load_error(NULL)
    
    tryCatch({
      read_sheet_csv <- function(url, label) {
        df <- readr::read_csv(url, col_names = TRUE, show_col_types = FALSE,
                              col_types = readr::cols(.default = "c"), trim_ws = TRUE)
        message("[DATA] ", label, ": ", nrow(df), " rows | cols: ",
                paste(names(df), collapse = ", "))
        df
      }
      
      df_cw <- read_sheet_csv(URL_COUNTY_WIDE, "County Wide")
      df_ur <- read_sheet_csv(URL_URBAN_RURAL, "Urban/Rural")
      
      fix_key <- function(df, label) {
        if ("Census Tract" %in% names(df)) return(df)
        idx <- which(str_detect(tolower(names(df)), "census|geoid"))[1]
        if (!is.na(idx)) {
          message("[DATA] '", label, "': 'Census Tract' not found - using '", names(df)[idx], "'")
          names(df)[idx] <- "Census Tract"
          return(df)
        }
        message("[DATA] '", label, "': key column not found - using first column '", names(df)[1], "'")
        names(df)[1] <- "Census Tract"
        df
      }
      
      df_cw <- fix_key(df_cw, "County Wide")
      df_ur <- fix_key(df_ur, "Urban/Rural")
      
      pad_key <- function(df) {
        df[["Census Tract"]] <- str_pad(
          sprintf("%.0f", suppressWarnings(as.numeric(trimws(as.character(df[["Census Tract"]]))))),
          width = 11, pad = "0", side = "left"
        )
        df
      }
      
      df_cw <- pad_key(df_cw)
      df_ur <- pad_key(df_ur)
      
      df_merged <- df_ur %>%
        full_join(
          df_cw %>% select(`Census Tract`, any_of(
            c("Poverty Rate (%)", "MFI Ratio (%)", "Tract Name", "City",
              "King County Area", "OZ 2.0 Eligible?", "Current OZ 1.0")
          )),
          by     = "Census Tract",
          suffix = c("", ".cw")
        )
      
      drop_cols <- names(df_merged)[str_detect(names(df_merged), "\\.cw$")]
      if (length(drop_cols) > 0) df_merged <- df_merged %>% select(-all_of(drop_cols))
      
      if (nrow(df_merged) == 0) stop("Merged dataset contains zero rows.")
      message("[DATA] Merged: ", nrow(df_merged), " rows.")
      df_merged
      
    }, error = function(e) {
      msg <- conditionMessage(e)
      message("[DATA] LOAD FAILED: ", msg)
      load_error(msg)
      NULL
    })
  })
  
  # --------------------------------------------------------------------------
  # SUBMISSION COUNTS
  # --------------------------------------------------------------------------
  submissions_counts <- reactive({
    invalidateLater(300000, session)
    if (SUBMISSIONS_SHEET_ID == "YOUR_SUBMISSIONS_SHEET_ID_HERE") {
      return(tibble(join_key = character(), Project_Count = integer()))
    }
    tryCatch({
      sh  <- googlesheets4::read_sheet(SUBMISSIONS_SHEET_ID)
      col <- intersect(c("Census Tract", "Census_Tract", "census_tract", "GEOID"), names(sh))[1]
      if (is.na(col)) return(tibble(join_key = character(), Project_Count = integer()))
      sh %>%
        mutate(join_key = str_pad(trimws(as.character(.[[col]])), 11, pad = "0")) %>%
        count(join_key, name = "Project_Count")
    }, error = function(e) {
      message("[SUBMISSIONS] Read failed: ", conditionMessage(e))
      tibble(join_key = character(), Project_Count = integer())
    })
  })
  
  # --------------------------------------------------------------------------
  # CORE DATA PROCESSING
  # --------------------------------------------------------------------------
  processed_data <- reactive({
    df_raw <- raw_data()
    if (is.null(df_raw)) return(NULL)
    if (is.null(kc_shapes)) {
      load_error("Shapefile could not be loaded.")
      return(NULL)
    }
    
    get_col <- function(df, nm, def = NA_character_) {
      if (nm %in% names(df)) trimws(as.character(df[[nm]]))
      else { message("[DATA] Column '", nm, "' not found."); rep(def, nrow(df)) }
    }
    
    df <- tryCatch({
      ruca_raw <- suppressWarnings(as.numeric(get_col(df_raw, "RUCA_Score", "1")))
      
      df_raw %>%
        mutate(
          `Census Tract` = as.character(`Census Tract`),
          TractName      = get_col(df_raw, "Tract Name",        ""),
          City           = get_col(df_raw, "City",              ""),
          Area           = get_col(df_raw, "King County Area",  ""),
          oz_eligible    = toupper(get_col(df_raw, "OZ 2.0 Eligible?", "No")) == "YES",
          is_oz1         = toupper(get_col(df_raw, "Current OZ 1.0",   "No")) == "YES",
          RUCA_Score     = ruca_raw,
          is_rural       = !is.na(ruca_raw) & ruca_raw >= ruca_d(),
          p_val          = suppressWarnings(as.numeric(get_col(df_raw, "Poverty Rate (%)", "0"))),
          mfi_raw        = suppressWarnings(as.numeric(get_col(df_raw, "MFI Ratio (%)",    "0")))
        ) %>%
        mutate(
          p_val   = coalesce(p_val, 0),
          i_val   = ifelse(is.na(mfi_raw), 0, pmax(0, 100 - mfi_raw)),
          PriorityLevel = case_when(
            p_val >= 25 | (!is.na(mfi_raw) & mfi_raw < 60) ~ "High",
            p_val >= 15 | (!is.na(mfi_raw) & mfi_raw < 80) ~ "Medium",
            TRUE ~ "Watch"
          ),
          rural_score = ifelse(is_rural, 100, 0),
          need_score  = {
            tw <- max(1, wp_d() + wm_d() + wr_d())
            (p_val * wp_d() / tw) + (i_val * wm_d() / tw) + (rural_score * wr_d() / tw)
          },
          need_score = coalesce(need_score, 0)
        )
    }, error = function(e) {
      load_error(paste0("Processing failed: ", conditionMessage(e)))
      NULL
    })
    
    if (is.null(df)) return(NULL)
    
    df <- df %>%
      filter(!is.na(`Census Tract`),
             nchar(`Census Tract`) == 11,
             !str_detect(`Census Tract`, "^0+$"),
             str_detect(`Census Tract`, "^53"))
    
    message("[GEOID] Valid: ", nrow(df),
            " | Rural (>= ", ruca_d(), "): ", sum(df$is_rural, na.rm = TRUE),
            " | Eligible: ", sum(df$oz_eligible, na.rm = TRUE))
    
    if (nrow(df) == 0) {
      load_error("All rows dropped after GEOID cleaning.")
      return(NULL)
    }
    
    if (input$elig_only)  df <- df %>% filter(!is.na(oz_eligible) & oz_eligible)
    if (input$rural_only) df <- df %>% filter(!is.na(is_rural)    & is_rural)
    if (input$hide_oz1)   df <- df %>% filter(is.na(is_oz1)       | !is_oz1)
    
    if (nrow(df) == 0) { message("[FILTER] All rows filtered out."); return(NULL) }
    
    df <- df %>%
      mutate(
        user_rank   = rank(-need_score, ties.method = "min"),
        in_top25pct = user_rank <= ceiling(nrow(df) * 0.25)
      )
    
    safe_shapes <- tryCatch({
      epsg <- st_crs(kc_shapes)$epsg
      if (is.na(epsg) || epsg != 4326L) st_transform(kc_shapes, 4326) else kc_shapes
    }, error = function(e) st_transform(kc_shapes, 4326))
    
    joined <- tryCatch({
      safe_shapes %>%
        inner_join(
          df %>% mutate(`Census Tract` = as.character(`Census Tract`)),
          by = c("GEOID" = "Census Tract")
        )
    }, error = function(e) {
      load_error(paste0("Spatial join failed: ", conditionMessage(e)))
      NULL
    })
    
    if (is.null(joined) || nrow(joined) == 0) {
      load_error("Spatial join returned zero matches. Check that Census Tract values are 11-digit WA FIPS codes.")
      return(NULL)
    }
    
    joined <- tryCatch(
      st_transform(joined, crs = 4326),
      error = function(e) { message("[CRS] Post-join transform failed."); joined }
    )
    
    message("[JOIN] Matched: ", nrow(joined),
            " | Rural: ", sum(!is.na(joined$is_rural) & joined$is_rural, na.rm = TRUE))
    joined
  })
  
  # --------------------------------------------------------------------------
  # COLOUR ASSIGNMENT
  # --------------------------------------------------------------------------
  get_colors <- function(df, mode) {
    rural <- !is.na(df$is_rural)      & df$is_rural
    hi    <- !is.na(df$PriorityLevel) & df$PriorityLevel == "High"
    med   <- !is.na(df$PriorityLevel) & df$PriorityLevel == "Medium"
    t25p  <- !is.na(df$in_top25pct)   & df$in_top25pct
    switch(mode,
           top25    = case_when(rural ~ PAL$rural, hi ~ PAL$high, med ~ PAL$medium, TRUE ~ PAL$watch),
           top25pct = case_when(t25p & rural ~ PAL$rural, t25p ~ PAL$top25p, TRUE ~ PAL$grey),
           both     = case_when(t25p & rural ~ PAL$rural, t25p ~ PAL$both,   TRUE ~ PAL$grey),
           case_when(hi ~ PAL$high, med ~ PAL$medium, TRUE ~ PAL$watch)
    )
  }
  
  # --------------------------------------------------------------------------
  # SIDEBAR UI OUTPUTS
  # --------------------------------------------------------------------------
  output$weight_bar <- renderUI({
    tw <- max(1, input$w_poverty + input$w_mfi + input$w_rural)
    pp <- round(input$w_poverty / tw * 100)
    pm <- round(input$w_mfi     / tw * 100)
    pr <- 100 - pp - pm
    tags$div(style = "margin:4px 0 14px;",
             tags$div(style = "display:flex;border-radius:8px;overflow:hidden;height:8px;gap:2px;",
                      tags$div(style = paste0("width:", pp, "%;background:#C0392B;border-radius:4px;")),
                      tags$div(style = paste0("width:", pm, "%;background:#F39C12;border-radius:4px;")),
                      tags$div(style = paste0("width:", pr, "%;background:#1A7A4A;border-radius:4px;"))
             ),
             tags$div(style = "display:flex;justify-content:space-between;margin-top:3px;font-size:9.5px;color:#8A9BAC;font-family:'DM Mono',monospace;",
                      tags$span(paste0("Poverty ", pp, "%")),
                      tags$span(paste0("MFI ",     pm, "%")),
                      tags$span(paste0("Rural ",   pr, "%"))
             )
    )
  })
  
  output$ruca_label <- renderUI({
    thr   <- input$ruca_slider
    label <- RUCA_LABELS[as.character(thr)]
    label <- if (is.na(label)) paste("RUCA", thr) else label
    d     <- processed_data()
    n_r   <- if (!is.null(d)) sum(!is.na(as.data.frame(d)$is_rural) & as.data.frame(d)$is_rural, na.rm = TRUE) else 0L
    div(class = "rtip", style = "margin-bottom:8px;",
        tags$b(paste0("Threshold: RUCA >= ", thr, " (", label, "). ")),
        paste0(n_r, " tracts classified as Rural Prioritization."))
  })
  
  output$filter_note <- renderUI({
    if (input$elig_only && input$rural_only)
      div(class = "warn",
          tags$b("Filter conflict: "),
          "All 80 OZ 2.0 eligible King County tracts have RUCA = 1. With both filters active, results will be empty unless the RUCA slider is at 1.")
    else NULL
  })
  
  output$status_box <- renderUI({
    err <- load_error()
    d   <- processed_data()
    if (!is.null(err) && is.null(d)) {
      div(class = "er", "Data load failed.", tags$pre(substr(err, 1, 400)))
    } else if (is.null(d)) {
      div(class = "warn", "No tracts match the current filters.")
    } else {
      df  <- as.data.frame(d)
      n_r <- sum(!is.na(df$is_rural)    & df$is_rural,    na.rm = TRUE)
      n_e <- sum(!is.na(df$oz_eligible) & df$oz_eligible, na.rm = TRUE)
      div(class = "ok",
          paste0(nrow(d), " matched tracts | ", n_r, " Rural Prioritization | ", n_e, " eligible"))
    }
  })
  
  output$stats <- renderUI({
    d  <- processed_data(); if (is.null(d)) return(NULL)
    df   <- as.data.frame(d)
    n_rt <- sum(!is.na(df$is_rural)    & df$is_rural    &
                  !is.na(df$in_top25pct) & df$in_top25pct, na.rm = TRUE)
    n_hi <- sum(!is.na(df$PriorityLevel) & df$PriorityLevel == "High", na.rm = TRUE)
    n_t  <- sum(!is.na(df$in_top25pct)  & df$in_top25pct, na.rm = TRUE)
    div(
      div(class = "sr",
          div(class = "sc", div(class = "sv", nrow(d)), div(class = "sl", "Matched Tracts")),
          div(class = "sc", div(class = "sv", n_hi),    div(class = "sl", "High Need"))
      ),
      div(class = "sr",
          div(class = if (n_rt > 0) "sc hl" else "sc",
              div(class = "sv", n_rt),
              div(class = "sl", "Rural Prioritization Top 25%")),
          div(class = "sc", div(class = "sv", n_t), div(class = "sl", "Top 25% Need"))
      )
    )
  })
  
  output$rural_tip <- renderUI({
    d  <- processed_data(); if (is.null(d)) return(NULL)
    df  <- as.data.frame(d)
    n_r <- sum(!is.na(df$is_rural) & df$is_rural, na.rm = TRUE)
    n_t <- sum(!is.na(df$is_rural) & df$is_rural & !is.na(df$in_top25pct) & df$in_top25pct, na.rm = TRUE)
    if (n_r == 0) {
      div(class = "warn",
          paste0("No Rural Prioritization tracts at RUCA >= ", input$ruca_slider,
                 ". Lower the threshold or uncheck 'OZ 2.0 Eligible Only'."))
    } else {
      div(class = "rtip",
          tags$b(paste0(n_r, " Rural Prioritization tracts. ")),
          if (input$w_rural == 0) "Rural Priority slider is at 0%. Raise it to increase rural tract scores."
          else paste0(n_t, " of ", n_r, " in Rural Prioritization Top 25%. ",
                      if (n_t < n_r) "Raise the Rural Priority weight to lift more."
                      else "All rural tracts are in the top tier."))
    }
  })
  
  output$debug_panel <- renderUI({
    d  <- processed_data(); if (is.null(d)) return(NULL)
    df <- as.data.frame(d) %>%
      filter(!is.na(is_rural) & is_rural, !is.na(GEOID)) %>%
      select(TractName, GEOID, RUCA_Score, need_score) %>%
      arrange(desc(coalesce(need_score, 0))) %>% head(8)
    if (nrow(df) == 0) {
      div(class = "dbg", paste0("DEBUG: 0 Rural Prioritization tracts at RUCA >= ", input$ruca_slider))
    } else {
      rows <- paste0(df$TractName, " [RUCA:", coalesce(as.character(df$RUCA_Score), "?"),
                     "] score:", round(coalesce(df$need_score, 0), 1), collapse = "\n")
      div(class = "dbg", tags$b("Rural Prioritization tracts (top 8 by score):"), tags$br(),
          tags$pre(style = "margin:4px 0 0;font-size:10px;", rows))
    }
  })
  
  output$rankings_header <- renderUI({
    d <- processed_data()
    if (is.null(d)) return(div(class = "warn", "Data not loaded or all tracts filtered out."))
    tw <- max(1, input$w_poverty + input$w_mfi + input$w_rural)
    pp <- round(input$w_poverty / tw * 100)
    pm <- round(input$w_mfi     / tw * 100)
    pr <- 100 - pp - pm
    div(style = "font-size:12px;color:#8A9BAC;margin-bottom:12px;",
        paste0("Ranked by Community Need Score (21 of ", TOTAL_POSSIBLE_POINTS,
               " pts). Weights: Poverty ", pp, "% | Income Gap ", pm, "% | Rural ", pr,
               "%. Click any column header to re-sort."))
  })
  
  # --------------------------------------------------------------------------
  # BASE MAP
  # --------------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = KC_LNG, lat = KC_LAT, zoom = KC_ZOOM)
  })
  
  # --------------------------------------------------------------------------
  # POLYGON UPDATES
  # --------------------------------------------------------------------------
  observe({
    res <- processed_data()
    if (is.null(res)) { leafletProxy("map") %>% clearShapes(); return() }
    
    res_wgs <- tryCatch({
      epsg <- st_crs(res)$epsg
      if (is.na(epsg) || epsg != 4326L) st_transform(res, 4326) else res
    }, error = function(e) tryCatch(st_transform(res, 4326), error = function(e2) res))
    
    df       <- as.data.frame(res_wgs)
    col      <- get_colors(df, input$mode)
    ruca_fmt <- ifelse(is.na(df$RUCA_Score), "N/A", as.character(df$RUCA_Score))
    pov_fmt  <- ifelse(is.na(df$p_val) | df$p_val == 0, "N/A", paste0(round(df$p_val, 1), "%"))
    mfi_fmt  <- ifelse(is.na(df$mfi_raw), "N/A", paste0(round(df$mfi_raw, 1), "% AMI"))
    
    popups <- paste0(
      "<div style='font-family:sans-serif;min-width:240px;font-size:12px;'>",
      "<div style='background:#0A1F35;color:#fff;padding:9px 12px;",
      "margin:-10px -12px 10px;border-radius:4px 4px 0 0;'>",
      "<div style='font-size:10px;color:#F39C12;font-weight:700;text-transform:uppercase;'>",
      ifelse(is.na(df$Area) | df$Area == "", "King County", df$Area), "</div>",
      "<b style='font-size:14px;'>", coalesce(df$TractName, ""), "</b></div>",
      "<table style='width:100%;border-collapse:collapse;'>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;width:130px;'>City</td>",
      "<td><b>", coalesce(df$City, ""), "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Census Tract</td>",
      "<td><code style='font-size:11px;'>", df$GEOID, "</code></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>RUCA Score</td>",
      "<td><b>", ruca_fmt, "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Classification</td>",
      "<td><b style='color:", ifelse(!is.na(df$is_rural) & df$is_rural, "#1A7A4A", "#95A5A6"), "'>",
      ifelse(!is.na(df$is_rural) & df$is_rural, "Rural Prioritization", "Urban"), "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>OZ 2.0 Eligible</td>",
      "<td><b>", ifelse(!is.na(df$oz_eligible) & df$oz_eligible, "Yes", "No"), "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Priority Tier</td>",
      "<td><b>", coalesce(df$PriorityLevel, ""), "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Poverty Rate</td>",
      "<td>", pov_fmt, "</td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>MFI Ratio</td>",
      "<td>", mfi_fmt, "</td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Need Score</td>",
      "<td><b>", ifelse(is.na(df$need_score), "N/A", round(df$need_score, 1)), "</b></td></tr>",
      "<tr><td style='color:#8A9BAC;padding:3px 0;'>Rural Prioritization Top 25%</td>",
      "<td>", ifelse(!is.na(df$in_top25pct) & df$in_top25pct, "Yes", "No"), "</td></tr>",
      "</table>",
      "<div style='margin-top:8px;padding-top:6px;border-top:1px solid #EEF2F7;",
      "font-size:10.5px;color:#8A9BAC;font-style:italic;'>",
      paste0("Community Need = 21 of ", TOTAL_POSSIBLE_POINTS,
             " pts. Market Readiness and Policy Alignment require project submission."),
      "</div></div>"
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data             = res_wgs,
        fillColor        = col,
        fillOpacity      = 0.72,
        color            = "#FFFFFF",
        weight           = 1.4,
        popup            = popups,
        highlightOptions = highlightOptions(
          weight       = 3,
          color        = "#F39C12",
          fillOpacity  = 0.88,
          bringToFront = TRUE
        )
      )
  })
  
  # --------------------------------------------------------------------------
  # RANKINGS TABLE
  # --------------------------------------------------------------------------
  output$tbl <- renderDT({
    res <- processed_data()
    validate(need(!is.null(res), "Data not loaded or all tracts filtered out."))
    
    sub_counts <- submissions_counts() %>%
      rename(`Census Tract` = join_key)
    
    df_out <- as.data.frame(res) %>%
      rename(`Census Tract` = GEOID) %>%
      left_join(sub_counts, by = "Census Tract") %>%
      mutate(Project_Count = coalesce(Project_Count, 0L)) %>%
      arrange(coalesce(user_rank, 99999L)) %>%
      transmute(
        `Need Rank`                    = user_rank,
        `Tract Name`                   = TractName,
        City, Area,
        `Census Tract`                 = `Census Tract`,
        `Priority`                     = PriorityLevel,
        `Eligible`                     = ifelse(!is.na(oz_eligible) & oz_eligible, "Yes", ""),
        `Rural`                        = ifelse(!is.na(is_rural)    & is_rural,    "Yes", ""),
        `RUCA Score`                   = RUCA_Score,
        `OZ 1.0`                       = ifelse(!is.na(is_oz1)      & is_oz1,      "Yes", ""),
        `Poverty %`                    = round(p_val,      1),
        `MFI Ratio`                    = round(mfi_raw,    1),
        `Need Score`                   = round(need_score, 1),
        `Rural Prioritization Top 25%` = ifelse(!is.na(in_top25pct) & in_top25pct, "Yes", ""),
        `Projects`                     = Project_Count
      )
    
    datatable(
      df_out, rownames = FALSE,
      options = list(
        pageLength = 25, scrollX = TRUE, dom = "frtip",
        order = list(list(0, "asc")),
        columnDefs = list(list(className = "dt-center", targets = c(0, 6, 7, 9, 13, 14)))
      ),
      class = "stripe hover compact"
    ) %>%
      formatStyle("Priority",
                  backgroundColor = styleEqual(
                    c("High", "Medium", "Watch"),
                    c("#FADBD8", "#FDEBD0", "#FEF9E7")
                  ), fontWeight = "bold") %>%
      formatStyle("Rural", backgroundColor = styleEqual("Yes", "#E8F8F0")) %>%
      formatStyle("Rural Prioritization Top 25%",
                  backgroundColor = styleEqual("Yes", "#EDE7F6")) %>%
      formatStyle("Projects",
                  backgroundColor = styleInterval(
                    c(0, 1, 3), c("white", "#FEF9E7", "#FFF3CD", "#FFE0B2")
                  ), fontWeight = "bold") %>%
      formatStyle("Need Score",
                  background = styleColorBar(c(0, 100), "#D6E4F0"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
}

shinyApp(ui = ui, server = server)

# ==============================================================================
# VERSION 28.0 REPAIR SUMMARY
# ==============================================================================
#
# ROOT CAUSE OF "Failed to fetch"
# --------------------------------
# The v26.0 geocoding was implemented entirely in JavaScript using the browser's
# fetch() API to call https://geocoding.geo.census.gov directly. On
# shinyapps.io (and some browsers), outbound fetch() calls to third-party
# domains are blocked by:
#   (a) shinyapps.io sandbox restrictions on client-side network requests, and
#   (b) browser security policies that may block mixed-content or cross-origin
#       requests originating from embedded iframes.
# This produced the "Geocoding error: Failed to fetch" message shown in the
# screenshot regardless of address input.
#
# THE FIX: SERVER-SIDE GEOCODING WITH httr
# -----------------------------------------
# All geocoding logic has been moved from the browser (JavaScript) to the
# R server process (httr::GET). The flow is:
#
#   1. User types an address into textInput("geo_address_input") and either
#      presses Enter (captured by a JS keydown handler) or clicks the
#      actionButton("verify_btn").
#
#   2. observeEvent(input$verify_btn, { ... }) fires on the R server.
#
#   3. httr::GET() calls the Census Bureau Geocoding API:
#      https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress
#      with benchmark = "Public_AR_Current" and vintage = "Current_Current".
#      A 12-second timeout is set via httr::timeout(12).
#
#   4. The JSON response is parsed with httr::content(..., simplifyVector = FALSE).
#      The STATE, COUNTY, and TRACT fields from the first matched address are
#      zero-padded and concatenated into an 11-digit GEOID.
#
#   5. updateTextInput(session, "census_id", value = geoid) instantly populates
#      the Census Tract ID field in the Submit Project tab.
#
#   6. leafletProxy("map") %>% flyTo(lng, lat, zoom = 14) pans the map to the
#      matched coordinates.
#
#   7. If httr::GET() throws an error (network timeout, DNS failure, HTTP error),
#      the tryCatch block catches it and calls:
#        showNotification("Address search currently unavailable. Please manually
#        type the 11-digit Census Tract ID.", type = "error", duration = 8)
#      and returns a status = "error" result to geo_result_rv() for the
#      inline banner display.
#
# This approach is immune to browser CORS restrictions because the HTTP request
# originates from the server process, not the client browser.
# ==============================================================================