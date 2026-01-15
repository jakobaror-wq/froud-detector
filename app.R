#   FRAUD BOT AI – FINAL APP
#   Save as app.R in same folder as fraud_model_brain.rds, then Run App.

# =========================
# 0) Packages
# =========================
required_packages <- c(
  "shiny","bslib","thematic","plotly","DT","dplyr",
  "rvest","httr","jsonlite","class","rpart","stringr"
)

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    tryCatch(install.packages(p, dependencies = TRUE), error=function(e) NULL)
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}
invisible(lapply(required_packages, install_if_missing))

# =========================
# 1) Lux Dark Theme
# =========================
safe_font <- function(name) {
  tryCatch(bslib::font_google(name), error=function(e) NULL)
}

theme_lux_dark <- bslib::bs_theme(
  bootswatch = "darkly",
  base_font = safe_font("Inter"),
  heading_font = safe_font("Inter"),
  primary = "#19D3AE",
  danger  = "#FF4D6D",
  bg      = "#070A12",
  fg      = "#E9EEF7"
)
thematic::thematic_shiny(font = "auto")

LUX_CSS <- "
:root{
  --bg0:#050712;
  --bg1:#070A12;
  --card: rgba(16, 20, 32, 0.72);
  --card2: rgba(12, 16, 28, 0.72);
  --stroke: rgba(255,255,255,0.08);
  --stroke2: rgba(255,255,255,0.12);
  --text: #E9EEF7;
  --muted: rgba(233,238,247,0.62);
  --emerald:#19D3AE;
  --gold:#D8B56B;
  --red:#FF4D6D;
  --shadow: 0 14px 34px rgba(0,0,0,0.55);
}

body{
  background:
    radial-gradient(1200px 700px at 15% 10%, rgba(25,211,174,0.18), transparent 55%),
    radial-gradient(1000px 700px at 85% 15%, rgba(216,181,107,0.16), transparent 55%),
    radial-gradient(900px 700px at 55% 90%, rgba(255,77,109,0.10), transparent 60%),
    linear-gradient(180deg, var(--bg0) 0%, var(--bg1) 100%) !important;
}

.main-header{
  text-align:center;
  margin-top:26px;
  margin-bottom:18px;
}

.h-title{
  font-weight: 900;
  letter-spacing: 2px;
  text-transform: uppercase;
  font-size: 34px;
  background: linear-gradient(90deg, var(--gold), var(--emerald));
  -webkit-background-clip:text;
  background-clip:text;
  -webkit-text-fill-color: transparent;
}

.h-sub{
  color: var(--muted);
  margin-top: 6px;
}

.lux-card{
  background: var(--card) !important;
  border: 1px solid var(--stroke) !important;
  border-radius: 18px !important;
  box-shadow: var(--shadow) !important;
  backdrop-filter: blur(10px);
}

.lux-card .card-header{
  border-bottom: 1px solid var(--stroke) !important;
  background: rgba(0,0,0,0.15) !important;
  font-weight: 800;
  letter-spacing: 1px;
}

.lux-panel{
  background: var(--card2);
  border: 1px solid var(--stroke);
  border-radius: 16px;
  padding: 14px;
}

.vbox{
  background: rgba(0,0,0,0.18);
  border: 1px solid var(--stroke);
  border-left: 4px solid var(--emerald);
  border-radius: 14px;
  padding: 14px;
  margin-bottom: 10px;
  text-align:center;
}

.vbox-title{
  font-size: 12px;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 1.2px;
  margin-bottom: 6px;
}
.vbox-num{
  font-size: 26px;
  font-weight: 900;
  color: var(--text);
}

.small-note{ color: var(--muted); font-size: 0.86rem; }
.badge-soft{
  display:inline-block;
  padding: 6px 10px;
  border-radius: 999px;
  border: 1px solid var(--stroke);
  background: rgba(0,0,0,0.18);
  color: var(--muted);
  font-size: 0.82rem;
}
.alert{
  border-radius: 16px !important;
  border: 1px solid var(--stroke2) !important;
  box-shadow: var(--shadow);
}
.btn-primary{
  font-weight: 900 !important;
  letter-spacing: 1px !important;
  border-radius: 14px !important;
}

table.dataTable, .dataTables_wrapper .dataTables_filter input{
  color: var(--text) !important;
}
.dataTables_wrapper .dataTables_filter input{
  background: rgba(0,0,0,0.15) !important;
  border: 1px solid var(--stroke) !important;
  border-radius: 10px !important;
}
"

# =========================
# 2) API Key (FMP)
# =========================
MY_FMP_KEY <- Sys.getenv("FMP_KEY")
if (is.na(MY_FMP_KEY) || MY_FMP_KEY == "") {
  MY_FMP_KEY <- "kWPsglM2KMZ6aXGKlqwPpFz9MGT6Xp1l"
}

# =========================
# 3) HTTP helpers + diagnostics
# =========================
.data_cache <- new.env(parent=emptyenv())
.meta_cache <- new.env(parent=emptyenv())

http_get_ai <- function(url, headers=list(), handle=NULL) {
  uas <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15"
  )

  Sys.sleep(runif(1, 0.25, 1.05))

  hdr_list <- c(
    list(
      "User-Agent" = sample(uas, 1),
      "Accept-Language" = "en-US,en;q=0.9",
      "Referer" = "https://www.google.com/",
      "Accept" = "*/*"
    ),
    headers
  )

  hdr_obj <- do.call(httr::add_headers, hdr_list)

  out <- tryCatch({
    resp <- httr::RETRY(
      "GET", url,
      hdr_obj,
      times = 3, pause_min = 1, pause_cap = 6,
      handle = handle,
      httr::timeout(25)
    )
    list(
      ok = httr::status_code(resp) == 200,
      status = httr::status_code(resp),
      text = httr::content(resp, "text", encoding="UTF-8"),
      error = ""
    )
  }, error=function(e){
    list(ok=FALSE, status=NA_integer_, text="", error=e$message)
  })

  out
}

# =========================
# 4) Yahoo session (cookie + crumb)
# =========================
.yh <- new.env(parent=emptyenv())
.yh$handle <- httr::handle("https://finance.yahoo.com")
.yh$crumb <- NULL
.yh$crumb_ts <- as.POSIXct(0)

yahoo_refresh_session <- function(force=FALSE) {
  if (!force && !is.null(.yh$crumb) && difftime(Sys.time(), .yh$crumb_ts, units="mins") < 45) return(TRUE)

  h1 <- http_get_ai("https://finance.yahoo.com/quote/AAPL", handle=.yh$handle)
  if (!h1$ok) return(FALSE)

  h2 <- http_get_ai("https://query1.finance.yahoo.com/v1/test/getcrumb", handle=.yh$handle)
  if (!h2$ok) return(FALSE)

  crumb <- trimws(h2$text)
  if (crumb == "" || nchar(crumb) < 6) return(FALSE)

  .yh$crumb <- crumb
  .yh$crumb_ts <- Sys.time()
  TRUE
}

# =========================
# 5) Data pull: Meta + Annual
# =========================
get_yahoo_quote_meta <- function(tkr) {
  if (exists(tkr, envir=.meta_cache)) return(get(tkr, envir=.meta_cache))
  yahoo_refresh_session()

  url <- paste0(
    "https://query1.finance.yahoo.com/v7/finance/quote?symbols=", tkr,
    if(!is.null(.yh$crumb)) paste0("&crumb=", utils::URLencode(.yh$crumb, reserved=TRUE)) else ""
  )

  r <- http_get_ai(url, handle=.yh$handle)
  if (!r$ok) return(NULL)

  out <- tryCatch({
    j <- jsonlite::fromJSON(r$text)
    q <- j$quoteResponse$result
    if (length(q) == 0) return(NULL)
    data.frame(
      Ticker   = tkr,
      Name     = ifelse(is.null(q$longName[1]), ifelse(is.null(q$shortName[1]), tkr, q$shortName[1]), q$longName[1]),
      Exchange = ifelse(is.null(q$fullExchangeName[1]), "", q$fullExchangeName[1]),
      Currency = ifelse(is.null(q$currency[1]), "", q$currency[1]),
      stringsAsFactors = FALSE
    )
  }, error=function(e) NULL)

  if (!is.null(out)) assign(tkr, out, envir=.meta_cache)
  out
}

v_raw_b <- function(val) {
  if (is.null(val) || is.null(val$raw) || is.na(val$raw)) return(NA_real_)
  as.numeric(val$raw) / 1e9
}

get_yahoo_annual <- function(tkr, fetch_log) {
  yahoo_refresh_session()

  url <- paste0(
    "https://query2.finance.yahoo.com/v10/finance/quoteSummary/", tkr,
    "?modules=incomeStatementHistory,balanceSheetHistory,cashflowStatementHistory,financialData",
    if(!is.null(.yh$crumb)) paste0("&crumb=", utils::URLencode(.yh$crumb, reserved=TRUE)) else ""
  )

  r <- http_get_ai(url, handle=.yh$handle)
  fetch_log[["Yahoo Annual"]] <- list(url=url, status=r$status, ok=r$ok, err=r$error)
  if (!r$ok) return(NULL)

  tryCatch({
    d <- jsonlite::fromJSON(r$text)$quoteSummary$result[[1]]
    inc <- d$incomeStatementHistory$incomeStatementHistory[[1]]
    bal <- d$balanceSheetHistory$balanceSheetStatements[[1]]
    cfl <- d$cashflowStatementHistory$cashflowStatements[[1]]

    report_date <- NA
    if (!is.null(inc$endDate$raw)) {
      report_date <- as.Date(as.POSIXct(inc$endDate$raw, origin="1970-01-01", tz="UTC"))
    }

    sales <- v_raw_b(inc$totalRevenue)
    if (is.na(sales) || sales == 0) return(NULL)

    data.frame(
      Sales                 = sales,
      Net.Income            = v_raw_b(inc$netIncome),
      Gross.Profit          = v_raw_b(inc$grossProfit),
      Operating.Cash.Flow   = v_raw_b(cfl$totalCashFromOperatingActivities),
      Total.Debt            = v_raw_b(d$financialData$totalDebt),
      Inventory             = v_raw_b(bal$inventory),
      Receivable            = v_raw_b(bal$netReceivables),
      Current.Assets        = v_raw_b(bal$totalCurrentAssets),
      Current.Liabilities   = v_raw_b(bal$totalCurrentLiabilities),
      Total.Assets          = v_raw_b(bal$totalAssets),
      Total.Equity          = v_raw_b(bal$totalStockholderEquity),
      ReportDate            = report_date,
      Source                = "Yahoo Annual Data",
      stringsAsFactors = FALSE
    )
  }, error=function(e) NULL)
}

get_fmp_annual <- function(tkr, key, fetch_log) {
  if (is.null(key) || key == "") {
    fetch_log[["FMP Annual"]] <- list(url="(missing key)", status=NA, ok=FALSE, err="Missing FMP key")
    return(NULL)
  }

  base <- "https://financialmodelingprep.com/api/v3/"
  ep <- function(name) paste0(base, name, "/", tkr, "?period=annual&limit=1&apikey=", key)

  urls <- list(
    is = ep("income-statement"),
    bs = ep("balance-sheet-statement"),
    cf = ep("cash-flow-statement")
  )

  r_is <- http_get_ai(urls$is)
  fetch_log[["FMP Annual (IS)"]] <- list(url=urls$is, status=r_is$status, ok=r_is$ok, err=r_is$error)
  if (!r_is$ok) return(NULL)

  is_df <- tryCatch(jsonlite::fromJSON(r_is$text), error=function(e) NULL)
  if (is.null(is_df) || length(is_df) == 0 || (is.data.frame(is_df) && nrow(is_df) == 0)) return(NULL)

  r_bs <- http_get_ai(urls$bs)
  fetch_log[["FMP Annual (BS)"]] <- list(url=urls$bs, status=r_bs$status, ok=r_bs$ok, err=r_bs$error)

  r_cf <- http_get_ai(urls$cf)
  fetch_log[["FMP Annual (CF)"]] <- list(url=urls$cf, status=r_cf$status, ok=r_cf$ok, err=r_cf$error)

  bs_df <- if (r_bs$ok) tryCatch(jsonlite::fromJSON(r_bs$text), error=function(e) NULL) else NULL
  cf_df <- if (r_cf$ok) tryCatch(jsonlite::fromJSON(r_cf$text), error=function(e) NULL) else NULL

  safe_b <- function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x)/1e9
  rep_date <- if (!is.null(is_df$date[1])) as.Date(is_df$date[1]) else as.Date(NA)

  get_col <- function(df, col) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !(col %in% names(df))) return(NA_real_)
    df[[col]][1]
  }

  data.frame(
    Sales                 = safe_b(is_df$revenue[1]),
    Net.Income            = safe_b(is_df$netIncome[1]),
    Gross.Profit          = safe_b(is_df$grossProfit[1]),
    Operating.Cash.Flow   = safe_b(get_col(cf_df, "operatingCashFlow")),
    Total.Debt            = safe_b(get_col(bs_df, "totalDebt")),
    Inventory             = safe_b(get_col(bs_df, "inventory")),
    Receivable            = safe_b(get_col(bs_df, "netReceivables")),
    Current.Assets        = safe_b(get_col(bs_df, "totalCurrentAssets")),
    Current.Liabilities   = safe_b(get_col(bs_df, "totalCurrentLiabilities")),
    Total.Assets          = safe_b(get_col(bs_df, "totalAssets")),
    Total.Equity          = safe_b(get_col(bs_df, "totalStockholdersEquity")),
    ReportDate            = rep_date,
    Source                = "FMP Annual API",
    stringsAsFactors = FALSE
  )
}

get_finviz_raw <- function(tkr, fetch_log) {
  url <- paste0("https://finviz.com/quote.ashx?t=", tkr)
  r <- http_get_ai(url, headers=list("Referer"="https://finviz.com/"))
  fetch_log[["Finviz"]] <- list(url=url, status=r$status, ok=r$ok, err=r$error)
  if (!r$ok) return(NULL)

  tryCatch({
    pg <- rvest::read_html(r$text)
    tables <- pg %>% rvest::html_table(fill = TRUE)

    find_val <- function(key) {
      for (tbl in tables) {
        if (ncol(tbl) < 2) next
        col1 <- as.character(tbl[[1]])
        if (any(stringr::str_detect(col1, key), na.rm=TRUE)) {
          idx <- which(stringr::str_detect(col1, key))[1]
          val_str <- as.character(tbl[idx, 2])
          mult <- 1
          if (stringr::str_detect(val_str, "M")) mult <- 0.001
          if (stringr::str_detect(val_str, "B")) mult <- 1
          clean <- suppressWarnings(as.numeric(stringr::str_replace_all(val_str, "[^0-9.-]", "")))
          if (!is.na(clean)) return(clean * mult)
        }
      }
      NA_real_
    }

    sales <- find_val("Sales")
    if (is.na(sales) || sales == 0) return(NULL)

    data.frame(
      Sales               = sales,
      Net.Income          = find_val("Income"),
      Gross.Profit        = NA_real_,
      Operating.Cash.Flow = NA_real_,
      Total.Debt          = NA_real_,
      Inventory           = NA_real_,
      Receivable          = NA_real_,
      Current.Assets      = NA_real_,
      Current.Liabilities = NA_real_,
      Total.Assets        = NA_real_,
      Total.Equity        = NA_real_,
      ReportDate          = as.Date(NA),
      Source              = "Finviz Text Mining",
      stringsAsFactors = FALSE
    )
  }, error=function(e) NULL)
}

# Master waterfall with preference
get_data_master <- function(tkr, key, pref, fetch_log) {
  cache_key <- paste0(tkr, "::", pref)
  if (exists(cache_key, envir=.data_cache)) return(get(cache_key, envir=.data_cache))

  r <- NULL
  if (pref == "FMP_ONLY") {
    r <- get_fmp_annual(tkr, key, fetch_log)
  } else if (pref == "YAHOO_ONLY") {
    r <- get_yahoo_annual(tkr, fetch_log)
  } else if (pref == "FMP_FIRST") {
    r <- get_fmp_annual(tkr, key, fetch_log)
    if (is.null(r)) r <- get_yahoo_annual(tkr, fetch_log)
    if (is.null(r)) r <- get_finviz_raw(tkr, fetch_log)
  } else { # YAHOO_FIRST
    r <- get_yahoo_annual(tkr, fetch_log)
    if (is.null(r)) r <- get_fmp_annual(tkr, key, fetch_log)
    if (is.null(r)) r <- get_finviz_raw(tkr, fetch_log)
  }

  if (!is.null(r)) assign(cache_key, r, envir=.data_cache)
  r
}

# =========================
# 6) Brain (v2 list OR legacy training df)
# =========================
compute_features <- function(raw) {
  s_div <- function(a,b) ifelse(is.na(b) | b==0, 0, a/b)
  imputed <- c()

  for (nm in names(raw)) raw[[nm]] <- suppressWarnings(as.numeric(raw[[nm]]))
  if (is.na(raw$Sales) || raw$Sales == 0) stop("Sales/Revenue missing or zero.")

  if (is.na(raw$Inventory)) { raw$Inventory <- 0; imputed <- c(imputed,"Inventory") }
  if (is.na(raw$Receivable)) { raw$Receivable <- 0; imputed <- c(imputed,"Receivable") }
  if (is.na(raw$Current.Assets) || raw$Current.Assets==0) { raw$Current.Assets <- raw$Sales*0.25; imputed <- c(imputed,"Current.Assets") }
  if (is.na(raw$Current.Liabilities) || raw$Current.Liabilities==0) { raw$Current.Liabilities <- raw$Sales*0.15; imputed <- c(imputed,"Current.Liabilities") }
  if (is.na(raw$Total.Debt)) { raw$Total.Debt <- 0; imputed <- c(imputed,"Total.Debt") }
  if (is.na(raw$Total.Assets) || raw$Total.Assets==0) { raw$Total.Assets <- raw$Current.Assets/0.35; imputed <- c(imputed,"Total.Assets") }
  if (is.na(raw$Total.Equity)) { raw$Total.Equity <- 0; imputed <- c(imputed,"Total.Equity") }
  if (is.na(raw$Gross.Profit)) { raw$Gross.Profit <- raw$Sales*0.4; imputed <- c(imputed,"Gross.Profit") }
  if (is.na(raw$Net.Income)) { raw$Net.Income <- 0; imputed <- c(imputed,"Net.Income") }
  if (is.na(raw$Operating.Cash.Flow)) { raw$Operating.Cash.Flow <- 0; imputed <- c(imputed,"Operating.Cash.Flow") }

  target <- data.frame(
    Inv_Sales       = s_div(raw$Inventory, raw$Sales),
    Rec_Sales       = s_div(raw$Receivable, raw$Sales),
    Gross_Margin    = s_div(raw$Gross.Profit, raw$Sales),
    Net_Margin      = s_div(raw$Net.Income, raw$Sales),
    OCF_Margin      = s_div(raw$Operating.Cash.Flow, raw$Sales),
    Current_Ratio   = s_div(raw$Current.Assets, raw$Current.Liabilities),
    Debt_Leverage   = s_div(raw$Total.Debt, raw$Current.Assets),

    Debt_Assets     = s_div(raw$Total.Debt, raw$Total.Assets),
    ROA             = s_div(raw$Net.Income, raw$Total.Assets),
    ROE             = s_div(raw$Net.Income, ifelse(raw$Total.Equity==0, NA, raw$Total.Equity)),
    Asset_Turnover  = s_div(raw$Sales, raw$Total.Assets)
  )

  target <- do.call(data.frame, lapply(target, function(x) replace(x, is.infinite(x)|is.na(x), 0)))
  list(target=target, imputed=imputed, raw=raw)
}

build_brain_if_needed <- function(brain_obj) {
  if (is.list(brain_obj) && !is.null(brain_obj$version) && brain_obj$version == "v2") return(brain_obj)

  if (is.data.frame(brain_obj)) {
    if (!("Fraud" %in% colnames(brain_obj))) stop("Brain training data.frame must include column 'Fraud' (0/1).")

    candidate_feats <- c(
      "Inv_Sales","Rec_Sales","Gross_Margin","Net_Margin","OCF_Margin","Current_Ratio","Debt_Leverage",
      "Debt_Assets","ROA","ROE","Asset_Turnover"
    )
    feats <- intersect(candidate_feats, colnames(brain_obj))
    if (length(feats) < 5) stop("Brain file doesn't include the expected ratio columns (features).")

    tr_x <- brain_obj[, feats, drop=FALSE]
    tr_y <- factor(brain_obj$Fraud)

    tr_means <- colMeans(tr_x, na.rm=TRUE)
    tr_sds <- apply(tr_x, 2, sd, na.rm=TRUE)
    tr_sds[is.na(tr_sds) | tr_sds == 0] <- 1

    tr_x_scaled <- scale(tr_x, center=tr_means, scale=tr_sds)
    dt_model <- rpart::rpart(Fraud ~ ., data=brain_obj[, c(feats,"Fraud"), drop=FALSE], method="class")

    avg_safe <- colMeans(brain_obj[brain_obj$Fraud == 0, feats, drop=FALSE], na.rm=TRUE)

    list(
      version="v2",
      features=feats,
      scaler=list(center=tr_means, scale=tr_sds),
      models=list(
        dt=dt_model,
        knn=list(train_x=tr_x_scaled, train_y=tr_y, k=5),
        z=list(threshold=3)
      ),
      voting=list(weights=c(knn=0.4, dt=0.4, z=0.2), threshold=0.5),
      baselines=list(avg_safe=avg_safe)
    )
  } else {
    stop("Unsupported brain format.")
  }
}

predict_with_brain <- function(brain, target) {
  feats <- brain$features
  missing_feats <- setdiff(feats, colnames(target))
  if (length(missing_feats) > 0) for (m in missing_feats) target[[m]] <- 0
  target <- target[, feats, drop=FALSE]

  ctr <- brain$scaler$center[feats]; ctr[is.na(ctr)] <- 0
  scl <- brain$scaler$scale[feats]; scl[is.na(scl) | scl==0] <- 1

  target_scaled <- scale(target, center=ctr, scale=scl)
  target_scaled[is.na(target_scaled)] <- 0

  p_knn <- as.numeric(as.character(class::knn(brain$models$knn$train_x, target_scaled, brain$models$knn$train_y, k=brain$models$knn$k)))
  p_dt  <- as.numeric(as.character(stats::predict(brain$models$dt, target, type="class")))

  z <- abs((as.numeric(target) - ctr) / scl); names(z) <- feats
  p_z <- ifelse(max(z, na.rm=TRUE) > brain$models$z$threshold, 1, 0)

  w <- brain$voting$weights
  score <- p_knn*w["knn"] + p_dt*w["dt"] + p_z*w["z"]
  label <- ifelse(score >= brain$voting$threshold, "חשוד בהונאה", "אינו חשוד בהונאה")

  list(
    score=as.numeric(score),
    label=label,
    missing_features_filled=missing_feats,
    votes=data.frame(Model=c("KNN","Decision Tree","Z-Score"), Vote=c(p_knn,p_dt,p_z), Weight=as.numeric(w[c("knn","dt","z")])),
    zscores=data.frame(Feature=feats, Z=as.numeric(z))
  )
}

# =========================
# 7) UI
# =========================
ui <- bslib::page_fluid(
  theme = theme_lux_dark,
  tags$head(tags$style(HTML(LUX_CSS))),
  div(class="main-header",
      div(class="h-title","FRAUD DETECTION SYSTEM"),
      div(class="h-sub","Lux Dark | Annual Reports | Web + Manual Input Modes")
  ),
  uiOutput("brain_loader_ui"),
  uiOutput("dashboard_ui")
)

# =========================
# 8) Server
# =========================
server <- function(input, output, session) {

  vals <- reactiveValues(
    brain=NULL, brain_loaded=FALSE,
    data=NULL, ratios=NULL, pred=NULL,
    source=NULL, meta=NULL, imputed=NULL,
    fetched_at=NULL, fetch_log=NULL
  )

  # Auto-load brain if exists
  observe({
    if (!vals$brain_loaded && file.exists("fraud_model_brain.rds")) {
      tryCatch({
        obj <- readRDS("fraud_model_brain.rds")
        vals$brain <- build_brain_if_needed(obj)
        vals$brain_loaded <- TRUE
        showNotification("Brain loaded (fraud_model_brain.rds).", type="message")
      }, error=function(e){
        showNotification(paste("Brain load failed:", e$message), type="error")
      })
    }
  })

  output$brain_loader_ui <- renderUI({
    if (vals$brain_loaded) return(NULL)
    div(class="lux-panel", style="max-width:720px;margin:0 auto;",
        h3("SYSTEM OFFLINE"),
        p(class="small-note","Upload fraud_model_brain.rds (or put it in the same folder as app.R)."),
        fileInput("brain_upload","Select Brain File (.rds)", accept=c(".rds"), width="100%")
    )
  })

  observeEvent(input$brain_upload, {
    req(input$brain_upload)
    tryCatch({
      obj <- readRDS(input$brain_upload$datapath)
      vals$brain <- build_brain_if_needed(obj)
      vals$brain_loaded <- TRUE
      showNotification("Brain uploaded and validated.", type="message")
    }, error=function(e){
      showNotification(paste("Invalid brain file:", e$message), type="error")
    })
  })

  output$dashboard_ui <- renderUI({
    if (!vals$brain_loaded) return(NULL)

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width=380,
        title="CONTROLS",
        div(class="badge-soft","מצב עבודה: משיכת נתונים / קלט ידני"),
        br(), br(),

        radioButtons(
          "input_mode", "Source Mode",
          choices = c("משיכת נתונים מהאינטרנט" = "web",
                      "הזנת נתונים ידנית"      = "manual"),
          selected = "web"
        ),

        conditionalPanel(
          condition = "input.input_mode == 'web'",
          tagList(
            div(class="badge-soft","Yahoo format examples: AAPL / NVDA | Tel-Aviv: BEZQ.TA"),
            br(), br(),
            textInput("ticker","Target Ticker", placeholder="e.g. AAPL, NVDA, BEZQ.TA"),
            selectInput(
              "pref",
              "Data Source Preference (fixes fetch issues)",
              choices=c(
                "FMP First (recommended)"="FMP_FIRST",
                "Yahoo First"="YAHOO_FIRST",
                "FMP Only"="FMP_ONLY",
                "Yahoo Only"="YAHOO_ONLY"
              ),
              selected="FMP_FIRST"
            )
          )
        ),

        conditionalPanel(
          condition = "input.input_mode == 'manual'",
          tagList(
            h5("קלט ידני (במיליארדים, עד 4 ספרות אחרי הנקודה)"),
            numericInput("m_sales", "Sales",                 value = 1.0000, min = 0, step = 0.0001),
            numericInput("m_inv",   "Inventory",             value = 0.1000, min = 0, step = 0.0001),
            numericInput("m_rec",   "Receivable",            value = 0.1000, min = 0, step = 0.0001),
            numericInput("m_ni",    "Net Income",            value = 0.0500,           step = 0.0001),
            numericInput("m_gp",    "Gross Profit",          value = 0.4000,           step = 0.0001),
            numericInput("m_ocf",   "Operating Cash Flow",   value = 0.0500,           step = 0.0001),
            numericInput("m_debt",  "Total Debt",            value = 0.2000, min = 0, step = 0.0001),
            numericInput("m_ca",    "Current Assets",        value = 0.3000, min = 0, step = 0.0001),
            numericInput("m_cl",    "Current Liabilities",   value = 0.1500, min = 0.0001, step = 0.0001),
            numericInput("m_ta",    "Total Assets",          value = 0.8000, min = 0.0001, step = 0.0001),
            numericInput("m_te",    "Total Equity",          value = 0.3000,           step = 0.0001),
            div(class="small-note","שים לב: כל המספרים במיליארדי דולר/ש\"ח (לפי ההקשר).")
          )
        ),

        br(),
        actionButton("analyze","RUN ANALYSIS", class="btn-primary btn-lg", width="100%"),
        br(), br(),
        actionButton("test_sources","TEST DATA SOURCES (web mode)", width="100%"),
        br(), br(),
        div(class="small-note",
            "אם משיכת נתונים נכשלת: לחץ על TEST DATA SOURCES, ",
            "בדוק את Diagnostics ונסה מצב FMP Only / FMP First.")
      ),
      bslib::card(class="lux-card",
        bslib::card_header("INTELLIGENCE REPORT"),
        uiOutput("results_area")
      )
    )
  })

  # Diagnostics table
  output$diag_table <- renderDT({
    req(vals$fetch_log)
    df <- do.call(rbind, lapply(names(vals$fetch_log), function(nm){
      x <- vals$fetch_log[[nm]]
      data.frame(
        Method=nm,
        Status=ifelse(is.null(x$status), NA, x$status),
        OK=ifelse(is.null(x$ok), FALSE, x$ok),
        Error=ifelse(is.null(x$err), "", x$err),
        URL=ifelse(is.null(x$url), "", x$url),
        stringsAsFactors=FALSE
      )
    }))
    DT::datatable(df, options=list(pageLength=6, scrollX=TRUE), rownames=FALSE)
  })

  # Test sources button (web only)
  observeEvent(input$test_sources, {
    req(input$input_mode == "web")
    tkr <- toupper(trimws(ifelse(is.null(input$ticker) || input$ticker=="", "AAPL", input$ticker)))
    fetch_log <- list()

    tmp1 <- get_fmp_annual(tkr, MY_FMP_KEY, fetch_log)
    tmp2 <- get_yahoo_annual(tkr, fetch_log)
    tmp3 <- get_finviz_raw(tkr, fetch_log)

    vals$fetch_log <- fetch_log
    showNotification("Diagnostics updated (see Diagnostics table).", type="message")
  })

  # MAIN ANALYSIS
  observeEvent(input$analyze, {
    if (input$input_mode == "web") {
      req(input$ticker)
    }

    showModal(modalDialog(
      div(class="spinner-border text-primary", style="width:3rem;height:3rem;"),
      h4(if (input$input_mode=="web") "Fetching annual data & running brain..."
         else "Running brain on manual input..."),
      footer=NULL, easyClose=FALSE
    ))

    fetch_log <- list()
    vals$meta <- NULL

    if (input$input_mode == "web") {
      tkr <- toupper(trimws(input$ticker))
      vals$meta <- tryCatch(get_yahoo_quote_meta(tkr), error=function(e) NULL)
      raw <- get_data_master(tkr, MY_FMP_KEY, input$pref, fetch_log)
      vals$fetch_log <- fetch_log
    } else {
      raw <- data.frame(
        Sales               = input$m_sales,
        Net.Income          = input$m_ni,
        Gross.Profit        = input$m_gp,
        Operating.Cash.Flow = input$m_ocf,
        Total.Debt          = input$m_debt,
        Inventory           = input$m_inv,
        Receivable          = input$m_rec,
        Current.Assets      = input$m_ca,
        Current.Liabilities = input$m_cl,
        Total.Assets        = input$m_ta,
        Total.Equity        = input$m_te,
        ReportDate          = as.Date(NA),
        Source              = "Manual Input",
        stringsAsFactors    = FALSE
      )
    }

    removeModal()

    if (!is.null(raw)) {
      vals$data <- raw
      vals$source <- raw$Source[1]
      vals$fetched_at <- Sys.time()

      tryCatch({
        calc <- compute_features(raw)
        vals$data <- calc$raw
        vals$ratios <- calc$target
        vals$imputed <- calc$imputed
        vals$pred <- predict_with_brain(vals$brain, calc$target)
      }, error=function(e){
        vals$pred <- NULL
        showNotification(paste("Computation error:", e$message), type="error")
      })
    } else {
      vals$pred <- NULL
      if (input$input_mode == "web") {
        showNotification("Fetch failed (web mode). Check Diagnostics.", type="error")
      } else {
        showNotification("Manual input error.", type="error")
      }
    }
  })

  output$votes_table <- renderDT({
    req(vals$pred)
    DT::datatable(vals$pred$votes, options=list(dom='t'), rownames=FALSE)
  })

  output$zs_table <- renderDT({
    req(vals$pred)
    top <- vals$pred$zscores[order(-vals$pred$zscores$Z), ][1:min(5, nrow(vals$pred$zscores)), ]
    DT::datatable(top, options=list(dom='t'), rownames=FALSE) %>% DT::formatRound("Z", digits=2)
  })

  output$raw_table <- renderDT({
    req(vals$data)
    d <- vals$data %>% dplyr::select(-Source)
    num_cols <- names(d)[sapply(d, is.numeric)]
    DT::datatable(d, options=list(pageLength=5, scrollX=TRUE), rownames=FALSE) %>%
      DT::formatRound(columns=num_cols, digits=4)
  })

  output$radar_plot <- renderPlotly({
    req(vals$ratios, vals$brain)
    feats <- vals$brain$features
    market_avg <- vals$brain$baselines$avg_safe
    market_avg <- market_avg[feats]; market_avg[is.na(market_avg)] <- 0
    target <- vals$ratios[1, feats, drop=FALSE]

    plotly::plot_ly(type='scatterpolar', fill='toself') %>%
      plotly::add_trace(r=as.numeric(market_avg), theta=feats, name='Market Avg',
                        line=list(color='rgba(233,238,247,0.40)'), fillcolor='rgba(233,238,247,0.06)') %>%
      plotly::add_trace(r=as.numeric(target[1,]), theta=feats, name='Target',
                        line=list(color='#19D3AE'), fillcolor='rgba(25,211,174,0.22)') %>%
      plotly::layout(
        paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)',
        font=list(color='#E9EEF7'),
        polar=list(bgcolor="rgba(0,0,0,0)", radialaxis=list(visible=TRUE, showline=FALSE)),
        showlegend=TRUE, legend=list(orientation="h", x=0.25, y=-0.12)
      )
  })

  output$results_area <- renderUI({
    if (is.null(vals$pred)) {
      return(div(style="padding:22px;",
                 div(class="lux-panel",
                     h4("WAITING FOR INPUT / OR FETCH FAILED"),
                     p(class="small-note","Run analysis. In web mode – if data didn’t load, open Diagnostics."),
                     br(),
                     h5("Diagnostics"),
                     DTOutput("diag_table")
                 )))
    }

    score_pct <- round(vals$pred$score*100, 1)
    is_risk <- vals$pred$label == "חשוד בהונאה"

    meta_line <- ""
    if (!is.null(vals$meta) && nrow(vals$meta) > 0) {
      meta_line <- paste0(vals$meta$Name[1], " | ", vals$meta$Exchange[1], " | ", vals$meta$Currency[1])
    }

    rep_date <- if (!is.null(vals$data$ReportDate[1]) && !is.na(vals$data$ReportDate[1])) as.character(vals$data$ReportDate[1]) else "Unknown"

    warn_lines <- c()
    if (!is.null(vals$imputed) && length(vals$imputed) > 0) warn_lines <- c(warn_lines, paste0("Imputed: ", paste(unique(vals$imputed), collapse=", ")))
    if (!is.null(vals$pred$missing_features_filled) && length(vals$pred$missing_features_filled) > 0) warn_lines <- c(warn_lines, paste0("Missing features filled with 0: ", paste(vals$pred$missing_features_filled, collapse=", ")))

    div(style="padding:18px;",
      div(class=paste0("alert ", if(is_risk) "alert-danger" else "alert-primary"),
          style="text-align:center;",
          h1(paste0(score_pct,"%")),
          h3(vals$pred$label),
          if (meta_line!="") div(class="small-note", meta_line),
          div(class="small-note", paste0("Source: ", vals$source, " | ReportDate: ", rep_date)),
          div(class="small-note", paste0("Fetched at: ", format(vals$fetched_at, "%Y-%m-%d %H:%M:%S")))
      ),

      if (length(warn_lines) > 0) {
        div(class="lux-panel",
            strong("Notes"), tags$ul(lapply(warn_lines, tags$li)))
      },

      bslib::layout_columns(
        col_widths=c(8,4),

        bslib::card(class="lux-card",
          bslib::card_header("RISK RADAR"),
          plotlyOutput("radar_plot", height="360px")
        ),

        bslib::card(class="lux-card",
          bslib::card_header("KEY FINANCIALS (Billions)"),
          div(class="vbox", div(class="vbox-title","REVENUE"),     div(class="vbox-num", round(vals$data$Sales,4))),
          div(class="vbox", div(class="vbox-title","NET INCOME"),  div(class="vbox-num", round(vals$data$Net.Income,4))),
          div(class="vbox", div(class="vbox-title","TOTAL DEBT"),  div(class="vbox-num", round(vals$data$Total.Debt,4))),
          bslib::card_header("MODEL VOTES"),
          DTOutput("votes_table"),
          br(),
          bslib::card_header("TOP OUTLIERS (Z)"),
          DTOutput("zs_table")
        )
      ),

      br(),
      bslib::card(class="lux-card",
        bslib::card_header("RAW ANNUAL DATA"),
        DTOutput("raw_table")
      ),

      br(),
      div(class="lux-panel",
          h5("Diagnostics (Why fetch fails – web mode only)"),
          p(class="small-note","If Yahoo/FMP block (403/401/429), switch preference to FMP Only / FMP First, or use Manual Input mode."),
          DTOutput("diag_table")
      )
    )
  })
}

shiny::shinyApp(ui, server)
