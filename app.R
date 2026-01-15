# ============================================================
# FRAUD BOT AI – FINAL PRODUCTION VERSION
# ============================================================

# 0) חבילות נדרשות - בדיקה והתקנה אוטומטית בשרת
required_packages <- c(
  "shiny", "bslib", "thematic", "plotly", "DT", "dplyr",
  "rvest", "httr", "jsonlite", "class", "rpart", "stringr"
)

for (p in required_packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# 1) הגדרות עיצוב (Lux Dark Theme)
theme_lux_dark <- bslib::bs_theme(
  bootswatch = "darkly",
  primary = "#19D3AE",
  danger  = "#FF4D6D",
  bg      = "#070A12",
  fg      = "#E9EEF7"
)
thematic::thematic_shiny(font = "auto")

# 2) ניהול מפתחות API - שימוש במשתני סביבה לאבטחה
# בשרת (GitHub/Shinyapps), הגדר משתנה בשם FMP_API_KEY
MY_FMP_KEY <- Sys.getenv("FMP_API_KEY")
if (MY_FMP_KEY == "") {
  # מפתח גיבוי (זמני בלבד - מומלץ להסיר לפני שהופכים את ה-Repo לציבורי)
  MY_FMP_KEY <- "kWPsglM2KMZ6aXGKlqwPpFz9MGT6Xp1l" 
}

# --- לוגיקת צד שרת (המוח) ---

# טעינת המודל מהקובץ
load_brain <- function() {
  file_path <- "fraud_model_brain.rds"
  if (!file.exists(file_path)) return(NULL)
  
  obj <- readRDS(file_path)
  # פונקציית העזר שלך לבניית המבנה במידה והוא דאטה פרים גולמי
  if (is.data.frame(obj)) {
    # כאן נכנסת הלוגיקה של build_brain_if_needed שסיפקת
    candidate_feats <- c("Inv_Sales","Rec_Sales","Gross_Margin","Net_Margin","OCF_Margin","Current_Ratio","Debt_Leverage")
    feats <- intersect(candidate_feats, colnames(obj))
    tr_x <- obj[, feats, drop=FALSE]
    tr_y <- factor(obj$Fraud)
    
    list(
      version="v2",
      features=feats,
      scaler=list(center=colMeans(tr_x, na.rm=TRUE), scale=apply(tr_x, 2, sd, na.rm=TRUE)),
      models=list(
        dt=rpart::rpart(Fraud ~ ., data=obj[, c(feats,"Fraud")], method="class"),
        knn=list(train_x=scale(tr_x), train_y=tr_y, k=5),
        z=list(threshold=3)
      ),
      voting=list(weights=c(knn=0.4, dt=0.4, z=0.2), threshold=0.5),
      baselines=list(avg_safe=colMeans(obj[obj$Fraud == 0, feats, drop=FALSE], na.rm=TRUE))
    )
  } else {
    return(obj)
  }
}

# [כאן מגיעות שאר פונקציות ה-HTTP וה-Scraping שסיפקת - הן נשארות ללא שינוי]

# --- UI & SERVER ---
# השתמש ב-UI וב-Server שסיפקת, אך וודא שהטעינה האוטומטית עובדת:

ui <- bslib::page_fluid(
  theme = theme_lux_dark,
  # ... (שאר ה-UI שלך)
)

server <- function(input, output, session) {
  vals <- reactiveValues(brain = load_brain(), brain_loaded = FALSE)
  
  observe({
    if (!is.null(vals$brain)) vals$brain_loaded <- TRUE
  })
  
  # ... (שאר הלוגיקה של ה-Server)
}

shinyApp(ui, server)
