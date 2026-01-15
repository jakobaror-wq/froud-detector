# 🤖 FRAUD BOT AI - מערכת לזיהוי הונאות חשבונאיות

מערכת מבוססת בינה מלאכותית המנתחת דוחות כספיים שנתיים של חברות ציבוריות וחוזה את הסיכוי להונאה או מניפולציה חשבונאית.

## 📋 תיאור כללי
האפליקציה מבצעת שימוש ב-3 מודלים של למידת מכונה (Ensemble Model):
1. **KNN (K-Nearest Neighbors)** - השוואת החברה לחברות דומות.
2. **Decision Tree** - בחינת ספי רגישות ביחסים הפיננסיים.
3. **Z-Score Outliers** - זיהוי חריגות סטטיסטיות קיצוניות.

## 🛠 כיצד לתפעל את המערכת
1. **מצב משיכת נתונים (Web Mode):**
   * הזן סימול חברה (Ticker). דוגמה: `AAPL` (ארה"ב) או `BEZQ.TA` (ישראל).
   * המערכת תמשוך אוטומטית נתונים מ-Yahoo Finance ו-FMP.
2. **מצב הזנה ידנית (Manual Mode):**
   * הזן נתונים מתוך מאזן ודו"ח רווח והפסד (בערכים של מיליארדים).
3. **תוצאות:**
   * **Intelligence Report:** ציון באחוזים המבטא את רמת החשד.
   * **Risk Radar:** תרשים השוואתי בין החברה לממוצע השוק.

## 🚀 הוראות התקנה למפתחים
1. שכפל את המאגר: `git clone https://github.com/YOUR_USER/fraud-bot-ai.git`
2. וודא שקובץ המודל `fraud_model_brain.rds` נמצא בתיקיית השורש.
3. הרץ את הפקודה `shiny::runApp()` ב-RStudio.

## 🔒 אבטחה
מפתח ה-API מנוהל באמצעות משתני סביבה (`FMP_API_KEY`). אין להעלות מפתחות פרטיים לקוד בצורה גלויה.
