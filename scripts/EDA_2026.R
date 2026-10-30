# ============================================================================== 
# 2026
# ============================================================================== 
dim(app_2026)
names(app_2026)
str(app_2026)
summary(app_2026)


# ============================================================================== 
# 1. ANALISI UNIVARIATA: VARIABILI CATEGORICHE
# ============================================================================== 

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Status
tab_status_26 <- table(app_2026$status, useNA = "ifany")
prop_status_26 <- prop.table(tab_status_26) * 100
print(tab_status_26)
print(round(prop_status_26, 2))
barplot(tab_status_26, 
        main = "Status di Selezione (2026)", 
        ylab = "Frequenza Assoluta", 
        col = c("#00A4CC", "#042333"), 
        border = NA)
box(bty = "l")

# Is Registered
tab_isreg_26 <- table(app_2026$is_registered, useNA = "ifany")
prop_isreg_26 <- prop.table(tab_isreg_26) * 100
print(tab_isreg_26)
print(round(prop_isreg_26, 2))
barplot(tab_isreg_26, 
        main = "Startup Registrata? (2026)", 
        ylab = "Frequenza Assoluta", 
        col = c("#B0E0E6", "#104E8B"), 
        border = NA)
box(bty = "l")

# Dev Stage
tab_dev_26 <- table(app_2026$dev_stage, useNA = "ifany")
prop_dev_26 <- prop.table(tab_dev_26) * 100
print(tab_dev_26)
print(round(prop_dev_26, 2))
par(mar = c(7, 4, 3, 2) + 0.1)
barplot(tab_dev_26, 
        main = "Fase di Sviluppo (Dev Stage) - 2026", 
        ylab = "Frequenza Assoluta (N)", 
        col = "#4682B4", border = "white", las = 2, cex.names = 0.9)
box(bty = "l")

# Fund Stage
tab_fund_26 <- table(app_2026$fund_stage, useNA = "ifany")
prop_fund_26 <- prop.table(tab_fund_26) * 100
print(tab_fund_26)
print(round(prop_fund_26, 2))
par(mar = c(8, 4, 3, 2) + 0.1)
barplot(tab_fund_26, 
        main = "Stadio di Finanziamento (Fund Stage) - 2026", 
        ylab = "Frequenza Assoluta (N)", 
        col = "#001F3F", border = "white", las = 2, cex.names = 0.85)
box(bty = "l")

# Business Model
tab_bm_26 <- table(app_2026$business_model, useNA = "ifany")
prop_bm_26 <- prop.table(tab_bm_26) * 100
print(tab_bm_26)
print(round(prop_bm_26, 2))
par(mar = c(5, 4, 4, 2))
barplot(tab_bm_26, 
        main = "Modello di Business (2026)", 
        ylab = "Frequenza Assoluta", 
        col = "#104E8B", border = "white")
box(bty = "l")

# Top 10 Paesi di Provenienza (Grafico Orizzontale Standardizzato)
tab_country_26 <- table(app_2026$country, useNA = "ifany")
tab_country_sorted <- sort(tab_country_26, decreasing = TRUE)
print(head(tab_country_sorted, 10))
par(mar = c(5, 10, 4, 2))
barplot(head(tab_country_sorted, 10), 
        main = "Top 10 Paesi di Provenienza (2026)", 
        xlab = "Frequenza Assoluta", 
        col = "#00A4CC", border = "white", horiz = TRUE, las = 1, cex.names = 0.85)
box(bty = "l")

# Settori Blue Economy / Q1_category (Grafico Orizzontale Standardizzato)
tab_cat_26 <- table(app_2026$Q1_category, useNA = "ifany")
tab_cat_sorted <- sort(tab_cat_26, decreasing = TRUE)
print(tab_cat_sorted)
par(mar = c(5, 14, 4, 2))
barplot(tab_cat_sorted, 
        main = "Settori Blue Economy (2026)", 
        xlab = "Frequenza Assoluta", 
        col = "#50BFE6", border = "white", horiz = TRUE, las = 1, cex.names = 0.7)
box(bty = "l")
par(mar = c(5, 4, 4, 2)) # Ripristino margini


# ============================================================================== 
# 2. ANALISI UNIVARIATA: VARIABILI QUANTITATIVE (Layout Simmetrico 1x2)
# ============================================================================== 

# Founding Year
summary_founding_26 <- summary(app_2026$founding_year)
sd_founding_26 <- sd(app_2026$founding_year, na.rm = TRUE)
print(summary_founding_26)
print(round(sd_founding_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$founding_year, 
     main = "Anno di Fondazione (2026)", 
     xlab = "Anno", ylab = "Frequenza", col = "#4682B4", border = "white")
boxplot(app_2026$founding_year, 
        main = "Distribuzione (2026)", 
        ylab = "Anno di Fondazione", col = "lightblue", border = "#042333")
par(mfrow = c(1, 1))


# Team Size
summary_team_26 <- summary(app_2026$team_size)
sd_team_26 <- sd(app_2026$team_size, na.rm = TRUE)
print(summary_team_26)
print(round(sd_team_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$team_size, 
     main = "Dimensione del Team (2026)", 
     xlab = "Numero di Membri", ylab = "Frequenza", col = "#104E8B", border = "white")
boxplot(app_2026$team_size, 
        main = "Distribuzione (2026)", 
        ylab = "Numero di Membri", col = "lightblue", border = "#042333")
par(mfrow = c(1, 1))


# Money Raised
summary_money_26 <- summary(app_2026$money_raised)
sd_money_26 <- sd(app_2026$money_raised, na.rm = TRUE)
print(summary_money_26)
print(round(sd_money_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$money_raised, 
     main = "Istogramma: Money Raised (2026)", 
     xlab = "Fondi Raccolti", ylab = "Frequenza", col = "#4682B4", border = "white")
boxplot(app_2026$money_raised, 
        main = "Distribuzione (2026)", 
        ylab = "Fondi Raccolti", col = "#4682B4", border = "#042333", pch = 18, outcol = "#104E8B")
par(mfrow = c(1, 1))


# Amount Raising
summary_amount_26 <- summary(app_2026$amount_raising)
sd_amount_26 <- sd(app_2026$amount_raising, na.rm = TRUE)
print(summary_amount_26)
print(round(sd_amount_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$amount_raising, 
     main = "Amount Raising Target (2026)", 
     xlab = "Target Capitale (EUR/USD)", ylab = "Frequenza", col = "#000080", border = "white")
boxplot(app_2026$amount_raising, 
        main = "Distribuzione (2026)", 
        ylab = "Target Capitale (EUR/USD)", col = "lightblue", border = "#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))


# Runway
summary_runway_26 <- summary(app_2026$runway)
sd_runway_26 <- sd(app_2026$runway, na.rm = TRUE)
print(summary_runway_26)
print(round(sd_runway_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$runway, 
     main = "Istogramma: Runway (2026)", 
     xlab = "Mesi", ylab = "Frequenza", col = "#104E8B", border = "white")
boxplot(app_2026$runway, 
        main = "Distribuzione (2026)", 
        ylab = "Mesi", col = "lightblue", border = "#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))


# TRL Clean Extraction
app_2026$trl_clean <- app_2026$trl_level
app_2026$trl_clean <- gsub("‚Äì", "-", app_2026$trl_clean)
app_2026$trl_clean <- gsub("^([0-9]+).*", "\\1", app_2026$trl_clean)
app_2026$trl_clean <- as.numeric(app_2026$trl_clean)

summary_trl_26 <- summary(app_2026$trl_clean)
sd_trl_26 <- sd(app_2026$trl_clean, na.rm = TRUE)
print(summary_trl_26)

par(mfrow = c(1, 2))
hist(app_2026$trl_clean, 
     main = "Istogramma: TRL (2026)", 
     xlab = "Livello TRL", ylab = "Frequenza", col = "#00A4CC", border = "white", breaks = seq(0, 9, by = 1))
boxplot(app_2026$trl_clean, 
        main = "Distribuzione (2026)", 
        ylab = "Livello TRL", col = "lightblue", border = "#042333")
par(mfrow = c(1, 1))


# ============================================================================== 
# 3. MERGE PUNTEGGI JURY E INTERNAL SCORE (MANUAL MAPPING)
# ============================================================================== 

app_2026$startup_name <- trimws(app_2026$startup_name)

# JURY SCORE
df_jury <- data.frame(
  startup_name = c(
    "OceanEye", "Windtracx", "Sea Technologies International", 
    "Green Brine Technologies", "Nereia", "Ozeaon", 
    "Schrodinger Technologie Ltd", "AquaCircle Biorefinery", 
    "Sundew Bio ApS", "PhyCo Technologies", "Divescout", 
    "Pack Away", "Algrid", "ClimaFund", "Recycllux"
  ),
  jury_avg_score_raw = c(
    "4,89", "7,15", "5,89", "5,81", "6,70", "5,52", 
    "8,37", "6,52", "7,19", "7,56", "5,48", "7,37", 
    "5,56", "6,19", "6,48"
  )
)

df_jury$jury_avg_score_clean <- as.numeric(gsub(",", ".", df_jury$jury_avg_score_raw))
df_jury$startup_name[df_jury$startup_name == "OceanEye"] <- "Ocean Eye"
df_jury$startup_name[df_jury$startup_name == "Sea Technologies International"] <- "Sea Technology International"
df_jury$startup_name[df_jury$startup_name == "Green Brine Technologies"] <- "Green Brine Technologies CC"
df_jury$startup_name[df_jury$startup_name == "Nereia"] <- "NEREIA"
df_jury$startup_name[df_jury$startup_name == "Schrodinger Technologie Ltd"] <- "Schrödinger Technologie"
df_jury$startup_name[df_jury$startup_name == "PhyCo Technologies"] <- "PhyCo Technologies Inc."
df_jury$startup_name[df_jury$startup_name == "Divescout"] <- "DiveScout"
df_jury$startup_name[df_jury$startup_name == "Pack Away"] <- "PACK AWAY AB"
df_jury$startup_name <- trimws(df_jury$startup_name)

app_2026 <- merge(app_2026, df_jury[, c("startup_name", "jury_avg_score_clean")], by = "startup_name", all.x = TRUE)
app_2026$jury_avg_score <- app_2026$jury_avg_score_clean
app_2026$jury_avg_score_clean <- NULL

# INTERNAL SCORE
df_internal <- data.frame(
  startup_name = c(
    "OceanEye", "Windtracx", "Sea Technologies International", 
    "Green Brine Technologies", "Nereia", "Ozeaon", 
    "Schrodinger Technologie Ltd", "AquaCircle Biorefinery", 
    "Sundew Bio ApS", "PhyCo Technologies", "Divescout", 
    "Pack Away", "Algrid", "ClimaFund", "AGROM", 
    "Aykrone Corp.", "Before the Sea Africa", "Climatenza", 
    "Conceptosplasticos", "Coral Collective", "D-Olivette Labs", 
    "Das AquaVita", "Recycllux", "ecotera home", "ETS PROMINDS", 
    "Fundacion Mar del Sur", "Geb-Ra Food & Powers", "Goma soil", 
    "Grocenics", "NanBioWater", "NOPA Desing", 
    "Novaqua Technologies Private Limited", "Ocean ops", "RedGet.io", 
    "Solacia, Inc.", "Tabernacle Space Islands", "Trascargavela Lda.", 
    "Water Ingenious", "weavair sp.zoo"
  ),
  internal_avg_score_raw = c(
    "4,45", "4,56", "4,30", "4,15", "4,33", "4,50", 
    "4,24", "4,45", "4,65", "4,48", "4,39", "4,60", 
    "4,47", "4,70", "3,69", "3,37", "4,03", "4,46", 
    "4,67", "4,31", "4,43", "4,48", "6,48", "3,48", 
    "4,54", "4,64", "3,38", "3,35", "4,08", "3,52", 
    "4,50", "4,33", "3,89", "4,25", "3,56", "4,28", 
    "4,53", "3,77", "4,25"
  )
)

df_internal$internal_avg_score_clean <- as.numeric(gsub(",", ".", df_internal$internal_avg_score_raw))
df_internal$startup_name[df_internal$startup_name == "OceanEye"] <- "Ocean Eye"
df_internal$startup_name[df_internal$startup_name == "Sea Technologies International"] <- "Sea Technology International"
df_internal$startup_name[df_internal$startup_name == "Green Brine Technologies"] <- "Green Brine Technologies CC"
df_internal$startup_name[df_internal$startup_name == "Nereia"] <- "NEREIA"
df_internal$startup_name[df_internal$startup_name == "Schrodinger Technologie Ltd"] <- "Schrödinger Technologie"
df_internal$startup_name[df_internal$startup_name == "PhyCo Technologies"] <- "PhyCo Technologies Inc."
df_internal$startup_name[df_internal$startup_name == "Divescout"] <- "DiveScout"
df_internal$startup_name[df_internal$startup_name == "Pack Away"] <- "PACK AWAY AB"
df_internal$startup_name[df_internal$startup_name == "weavair sp.zoo"] <- "weavair sp.zoo."
df_internal$startup_name[df_internal$startup_name == "Fundacion Mar del Sur"] <- "Fundación Mar del Sur"
df_internal$startup_name[df_internal$startup_name == "Geb-Ra Food & Powers"] <- "Geb-Ra Food & Power"
df_internal$startup_name[df_internal$startup_name == "Grocenics"] <- "Grogenics"
df_internal$startup_name[df_internal$startup_name == "NOPA Desing"] <- "NOPA Design"
df_internal$startup_name[df_internal$startup_name == "Trascargavela Lda."] <- "Transcargavela Lda."
df_internal$startup_name <- trimws(df_internal$startup_name)

punteggio_tabernacle <- df_internal$internal_avg_score_clean[df_internal$startup_name == "Tabernacle Space Islands"]
df_internal <- df_internal[df_internal$startup_name != "Tabernacle Space Islands", ]

app_2026 <- merge(app_2026, df_internal[, c("startup_name", "internal_avg_score_clean")], by = "startup_name", all.x = TRUE)
app_2026$internal_avg_score <- app_2026$internal_avg_score_clean
app_2026$internal_avg_score_clean <- NULL

# Assegnazioni Manuali
idx_final_reset <- which(app_2026$app_id == 2723555)
app_2026$internal_avg_score[idx_final_reset] <- punteggio_tabernacle

idx_ocean <- which(app_2026$app_id == 2709050)
app_2026$internal_avg_score[idx_ocean] <- 3.89

print(sum(!is.na(app_2026$jury_avg_score)))
print(sum(!is.na(app_2026$internal_avg_score)))


# Internal Avg Score - Grafici (Sistemati)
summary_internal_26 <- summary(app_2026$internal_avg_score)
sd_internal_26 <- sd(app_2026$internal_avg_score, na.rm = TRUE)
iqr_internal_26 <- IQR(app_2026$internal_avg_score, na.rm = TRUE)

print(summary_internal_26)
print(round(sd_internal_26, 2))
print(round(iqr_internal_26, 2))

par(mfrow = c(1, 2))
hist(app_2026$internal_avg_score, 
     main = "Punteggio Interno (2026)", 
     xlab = "Punteggio (1-5)", ylab = "Frequenza", col = "#104E8B", border = "white", breaks = 15) 
boxplot(app_2026$internal_avg_score, 
        main = "Distribuzione (2026)", 
        ylab = "Punteggio (1-5)", col = "lightblue", border = "#042333", pch = 19)
par(mfrow = c(1, 1))

# Jury Avg Score - Grafici
par(mfrow = c(1, 2))
hist(app_2026$jury_avg_score, 
     main = "Punteggio Giuria (2026)", 
     xlab = "Punteggio", ylab = "Frequenza", col = "#00A4CC", border = "white") 
boxplot(app_2026$jury_avg_score, 
        main = "Distribuzione (2026)", 
        ylab = "Punteggio Giuria", col = "#B0E0E6", border = "#042333", pch = 19)
par(mfrow = c(1, 1))


# ============================================================================== 
# 4. ANALISI DI CORRELAZIONE (PARTE COMPLETATA)
# ============================================================================== 
library(corrplot)

num_vars_2026 <- app_2026[, c("founding_year", "money_raised", "runway", "amount_raising", "team_size", "internal_avg_score", "jury_avg_score", "trl_clean")]
cor_matrix_2026 <- cor(num_vars_2026, use = "pairwise.complete.obs")
print(round(cor_matrix_2026, 2))

col_blue_economy <- colorRampPalette(c("#042333", "#ffffff", "#00A4CC"))(200)

par(mar = c(1, 1, 4, 1))
corrplot(cor_matrix_2026, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         col = col_blue_economy,
         title = "Matrice di Correlazione (2026)",
         mar = c(0, 0, 2, 0))
par(mar = c(5, 4, 4, 2) + 0.1)



# ============================================================================== 
# 5. ANALISI BIVARIATA: INCROCI QUALITATIVI ESPLICATIVI (PARTE COMPLETATA)
# ============================================================================== 

# --- Creazione variabili raggruppate per l'incrocio ---
freq_country <- sort(table(app_2026$country), decreasing = TRUE)
top_countries <- names(freq_country)[1:10] 
app_2026$country_grouped <- ifelse(is.na(app_2026$country), "Not Specified",
                                   ifelse(app_2026$country %in% top_countries, 
                                          app_2026$country, 
                                          "Rest of the World"))

freq_tags <- sort(table(app_2026$tags), decreasing = TRUE)
top_tags <- names(freq_tags)[1:min(10, length(freq_tags))]
app_2026$tags_grouped <- ifelse(is.na(app_2026$tags), "Not Specified",
                                ifelse(app_2026$tags %in% top_tags, 
                                       app_2026$tags, 
                                       "Other Sectors"))

app_2026$country_grouped <- factor(app_2026$country_grouped)
app_2026$tags_grouped <- factor(app_2026$tags_grouped)


# ============================================================================== 
# ESECUZIONE ANALISI BIVARIATA CON VARIABILI TOP 10
# ============================================================================== 

# 1. Dev Stage vs Business Model
tab_dev_bm_26 <- table(app_2026$dev_stage, app_2026$business_model)
print(tab_dev_bm_26)
print(chisq.test(tab_dev_bm_26, simulate.p.value = TRUE))
mosaicplot(tab_dev_bm_26, main = "Dev Stage vs Business Model (2026)", col = c("#B0E0E6", "#4682B4", "#104E8B"), border = "white")

opar <- par(no.readonly = TRUE)

# 2. Country (Top 10) vs Dev Stage
tab_country_dev_26 <- table(app_2026$country_grouped, app_2026$dev_stage)
print(tab_country_dev_26)
print(chisq.test(tab_country_dev_26, simulate.p.value = TRUE))

par(mar = c(6, 12, 4, 2), cex.axis = 0.7)
mosaicplot(tab_country_dev_26, 
           main = "Top 10 Countries vs Dev Stage (2026)", 
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"), 
           border = "white", las = 1) 
par(opar)


# 3. Tags (Top 10) vs Country (Top 10)
tab_tags_country_26 <- table(app_2026$tags_grouped, app_2026$country_grouped)
print(tab_tags_country_26)
print(chisq.test(tab_tags_country_26, simulate.p.value = TRUE))

par(mar = c(10, 15, 4, 2), cex.axis = 0.6)
mosaicplot(tab_tags_country_26, 
           main = "Top 10 Sectors vs Top 10 Countries (2026)", 
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4", "#104E8B"), 
           border = "white", las = 2) 
par(opar) # Ripristino


# 4. Fund Stage vs Dev Stage 
tab_fund_dev_26 <- table(app_2026$fund_stage, app_2026$dev_stage)
print(tab_fund_dev_26)
print(chisq.test(tab_fund_dev_26, simulate.p.value = TRUE))
par(mar=c(6,6,4,2), cex.axis=0.8)
mosaicplot(tab_fund_dev_26, main = "Fund Stage vs Dev Stage (2026)", col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"), border = "white", las=1)
par(opar)


# ============================================================================== 
# 6. ANALISI BIVARIATA: ESPLICATIVE vs TARGET STATUS (PARTE COMPLETATA)
# ============================================================================== 
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1, 1), cex.axis = 1)
col_blue_palette <- c("#042333", "#00A4CC")
col_acc <- "#00A4CC" 
col_rej <- "#042333" 

# --- A) CATEGORICHE vs STATUS ---

# Status vs Dev Stage
tab_dev_status_26 <- table(app_2026$dev_stage, app_2026$status)
print(tab_dev_status_26)
print(chisq.test(tab_dev_status_26, simulate.p.value = TRUE))
mosaicplot(tab_dev_status_26, color = col_blue_palette, main = "Dev Stage per Status (2026)", border = "white")

# Status vs Business Model
tab_bm_status_26 <- table(app_2026$business_model, app_2026$status)
print(tab_bm_status_26)
print(chisq.test(tab_bm_status_26, simulate.p.value = TRUE))
mosaicplot(tab_bm_status_26, color = col_blue_palette, main = "Business Model per Status (2026)", border = "white")

# Status vs Fund Stage
tab_fund_status_26 <- table(app_2026$fund_stage, app_2026$status)
print(tab_fund_status_26)
print(chisq.test(tab_fund_status_26, simulate.p.value = TRUE))
mosaicplot(tab_fund_status_26, color = col_blue_palette, main = "Fund Stage per Status (2026)", border = "white")

# Status vs Country
tab_country_status_26 <- table(temp_country_26, app_2026$status)
print(tab_country_status_26)
print(chisq.test(tab_country_status_26, simulate.p.value = TRUE))
mosaicplot(tab_country_status_26, color = col_blue_palette, main = "Country vs Status (2026)", border = "white", las=2)

# Status vs Tags
tab_tags_status_26 <- table(app_2026$tags, app_2026$status)
print(tab_tags_status_26)
print(chisq.test(tab_tags_status_26, simulate.p.value = TRUE))
mosaicplot(tab_tags_status_26, color = col_blue_palette, main = "Tags vs Status (2026)", border = "white", las=2)


# --- B) QUANTITATIVE vs STATUS (Triade Completa) ---

# Internal Avg Score
print(shapiro.test(na.omit(app_2026$internal_avg_score)))
print(tapply(app_2026$internal_avg_score, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$internal_avg_score, app_2026$status, sd, na.rm = TRUE))
print(t.test(internal_avg_score ~ status, data = app_2026))
boxplot(internal_avg_score ~ status, data = app_2026, main = "Internal Score per Status", ylab = "Punteggio", col = c(col_acc, col_rej), border = "black")

# Jury Avg Score
print(shapiro.test(na.omit(app_2026$jury_avg_score)))
print(tapply(app_2026$jury_avg_score, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$jury_avg_score, app_2026$status, sd, na.rm = TRUE))
print(t.test(jury_avg_score ~ status, data = app_2026))
boxplot(jury_avg_score ~ status, data = app_2026, main = "Jury Score per Status", ylab = "Punteggio", col = c(col_acc, col_rej), border = "black")

# Team Size
print(shapiro.test(na.omit(app_2026$team_size)))
print(tapply(app_2026$team_size, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$team_size, app_2026$status, sd, na.rm = TRUE))
print(t.test(team_size ~ status, data = app_2026))
boxplot(team_size ~ status, data = app_2026, main = "Team Size per Status", ylab = "Membri", col = c(col_acc, col_rej), border = "black")

# TRL Clean
print(shapiro.test(na.omit(app_2026$trl_clean)))
print(tapply(app_2026$trl_clean, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$trl_clean, app_2026$status, sd, na.rm = TRUE))
print(t.test(trl_clean ~ status, data = app_2026))
boxplot(trl_clean ~ status, data = app_2026, main = "TRL per Status", ylab = "TRL", col = c(col_acc, col_rej), border = "black")

# Runway
print(shapiro.test(na.omit(app_2026$runway)))
print(tapply(app_2026$runway, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$runway, app_2026$status, sd, na.rm = TRUE))
print(t.test(runway ~ status, data = app_2026))
boxplot(runway ~ status, data = app_2026, main = "Runway per Status", ylab = "Mesi", col = c(col_acc, col_rej), border = "black")

# Money Raised
print(shapiro.test(na.omit(app_2026$money_raised)))
print(tapply(app_2026$money_raised, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$money_raised, app_2026$status, sd, na.rm = TRUE))
print(t.test(money_raised ~ status, data = app_2026))
boxplot(money_raised ~ status, data = app_2026, main = "Money Raised per Status", ylab = "Euro", col = c(col_acc, col_rej), border = "black")

# Amount Raising
print(shapiro.test(na.omit(app_2026$amount_raising)))
print(tapply(app_2026$amount_raising, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$amount_raising, app_2026$status, sd, na.rm = TRUE))
print(t.test(amount_raising ~ status, data = app_2026))
boxplot(amount_raising ~ status, data = app_2026, main = "Amount Raising per Status", ylab = "Euro", col = c(col_acc, col_rej), border = "black")

# Founding Year
print(shapiro.test(na.omit(app_2026$founding_year)))
print(tapply(app_2026$founding_year, app_2026$status, mean, na.rm = TRUE))
print(tapply(app_2026$founding_year, app_2026$status, sd, na.rm = TRUE))
print(t.test(founding_year ~ status, data = app_2026))
boxplot(founding_year ~ status, data = app_2026, main = "Founding Year per Status", ylab = "Anno", col = c(col_acc, col_rej), border = "black")
