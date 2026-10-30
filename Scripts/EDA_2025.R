# ============================================================================== 
# 2025
# ============================================================================== 
dim(app_2025)
str(app_2025)
summary(app_2025)


# ============================================================================== 
# ANALISI UNIVARIATA
# ============================================================================== 

# VARIABILI CATEGORICHE
# status, dev_stage, business_model, fund_stage, country, tags, is_registered, is_winner

tab_status_25 <- table(app_2025$status, useNA = "ifany")
prop_status_25 <- prop.table(tab_status_25) * 100
print(tab_status_25)
print(round(prop_status_25, 2))

colori_status_25 <- c("#4682B4", "#104E8B")
barplot(tab_status_25, 
        main = "Esito della Selezione: Accepted vs Rejected (2025)", 
        ylab = "Numero di Startup", 
        xlab = "Status della Candidatura",
        col = colori_status_25, 
        border = NA, 
        ylim = c(0, max(tab_status_25) + 5), 
        cex.names = 1.1)
box(bty = "l")


tab_isreg_25 <- table(app_2025$is_registered, useNA = "ifany")
prop_isreg_25 <- prop.table(tab_isreg_25) * 100
print(tab_isreg_25)
print(round(prop_isreg_25, 2))

barplot(tab_isreg_25, 
        main = "Startup Registrate o Incorporate? (2025)", 
        ylab = "Numero di Startup", 
        col = c("#B0E0E6", "#104E8B"), 
        border = NA,
        ylim = c(0, max(tab_isreg_25, na.rm = TRUE) + 3))
box(bty = "l")


tab_dev_25 <- table(app_2025$dev_stage, useNA = "ifany")
prop_dev_25 <- prop.table(tab_dev_25) * 100
print(tab_dev_25)
print(round(prop_dev_25, 2))

barplot(tab_dev_25, 
        main = "Stadio di Sviluppo delle Startup (2025)", 
        ylab = "Numero di Startup", 
        col = "#4682B4", 
        border = NA, 
        las = 2,           # Ruota verticalmente le etichette
        cex.names = 1.0, 
        ylim = c(0, max(tab_dev_25, na.rm = TRUE) + 3))
box(bty = "l")


tab_bm_25 <- table(app_2025$business_model, useNA = "ifany")
prop_bm_25 <- prop.table(tab_bm_25) * 100
print(tab_bm_25)
print(round(prop_bm_25, 2))

barplot(tab_bm_25, 
        main = "Modello di Business delle Startup (2025)", 
        ylab = "Numero di Startup", 
        col = c("#104E8B", "#4682B4", "#B0E0E6"), 
        border = NA,
        las = 2,
        ylim = c(0, max(tab_bm_25, na.rm = TRUE) + 3))
box(bty = "l")


tab_fund_25 <- table(app_2025$fund_stage, useNA = "ifany")
prop_fund_25 <- prop.table(tab_fund_25) * 100
print("--- Frequenze: fund_stage ---")
print(tab_fund_25)
print(round(prop_fund_25, 2))

barplot(tab_fund_25, 
        main = "Stadio di Finanziamento / Fund Stage (2025)", 
        ylab = "Numero di Startup", 
        col = "#00A4CC", 
        border = NA,
        las = 2,
        cex.names = 0.9,
        ylim = c(0, max(tab_fund_25, na.rm = TRUE) + 3))
box(bty = "l")


# Country (Armonizzato: Grafico a barre orizzontali)
tab_country_25 <- table(app_2025$country, useNA = "ifany")
prop_country_25 <- prop.table(tab_country_25) * 100
print(tab_country_25)
tab_country_sorted_25 <- sort(tab_country_25, decreasing = TRUE)

par(mar = c(5, 10, 4, 2)) 
barplot(tab_country_sorted_25, 
        main = "Distribuzione per Paese (2025)", 
        xlab = "Numero di Startup", 
        col = "#4682B4", 
        border = NA,
        horiz = TRUE, las = 1,
        cex.names = 0.8)
box(bty = "l")
par(mar = c(5, 4, 4, 2))


# Tags (Armonizzato: Grafico a barre orizzontali)
tab_tags_25 <- table(app_2025$tags, useNA = "ifany")
tab_tags_sorted_25 <- sort(tab_tags_25, decreasing = TRUE)
prop_tags_25 <- prop.table(tab_tags_sorted_25) * 100
print(tab_tags_sorted_25)

par(mar = c(5, 14, 4, 2)) 
barplot(tab_tags_sorted_25, 
        main = "Distribuzione per Settore / Tags (2025)", 
        xlab = "Numero di Startup", 
        col = "#042333", 
        border = NA,
        horiz = TRUE, las = 1, 
        cex.names = 0.8)
box(bty = "l")
par(mar = c(5, 4, 4, 2))



# VARIABILI QUANTITATIVE (Layout Simmetrico 1x2 Istogramma + Boxplot)
# team_size, monay_raised, founding_year, runway, amount_raising, internal_avg_score, jury_avg_score, trl_level

# Team Size
summary_team_25 <- summary(app_2025$team_size)
sd_team_25 <- sd(app_2025$team_size, na.rm = TRUE)
print(summary_team_25)
print(round(sd_team_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$team_size, 
     main = "Distribuzione Team Size (2025)", 
     xlab = "Numero di Componenti", ylab = "Frequenza", 
     col = "#4682B4", border = "white")
boxplot(app_2025$team_size, 
        main = "Boxplot Team Size (2025)", 
        ylab = "Componenti", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1)) 


# Money Raised
summary_money_25 <- summary(app_2025$money_raised)
sd_money_25 <- sd(app_2025$money_raised, na.rm = TRUE)
print(summary_money_25)
print(round(sd_money_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$money_raised, 
     main = "Distribuzione Money Raised (2025)", 
     xlab = "Euro", ylab = "Frequenza", 
     col = "#104E8B", border = "white")
boxplot(app_2025$money_raised, 
        main = "Boxplot Money Raised (2025)", 
        ylab = "Euro", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1)) 


# Runway
summary_runway_25 <- summary(app_2025$runway)
sd_runway_25 <- sd(app_2025$runway, na.rm = TRUE)
print(summary_runway_25)
print(round(sd_runway_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$runway, 
     main = "Distribuzione Runway (2025)", 
     xlab = "Mesi", ylab = "Frequenza", 
     col = "#104E8B", border = "white")
boxplot(app_2025$runway, 
        main = "Boxplot Runway (2025)", 
        ylab = "Mesi di sopravvivenza", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1)) 


# Amount Raising
summary_amount_25 <- summary(app_2025$amount_raising)
sd_amount_25 <- sd(app_2025$amount_raising, na.rm = TRUE)
print(summary_amount_25)
print(round(sd_amount_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$amount_raising, 
     main = "Capitale Richiesto (2025)", 
     xlab = "Euro", ylab = "Frequenza", 
     col = "#00A4CC", border = "white")
boxplot(app_2025$amount_raising, 
        main = "Boxplot Amount Raising (2025)", 
        ylab = "Euro", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1))


# PULIZIA E ESTRAZIONE DELLA VARIABILE TRL
app_2025$trl_level <- as.numeric(sub(".*?([0-9]+).*", "\\1", app_2025$Q17_trl))
tab_trl <- table(app_2025$trl_level, useNA = "ifany")
print(tab_trl)

# TRL Level (Layout Simmetrico)
summary_trl_25 <- summary(app_2025$trl_level)
sd_trl_25 <- sd(app_2025$trl_level, na.rm = TRUE)
print(summary_trl_25)
print(round(sd_trl_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$trl_level, 
     main = "Livello TRL (2025)", 
     xlab = "TRL (1-9)", ylab = "Frequenza", 
     col = "#4682B4", border = "white")
boxplot(app_2025$trl_level, 
        main = "Boxplot TRL Level (2025)", 
        ylab = "TRL (1-9)", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1))


# Founding Year (Layout Simmetrico)
summary_founding_25 <- summary(app_2025$founding_year)
sd_founding_25 <- sd(app_2025$founding_year, na.rm = TRUE)
print(summary_founding_25)
print(round(sd_founding_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$founding_year, 
     main = "Anno di Fondazione (2025)", 
     xlab = "Anno", ylab = "Frequenza", 
     col = "#104E8B", border = "white")
boxplot(app_2025$founding_year, 
        main = "Boxplot Founding Year (2025)", 
        ylab = "Anno", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1))


# Internal Avg Score (Layout Simmetrico)
summary_internal_25 <- summary(app_2025$internal_avg_score)
sd_internal_25 <- sd(app_2025$internal_avg_score, na.rm = TRUE)
print(summary_internal_25)
print(round(sd_internal_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$internal_avg_score, 
     main = "Distribuzione Internal Score", 
     xlab = "Punteggio", ylab = "Frequenza", 
     col = "#B0E0E6", border = "white")
boxplot(app_2025$internal_avg_score, 
        main = "Internal Avg Score (2025)", 
        ylab = "Punteggio", 
        col = "#B0E0E6", border = "#042333", pch=19)
par(mfrow = c(1, 1))


# Jury Avg Score (Layout Simmetrico)
summary_jury_25 <- summary(app_2025$jury_avg_score)
sd_jury_25 <- sd(app_2025$jury_avg_score, na.rm = TRUE)
print(summary_jury_25)
print(round(sd_jury_25, 2))

par(mfrow = c(1, 2))
hist(app_2025$jury_avg_score, 
     main = "Distribuzione Jury Score", 
     xlab = "Punteggio", ylab = "Frequenza", 
     col = "#00A4CC", border = "white")
boxplot(app_2025$jury_avg_score, 
        main = "Jury Avg Score (2025)", 
        ylab = "Punteggio", 
        col = "#00A4CC", border = "#042333", pch=19)
par(mfrow = c(1, 1))



# ============================================================================== 
# ANALISI DI CORRELAZIONE
# ============================================================================== 

# ANALISI FRA VARIABILI QUANTITATIVE

num_vars_2025 <- app_2025[, c("founding_year", "money_raised", "runway", "amount_raising", "team_size", "internal_avg_score", "jury_avg_score", "trl_level")]
cor_matrix_2025 <- cor(num_vars_2025, use = "pairwise.complete.obs")
print(round(cor_matrix_2025, 2))

library(corrplot)

par(mar = c(1, 1, 4, 1))
col_blue_economy <- colorRampPalette(c("#042333", "#ffffff", "#00A4CC"))(200)

# Allineato alla grafica del 2024
corrplot(cor_matrix_2025, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         diag = FALSE,
         col = col_blue_economy,
         title = "Matrice di Correlazione (2025)",
         mar = c(0, 0, 2, 0)) 
par(mar = c(5, 4, 4, 2) + 0.1)


# ==============================================================================
# ANALISI BIVARIATA: INCROCI TRA VARIABILI QUALITATIVE ESPLICATIVE
# ==============================================================================

# 1. dev_stage vs business_model
tab_dev_bm_2025 <- table(app_2025$dev_stage, app_2025$business_model)
print(tab_dev_bm_2025)
test_chi_dev_bm_2025 <- chisq.test(tab_dev_bm_2025, simulate.p.value = TRUE)
print(test_chi_dev_bm_2025)
mosaicplot(tab_dev_bm_2025, 
           main = "Dev Stage vs Business Model (2025)",
           col = c("#B0E0E6", "#4682B4", "#104E8B"),
           border = "white")

# 2. Country vs Dev Stage
temp_country <- ifelse(is.na(app_2025$country), "Not Specified", app_2025$country)
tab_country_dev <- table(temp_country, app_2025$dev_stage)
print(tab_country_dev)
test_chi_country_dev <- chisq.test(tab_country_dev, simulate.p.value = TRUE)
print(test_chi_country_dev)
mosaicplot(tab_country_dev,
           main = "Country vs Development Stage (2025)",
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"),
           border = "white", xlab = "Country", ylab = "Development Stage", las = 2) 

# 3. Business Model vs Dev Stage
tab_bm_dev <- table(app_2025$business_model, app_2025$dev_stage)
print(tab_bm_dev)
test_chi_bm_dev <- chisq.test(tab_bm_dev, simulate.p.value = TRUE)
print(test_chi_bm_dev)
mosaicplot(tab_bm_dev,
           main = "Business Model vs Development Stage",
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"),
           border = "white", xlab = "Business Model", ylab = "Development Stage")

# 4. Tags vs Dev Stage
tab_tags_dev <- table(app_2025$tags, app_2025$dev_stage)
print(tab_tags_dev)
test_chi_tags_dev <- chisq.test(tab_tags_dev, simulate.p.value = TRUE)
print(test_chi_tags_dev)
par(mar=c(12,4,4,2))
mosaicplot(tab_tags_dev,
           main = "Tags vs Development Stage",
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"),
           border = "white", xlab = "Sector (Tags)", ylab = "Development Stage", las = 2) 
par(mar=c(5,4,4,2))

# 5. Country vs Business Model
temp_country_2 <- ifelse(is.na(app_2025$country), "Not Specified", app_2025$country)
tab_country_bm <- table(temp_country_2, app_2025$business_model)
print(tab_country_bm)
test_chi_country_bm <- chisq.test(tab_country_bm, simulate.p.value = TRUE)
print(test_chi_country_bm)
mosaicplot(tab_country_bm,
           main = "Country vs Business Model",
           col = c("#042333", "#00A4CC", "#B0E0E6"), 
           border = "white", xlab = "Country", ylab = "Business Model", las = 2)

# 6. Tags vs Business Model
tab_tags_bm <- table(app_2025$tags, app_2025$business_model)
print(tab_tags_bm)
test_chi_tags_bm <- chisq.test(tab_tags_bm, simulate.p.value = TRUE)
print(test_chi_tags_bm)
par(mar=c(12,4,4,2))
mosaicplot(tab_tags_bm,
           main = "Sector (Tags) vs Business Model",
           col = c("#042333", "#00A4CC", "#B0E0E6"),
           border = "white", xlab = "Sector (Tags)", ylab = "Business Model", las = 2)
par(mar=c(5,4,4,2))

# 7. Fund Stage vs Dev Stage
tab_fund_dev <- table(app_2025$fund_stage, app_2025$dev_stage)
print(tab_fund_dev)
test_chi_fund_dev <- chisq.test(tab_fund_dev, simulate.p.value = TRUE)
print(test_chi_fund_dev)
mosaicplot(tab_fund_dev,
           main = "Funding Stage vs Dev Stage",
           col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"),
           border = "white", xlab = "Funding Stage", ylab = "Development Stage", las = 2)

# 8. Fund Stage vs Country
tab_fund_country <- table(app_2025$fund_stage, app_2025$country)
print(tab_fund_country)
test_chi_fund_country <- chisq.test(tab_fund_country, simulate.p.value = TRUE)
print(test_chi_fund_country)
mosaicplot(tab_fund_country,
           main = "Fund Stage vs Country",
           col = c("#042333", "#00A4CC", "#B0E0E6"),
           border = "white", xlab = "Fund Stage", ylab = "Country", las = 2)


# ==============================================================================
# ANALISI BIVARIATA: VARIABILI ESPLICATIVE vs TARGET (STATUS)
# ==============================================================================

# --- A) QUALITATIVE vs STATUS ---

# 1. Status vs Dev stage
tab_status_dev_abs_25 <- table(Status = app_2025$status, DevStage = app_2025$dev_stage)
print(tab_status_dev_abs_25)
chi_status_dev_25 <- chisq.test(tab_status_dev_abs_25, simulate.p.value = TRUE)
print(chi_status_dev_25)

tab_status_dev_prop_row_25 <- prop.table(tab_status_dev_abs_25, margin = 1) * 100
print(round(tab_status_dev_prop_row_25, 1))

my_colors_stat_dev <- c("#00A4CC", "#B0E0E6", "#4682B4", "#104E8B")
barplot(tab_status_dev_prop_row_25,
        main = "Distribuzione Stadio di Sviluppo per Status (2025)",
        xlab = "Development Stage", ylab = "Percentuale (%)",
        col = my_colors_stat_dev[1:ncol(tab_status_dev_prop_row_25)],
        border = "black", legend = TRUE, 
        args.legend = list(x = "topright", bty = "n")) 

# 2. Status vs Business model
tab_status_bm_abs_25 <- table(Status = app_2025$status, BusinessModel = app_2025$business_model)
print(tab_status_bm_abs_25)
chi_status_bm_25 <- chisq.test(tab_status_bm_abs_25, simulate.p.value = TRUE)
print(chi_status_bm_25)

tab_status_bm_prop_row_25 <- prop.table(tab_status_bm_abs_25, margin = 1) * 100
print(round(tab_status_bm_prop_row_25, 1))

my_colors_stat_bm <- c("#00A4CC", "#B0E0E6", "#104E8B")
barplot(tab_status_bm_prop_row_25,
        main = "Business Model per Status (Coorte 2025)",
        xlab = "Business Model", ylab = "Percentuale (%)",
        col = my_colors_stat_bm[1:ncol(tab_status_bm_prop_row_25)],
        border = "black", legend = TRUE,
        args.legend = list(x = "topright", bty = "n"))

# 3. Status vs Tags
tab_status_tags_abs_25 <- table(Status = app_2025$status, Sector = app_2025$tags)
print(tab_status_tags_abs_25)
chi_status_tags_25 <- chisq.test(tab_status_tags_abs_25, simulate.p.value = TRUE)
print(chi_status_tags_25)

tab_status_tags_prop_row_25 <- prop.table(tab_status_tags_abs_25, margin = 1) * 100
print(round(tab_status_tags_prop_row_25, 1))

opar <- par(no.readonly = TRUE)
par(mar=c(15, 5, 2, 2))
barplot(tab_status_tags_prop_row_25,
        main = "Settore di Attività (Tags) per Status (2025)",
        xlab = "", ylab = "Percentuale (%)",
        col = c("#00A4CC", "#B0E0E6"), 
        border = "black", legend = TRUE,
        args.legend = list(x = "topright", bty = "n", inset = c(0, -0.1)), 
        las = 2, cex.names = 0.7)   
par(opar)

# 4. Status vs Fund stage
tab_status_fund_abs_25 <- table(Status = app_2025$status, FundStage = app_2025$fund_stage)
print(tab_status_fund_abs_25)
chi_status_fund_25 <- chisq.test(tab_status_fund_abs_25, simulate.p.value = TRUE)
print(chi_status_fund_25)

tab_status_fund_prop_row_25 <- prop.table(tab_status_fund_abs_25, margin = 1) * 100
print(round(tab_status_fund_prop_row_25, 1))

my_colors_stat_fund <- c("#00A4CC", "#B0E0E6", "#4682B4", "#104E8B", "#042333")
barplot(tab_status_fund_prop_row_25,
        main = "Stadio di Finanziamento per Status (Coorte 2025)",
        xlab = "Funding Stage", ylab = "Percentuale (%)",
        col = my_colors_stat_fund[1:ncol(tab_status_fund_prop_row_25)],
        border = "black", legend = TRUE,
        args.legend = list(x = "topright", bty = "n"))

# 5. Status vs Country
temp_country_clean <- ifelse(is.na(app_2025$country), "Not Specified", app_2025$country)
tab_status_country_abs <- table(Status = app_2025$status, Country = temp_country_clean)
print(tab_status_country_abs)
chi_status_country <- chisq.test(tab_status_country_abs, simulate.p.value = TRUE)
print(chi_status_country)

par(mar=c(10, 4, 4, 2)) 
barplot(prop.table(tab_status_country_abs, margin = 1) * 100,
        main = "Paese di Provenienza per Status (2025)",
        xlab = "", ylab = "Percentuale (%)",
        col = c("#00A4CC", "#B0E0E6"), 
        border = "black", legend = TRUE,
        args.legend = list(x = "topright", bty = "n"),
        las = 2) 
par(mar=c(5, 4, 4, 2))


# --- B) QUANTITATIVE vs STATUS (Triade Statistica Completa) ---

my_colors_basic <- c("#00A4CC", "#B0E0E6")

# 1. Status vs Internal avg score
print(shapiro.test(na.omit(app_2025$internal_avg_score)))
media_score_status_25 <- tapply(app_2025$internal_avg_score, app_2025$status, mean, na.rm = TRUE)
print(media_score_status_25)
sd_score_status_25 <- tapply(app_2025$internal_avg_score, app_2025$status, sd, na.rm = TRUE)
print(sd_score_status_25)
test_t_score_status_25 <- t.test(internal_avg_score ~ status, data = app_2025)
print(test_t_score_status_25)

boxplot(internal_avg_score ~ status, data = app_2025,
        main = "Punteggio Medio Interno per Status (2025)",
        xlab = "Status", ylab = "Internal Average Score (1-5)",
        col = my_colors_basic, border = "black", pch=19) 

# 2. Status vs Team size
print(shapiro.test(na.omit(app_2025$team_size)))
media_team_score_25 <- tapply(app_2025$team_size, app_2025$status, mean, na.rm = TRUE)
print(media_team_score_25)
sd_team_score_25 <- tapply(app_2025$team_size, app_2025$status, sd, na.rm = TRUE)
print(sd_team_score_25)
test_t_team_score_25 <- t.test(team_size ~ status, data = app_2025)
print(test_t_team_score_25)

boxplot(team_size ~ status, data = app_2025,
        main = "Dimensione del Team per Status (2025)",
        xlab = "Status", ylab = "Team Size (Numero di persone)",
        col = my_colors_basic, border = "black", pch=19)

# 3. Status vs Trl level
print(shapiro.test(na.omit(app_2025$trl_level)))
media_status_trl_25 <- tapply(app_2025$trl_level, app_2025$status, mean, na.rm = TRUE)
print(media_status_trl_25)
sd_status_trl_25 <- tapply(app_2025$trl_level, app_2025$status, sd, na.rm = TRUE)
print(sd_status_trl_25)
test_t_status_trl_25 <- t.test(trl_level ~ status, data = app_2025)
print(test_t_status_trl_25)

boxplot(trl_level ~ status, data = app_2025,
        main = "Livello TRL per Status (2025)",
        xlab = "Status", ylab = "TRL Level (1-9)",
        col = my_colors_basic, border = "black", pch=19)

# 4. Status vs Runway
print(shapiro.test(na.omit(app_2025$runway)))
media_status_runway_25 <- tapply(app_2025$runway, app_2025$status, mean, na.rm = TRUE)
print(media_status_runway_25)
sd_status_runway_25 <- tapply(app_2025$runway, app_2025$status, sd, na.rm = TRUE)
print(sd_status_runway_25)
test_t_status_runway_25 <- t.test(runway ~ status, data = app_2025)
print(test_t_status_runway_25)

boxplot(runway ~ status, data = app_2025,
        main = "Runway per Status (2025)",
        xlab = "Status", ylab = "Runway (Mesi di autonomia)",
        col = my_colors_basic, border = "black", pch=19)

# 5. Status vs Money raised
print(shapiro.test(na.omit(app_2025$money_raised)))
media_status_money_25 <- tapply(app_2025$money_raised, app_2025$status, mean, na.rm = TRUE)
print(media_status_money_25)
sd_status_money_25 <- tapply(app_2025$money_raised, app_2025$status, sd, na.rm = TRUE)
print(sd_status_money_25)
test_t_status_money_25 <- t.test(money_raised ~ status, data = app_2025)
print(test_t_status_money_25)

boxplot(money_raised ~ status, data = app_2025,
        main = "Money Raised per Status (2025)",
        xlab = "Status", ylab = "Money Raised (Euro/USD)",
        col = my_colors_basic, border = "black", pch=19)

# 6. Status vs Amount raising
print(shapiro.test(na.omit(app_2025$amount_raising)))
media_status_amount_25 <- tapply(app_2025$amount_raising, app_2025$status, mean, na.rm = TRUE)
print(media_status_amount_25)
sd_status_amount_25 <- tapply(app_2025$amount_raising, app_2025$status, sd, na.rm = TRUE)
print(sd_status_amount_25)
test_t_status_amount_25 <- t.test(amount_raising ~ status, data = app_2025)
print(test_t_status_amount_25)

boxplot(amount_raising ~ status, data = app_2025,
        main = "Amount Raising per Status (2025)",
        xlab = "Status", ylab = "Amount Raising (Euro/USD)",
        col = my_colors_basic, border = "black", pch=19)

# 7. Status vs Founding year
print(shapiro.test(na.omit(app_2025$founding_year)))
media_status_year_25 <- tapply(app_2025$founding_year, app_2025$status, mean, na.rm = TRUE)
print(media_status_year_25)
sd_status_year_25 <- tapply(app_2025$founding_year, app_2025$status, sd, na.rm = TRUE)
print(sd_status_year_25)
test_t_status_year_25 <- t.test(founding_year ~ status, data = app_2025)
print(test_t_status_year_25)

boxplot(founding_year ~ status, data = app_2025,
        main = "Founding Year per Status (2025)",
        xlab = "Status", ylab = "Anno di Fondazione",
        col = my_colors_basic, border = "black", pch=19)