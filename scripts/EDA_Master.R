# ============================================================================== 
# DATASET MASTER: FUSIONE E ARMONIZZAZIONE (2024, 2025, 2026)
# ==============================================================================

app_2024$trl <- NA                  # Nel 2024 non era rilevata
app_2025$trl <- app_2025$trl_level  # Nome usato nel 2025
app_2026$trl <- app_2026$trl_clean  # Nome usato nel 2026

# 2. Selezione chirurgica delle colonne Master comuni ai 3 anni
master_cols <- c("app_id", "startup_name", "year", "country", "tags", 
                 "status", "is_registered", "founding_year", "dev_stage", 
                 "business_model", "fund_stage", "team_size", "money_raised", 
                 "runway", "amount_raising", "internal_avg_score", 
                 "jury_avg_score", "trl")

# 3. Unione per righe (Row Bind)
app_master <- rbind(app_2024[, master_cols], 
                    app_2025[, master_cols], 
                    app_2026[, master_cols])

# 4. Creazione delle variabili "Top 10" globali sull'intero dataset
# Country Master
freq_country_master <- sort(table(app_master$country), decreasing = TRUE)
top_countries_master <- names(freq_country_master)[1:10]

app_master$country_grouped <- ifelse(is.na(app_master$country), "Not Specified",
                                     ifelse(app_master$country %in% top_countries_master, 
                                            app_master$country, "Rest of the World"))

# Tags Master
freq_tags_master <- sort(table(app_master$tags), decreasing = TRUE)
top_tags_master <- names(freq_tags_master)[1:min(10, length(freq_tags_master))]

app_master$tags_grouped <- ifelse(is.na(app_master$tags), "Not Specified",
                                  ifelse(app_master$tags %in% top_tags_master, 
                                         app_master$tags, "Other Sectors"))

# 5. Pulizia Fattori (Droplevels e conversioni)
app_master$year <- as.factor(app_master$year) # L'anno diventa un factor per fare i gruppi!
app_master$country_grouped <- droplevels(factor(app_master$country_grouped))
app_master$tags_grouped <- droplevels(factor(app_master$tags_grouped))
app_master$dev_stage <- droplevels(factor(app_master$dev_stage))

# Controllo finale del dataset
print(dim(app_master))
print(summary(app_master))



# ANALISI LONGITUDINALE

graphics.off()
col_palette_blue <- c("#042333", "#00A4CC")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Year vs Status (Tasso di Accettazione)

tab_status_year_M <- table(app_master$year, app_master$status, useNA = "ifany")
prop_status_year_M <- prop.table(tab_status_year_M, margin = 1) * 100

print(tab_status_year_M)
print(round(prop_status_year_M, 2))

print(chisq.test(tab_status_year_M, simulate.p.value = TRUE))

barplot(t(prop_status_year_M), 
        main = "Evoluzione Status Ammissione", 
        xlab = "Cohort Year", 
        ylab = "Percentuale (%)", 
        col = col_palette_blue, 
        border = "white",
        ylim = c(0, 120),
        legend.text = c("Accepted", "Rejected"), 
        args.legend = list(x = "top", horiz = TRUE, bty = "n"))


# YEAR vs IS_REGISTERED

tab_reg_year_M <- table(app_master$year, app_master$is_registered, useNA = "ifany")
prop_reg_year_M <- prop.table(tab_reg_year_M, margin = 1) * 100

print(tab_reg_year_M)
print(round(prop_reg_year_M, 2))
print(chisq.test(tab_reg_year_M, simulate.p.value = TRUE))

col_palette_blue <- c("#042333", "#00A4CC")
barplot(t(prop_reg_year_M), 
        main = "Evoluzione Setup Legale (2024-2026)", 
        xlab = "Cohort Year", 
        ylab = "Percentuale (%)", 
        col = col_palette_blue, 
        border = "white",
        ylim = c(0, 120),
        legend.text = c("No", "Yes"), 
        args.legend = list(x = "top", horiz = TRUE, bty = "n"))
box(bty = "l")




graphics.off()
par(mar = c(5, 15, 4, 2) + 0.1) 
col_anni <- c("#B0E0E6", "#4682B4", "#042333") 


# Year vs Country Grouped

tab_country_year <- table(app_master$year, app_master$country_grouped, useNA = "ifany")

print(tab_country_year)
print(chisq.test(tab_country_year, simulate.p.value = TRUE))

tab_country_year_ord <- tab_country_year[, order(colSums(tab_country_year), decreasing = FALSE)]

barplot(tab_country_year_ord, 
        main = "Evoluzione Geografica (2024-2026)", 
        horiz = TRUE, las = 1, 
        col = col_anni, border = "white",
        xlab = "Numero di Candidature",
        legend.text = c("2024", "2025", "2026"),
        args.legend = list(x = "bottomright", bty = "n"))

# Year vs Tags Grouped

tab_tags_year <- table(app_master$year, app_master$tags_grouped, useNA = "ifany")

print(tab_tags_year)
print(chisq.test(tab_tags_year, simulate.p.value = TRUE))

tab_tags_year_ord <- tab_tags_year[, order(colSums(tab_tags_year), decreasing = FALSE)]

barplot(tab_tags_year_ord, 
        main = "Evoluzione dei Settori Tecnologici (2024-2026)", 
        horiz = TRUE, las = 1, 
        col = col_anni, border = "white",
        xlab = "Numero di Candidature",
        legend.text = c("2024", "2025", "2026"),
        args.legend = list(x = "bottomright", bty = "n"))

par(mar = c(5, 4, 4, 2) + 0.1)


graphics.off()
col_palette_blue_ext <- c("#042333", "#00A4CC", "#4682B4", "#B0E0E6", "#104E8B", "#50BFE6", "gray80")


# Year vs Development Stage

tab_year_dev_M <- table(app_master$year, app_master$dev_stage, useNA = "ifany")
prop_year_dev_M <- prop.table(tab_year_dev_M, margin = 1) * 100

print(tab_year_dev_M)
print(round(prop_year_dev_M, 2))
print(chisq.test(tab_year_dev_M, simulate.p.value = TRUE))

mosaicplot(tab_year_dev_M, 
           main = "Evoluzione Fase di Sviluppo (2024-2026)", 
           col = col_palette_blue_ext, 
           border = "white", las = 1,
           xlab = "Cohort Year", ylab = "Development Stage")


# 2. Year vs Funding Stage

tab_year_fund_M <- table(app_master$year, app_master$fund_stage, useNA = "ifany")
prop_year_fund_M <- prop.table(tab_year_fund_M, margin = 1) * 100

print(tab_year_fund_M)
print(round(prop_year_fund_M, 2))
print(chisq.test(tab_year_fund_M, simulate.p.value = TRUE))

mosaicplot(tab_year_fund_M, 
           main = "Evoluzione Stadio Finanziamento (2024-2026)", 
           col = col_palette_blue_ext, 
           border = "white", las = 1,
           xlab = "Cohort Year", ylab = "Funding Stage")


# 3. Year vs Business Model

tab_year_bm_M <- table(app_master$year, app_master$business_model, useNA = "ifany")
prop_year_bm_M <- prop.table(tab_year_bm_M, margin = 1) * 100

print(tab_year_bm_M)
print(round(prop_year_bm_M, 2))
print(chisq.test(tab_year_bm_M, simulate.p.value = TRUE))

mosaicplot(tab_year_bm_M, 
           main = "Evoluzione Modello di Business (2024-2026)", 
           col = col_palette_blue_ext, 
           border = "white", las = 1,
           xlab = "Cohort Year", ylab = "Business Model")


# ==========================================
# METRICHE QUANTITATIVE (ANOVA)
# ==========================================
graphics.off()
col_anni <- c("#B0E0E6", "#4682B4", "#042333") 
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)


# ANOVA: Year vs Team Size
print(tapply(app_master$team_size, app_master$year, mean, na.rm = TRUE))

anova_team_year_M <- aov(team_size ~ year, data = app_master)
print(summary(anova_team_year_M))

boxplot(team_size ~ year, data = app_master, 
        main = "Evoluzione Team Size (24-26)", 
        xlab = "Cohort Year", ylab = "Numero di Componenti",
        col = col_anni, border = "black")


# ANOVA: Year vs TRL
print(tapply(app_master$trl, app_master$year, mean, na.rm = TRUE))

anova_trl_year_M <- aov(trl ~ year, data = app_master)
print(summary(anova_trl_year_M))

boxplot(trl ~ year, data = app_master, 
        main = "Evoluzione TRL (24-26)", 
        xlab = "Cohort Year", ylab = "Livello TRL (1-9)",
        col = col_anni, border = "black")

par(mfrow = c(1, 1)) 


# ANOVA: Year vs Money Raised
print(tapply(app_master$money_raised, app_master$year, mean, na.rm = TRUE))

anova_raised_year_M <- aov(money_raised ~ year, data = app_master)
print(summary(anova_raised_year_M))

boxplot(money_raised ~ year, data = app_master, 
        main = "Evoluzione Fondi Raccolti (24-26)", 
        xlab = "Cohort Year", ylab = "Money Raised (Log Scale)",
        col = col_anni, border = "black", log = "y")


# ANOVA: Year vs Amount Raising
print(tapply(app_master$amount_raising, app_master$year, mean, na.rm = TRUE))

anova_raising_year_M <- aov(amount_raising ~ year, data = app_master)
print(summary(anova_raising_year_M))

# 1. Boxplot Money Raised

boxplot((amount_raising + 1) ~ year, data = app_master, 
        main = "Evoluzione Fondi Richiesti (24-26)", 
        xlab = "Cohort Year", ylab = "Amount Raising (Log Scale)",
        col = col_anni, border = "black", log = "y")

par(mfrow = c(1, 1))




# LIVELLO 2: LE DETERMINANTI DEL SUCCESSO

graphics.off()
col_status <- c("#00A4CC", "#042333") 
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# Dev Stage vs Status

tab_dev_status_M <- table(app_master$dev_stage, app_master$status, useNA = "ifany")
prop_dev_status_M  <- prop.table(tab_dev_status_M, margin = 1) * 100 

print(tab_dev_status_M )
print(round(prop_dev_status_M , 2))
print(chisq.test(tab_dev_status_M , simulate.p.value = TRUE))

mosaicplot(tab_dev_status_M , 
           main = "Status per Development Stage (173 Startup)", 
           col = col_status, border = "white", las = 1,
           xlab = "Development Stage", ylab = "Status")

# Fund Stage vs Status

tab_fund_status_M  <- table(app_master$fund_stage, app_master$status, useNA = "ifany")
prop_fund_status_M  <- prop.table(tab_fund_status_M , margin = 1) * 100

print(tab_fund_status_M )
print(round(prop_fund_status_M , 2))
print(chisq.test(tab_fund_status_M , simulate.p.value = TRUE))

par(mar = c(5, 9, 4, 2) + 0.1)
mosaicplot(tab_fund_status_M, 
           main = "Status per Fund Stage", 
           col = col_status, border = "white", 
           dir = c("v", "h"), 
           las = 1, 
           cex.axis = 0.8,
           xlab = "Status", ylab = "")

# Is Registered vs Status

tab_reg_status_M  <- table(app_master$is_registered, app_master$status, useNA = "ifany")
prop_reg_status_M  <- prop.table(tab_reg_status_M , margin = 1) * 100

print(tab_reg_status_M )
print(round(prop_reg_status_M , 2))
print(chisq.test(tab_reg_status_M , simulate.p.value = TRUE))

mosaicplot(tab_reg_status_M , 
           main = "Status per Registrazione Legale (173 Startup)", 
           col = col_status, border = "white", las = 1,
           xlab = "Is Registered?", ylab = "Status")




# 1. Country Grouped vs Status

tab_country_status_M <- table(app_master$country_grouped, app_master$status, useNA = "ifany")
prop_country_status_M <- prop.table(tab_country_status_M, margin = 1) * 100

print(tab_country_status_M)
print(round(prop_country_status_M, 2))
print(chisq.test(tab_country_status_M, simulate.p.value = TRUE))

par(mar = c(5, 12, 4, 2) + 0.1) # Margine sinistro a 12 per i Paesi
mosaicplot(tab_country_status_M, 
           main = "Status per Paese di Provenienza", 
           col = col_status, border = "white", 
           dir = c("v", "h"), 
           las = 1, 
           cex.axis = 0.8,
           xlab = "Status", ylab = "")


# 2. Tags Grouped vs Status

tab_tags_status_M <- table(app_master$tags_grouped, app_master$status, useNA = "ifany")
prop_tags_status_M <- prop.table(tab_tags_status_M, margin = 1) * 100

print(tab_tags_status_M)
print(round(prop_tags_status_M, 2))
print(chisq.test(tab_tags_status_M, simulate.p.value = TRUE))

par(mar = c(10, 5, 7, 2) + 0.1)
mosaicplot(tab_tags_status_M, 
           main = "Status per Settore Tecnologico (Tags)", 
           col = col_status, border = "white", las = 2, cex.axis = 0.7,
           xlab = "Tags", ylab = "Status")





graphics.off()
col_status <- c("#00A4CC", "#042333") 
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)


# Team Size vs Status

print(shapiro.test(app_master$team_size))
print(tapply(app_master$team_size, app_master$status, mean, na.rm = TRUE))
print(tapply(app_master$team_size, app_master$status, sd, na.rm = TRUE))
print(t.test(team_size ~ status, data = app_master))

boxplot(team_size ~ status, data = app_master, 
        main = "Team Size per Status", 
        ylab = "Numero di Componenti", col = col_status)

# TRL vs Status

print(shapiro.test(app_master$trl))
print(tapply(app_master$trl, app_master$status, mean, na.rm = TRUE))
print(tapply(app_master$trl, app_master$status, sd, na.rm = TRUE))
print(t.test(trl ~ status, data = app_master))

boxplot(trl ~ status, data = app_master, 
        main = "TRL per Status", 
        ylab = "Livello TRL (1-9)", col = col_status)

# Founding Year vs Status

print(shapiro.test(app_master$founding_year))
print(tapply(app_master$founding_year, app_master$status, mean, na.rm = TRUE))
print(tapply(app_master$founding_year, app_master$status, sd, na.rm = TRUE))
print(t.test(founding_year ~ status, data = app_master))

boxplot(founding_year ~ status, data = app_master, 
        main = "Anno Fondazione per Status", 
        ylab = "Anno", col = col_status)

par(mfrow = c(1, 1))




graphics.off()
col_status <- c("#00A4CC", "#042333") 
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2) + 0.1)


# 1. Money Raised vs Status

print(shapiro.test(app_master$money_raised))
print(tapply(app_master$money_raised, app_master$status, mean, na.rm = TRUE))
print(tapply(app_master$money_raised, app_master$status, sd, na.rm = TRUE))
print(t.test(money_raised ~ status, data = app_master))

boxplot(money_raised ~ status, data = app_master, 
        main = "Fondi Raccolti per Status", 
        ylab = "Money Raised (€, Log Scale)", 
        col = col_status, border = "black", log = "y")


# 2. Amount Raising vs Status

print(shapiro.test(app_master$amount_raising))
print(tapply(app_master$amount_raising, app_master$status, mean, na.rm = TRUE))
print(tapply(app_master$amount_raising, app_master$status, sd, na.rm = TRUE))
print(t.test(amount_raising ~ status, data = app_master))

boxplot((amount_raising + 1) ~ status, data = app_master, 
        main = "Fondi Richiesti per Status", 
        ylab = "Amount Raising (€, Log Scale)", 
        col = col_status, border = "black", log = "y")

par(mfrow = c(1, 1))
