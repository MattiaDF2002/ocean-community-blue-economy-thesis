# ============================================================================== 
# TESI MAGISTRALE: ANALISI ESPLORATIVA ED INFERENZIALE COORTE 2024
# ============================================================================== 
dim(app_2024)
names(app_2024)
str(app_2024)

# ============================================================================== 
# 1. ANALISI UNIVARIATA: VARIABILI CATEGORICHE
# ============================================================================== 

# Status
tab_status <- table(app_2024$status, useNA = "ifany")
prop_status <- prop.table(tab_status) * 100
print(tab_status)
print(round(prop_status, 2))
colori_status <- c("#4682B4", "#104E8B") 
barplot(tab_status, 
        main = "Esito della Selezione: Accepted vs Rejected (2024)", 
        ylab = "Numero di Startup", 
        xlab = "Status della Candidatura",
        col = colori_status, 
        border = NA,                               
        ylim = c(0, max(tab_status) + 5),          
        cex.names = 1.1)

# Dev Stage
tab_dev <- table(app_2024$dev_stage, useNA = "ifany")
prop_dev <- prop.table(tab_dev) * 100
print(tab_dev)
print(round(prop_dev, 2))
colori_dev <- c("#B0E0E6", "#4682B4", "#104E8B", "#000080") 
barplot(tab_dev, 
        main = "Distribuzione per Fase di Sviluppo (Coorte 2024)", 
        ylab = "Numero di Startup",
        xlab = "Fase di Sviluppo",
        col = colori_dev[1:length(tab_dev)], 
        border = NA, 
        ylim = c(0, max(tab_dev) + 5), 
        cex.names = 0.85)

# Business Model
tab_bm <- table(app_2024$business_model, useNA = "ifany")
prop_bm <- prop.table(tab_bm) * 100
print(tab_bm)
print(round(prop_bm, 2))
colori_bm <- c("#B0E0E6", "#4682B4", "#104E8B", "#000080") 
barplot(tab_bm, 
        main="Distribuzione Business Model",
        ylab = "Numero di Startup",
        xlab = "Modello di Business",
        col=colori_bm[1:length(tab_bm)], 
        border=NA,
        ylim = c(0, max(tab_bm) + 5), 
        cex.names = 0.85)

# Fund Stage
tab_fund <- table(app_2024$fund_stage, useNA = "ifany")
prop_fund <- prop.table(tab_fund) * 100
print(tab_fund)
print(round(prop_fund, 2))
colori_fund <- c("#B0E0E6", "#4682B4", "#104E8B", "#000080","#7FFFD4") 
barplot(tab_fund, 
        main="Distribuzione Fund Stage",
        ylab = "Numero di Startup",
        xlab = "Stage di Finanziamento",
        col=colori_fund[1:length(tab_fund)], 
        border=NA,
        ylim = c(0, max(tab_fund) + 5), 
        cex.names = 0.85)

# Country (Alta cardinalità -> Barplot orizzontale per evitare sovrapposizioni)
tab_country <- table(app_2024$country, useNA = "ifany")
prop_country <- prop.table(tab_country) * 100
print(tab_country)
print(round(prop_country, 2))
tab_country_sorted <- sort(tab_country, decreasing = TRUE)
par(mar = c(5, 8, 4, 2)) 
barplot(tab_country_sorted, 
        main="Distribuzione per Paese", 
        horiz=TRUE, las=1, 
        col="#4682B4", border=NA, cex.names=0.6)
par(mar = c(5, 4, 4, 2)) 

# Tags (Alta cardinalità -> Barplot orizzontale)
tab_tags <- table(app_2024$tags, useNA = "ifany")
prop_tags <- prop.table(tab_tags) * 100
print(tab_tags)
print(round(prop_tags, 2))
tab_tags_sorted <- sort(tab_tags, decreasing = TRUE)
par(mar = c(5, 12, 4, 2)) 
barplot(tab_tags_sorted, 
        main="Distribuzione per Tag", 
        horiz=TRUE, las=1, 
        col="#104E8B", border=NA, cex.names=0.6)
par(mar = c(5, 4, 4, 2)) 

# Is Registered
tab_isreg <- table(app_2024$is_registered, useNA = "ifany")
prop_isreg <- prop.table(tab_isreg) * 100
print(tab_isreg)
print(round(prop_isreg, 2))
barplot(tab_isreg, 
        main="Registrata?", 
        col=c("#B0E0E6", "#104E8B"), 
        border=NA)

# Is Winner
tab_iswin <- table(app_2024$is_winner, useNA = "ifany")
prop_iswin <- prop.table(tab_iswin) * 100
print(tab_iswin)
print(round(prop_iswin, 2))
barplot(tab_iswin, 
        main="Vincitore?", 
        col=c("#B0E0E6", "#104E8B"), 
        border=NA)


# ============================================================================== 
# 2. ANALISI UNIVARIATA: VARIABILI QUANTITATIVE (Layout Simmetrico 1x2)
# ============================================================================== 

# Team Size
summary_team <- summary(app_2024$team_size)
sd_team <- sd(app_2024$team_size, na.rm = TRUE)
print(summary_team)
print(round(sd_team, 2))
par(mfrow = c(1, 2))
hist(app_2024$team_size, main = "Istogramma: Team Size", xlab = "Membri", ylab = "Frequenza", col = "#B0E0E6", border = "white")
boxplot(app_2024$team_size, 
        main = "Distribuzione Dimensione Team", 
        ylab = "Numero di Componenti",
        col = "lightblue", border = "#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))

# Money Raised
summary_money <- summary(app_2024$money_raised)
sd_money <- sd(app_2024$money_raised, na.rm = TRUE)
print(summary_money)
print(round(sd_money, 2))
par(mfrow = c(1, 2))
hist(app_2024$money_raised, main = "Istogramma: Money Raised", xlab = "Fondi (€)", ylab = "Frequenza", col = "#4682B4", border = "white")
boxplot(app_2024$money_raised, 
        main = "Distribuzione Fondi Raccolti", 
        ylab = "Fondi Raccolti (in €)",
        col = "#4682B4", border = "#000080", pch = 18, outcol = "#104E8B", yaxt = "n")   
par(mfrow = c(1, 1))

# Runway
summary_runway <- summary(app_2024$runway)
sd_runway<- sd(app_2024$runway, na.rm = TRUE)
print(summary_runway)
print(round(sd_runway, 2))
par(mfrow = c(1, 2))
hist(app_2024$runway, main = "Istogramma: Runway", xlab = "Mesi", ylab = "Frequenza", col = "#104E8B", border = "white")
boxplot(app_2024$runway, 
        main = "Distribuzione di Runway", 
        ylab = "Periodo di Runway",
        col = "lightblue", border = "#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))

# Founding Year
summary_founding <- summary(app_2024$founding_year)
sd_founding<- sd(app_2024$founding_year, na.rm = TRUE)
print(summary_founding)
print(round(sd_founding, 2))
par(mfrow = c(1, 2))
hist(app_2024$founding_year, main = "Istogramma: Founding Year", xlab = "Anno di Fondazione", ylab = "Frequenza", col = "#4682B4", border = "white")
boxplot(app_2024$founding_year, 
        main = "Anno di Fondazione", 
        ylab = "Anno di Fondazione",
        col = "lightblue", border = "#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))

# Amount Raising
summary_amraising <- summary(app_2024$amount_raising)
sd_amraising<- sd(app_2024$amount_raising, na.rm = TRUE)
print(summary_amraising)
print(round(sd_amraising, 2))
par(mfrow = c(1, 2))
hist(app_2024$amount_raising, main = "Istogramma: Amount Raising", xlab = "Target (€)", ylab = "Frequenza", col = "#000080", border = "white")
boxplot(app_2024$amount_raising, 
        main="Amount Raising (€)", 
        ylab = "Ammontare Raccolto",
        col="lightblue", border="#104E8B", pch = 19, outcol = "#4682B4")
par(mfrow = c(1, 1))


# ============================================================================== 
# 3. ANALISI DI CORRELAZIONE TRA QUANTITATIVE E SCATTERPLOTS
# ==============================================================================

library(corrplot)
num_vars_2024 <- app_2024[, c("founding_year", "money_raised", "runway", "amount_raising", "team_size")]
cor_matrix_2024 <- cor(num_vars_2024, use = "pairwise.complete.obs")
print(round(cor_matrix_2024, 2))

col_blue_economy <- colorRampPalette(c("#042333", "#ffffff", "#00A4CC"))(200)

corrplot(cor_matrix_2024, 
         method = "color", 
         type = "upper", 
         col = col_blue_economy,
         addCoef.col = "black", 
         tl.col = "#042333",    
         tl.srt = 45,           
         diag = FALSE,          
         title = "Matrice di Correlazione (2024)",
         mar = c(0,0,2,0)) 

plot(app_2024$money_raised, app_2024$amount_raising,
     main = "Relazione tra Money Raised e Amount Raising",
     xlab = "Money Raised (€)", ylab = "Amount Raising (€)",
     col = "#104E8B", pch = 19, frame = FALSE)

plot(log(app_2024$money_raised + 1), log(app_2024$amount_raising + 1),
     main = "Relazione Log-Log: Money Raised vs Amount Raising",
     xlab = "log(Money Raised + 1)", ylab = "log(Amount Raising + 1)",
     col = "#00A4CC", pch = 19, frame = FALSE)


# ============================================================================== 
# 4. ANALISI BIVARIATA: INCROCI TRA VARIABILI QUALITATIVE ESPLICATIVE
# ==============================================================================

# 1. Dev Stage vs Business Model
tab_dev_bm_2024 <- table(app_2024$dev_stage, app_2024$business_model)
print(tab_dev_bm_2024)
print(chisq.test(tab_dev_bm_2024, simulate.p.value = TRUE))
mosaicplot(tab_dev_bm_2024, main = "Dev Stage vs Business Model (2024)", col = c("#4682B4", "#B0E0E6", "#104E8B"), border = "white")

# 2. Country vs Dev Stage
temp_country_24 <- ifelse(is.na(app_2024$country), "Not Specified", app_2024$country)
tab_country_dev_2024 <- table(temp_country_24, app_2024$dev_stage)
print(tab_country_dev_2024)
print(chisq.test(tab_country_dev_2024, simulate.p.value = TRUE))
mosaicplot(tab_country_dev_2024, main = "Country vs Dev Stage (2024)", col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"), border = "white", las = 2)

# 3. Tags vs Dev Stage
tab_tags_dev_2024 <- table(app_2024$tags, app_2024$dev_stage)
print(tab_tags_dev_2024)
print(chisq.test(tab_tags_dev_2024, simulate.p.value = TRUE))
mosaicplot(tab_tags_dev_2024, main = "Tags vs Dev Stage (2024)", col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"), border = "white", las = 2)

# 4. Country vs Business Model
tab_country_bm_2024 <- table(temp_country_24, app_2024$business_model)
print(tab_country_bm_2024)
print(chisq.test(tab_country_bm_2024, simulate.p.value = TRUE))
mosaicplot(tab_country_bm_2024, main = "Country vs Business Model (2024)", col = c("#042333", "#00A4CC", "#B0E0E6"), border = "white", las = 2)

# 5. Tags vs Business Model
tab_tags_bm_2024 <- table(app_2024$tags, app_2024$business_model)
print(tab_tags_bm_2024)
print(chisq.test(tab_tags_bm_2024, simulate.p.value = TRUE))
mosaicplot(tab_tags_bm_2024, main = "Tags vs Business Model (2024)", col = c("#042333", "#00A4CC", "#B0E0E6"), border = "white", las = 2)

# 6. Fund Stage vs Dev Stage
tab_fund_dev_2024 <- table(app_2024$fund_stage, app_2024$dev_stage)
print(tab_fund_dev_2024)
print(chisq.test(tab_fund_dev_2024, simulate.p.value = TRUE))
mosaicplot(tab_fund_dev_2024, main = "Fund Stage vs Development Stage (2024)", col = c("#042333", "#00A4CC", "#B0E0E6", "#4682B4"), border = "white")

# 7. Fund Stage vs Country
tab_fund_country_2024 <- table(app_2024$fund_stage, temp_country_24)
print(tab_fund_country_2024)
print(chisq.test(tab_fund_country_2024, simulate.p.value = TRUE))
mosaicplot(tab_fund_country_2024, main = "Fund Stage vs Country (2024)", col = c("#042333", "#00A4CC", "#B0E0E6"), border = "white", las = 2)


# ============================================================================== 
# 5. ANALISI BIVARIATA: VARIABILI ESPLICATIVE vs STATUS (TARGET)
# ==============================================================================

col_blue_palette <- c("#042333", "#00A4CC")
col_acc <- "#00A4CC" 
col_rej <- "#042333" 

# --- A) QUALITATIVE vs STATUS ---

# 1. Is Registered vs Status
tab_reg_status_2024 <- table(app_2024$is_registered, app_2024$status)
print(tab_reg_status_2024)
print(chisq.test(tab_reg_status_2024, simulate.p.value = TRUE))
mosaicplot(tab_reg_status_2024, color = col_blue_palette, main = "Stato Legale (Is Registered) vs Status", xlab = "Is Registered", ylab = "Status", border = "white")

# 2. Dev Stage vs Status
tab_dev_status <- table(app_2024$dev_stage, app_2024$status)
print(tab_dev_status)
print(chisq.test(tab_dev_status, simulate.p.value = TRUE))
mosaicplot(tab_dev_status, color = col_blue_palette, main = "Stadio di Sviluppo vs Status", xlab = "Dev Stage", ylab = "Status", border = "white")

# 3. Business Model vs Status
tab_bm_status <- table(app_2024$business_model, app_2024$status)
print(tab_bm_status)
print(chisq.test(tab_bm_status, simulate.p.value = TRUE))
mosaicplot(tab_bm_status, color = col_blue_palette, main = "Modello di Business vs Status", xlab = "Business Model", ylab = "Status", border = "white")

# 4. Fund Stage vs Status
tab_fund_status <- table(app_2024$fund_stage, app_2024$status)
print(tab_fund_status)
print(chisq.test(tab_fund_status, simulate.p.value = TRUE))
mosaicplot(tab_fund_status, color = col_blue_palette, main = "Fund Stage vs Status", xlab = "Fund Stage", ylab = "Status", border = "white")

# 5. Country vs Status
tab_country_status_24 <- table(temp_country_24, app_2024$status)
print(tab_country_status_24)
print(chisq.test(tab_country_status_24, simulate.p.value = TRUE))
mosaicplot(tab_country_status_24, color = col_blue_palette, main = "Country vs Status (2024)", xlab = "Country", ylab = "Status", border = "white", las=2)

# 6. Tags vs Status
tab_tags_status_24 <- table(app_2024$tags, app_2024$status)
print(tab_tags_status_24)
print(chisq.test(tab_tags_status_24, simulate.p.value = TRUE))
par(mar=c(12, 4, 4, 2))
mosaicplot(tab_tags_status_24, color = col_blue_palette, main = "Tags vs Status (2024)", xlab = "Tags", ylab = "Status", border = "white", las=2)
par(mar=c(5, 4, 4, 2))


# --- B) QUANTITATIVE vs STATUS (Triade Statistica Completa per ciascuna) ---

# 1. Money Raised vs Status
print(shapiro.test(na.omit(app_2024$money_raised)))
print(tapply(app_2024$money_raised, app_2024$status, mean, na.rm = TRUE))
print(tapply(app_2024$money_raised, app_2024$status, sd, na.rm = TRUE))
print(t.test(money_raised ~ status, data = app_2024))
boxplot(money_raised ~ status, data = app_2024, col = c(col_acc, col_rej), main = "Fondi Raccolti vs Status", ylab = "Money Raised (€)", xlab = "Status", frame = FALSE) 

# 2. Team Size vs Status
print(shapiro.test(na.omit(app_2024$team_size)))
print(tapply(app_2024$team_size, app_2024$status, mean, na.rm = TRUE))
print(tapply(app_2024$team_size, app_2024$status, sd, na.rm = TRUE))
print(t.test(team_size ~ status, data = app_2024))
boxplot(team_size ~ status, data = app_2024, col = c(col_acc, col_rej), main = "Team Size vs Status", ylab = "Numero di membri", xlab = "Status", frame = FALSE)

# 3. Amount Raising vs Status
print(shapiro.test(na.omit(app_2024$amount_raising)))
print(tapply(app_2024$amount_raising, app_2024$status, mean, na.rm = TRUE))
print(tapply(app_2024$amount_raising, app_2024$status, sd, na.rm = TRUE))
print(t.test(amount_raising ~ status, data = app_2024))
boxplot(amount_raising ~ status, data = app_2024, col = c(col_acc, col_rej), main = "Target Raccolta vs Status", ylab = "Amount Raising", xlab="Status", frame = FALSE)

# 4. Runway vs Status
print(shapiro.test(na.omit(app_2024$runway)))
print(tapply(app_2024$runway, app_2024$status, mean, na.rm = TRUE))
print(tapply(app_2024$runway, app_2024$status, sd, na.rm = TRUE))
print(t.test(runway ~ status, data = app_2024))
boxplot(runway ~ status, data = app_2024, col = c(col_acc, col_rej), main = "Runway vs Status", ylab = "Mesi", xlab="Status", frame = FALSE)

# 5. Founding Year vs Status
print(shapiro.test(na.omit(app_2024$founding_year)))
print(tapply(app_2024$founding_year, app_2024$status, mean, na.rm = TRUE))
print(tapply(app_2024$founding_year, app_2024$status, sd, na.rm = TRUE))
print(t.test(founding_year ~ status, data = app_2024))
boxplot(founding_year ~ status, data = app_2024, col = c(col_acc, col_rej), main = "Anzianità vs Status", ylab = "Anno fondazione", xlab="Status", frame = FALSE)