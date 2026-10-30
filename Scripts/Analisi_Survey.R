# 1. DATASET SURVEY OCC (N = 15)
# ==============================================================================

survey <- read.csv("Dati_Survey.csv", 
                   stringsAsFactors = FALSE, check.names = FALSE)

dim(survey)
names(survey)
str(survey)

# FILTRAGGIO TEST, STANDARDIZZAZIONE NOMI E ANALISI STATUS STARTUP
# ================================================================

survey_clean <- survey[survey[, 2] != "Susanne Wedin Schildt", ]

print(dim(survey_clean))

# 1.3 Assegnazione nomi sintetici standardizzati per tutte le 35 variabili
colnames(survey_clean) <- c(
  "timestamp", "startup_name", "status_startup", "progress_occ", 
  "pmf_stage", "raised_post_occ", "funding_range", "funding_source", 
  "annual_revenue", "other_programs", "stage_pre_occ", 
  "diff_trl", "diff_crl", "diff_brl", "team_structure", 
  "pivot_type", "main_impact", "relevant_actors", 
  "eval_mentorship", "eval_pitch_vis", "eval_training", 
  "eval_ext_network", "eval_peer_learning", 
  "out_patents", "out_prototypes", "out_certifications", 
  "barr_capital", "barr_technology", "barr_regulation", 
  "barr_market", "barr_sociocultural", "barr_internal", 
  "barr_timing", "next_goal", "notes_open"
)

# 'status_startup'
tab_status <- table(survey_clean$status_startup, useNA = "ifany")
prop_status <- round(prop.table(tab_status) * 100, 2)
print(tab_status)
print(prop_status)

# 'progress_occ'

tab_progress <- table(survey_clean$progress_occ, useNA = "ifany")
prop_progress <- round(prop.table(tab_progress) * 100, 2)
print(tab_progress)
print(prop_progress)

prog_val <- survey_clean$progress_occ[!is.na(survey_clean$progress_occ)]

cat("Minimo: ", min(prog_val), "\n")
cat("1° Quartile (Q1): ", quantile(prog_val, 0.25), "\n")
cat("Mediana: ", median(prog_val), "\n")
cat("3° Quartile (Q3): ", quantile(prog_val, 0.75), "\n")
cat("Range Interquartilico (IQR): ", IQR(prog_val), "\n")
cat("Massimo: ", max(prog_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
col_prog <- c("#B0E0E6", "#00A4CC", "#4682B4", "#104E8B", "#042333", "gray70")
barplot(tab_progress,
        main = "Progresso Complessivo Percepito Post-OCC",
        xlab = "Livello di Progresso (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_prog[1:length(tab_progress)],
        border = NA,
        ylim = c(0, max(tab_progress, na.rm = TRUE) + 2))
box(bty = "l")


# pmf_stage

tab_pmf <- table(survey_clean$pmf_stage, useNA = "ifany")
prop_pmf <- round(prop.table(tab_pmf) * 100, 2)
print(tab_pmf)
print(prop_pmf)

par(mar = c(5, 14, 4, 10) + 0.1)
col_pmf <- c("#00A4CC", "#4682B4", "#104E8B", "#B0E0E6", "gray70")
barplot(tab_pmf,
        horiz = TRUE,
        las = 1,
        main = "Livello di Validazione Commerciale (PMF)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_pmf[1:length(tab_pmf)],
        border = NA,
        xlim = c(0, max(tab_pmf, na.rm = TRUE) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)



# raised_post_occ

tab_raised <- table(survey_clean$raised_post_occ, useNA = "ifany")
prop_raised <- round(prop.table(tab_raised) * 100, 2)
print(tab_raised)
print(prop_raised)

col_raised <- c("#00A4CC", "#042333", "gray70")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(tab_raised,
        main = "Raccolta Fondi Post-Partecipazione OCC",
        xlab = "Ha raccolto finanziamenti post-programma?",
        ylab = "Frequenza Assoluta (N)",
        col = col_raised[1:length(tab_raised)],
        border = NA,
        ylim = c(0, max(tab_raised, na.rm = TRUE) + 3))
box(bty = "l")


# funding_range & funding_source

tab_frange <- table(survey_clean$funding_range, useNA = "ifany")
prop_frange <- round(prop.table(tab_frange) * 100, 2)
print(tab_frange)
print(prop_frange)

tab_fsource <- table(survey_clean$funding_source, useNA = "ifany")
prop_fsource <- round(prop.table(tab_fsource) * 100, 2)

print(tab_fsource)
print(prop_fsource)

par(mfrow = c(1, 2), mar = c(5, 12, 4, 2) + 0.1)
col_ocean <- c("#00A4CC", "#104E8B", "#4682B4", "#B0E0E6", "#042333", "gray70")

barplot(tab_frange,
        horiz = TRUE, las = 1,
        main = "Range di Funding Raccolto",
        xlab = "Frequenza Assoluta (N)",
        col = col_ocean[1:length(tab_frange)],
        border = NA)
box(bty = "l")

barplot(tab_fsource,
        horiz = TRUE, las = 1,
        main = "Fonte Principale di Funding",
        xlab = "Frequenza Assoluta (N)",
        col = col_ocean[1:length(tab_fsource)],
        border = NA)
box(bty = "l")

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# annual_revenue
survey_clean$annual_revenue[survey_clean$annual_revenue == ""] <- NA

tab_rev <- table(survey_clean$annual_revenue, useNA = "ifany")
prop_rev_tot <- round(prop.table(tab_rev) * 100, 2)

print(tab_rev)
print(prop_rev_tot)

tab_rev_val <- table(survey_clean$annual_revenue)
prop_rev_val <- round(prop.table(tab_rev_val) * 100, 2)

print(prop_rev_val)

par(mar = c(5, 6, 4, 2) + 0.1)
col_rev <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_rev_val,
        horiz = TRUE,
        las = 1,
        main = "Fatturato Annuo Corrente (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_rev,
        border = NA,
        xlim = c(0, max(tab_rev_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# other_programs

survey_clean$other_programs[survey_clean$other_programs == ""] <- NA

tab_prog <- table(survey_clean$other_programs, useNA = "ifany")
prop_prog_tot <- round(prop.table(tab_prog) * 100, 2)

print(tab_prog)
print(prop_prog_tot)

tab_prog_val <- table(survey_clean$other_programs)
prop_prog_val <- round(prop.table(tab_prog_val) * 100, 2)

print(prop_prog_val)

par(mar = c(5, 6, 4, 2) + 0.1)
col_prog <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_prog_val,
        horiz = TRUE,
        las = 1,
        main = "Partecipazione ad Altri Programmi (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_prog,
        border = NA,
        xlim = c(0, max(tab_prog_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# stage_pre_occ

survey_clean$stage_pre_occ[survey_clean$stage_pre_occ == ""] <- NA

tab_pre <- table(survey_clean$stage_pre_occ, useNA = "ifany")
prop_pre_tot <- round(prop.table(tab_pre) * 100, 2)

print(tab_pre)

print(prop_pre_tot)

tab_pre_val <- table(survey_clean$stage_pre_occ)
prop_pre_val <- round(prop.table(tab_pre_val) * 100, 2)

print(prop_pre_val)

par(mar = c(5, 7, 4, 2) + 0.1)
col_pre <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_pre_val,
        horiz = TRUE,
        las = 1,
        main = "Stadio di Maturità Pre-OCC (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_pre,
        border = NA,
        xlim = c(0, max(tab_pre_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# diff_trl

survey_clean$diff_trl[survey_clean$diff_trl == ""] <- NA

livelli_diff <- c("No increase", "+1 level", "+2 levels", "+3 or more")
survey_clean$diff_trl_ord <- factor(survey_clean$diff_trl, levels = livelli_diff)

tab_trl <- table(survey_clean$diff_trl_ord, useNA = "ifany")
prop_trl_tot <- round(prop.table(tab_trl) * 100, 2)

print(tab_trl)

print(prop_trl_tot)

tab_trl_val <- table(survey_clean$diff_trl_ord[!is.na(survey_clean$diff_trl_ord)])
prop_trl_val <- round(prop.table(tab_trl_val) * 100, 2)

print(prop_trl_val)

par(mar = c(5, 7, 4, 2) + 0.1)
col_trl <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_trl_val,
        horiz = TRUE,
        las = 1,
        main = "Avanzamento TRL Post-OCC (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_trl,
        border = NA,
        xlim = c(0, max(tab_trl_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)



# diff_crl

survey_clean$diff_crl[survey_clean$diff_crl == ""] <- NA

livelli_diff <- c("No increase", "+1 level", "+2 levels", "+3 or more")
survey_clean$diff_crl_ord <- factor(survey_clean$diff_crl, levels = livelli_diff)

tab_crl <- table(survey_clean$diff_crl_ord, useNA = "ifany")
prop_crl_tot <- round(prop.table(tab_crl) * 100, 2)

print(tab_crl)

print(prop_crl_tot)

tab_crl_val <- table(survey_clean$diff_crl_ord[!is.na(survey_clean$diff_crl_ord)])
prop_crl_val <- round(prop.table(tab_crl_val) * 100, 2)

print(prop_crl_val)

par(mar = c(5, 7, 4, 2) + 0.1)
col_crl <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_crl_val,
        horiz = TRUE,
        las = 1,
        main = "Avanzamento CRL Post-OCC (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_crl,
        border = NA,
        xlim = c(0, max(tab_crl_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# diff_brl

survey_clean$diff_brl[survey_clean$diff_brl == ""] <- NA

livelli_diff <- c("No increase", "+1 level", "+2 levels", "+3 or more")
survey_clean$diff_brl_ord <- factor(survey_clean$diff_brl, levels = livelli_diff)

tab_brl <- table(survey_clean$diff_brl_ord, useNA = "ifany")
prop_brl_tot <- round(prop.table(tab_brl) * 100, 2)

print(tab_brl)

print(prop_brl_tot)

tab_brl_val <- table(survey_clean$diff_brl_ord[!is.na(survey_clean$diff_brl_ord)])
prop_brl_val <- round(prop.table(tab_brl_val) * 100, 2)

print(prop_brl_val)

par(mar = c(5, 7, 4, 2) + 0.1)
col_brl <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")

barplot(tab_brl_val,
        horiz = TRUE,
        las = 1,
        main = "Avanzamento BRL Post-OCC (n = 13)",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_brl,
        border = NA,
        xlim = c(0, max(tab_brl_val) + 1.5))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# team_structure

survey_clean$team_structure[survey_clean$team_structure == ""] <- NA

tab_team <- table(survey_clean$team_structure, useNA = "ifany")
prop_team_tot <- round(prop.table(tab_team) * 100, 2)
tab_team_val <- table(survey_clean$team_structure)
prop_team_val <- round(prop.table(tab_team_val) * 100, 2)

print(tab_team)
print(prop_team_val)

par(mar = c(5, 14, 4, 2) + 0.1)
col_team <- c("#042333", "#00A4CC", "#B0E0E6")
bp_team <- barplot(tab_team_val,
                   horiz = TRUE, las = 1,
                   main = "Struttura del Team Fondativo Corrente",
                   xlab = "Numero di Startup (Frequenza Assoluta)",
                   col = col_team, border = NA,
                   xlim = c(0, max(tab_team_val) + 1.5))
box(bty = "l")

# pivot_type
survey_clean$pivot_type[survey_clean$pivot_type == ""] <- NA

pivot_counts <- c(
  "Business Model Pivot" = sum(grepl("Business", survey_clean$pivot_type, ignore.case = TRUE)),
  "Product Pivot"        = sum(grepl("Product", survey_clean$pivot_type, ignore.case = TRUE)),
  "Market/Target Pivot"  = sum(grepl("Market", survey_clean$pivot_type, ignore.case = TRUE)),
  "Nessun Pivot / NA"    = sum(is.na(survey_clean$pivot_type))
)
prop_pivot <- round(pivot_counts / nrow(survey_clean) * 100, 2)

print(pivot_counts)
print(prop_pivot)

par(mar = c(5, 12, 4, 2) + 0.1)
col_pivot <- c("#042333", "#104E8B", "#00A4CC", "gray70")
bp_piv <- barplot(sort(pivot_counts[1:3], decreasing = FALSE),
                  horiz = TRUE, las = 1,
                  main = "Frequenza dei Pivot Strategici Realizzati",
                  xlab = "Numero di Menzioni",
                  col = c("#00A4CC", "#104E8B", "#042333"), border = NA,
                  xlim = c(0, max(pivot_counts[1:3]) + 1.5))
box(bty = "l")

# main_impact

survey_clean$main_impact[survey_clean$main_impact == ""] <- NA

tab_impact <- table(survey_clean$main_impact, useNA = "ifany")
prop_impact_tot <- round(prop.table(tab_impact) * 100, 2)
tab_impact_val <- sort(table(survey_clean$main_impact), decreasing = FALSE)
prop_impact_val <- round(prop.table(tab_impact_val) * 100, 2)

print(tab_impact)
print(prop_impact_val)

par(mar = c(5, 15, 4, 2) + 0.1)
col_impact <- c("#B0E0E6", "#4682B4", "#00A4CC", "#042333")
bp_imp <- barplot(tab_impact_val,
                  horiz = TRUE, las = 1,
                  main = "Impatto Principale del Percorso OCC",
                  xlab = "Numero di Startup (Frequenza Assoluta)",
                  col = col_impact, border = NA,
                  xlim = c(0, max(tab_impact_val) + 1))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# relevant_actors
survey_clean$relevant_actors[survey_clean$relevant_actors == ""] <- NA

actors_counts <- c(
  "Peers / Other Founders" = sum(grepl("Peers", survey_clean$relevant_actors, ignore.case = TRUE)),
  "Private Investors"      = sum(grepl("Private", survey_clean$relevant_actors, ignore.case = TRUE)),
  "Public Institutions"   = sum(grepl("Public", survey_clean$relevant_actors, ignore.case = TRUE)),
  "Universities / R&D"    = sum(grepl("Universities", survey_clean$relevant_actors, ignore.case = TRUE)),
  "Corporate Clients"     = sum(grepl("Corporate", survey_clean$relevant_actors, ignore.case = TRUE))
)

prop_actors_tot <- round(actors_counts / nrow(survey_clean) * 100, 2)
valid_actors_n <- sum(!is.na(survey_clean$relevant_actors))
prop_actors_val <- round(actors_counts / valid_actors_n * 100, 2)

print(actors_counts)
print(prop_actors_val)

par(mar = c(5, 13, 4, 2) + 0.1)
col_act <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")
bp_act <- barplot(sort(actors_counts, decreasing = FALSE),
                  horiz = TRUE, las = 1,
                  main = "Attori dell'Ecosistema più Rilevanti (n = 9)",
                  xlab = "Numero di Menzioni",
                  col = col_act, border = NA,
                  xlim = c(0, max(actors_counts) + 1.5))
box(bty = "l")


# eval_mentorship

survey_clean$ment_num <- as.numeric(substr(survey_clean$eval_mentorship, 1, 1))

tab_ment <- table(survey_clean$ment_num, useNA = "ifany")
prop_ment <- round(prop.table(table(survey_clean$ment_num)) * 100, 2)
ment_val <- na.omit(survey_clean$ment_num)

print(tab_ment)
print(prop_ment)
quantile(ment_val, 0.25)
median(ment_val)
quantile(ment_val, 0.75)
IQR(ment_val)
max(ment_val)

par(mar = c(5, 4, 4, 2) + 0.1)
col_likert <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")
barplot(table(survey_clean$ment_num),
        main = "Valutazione Utilità: Mentorship 1-to-1",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert, border = NA,
        ylim = c(0, max(table(survey_clean$ment_num)) + 2))
box(bty = "l")


# eval_pitch_vis

survey_clean$pitch_num <- as.numeric(substr(survey_clean$eval_pitch_vis, 1, 1))

tab_pitch <- table(survey_clean$pitch_num, useNA = "ifany")
prop_pitch <- round(prop.table(table(survey_clean$pitch_num)) * 100, 2)
pitch_val <- na.omit(survey_clean$pitch_num)

print(tab_pitch)
print(prop_pitch)
cat("Metriche Ordinali (n =", length(pitch_val), "): Min =", min(pitch_val), 
    "| Q1 =", quantile(pitch_val, 0.25), "| Mediana =", median(pitch_val), 
    "| Q3 =", quantile(pitch_val, 0.75), "| IQR =", IQR(pitch_val), 
    "| Max =", max(pitch_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(survey_clean$pitch_num),
        main = "Valutazione Utilità: Pitch Event & Visibility",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert, border = NA,
        ylim = c(0, max(table(survey_clean$pitch_num)) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)




# Training, Network, Peer Learning

survey_clean$train_num <- as.numeric(substr(survey_clean$eval_training, 1, 1))
survey_clean$net_num   <- as.numeric(substr(survey_clean$eval_ext_network, 1, 1))
survey_clean$peer_num  <- as.numeric(substr(survey_clean$eval_peer_learning, 1, 1))

tab_train <- table(survey_clean$train_num, useNA = "ifany")
prop_train <- round(prop.table(table(survey_clean$train_num)) * 100, 2)
train_val <- na.omit(survey_clean$train_num)

print(tab_train)
print(prop_train)
cat("Metriche Ordinali (n =", length(train_val), "): Min =", min(train_val), 
    "| Q1 =", quantile(train_val, 0.25), "| Mediana =", median(train_val), 
    "| Q3 =", quantile(train_val, 0.75), "| IQR =", IQR(train_val), 
    "| Max =", max(train_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
col_likert <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")
barplot(table(survey_clean$train_num),
        main = "Valutazione Utilità: Masterclasses / Training",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert[1:length(table(survey_clean$train_num))], border = NA,
        ylim = c(0, max(table(survey_clean$train_num)) + 2))
box(bty = "l")




tab_net <- table(survey_clean$net_num, useNA = "ifany")
prop_net <- round(prop.table(table(survey_clean$net_num)) * 100, 2)
net_val <- na.omit(survey_clean$net_num)

print(tab_net)
print(prop_net)
cat("Metriche Ordinali (n =", length(net_val), "): Min =", min(net_val), 
    "| Q1 =", quantile(net_val, 0.25), "| Mediana =", median(net_val), 
    "| Q3 =", quantile(net_val, 0.75), "| IQR =", IQR(net_val), 
    "| Max =", max(net_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(survey_clean$net_num),
        main = "Valutazione Utilità: External Networking",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert[1:length(table(survey_clean$net_num))], border = NA,
        ylim = c(0, max(table(survey_clean$net_num)) + 2))
box(bty = "l")




tab_peer <- table(survey_clean$peer_num, useNA = "ifany")
prop_peer <- round(prop.table(table(survey_clean$peer_num)) * 100, 2)
peer_val <- na.omit(survey_clean$peer_num)

print(tab_peer)
print(prop_peer)
cat("Metriche Ordinali (n =", length(peer_val), "): Min =", min(peer_val), 
    "| Q1 =", quantile(peer_val, 0.25), "| Mediana =", median(peer_val), 
    "| Q3 =", quantile(peer_val, 0.75), "| IQR =", IQR(peer_val), 
    "| Max =", max(peer_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(survey_clean$peer_num),
        main = "Valutazione Utilità: Peer Learning",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert[1:length(table(survey_clean$peer_num))], border = NA,
        ylim = c(0, max(table(survey_clean$peer_num)) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# valutazione utenza - mentoring

tab_mentor <- table(survey_clean$mentor_num, useNA = "ifany")
prop_mentor <- round(prop.table(table(survey_clean$mentor_num)) * 100, 2)
mentor_val <- na.omit(survey_clean$mentor_num)

print(tab_mentor)
print(prop_mentor)

cat("Metriche Ordinali Mentoring (n =", length(mentor_val), "): Min =", min(mentor_val), 
    "| Q1 =", quantile(mentor_val, 0.25), "| Mediana =", median(mentor_val), 
    "| Q3 =", quantile(mentor_val, 0.75), "| IQR =", IQR(mentor_val), 
    "| Max =", max(mentor_val), "\n")

par(mar = c(5, 4, 4, 2) + 0.1)
col_likert <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")

barplot(table(survey_clean$mentor_num),
        main = "Valutazione Utilità: Mentoring Individuale",
        xlab = "Punteggio Likert (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_likert[1:length(table(survey_clean$mentor_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$mentor_num)) + 2))
box(bty = "l")




# Brevetti, Prototipi, Certificazioni

survey_clean$out_patents[survey_clean$out_patents == ""] <- NA
survey_clean$out_prototypes[survey_clean$out_prototypes == ""] <- NA
survey_clean$out_certifications[survey_clean$out_certifications == ""] <- NA

livelli_out <- c("0", "1", "2", "3 or more")
survey_clean$pat_ord   <- factor(survey_clean$out_patents, levels = livelli_out)
survey_clean$proto_ord <- factor(survey_clean$out_prototypes, levels = livelli_out)
survey_clean$cert_ord  <- factor(survey_clean$out_certifications, levels = livelli_out)




tab_pat <- table(survey_clean$pat_ord, useNA = "ifany")
prop_pat <- round(prop.table(table(survey_clean$pat_ord)) * 100, 2)

print(tab_pat)
print(prop_pat)

par(mar = c(5, 5, 4, 2) + 0.1)
col_out <- c("#B0E0E6", "#00A4CC", "#104E8B", "#042333")
bp_pat <- barplot(table(survey_clean$pat_ord),
                  main = "Brevetti Depositati ad Oggi",
                  xlab = "Numero di Brevetti",
                  ylab = "Frequenza Assoluta",
                  col = col_out, border = NA,
                  ylim = c(0, max(table(survey_clean$pat_ord)) + 2))
box(bty = "l")




tab_proto <- table(survey_clean$proto_ord, useNA = "ifany")
prop_proto <- round(prop.table(table(survey_clean$proto_ord)) * 100, 2)

print(tab_proto)
print(prop_proto)

bp_proto <- barplot(table(survey_clean$proto_ord),
                    main = "Prototipi Testati in Ambiente Reale",
                    xlab = "Numero di Prototipi",
                    ylab = "Frequenza Assoluta",
                    col = col_out, border = NA,
                    ylim = c(0, max(table(survey_clean$proto_ord)) + 2))
box(bty = "l")




tab_cert <- table(survey_clean$cert_ord, useNA = "ifany")
prop_cert <- round(prop.table(table(survey_clean$cert_ord)) * 100, 2)

print(tab_cert)
print(prop_cert)

bp_cert <- barplot(table(survey_clean$cert_ord),
                   main = "Certificazioni Ottenute ad Oggi",
                   xlab = "Numero di Certificazioni",
                   ylab = "Frequenza Assoluta",
                   col = col_out, border = NA,
                   ylim = c(0, max(table(survey_clean$cert_ord)) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)



# Capitale, Tech, Regolazione


survey_clean$cap_num <- as.numeric(substr(survey_clean$barr_capital, 1, 1))

tab_cap <- table(survey_clean$cap_num, useNA = "ifany")
prop_cap <- round(prop.table(table(survey_clean$cap_num)) * 100, 2)
cap_val <- na.omit(survey_clean$cap_num)

print(tab_cap)
print(prop_cap)
cat("Metriche (n =", length(cap_val), "): Min =", min(cap_val), 
    "| Q1 =", quantile(cap_val, 0.25), "| Mediana =", median(cap_val), 
    "| Q3 =", quantile(cap_val, 0.75), "| IQR =", IQR(cap_val), 
    "| Max =", max(cap_val), "\n\n")

par(mar = c(5, 4, 4, 2) + 0.1)
col_barr <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")
barplot(table(survey_clean$cap_num),
        main = "Barriera: Accesso a Capitali e Liquidità",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$cap_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$cap_num)) + 2))
box(bty = "l")




survey_clean$tech_num <- as.numeric(substr(survey_clean$barr_technology, 1, 1))

tab_tech <- table(survey_clean$tech_num, useNA = "ifany")
prop_tech <- round(prop.table(table(survey_clean$tech_num)) * 100, 2)
tech_val <- na.omit(survey_clean$tech_num)

print(tab_tech)
print(prop_tech)
cat("Metriche (n =", length(tech_val), "): Min =", min(tech_val), 
    "| Q1 =", quantile(tech_val, 0.25), "| Mediana =", median(tech_val), 
    "| Q3 =", quantile(tech_val, 0.75), "| IQR =", IQR(tech_val), 
    "| Max =", max(tech_val), "\n\n")

barplot(table(survey_clean$tech_num),
        main = "Barriera: Complessità Tecnologica e R&D",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$tech_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$tech_num)) + 2))
box(bty = "l")




survey_clean$reg_num <- as.numeric(substr(survey_clean$barr_regulation, 1, 1))

tab_reg <- table(survey_clean$reg_num, useNA = "ifany")
prop_reg <- round(prop.table(table(survey_clean$reg_num)) * 100, 2)
reg_val <- na.omit(survey_clean$reg_num)

print(tab_reg)
print(prop_reg)
cat("Metriche (n =", length(reg_val), "): Min =", min(reg_val), 
    "| Q1 =", quantile(reg_val, 0.25), "| Mediana =", median(reg_val), 
    "| Q3 =", quantile(reg_val, 0.75), "| IQR =", IQR(reg_val), 
    "| Max =", max(reg_val), "\n\n")

barplot(table(survey_clean$reg_num),
        main = "Barriera: Normative e Burocrazia",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$reg_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$reg_num)) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)


# barr_market

survey_clean$market_num <- as.numeric(substr(survey_clean$barr_market, 1, 1))

tab_market <- table(survey_clean$market_num, useNA = "ifany")
prop_market <- round(prop.table(table(survey_clean$market_num)) * 100, 2)
market_val <- na.omit(survey_clean$market_num)

print(tab_market)
print(prop_market)

cat("Metriche Market (n =", length(market_val), "): Min =", min(market_val), 
    "| Q1 =", quantile(market_val, 0.25), 
    "| Mediana =", median(market_val), 
    "| Q3 =", quantile(market_val, 0.75), 
    "| IQR =", IQR(market_val), 
    "| Max =", max(market_val), "\n\n")

col_barr <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(survey_clean$market_num),
        main = "Barriera: Difficoltà di Mercato e Customer Discovery",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$market_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$market_num)) + 2))
box(bty = "l")


# socio-culturale e interna

survey_clean$soc_num <- as.numeric(substr(survey_clean$barr_sociocultural, 1, 1))

tab_soc <- table(survey_clean$soc_num, useNA = "ifany")
prop_soc <- round(prop.table(table(survey_clean$soc_num)) * 100, 2)
soc_val <- na.omit(survey_clean$soc_num)

print(tab_soc)
print(prop_soc)
cat("Metriche Socio-Cultural (n =", length(soc_val), "): Min =", min(soc_val), 
    "| Q1 =", quantile(soc_val, 0.25), 
    "| Mediana =", median(soc_val), 
    "| Q3 =", quantile(soc_val, 0.75), 
    "| IQR =", IQR(soc_val), 
    "| Max =", max(soc_val), "\n\n")

col_barr <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")
par(mar = c(5, 4, 4, 2) + 0.1)

barplot(table(survey_clean$soc_num),
        main = "Barriera: Inerzia Socio-Culturale",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$soc_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$soc_num)) + 2))
box(bty = "l")




survey_clean$int_num <- as.numeric(substr(survey_clean$barr_internal, 1, 1))

tab_int <- table(survey_clean$int_num, useNA = "ifany")
prop_int <- round(prop.table(table(survey_clean$int_num)) * 100, 2)
int_val <- na.omit(survey_clean$int_num)

print(tab_int)
print(prop_int)
cat("Metriche Internal (n =", length(int_val), "): Min =", min(int_val), 
    "| Q1 =", quantile(int_val, 0.25), 
    "| Mediana =", median(int_val), 
    "| Q3 =", quantile(int_val, 0.75), 
    "| IQR =", IQR(int_val), 
    "| Max =", max(int_val), "\n\n")

barplot(table(survey_clean$int_num),
        main = "Barriera: Limiti e Competenze Interne",
        xlab = "Grado di Ostacolo (1 = Min, 5 = Max)",
        ylab = "Frequenza Assoluta",
        col = col_barr[1:length(table(survey_clean$int_num))],
        border = NA,
        ylim = c(0, max(table(survey_clean$int_num)) + 2))
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)



# barr_timing

survey_clean$barr_timing[survey_clean$barr_timing == ""] <- NA

tab_timing <- table(survey_clean$barr_timing, useNA = "ifany")
prop_timing_tot <- round(prop.table(tab_timing) * 100, 2)

tab_timing_val <- sort(table(survey_clean$barr_timing), decreasing = FALSE)
prop_timing_val <- round(prop.table(tab_timing_val) * 100, 2)

print(tab_timing)

print(prop_timing_val)

par(mar = c(5, 15, 4, 2) + 0.1)
col_timing <- c("#B0E0E6", "#00A4CC", "#042333")

bp_time <- barplot(tab_timing_val,
                   horiz = TRUE,
                   las = 1,
                   main = "Momento di Emersione delle Barriere (n = 13)",
                   xlab = "Numero di Startup (Frequenza Assoluta)",
                   col = col_timing,
                   border = NA,
                   xlim = c(0, max(tab_timing_val) + 1.5))

par(mar = c(5, 4, 4, 2) + 0.1)



# next_goal

survey_clean$next_goal[survey_clean$next_goal == ""] <- NA

tab_goal <- table(survey_clean$next_goal, useNA = "ifany")
prop_goal_tot <- round(prop.table(tab_goal) * 100, 2)

tab_goal_val <- table(survey_clean$next_goal)
prop_goal_val <- round(prop.table(tab_goal_val) * 100, 2)

print(tab_goal)

print(prop_goal_val)

par(mar = c(5, 18, 4, 2) + 0.1)

barplot(tab_goal_val,
        horiz = TRUE,
        las = 1,
        main = "Obiettivi Strategici a 12 Mesi",
        xlab = "Frequenza Assoluta",
        col = "#00A4CC",
        border = NA)
box(bty = "l")

par(mar = c(5, 4, 4, 2) + 0.1)



# notes_open

survey_clean$notes_open[survey_clean$notes_open == ""] <- NA

tab_notes <- table(is.na(survey_clean$notes_open))
names(tab_notes) <- c("Commento Presente", "Nessun Commento (NA)")
print(tab_notes)

print(survey_clean$notes_open[!is.na(survey_clean$notes_open)])



# FIGURA 4.6: GROUPED BARPLOT PER LA TRIADE DI MATURITÀ (TRL, CRL, BRL)
col_triade <- c("#B0E0E6", "#00A4CC", "#042333") # Palette coerente Ocean Blue

# Costruiamo la matrice delle frequenze reali (13 rispondenti attivi validi)
mat_maturita <- matrix(c(
  3, 0, 0,  # No increase: TRL = 3, CRL = 0, BRL = 0
  3, 6, 6,  # +1 level   : TRL = 3, CRL = 6, BRL = 6
  3, 5, 5,  # +2 levels  : TRL = 3, CRL = 5, BRL = 5
  4, 2, 2   # +3 or more : TRL = 4, CRL = 2, BRL = 2
), nrow = 3, byrow = FALSE)

# Nominiamo le colonne (i livelli di incremento)
colnames(mat_maturita) <- c("Nessun\nincremento", "+1 Livello", "+2 Livelli", "+3 o piu'")

# Salviamo i parametri grafici originali
old_par <- par(no.readonly = TRUE)

# Allarghiamo il margine sinistro per contenere le scritte delle variabili
par(mar = c(5, 11, 4, 2) + 0.1)

# Disegniamo il grafico a barre raggruppate in senso orizzontale
barplot(mat_maturita,
        beside = TRUE,
        horiz = TRUE,
        las = 1,
        main = "Differenziali di Maturita' Post-OCC",
        xlab = "Numero di Startup (Frequenza Assoluta)",
        col = col_triade,
        border = "white")

# Inseriamo la legenda manuale in basso a destra
legend("bottomright", 
       legend = c("Delta TRL (Tech)", "Delta CRL (Market)", "Delta BRL (Business)"),
       fill = col_triade, 
       bty = "n", 
       cex = 0.8)

box(bty = "l") # Cornice ad L elegante

# Ripristiniamo i parametri originali
par(old_par)




df_servizi <- data.frame(
  Mentorship = c(5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 1),
  Training   = c(4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 1),
  Peer       = c(4, 4, 4, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1),
  Pitch      = c(5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 1),
  Network    = c(5, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1)
)

# Definiamo nomi leggibili per le etichette
colnames(df_servizi) <- c("Mentorship 1-to-1", "Masterclass/Training", "Peer Learning", "Pitch & Visibility", "External Networking")

col_servizi <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333")

old_par <- par(no.readonly = TRUE)
par(mar = c(5, 12, 4, 2) + 0.1) # Allarghiamo il margine sinistro (12) per accogliere i nomi lunghi

# Generazione del boxplot orizzontale
boxplot(df_servizi,
        horizontal = TRUE,
        las = 1,
        main = "Utilita' Percepita dei Servizi OCC",
        xlab = "Valutazione Likert (1 = Per nulla utile, 5 = Estremamente utile)",
        col = col_servizi,
        border = "black",
        cex.axis = 0.8)
box(bty = "l")

par(old_par)



  # FIGURA 4.8: BOXPLOT COMPARATIVO DELLE 6 BARRIERE ALLO SCALING (Q16)
df_barriere <- data.frame(
  Capitale     = c(5, 5, 5, 5, 5, 5, 4, 4, 3, 3, 2, 1, 1),
  Tecnologia   = c(5, 4, 4, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1),
  Regolazione  = c(5, 5, 4, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1),
  Mercato      = c(5, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1),
  Socio_Cult   = c(4, 4, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1),
  Interni      = c(3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Definiamo nomi leggibili per le etichette delle barriere
colnames(df_barriere) <- c("Capital & Funding", "Technical Complexity", "Regulation & Permits", "Market & PMF", "Socio-Cultural Inertia", "Internal Skills")

col_barriere <- c("#B0E0E6", "#50BFE6", "#00A4CC", "#104E8B", "#042333", "gray85")

old_par <- par(no.readonly = TRUE)
par(mar = c(5, 12, 4, 2) + 0.1) # Margine sinistro a 12 per far respirare le etichette

# Generazione del boxplot in senso orizzontale
boxplot(df_barriere,
        horizontal = TRUE,
        las = 1,
        main = "Severita' delle Barriere allo Scaling",
        xlab = "Grado di Ostacolo (1 = Nessun ostacolo, 5 = Ostacolo critico)",
        col = col_barriere,
        border = "black",
        cex.axis = 0.8)
box(bty = "l")

par(old_par)
