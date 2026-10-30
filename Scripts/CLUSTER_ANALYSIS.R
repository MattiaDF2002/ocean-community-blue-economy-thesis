# PREPARAZIONE MATRICE ATTIVA E DISTANZE (Barriere e Servizi)

df_cluster_all <- data.frame(
  Capitale    = survey_clean$cap_num,
  Tecnologia  = survey_clean$tech_num,
  Regolazione = survey_clean$reg_num,
  Mercato     = survey_clean$market_num,
  SocioCult   = survey_clean$soc_num,
  Interni     = survey_clean$int_num,
  Mentorship  = survey_clean$ment_num,
  Pitch_Vis   = survey_clean$pitch_num,
  Training    = survey_clean$train_num,
  Networking  = survey_clean$net_num,
  Peer_Learn  = survey_clean$peer_num
)

rownames(df_cluster_all) <- substr(survey_clean$startup_name, 1, 15)

df_cluster <- na.omit(df_cluster_all)

cat("Dimensioni dataframe attivo:", dim(df_cluster), "\n")

dist_manhattan <- dist(df_cluster, method = "manhattan")
hc_ward <- hclust(dist_manhattan, method = "ward.D2")

par(mar = c(6, 4, 4, 2) + 0.1) 
plot(hc_ward, 
     main = "Dendrogramma delle Startup (Ward.D2, Manhattan)",
     xlab = "Startup Finaliste", 
     ylab = "Distanza / Altezza di Fusione",
     sub = "Fonte: elaborazione personale su dati primari dell'indagine (2026)",
     col = "#042333", 
     col.main = "#042333",
     cex = 0.9,
     lwd = 2) 



# ==============================================================================
# STEP 4B.1: SANITY CHECK PER IL DATA MERGING (Nomi Startup)
# ==============================================================================

# 1. Estraiamo i nomi delle 13 startup valide dalla Survey
nomi_survey <- survey_clean$startup_name[idx_valid]

# 2. Estraiamo i nomi delle 173 startup dal Master Dataset
nomi_master <- app_master$startup_name

# 3. Cerchiamo la corrispondenza esatta (ignorando maiuscole/minuscole per sicurezza)
# La funzione match() restituisce la posizione, oppure NA se non trova il nome
matching_index <- match(tolower(nomi_survey), tolower(nomi_master))

# 4. Verifichiamo quali startup NON hanno trovato una corrispondenza esatta
startup_senza_match <- nomi_survey[is.na(matching_index)]

cat("--- RISULTATO DEL SANITY CHECK DEI NOMI ---\n")
cat("Numero di startup con match perfetto:", sum(!is.na(matching_index)), "su 13\n")

if(length(startup_senza_match) > 0) {
  cat("\nATTENZIONE! Le seguenti startup della Survey NON trovano un match esatto nel Master Dataset:\n")
  print(startup_senza_match)
} else {
  cat("\nPERFETTO! Tutte le 13 startup hanno una corrispondenza esatta. Possiamo procedere al Merge diretto.\n")
}


print(app_master$startup_name[grep("SmartDry", app_master$startup_name, ignore.case = TRUE)])
print(app_master$startup_name[grep("ALGAESYS|Algae", app_master$startup_name, ignore.case = TRUE)])
print(app_master$startup_name[grep("Fish|Aquaponics", app_master$startup_name, ignore.case = TRUE)])
print(app_master$startup_name[grep("Slow Mill", app_master$startup_name, ignore.case = TRUE)])
print(app_master$startup_name[grep("C-Plastic|Capture", app_master$startup_name, ignore.case = TRUE)])


survey_clean$startup_name[survey_clean$startup_name == "SmartDry Liquefaction"] <- "SmartDry Dredging"
survey_clean$startup_name[survey_clean$startup_name == "ALGAESYS"] <- "ALGAESYS S.A."
survey_clean$startup_name[survey_clean$startup_name == "Fish n' Greens (company: Aquaponics Iberia)"] <- "Fish n' Greens"
survey_clean$startup_name[survey_clean$startup_name == "Slow Mill Sustainable Power"] <- "Slow Mill Wave Power"
survey_clean$startup_name[survey_clean$startup_name == "C-Plastic (Capture Plastic, Lda)"] <- "C-PLASTIC"

survey_finale <- survey_clean[!is.na(survey_clean$Cluster_ID), c("startup_name", "Cluster_ID")]

dataset_finale <- merge(survey_finale, app_master, by = "startup_name", all.x = TRUE, all.y = FALSE)

print(dim(dataset_finale))
colnames(dataset_finale)
View(dataset_finale)


survey_valide <- survey_clean[!is.na(survey_clean$Cluster_ID), ]

survey_valide$merge_key <- tolower(survey_valide$startup_name)
app_master$merge_key <- tolower(app_master$startup_name)

dataset_finale <- merge(survey_valide, app_master, by = "merge_key", all.x = TRUE, all.y = FALSE)

names(dataset_finale)[names(dataset_finale) == "startup_name.x"] <- "startup_name"
dataset_finale$startup_name.y <- NULL
dataset_finale$merge_key <- NULL # 

print(dim(dataset_finale))

print(dataset_finale[grep("trigan", tolower(dataset_finale$startup_name)), c("startup_name", "country", "tags", "Cluster_ID")])


dataset_finale <- dataset_finale[-12, ]
print(dim(dataset_finale))
print(dataset_finale[grep("trigan", tolower(dataset_finale$startup_name)), c("startup_name", "country", "tags", "Cluster_ID")])

write.csv(dataset_finale, file = "Dataset Finale.csv", row.names = FALSE)
write.csv(app_master, file = "Dataset Master.csv", row.names = FALSE)
write.csv(survey_clean, file = "Dataset Survey.csv", row.names = FALSE)




# Importazione del dataset finale
dataset_finale <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

# Undici variabili utilizzate per costruire i cluster
variabili_cluster <- c(
  "cap_num", "tech_num", "reg_num", "market_num",
  "soc_num", "int_num", "ment_num", "pitch_num",
  "train_num", "net_num", "peer_num"
)

df_cluster <- dataset_finale[, variabili_cluster]
rownames(df_cluster) <- dataset_finale$startup_name

# Metodo effettivamente usato
hc_manhattan <- hclust(
  dist(df_cluster, method = "manhattan"),
  method = "ward.D2"
)

# Configurazione applicata nell'esempio del manuale
hc_euclidea_std <- hclust(
  dist(scale(df_cluster), method = "euclidean"),
  method = "ward.D2"
)

cluster_manhattan <- cutree(hc_manhattan, k = 3)
cluster_euclidea_std <- cutree(hc_euclidea_std, k = 3)

print(table(
  Salvato = dataset_finale$Cluster_ID,
  Manhattan_Ward = cluster_manhattan,
  useNA = "ifany"
))

print(table(
  Salvato = dataset_finale$Cluster_ID,
  Euclidea_Standardizzata_Ward = cluster_euclidea_std,
  useNA = "ifany"
))

print(data.frame(
  Startup = dataset_finale$startup_name,
  Cluster_salvato = dataset_finale$Cluster_ID,
  Manhattan_Ward = cluster_manhattan,
  Euclidea_standardizzata_Ward = cluster_euclidea_std
))


# Distanze originali dei due metodi
dist_manhattan <- dist(
  df_cluster,
  method = "manhattan"
)

dist_euclidea_std <- dist(
  scale(df_cluster),
  method = "euclidean"
)

# Correlazione tra distanze originali e distanze cophenetiche
correlazioni_cophenetiche <- c(
  Manhattan_Ward = cor(
    dist_manhattan,
    cophenetic(hc_manhattan)
  ),
  Euclidea_Standardizzata_Ward = cor(
    dist_euclidea_std,
    cophenetic(hc_euclidea_std)
  )
)

print(round(correlazioni_cophenetiche, 3))



# Metodi alternativi con distanza Manhattan
hc_manhattan_average <- hclust(
  dist_manhattan,
  method = "average"
)

hc_manhattan_complete <- hclust(
  dist_manhattan,
  method = "complete"
)

# Metodi alternativi con distanza euclidea standardizzata
hc_euclidea_std_average <- hclust(
  dist_euclidea_std,
  method = "average"
)

hc_euclidea_std_complete <- hclust(
  dist_euclidea_std,
  method = "complete"
)

# Confronto delle correlazioni cophenetiche
confronto_cophenetico <- c(
  Manhattan_Ward = cor(
    dist_manhattan,
    cophenetic(hc_manhattan)
  ),
  Manhattan_Average = cor(
    dist_manhattan,
    cophenetic(hc_manhattan_average)
  ),
  Manhattan_Complete = cor(
    dist_manhattan,
    cophenetic(hc_manhattan_complete)
  ),
  EuclideaStd_Ward = cor(
    dist_euclidea_std,
    cophenetic(hc_euclidea_std)
  ),
  EuclideaStd_Average = cor(
    dist_euclidea_std,
    cophenetic(hc_euclidea_std_average)
  ),
  EuclideaStd_Complete = cor(
    dist_euclidea_std,
    cophenetic(hc_euclidea_std_complete)
  )
)

print(round(confronto_cophenetico, 3))



# Taglio a tre cluster con il metodo average
cluster_manhattan_average <- cutree(
  hc_manhattan_average,
  k = 3
)

cluster_euclidea_std_average <- cutree(
  hc_euclidea_std_average,
  k = 3
)

# Confronto con i cluster salvati
print(table(
  Salvato = dataset_finale$Cluster_ID,
  Manhattan_Average = cluster_manhattan_average,
  useNA = "ifany"
))

print(table(
  Salvato = dataset_finale$Cluster_ID,
  EuclideaStd_Average = cluster_euclidea_std_average,
  useNA = "ifany"
))

# Confronto nominativo fra le configurazioni principali
confronto_appartenenze <- data.frame(
  Startup = dataset_finale$startup_name,
  Cluster_salvato = dataset_finale$Cluster_ID,
  Manhattan_Ward = cluster_manhattan,
  Manhattan_Average = cluster_manhattan_average,
  EuclideaStd_Ward = cluster_euclidea_std,
  EuclideaStd_Average = cluster_euclidea_std_average
)

print(confronto_appartenenze, row.names = FALSE)



# Tagli alternativi del dendrogramma Ward euclideo standardizzato
cluster_k2 <- cutree(hc_euclidea_std, k = 2)
cluster_k3 <- cutree(hc_euclidea_std, k = 3)
cluster_k4 <- cutree(hc_euclidea_std, k = 4)

cat("--- Numerosità con k = 2 ---\n")
print(table(cluster_k2, useNA = "ifany"))

cat("\n--- Numerosità con k = 3 ---\n")
print(table(cluster_k3, useNA = "ifany"))

cat("\n--- Numerosità con k = 4 ---\n")
print(table(cluster_k4, useNA = "ifany"))

# Appartenenze prodotte dai tre possibili tagli
confronto_k <- data.frame(
  Startup = dataset_finale$startup_name,
  K2 = cluster_k2,
  K3 = cluster_k3,
  K4 = cluster_k4
)

print(confronto_k, row.names = FALSE)

# Ultime sei altezze e relativi incrementi
altezze_fusione <- data.frame(
  Fusione = seq_along(hc_euclidea_std$height),
  Altezza = round(hc_euclidea_std$height, 3),
  Incremento = c(
    NA,
    round(diff(hc_euclidea_std$height), 3)
  )
)

print(tail(altezze_fusione, 6), row.names = FALSE)



# Funzione R base per calcolare la silhouette media
silhouette_media_base <- function(cluster, distanza) {
  
  matrice_distanze <- as.matrix(distanza)
  valori_silhouette <- numeric(length(cluster))
  
  for (i in seq_along(cluster)) {
    
    stesso_cluster <- which(cluster == cluster[i])
    stesso_cluster <- stesso_cluster[stesso_cluster != i]
    
    # Per convenzione, la silhouette di un cluster singleton è zero
    if (length(stesso_cluster) == 0) {
      
      valori_silhouette[i] <- 0
      
    } else {
      
      a_i <- mean(matrice_distanze[i, stesso_cluster])
      
      altri_cluster <- unique(cluster[cluster != cluster[i]])
      b_i <- Inf
      
      for (gruppo in altri_cluster) {
        
        distanza_media <- mean(
          matrice_distanze[i, cluster == gruppo]
        )
        
        if (distanza_media < b_i) {
          b_i <- distanza_media
        }
      }
      
      valori_silhouette[i] <- (b_i - a_i) / max(a_i, b_i)
    }
  }
  
  return(mean(valori_silhouette))
}

silhouette_medie <- c(
  K2 = silhouette_media_base(cluster_k2, dist_euclidea_std),
  K3 = silhouette_media_base(cluster_k3, dist_euclidea_std),
  K4 = silhouette_media_base(cluster_k4, dist_euclidea_std)
)

print(round(silhouette_medie, 3))


# Dendrogramma definitivo: Euclidea standardizzata + Ward.D2
par(mar = c(9, 4, 4, 2) + 0.1)

plot(
  hc_euclidea_std,
  labels = dataset_finale$startup_name,
  main = "Dendrogramma delle Startup",
  sub = "Ward.D2, distanza euclidea e variabili standardizzate",
  xlab = "Startup finaliste",
  ylab = "Altezza di fusione",
  col = "#042333",
  col.main = "#042333",
  cex = 0.75,
  hang = -1,
  lwd = 2
)

rect.hclust(
  hc_euclidea_std,
  k = 3,
  border = c("#00A4CC", "#104E8B", "#042333")
)


#__________________________________________________________________
# FASE 1A: MEDIANE DELLE BARRIERE PER CLUSTER

dataset_finale$Cluster <- factor(
  dataset_finale$Cluster_ID,
  levels = c(1, 2, 3),
  labels = c("Cluster 1", "Cluster 2", "Cluster 3")
)

variabili_barriere <- c(
  "cap_num",
  "tech_num",
  "reg_num",
  "market_num",
  "soc_num",
  "int_num"
)

mediane_barriere <- aggregate(
  dataset_finale[, variabili_barriere],
  by = list(Cluster = dataset_finale$Cluster),
  FUN = median,
  na.rm = TRUE
)
print(mediane_barriere)


iqr_barriere <- aggregate(
  dataset_finale[, variabili_barriere],
  by = list(Cluster = dataset_finale$Cluster),
  FUN = IQR,
  na.rm = TRUE
)

print(iqr_barriere)


tab_capitale <- table(
  Cluster = dataset_finale$Cluster,
  Capitale = dataset_finale$cap_num,
  useNA = "ifany"
)

print(tab_capitale)

print(
  round(
    prop.table(tab_capitale, margin = 1) * 100,
    1
  )
)

tab_tecnologia <- table(
  Cluster = dataset_finale$Cluster,
  Tecnologia = dataset_finale$tech_num,
  useNA = "ifany"
)

print(tab_tecnologia)

print(
  round(
    prop.table(tab_tecnologia, margin = 1) * 100,
    1
  )
)


tab_regolazione <- table(
  Cluster = dataset_finale$Cluster,
  Regolazione = dataset_finale$reg_num,
  useNA = "ifany"
)

print(tab_regolazione)

print(
  round(
    prop.table(tab_regolazione, margin = 1) * 100,
    1
  )
)

tab_mercato <- table(
  Cluster = dataset_finale$Cluster,
  Mercato = dataset_finale$market_num,
  useNA = "ifany"
)

print(tab_mercato)

print(
  round(
    prop.table(tab_mercato, margin = 1) * 100,
    1
  )
)


tab_socioculturale <- table(
  Cluster = dataset_finale$Cluster,
  Socioculturale = dataset_finale$soc_num,
  useNA = "ifany"
)

print(tab_socioculturale)

print(
  round(
    prop.table(tab_socioculturale, margin = 1) * 100,
    1
  )
)


tab_interna <- table(
  Cluster = dataset_finale$Cluster,
  Interna = dataset_finale$int_num,
  useNA = "ifany"
)

print(tab_interna)

print(
  round(
    prop.table(tab_interna, margin = 1) * 100,
    1
  )
)



# GRAFICO DELLE MEDIANE DELLE 6 BARRIERE

par(mar = c(7, 4, 4, 2) + 0.1)

barplot(
  as.matrix(mediane_barriere[, -1]),
  beside = TRUE,
  names.arg = c(
    "Capitale",
    "Tecnologia",
    "Regolazione",
    "Mercato",
    "Socio-\nculturale",
    "Interna"
  ),
  col = c("#00A4CC", "#104E8B", "#042333"),
  border = NA,
  ylim = c(0, 5),
  main = "Profilo mediano delle barriere per cluster",
  ylab = "Mediana della barriera (1–5)",
  las = 1,
  legend.text = mediane_barriere$Cluster,
  args.legend = list(
    x = "topright",
    bty = "n",
    cex = 0.8
  )
)
box(bty = "l")


# ==============================================================================
# FASE 1B

variabili_servizi <- c(
  "ment_num",
  "pitch_num",
  "train_num",
  "net_num",
  "peer_num"
)

mediane_servizi <- aggregate(
  dataset_finale[, variabili_servizi],
  by = list(Cluster = dataset_finale$Cluster),
  FUN = median,
  na.rm = TRUE
)

print(mediane_servizi)


iqr_servizi <- aggregate(
  dataset_finale[, variabili_servizi],
  by = list(Cluster = dataset_finale$Cluster),
  FUN = IQR,
  na.rm = TRUE
)

print(iqr_servizi)

tab_mentoring <- table(
  Cluster = dataset_finale$Cluster,
  Mentoring = dataset_finale$ment_num,
  useNA = "ifany"
)
print(tab_mentoring)
print(round(prop.table(tab_mentoring, margin = 1) * 100, 1))

tab_pitch <- table(
  Cluster = dataset_finale$Cluster,
  Pitch = dataset_finale$pitch_num,
  useNA = "ifany"
)
print(tab_pitch)
print(round(prop.table(tab_pitch, margin = 1) * 100, 1))

tab_formazione <- table(
  Cluster = dataset_finale$Cluster,
  Formazione = dataset_finale$train_num,
  useNA = "ifany"
)
print(tab_formazione)
print(round(prop.table(tab_formazione, margin = 1) * 100, 1))


tab_networking <- table(
  Cluster = dataset_finale$Cluster,
  Networking = dataset_finale$net_num,
  useNA = "ifany"
)
print(tab_networking)
print(round(prop.table(tab_networking, margin = 1) * 100, 1))

tab_peer <- table(
  Cluster = dataset_finale$Cluster,
  Scambio_pari = dataset_finale$peer_num,
  useNA = "ifany"
)
print(tab_peer)
print(round(prop.table(tab_peer, margin = 1) * 100, 1))


margini <- par("mar")
par(mar = c(5, 10, 4, 2))

barplot(
  as.matrix(mediane_servizi[, -1]),
  beside = TRUE,
  horiz = TRUE,
  names.arg = c(
    "Mentoring", "Pitch e visibilita", "Formazione",
    "Networking", "Scambio tra pari"
  ),
  col = c("#00A4CC", "#042333", "#104E8B"),
  main = "Utilita percepita dei servizi OCC",
  xlab = "Mediana del punteggio (scala 1-5)",
  xlim = c(0, 5),
  las = 1,
  cex.names = 0.8,
  legend.text = mediane_servizi$Cluster,
  args.legend = list(x = "topright", bty = "n", cex = 0.8)
)

par(mar = margini)


# ==============================================================================
# FASE 2

tab_fondi_post <- table(
  Cluster = dataset_finale$Cluster,
  Fondi_post_OCC = dataset_finale$raised_post_occ,
  useNA = "ifany"
)
print(tab_fondi_post)
print(round(prop.table(tab_fondi_post, margin = 1) * 100, 1))

tab_pmf <- table(
  Cluster = dataset_finale$Cluster,
  Validazione_commerciale = dataset_finale$pmf_stage,
  useNA = "ifany"
)
print(tab_pmf)
print(round(prop.table(tab_pmf, margin = 1) * 100, 1))

set.seed(123)

test_fondi_post <- chisq.test(
  tab_fondi_post,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_fondi_post)

test_pmf <- chisq.test(
  tab_pmf,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_pmf)


startup_finanziate <- dataset_finale[
  dataset_finale$raised_post_occ == "Yes",
]

tab_importi <- table(
  Cluster = startup_finanziate$Cluster,
  Fascia_raccolta = startup_finanziate$funding_range,
  useNA = "ifany"
)
print(tab_importi)
print(round(prop.table(tab_importi, margin = 1) * 100, 1))

tab_ricavi <- table(
  Cluster = dataset_finale$Cluster,
  Ricavi_annui = dataset_finale$annual_revenue,
  useNA = "ifany"
)
print(tab_ricavi)
print(round(prop.table(tab_ricavi, margin = 1) * 100, 1))


print(
  dataset_finale[, c(
    "startup_name", "Cluster",
    "pmf_stage", "annual_revenue"
  )],
  row.names = FALSE
)

tab_fonti <- table(
  Cluster = startup_finanziate$Cluster,
  Fonte = startup_finanziate$funding_source,
  useNA = "ifany"
)
print(tab_fonti)
print(round(prop.table(tab_fonti, margin = 1) * 100, 1))



fonti_finanziate <- trimws(startup_finanziate$funding_source)
fonti_finanziate[fonti_finanziate == ""] <- NA

tab_fonti <- table(
  Cluster = startup_finanziate$Cluster,
  Fonte = fonti_finanziate,
  useNA = "ifany"
)
print(tab_fonti)
print(round(prop.table(tab_fonti, margin = 1) * 100, 1))

print(
  startup_finanziate[
    is.na(fonti_finanziate),
    c("startup_name", "Cluster")
  ],
  row.names = FALSE
)


set.seed(123)

test_importi <- chisq.test(
  tab_importi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_importi)

test_ricavi <- chisq.test(
  tab_ricavi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_ricavi)


tab_stato <- table(
  Cluster = dataset_finale$Cluster,
  Stato_attuale = dataset_finale$status_startup,
  useNA = "ifany"
)
print(tab_stato)
print(round(prop.table(tab_stato, margin = 1) * 100, 1))

tab_progressi <- table(
  Cluster = dataset_finale$Cluster,
  Progressi = dataset_finale$progress_occ,
  useNA = "ifany"
)
print(tab_progressi)
print(round(prop.table(tab_progressi, margin = 1) * 100, 1))

tapply(
  dataset_finale$progress_occ,
  dataset_finale$Cluster,
  median,
  na.rm = TRUE
)

tapply(
  dataset_finale$progress_occ,
  dataset_finale$Cluster,
  IQR,
  na.rm = TRUE
)

set.seed(123)

test_progressi <- chisq.test(
  tab_progressi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_progressi)


tab_trl <- table(
  Cluster = dataset_finale$Cluster,
  Incremento_TRL = dataset_finale$diff_trl,
  useNA = "ifany"
)
print(tab_trl)
print(round(prop.table(tab_trl, margin = 1) * 100, 1))

tab_crl <- table(
  Cluster = dataset_finale$Cluster,
  Incremento_CRL = dataset_finale$diff_crl,
  useNA = "ifany"
)
print(tab_crl)
print(round(prop.table(tab_crl, margin = 1) * 100, 1))

tab_brl <- table(
  Cluster = dataset_finale$Cluster,
  Incremento_BRL = dataset_finale$diff_brl,
  useNA = "ifany"
)
print(tab_brl)
print(round(prop.table(tab_brl, margin = 1) * 100, 1))



tab_crl_brl <- table(
  Incremento_CRL = dataset_finale$diff_crl,
  Incremento_BRL = dataset_finale$diff_brl,
  useNA = "ifany"
)
print(tab_crl_brl)

print(
  dataset_finale[, c(
    "startup_name", "Cluster",
    "diff_trl", "diff_crl", "diff_brl"
  )],
  row.names = FALSE
)


set.seed(123)
test_trl <- chisq.test(
  tab_trl,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_trl)

set.seed(123)
test_crl <- chisq.test(
  tab_crl,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_crl)

set.seed(123)
test_brl <- chisq.test(
  tab_brl,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_brl)


tab_stage_pre <- table(
  Cluster = dataset_finale$Cluster,
  Stadio_pre_OCC = dataset_finale$stage_pre_occ,
  useNA = "ifany"
)
print(tab_stage_pre)
print(round(prop.table(tab_stage_pre, margin = 1) * 100, 1))

tab_altri_programmi <- table(
  Cluster = dataset_finale$Cluster,
  Altri_programmi = dataset_finale$other_programs,
  useNA = "ifany"
)
print(tab_altri_programmi)
print(round(prop.table(tab_altri_programmi, margin = 1) * 100, 1))


set.seed(123)
test_stage_pre <- chisq.test(
  tab_stage_pre,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_stage_pre)

set.seed(123)
test_altri_programmi <- chisq.test(
  tab_altri_programmi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_altri_programmi)



tab_team <- table(
  Cluster = dataset_finale$Cluster,
  Struttura_team = dataset_finale$team_structure,
  useNA = "ifany"
)
print(tab_team)
print(round(prop.table(tab_team, margin = 1) * 100, 1))

pivot_presente <- factor(
  dataset_finale$pivot_type != "None",
  levels = c(FALSE, TRUE),
  labels = c("No", "Si")
)

tab_pivot <- table(
  Cluster = dataset_finale$Cluster,
  Almeno_un_pivot = pivot_presente,
  useNA = "ifany"
)
print(tab_pivot)
print(round(prop.table(tab_pivot, margin = 1) * 100, 1))


tab_pivot_prodotto <- table(
  Cluster = dataset_finale$Cluster,
  Prodotto = grepl("Product pivot", dataset_finale$pivot_type, fixed = TRUE),
  useNA = "ifany"
)
print(tab_pivot_prodotto)
print(round(prop.table(tab_pivot_prodotto, margin = 1) * 100, 1))

tab_pivot_mercato <- table(
  Cluster = dataset_finale$Cluster,
  Mercato = grepl("Market/target pivot", dataset_finale$pivot_type, fixed = TRUE),
  useNA = "ifany"
)
print(tab_pivot_mercato)
print(round(prop.table(tab_pivot_mercato, margin = 1) * 100, 1))

tab_pivot_modello <- table(
  Cluster = dataset_finale$Cluster,
  Modello = grepl("Business model pivot", dataset_finale$pivot_type, fixed = TRUE),
  useNA = "ifany"
)
print(tab_pivot_modello)
print(round(prop.table(tab_pivot_modello, margin = 1) * 100, 1))


dati_team <- dataset_finale[!is.na(dataset_finale$team_structure), ]

tab_team_test <- table(
  Cluster = dati_team$Cluster,
  Team = dati_team$team_structure,
  useNA = "ifany"
)
print(tab_team_test)

set.seed(123)
test_team <- chisq.test(tab_team_test, simulate.p.value = TRUE, B = 10000)
print(test_team)

set.seed(123)
test_pivot <- chisq.test(tab_pivot, simulate.p.value = TRUE, B = 10000)
print(test_pivot)

set.seed(123)
test_prodotto <- chisq.test(tab_pivot_prodotto, simulate.p.value = TRUE, B = 10000)
print(test_prodotto)

set.seed(123)
test_mercato <- chisq.test(tab_pivot_mercato, simulate.p.value = TRUE, B = 10000)
print(test_mercato)

set.seed(123)
test_modello <- chisq.test(tab_pivot_modello, simulate.p.value = TRUE, B = 10000)
print(test_modello)



tab_brevetti <- table(
  Cluster = dataset_finale$Cluster,
  Brevetti_depositati = dataset_finale$out_patents,
  useNA = "ifany"
)
print(tab_brevetti)
print(round(prop.table(tab_brevetti, margin = 1) * 100, 1))

tab_prototipi <- table(
  Cluster = dataset_finale$Cluster,
  Prototipi_testati = dataset_finale$out_prototypes,
  useNA = "ifany"
)
print(tab_prototipi)
print(round(prop.table(tab_prototipi, margin = 1) * 100, 1))

tab_certificazioni <- table(
  Cluster = dataset_finale$Cluster,
  Certificazioni = dataset_finale$out_certifications,
  useNA = "ifany"
)
print(tab_certificazioni)
print(round(prop.table(tab_certificazioni, margin = 1) * 100, 1))


# Brevetti: escludiamo solo la risposta mancante
dati_brevetti <- dataset_finale[
  !is.na(dataset_finale$out_patents),
]

tab_brevetti_test <- table(
  Cluster = dati_brevetti$Cluster,
  Brevetti = dati_brevetti$out_patents,
  useNA = "ifany"
)

set.seed(123)
test_brevetti <- chisq.test(
  tab_brevetti_test,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_brevetti)

# Prototipi: la tabella è già completa
set.seed(123)
test_prototipi <- chisq.test(
  tab_prototipi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_prototipi)

# Certificazioni: escludiamo solo la risposta mancante
dati_certificazioni <- dataset_finale[
  !is.na(dataset_finale$out_certifications),
]

tab_certificazioni_test <- table(
  Cluster = dati_certificazioni$Cluster,
  Certificazioni = dati_certificazioni$out_certifications,
  useNA = "ifany"
)

set.seed(123)
test_certificazioni <- chisq.test(
  tab_certificazioni_test,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_certificazioni)


# Stesso test, con maggiore precisione della simulazione
set.seed(123)

test_prototipi_preciso <- chisq.test(
  tab_prototipi,
  simulate.p.value = TRUE,
  B = 1000000
)

print(test_prototipi_preciso)


tab_impatto <- table(
  Cluster = dataset_finale$Cluster,
  Impatto_principale = dataset_finale$main_impact,
  useNA = "ifany"
)

print(tab_impatto)

print(
  round(
    prop.table(tab_impatto, margin = 1) * 100,
    1
  )
)

set.seed(123)

test_impatto <- chisq.test(
  tab_impatto,
  simulate.p.value = TRUE,
  B = 10000
)

print(test_impatto)


tab_attori_pari <- table(
  Cluster = dataset_finale$Cluster,
  Altri_founder_startup = grepl(
    "Peers/other founders or startups",
    dataset_finale$relevant_actors,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_attori_pari)
print(round(prop.table(tab_attori_pari, margin = 1) * 100, 1))

tab_attori_investitori <- table(
  Cluster = dataset_finale$Cluster,
  Investitori_privati = grepl(
    "Private investors",
    dataset_finale$relevant_actors,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_attori_investitori)
print(round(prop.table(tab_attori_investitori, margin = 1) * 100, 1))

tab_attori_istituzioni <- table(
  Cluster = dataset_finale$Cluster,
  Istituzioni_pubbliche = grepl(
    "Public institutions",
    dataset_finale$relevant_actors,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_attori_istituzioni)
print(round(prop.table(tab_attori_istituzioni, margin = 1) * 100, 1))


tab_attori_ricerca <- table(
  Cluster = dataset_finale$Cluster,
  Universita_ricerca = grepl(
    "Universities/research",
    dataset_finale$relevant_actors,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_attori_ricerca)
print(round(prop.table(tab_attori_ricerca, margin = 1) * 100, 1))

tab_attori_clienti <- table(
  Cluster = dataset_finale$Cluster,
  Clienti_aziendali = grepl(
    "Corporate clients",
    dataset_finale$relevant_actors,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_attori_clienti)
print(round(prop.table(tab_attori_clienti, margin = 1) * 100, 1))

tab_attori_nessuno <- table(
  Cluster = dataset_finale$Cluster,
  Nessun_attore = dataset_finale$relevant_actors == "None",
  useNA = "ifany"
)
print(tab_attori_nessuno)
print(round(prop.table(tab_attori_nessuno, margin = 1) * 100, 1))

set.seed(123)
test_attori_pari <- chisq.test(
  tab_attori_pari,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_attori_pari)

set.seed(123)
test_attori_investitori <- chisq.test(
  tab_attori_investitori,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_attori_investitori)

set.seed(123)
test_attori_istituzioni <- chisq.test(
  tab_attori_istituzioni,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_attori_istituzioni)

set.seed(123)
test_attori_nessuno <- chisq.test(
  tab_attori_nessuno,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_attori_nessuno)


tab_timing <- table(
  Cluster = dataset_finale$Cluster,
  Momento_barriere = dataset_finale$barr_timing,
  useNA = "ifany"
)

print(tab_timing)

print(
  round(
    prop.table(tab_timing, margin = 1) * 100,
    1
  )
)


set.seed(123)

test_timing <- chisq.test(
  tab_timing,
  simulate.p.value = TRUE,
  B = 10000
)

print(test_timing)




tab_goal_pmf <- table(
  Cluster = dataset_finale$Cluster,
  Product_market_fit = grepl(
    "Achieving product-market fit",
    dataset_finale$next_goal,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_goal_pmf)
print(round(prop.table(tab_goal_pmf, margin = 1) * 100, 1))

tab_goal_ricavi <- table(
  Cluster = dataset_finale$Cluster,
  Obiettivi_ricavo = grepl(
    "Reaching revenue targets",
    dataset_finale$next_goal,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_goal_ricavi)
print(round(prop.table(tab_goal_ricavi, margin = 1) * 100, 1))

tab_goal_fondi <- table(
  Cluster = dataset_finale$Cluster,
  Round_finanziamento = grepl(
    "Closing a funding round",
    dataset_finale$next_goal,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_goal_fondi)
print(round(prop.table(tab_goal_fondi, margin = 1) * 100, 1))

tab_goal_proprieta <- table(
  Cluster = dataset_finale$Cluster,
  Nuova_proprieta_intellettuale = grepl(
    "Development of new intellectual property",
    dataset_finale$next_goal,
    fixed = TRUE
  ),
  useNA = "ifany"
)
print(tab_goal_proprieta)
print(round(prop.table(tab_goal_proprieta, margin = 1) * 100, 1))


set.seed(123)
test_goal_pmf <- chisq.test(
  tab_goal_pmf,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_goal_pmf)

set.seed(123)
test_goal_ricavi <- chisq.test(
  tab_goal_ricavi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_goal_ricavi)

set.seed(123)
test_goal_fondi <- chisq.test(
  tab_goal_fondi,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_goal_fondi)



# Individuiamo le risposte presenti, gestendo anche gli spazi vuoti
note_pulite <- trimws(dataset_finale$notes_open)
note_pulite[note_pulite == ""] <- NA

tab_note <- table(
  Cluster = dataset_finale$Cluster,
  Campo_compilato = !is.na(note_pulite),
  useNA = "ifany"
)

print(tab_note)
print(round(prop.table(tab_note, margin = 1) * 100, 1))

# Leggiamo i commenti delle startup che hanno compilato il campo
print(
  dataset_finale[
    !is.na(note_pulite),
    c("startup_name", "Cluster", "notes_open")
  ],
  row.names = FALSE
)


# ==============================================================================
#DATASET MASTER
# ==============================================================================

tab_anno <- table(
  Cluster = dataset_finale$Cluster,
  Anno_candidatura = dataset_finale$year,
  useNA = "ifany"
)

print(tab_anno)
print(round(prop.table(tab_anno, margin = 1) * 100, 1))

tab_paese <- table(
  Cluster = dataset_finale$Cluster,
  Paese = dataset_finale$country,
  useNA = "ifany"
)

print(tab_paese)
print(round(prop.table(tab_paese, margin = 1) * 100, 1))

print(round(prop.table(tab_paese, margin = 1) * 100, 1))

tab_anno_test <- tab_anno[, c("2024", "2025")]
print(tab_anno_test)

set.seed(123)

test_anno <- chisq.test(
  tab_anno_test,
  simulate.p.value = TRUE,
  B = 10000
)

print(test_anno)

tab_settore <- table(
  Cluster = dataset_finale$Cluster,
  Settore = as.character(dataset_finale$tags_grouped),
  useNA = "ifany"
)

print(tab_settore)
print(round(prop.table(tab_settore, margin = 1) * 100, 1))

print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "tags_grouped"
  )],
  row.names = FALSE
)


str(dataset_finale[, c("year", "founding_year")])

anno_candidatura <- as.character(dataset_finale$year)
anno_candidatura <- as.numeric(anno_candidatura)

dataset_finale$eta_candidatura <- anno_candidatura -
  dataset_finale$founding_year

print(
  dataset_finale[, c(
    "startup_name", "Cluster",
    "year", "founding_year", "eta_candidatura"
  )],
  row.names = FALSE
)

tab_registrazione <- table(
  Cluster = dataset_finale$Cluster,
  Registrata = as.character(dataset_finale$is_registered),
  useNA = "ifany"
)

print(tab_registrazione)
print(round(prop.table(tab_registrazione, margin = 1) * 100, 1))

# Conserviamo l'età originale e creiamo la versione per l'analisi
dataset_finale$eta_candidatura_valida <- dataset_finale$eta_candidatura

dataset_finale$eta_candidatura_valida[
  dataset_finale$eta_candidatura_valida < 0
] <- NA

# Controlliamo quanti valori sono utilizzabili per cluster
tab_eta_valida <- table(
  Cluster = dataset_finale$Cluster,
  Eta_disponibile = !is.na(dataset_finale$eta_candidatura_valida),
  useNA = "ifany"
)
print(tab_eta_valida)

# Statistiche descrittive, compreso il numero di eventuali NA
tapply(
  dataset_finale$eta_candidatura_valida,
  dataset_finale$Cluster,
  summary
)

# Dispersione nella parte centrale della distribuzione
tapply(
  dataset_finale$eta_candidatura_valida,
  dataset_finale$Cluster,
  IQR,
  na.rm = TRUE
)





# Cluster e stadio di sviluppo alla candidatura
tab_sviluppo <- table(
  Cluster = dataset_finale$Cluster,
  Stadio_sviluppo = as.character(dataset_finale$dev_stage),
  useNA = "ifany"
)
print(tab_sviluppo)
print(round(prop.table(tab_sviluppo, margin = 1) * 100, 1))

# Cluster e modello di business alla candidatura
tab_business_model <- table(
  Cluster = dataset_finale$Cluster,
  Modello_business = as.character(dataset_finale$business_model),
  useNA = "ifany"
)
print(tab_business_model)
print(round(prop.table(tab_business_model, margin = 1) * 100, 1))

# Cluster e stadio di sviluppo: Chi-quadrato con p-value simulato
set.seed(123)

test_sviluppo <- chisq.test(
  tab_sviluppo,
  simulate.p.value = TRUE,
  B = 10000
)

print(test_sviluppo)


# Cluster e stadio di finanziamento alla candidatura
tab_fund_stage <- table(
  Cluster = dataset_finale$Cluster,
  Stadio_finanziamento = as.character(dataset_finale$fund_stage),
  useNA = "ifany"
)

print(tab_fund_stage)

# Percentuali all'interno di ciascun cluster
print(round(prop.table(tab_fund_stage, margin = 1) * 100, 1))


# Selezioniamo le risposte disponibili
dati_fund_stage <- dataset_finale[
  !is.na(dataset_finale$fund_stage),
]

# Tabella per il test
tab_fund_stage_test <- table(
  Cluster = dati_fund_stage$Cluster,
  Stadio_finanziamento = as.character(dati_fund_stage$fund_stage),
  useNA = "ifany"
)
print(tab_fund_stage_test)

# Chi-quadrato con p-value simulato
set.seed(123)
test_fund_stage <- chisq.test(
  tab_fund_stage_test,
  simulate.p.value = TRUE,
  B = 10000
)
print(test_fund_stage)


# Controlliamo il formato delle variabili finanziarie
str(dataset_finale[, c(
  "money_raised", "amount_raising", "runway"
)])

# Leggiamo i valori delle singole startup
print(
  dataset_finale[, c(
    "startup_name", "Cluster",
    "money_raised", "amount_raising", "runway"
  )],
  row.names = FALSE
)



# Capitale raccolto alla candidatura
tapply(
  dataset_finale$money_raised,
  dataset_finale$Cluster,
  summary
)

# Importo ricercato alla candidatura
tapply(
  dataset_finale$amount_raising,
  dataset_finale$Cluster,
  summary
)

# Autonomia finanziaria alla candidatura
tapply(
  dataset_finale$runway,
  dataset_finale$Cluster,
  summary
)


# Controlliamo il formato della dimensione del team
str(dataset_finale$team_size)

# Dimensione del team alla candidatura per ciascuna startup
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "team_size"
  )],
  row.names = FALSE
)

# Distribuzione dell'indicatore registrato nel Master
tab_team_size <- table(
  Cluster = dataset_finale$Cluster,
  Valore_team_size = dataset_finale$team_size,
  useNA = "ifany"
)
print(tab_team_size)
print(round(prop.table(tab_team_size, margin = 1) * 100, 1))

# Statistiche descrittive per cluster
tapply(
  dataset_finale$team_size,
  dataset_finale$Cluster,
  summary
)


# Indicatori da verificare
variabili_valutazione <- c(
  "internal_avg_score",
  "jury_avg_score",
  "trl",
  "is_winner"
)

# Formato delle variabili
str(dataset_finale[, variabili_valutazione])

# Valori disponibili per startup, cluster e coorte
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "year",
    variabili_valutazione
  )],
  row.names = FALSE
)


#____________________________________________________________________________________
# Distribuzione della codifica nel Master attuale
print(table(
  Anno = app_master$year,
  Is_winner = as.character(app_master$is_winner),
  useNA = "ifany"
))

# Valori nel Master per le candidature presenti nella Survey
print(
  app_master[
    app_master$app_id %in% dataset_finale$app_id,
    c("app_id", "startup_name", "year", "is_winner")
  ],
  row.names = FALSE
)

#____________________________________________________________________________________

# Etichette dell'esito finale della Challenge
esito_challenge <- factor(
  dataset_finale$is_winner,
  levels = c(0, 1),
  labels = c("Non vincitrice", "Vincitrice")
)

# Distribuzione dell'esito nei tre cluster
tab_vincita <- table(
  Cluster = dataset_finale$Cluster,
  Esito_challenge = esito_challenge,
  useNA = "ifany"
)
print(tab_vincita)

# Percentuali all'interno di ciascun cluster
print(round(prop.table(tab_vincita, margin = 1) * 100, 1))

# Cluster ed esito della Challenge
set.seed(123)

test_vincita <- chisq.test(
  tab_vincita,
  simulate.p.value = TRUE,
  B = 10000
)

print(test_vincita)


# Startup della coorte 2025 presenti nella profilazione
dati_profili_2025 <- dataset_finale[
  dataset_finale$year == "2025",
]

# Punteggio interno per cluster
tapply(
  dati_profili_2025$internal_avg_score,
  dati_profili_2025$Cluster,
  summary
)

# Punteggio della giuria per cluster
tapply(
  dati_profili_2025$jury_avg_score,
  dati_profili_2025$Cluster,
  summary
)

# Livelli TRL alla candidatura, compreso il dato mancante
tab_trl_iniziale <- table(
  Cluster = dati_profili_2025$Cluster,
  TRL_iniziale = dati_profili_2025$trl,
  useNA = "ifany"
)
print(tab_trl_iniziale)





# Lettura congiunta del profilo tecnologico
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "dev_stage",
    "diff_trl",
    "out_prototypes",
    "tech_num"
  )],
  row.names = FALSE
)



# Lettura congiunta del profilo finanziario
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "money_raised",
    "amount_raising",
    "raised_post_occ",
    "funding_range",
    "cap_num"
  )],
  row.names = FALSE
)


# Lettura congiunta di organizzazione e supporto percepito
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "team_structure",
    "int_num",
    "ment_num",
    "train_num"
  )],
  row.names = FALSE
)


# Lettura congiunta del profilo commerciale
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "dev_stage",
    "pmf_stage",
    "annual_revenue",
    "market_num"
  )],
  row.names = FALSE
)


# Lettura congiunta del contributo relazionale di OCC
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "net_num",
    "peer_num",
    "relevant_actors",
    "main_impact"
  )],
  row.names = FALSE
)


# Lettura congiunta di adattamento e priorità future
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "pivot_type",
    "progress_occ",
    "next_goal"
  )],
  row.names = FALSE
)

# Contesto delle barriere regolatorie e socioculturali
print(
  dataset_finale[, c(
    "startup_name",
    "Cluster",
    "year",
    "tags_grouped",
    "reg_num",
    "soc_num",
    "barr_timing"
  )],
  row.names = FALSE
)

# Numerosità dei tre cluster
print(table(
  Cluster = dataset_finale$Cluster,
  useNA = "ifany"
))

# Sintesi delle undici variabili utilizzate nel clustering
profili_percettivi <- aggregate(
  dataset_finale[, variabili_cluster],
  by = list(Cluster = dataset_finale$Cluster),
  FUN = median,
  na.rm = TRUE
)

print(profili_percettivi)


# Denominazioni descrittive dei tre profili
dataset_finale$Profilo_cluster <- factor(
  dataset_finale$Cluster_ID,
  levels = c(1, 2, 3),
  labels = c(
    "Barriere tecnologiche e utilita relazionale contenuta",
    "Barriera finanziaria prevalente",
    "Barriere esterne elevate"
  )
)

# Startup e profilo assegnato
print(
  dataset_finale[, c(
    "startup_name", "Cluster", "Profilo_cluster"
  )],
  row.names = FALSE
)



