

#-------------------------#
# 1. Setup                #
#-------------------------#
#install.packages("Biostrings")
library("rotl")
library ("metafor")
library ("meta")
library("dplyr")
library ("orchaRd")
library ("ape")
library ("rotl")
library ("MCMCglmm")
#load(file="./data/BTdata.tab")
library("ggplot2")
library("plyr")
library ("igraph")
library("ggplot2")
library("ggtree")
library("Biostrings")
library("ggplot2")
#install.packages("ggtreeExtra")#
install.packages("ggtree")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")
install.packages('Biostrings')

library("ggtree")
library("ggtreeExtra")
library("ggstar")
library("ggnewscale")



setwd("/Users/hawraal-ghafli/Desktop/Meta/Phylogentic tree")

#a study with samples collected from more than one country are inserted in one row 

#host<-read.csv("Host_table.csv", na.strings=c("", NA))

 
#-
#making a matrix of the host, or parasite 
h<- c("Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis",
      "Bos_frontalis", "Turdus_migratorius", "Capra_hircus", "Bos_frontalis", "Ovis_aries", "Bos_frontalis", "Capra_hircus", "Ovis_aries", "Bos_frontalis", 
      "Capra_hircus", "Ovis_aries", "Gerbilliscus_vicinus", "Rattus_rattus", "Mastomys_natalensis", "Graphiurus_murinus", "Thryonomys_swinderianus", 
      "Macaca_nemestrina", "Bichromomyia_flaviscutellata", 
      "Canis_lupus_familiaris", "Catorhintha_guttula", "Catorhintha_guttula", "Catorhintha_guttula", "Catorhintha_guttula", "Chrysomya_albiceps", "Equus_asinus", "Camelus_dromedarius",
      "Felis_catus", "Felis_catus", "Bubalus_bubalis", "Bos_frontalis", 
      "Felis_catus", "Canis_lupus_familiaris", "Canis_lupus_familiaris", 
      "Canis_lupus_familiaris", "Apis_mellifera", "Prunella_modularis", 
      "Bos_frontalis", "Chaetopsylla_globiceps", "Nycteridopsylla_eusarca", 
      "Nosopsyllus_fasciatus", "Ctenophthalmus_agyrtes", "Ctenocephalides_canis", 
      "Chaetopsylla_globiceps", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera",
      "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera", "Apis_mellifera",
      "Chrysomya_putoria", "Ornidia_obesa", "Cochliomyia_macellaria", "Lucilia_eximia", "Lucilia_sericata", "Chrysomya_megacephala", "Culex_pipiens", "Culex_pipiens", "Culex_pipiens", "Culex_pipiens",
      "Canis_lupus_familiaris", "Triatoma_dimidiata", "Triatoma_nitida", "Triatoma_sordida", "Triatoma_brasiliensis", "Canis_lupus_familiaris",
      "Culex_torrentium", "Drosophila_obscura", "Triatoma_rubrovaria", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Canis_lupus_familiaris",
      "Drosophila_obscura", "Canis_lupus_familiaris", "Drosophila_obscura", "Drosophila_obscura", "Canis_lupus_familiaris", "Drosophila_obscura", "Glossina_brevipalpis", "Glossina_brevipalpis", "Apis_mellifera",
      "Glossina_brevipalpis", "Glossina_brevipalpis", "Nasua_nasua", "Lepus_europaeus", "Callithrix_jacchus", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Apis_mellifera", "Apis_mellifera", "Bombus_asiaticus", "Bombus_auricomus",
      "Bombus_bifarius", "Bombus_bimaculatus", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Bubalus_bubalis", "Camelus_dromedarius", "Camelus_dromedarius", "Myodes_glareolus", "Turdus_migratorius", "Glossina_swynnertoni", "Glossina_swynnertoni",
      "Glossina_swynnertoni", "Glossina_swynnertoni", "Glossina_swynnertoni", "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Camelus_dromedarius", "Camelus_dromedarius", "Canis_lupus_familiaris", "Bos_frontalis",
      "Sus_scrofa", "Bos_frontalis", "Sus_scrofa", "Ovis_aries", "Bos_frontalis", "Equus_caballus", "Rhodnius_prolixus", "Triatoma_dimidiata", "Glossina_brevipalpis", "Glossina_brevipalpis", "Glossina_brevipalpis",
      "Glossina_brevipalpis", "Glossina_brevipalpis", "Glossina_brevipalpis", "Glossina_morsitans_morsitans", "Glossina_morsitans_morsitans", "Glossina_morsitans_morsitans", "Glossina_morsitans_morsitans",
      "Glossina_morsitans_morsitans", "Glossina_morsitans_morsitans", "Glossina_morsitans_morsitans", "Glossina_morsitans_submorsitans",
      "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans",
      "Glossina_morsitans_submorsitans", "Camelus_dromedarius", "Canis_lupus_familiaris", "Camelus_dromedarius", "Canis_lupus_familiaris", "Illadopsis_rufipennis", "Hylia_prasina", "Pycnonotus_barbatus",
      "Phyllastrephus_albigularis", "Bleda_syndactylus", "Bleda_eximius", "Bleda_canicapillus", "Eurillas_virens", "Eurillas_latirostris", "Campethera_caroli",
      "Nigrita_bicolor", "Stizorhina_fraseri", "Stiphrornis_erythrothorax", "Neocossyphus_poensis", "Alethe_poliocephala", "Alethe_diademata", "Alethe_castanea", "Turtur_afer",
      "Canis_lupus_familiaris", "Triatoma_infestans", "Triatoma_guasayana", "Glossina_morsitans_submorsitans", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Canis_lupus_familiaris", "Bos_frontalis", "Ctenocephalides_felis_felis",
      "Icterus_spurius", "Passerina_ciris", "Passerina_cyanea", "Vireo_griseus", "Hylocichla_mustelina", "Piranga_rubra", "Canis_lupus_familiaris", "Bos_frontalis", "Capra_hircus", "Sus_scrofa", "Bos_frontalis", "Glossina_morsitans_submorsitans",
      "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Triatoma_dimidiata", "Triatoma_gerstaeckeri", "Triatoma_mexicana", "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans",
      "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Glossina_morsitans_submorsitans", "Glossina_morsitans_submorsitans", "Glossina_pallidipes",
      "Glossina_pallidipes", "Leptocorisa_lepida", "Leptocorisa_lepida", "Leptocorisa_lepida", "Equus_caballus", "Procyon_lotor", "Canis_lupus_familiaris",
      "Equus_caballus", "Panstrongylus_diasi", "Panstrongylus_megistus", "Triatoma_sordida", "Ovis_aries", "Ovis_aries", "Capra_hircus", "Capra_hircus", "Sus_scrofa", "Sus_scrofa",
      "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Bubalus_bubalis", "Merluccius_merluccius", "Canis_lupus_familiaris", "Bombus_bimaculatus", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis",
      "Glossina_pallidipes", "Bos_frontalis", "Equus_caballus", "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Glossina_pallidipes", "Glossina_pallidipes", "Bombus_bimaculatus", "Bombus_bimaculatus",
      "Bombus_bimaculatus", "Bombus_bimaculatus", "Bombus_terrestris", "Bombus_terrestris", "Bombus_californicus", "Bombus_californicus", "Bos_frontalis", "Bos_frontalis", "Glossina_pallidipes",
      "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Meriones_libycus", "Psammomys_obesus", "Bomus_californicus", "Bombus_citrinus", "Bombus_diligens", "Bombus_diligens", "Bombus_ephippiatus", "Bombus_ephippiatus", "Bombus_ephippiatus",
      "Bombus_fervidus", "Bombus_fervidus", "Bombus_fervidus", "Bombus_fervidus", "Bombus_fervidus", "Bombus_flavifrons", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Lochmias_nematura", "Automolus_leucophthalmus",
      "Syndactyla_rufosuperciliata", "Conopophaga_lineata", "Chamaeza_campanisona", "Myrmeciza_loricata", "Pyriglena_leucoptera", "Thamnophilus_caerulescens", "Equus_caballus", "Canis_lupus_familiaris",
      "Bos_frontalis", "Bos_frontalis", "Glossina_pallidipes", "Sus_scrofa", "Bos_frontalis", "Sus_scrofa", "Bos_frontalis", "Glossina_pallidipes", "Sus_scrofa", "Bos_frontalis", "Glossina_pallidipes", "Sus_scrofa",
      "Sus_scrofa", "Bombus_flavifrons", "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bombus_frigidus", "Rattus_rattus",
      "Meriones_libycus", "Rhombomys_opimus", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bubalus_bubalis", "Bos_frontalis", "Equus_caballus", "Equus_caballus", "Equus_caballus", "Equus_caballus",
      "Equus_caballus", "Equus_asinus", "Canis_lupus_familiaris", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Felis_catus", "Glossina_pallidipes", "Rattus_norvegicus", "Rattus_exulans",
      "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_pallidipes", "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis",
      "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis", "Glossina_palpalis_palpalis", "Glossina_tachinoides", "Glossina_tachinoides", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Artibeus_lituratus", "Pteronotus_personatus",
      "Esox_lucius", "Mastomys_erythroleucus",  "Crocidura_hirta", "Canis_lupus_familiaris", "Bubalus_bubalis", "Glossina_tachinoides", "Camelus_dromedarius", "Camelus_dromedarius", "Glossina_tachinoides", "Glossina_tachinoides", "Glossina_tachinoides", "Glossina_tachinoides", "Rhodnius_ecuadoriensis",
      "Rhodnius_ecuadoriensis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Vulpes_vulpes", "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Canis_lupus_familiaris", "Felis_catus", "Bombus_griseocollis", "Glossina_tachinoides", 
      "Bos_frontalis", "Glossina_tachinoides", "Bos_frontalis", "Chrysomya_albiceps", "Chrysomya_putoria", "Chrysomya_megacephala", "Lucilia_eximia", "Bos_frontalis", "Trichophoromyia_ubiquitalis", "Trichophoromyia_ubiquitalis", "Sus_scrofa", "Psathyromyia_aragaoi",
      "Lutzomyia_auraensis", "Lutzomyia_auraensis", "Lutzomyia_auraensis", "Sus_scrofa", "Sus_scrofa", "Lutzomyia_ayacuchensis", "Sus_scrofa", "Nycticeius_humeralis", "Nycticeius_humeralis",
      "Parastrellus_hesperus", "Antrozous_pallidus", "Canis_lupus_familiaris", "Triatoma_infestans", "Lutzomyia_cortelezzii", "Psychodopygus_davisi", "Psychodopygus_davisi",
      "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Bombus_griseocollis", "Baiomys_taylori", "Rattus_rattus", "Rattus_norvegicus", "Cricetomys_gambianus", "Arvicanthis_niloticus", "Mastomys_erythroleucus",
      "Psychodopygus_davisi", "Canis_lupus_familiaris", "Psathyromyia_dendrophyla", "Psychodopygus_hirsutus", "Psychodopygus_llanosmartinsi", "Lutzomyia_longipalpis", "Lutzomyia_longipalpis", "Lutzomyia_longipalpis", "Lutzomyia_longipalpis",
      "Migonemyia_migonei", "Lutzomyia_nevesi", "Lutzomyia_punctigeniculata", "Evandromyia_sallesi", "Lutzomyia_shawi", "Lutzomyia_shawi", "Lutzomyia_sherlocki", "Lutzomyia_spathotrichia", "Lutzomyia_tejadai", "Nyssomyia_umbratilis", "Lutzomyia_walkeri", "Bubalus_bubalis", "Bos_frontalis",
      "Bubalus_bubalis", "Bos_frontalis", "Bos_frontalis", "Bombus_griseocollis", "Bombus_griseocollis", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Nyssomyia_whitmani", "Nyssomyia_whitmani", "Nyssomyia_whitmani", "Lutzomyia_yucumensis", "Neomegalotomus_parvus", "Neomegalotomus_parvus",
      "Neomegalotomus_parvus", "Canis_lupus_familiaris", "Felis_catus", "Felis_catus", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "Canis_lupus_familiaris", "Neomegalotomus_parvus", "Bettongia_penicillata", "Bettongia_penicillata", "Bettongia_penicillata",
      "Bettongia_penicillata",  "Canis_lupus_familiaris", "Bos_frontalis", "Camelus_dromedarius", "Equus_caballus", "Equus_asinus", "Equus_asinus", "Bombus_hortorum", "Bombus_huntii", "Neomegalotomus_parvus", "Niesthrea_vincentii", "Niesthrea_vincentii", "Phlebotomus_kandelakii", "Phlebotomus_argentipes",
      "Capra_hircus", "Capra_hircus", "Capra_hircus", "Bos_frontalis", "Camelus_dromedarius", "Bos_frontalis", "Bos_frontalis", "Bos_frontalis", "", "", "Canis_lupus_familiaris", "", "", "", "Canis_lupus_familiaris", "", "Bos_frontalis", "Bos_frontalis", "Phlebotomus_argentipes", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Felis_catus",
      "Canis_lupus_familiaris", "Canis_lupus_familiaris", "", "Bos_frontalis", "Atelerix algirus ", "Phlebotomus_argentipes", "Sus_scrofa", "Didelphis albiventris", "Conepatus chinga", "Felis_catus", "Engystomops pustulosus", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Capra_hircus", "Capra_hircus",
      "Sus_scrofa", "Ovis_aries", "Ovis_aries", "Canis_lupus_familiaris", "Bombus_huntii", "Bombus_huntii", "Bombus_huntii", "Phlebotomus_brevis", "Meriones_libycus", "Nasua_narica", "Procyon_lotor", "Phlebotomus_caucasicus", "Phlebotomus_chinensis",
      "Phlebotomus_duboscqi", "Neomegalotomus_parvus", "Phlebotomus_halepensis", "Phlebotomus_kandelakii", "Phlebotomus_papatasi", "Phlebotomus_papatasi", "Phlebotomus_transcaucasicus", "Phlebotomus_perniciosus", "Phlebotomus_saevus",
      "Neomegalotomus_parvus", "Phlebotomus_sergenti", "Phlebotomus_sergenti", "Bombus_impatiens", "Bombus_impatiens", "Bombus_impatiens", "Bombus_impatiens", "Bombus_impatiens", "Bombus_impatiens", "Bombus_impatiens",
      "Bombus_impatiens", "Bombus_lapidarius", "Bombus_lapidarius", "Bombus_lucorum", "Bombus_lucorum", "Bombus_macgregori", "Bombus_macgregori", "Bombus_melanopygus", "Bombus_melanopygus", "Bombus_mesomelas", "Bombus_mixtus", "Bombus_mixtus", "Bombus_mixtus",
      "Bombus_monticola", "Bombus_terrestris", "Bombus_terrestris", "Bombus_pascuorum", "Bombus_pascuorum", "Bombus_pensylvanicus", "Bombus_pensylvanicus", "Bombus_pensylvanicus", "Bombus_perplexus",
      "Bombus_perplexus", "Bombus_perplexus", "Bombus_pratorum", "Bombus_pratorum", "Bombus_pyrenaeus", "Bombus_ruderarius", "Bombus_rufocinctus", "Bombus_rufocinctus", "Bombus_sandersoni", "Bombus_sandersoni","Bombus_sichelii", "Bombus_simillimus", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terrestris", "Bombus_sylvicola", "Bombus_ternarius", "Bombus_ternarius", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terrestris", "Anoura_caudifer", "Carollia_perspicillata", "Sturnira_lilium", "Anoura_geoffroyi", "Felis_catus", "Canis_lupus_familiaris", "Felis_catus",
      "Canis_lupus_familiaris", "Sus_scrofa", "Ovis_aries", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terrestris", "Bombus_terricola", "Bombus_trinominatus", "Bombus_trinominatus",
      "Bombus_trinominatus", "Bombus_vagans", "Equus_asinus", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Bos_frontalis", "Bos_frontalis", "Equus_asinus", "Equus_asinus", "Bos_frontalis",
      "Bos_frontalis", "Canis_lupus_familiaris", "Canis_lupus_familiaris", "Capra_hircus", "Equus_caballus", "Equus_asinus", "Camelus_dromedarius", "Equus_asinus", "Capra_hircus", "Ovis_aries", "Ovis_aries", "Ovis_aries",
      "Ovis_aries", "Camelus_dromedarius", "Equus_asinus", "Capra_hircus", "Ovis_aries", "Ovis_aries", "Bombus_vagans", "Bombus_vosnesenskii", "Bombus_vosnesenskii", "Bombus_vosnesenskii", "Bombus_weisi", "Bombus_weisi",
      "Bombus_wilmattae", "Bombus_wurflenii", "Rhodnius_prolixus", "Canis_lupus_familiaris", "Artibeus_lituratus", "Anoura_caudifer", "Glossophaga_soricina", "Carollia_perspicillata", "Triatoma_longipennis")
info <- read.csv("tree_ring1.csv")
h<- c("Engystomops_pustulosus","Alethe_castanea", "Alethe_diademata", "Alethe_poliocephala", "Anoura_caudifer", "Anoura_geoffroyi",
      "Antrozous_pallidus", "Apis_mellifera", "Artibeus_lituratus", "Arvicanthis_niloticus", "Automolus_leucophthalmus", "Baiomys_taylori", "Bettongia_penicillata", "Bichromomyia_flaviscutellata",
      "Bleda_canicapillus", "Bleda_eximius", "Bleda_syndactylus", "Bombus_asiaticus", "Bombus_auricomus", "Bombus_bifarius", "Bombus_bimaculatus", "Bombus_citrinus", "Bombus_diligens", "Bombus_ephippiatus",
      "Bombus_fervidus", "Bombus_flavifrons", "Bombus_frigidus", "Bombus_griseocollis", "Bombus_hortorum", "Bombus_huntii", "Bombus_impatiens", "Bombus_lapidarius", "Bombus_lucorum", "Bombus_macgregori",
      "Bombus_melanopygus", "Bombus_mesomelas", "Bombus_mixtus", "Bombus_monticola", "Bombus_pascuorum", "Bombus_pensylvanicus", "Bombus_perplexus", "Bombus_pratorum", "Bombus_pyrenaeus", "Bombus_ruderarius",
      "Bombus_rufocinctus", "Bombus_sandersoni", "Bombus_sichelii", "Bombus_simillimus", "Bombus_sylvicola", "Bombus_ternarius", "Bombus_terrestris", "Bombus_terricola", "Bombus_trinominatus",
      "Bombus_vagans", "Bombus_vosnesenskii", "Bombus_weisi", "Bombus_wilmattae", "Bombus_wurflenii", "BomBus_californicus", "Bos_frontalis", "Bubalus_bubalis",
      "Callithrix_jacchus", "Camelus_dromedarius", "Campethera_caroli", "Canis_lupus_familiaris", "Capra_hircus", "Carollia_perspicillata", "Catorhintha_guttula",
      "Chaetopsylla_globiceps", "Chamaeza_campanisona", "Chrysomya_albiceps", "Chrysomya_megacephala", "Chrysomya_putoria", "Cochliomyia_macellaria", "Conopophaga_lineata",
      "Cricetomys_gambianus", "Crocidura_hirta", "Ctenocephalides_canis", "Ctenocephalides_felis_felis", "Ctenophthalmus_agyrtes", "Culex_pipiens", "Culex_torrentium",
      "Drosophila_obscura", "Equus_asinus", "Equus_caballus", "Esox_lucius", "Eurillas_latirostris", "Eurillas_virens", "Evandromyia_sallesi",
      "Felis_catus", "Gerbilliscus_vicinus", "Glossina_brevipalpis", "Glossina_morsitans_morsitans", "Glossina_morsitans_submorsitans", "Glossina_pallidipes", "Glossina_palpalis_palpalis",
      "Glossina_swynnertoni", "Glossina_tachinoides", "Glossophaga_soricina", "Graphiurus_murinus", "Hylia_prasina", "Hylocichla_mustelina", "Icterus_spurius", "Illadopsis_rufipennis", "Leptocorisa_lepida",
      "Lepus_europaeus", "Lochmias_nematura", "Lucilia_eximia", "Lucilia_sericata", "Lutzomyia_auraensis", "Lutzomyia_ayacuchensis", "Lutzomyia_cortelezzii", "Lutzomyia_longipalpis", "Lutzomyia_nevesi",
      "Lutzomyia_punctigeniculata", "Lutzomyia_shawi", "Lutzomyia_sherlocki", "Lutzomyia_spathotrichia", "Lutzomyia_tejadai", "Lutzomyia_walkeri",
      "Lutzomyia_yucumensis", "Macaca_nemestrina", "Mastomys_erythroleucus", "Mastomys_natalensis", "Meriones_libycus", "Merluccius_merluccius", "Migonemyia_migonei",
      "Myodes_glareolus", "Myrmeciza_loricata", "Nasua_narica", "Nasua_nasua", "Neocossyphus_poensis", "Neomegalotomus_parvus", "Niesthrea_vincentii", "Nigrita_bicolor",
      "Nosopsyllus_fasciatus", "Nycteridopsylla_eusarca", "Nycticeius_humeralis", "Nyssomyia_umbratilis",
      "Nyssomyia_whitmani", "Ornidia_obesa", "Ovis_aries", "Panstrongylus_diasi", "Panstrongylus_megistus", "Parastrellus_hesperus", "Passerina_ciris", "Passerina_cyanea", "Phlebotomus_argentipes", "Phlebotomus_brevis", "Phlebotomus_caucasicus",
      "Phlebotomus_chinensis", "Phlebotomus_duboscqi",
      "Phlebotomus_halepensis", "Phlebotomus_kandelakii", "Phlebotomus_papatasi", "Phlebotomus_perniciosus", "Phlebotomus_saevus", "Phlebotomus_sergenti", "Phlebotomus_transcaucasicus",
      "Phyllastrephus_albigularis", "Piranga_rubra", "Procyon_lotor", "Prunella_modularis", "Psammomys_obesus", "Psathyromyia_aragaoi", "Psathyromyia_dendrophyla", "Psychodopygus_davisi", "Psychodopygus_hirsutus", "Psychodopygus_llanosmartinsi",
      "Pteronotus_personatus", "Pycnonotus_barbatus",
      "Pyriglena_leucoptera", "Rattus_exulans", "Rattus_norvegicus", "Rattus_rattus", "Rhodnius_ecuadoriensis", "Rhodnius_prolixus", "Rhombomys_opimus",
      "Stiphrornis_erythrothorax", "Stizorhina_fraseri", "Sturnira_lilium", "Sus_scrofa", "Syndactyla_rufosuperciliata", "Thamnophilus_caerulescens", "Thryonomys_swinderianus", "Triatoma_brasiliensis", "Triatoma_dimidiata", "Triatoma_gerstaeckeri",
      "Triatoma_guasayana", "Triatoma_infestans", "Triatoma_longipennis", "Triatoma_mexicana", "Triatoma_nitida", "Triatoma_rubrovaria", "Triatoma_sordida", "Trichophoromyia_ubiquitalis", "Turdus_migratorius", "Turtur_afer", "Vireo_griseus", "Vulpes_vulpes")
#h<-info$Host
h1<-unique(h)
h1<- as.character(h1)
#h1<-as.character(h1)
#making a tree for parasite (p1) and for host (h1)
taxon_searchh <- tnrs_match_names(names = h, context_name = "All life")
#<-as.character(h1)
#taxon_searchh <- tnrs_match_names(names = h1, context_name = "Animals")
knitr::kable(taxon_searchh)
#knitr::kable(info)
ott_in_tree3<- ott_id(taxon_searchh)[is_in_tree(ott_id(taxon_searchh))]
tr3 <- tol_induced_subtree(ott_ids = ott_in_tree3)
plot(tr3, cex=.6)
tr3$tip.label
tree3_grafen <- compute.brlen(tr3, method="Grafen", power=1)
plot(tree3_grafen, cex=.2)
tr3
ggtree(tr3, layout="circular") + ggtitle("(Phylogram) circular layout")
dev.new(width=500, height=500)

groupInfo <- split(tr3$tip.label, gsub("_\\w+", "", tr3$tip.label))
tr3 <- groupOTU(info$Host, groupInfo)
tip <- c("Lutzomyia_chiapanensis_ott4457052", "Omus_californicus_ott370841")

plot(drop.tip(tr3, tip), trim.internal = FALSE))
ggtree(drop.tip(tr3, tip), layout='circular') + geom_tiplab(size=2.1, aes(angle=angle))+ xlim(-31, 31)
ggtree(tr3, layout='circular') + geom_highlight()


hh1<- as.data.frame(host)
host$Order<-as.factor(host$Order)
unique(host$all_type2)
nodeids <- nodeid(tr3, tr3$node.label[nchar(tr3$node.label)>4])
nodedf <- data.frame(node=nodeids)
nodelab <- gsub("[\\.0-9]", "", tr3$node.label[nchar(tr3$node.label)>4])
# The layers of clade and hightlight
poslist <- c(1.6, 1.4, 1.6, 0.8, 0.1, 0.25, 1.6, 1.6, 1.2, 0.4,
             1.2, 1.8, 0.3, 0.8, 0.4, 0.3, 0.4, 0.4, 0.4, 0.6,
             0.3, 0.4, 0.3)
labdf <- data.frame(node=nodeids, label=nodelab, pos=poslist)

p<-  ggtree(drop.tip(tr3, tip), layout="circular", open.angle=10) + 
  geom_tippoint(info, mapping=aes(y= Host, fill=all_type), 
                size=1.5,
                show.legend=FALSE)
p

x <- tr3
info <- read.csv("tree_ring1.csv")
head (info)
info <- as.data.frame(info)
knitr::kable((info))

unwanted=c("mixed", "mixed ", "")
#info<- info[which(info$host_species!=unwanted[1]),]
#info<- info[which(info$host_species!=unwanted[2]),]
#info<- info[which(info$host_species!=unwanted[3]),]

head(info)
info<- as.data.frame(info)
p <- ggtree(drop.tip(x, tip), layout="circular", open.angle=5) 
p <- ggtree(drop.tip(x, tip), layout="fan", open.angle=10, size=0.5)
#p<-  ggtree(x, layout="fan", open.angle=5, size=0.5) %<+% info + xlim(-.1, 4)
p

p1.1<-  p+  geom_tippoint(data=info,
                          mapping=aes(color=all_type), 
                          size=1.5,
                          show.legend=FALSE)
p1.1





p1 <- p +
  geom_fruit(
    geom=geom_tile,
    data=info,
    mapping = aes(
      y=Host,
      fill=all_type,
      Group=all_type),
      position="identity",starstroke=0.2) +
      scale_fill_manual(values=c("#0000FF","#FFA500","#FF0000"),
                        guide=guide_legend(keywidth = 0.7, keyheight = 0.7, order=3,
                                           override.aes=list(starshape=3)),
                        na.translate=FALSE)+
      scale_starshape_manual(values=c(0, 3),
                             guide=guide_legend(keywidth = 0.8, keyheight = 0.8, order=3),
                             na.translate=FALSE)+
      scale_size_continuous(range = c(0, 3),
                            guide = guide_legend(keywidth = 0.5, keyheight = 0.5, order=3,
                                                 override.aes=list(starshape=3)))
    
p1
dev.new(width=1500, height=1500)




#range(info$abundance)

#try witout abundance
p2.1<- p1 +  new_scale_color() +
  geom_fruit(data=info, geom=geom_tile,
             mapping=aes(y=Host,  alpha=abundance, fill=all_type),
             color = "grey50", offset = 0.04,size = 0.05)+
  scale_alpha_continuous(range=c(0.1, 10),
                         guide=guide_legend(keywidth = 0.3, keyheight = 0.3, order=3)) +
  geom_fruit(data=info, geom=geom_bar,
             mapping=aes(y=Host, fill=all_type, alpha=abundance),
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
  ) +
  scale_fill_manual(values=c("#0000FF","#FFA500","#FF0000"),
                    guide=guide_legend(keywidth = 0.3, keyheight = 0.3, order=3))+
  geom_treescale(fontsize=1, linesize=0, x=4.9, y=0.1) +
  theme(legend.position=c(0.93, 0.8),
        legend.background=element_rect(fill=NA),
        legend.title=element_text(size=6.5),
        legend.text=element_text(size=4.5),
        legend.spacing.y = unit(0.02, "cm"),
  )
p2.1

p2.2<- p2.1 +  new_scale_fill() +
  geom_fruit(data=info, geom=geom_bar,
             mapping=aes(y=Host, x=prev, fill=all_type),
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
  ) +
  scale_fill_manual(values=c("#0000FF","#FFA500","#FF0000"),
                    guide=guide_legend(keywidth = 0.7, keyheight = 0.7, order=4))+
  geom_treescale(fontsize=1, linesize=0, x=4.9, y=0.1) +
  theme(legend.position=c(0.99, 0.5),
        legend.background=element_rect(fill=NA),
        legend.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.spacing.y = unit(0.03, "cm"),
  )


dev.new(width=1500, height=1500)


library(RColorBrewer)
my.cols <- brewer.pal(9, "Blues")
my.cols1 <- brewer.pal(9, "Greens")
my.cols2 <- brewer.pal(9, "RdBu")

my.cols
p3<- p2.1 +  new_scale_fill() +
  geom_fruit(data=info, geom=geom_tile,
             mapping=aes(y=Host, fill=host_group),
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
  ) + scale_fill_manual(values=c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#bc74c4", "#74C476", 
                                 "#b0057a", "0570B0", "#520e3c", "#2166AC","#B2182B", "#D6604D", "#F4A582", "#FDDBC7" , "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
                    guide=guide_legend(keywidth = 0.7, keyheight = 0.7, order=22))+
  geom_treescale(fontsize=1, linesize=0, x=4.9, y=0.1) +
  theme(legend.position=c(0.99, 0.5),
        legend.background=element_rect(fill=NA),
        legend.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.spacing.y = unit(0.03, "cm"),
  )
p3

p3.1<- p3 +  new_scale_fill() +
  geom_fruit(data=info, geom=geom_bar,
             mapping=aes(y=Host, x=prev, fill=all_type),
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
             axis.params=list(
               axis="x", # add axis text of the layer.
               text.angle=-45, # the text size of axis.
               hjust=0  # adust the horizontal position of text of axis.
             ),
             grid.params=list() # add the grid line of the external bar plot.
  ) +scale_fill_manual(values=c("#0000FF","#FFA500","#FF0000"),
                    guide=guide_legend(keywidth = 0.7, keyheight = 0.7, order=4))+
  geom_treescale(fontsize=1, linesize=0, x=4.9, y=0.1) +
  theme(legend.position=c(0.99, 0.5),
        legend.background=element_rect(fill=NA),
        legend.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.spacing.y = unit(0.03, "cm"),
  )

p3.1
p1 <- p + geom_star(data=info,
  mapping=aes(y= Host, starshape=all_type),
  position="identity",starstroke=0.2) +
  scale_fill_manual(values=c("#FFC125","#87CEFA","#7B68EE","#808080","#800080",
                             "#9ACD32","#D15FEE","#FFC0CB","#EE6A50","#8DEEEE",
                             "#006400","#800000","#B0171F","#191970"),
                    guide=guide_legend(keywidth = 0.5, keyheight = 0.5, order=1,
                                       override.aes=list(starshape=15)),
                    na.translate=FALSE)+
  scale_starshape_manual(values=c(15, 1),
                         guide=guide_legend(keywidth = 0.5, keyheight = 0.5, order=2),
                         na.translate=FALSE)+
  scale_size_continuous(range = c(1, 2.5),
                        guide = guide_legend(keywidth = 0.5, keyheight = 0.5, order=3,
                                             override.aes=list(starshape=15)))
p3.1
tiff(filename =  "tree.tiff",
     res = 300,width = 5.5, height = 5.2, units = 'in',
     compression = c( "lzw") )
p3.1
dev.off()

