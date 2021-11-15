#factor loadings
#memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score
#working_memory =~ lns_raw + mmdsbt + tb_list_rawscore
#speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score
#executive_functioning =~ trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
#vocab =~ simil_wais_raw + tb_picturevocab_theta (only similarities as gold standard measure)
#reading =~ tb_oralread_theta (no gold standard measures)

#convergent validity
#averages for gold standard measures of cog domains
#memory
#generate z-scores 
MBAR_dataforNIHTBanalysis$CVLT_z <- scale(MBAR_dataforNIHTBanalysis$cvlt_lgdelay_free_raw, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$benson_z <- scale(MBAR_dataforNIHTBanalysis$score_benson_del, center = TRUE, scale = TRUE)

#get average for all gold-standard measures in the domain
rowwise_mean <- function(MBAR_dataforNIHTBanalysis) {Reduce(`+`, MBAR_dataforNIHTBanalysis) / ncol(MBAR_dataforNIHTBanalysis)}
MBAR_dataforNIHTBanalysis$mem_average <- rowwise_mean(MBAR_dataforNIHTBanalysis[, c("CVLT_z", "benson_z")])

#working memory
#generate z-scores for variable A using the scale() function
MBAR_dataforNIHTBanalysis$lns_z <- scale(MBAR_dataforNIHTBanalysis$lns_raw, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$dsb_z <- scale(MBAR_dataforNIHTBanalysis$mmdsbt, center = TRUE, scale = TRUE)

rowwise_mean <- function(MBAR_dataforNIHTBanalysis) {Reduce(`+`, MBAR_dataforNIHTBanalysis) / ncol(MBAR_dataforNIHTBanalysis)}
MBAR_dataforNIHTBanalysis$wm_average <- rowwise_mean(MBAR_dataforNIHTBanalysis[, c("lns_z", "dsb_z")])

#speed
#generate z-scores for variable A using the scale() function
MBAR_dataforNIHTBanalysis$coding_z <- scale(MBAR_dataforNIHTBanalysis$coding_wais, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$ss_z <- scale(MBAR_dataforNIHTBanalysis$symbol_raw, center = TRUE, scale = TRUE)

rowwise_mean <- function(MBAR_dataforNIHTBanalysis) {Reduce(`+`, MBAR_dataforNIHTBanalysis) / ncol(MBAR_dataforNIHTBanalysis)}
MBAR_dataforNIHTBanalysis$speed_average <- rowwise_mean(MBAR_dataforNIHTBanalysis[, c("coding_z", "ss_z")])

#ef
#generate z-scores for variable A using the scale() function
MBAR_dataforNIHTBanalysis$trailsb_z <- scale(MBAR_dataforNIHTBanalysis$trails_b_secondsperline, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$matrix_z <- scale(MBAR_dataforNIHTBanalysis$matrix_raw, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$stroop_z <- scale(MBAR_dataforNIHTBanalysis$stroop_interference, center = TRUE, scale = TRUE)

rowwise_mean <- function(MBAR_dataforNIHTBanalysis) {Reduce(`+`, MBAR_dataforNIHTBanalysis) / ncol(MBAR_dataforNIHTBanalysis)}
MBAR_dataforNIHTBanalysis$ef_average <- rowwise_mean(MBAR_dataforNIHTBanalysis[, c("trailsb_z", "matrix_z","stroop_z")])

#ef toolbox measures
MBAR_dataforNIHTBanalysis$dccs_z <- scale(MBAR_dataforNIHTBanalysis$tb_cardsort_comp_score, center = TRUE, scale = TRUE)
MBAR_dataforNIHTBanalysis$flanker_z <- scale(MBAR_dataforNIHTBanalysis$tb_flanker_comp_score, center = TRUE, scale = TRUE)
rowwise_mean <- function(MBAR_dataforNIHTBanalysis) {Reduce(`+`, MBAR_dataforNIHTBanalysis) / ncol(MBAR_dataforNIHTBanalysis)}
MBAR_dataforNIHTBanalysis$eftb_average <- rowwise_mean(MBAR_dataforNIHTBanalysis[, c("dccs_z", "flanker_z")])

#convergent validity
#correlations of gold standard measures and within NIH TB domain test

#discriminant validity
#correlations of gold standard measures and outsidelibrary(corrplot) NIH TB domain test
library(corrplot)
DV<-MBAR_dataforNIHTBanalysis[c("mem_average", "wm_average", "speed_average", "ef_average", "simil_wais_raw", "tb_picseq_comp_score", "tb_list_rawscore", "tb_pttrncomp_comp_score", "tb_cardsort_comp_score", "tb_flanker_comp_score", "tb_picturevocab_theta", "tb_oralread_theta")]
M=cor(DV)
corrplot(M, method = 'ellipse', type = 'lower', cl.pos = 'r')%>% 
  corrRect(name = c('mem_average', 'simil_wais_raw'))
library(openxlsx)
write.xlsx(M, "c:/Factor_correlations.xlsx")