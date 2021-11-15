#Confirmatory factor analysis of NIH TB in MBAR sample
#CFA with standardized factors
cfa.1 <-'#factor loadings
          global_cognition =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score + lns_raw + mmdsbt + tb_list_rawscore + coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta + tb_flanker_comp_score
         '

cfa.2a <- '#factor loadings
          fluid =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score + lns_raw + mmdsbt + tb_list_rawscore + coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
         crystallized =~ simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta
          '
cfa.2b <- 'memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score + lns_raw + mmdsbt + tb_list_rawscore 
          nonmemory =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta + tb_flanker_comp_score
          '
cfa.3a <- 'language =~ simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta
          memory_wm =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score + lns_raw + mmdsbt + tb_list_rawscore 
          executive_speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
          '
cfa.3b <- 'language =~ simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score 
          executive_speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_list_rawscore + lns_raw + mmdsbt+ tb_flanker_comp_score
          '
cfa.4a <- 'language =~ simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score 
          working_memory =~ tb_list_rawscore + lns_raw + mmdsbt
          executive_speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
          '
cfa.4b <- 'vocab =~ simil_wais_raw + tb_picturevocab_theta
          reading =~ tb_oralread_theta
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score 
          executive_speed_wm =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_list_rawscore + lns_raw + mmdsbt + tb_flanker_comp_score
          '
cfa.4c <- 'vocab =~ simil_wais_raw + tb_picturevocab_theta
          reading =~ tb_oralread_theta
          memory_wm =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score +tb_list_rawscore + lns_raw + mmdsbt
          executive_speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
          '
cfa.5a<- 'language =~ simil_wais_raw + tb_picturevocab_theta + tb_oralread_theta
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score 
          working_memory =~ tb_list_rawscore + lns_raw + mmdsbt
          speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score
          executive =~ trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
          '
cfa.5b <- 'vocab =~ simil_wais_raw + tb_picturevocab_theta
          reading =~ tb_oralread_theta
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score 
          working_memory =~ tb_list_rawscore + lns_raw + mmdsbt
          executive_speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score + trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
            '

cfa.6a <-'#factor loadings
          memory =~ cvlt_lgdelay_free_raw + score_benson_del + tb_picseq_comp_score
          working_memory =~ lns_raw + mmdsbt + tb_list_rawscore
          speed =~ coding_wais + symbol_raw + tb_pttrncomp_comp_score
          executive_functioning =~ trails_b_secondsperline + matrix_raw + stroop_interference + tb_cardsort_comp_score + tb_flanker_comp_score
          vocab =~ simil_wais_raw + tb_picturevocab_theta 
          reading =~ tb_oralread_theta
         '
fit.1 <- cfa(cfa.1, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.2a <- cfa(cfa.2a, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.2b <- cfa(cfa.2b, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.3a <- cfa(cfa.3a, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.3b <- cfa(cfa.3b, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.4a <- cfa(cfa.4a, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.4b <- cfa(cfa.4b, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.4c <- cfa(cfa.4c, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.5a <- cfa(cfa.5a, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.5b <- cfa(cfa.5b, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)
fit.6a <- cfa(cfa.6a, data=MBAR_dataforNIHTBanalysis, meanstructure=TRUE, std.lv=TRUE)

summary(fit.1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.1, sort.=TRUE, minimum.value=10)

summary(fit.2a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.2a, sort.=TRUE, minimum.value=10)

summary(fit.2b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.2b, sort.=TRUE, minimum.value=10)

summary(fit.3a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.3a, sort.=TRUE, minimum.value=10)

summary(fit.3b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.3b, sort.=TRUE, minimum.value=10)

summary(fit.4a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.4a, sort.=TRUE, minimum.value=10)

summary(fit.4b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.4b, sort.=TRUE, minimum.value=10)

summary(fit.4c, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.4c, sort.=TRUE, minimum.value=10)

summary(fit.5a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.5a, sort.=TRUE, minimum.value=10)

summary(fit.5b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.5b, sort.=TRUE, minimum.value=10)

summary(fit.6a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
modindices(fit.6a, sort.=TRUE, minimum.value=10)