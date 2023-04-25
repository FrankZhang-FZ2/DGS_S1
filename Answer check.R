#CFA test

Empathy <- '
F1 =~ DGS_29+DGS_52+DGS_27+DGS_9+DGS_51
DGS_9 ~~ DGS_51
'
temp <- cfa(
  model = Empathy,
  data = DGS,
  std.lv = TRUE)

summary(temp,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures=TRUE)

semPaths(temp,
         whatLabels = "std",
         what = "std",
         layout = "tree2",
         edge.label.cex = 1)

###
Universality <- '
F1 =~ DGS_38+DGS_71+DGS_70+DGS_45+DGS_39+DGS_6+DGS_67+DGS_25

'
temp <- cfa(
  model = Universality,
  data = DGS,
  std.lv = TRUE)

summary(temp,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures=TRUE)

semPaths(temp,
         whatLabels = "std",
         what = "std",
         layout = "tree",
         edge.label.cex = 1)

###
Effective_altruism <- '
F1 =~ DGS_2+DGS_11+DGS_13+DGS_73+DGS_28+DGS_68+DGS_35+DGS_48+DGS_20+DGS_64+DGS_24+DGS_59
N =~ DGS_2+DGS_11+DGS_13+DGS_73
'
temp <- cfa(
  model = Effective_altruism,
  data = DGS,
  std.lv = TRUE)

summary(temp,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measures=TRUE)

semPaths(temp,
         whatLabels = "std",
         what = "std",
         layout = "tree",
         edge.label.cex = 1)
    
