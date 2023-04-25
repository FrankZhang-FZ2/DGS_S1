
DGS_neg <- dict %>% 
  filter(scale == "DGS")%>%
  filter (keying == "-1") %>% 
  pull(variable)

DGS_fit_1 <- DGS %>%
  select(c(DGS_neg)) %>%
  select(!c())

#Kaiser Criterion
ev<- eigen(cor(DGS_fit_1))
ev$values
sum(ev$values > 1)
sum(ev$values > .7)

#scree plot and parallel analysis
scree(DGS_fit_1, pc=FALSE)
fa.parallel(DGS_fit_1,
            fm="ml",
            fa="fa")


EFA_fit_1 <- fa(DGS_fit_1,
                nfactors = 5,
                rotate = "oblimin",
                fm="ml")
EFA_fit_1 <- fa.sort(EFA_fit_1)
EFA_fit_1
print(EFA_fit_1$loadings, cutoff = 0.3)
print(EFA_fit_1$loadings)


###normal coded

DGS_pos <- dict %>% 
  filter(scale == "DGS")%>%
  filter (keying == "1") %>% 
  pull(variable)

DGS_fit_1 <- DGS %>%
  select(c(DGS_pos))

#Kaiser Criterion
ev<- eigen(cor(DGS_fit_1))
ev$values
sum(ev$values > 1)
sum(ev$values > .7)

#scree plot and parallel analysis
scree(DGS_fit_1, pc=FALSE)
fa.parallel(DGS_fit_1,
            fm="ml",
            fa="fa")


EFA_fit_1 <- fa(DGS_fit_1,
                nfactors = 4,
                rotate = "oblimin",
                fm="ml")
EFA_fit_1 <- fa.sort(EFA_fit_1)
EFA_fit_1
print(EFA_fit_1$loadings, cutoff = 0.3)
print(EFA_fit_1$loadings)