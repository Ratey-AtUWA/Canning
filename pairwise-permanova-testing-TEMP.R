(pairs <- as.data.frame(combn(levels(s26_clr2$TypeRip), 2, simplify = FALSE)))

pr <- pairs[[1]]

res <- lapply(pairs, function(pr) {
  keep <- which(s26_clr2$TypeRip %in% pr)
  Dsub <- as.dist(as.matrix(s26_clr2[,n1:nn])[keep, ])
  set.seed(42)
  a <- adonis2(Dsub ~ s26_clr2$TypeRip[keep], permutations = 999)
  data.frame(
    group1 = pr[1], group2 = pr[2],
    R2 = round(a$R2[1], 3), F = round(a$F[1], 2),
    p_raw = a$`Pr(>F)`[1]
  )
})
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

pairz <- combn(levels(s26_clr2$TypeRip), 2)
row.names(s26_clr2) <- NULL;row.names(s26_clr2) <- paste0(s26_clr2$TypeRip, row.names(s26_clr2))

# pr <- pairz[,1]

pw0 <- apply(pairz,2, FUN=function(pr) {
  keep <- which(s26_clr2$TypeRip %in% pr)
  Dsub <- as.matrix(s26_clr2[keep,n1:nn])
  set.seed(31416)
  a <- adonis2(Dsub ~ s26_clr2$TypeRip[keep], permutations=999, method="euclid")
  c(group1=pr[1], group2=pr[2], Rsq=round(a$R2[1], 3), 
    Fstat=round(a$F[1], 2), p_raw=a$`Pr(>F)`[1])
})
pw0 <- as.data.frame(t(pw0))
pw0[,3:5] <- apply(pw0[,3:5],2,as.numeric)
# colnames(pw0) <- c("group1","group2","Rsq","F","p_raw")
print(pw0, digits=2)
