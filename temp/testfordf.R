test.df  <- data.frame(row.names = colnames(adj[,-1]))
View(test.df)
test.df  <- data.frame(x = 1:50)

test.df = mutate( test.df, `p-value = 0.01` = case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)

)

View(select(test.df,`p-value = 0.01`))



library(foreach)
library(doParallel)

Z <- matrix(rnorm(10000), ncol = 100)
test = foreach(i = iter(adj['adj.P.Val_LKO_CTRL.LWT_CTRL'], by = "col"), .combine = c) %dopar%  {test = (i<pv)} %>%
  as.data.frame()
View(test)


test = as.data.frame(test)
colnames(test) = c("test")
View(test)

filter(test, TRUE)

test = (i = iter(adj['adj.P.Val_LKO_CTRL.LWT_CTRL'], by ="col")) %>%
  foreach(.combine = c) %dopar%
  test = i < pv

grp1 = foreach(i = iter(adj['adj.P.Val_LKO_CTRL.LWT_CTRL'], by = "col"), .combine = c) %dopar%  
  (test= {i < pv}) %>%
  as.data.frame() #%>%
  #filter(. == T) %>%
  #nrow()
View(grp1)

df <- data.frame(matrix(ncol = 3, nrow = 2))
x <- c("name", "age", "gender")
colnames(df) <- x
View(df)

dtsign = data.frame(matrix(ncol=2,nrow = length(adj[,-1])))
y <- c("pvalue(0.01)","pvalue(0.05)")
colnames(dtsign) <- y
rownames(dtsign) <- colnames(adj[,-1])
dtsign$`pvalue(0.01)`[1] = 5
View(dtsign)
c