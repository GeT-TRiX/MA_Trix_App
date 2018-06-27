app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(sidebarCollapsed = FALSE)
app$uploadFile(file = c("All_topTableAll.csv", "TOXA_HEGU_MA0191_AllChip_pData.csv", "TOXA_HEGU_MA0191_AllChip_WorkingSet.csv"))
# Input 'new_group_rows_current' was set, but doesn't have an input binding.
# Input 'new_group_rows_all' was set, but doesn't have an input binding.
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
# Input 'new_test_rows_current' was set, but doesn't have an input binding.
# Input 'new_test_rows_all' was set, but doesn't have an input binding.
app$setInputs(method = "None")
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$setInputs(pval1 = 0.01)
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(side = "PCA")
app$setInputs(indivpca = c("LKO_CTRL", "LKO_MCD ", "LWT_CTRL"))
app$setInputs(indivpca = c("LKO_CTRL", "LKO_MCD ", "LWT_CTRL", "LWT_MCD "))
app$snapshot()
app$setInputs(indivpca = c("LKO_CTRL", "LKO_MCD ", "LWT_CTRL"))
app$snapshot()
app$setInputs(colpca_1 = "#7570B3")
app$snapshot()
app$snapshot()
app$setInputs(side = "Venn")
app$setInputs(allCont = "click")
app$setInputs(dispvenn = "genes")
app$setInputs(intscol = "LWT_MCD-LWT_CTRL")
app$snapshot()
app$setInputs(side = "Heatmap")
app$setInputs(test = "LKO_CTRL-LWT_CTRL")
app$setInputs(allTests = "click")
app$setInputs(fc = 2)
app$setInputs(pval = 0.04)
app$setInputs(heatm = "click")
# Input 'clustering_rows_current' was set, but doesn't have an input binding.
# Input 'clustering_rows_all' was set, but doesn't have an input binding.
# Input 'totalgenbyc_rows_current' was set, but doesn't have an input binding.
# Input 'totalgenbyc_rows_all' was set, but doesn't have an input binding.
app$snapshot()
