app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(sidebarCollapsed = FALSE)
app$uploadFile(file = c("All_topTableAll.csv", "TOXA_HEGU_MA0191_AllChip_pData.csv", "TOXA_HEGU_MA0191_AllChip_WorkingSet.csv"))
# Input 'new_data_rows_current' was set, but doesn't have an input binding.
# Input 'new_data_rows_all' was set, but doesn't have an input binding.
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
# Input 'new_test_rows_current' was set, but doesn't have an input binding.
# Input 'new_test_rows_all' was set, but doesn't have an input binding.
app$setInputs(pval1 = 0.01)
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$setInputs(pval1 = 0.04)
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$setInputs(pval1 = 0.05)
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(pval1 = 0.01)
# Input 'data_summary_rows_current' was set, but doesn't have an input binding.
# Input 'data_summary_rows_all' was set, but doesn't have an input binding.
app$setInputs(side = "PCA")
app$snapshot()
app$setInputs(side = "Venn")
app$setInputs(cont = "(LWT_MCD-LWT_CTRL)-(LKO_MCD-LKO_CTRL)")
app$setInputs(cont = c("LKO_CTRL-LWT_CTRL", "(LWT_MCD-LWT_CTRL)-(LKO_MCD-LKO_CTRL)"))
app$setInputs(vennsize = 2)
app$setInputs(vennsize = 1.6)
app$setInputs(vennsize = 0.3)
app$snapshot()
app$setInputs(side = "Heatmap")
app$setInputs(test = "LKO_CTRL-LWT_CTRL")
app$setInputs(heatm = "click")
app$snapshot()
# Input 'clustering_rows_current' was set, but doesn't have an input binding.
# Input 'clustering_rows_all' was set, but doesn't have an input binding.
# Input 'clustering_rows_current' was set, but doesn't have an input binding.
# Input 'clustering_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(mainhmtabset = "maingo")
app$setInputs(side = "About")
app$setInputs(session = "click")
# Input 'sessinfo_rows_current' was set, but doesn't have an input binding.
# Input 'sessinfo_rows_all' was set, but doesn't have an input binding.
