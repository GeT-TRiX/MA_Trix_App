# Source
# https://stackoverflow.com/questions/35632586/merging-csv-table-rows-according-to-the-first-column-in-bash?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

BEGIN { FS=OFS="," }
NR==1 { hdrKey = $1 }
{
    for (i=1;i<=NF;i++) {
        vals[$1][ARGIND][i] = $i (FNR>1?"":ARGIND)
    }
}
END {
    prtRow(hdrKey)

    PROCINFO["sorted_in"] = "@ind_str_asc"
    for (date in vals) {
        prtRow(date)
    }
}
function prtRow(key,    val,fileNr,fieldNr) {
    printf "%s", key
    for (fileNr=1;fileNr<=ARGIND;fileNr++) {
        for (fieldNr=2;fieldNr<=NF;fieldNr++) {
            val = vals[key][fileNr][fieldNr]
            printf "%s%s", OFS, (val?val:0)
        }
    }
    print ""
    delete vals[key]
}
