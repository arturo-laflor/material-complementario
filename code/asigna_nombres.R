asigna_nombres=function(dataSet){
  colnames(dataSet)<- c("EMAIL",
                      "DD1","DD2","DD3","DD4","DD5",	"DD6",	"DD7",	"DD8",	
                      "SQ1",	"SQ2",	"SQ3",	"SQ4",	"SQ5a",	"SQ5b",	"SQ5c",	"SQ5d",	"SQ5e",	
                      "SQ5f","SQ5g",	"SQ5h",	"SQ5i",	"SQ5j",	"SQ6",	"SQ7",	"SQ8",	"SQ9",	
                      "SH1",	"SH2",	"SH3",	"SH4",	"SH5",	"SH6", "SH7",	"SH8",	"SH9",	"SH10",
                      "SH11",	"SH12",	"SH13",	"SH14",	"SH15",	"SH16",	"SH17",	"SH18",	"SH19",	"SH20",	"SH21")
  return(dataSet)
}

