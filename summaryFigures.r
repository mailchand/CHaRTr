whatToRun = "cs2, slow, 400, 10000"
load(paste(whatToRun,"_figure"));
p11 = p2;

whatToRun = "cs2, fast, 400, 10000"
load(paste(whatToRun,"_figure"));
p21 = p2;

whatToRun = "cs2, fast, 200, 10000"
load(paste(whatToRun,"_figure"));
p31 = p2;

whatToRun = "cs2, fast, 200, 5000"
load(paste(whatToRun,"_figure"));
p41 = p2;


grid.arrange(p11,p21,p31,p41,nrow=2, ncol=2)
