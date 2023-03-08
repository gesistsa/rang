require(rang)

## all packages used in the project
cran_pkgs <- as_pkgrefs("meta-analysis") ## dmetar is an undeclared github package: MathiasHarrer/dmetar

cran_pkgs[cran_pkgs == "cran::dmetar"] <- "MathiasHarrer/dmetar"
x <- resolve(cran_pkgs, "2021-08-11", verbose = TRUE)
##print(x, all_pkgs = TRUE)
dockerize(x, "oserdocker", materials_dir = "meta-analysis", cache = TRUE)
