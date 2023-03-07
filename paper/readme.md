## Readme

You'll need to clone the following repo here.

```sh
git clone https://github.com/Joe-Hilgard/vvg-2d4d.git
```

As well as get the data and code shared by Oser et al.

```r
require(osfr)
osf_retrieve_file("https://osf.io/y7cg5") %>% osf_download()
unzip("meta-analysis replication files.zip", exdir = "oser")
```

And install the quarto extension

```sh
quarto install extension quarto-journals/plos
```

