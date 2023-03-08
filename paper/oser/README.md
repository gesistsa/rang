# Executable compendium of Oser et al. (2022)

This is enhanced from the [original data and code](http://doi.org/10.17605/OSF.IO/AF5DR) shared by Oser et al. (2022) for their article "How Political Efficacy Relates to Online and Offline Political Participation: A Multilevel Meta-analysis" in *Political Communication* ([doi](https://doi.org/10.1080/10584609.2022.2086329)).

In order to run this executable compendium, the following components are needed.

* Docker (Please install it with [rootless mode](https://docs.docker.com/engine/security/rootless/))
* Make (on a standard Ubuntu machine: `sudo apt install -y make`)
* R (please follow [this guide](https://cran.r-project.org/bin/linux/ubuntu/) to install the latest version on a standard Ubuntu machine)
* the R package `rang` (`install.packages('rang')`)

All the instructions to execute the analysis in batch is available in the provided `Makefile`.

* `make resolve`: scan the code provided by Oser et al. for all R packages used, resolve their dependency structure, generate `Dockerfile`, and cache all R packages.
* `make build`: build the Docker image
* `make render`: render the RMarkdown file provided by Oser et al. inside a container and obtain the rendered HTML file back
* `make export`: export the Docker image
* `make rebuild`: rebuild the exported Docker image
* `make all`: running `make resolve`, `make build`, and `make render`

The complete version of the executable compendium is available here: https://doi.org/10.5281/zenodo.7708417

# Disclaimer

This executable compendium was created by the authors of `rang`. However, the data and code are shared by Oser et al. The authors of `rang` claims no ownership to the data and code shared by Oser et al.
