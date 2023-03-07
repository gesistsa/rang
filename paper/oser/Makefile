rmd = "rmarkdown::render('materials/README.Rmd', output_file = 'reproduced.html')"
resolve:
	Rscript oser.R
build: oser_docker
	docker build -t oserimg oser_docker
render:
	docker run -d --rm --name "osercontainer" -ti oserimg
	docker exec osercontainer Rscript -e ${rmd}
	docker cp osercontainer:/materials/reproduced.html oser_README.html
	docker stop osercontainer
export:
	docker save oserimg | gzip > oserimg.tar.gz
rebuild: oserimg.tar.gz
	docker load < oserimg.tar.gz
all: resolve build render
	echo "finished"
