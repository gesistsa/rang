output_file=reproduced.html
r_cmd = "rmarkdown::render('materials/README.Rmd', \
output_file = '${output_file}')"
handle=oser
local_file=${handle}_README.html

.PHONY: all resolve build render export rebuild

all: resolve build render
	echo "finished"
resolve:
	Rscript ${handle}.R
build: ${handle}docker
	docker build -t ${handle}img ${handle}docker
render:
	docker run -d --rm --name "${handle}container" -ti ${handle}img
	docker exec ${handle}container Rscript -e ${r_cmd}
	docker cp ${handle}container:/materials/${output_file} ${local_file}
	docker stop ${handle}container
export:
	docker save ${handle}img | gzip > ${handle}img.tar.gz
rebuild: ${handle}img.tar.gz
	docker load < ${handle}img.tar.gz
