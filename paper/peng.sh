Rscript -e "require(rang); dockerize(resolve('ptproc', '2004-07-01'), 'pengdocker', cache = TRUE)"
docker build -t pengimg ./pengdocker
docker run -d --rm --name "pengcontainer" -ti pengimg
docker cp peng.R pengcontainer:/peng.R
docker exec pengcontainer R CMD BATCH peng.R
docker exec pengcontainer cat peng.Rout
docker cp pengcontainer:/peng.Rout peng.Rout
docker stop pengcontainer
