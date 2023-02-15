This README offers some guidance on how to work with the included docker container 
which was created with the R package rang on __DATE__

# Installing docker

The installation depends on the OS you are running
Follow the steps outlined in the official docs: https://docs.docker.com/engine/install/
For Windows, you can also check out https://learn.microsoft.com/en-us/virtualization/windowscontainers/quick-start/set-up-environment

# Run the docker container
Open a command prompt window (In Windows this could be the built-in command prompt, PowerShell, or Windows Terminal)
navigate to output_dir in the command prompt: `cd __OUTPUT__`

Note that all docker commands below might need sudo rights on Linux distros
If you want to run docker without sudo, follow the steps outlined here: https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user

The execution depends on what image you chose when dockerizing (image used here: __IMAGE__)

## image!="rstudio"
docker build -t rang .
docker run --rm --name "rangtest" -ti rang

You can change the --name paramater to something more suitable for your container
An R command prompt should open. To confirm that you have succesfully gone back in time, 
use the `sessionInfo()` command 

## image = "rstudio"
docker build -t rang .
docker run -p 8787:8787 -e PASSWORD=abc123 --rm --name "rangtest" -ti rang

You can change the --name paramater to something more suitable for your container
when the building is finished, open a browser and go to localhost:8787
The default username is rstudio, password as specified above (in this case abc123).
To confirm that you have succesfully gone back in time, 
use the `sessionInfo()` command  

## Need more information about rang?
Check the vignette included in the package

## Need more information about docker?

- Consult the docker documentation: https://docs.docker.com/get-started/
- Consult this docker tutorial: https://docker-curriculum.com/ 
- Using Windows? Checkout Microsofts documentation: https://learn.microsoft.com/en-us/virtualization/windowscontainers/

## Issues?

If you are unable to run this docker container please file an issue at https://github.com/chainsawriot/rang/issues
containing the following information:

- The `resolve()` command you executed in R
- The `dockerize()` command you executed in R 
- The error message shown in your command prompt
