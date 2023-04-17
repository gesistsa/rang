This README offers some guidance on how to work with the included Apptainer/Singularity container 
which was created with the R package rang on __DATE__

# Installing Apptainer/Singularity

Apptained and Singularity are almost the same thing.
Apptainer is the new name for Singularity, (for more info see
https://apptainer.org/news/community-announcement-20211130/ ),
but there is stil aslo a Singularity Community Edition ( https://sylabs.io/singularity/ ). So far the work in the same way and the containers are identical.

The Apptainer installation depends on the OS you are running
Follow the steps outlined in the official docs:
- for Linux: https://apptainer.org/docs/admin/main/installation.html#installation-on-linux
- for macOS: https://apptainer.org/docs/admin/main/installation.html#mac
- for Windows: https://apptainer.org/docs/admin/main/installation.html#windows

On Ubuntu, the easiest way to install Apptainer would be to:
sudo apt update
sudo apt install -y software-properties-common
sudo add-apt-repository -y ppa:apptainer/ppa
sudo apt update
sudo apt install -y apptainer

For SingularityCE:
- Linux: https://docs.sylabs.io/guides/latest/admin-guide/installation.html#installation-on-linux
- macOS: https://docs.sylabs.io/guides/latest/admin-guide/installation.html#mac
- Windows: https://docs.sylabs.io/guides/latest/admin-guide/installation.html#windows


If you have access to HPC, there is a good chance that Singularity or Apptainer is alredy installed there by your administrator. On your HPC node you can check this:

module avail singularity
module avail apptainer

# Run the docker container
Open a command prompt window (In Windows this could be the built-in command prompt, PowerShell, or Windows Terminal)
navigate to output_dir in the command prompt: `cd __OUTPUT__`

Note that all Singularity commands for buidling containers
might will sudo rights on Linux distros. Since 1.1.0 Apptainer is rootless
(for details see https://apptainer.org/news/apptainer-1-1-0-20220927), so you
can build and run containers with Apptainer without root. With Sngularity
you can only run containers without root access.

The execution depends on what image you chose when dockerizing (image used here: __IMAGE__)


## image!="rstudio"

apptainer build container.sif container.def
apptainer run container.sif R

or

sudo singularity build container.sif container.def
singularity run container.sif R

An R command prompt should open. To confirm that you have succesfully gone back in time, use the `sessionInfo()` command. To stop container, just quit R.

## image = "rstudio"

Build a container:

apptainer build container.sif container.def

or

sudo singularity build container.sif container.def

For running RStudio IDE in Apptainer/Singulairty container, some writable folders and a config file have to be created locally:

mkdir -p run var-lib-rstudio-server
printf 'provider=sqlite\ndirectory=/var/lib/rstudio-server\n' > database.conf

After that, you can run the container:

apptainer exec \
    --env PASSWORD='set_your_password' \
    --bind run:/run,var-lib-rstudio-server:/var/lib/rstudio-server,database.conf:/etc/rstudio/database.conf \
    container.sif \
    /usr/lib/rstudio-server/bin/rserver \
    --auth-none=0 --auth-pam-helper-path=pam-helper \
    --server-user=$(whoami)

or

singularity exec \
    --env PASSWORD='set_your_password' \
    --bind run:/run,var-lib-rstudio-server:/var/lib/rstudio-server,database.conf:/etc/rstudio/database.conf \
    container.sif \
    /usr/lib/rstudio-server/bin/rserver \
    --auth-none=0 --auth-pam-helper-path=pam-helper \
    --server-user=$(whoami)

The default port is 8787, you can also change the port by adding `--www-port=` in the end of the line above (e.g. `--www-port=8080`).

Now open a browser and go to localhost:8787.
The default username is your local username, password as specified above (in this case 'set_your_password').


## Need more information about rang?
Check the vignette included in the package

## Need more information about Apptainer/Singularity?

- Consult the Apptainer documentation: https://apptainer.org/docs
- Consult the Singularity documentation: https://sylabs.io/docs/
- Read Rocker Project guide on running Rocker in Singularity
and example SLURM script for running in HPC environment: https://rocker-project.org/use/singularity.html

## Issues?

If you are unable to run this Apptainer/Singularity container please file an issue at https://github.com/chainsawriot/rang/issues
containing the following information:

- The `resolve()` command you executed in R
- The `apptainerize()` command you executed in R 
- The error message shown in your command prompt
