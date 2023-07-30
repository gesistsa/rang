.generate_debian_eol_apptainer_content <- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny",
                                                   post_installation_steps = NULL,
                                                   rel_dir = "",
                                                   copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    compile_path <- file.path(rel_dir, "compile_r.sh")
    environment_vars <- c("export TZ=UTC", paste0("export COMPILE_PATH=", compile_path), paste0("export RANG_PATH=", rang_path))
    containerfile_content <- list(
        BOOTSTRAP = "Bootstrap: docker",
        FROM = c(paste0("From: debian/eol:", debian_version)),
        ENV_section = "\n%environment\n",
        ENV = environment_vars,
        FILES_section = "\n%files\n",
        FILES = c(paste0("rang.R ", rang_path), paste0("compile_r.sh ", compile_path)),
        POST_section = "\n%post\n",
        POST = c(
            environment_vars,
            "ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev -y",
            sysreqs_cmd
        ),
        STARTSCRIPT_section = "\n%startscript\n",
        STARTSCRIPT = c("exec R \"${@}\"")
    )
    if (!is.na(lib)) {
        containerfile_content$POST <- append(containerfile_content$POST, paste0("mkdir ", lib, " && bash $COMPILE_PATH ", r_version))
    } else {
        containerfile_content$POST <- append(containerfile_content$POST, paste0("bash $COMPILE_PATH ", r_version))
    }
    if (isTRUE(cache)) {
        containerfile_content$BOOTSTRAP <- "Bootstrap: docker"
        containerfile_content$FROM <- c(paste0("From: debian/eol:", debian_version))
        containerfile_content$FILES <- append(
            containerfile_content$FILES,
            c(
                paste0("cache/rpkgs ", file.path(cache_path, "rpkgs")),
                paste0("cache/rsrc ", file.path(cache_path, "rsrc"))
            )
        )
        containerfile_content$POST <- append(paste0("export CACHE_PATH=", cache_path), containerfile_content$POST)
    }
    containerfile_content$POST <- append(containerfile_content$POST, post_installation_steps)
    if (isTRUE(copy_all)) {
        containerfile_content$FILES <- c(". /")
    }
    return(containerfile_content)
}

.generate_rocker_apptainer_content <- function(r_version, lib, sysreqs_cmd, cache, image,
                                               post_installation_steps = NULL,
                                               rel_dir = "",
                                               copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    environment_vars <- c(paste0("export RANG_PATH=", rang_path))
    containerfile_content <- list(
        BOOTSTRAP = "Bootstrap: docker",
        FROM = c(paste0("From: rocker/", image, ":", r_version)),
        ENV_section = "\n%environment\n",
        ENV = c(environment_vars, "export RPORT=${RPORT:-8787}",
                "export USER=$(whoami)", "export PASSWORD=${PASSWORD:-set_your_password}"),
        FILES_section = "\n%files\n",
        FILES = c(paste0("rang.R ", rang_path)),
        POST_section = "\n%post\n",
        POST = c(environment_vars, sysreqs_cmd),
        STARTSCRIPT_section = "\n%startscript\n",
        STARTSCRIPT = c("exec R \"${@}\"")
    )
    if (!is.na(lib)) {
        containerfile_content$POST <- append(containerfile_content$POST, paste0("mkdir ", lib, " && Rscript $RANG_PATH"))
    } else {
        containerfile_content$POST <- append(containerfile_content$POST, "Rscript $RANG_PATH")
    }
    if (isTRUE(cache)) {
        containerfile_content$FILES <- append(containerfile_content$FILES, paste0("cache ", cache_path))
        containerfile_content$POST <- append(paste0("export CACHE_PATH=", cache_path), containerfile_content$POST)
    }
    if (image == "rstudio") {
        containerfile_content$STARTSCRIPT <- c("exec /usr/lib/rstudio-server/bin/rserver \\\
    --auth-none=0 --auth-pam-helper-path=pam-helper \\\
    --server-user=${USER} --www-port=${RPORT}")
    }
    containerfile_content$POST <- append(containerfile_content$POST, post_installation_steps)
    if (isTRUE(copy_all)) {
        containerfile_content$FILES <- c(". /")
    }
    return(containerfile_content)
}
