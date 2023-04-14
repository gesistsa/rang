.generate_debian_eol_apptainer_content <- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny",
                                                     post_installation_steps = NULL,
                                                     rel_dir = "",
                                                     copy_all = FALSE) {
  rang_path <- file.path(rel_dir, "rang.R")
  cache_path <- file.path(rel_dir, "cache")
  compile_path <- file.path(rel_dir, "compile_r.sh")
  environment_vars <- c("export TZ=UTC", paste0("export COMPILE_PATH=", compile_path), paste0("export RANG_PATH=", rang_path))
  apptainer_content <- list(
    BOOTSTRAP = "Bootstrap: docker",
    FROM = c(paste0("From: debian/eol:", debian_version)),
    ENV_section = "\n%environment\n",
    ENV = environment_vars,
    FILES_section = "\n%files\n",
    FILES = c(paste0("rang.R ", rang_path), paste0("compile_r.sh ", compile_path)),
    POST_section = "\n%post\n",
    POST = c(environment_vars,
             "ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev -y",
             sysreqs_cmd),
    STARTSCRIPT_section = "\n%startscript\n",
    STARTSCRIPT = c("exec R \"${@}\""))
  if (!is.na(lib)) {
    apptainer_content$POST <- append(apptainer_content$POST, paste0("mkdir ", lib, " && bash $COMPILE_PATH ", r_version))
  } else {
    apptainer_content$POST <- append(apptainer_content$POST, paste0("bash $COMPILE_PATH ", r_version))
  }
  if (isTRUE(cache)) {
    apptainer_content$FILES <- append(apptainer_content$FILES,
                                        c(paste0("cache/rpkgs ", file.path(cache_path, "rpkgs")),
                                          paste0("cache/rsrc ", file.path(cache_path, "rsrc"))))
    apptainer_content$FROM <- c("From: scratch", paste0("ADD ", file.path(rel_dir, "cache/debian/rootfs.tar.xz"), " /"))
    apptainer_content$ENV <- append(apptainer_content$ENV, paste0("CACHE_PATH=", cache_path))
  }
  apptainer_content$POST <- append(apptainer_content$POST, post_installation_steps)
  if (isTRUE(copy_all)) {
    apptainer_content$FILES <- c(". /")
  }
  return(apptainer_content)
}

.generate_rocker_apptainer_content <- function(r_version, lib, sysreqs_cmd, cache, image,
                                                 post_installation_steps = NULL,
                                                 rel_dir = "",
                                                 copy_all = FALSE) {
  rang_path <- file.path(rel_dir, "rang.R")
  cache_path <- file.path(rel_dir, "cache")
  apptainer_content <- list(
    BOOTSTRAP = "Bootstrap: docker",
    FROM = c(paste0("From: rocker/", image, ":", r_version)),
    ENV_section = "\n%environment\n",
    ENV = c(paste0("export RANG_PATH=", rang_path)),
    FILES_section = "\n%files\n",
    FILES = c(paste0("rang.R ", rang_path)),
    POST_section = "\n%post\n",
    POST = sysreqs_cmd,
    STARTSCRIPT_section = "\n%startscript\n",
    STARTSCRIPT = c("exec R \"${@}\""))
  if (!is.na(lib)) {
    apptainer_content$POST <- append(apptainer_content$POST, paste0("mkdir ", lib, " && Rscript $RANG_PATH"))
  } else {
    apptainer_content$POST <- append(apptainer_content$POST, "Rscript $RANG_PATH")
  }
  if (isTRUE(cache)) {
    apptainer_content$FILES <- append(apptainer_content$FILES, paste0("cache ", cache_path))
    apptainer_content$ENV <- append(apptainer_content$ENV, paste0("export CACHE_PATH ", cache_path))
  }
  if (image == "rstudio") {
    apptainer_content$STARTSCRIPT <- c("exec /init \"$@\"")
  }
  apptainer_content$POST <- append(apptainer_content$POST, post_installation_steps)
  if (isTRUE(copy_all)) {
    apptainer_content$FILES <- c(". /")
  }
  return(apptainer_content)
}

