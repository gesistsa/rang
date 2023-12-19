## for normalizing post_installation_steps
.normalize_docker_steps <- function(steps) {
    fx <- function(step) {
        docker_regex <- "^#|^ADD |^COPY |^ENV |^EXPOSE |^FROM |^LABEL |^STOPSIGNAL |^USER |^VOLUME |^WORKDIR |^ONBUILD |^RUN |^CMD |^ENTRYPOINT |^ARG |^HEALTHCHECK |^SHELL "
        splitted_step <- strsplit(step, "\n")[[1]]
        docker_line_lgl <- grepl(docker_regex, splitted_step)
        splitted_step[!docker_line_lgl] <- paste0("RUN ", splitted_step[!docker_line_lgl])
        paste0(splitted_step, collapse = "\n")
    }
    vapply(steps, fx, character(1), USE.NAMES = FALSE)
}

.generate_debian_eol_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny",
                                                    post_installation_steps = NULL,
                                                    rel_dir = "",
                                                    copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    compile_path <- file.path(rel_dir, "compile_r.sh")
    containerfile_content <- list(
        FROM = c(paste0("FROM debian/eol:", debian_version)),
        ENV = c("ENV TZ UTC",
                   "RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev  -y", paste0("ENV RANG_PATH ", rang_path), paste0("ENV COMPILE_PATH ", compile_path)),
        COPY = c(paste0("COPY rang.R ", rang_path), paste0("COPY compile_r.sh ", compile_path)),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        containerfile_content$RUN <- append(containerfile_content$RUN, paste0("RUN mkdir ", lib, " && bash $COMPILE_PATH ", r_version))
    } else {
        containerfile_content$RUN <- append(containerfile_content$RUN, paste0("RUN bash $COMPILE_PATH ", r_version))
    }
    if (isTRUE(cache)) {
        containerfile_content$COPY <- append(containerfile_content$COPY,
                                          c(paste0("COPY cache/rpkgs ", file.path(cache_path, "rpkgs")),
                                            paste0("COPY cache/rsrc ", file.path(cache_path, "rsrc"))))
        containerfile_content$FROM <- c("FROM scratch", paste0("ADD ", file.path(rel_dir, "cache/debian/rootfs.tar.xz"), " /"))
        containerfile_content$ENV <- append(containerfile_content$ENV, paste0("ENV CACHE_PATH ", cache_path))
    }
    containerfile_content$RUN <- append(containerfile_content$RUN, .normalize_docker_steps(post_installation_steps))
    if (isTRUE(copy_all)) {
        containerfile_content$COPY <- c("COPY . /")
    }
    return(containerfile_content)
}

.generate_rocker_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, image,
                                                post_installation_steps = NULL,
                                                rel_dir = "",
                                                copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    containerfile_content <- list(
        FROM = c(paste0("FROM rocker/", image, ":", r_version)),
        ENV = c(paste0("ENV RANG_PATH ", rang_path)),
        COPY = c(paste0("COPY rang.R ", rang_path)),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        containerfile_content$RUN <- append(containerfile_content$RUN, paste0("RUN mkdir ", lib, " && Rscript $RANG_PATH"))
    } else {
        containerfile_content$RUN <- append(containerfile_content$RUN, "RUN Rscript $RANG_PATH")
    }
    if (isTRUE(cache)) {
        containerfile_content$COPY <- append(containerfile_content$COPY, paste0("COPY cache ", cache_path))
        containerfile_content$ENV <- append(containerfile_content$ENV, paste0("ENV CACHE_PATH ", cache_path))
    }
    if (image == "rstudio") {
        containerfile_content$CMD <- c("EXPOSE 8787", "CMD [\"/init\"]")
    }
    containerfile_content$RUN <- append(containerfile_content$RUN, .normalize_docker_steps(post_installation_steps))
    if (isTRUE(copy_all)) {
        containerfile_content$COPY <- c("COPY . /")
    }
    return(containerfile_content)
}

.generate_evercran_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache,
                                                  post_installation_steps = NULL,
                                                  rel_dir = "",
                                                  copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    containerfile_content <- list(
        ## evercran only works with semver
        FROM = c(paste0("FROM ghcr.io/r-hub/evercran/", r_version)),
        ENV = c(paste0("ENV RANG_PATH ", rang_path)),
        COPY = c(paste0("COPY rang.R ", rang_path)),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    run_cmd <- "RUN "
    if (!is.na(lib)) {
        run_cmd <- paste(run_cmd, "mkdir", lib, "&&")
    }
    if (.is_r_version_older_than(r_version, "2.5.0")) {
        run_cmd <- paste(run_cmd, "R --no-save < $RANG_PATH")
    } else {
        run_cmd <- paste(run_cmd, "Rscript $RANG_PATH")
    }
    containerfile_content$RUN <- append(containerfile_content$RUN, run_cmd)
    if (isTRUE(cache)) {
        containerfile_content$COPY <- append(containerfile_content$COPY, paste0("COPY cache ", cache_path))
        containerfile_content$ENV <- append(containerfile_content$ENV, paste0("ENV CACHE_PATH ", cache_path))
    }
    containerfile_content$RUN <- append(containerfile_content$RUN, .normalize_docker_steps(post_installation_steps))
    if (isTRUE(copy_all)) {
        containerfile_content$COPY <- c("COPY . /")
    }
    return(containerfile_content)
}
