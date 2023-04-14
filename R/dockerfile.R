## .insert_cache_dir <- function(dockerfile_content) {
##     rang_line <- which(dockerfile_content == "RUN Rscript rang.R")
##     c(dockerfile_content[1:(rang_line - 1)],
##       "COPY cache ./cache",
##       dockerfile_content[rang_line:length(dockerfile_content)])
## }

.generate_debian_eol_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny",
                                                    post_installation_steps = NULL,
                                                    rel_dir = "",
                                                    copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    compile_path <- file.path(rel_dir, "compile_r.sh")
    dockerfile_content <- list(
        FROM = c(paste0("FROM debian/eol:", debian_version)),
        ENV = c("ENV TZ UTC",
                   "RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev  -y", paste0("ENV RANG_PATH ", rang_path), paste0("ENV COMPILE_PATH ", compile_path)),
        COPY = c(paste0("COPY rang.R ", rang_path), paste0("COPY compile_r.sh ", compile_path)),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN mkdir ", lib, " && bash $COMPILE_PATH ", r_version))
    } else {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN bash $COMPILE_PATH ", r_version))
    }
    if (isTRUE(cache)) {
        dockerfile_content$COPY <- append(dockerfile_content$COPY,
                                          c(paste0("COPY cache/rpkgs ", file.path(cache_path, "rpkgs")),
                                            paste0("COPY cache/rsrc ", file.path(cache_path, "rsrc"))))
        dockerfile_content$FROM <- c("FROM scratch", paste0("ADD ", file.path(rel_dir, "cache/debian/rootfs.tar.xz"), " /"))
        dockerfile_content$ENV <- append(dockerfile_content$ENV, paste0("ENV CACHE_PATH ", cache_path))
    }
    dockerfile_content$RUN <- append(dockerfile_content$RUN, post_installation_steps)
    if (isTRUE(copy_all)) {
        dockerfile_content$COPY <- c("COPY . /")
    }
    return(dockerfile_content)
}

.generate_rocker_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, image,
                                                post_installation_steps = NULL,
                                                rel_dir = "",
                                                copy_all = FALSE) {
    rang_path <- file.path(rel_dir, "rang.R")
    cache_path <- file.path(rel_dir, "cache")
    dockerfile_content <- list(
        FROM = c(paste0("FROM rocker/", image, ":", r_version)),
        ENV = c(paste0("ENV RANG_PATH ", rang_path)),
        COPY = c(paste0("COPY rang.R ", rang_path)),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN mkdir ", lib, " && Rscript $RANG_PATH"))
    } else {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, "RUN Rscript $RANG_PATH")
    }
    if (isTRUE(cache)) {
        dockerfile_content$COPY <- append(dockerfile_content$COPY, paste0("COPY cache ", cache_path))
        dockerfile_content$ENV <- append(dockerfile_content$ENV, paste0("ENV CACHE_PATH ", cache_path))
    }
    if (image == "rstudio") {
        dockerfile_content$CMD <- c("EXPOSE 8787", "CMD [\"/init\"]")
    }
    dockerfile_content$RUN <- append(dockerfile_content$RUN, post_installation_steps)
    if (isTRUE(copy_all)) {
        dockerfile_content$COPY <- c("COPY . /")
    }
    return(dockerfile_content)
}

.write_dockerfile <- function(dockerfile_content, path) {
    content <- unlist(lapply(dockerfile_content, .generate_wrapped_line))
    writeLines(content, path)
}
