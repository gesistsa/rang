## .insert_cache_dir <- function(dockerfile_content) {
##     rang_line <- which(dockerfile_content == "RUN Rscript rang.R")
##     c(dockerfile_content[1:(rang_line - 1)],
##       "COPY cache ./cache",
##       dockerfile_content[rang_line:length(dockerfile_content)])
## }

.insert_materials_dir <- function(dockerfile_content) {
    dockerfile_content$COPY <- append(dockerfile_content$COPY, "COPY materials/ ./materials/")
    dockerfile_content
}

.generate_debian_eol_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny",
                                                    post_installation_steps = NULL) {
    dockerfile_content <- list(
        FROM = c(paste0("FROM debian/eol:", debian_version)),
        chores = c("ENV TZ UTC",
                   "RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev  -y"),
        COPY = c("COPY rang.R ./rang.R", "COPY compile_r.sh ./compile_r.sh"),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN mkdir ", lib, " && bash compile_r.sh ", r_version))
    } else {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN bash compile_r.sh ", r_version))
    }
    if (isTRUE(cache)) {
        dockerfile_content$COPY <- append(dockerfile_content$COPY,
                                          c("COPY cache/rpkgs ./cache/rpkgs", "COPY cache/rsrc ./cache/rsrc"))
        dockerfile_content$FROM <- c("FROM scratch", "ADD cache/debian/rootfs.tar.xz /")
    }
    dockerfile_content$RUN <- append(dockerfile_content$RUN, post_installation_steps)
    return(dockerfile_content)
}

.generate_rocker_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, image,
                                                post_installation_steps = NULL) {
    dockerfile_content <- list(
        FROM = c(paste0("FROM rocker/", image, ":", r_version)),
        chores = NULL,
        COPY = c("COPY rang.R ./rang.R"),
        RUN = c(paste("RUN", sysreqs_cmd)),
        CMD = c("CMD [\"R\"]"))
    if (!is.na(lib)) {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, paste0("RUN mkdir ", lib, " && Rscript rang.R"))
    } else {
        dockerfile_content$RUN <- append(dockerfile_content$RUN, "Rscript rang.R")
    }
    if (isTRUE(cache)) {
        dockerfile_content$COPY <- append(dockerfile_content$COPY, "COPY cache ./cache")
    }
    if (image == "rstudio") {
        dockerfile_content$CMD <- c("EXPOSE 8787", "CMD [\"/init\"]")
    }
    dockerfile_content$RUN <- append(dockerfile_content$RUN, post_installation_steps)
    return(dockerfile_content)
}

.write_dockerfile <- function(dockerfile_content, path) {
    content <- c(dockerfile_content$FROM, dockerfile_content$chores,
                 dockerfile_content$COPY, dockerfile_content$RUN,
                 dockerfile_content$CMD)
    writeLines(content, path)
}
