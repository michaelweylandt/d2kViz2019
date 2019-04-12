library(tidyverse)
library(clustRviz)
library(gh)
library(gganimate)
library(grid)
library(png)
setwd("~/d2k_viz_contest/")

TIDYVERSE_PACKAGES <- c("tidyverse/dbplyr",
                        "tidyverse/ggplot2",
                        "tidyverse/dplyr",
                        "tidyverse/hms",
                        "tidyverse/stringr",
                        "tidyverse/tidyr",
                        "tidyverse/purrr",
                        "tidyverse/forcats",
                        "tidyverse/rvest",
                        "tidyverse/glue",
                        "tidyverse/tibble",
                        "tidyverse/haven",
                        "tidyverse/readxl",
                        "tidyverse/lubridate",
                        "tidyverse/readr",
                        "tidyverse/magrittr",
                        "tidyverse/tidyselect")
RLIB_PACKAGES <- c("r-lib/usethis",
                   "r-lib/httr",
                   "r-lib/covr",
                   "r-lib/scales",
                   "r-lib/fs",
                   "r-lib/vctrs",
                   "r-lib/remotes",
                   "r-lib/devtools",
                   "r-lib/rlang",
                   "r-lib/testthat")
OTHER_PACKAGES <- c("rstudio/shiny",
                    "rstudio/rmarkdown",
                    "yihui/knitr")

ALL_PACKAGES <- c(TIDYVERSE_PACKAGES,
                  RLIB_PACKAGES,
                  OTHER_PACKAGES)

clone_repo <- function(github_identifier){
    pkg_stub <- str_split(github_identifier, "/")[[1]][2]

    if(dir.exists(pkg_stub)){
        return(0)
    }

    system(paste0("git clone https://github.com/", github_identifier, ".git"))
}

get_contribs <- function(github_identifier){
    pkg <- str_split(github_identifier, "/")[[1]][2]
    shortlog <- system(paste0("git -C ", pkg, " --no-pager shortlog -sne"),
                       intern = TRUE)

    contrib_counts <- sapply(str_split(shortlog, "\t"), function(x) as.integer(x[1]))
    contrib_email  <- str_match(shortlog, "<(.*)>")[,2]

    tibble(count = contrib_counts,
           email = contrib_email,
           package = pkg)
}

if(!file.exists("contributions.rds")){
    CONTRIBUTIONS <- bind_rows(lapply(ALL_PACKAGES, function(pkg){
        clone_repo(pkg);
        get_contribs(pkg)
    }))
    CONTRIBUTIONS <- CONTRIBUTIONS %>% group_by(email, package) %>% summarize(count = sum(count)) %>% ungroup
    saveRDS(CONTRIBUTIONS, "contributions.rds")
}

## FIXME -- Improve de-dups in this data!
CONTRIBUTIONS <- readRDS("contributions.rds")

NAME_MAP <- tribble(
    ~email, ~name, ~github_id,
    "aron@gweep.net", "Aron Atkins", "aronatkins",
    "barb.b.ribeiro@gmail.com" , "Barbara Borges Ribeiro", "bborgesr",
    "bertbelder@gmail.com" , "Bert Belder", "piscisaureus",
    "cjihrig@gmail.com" , "Colin Ihrig", "cjihrig",
    "csardi.gabor@gmail.com" , "Gabor Csardi", "gaborcsardi",
    "diggsb@ohsu.edu", "Brian Diggs", "BrianDiggs",
    "fedor.indutny@gmail.com" , "Fedor Indutny", "indutny",
    "grolemund@gmail.com" , "Garrett Grolemund", "garrettgman",
    "h.wickham@gmail.com" , "Hadley Wickham", "hadley",
    "igorzi@microsoft.com" , "Igor Zinkovsky", "igorzi",
    "info@bnoordhuis.nl" , "Ben Noordhuis", "bnoordhuis",
    "irisson@normalesup.org" , "Jean-Olivier Irisson", "jiho",
    "james.f.hester@gmail.com" , "Jim Hester", "jimhester",
    "javierluraschi@hotmail.com" , "Javier Luraschi", "javierluraschi",
    "jbarboza@ca.ibm.com", "John Barboza", "jBarz",
    "jeff.allen@trestletechnology.net", "Jeff Allen", "trestletech",
    "jeffrey.arnold@gmail.com" , "Jeffrey Arnold", "jrnold",
    "jenny.f.bryan@gmail.com" , "Jenny Bryan", "jennybc",
    "jenny@stat.ubc.ca" , "Jenny Bryan", "jennybc",
    "jeroenooms@gmail.com", "Jeroen Ooms", "jeroen",
    "jj.allaire@gmail.com" , "JJ Allaire", "jjallaire",
    "jj@rstudio.com" , "JJ Allaire", "jjallaire",
    "jj@rstudio.org" , "JJ Alaire", "jjallaire",
    "joe@joecheng.com" , "Joe Cheng", "jcheng5",
    "joe@rstudio.com" , "Joe Cheng", "jcheng5",
    "joe@rstudio.org" , "Joe Cheng", "jcheng5",
    "jonathan@rstudio.com" , "Jonathan McPherson", "jmcphers",
    "karawoo@users.noreply.github.com", "Kara Woo", "karawoo",
    "kevinushey@gmail.com" , "Kevin Ushey", "kevinushey",
    "kirill.mueller@ivt.baug.ethz.ch" , "Kirill Mueller", "krlmlr",
    "krlmlr@users.noreply.github.com" , "Kirill Mueller", "krlmlr",
    "krlmlr+r@mailbox.org" , "Kirill Mueller", "krlmlr",
    "lindbrook@gmail.com", "lindbrook", "lindbrook",
    "lionel.hry@gmail.com" , "Lionel Henry", "lionel-",
    "maraaverick@gmail.com" , "Mara Averick", "batpigandme",
    "ramnath.vaidya@gmail.com" , "Ramnath Vaidyanathan", "ramnathv",
    "romain@purrple.cat" , "Romain Francois", "romainfrancois",
    "romain@r-enthusiasts.com" , "Romain Francois", "romainfrancois",
    "ry@tinyclouds.org" , "Ryan Dahl", "ry",
    "r.flight79@gmail.com", "Robert M. Flight", "rmflight",
    "saghul@gmail.com" , "Saul Ibarra Corretge", "saghul",
    "schloerke@gmail.com" , "Barret Schloerke", "schloerke",
    "spinuvit@gmail.com" , "Vitalie Spinu", "vspinu",
    "stefan@stefanbache.dk" , "Stefan Milton Bache", "smbache",
    "takahashi.kohske@gmail.com" , "Kohske Takahashi", "kohske",
    "thomasp85@gmail.com" , "Thomas Lin Pedersen", "thomasp85",
    "tjfontaine@gmail.com" , "Timothy J. Fontaine", "tjfontaine",
    "wilke@austin.utexas.edu" , "Claus Wilke", "clauswilke",
    "winston@stdout.org" , "Winston Chang", "wch",
    "xie@yihui.name" , "Yihui Xie", "yihui",
    "yutani.ini@gmail.com" , "Hiroaki Yutani", "yutannihilation"
)

### Who contributes to the tidyverse?

## Filter on multiple (>10 total) contributors
REPEAT_CONTRIBUTORS <- CONTRIBUTIONS %>% group_by(email) %>%
                                         summarize(count = sum(count)) %>%
                                         filter(count >= 80) %>%
                                         pull(email)
CONTRIBUTIONS <- CONTRIBUTIONS %>% filter(email %in% REPEAT_CONTRIBUTORS) %>%
                                   left_join(NAME_MAP, by = "email") %>%
                                   group_by(name, package) %>%
                                   summarize(count = sum(count)) %>%
                                   ungroup %>%
                                   spread(package, count, fill = 0)

CONTRIB_MAT <- CONTRIBUTIONS %>% select(-name) %>% log1p %>% as.matrix
rownames(CONTRIB_MAT) <- CONTRIBUTIONS %>% pull(name)

carp_fit_persons  <- CARP(CONTRIB_MAT)
carp_fit_packages <- CARP(t(CONTRIB_MAT))

# Get avatars for users
if(!dir.exists("avatars")){
    dir.create("avatars")
}

for(nm in NAME_MAP$github_id){
    fname <- file.path("avatars", paste0(nm, ".png"))
    if(!file.exists(fname)){
        gh_user_info <- gh("/users/:username", username = nm)
        download.file(gh_user_info$avatar_url, fname)
    }
}

# might need to run `mogrify -format png *` to convert some of these which are actually jpg to png

## Download Hex stickers for vizualization of carp_fit_packages
clone_repo("rstudio/hex-stickers")

## Make path info -- this is a edited version of clustRviz:::carp_dynamic_path_plot
my_dynamic_path_plot <- function(fit, name_map = NAME_MAP){
    grid_points <- seq(0, 1, length.out = 51) #c(seq(0, 0.5, length.out = 21), seq(0.5, 1, length.out = 41))

    plot_frame_full <- clustRviz:::get_feature_paths.CARP(fit, c("PC1", "PC2")) %>%
                       mutate(PC1 = PC1 + abs(min(PC1)) + 0.2,
                              PC2 = PC2 + abs(min(PC2)) + 0.2) ## Move this all to positive orthant for easier plotting

    plot_frame_first <- plot_frame_full %>% filter(.data$Iter == min(.data$Iter)) %>%
                                            select(.data$Obs,
                                                   .data$PC1,
                                                   .data$PC2,
                                                   .data$ObsLabel) %>%
                                             rename(FirstPC1 = .data$PC1,
                                                    FirstPC2 = .data$PC2,
                                                    FirstObsLabel =  .data$ObsLabel)
    plot_frame_animation <- bind_rows(lapply(grid_points, function(pct) {
        plot_frame_full %>% filter(.data$GammaPercent <= pct) %>%
                            mutate(percent = pct)
    }))

    G <- ggplot(plot_frame_animation, aes(x = PC1, y = PC2, group = Obs))

    ## Add in user icons
    name_map <- left_join(plot_frame_first, name_map, by = c(FirstObsLabel = "name")) %>%
                          select(Obs, FirstPC1, FirstPC2, github_id) %>%
                          distinct %>%
                          na.omit

    for(ix in seq_len(NROW(name_map))){
        fname <- file.path("avatars", paste0(name_map[ix,] %>% pull(github_id), ".png"))
        img   <- readPNG(fname)
        g     <- rasterGrob(img, interpolate = TRUE, name = fname)
        G <- G + annotation_custom(g,
                                   xmin = name_map[[ix, "FirstPC1"]] - 1.5,
                                   xmax = name_map[[ix, "FirstPC1"]] + 1.5,
                                   ymin = name_map[[ix, "FirstPC2"]] - 1.5,
                                   ymax = name_map[[ix, "FirstPC2"]] + 1.5)
    }

    G <- G + geom_path(linejoin = "round", color = "red", size = 1) +
           #geom_point(color = "black", size = 0.2) +
           #geom_point(data = plot_frame_first,
           #          aes(x = FirstPC1, y = FirstPC2),
           #          color = "black",
           #          size = I(4)) +
           geom_text(data = plot_frame_first,
                    aes(x = FirstPC1, y = FirstPC2, label = FirstObsLabel), size = I(15)) +
        guides(color = FALSE, size = FALSE) +
        theme_bw() +
        theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20)) +
        transition_manual(.data$percent) +
        xlim(c(1.5, 1.05) * range(plot_frame_animation$PC1)) +
        ylim(c(1.5, 1.05) * range(plot_frame_animation$PC2))
}

G <- my_dynamic_path_plot(carp_fit_persons)
anim_save("favicons.gif", G, width = 1600, height = 1600)

## Leave out dominate 3 devs
G2 <- plot(CARP(CONTRIB_MAT[-c(9, 15, 21),]), type = "path", percent = 0)
ggsave(filename = "zoom.png", G2 + theme_bw(), height = 6, width = 6)

op <- par()

png("tv_devs.png", 600, 600);
par(mar = c(8.1, 4.1, 1.1, 2.1));
plot(carp_fit_persons, dend.labels.cex = 0.9, main = "Tidyverse Devs");
dev.off()

png("tv_pkgs.png", 600, 600)
par(mar = c(8.1, 4.1, 1.1, 0.1));
plot(carp_fit_packages, dend.labels.cex = 1.3, main = "Tidyverse Packages");
dev.off()
