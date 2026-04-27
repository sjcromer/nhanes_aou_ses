# Rscript for figure preparation


######################################################################
## Set Up & Load required pacakges & functions (julieeg/pantry) 
######################################################################

library(data.table) ; library(tidyverse)
library(ggplot2) ; library(ggpubr)

source("https://raw.githubusercontent.com/julieeg/pantry/main/functions/plotting.R")


## Load csv files with regression results
#cont <- fread("../data/processed/R1_nhanes_aou_racexses_cont.csv")
#cat <- fread("../data/processed/R1_nhanes_aou_racexses_cat.csv")
head(cont) ; head(cat)


## Sensitivity analyses:
#qbin <- fread("../data/processed/R1_nhanes_aou_racexses_quasibinom.csv")
#mpois <- fread("../data/processed/R1_nhanes_aou_racexses_modpois.csv")
#pois <- fread("../data/processed/R1_nhanes_aou_racexses_poisson.csv")


# =================================================================
## Make vectors of meaningful variable labels for plotting
# =================================================================

race_nhanes.labs <- c(all="All", all_raceadj = "All, Race-Adjusted", nhw="NHW", nhb="NHB",
               mexam="Mexican-American", othhis="Other Hispanic", nha="NHA",
               othmulti_pre2011 = "Other (pre-2011)", othmulti_post2011 = "Other (post-2011)")

race_aou.labs <- c(all="All", all_raceadj = "All, Race-Adjusted", nhw="NHW", nhb="NHB",
               his="Hispanic", nha="NHA",  multi="Multi-Race", other="Other", none="None of the above")

race.labs <- c(all="All", all_raceadj = "All, Race-Adjusted", nhw="NHW", nhb="NHB",
               mexam="Mexican-American", his="Hispanic", othhis="Other Hispanic", nha="NHA",
               othmulti_pre2011 = "Other (pre-2011)", othmulti_post2011 = "Other (post-2011)", 
               multi="Multi-Race", other="Other", none="None of the above")


# =================================================================
## Make study- and RE-specific color palettes (from Nature)
# =================================================================

## FIRST: manually computed middle color palette between red/purple (via chatGPT) for other (pre-2011)
palettes$NatExt$RedPurp <- c("#530F2F", "#763444", "#B64168", "#D1708E", "#DDA5B6", "#E0C6D0")
palettes$NatExt$RedOrng <- c("#712412", "#953E20", "#D7501E", "#E77E53", "#F2AF91", "#E5CABB")


## Set primary color palette (by race) based on Nature Extended Palette (pantry)
race_cont.palette <- c(
  "black","grey50", palettes$NatExt$Yellows[3], palettes$NatExt$Greens[3],
  palettes$NatExt$Teals[3],palettes$NatExt$Teals[3], palettes$NatExt$Blues[3], 
  palettes$NatExt$Purples[3], palettes$NatExt$RedPurp[3], palettes$NatExt$RedOrng[3], 
  palettes$NatExt$Reds[3], palettes$NatExt$Oranges[3],  palettes$NatExt$Browns[3]
  ) ; names(race_cont.palette) <- as.vector(race.labs)

race_cat.palette <- list(
  c("black", "black", "#404040", "#606060", "#808080"),
  palettes$NatMainBackground$Tones,
  palettes$NatExt$Yellows, palettes$NatExt$Greens, palettes$NatExt$Teals, palettes$NatExt$Teals,
  palettes$NatExt$Blues,  palettes$NatExt$Purples, palettes$NatExt$RedPurp,
  palettes$NatExt$RedOrng, palettes$NatExt$Reds, palettes$NatExt$Oranges, palettes$NatExt$Browns) 
names(race_cat.palette) <- unique(as.vector(race.labs)) #as.vector(race.labs[c(1:5,7:10)])

# nhanes color palette 
race_cat_nhanes.palette <- list(
  c("black", "black", "#404040", "#606060", "#808080"),
  palettes$NatMainBackground$Tones,
  palettes$NatExt$Yellows, palettes$NatExt$Greens, palettes$NatExt$Teals, palettes$NatExt$Blues, 
  palettes$NatExt$Purples, palettes$NatExt$RedPurp, palettes$NatExt$RedOrng)
names(race_cat_nhanes.palette) <- unique(as.vector(race_nhanes.labs)) #as.vector(race.labs[c(1:5,7:10)])

# aou color palette 
race_cat_aou.palette <- list(
  c("black", "black", "#404040", "#606060", "#808080"),
  palettes$NatMainBackground$Tones,
  palettes$NatExt$Yellows, palettes$NatExt$Greens, palettes$NatExt$Teals, palettes$NatExt$Purples, 
  palettes$NatExt$Reds, palettes$NatExt$Oranges, palettes$NatExt$Browns)
names(race_cat_aou.palette) <- unique(as.vector(race_aou.labs)) #as.vector(race.labs[c(1:5,7:10)])


## Color palettes for categorical SES exposures (main figures)
  
# education - nhanes
racecat_educXcat_nhanes_palette <- c(unlist(race_cat_nhanes.palette))
racecat_educXcat_nhanes_palette <- racecat_educXcat_nhanes_palette[
  !endsWith(names(racecat_educXcat_nhanes_palette), "1") & !endsWith(names(racecat_educXcat_nhanes_palette), "6")]
names(racecat_educXcat_nhanes_palette) <- c(paste0(rep(names(race_cat_nhanes.palette), each=4), c(
  " X No HS_NHANES", " X Some HS_NHANES", " X HS Degree_NHANES", " X College Degree_NHANES")))

# education - aou
racecat_educXcat_aou_palette <- c(unlist(race_cat_aou.palette))
racecat_educXcat_aou_palette <- racecat_educXcat_aou_palette[
  !endsWith(names(racecat_educXcat_aou_palette), "1") & !endsWith(names(racecat_educXcat_aou_palette), "6")]
names(racecat_educXcat_aou_palette) <- c(paste0(rep(names(race_cat_aou.palette), each=4), c(
  " X Some HS_AoU", " X HS Degree_AoU", " X Some College_AoU", " X College Degree_AoU")))

racecat_educXcat_palette <- c(racecat_educXcat_nhanes_palette, racecat_educXcat_aou_palette)

# income - nhanes
racecat_incXcat_nhanes_palette <- c(unlist(race_cat_nhanes.palette))
racecat_incXcat_nhanes_palette <- racecat_incXcat_nhanes_palette[
  !endsWith(names(racecat_incXcat_nhanes_palette), "1") & !endsWith(names(racecat_incXcat_nhanes_palette), "6")]
names(racecat_incXcat_nhanes_palette) <- c(paste0(rep(names(race_cat_nhanes.palette), each=4), c(
  " X Less than $20,000_NHANES", " X $20,000-44,999_NHANES", " X $45,000-74,999_NHANES", " X More than $75,000_NHANES")))

# income - aou
racecat_incXcat_aou_palette <- c(unlist(race_cat_aou.palette))
racecat_incXcat_aou_palette <- racecat_incXcat_aou_palette[!endsWith(names(racecat_incXcat_aou_palette), "1")]
racecat_incXcat_aou_palette <- c(racecat_incXcat_aou_palette[1:4], All6="#909090", racecat_incXcat_aou_palette[5:44])
names(racecat_incXcat_aou_palette) <- c(paste0(rep(names(race_cat_aou.palette), each=5), c(
  " X Less than $25,000_AoU", " X $25,000-49,999_AoU", " X $50,000-74,999_AoU", 
  " X More than $150,000_AoU", " X $75,000-149,999_AoU"))) ; racecat_incXcat_aou_palette

racecat_incXcat_palette <- c(racecat_incXcat_nhanes_palette, racecat_incXcat_aou_palette)


#######################################################################
## Prepare regression outputs for plotting
########################################################################

study_shapes <- c("NHANES"=19, "AoU"=17)

# A) Continuous SES exposure ----------

cont_formatted <- cont %>%
  mutate_at("dataset", ~factor(ifelse(. == "nhanes", toupper(.), "AoU"), levels = c("NHANES", "AoU"))) %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ_"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("Obesity", "T2D"))) %>%
  mutate_at("racecat", ~factor(., levels = rev(names(race.labs)), labels = rev(race.labs)))


# B) Categorical SES exposure -----------

cat %>% group_by(dataset, exposure, level) %>% reframe(N = n())

# Create SES Level variables/labels
educlvl_nhanes.labs <- c(NoHS = "No HS", SomeHS="Some HS", HSdegree="HS Degree")
inclvl_nhanes.labs <- c("0_19"="Less than $20,000", "20_45"="$20,000-44,999", "45_74"="$45,000-74,999")
seslvl_nhanes.labs <- c(paste0(rep(race.labs, each=3), "/", educlvl_nhanes.labs), 
                        paste0(rep(race.labs, each=3), "/", inclvl_nhanes.labs))

educlvl_aou.labs <- c(SomeHS="Some HS", Hsdegree="HS Degree", SomeColl="Some College")
inclvl_aou.labs <- c("0_24"="Less than $25,000", "25_49"="$25,000-49,999", "50_74"="$50,000-74,999", "gt_150"="More than $150,000")
seslvl_aou.labs <- c(paste0(rep(race.labs, each=3), "/", educlvl_aou.labs),
                     paste0(rep(race.labs, each=4), "/", inclvl_aou.labs))

outrace.labs <- paste0(rep(c("T2D.", "Obesity."), each=length(race.labs)), rep(race.labs, 2))

cat_formatted <- cat %>%
  mutate_at("dataset", ~factor(ifelse(. == "nhanes", toupper(.), "AoU"), levels = c("NHANES", "AoU"))) %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("T2D", "Obesity"))) %>%
  mutate_at("racecat", ~factor(., levels = rev(names(race.labs)), labels = rev(race.labs))) %>%
  mutate_at("level", ~factor(., levels=c(names(educlvl_nhanes.labs), names(educlvl_aou.labs),
                                         names(inclvl_nhanes.labs), names(inclvl_aou.labs)),
                             labels = c(educlvl_nhanes.labs, educlvl_aou.labs,
                                        inclvl_nhanes.labs, inclvl_aou.labs))) %>%
  mutate(shade_catlvl = factor(ifelse(dataset=="NHANES", 
                               case_when(level %in% c("No HS", "Less than $20,000") ~ "A",
                                         level %in% c("Some HS", "$20,000-44,999") ~"B",
                                         level %in% c("HS Degree", "$45,000-74,999") ~"C"),
                               case_when(level %in% c("Some HS", "Less than $25,000") ~ "A",
                                         level %in% c("HS Degree", "$25,000-49,999") ~"B",
                                         level == "$50,000-74,999" ~ "C",
                                         level %in% c("Some College", "More than $150,000") ~ "D")
  ), levels=c("A","B","C","D"))) %>%
  mutate(shade = paste0(racecat, " X ", level, "_", dataset))


## C) Quasi-Binomial (continuous SES)
qbin_formatted <- qbin %>%
  mutate_at("dataset", ~factor(ifelse(. == "nhanes", toupper(.), "AoU"), levels = c("NHANES", "AoU"))) %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ_"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("Obesity", "T2D"))) %>%
  mutate_at("racecat", ~factor(., levels = rev(names(race.labs)), labels = rev(race.labs)))


## D) Modified Poisson (continuous SES)
mpois_formatted <- mpois %>%
  mutate_at("dataset", ~factor(ifelse(. == "nhanes", toupper(.), "AoU"), levels = c("NHANES", "AoU"))) %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ_"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("Obesity", "T2D"))) %>%
  mutate_at("racecat", ~factor(., levels = rev(names(race.labs)), labels = rev(race.labs)))


## E) Poisson (continuous SES)
pois_formatted <- pois %>% 
  mutate_at("dataset", ~factor(ifelse(. == "nhanes", toupper(.), "AoU"), levels = c("NHANES", "AoU"))) %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ_"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("Obesity", "T2D"))) %>%
  mutate_at("racecat", ~factor(., levels = rev(names(race.labs)), labels = rev(race.labs)))


######################################################################
## Function to plot continuous SES exposure  
######################################################################

## Make function to apply over SES exposures & Datasets 
plot_contSESxY <- function(SES, Outcome, DataSet, plotlims=c(0.75,1.33), data=cont_formatted) {
  
  ## Create bounds to depict using arrows
  data.plot <- data %>% mutate(
    extreme = ifelse(lowerCI < plotlims[1], "lower", ifelse(upperCI>plotlims[2], "upper", F)),
    lowerCI.lim = ifelse(lowerCI<plotlims[1], plotlims[1], lowerCI), # only below lowerCI
    upperCI.lim = ifelse(upperCI>plotlims[2], plotlims[2], upperCI)) # only above upperCI
  out_of_bounds <- data.plot %>% filter(extreme!=F)
  
  data.plot %>%
    filter(exposure == SES & outcome == Outcome) %>%
    ggplot(aes(x = coeff, xmin = lowerCI.lim, xmax = upperCI.lim, y = racecat,
               color = racecat, shape=dataset)) + 
    facet_grid(rows=vars(dataset), scales="free") + 
    theme_bw() + theme(
      strip.background = element_blank(), 
      strip.text = element_text(face="bold", size=10, vjust = 1.5),
      strip.text.y.right = element_blank(),
      axis.text.y = element_text(size=8.5, color="black"),
      axis.text.x = element_text(size=8, color="black"),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(size=9),
      panel.grid.major.x = element_line(linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(color = "grey80"),
      panel.spacing.y = unit(0.25,"cm"), panel.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(0.2,0.2,0.35,0.02, unit="cm"),
      #Legend
      legend.key.size = unit(0.35,"cm"),
      legend.background = element_rect(color="grey70"),
      legend.title = element_text(size=9),
      legend.text = element_text(size=8)
    ) + 
  
    ylab("") + xlab(paste("OR (95% CI) per 1 unit increase in", SES)) +
    scale_x_log10(limits = plotlims, breaks = scales::log_breaks(n=6)) + #scale_y_discrete(expand=c(0,0.35)) +
    geom_vline(xintercept = 1, color = "black", linewidth=0.25) + 
    geom_point(size=1.35, position = position_dodge(0.75)) + 
    geom_errorbar(position = position_dodge(0.75), width=0.5, linewidth=0.35) + 
    scale_color_manual(values=race_cont.palette, name=paste0("Race/Ethnicity\nGroups")) + 
    scale_shape_manual(values=study_shapes, guide="none") +#+

    ## Overlay arrows for CI extremes
    # upperCI
    geom_text(data=out_of_bounds %>% filter(exposure==SES, outcome == Outcome, extreme == "upper"),
              aes(x = upperCI.lim, y = racecat, color = racecat), 
              label = "\u25B6", size=4, family = "Arial Unicode MS") + 
    ## lowerCI
    geom_text(data=out_of_bounds %>% filter(exposure==SES,outcome == Outcome, extreme == "lower"),
              aes(x = lowerCI.lim, y = racecat, color = racecat), 
              label = "\u25C0", size=4, family = "Arial Unicode MS")
}


## Example plot: education
ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
  ggarrange(text_grob(ifelse(y=="T2D", "Type 2 Diabetes", "Obesity"), 
                      rot = 90, just = "center"),
            plot_contSESxY(SES="Education", Outcome = y, plotlims=c(0.75, 1.33)) +
              theme(legend.position = "none"),
            ncol = 2, widths = c(0.02, 1))
  }), ncol=1) 


######################################################################
## Function to plot categorical SES exposure  
######################################################################

## Make function to apply over SES exposures & Datasets 
plot_catSESxY <- function(SES, Outcome, plotlims = c(0.3,3.33), data=cat_formatted) {
  
  ## Create bounds to depict using arrows
  data.plot <- data %>% mutate(
    extreme = ifelse(lowerCI < plotlims[1] | upperCI>plotlims[2], T, F),
    upperCI.lim = ifelse(upperCI>plotlims[2], plotlims[2], upperCI)) # only above upperCI
  out_of_bounds <- data.plot %>% filter(extreme==T)
  
  data.plot %>%
    filter(exposure == SES & outcome == Outcome) %>%
    ggplot(aes(x = coeff, xmin = lowerCI, xmax = upperCI.lim, y = racecat, group = level, 
               color = shade, shape = dataset)) + 
    facet_grid(rows = vars(dataset), scales = "free") + 
    theme_bw() + theme(
      strip.background = element_blank(), 
      strip.text = element_text(face="bold", size=10, vjust = 1.5),
      strip.text.y.right = element_blank(),
      axis.text.y = element_blank(), 
      axis.text.x = element_text(size=8), 
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size=9, color="black"),
      axis.text = element_text(color = "black"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(color = "grey80"),
      panel.spacing.y = unit(0.25,"cm"), panel.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(0.2,0.2,0.35,0.02, unit="cm"),
      #Legend
      legend.key.size = unit(0.35,"cm"),
      legend.background = element_rect(color="grey70"),
      legend.title = element_text(size=9),
      legend.text = element_text(size=8)) + 
    ylab("") + 
    xlab(paste("OR (95% CI) for", SES, "Level vs", ifelse(SES == "Education", "College Degree",
                                                                            "$75,000-149,999 or higher"))) +
    scale_x_log10(limits = plotlims, 
                  breaks=scales::log_breaks(n=8)) + ## log10x scale + BOUNDS 
    scale_y_discrete(expand=c(0,1)) +
    geom_point(size=1.15, position = position_dodge2(width=0.65, preserve = "single")) + 
    geom_errorbar(position = position_dodge2(width=0.65, preserve = "single"), width=0.65, linewidth=0.35) + 
    geom_vline(xintercept = 1, color = "black", linewidth=0.25) + 
    scale_shape_manual(values=study_shapes, guide="none") +
    scale_color_manual(values=c(racecat_educXcat_palette, racecat_incXcat_palette), name=paste0("Race/Ethnicity\nGroups"), guide="none") + 
    geom_text(data=out_of_bounds %>% filter(exposure==SES, outcome == Outcome),
               aes(x = upperCI.lim, y = racecat, group = level, color = shade), 
              label = "\u25B6", size=4.5, family = "Arial Unicode MS")
}


########################################
## Function to build custom legends
#######################################

# NHANES-Race/Ethnicity 
build_custom_legend.fun <- function(DataSet, SES, index="coeff", data = cat_formatted, plottype = "forest") {
  # Add flag to customize the shapes for the legend
  if(plottype == "forest") {shapes = c(19,17)} else (shapes = c(15,15))
  
  grid::grid.newpage()
  
  p <- data %>%
    filter(dataset == DataSet, exposure==SES) %>%
    rename(xvar = index) %>%
    ggplot(aes(x = xvar, y=racecat, color = racecat, group = level, shape = dataset, alpha = shade_catlvl)) + geom_point() + 
    scale_shape_manual(values=study_shapes, name="Data Set") +
    scale_color_manual(values=race_cont.palette, name=paste0("Race/Ethnicity\nGroups in ", DataSet)) + 
    scale_alpha_manual(values=c("E"=0.25, "D"=0.35, "C"=0.5, "B"=0.6, "A"=0.99), 
                       labels = if(DataSet=="NHANES") {
                         if(SES=="Education") { c(D="College Degree", C="HS Degree", B="Some HS", A="No HS") } else {
                           c(D="More than $75,000", C="$45,000-$74,999", B="$20,000-$44,999", A="Less than $20,000") }
                       } else {
                         if(SES=="Education") {c(D="College Degree", C="Some College", B="HS Degree", A="Some HS")} else {
                           c(D=ifelse(plottype=="forest", "More than $150,000", "$75,000-$149,999"), 
                             E=ifelse(plottype=="forest", "$75,000-$149,999", "More than $150,000"),
                             C="$50,000-$74,999", B="$25,000-$49,999", A="Less than $25,000") } 
                       }, name=paste(SES,"Levels \nin", DataSet)) +
    # Theme 
    theme_bw() + theme(
      legend.key.size = unit(0.35,"cm"),
      legend.background = element_blank(),
      legend.title = element_text(size=9),
      legend.text = element_text(size=8),
      legend.spacing.y = unit(0.1,"mm")) + 
    
    guides(
      shape=guide_legend(reverse=T, order=3, override.aes = c(
        shape=shapes, labels=c("NHANES", "AoU"))),
      color=guide_legend(reverse = T, order=1, override.aes = c(
        linewidth=0.3, size=1.5, shape = ifelse(DataSet=="NHANES", shapes[1], shapes[2]))),
      alpha = guide_legend(reverse = T, order=2, override.aes = c(shape = ifelse(DataSet=="NHANES", shapes[1], shapes[2])))
      ) ; 
  
   as_ggplot(get_legend(p)) + 
    theme(plot.margin = margin(0, 0, 0, 0))
}


###########################################################################
## MAIN FIGURE: Combined panel figure WITHOUT "other" RE categories
###########################################################################

racecat_other = c("Other (post-2011)", "Other (pre-2011)", "Other", "Multi-Race", "None of the above")

# Education
plot_educ_cont <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Education", plotlims=c(0.75, 1.2), Outcome = y,
                             data = cont_formatted %>% filter(!racecat %in% racecat_other)) +
                theme(legend.position = "none"), ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_educ_cont

plot_educ_cat <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_catSESxY(SES="Education", Outcome = y, data = cat_formatted %>% 
                  filter(!racecat %in% racecat_other), plotlims=c(0.4,3.33)) + 
    theme(legend.position = "none") + 
    scale_x_log10(limits=c(0.4,3.33), breaks=c(0.33,0.57,1,1.75,3.0))
}), ncol=1) ; plot_educ_cat

png(filename="../output/R1_plot_educxses_panel.png", height = 4250, width=5000, res=600)
ggarrange(plot_educ_cont, plot_educ_cat, widths = c(1.35,1), ncol=2, align = "hv")
dev.off()


# Income
plot_inc_cont <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Income", Outcome = y, plotlims = c(0.8,1.2),
                             data = cont_formatted %>% filter(!racecat %in% racecat_other)) +
                theme(legend.position = "none"), ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_inc_cont


plot_inc_cat <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_catSESxY(SES="Income", Outcome = y, plotlims = c(0.4,2.5),
                data = cat_formatted %>% filter(!racecat %in% racecat_other) %>%
                  filter(level != "More than $150,000")) + 
    theme(legend.position = "none") +
    scale_x_log10(limits=c(0.4,2.5), breaks=c(0.4,0.6,1,1.7,2.5))
}), ncol=1) ; plot_inc_cat

## Save as .png
png(filename="../output/R1_plot_incxses_panel.png", height = 4250, width=5000, res=600)
ggarrange(plot_inc_cont, plot_inc_cat, widths = c(1.35,1), ncol=2, align = "hv")
dev.off()


# ====================
## Custom Legends 
# ====================

# NHANES
#png(filename="../output/R1_legend_main_nhanes.png", height = 4250, width=5000, res=600)
ggarrange(
  build_custom_legend.fun(DataSet = "NHANES", SES="Education", data=cat_formatted %>% filter(!racecat %in% racecat_other)),
  build_custom_legend.fun(DataSet = "NHANES", SES="Income", data=cat_formatted %>% filter(!racecat %in% racecat_other)),
  ncol=1, align="hv")
#dev.off()

# AoU
#png(filename="../output/R1_legend_main_aou.png", height = 4250, width=5000, res=600)
ggarrange(
  build_custom_legend.fun(DataSet = "AoU", SES="Education", data=cat_formatted %>% filter(!racecat %in% racecat_other)),
  build_custom_legend.fun(DataSet = "AoU", SES="Income", data=cat_formatted %>% filter(!racecat %in% racecat_other) %>% 
                            filter(level != "More than $150,000")),
  ncol=1, align="hv")
#dev.off()


###########################################################
## Figure for sensitivity analyses with ALL R/E GROUPS
###########################################################

# Education
plot_educ_cont <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Education", plotlims=c(0.7, 1.43), Outcome = y, data = cont_formatted) +
                theme(legend.position = "none") + 
                scale_x_log10(limits=c(0.7,1.43), breaks=c(0.7,0.8,0.9,1,1.1, 1.25, 1.43)), ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_educ_cont

plot_educ_cat <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_catSESxY(SES="Education", Outcome = y, data = cat_formatted, plotlims=c(0.1,10)) + 
    theme(legend.position = "none") + 
    scale_x_log10(limits=c(0.1,10), breaks=c(0.1,0.25,0.5,1,2,4,10))
}), ncol=1) ; plot_educ_cat ## FIX THE X AXIS HERE***

#png(filename="../output/R1_plot_educxses_panel_allrac.png", height = 6250, width=5000, res=600)
ggarrange(plot_educ_cont, plot_educ_cat, widths = c(1.35,1), ncol=2, align = "hv")
#dev.off()


# Income
plot_inc_cont <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Income", Outcome = y, plotlims = c(0.75,1.33), data = cont_formatted) +
                theme(legend.position = "none"), ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_inc_cont

plot_inc_cat <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_catSESxY(SES="Income", Outcome = y, plotlims = c(0.25,6.25), data = cat_formatted) + 
    theme(legend.position = "none") +
    scale_x_log10(limits=c(0.16,6.25), breaks=c(0.13,0.25,0.5,1,2,4))
}), ncol=1) ; plot_inc_cat

# Save as .png
#png(filename="../output/R1_plot_incxses_panel_allrac.png", height = 6250, width=5000, res=600)
ggarrange(plot_inc_cont, plot_inc_cat, widths = c(1.35,1), ncol=2, align = "hv")
#dev.off()


# ====================
## Custom Legends 
# ====================

# NHANES
#png(filename="../output/R1_legend_suppl_nhanes.png", height = 6250, width=5000, res=600)
ggarrange(
  build_custom_legend.fun(DataSet = "NHANES", SES="Education", data=cat_formatted),
  build_custom_legend.fun(DataSet = "NHANES", SES="Income", data=cat_formatted),
  ncol=1, align="hv")
#dev.off()

# AoU
#png(filename="../output/R1_legend_suppl_aou.png", height = 6250, width=5000, res=600)
ggarrange(
  build_custom_legend.fun(DataSet = "AoU", SES="Education", data=cat_formatted),
  build_custom_legend.fun(DataSet = "AoU", SES="Income", data=cat_formatted),
  ncol=1, align="hv")
#dev.off()


############################################################################
## SENSITIVITY FIGURE: QBin, MonPois, Pois Regressions (all RE groups)
############################################################################

## Education
# Quasi-Binomial
plot_educ_qbin <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Education",  plotlims=c(0.7,1.43), Outcome = y,
                             data = qbin_formatted) + 
                theme(legend.position = "none") + 
                scale_x_log10(limits=c(0.7,1.43), breaks=c(0.7,0.8,0.9,1,1.11,1.25,1.43)),
              ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_educ_qbin


plot_educ_mpois <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_contSESxY(SES="Education", Outcome = y, data = mpois_formatted, plotlims=c(0.84, 1.2)) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }), ncol=1) ; plot_educ_mpois

plot_educ_pois <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_contSESxY(SES="Education", Outcome = y, data = pois_formatted , plotlims=c(0.83, 1.2)) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
}), ncol=1) ; plot_educ_pois


#png(filename="../output/R1_plot_educxses_panel_sens_altmodels.png", height = 5250, width=6650, res=600)
ggarrange(plot_educ_qbin, plot_educ_mpois, plot_educ_pois, widths = c(1.35,1,1), ncol=3, align = "hv")
#dev.off()


## Income
# Quasi-Binomial
plot_inc_qbin <- ggarrange(
  plotlist=lapply(c("T2D", "Obesity"), function(y) {
    ggarrange(text_grob(ifelse(y=="T2D", "\nType 2 Diabetes", "\nObesity"), 
                        rot = 90, just = "center", size=9),
              plot_contSESxY(SES="Income",  plotlims=c(0.75,1.25), Outcome = y,
                             data = qbin_formatted) +
                theme(legend.position = "none"),
              ncol = 2, widths = c(0.1,1))
  }), ncol=1, labels = c("A","B")) ; plot_inc_qbin

plot_inc_mpois <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_contSESxY(SES="Income", Outcome = y, data = mpois_formatted, plotlims=c(0.87, 1.15)) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
}), ncol=1) ; plot_inc_mpois

plot_inc_pois <- ggarrange(plotlist=lapply(c("T2D", "Obesity"), function(y) {
  plot_contSESxY(SES="Income", Outcome = y, data = pois_formatted, plotlims=c(0.8, 1.15)) + 
    scale_x_log10(limits=c(0.8,1.25), breaks=c(0.8,0.9,1,1.11,1.25)) +
    theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
}), ncol=1) ; plot_inc_pois


#png(filename="../output/R1_plot_incxses_panel_sens_altmodels.png", height = 5250, width=6650, res=600)
ggarrange(plot_inc_qbin, plot_inc_mpois, plot_inc_pois, widths = c(1.35,1,1), ncol=3, align = "hv")
#dev.off()


############################################################################
## FIGURE 1: Age-standardized T2D/Obesity Prevalence by RExSES
############################################################################

## NHANES ===================================

# Check for correct row/column labels: exposures (educ/inc) and outcomes (t2d/obes)

#agestd_nhs <- fread("../data/raw/nhanes_ses_agestdprev_forjulie.csv", header = T)
names(agestd_nhs)[1] <- "level"
#agestd_nhs.ci <- fread("../data/raw/nhanes_ses_agestdprev_ci_forjulie.csv", header = T)
names(agestd_nhs.ci) <- names(agestd_nhs)

agestd_nhs.edu <- agestd_nhs[,1:8] ; agestd_nhs.inc <- agestd_nhs[,c(1,9:15)]
agestd_nhs.edu <- (agestd_nhs.edu %>% mutate(outcome = c("outcome", rep("t2d", 4), rep("obesity",4))))[-1,] %>%
  mutate(level = rep(agestd_nhs.edu$level[2:5], 2), exposure="educ")
agestd_nhs.inc <- (agestd_nhs.inc %>% mutate(outcome = c("outcome", rep("t2d", 4), rep("obesity",4))))[-1,] %>%
  mutate(level = rep(agestd_nhs.inc$level[6:9], 2), exposure="inc")

agestd_nhs.edu.ci <- agestd_nhs.ci[,1:8] ; agestd_nhs.inc.ci <- agestd_nhs.ci[,c(1,9:15)]
agestd_nhs.edu.ci <- (agestd_nhs.edu.ci %>% mutate(outcome = c("outcome", rep("t2d", 4), rep("obesity",4))))[-1,] %>%
  mutate(level = rep(agestd_nhs.edu.ci$level[2:5], 2), exposure="educ")
agestd_nhs.inc.ci <- (agestd_nhs.inc.ci %>% mutate(outcome = c("outcome", rep("t2d", 4), rep("obesity",4))))[-1,] %>%
  mutate(level = rep(agestd_nhs.inc.ci$level[6:9], 2), exposure="inc")

agestd_nhs <- rbind.data.frame(
  agestd_nhs.edu %>% pivot_longer(cols = -c("exposure", "outcome", "level"), names_to=c("racecat"), values_to="estimate"),
  agestd_nhs.inc %>% pivot_longer(cols = -c("exposure", "outcome", "level"), names_to=c("racecat"), values_to="estimate")
) ; agestd_nhs.ci <- rbind.data.frame(
  agestd_nhs.edu.ci %>% pivot_longer(cols = -c("exposure", "outcome", "level"), names_to=c("racecat"), values_to="ci"),
  agestd_nhs.inc.ci %>% pivot_longer(cols = -c("exposure", "outcome", "level"), names_to=c("racecat"), values_to="ci")
) ; agestd_nhs <- agestd_nhs %>% left_join(agestd_nhs.ci, by = c("exposure", "level", "outcome", "racecat"))


agestd_nhs_formatted <- agestd_nhs %>%
  mutate(dataset="NHANES") %>%
  mutate_at("exposure", ~factor(ifelse(startsWith(., "educ"), "Education", "Income"), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("T2D", "Obesity"))) %>%
  mutate_at("racecat", ~ifelse(. == "Other/Multi-Racial including NHA (pre-2011)", "Other (pre-2011)", 
                               ifelse(. == "Other/Multi-Racial (post-2011)", "Other (post-2011)", 
                                      ifelse(. == "NHAsian", "NHA", .)))) %>%
  mutate_at("racecat", ~factor(., levels = race_nhanes.labs)) %>%
  mutate(across(c("estimate", "ci"), ~as.numeric(.))) %>%
  mutate_at("level", ~ifelse(.=="Under $20,000", "Less than $20,000", .)) %>%
  mutate_at("level", ~factor(., levels=c(educlvl_nhanes.labs, "College Degree",
                                         c("Less than $20,000", "$20,000-44,999", "$45,000-74,999", "More than $75,000")))) %>%
  mutate(lowerCI=estimate-ci,
         upperCI=estimate+ci) %>%
  mutate(shade = paste0(racecat, " X ", level, "_", dataset)) %>%
  mutate(shade_catlvl = factor(case_when(level %in% c("No HS", "Less than $20,000") ~ "A",
                                         level %in% c("Some HS", "$20,000-44,999") ~"B",
                                         level %in% c("HS Degree", "$45,000-74,999") ~"C",
                                         level %in% c("College Degree", "More than $75,000") ~"D"), 
                               levels=c("A","B","C","D"))) %>%
  mutate(across(c(estimate, lowerCI, upperCI), ~.*100))


## AoU =================================== 

#agestd_aou <- fread("../data/raw/R1_aou_ses_agestdprev_forjulie.csv", header = T)
names(agestd_aou) <- c("level", paste0(names(agestd_aou)[2:8], "_t2d"), paste0(names(agestd_aou)[9:15], "_ob"))
#agestd_aou.ci <- fread("../data/raw/R1_aou_ses_agestdprev_ci_forjulie.csv", header = T)
names(agestd_aou.ci) <- names(agestd_aou)

agestd_aou <- agestd_aou %>% mutate(exposure = c("outcome", rep("education",4), rep("income",5)), .before="level") %>%
  filter(exposure != "outcome") %>% 
  pivot_longer(cols = -c("exposure", "level"), names_sep = "_", 
               names_to=c("racecat", "outcome"), values_to="estimate") 

agestd_aou.ci <- agestd_aou.ci %>% mutate(exposure = c("outcome", rep("education",4), rep("income", 5)), .before="level") %>%
  filter(exposure != "outcome") %>% 
  pivot_longer(cols = -c("exposure", "level"), names_sep = "_", 
               names_to=c("racecat", "outcome"), values_to="ci")

agestd_aou <- agestd_aou %>% left_join(agestd_aou.ci, by = c("exposure", "level", "racecat", "outcome"))

race_aou.labs <- c(NHW="NHW", NHB="NHB", Hispanic="Hispanic", NHAsian="NHA",
                   Multiracial="Multi-Race", Other="Other", "None of these"="None of the above")

agestd_aou_formatted <- agestd_aou %>%
  mutate(dataset="AoU") %>%
  mutate_at("exposure", ~factor(str_to_sentence(.), levels = c("Education", "Income"))) %>%
  mutate_at("outcome", ~factor(ifelse(.=="t2d", "T2D", "Obesity"), levels = c("T2D", "Obesity"))) %>%
  mutate_at("racecat", ~ifelse(. == "NHAsian", "NHA", 
                               ifelse(. == "None of these", "None of the above",
                                      ifelse(. == "Multiracial", "Multi-Race", .)))) %>%
  mutate_at("racecat", ~factor(., levels = race_aou.labs)) %>%
  mutate(across(c("estimate", "ci"), ~as.numeric(.))) %>%
  mutate_at("level", ~ifelse(.=="Under $25,000", "Less than $25,000", 
                             ifelse(. == "Less than HS", "Some HS", .))) %>%
  mutate_at("level", ~factor(., levels=c("Some HS", "HS Degree", "Some College", "College Degree",
                                         "Less than $25,000", "$25,000-49,999", "$50,000-74,999",
                                           "$75,000-149,999", "More than $150,000"))) %>%
  mutate(lowerCI=estimate-ci, upperCI=estimate+ci) %>%
  mutate(shade = paste0(racecat, " X ", level, "_", dataset)) %>%
  mutate_at("shade", ~factor(., levels=)) %>%
  mutate(shade_catlvl = factor(case_when(level %in% c("Some HS", "Less than $25,000") ~ "A",
                                         level %in% c("HS Degree", "$25,000-49,999") ~"B",
                                         level %in% c("Some College", "$50,000-74,999") ~"C",
                                         level %in% c("College Degree", "$75,000-149,999") ~"D",
                                         level %in% c("More than $150,000") ~"E"), 
                               levels=c("A","B","C","D", "E"))) %>%
  
  mutate(across(c("estimate", "lowerCI", "upperCI"), ~(.*100)))


## Revise color coding for AOU to so More that $150,000 is the last color
# income - aou
racecat_incXcat_aou_palette_REV <- c(unlist(race_cat_aou.palette))
racecat_incXcat_aou_palette_REV <- racecat_incXcat_aou_palette_REV[!endsWith(names(racecat_incXcat_aou_palette_REV), "1")]
racecat_incXcat_aou_palette_REV <- c(racecat_incXcat_aou_palette_REV[1:4], All6="#909090", racecat_incXcat_aou_palette_REV[5:44])
names(racecat_incXcat_aou_palette_REV) <- c(paste0(rep(names(race_cat_aou.palette), each=5), c(
  " X Less than $25,000_AoU", " X $25,000-49,999_AoU", " X $50,000-74,999_AoU", 
  " X $75,000-149,999_AoU", " X More than $150,000_AoU"))) ; racecat_incXcat_aou_palette_REV


## Make function to apply over SES exposures & Datasets 
plot_agestd.fun <- function(DataSet, Exposure) {
  
  params <- if(DataSet == "NHANES") { 
    list(data = agestd_nhs_formatted,
         palette = c(racecat_educXcat_nhanes_palette, racecat_incXcat_nhanes_palette),
         xlabs = c("NHW", "NHB", "Mexican-\nAmerican", "Other\nHispanic", "NHA", 
                    "Other\n(pre-2011)", "Other\n(post-2011)")) } else { 
    list(data = agestd_aou_formatted, 
         palette = c(racecat_educXcat_aou_palette, racecat_incXcat_aou_palette_REV),
         xlabs = c("NHW", "NHB", "Hispanic", "NHA", "Multi-Race", "Other", "None of\nthe above"))
  }
  
  p.l <- lapply(c("T2D", "Obesity"), function(Y) {
    params$ymax = ifelse(Y == "T2D", 40, 80)
    params$ylab = ifelse(Y == "T2D", "Type 2 Diabetes", "Obesity")
    ## Create bounds to depict using arrows
    data.plot <- params$data %>% mutate(
      upperCI.extrm = ifelse(upperCI>params$ymax, params$ymax, NA), # only if above ylimit
      upperCI = ifelse(upperCI>params$ymax, NA, upperCI) # only if below ylimit
      #racecat = fct_rev(racecat)
      )
    #out_of_bounds <- data.plot %>% filter(extreme=="extreme")
  
    data.plot %>%
      filter(exposure == Exposure & outcome == Y) %>%
      ggplot(aes(x=racecat, group=level, fill = shade, y = estimate, ymin=lowerCI, ymax=upperCI)) + 
      facet_grid(rows = vars(outcome), cols = vars(dataset), scales = "free_y", 
                 labeller = as_labeller(c("NHANES" = "NHANES", "AoU" = "All of Us"))) + 
      geom_bar(stat="identity", position=position_dodge(0.9)) + 
      # Normal error bars
      geom_errorbar(data = data.plot %>% filter(outcome == Y & exposure == Exposure),
                    aes(x = racecat, group = level, ymax = upperCI, ymin=lowerCI), 
                    position=position_dodge(0.9), width = 0.35) + 
      # Extreme error bars
      geom_errorbar(data = data.plot %>% filter(outcome == Y & exposure == Exposure),
                    aes(x = racecat, group = level, ymax = upperCI.extrm, ymin=lowerCI), 
                    position=position_dodge(0.9), width = 0) + 
      scale_fill_manual(values=params$palette, guide="none") + 
      xlab("") + ylab(params$ylab) + 
      scale_x_discrete(labels = params$xlabs) +
      scale_y_continuous(limits=c(0,params$ymax), expand = expansion(mult = c(0.01, 0.05))) +
      theme_bw() + theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=9.5),
        strip.background = element_blank(),
        strip.text.x.top = element_text(size=11, face="bold"),
        strip.text.y.right = element_blank(),
        panel.spacing.y = unit(0.5,"cm"), panel.spacing.x = unit(0.5, "cm"),
        plot.margin = margin(0.1,0.5,0,0.5, unit="cm")) + 
      
      geom_text(data = data.plot %>% filter(outcome == Y & exposure == Exposure),
                aes(x = racecat, group=level, y = upperCI.extrm), color = "black",
                label="\u25B2", size=3, position=position_dodge(0.9))
    })  ; names(p.l) = c("t2d", "ob") ; p.l
      ## Overlay arrows for CI extremes
  }


## Combine & Compile plots into quadrants/Panels

# nhanes & education 
fig1_nhs_edu.l <- plot_agestd.fun("NHANES", "Education") ; fig1_nhs_edu.l
fig1_nhs_edu.l$ob <- fig1_nhs_edu.l$ob + theme(strip.text.x.top = element_blank())
fig1_nhs_edu.l$t2d <- fig1_nhs_edu.l$t2d + 
  theme(axis.text.x =  element_blank(), axis.ticks.x = element_blank())
# nhanes & income
fig1_nhs_inc.l <- plot_agestd.fun("NHANES", "Income") ; fig1_nhs_inc.l
fig1_nhs_inc.l$ob <- fig1_nhs_inc.l$ob + theme(strip.text.x.top = element_blank())
fig1_nhs_inc.l$t2d <- fig1_nhs_inc.l$t2d + 
  theme(axis.text.x =  element_blank(), axis.ticks.x = element_blank())

# aou & education
fig1_aou_edu.l <- lapply(plot_agestd.fun("AoU", "Education"), function(p) {
  p + theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) 
  }) ; fig1_aou_edu.l
fig1_aou_edu.l$t2d <- fig1_aou_edu.l$t2d + 
  theme(axis.text.x =  element_blank(), axis.ticks.x = element_blank())
fig1_aou_edu.l$ob <- fig1_aou_edu.l$ob + theme(strip.text.x.top = element_blank()) 

# aou & income
fig1_aou_inc.l <- lapply(plot_agestd.fun("AoU", "Income"), function(p) {
  p + theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) 
  }) 
fig1_aou_inc.l$t2d <- fig1_aou_inc.l$t2d +
  theme(axis.text.x =  element_blank(), axis.ticks.x = element_blank())
fig1_aou_inc.l$ob <- fig1_aou_inc.l$ob + theme(strip.text.x.top = element_blank()) 

# Combined education figure 
fig1_edu <- ggarrange(plotlist = list(
  ggarrange(plotlist=list(fig1_nhs_edu.l$t2d,"", fig1_aou_edu.l$t2d), widths=c(1.15,0,1), nrow=1),
  ggarrange(plotlist=list(fig1_nhs_edu.l$ob,"", fig1_aou_edu.l$ob), widths=c(1.15,0,1), nrow=1)),
  nrow=2) ; fig1_edu

# Combined income figure
fig1_inc <- ggarrange(plotlist = list(
  ggarrange(plotlist=list(fig1_nhs_inc.l$t2d,"", fig1_aou_inc.l$t2d), widths=c(1.15,0,1), nrow=1),
  ggarrange(plotlist=list(fig1_nhs_inc.l$ob,"", fig1_aou_inc.l$ob), widths=c(1.15,0,1), nrow=1)),
  nrow=2) ; fig1_inc


#png("../output/R1_plot_agestd_panel_byses.png", width = 5500, height=4500, res = 500)
ggarrange(plotlist=list(fig1_edu, fig1_inc), nrow=2, labels=c("A","B"))
#dev.off()


# Build Legend
#png(filename="../output/R1_legend_main_agestd.png", height = 5000, width=5000, res=600)
ggarrange(plotlist=list(
  build_custom_legend.fun(DataSet = "NHANES", SES="Education", index = "estimate", plottype = "bar", data = agestd_nhs_formatted),
  build_custom_legend.fun(DataSet = "NHANES", SES="Income", index = "estimate", plottype = "bar", data = agestd_nhs_formatted),
  build_custom_legend.fun(DataSet = "AoU", SES="Education", index = "estimate", plottype = "bar", data = agestd_aou_formatted),
  build_custom_legend.fun(DataSet = "AoU", SES="Income", index = "estimate", plottype = "bar", data = agestd_aou_formatted)),
  nrow=2, ncol=2, align="hv")
#dev.off()

## EOF 