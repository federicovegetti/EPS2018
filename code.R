# Check that needed packages are installed:
want = c("tidyverse", "gridExtra", "ggrepel", "labelled", "reshape2", 
         "here", "rio", "srvyr", "ordinal", "texreg")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# load packages
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

ifelse(!dir.exists("./figures"), dir.create("./figures"), FALSE)
ifelse(!dir.exists("./tables"), dir.create("./tables"), FALSE)

#====================================================================================================
# Import data 
#====================================================================================================
rm(list = ls())
data <- import("./data/dataEPS.csv")
  
#====================================================================================================
# YOUNG ADULTS - RECODES
#====================================================================================================
# Country names
data$cname <- NA
data$cname[data$CCODE == 1040] <- "AT"
data$cname[data$CCODE == 1203] <- "CZ"
data$cname[data$CCODE == 1208] <- "DK"
data$cname[data$CCODE == 1276] <- "DE"
data$cname[data$CCODE == 1300] <- "GR"
data$cname[data$CCODE == 1348] <- "HU"
data$cname[data$CCODE == 1380] <- "IT"
data$cname[data$CCODE == 1724] <- "ES"
data$cname[data$CCODE == 1756] <- "CH"
data$cname[data$CCODE == 1792] <- "TR"
data$cname[data$CCODE == 1826] <- "UK"
data$cname <- factor(data$cname, levels = unique(data$cname)[order(unique(data$CCODE))])

# Employment status
data$work <- NA
data$work[data$YQ14b == 1] <- "Employee"
data$work[data$YQ14b == 2] <- "Self-Employed"
data$work[data$YQ14b == 3] <- "Unemployed"
data$work[data$YQ14b == 4 | data$YQ14b == 5] <- "In Education/Training"
data$work[data$YQ14b >= 6 & data$YQ14b < 10] <- "Inactive"
data$work[data$YQ14b == 8 & data$YQ14a_1 == 1] <- "Employee"
data$work[data$YQ14b == 8 & data$YQ14a_2 == 1] <- "Self-Employed"
data$work <- factor(data$work, levels = unique(data$work)[c(1, 5, 3, 4, 2)])

# How many activities?
data$n_work <- rowSums(data[, paste0("YQ14a_", 1:9)])
round(prop.table(with(data, table(n_work))), 2)
round(prop.table(with(data, table(cname, n_work)), margin = 1)*100, 1)

# Income independence
data$indinc <- NA
data$indinc[(data$YQ10_a == 0 & data$YQ10_b == 0 & data$YQ10_c == 0 & 
               data$YQ10_f == 0 & data$YQ10_h == 0) & 
              (data$YQ10_d == 1 | data$YQ10_e == 1 | data$YQ10_g == 1)] <- "Fully\nDependent"
data$indinc[(data$YQ10_a == 1 | data$YQ10_b == 1 | data$YQ10_c == 1 | 
               data$YQ10_f == 1 | data$YQ10_h == 1) & 
              (data$YQ10_d == 1 | data$YQ10_e == 1 | data$YQ10_g == 1)] <- "Partially\nDependent"
data$indinc[(data$YQ10_a == 1 | data$YQ10_b == 1) & 
              data$YQ10_d == 0 & data$YQ10_e == 0 & data$YQ10_g == 0] <- "Fully\nIndependent"
data$indinc <- factor(data$indinc, 
                      levels = c("Fully\nDependent", "Partially\nDependent", "Fully\nIndependent"))

# Has moved out from parents' place?
data$movedout <- NA
data$movedout[data$YQ28a_2 != 1 & data$YQ28a_3 != 1 & data$YQ28a_6 != 1 & data$YQ28a_7 != 1] <- 1
data$movedout[data$YQ28a_2 == 1 | data$YQ28a_3 == 1 | data$YQ28a_6 == 1 | data$YQ28a_7 == 1] <- 0

# Has came back to parents' place?
data$boomerang <- NA
data$boomerang[data$YQ28d == 1 & data$YQ28a_2 == 1 | data$YQ28d == 1 & data$YQ28a_3 == 1] <- 1
data$boomerang[data$YQ28d == 2 & data$YQ28a_2 == 1 | data$YQ28d == 2 & data$YQ28a_3 == 1] <- 0

# Self-attested economic self-sufficiency
ess <- paste0("YQ9_", letters[1:4])
# Recode (not necessary, but better to keep it)
data[, ess] <- data[, ess] %>% mutate_all(funs(ifelse(. < 0, NA, .)))
rm(ess)
# Afford basics
data$essbasic <- rowMeans(data[, c("YQ9_a", "YQ9_c")], na.rm = T)
# Afford extras
data$essextra <- rowMeans(data[, c("YQ9_b", "YQ9_d")], na.rm = T)

# Is satisfied with own financial situation?
data$finsatdummy <- 0
data$finsatdummy[data$YQ8 == 3 & data$cname != "CH" & data$cname != "CZ" | 
                   data$YQ8 == 4 & data$cname != "CH" & data$cname != "CZ"] <- 1
data$finsatdummy[is.na(data$YQ8)] <- NA
data$finsatdummy[data$YQ8 == 4 & data$cname == "CH" |
                   data$YQ8 == 5 & data$cname == "CH"] <- 1
data$finsatdummy[data$YQ8 == 3 & data$cname == "CH"] <- NA
data$finsatdummy[data$YQ8 == 4 & data$cname == "CZ" & data$PRETEST == 1 |
                   data$YQ8 == 5 & data$cname == "CZ" & data$PRETEST == 1] <- 1
data$finsatdummy[data$YQ8 == 3 & data$cname == "CZ" & data$PRETEST == 0 |
                   data$YQ8 == 4 & data$cname == "CZ" & data$PRETEST == 0] <- 1
data$finsatdummy[data$YQ8 == 3 & data$cname == "CZ" & data$PRETEST == 1] <- NA

# Welfare attitudes
# YQ21_c: If welfare benefits are too high there is no incentive to find work
# YQ21_f: Everyone should have the right to a minimum income even if they are not working
# 1: Strongly disagree, 4: Strongly agree
data[, c("YQ21_c", "YQ21_f")] <- data[, c("YQ21_c", "YQ21_f")] %>% mutate_all(funs(ifelse(. < 0, NA, .)))
data$welfare <- 5 - data$YQ21_c
data$mininc <- data$YQ21_f

# Gender
data$female <- ifelse(data$YQ42 == 2, "Female", "Male")

# Survey mode - young adult
data$mode[data$YIMODE == 1] <- "Online"
data$mode[data$YIMODE == 3] <- "CAPI"
data$mode[data$YIMODE == 4] <- "Paper&Pencil (F2F)"



#====================================================================================================
# PARENTS - RECODES
#====================================================================================================
### Parents
# Select only cases where we have the parents as well
data.p <- subset(data, !is.na(PMIMODE) | !is.na(PFIMODE))

# Stack
var.cpl <- as.list(data.frame(rbind(names(data.p)[grep("PM", names(data.p))], 
                                    names(data.p)[grep("PF", names(data.p))])))
names(var.cpl) <- sub("PM", "PT", names(data)[grep("PM", names(data.p))])
var.cpl <- lapply(var.cpl, function(x) as.character(x))

data.p <- reshape(data.p, direction = "long",
                  varying = var.cpl,
                  v.names = names(var.cpl),
                  times = c("Mother","Father"), 
                  timevar = "PARENT",
                  idvar = "YRESID")
rm(var.cpl)

# Drop all cases where parent information is not there
data.p <- data.p[which(rowSums(is.na(data.p[, which(substring(names(data.p), 1, 2) == "PT")])) < 
                         ncol(data.p[, which(substring(names(data.p), 1, 2) == "PT")])), ]


# Self-attested economic self-sufficiency (parents)
ess <- paste0("PTQ11_", letters[1:4])
# Recode
data.p[, ess] <- data.p[, ess] %>% mutate_all(funs(ifelse(. < 0, NA, .)))
rm(ess)
# Afford basics
data.p$p.essbasic <- rowMeans(data.p[, c("PTQ11_a", "PTQ11_c")], na.rm = T)
# Afford extras
data.p$p.essextra <- rowMeans(data.p[, c("PTQ11_b", "PTQ11_d")], na.rm = T)
# Difference
data.p$essbasic_diff <- data.p$p.essbasic - data.p$essbasic
data.p$essextra_diff <- data.p$p.essextra - data.p$essextra

# Is satisfied with own financial situation? (parents)
data.p$p.finsatdummy <- NA
data.p$p.finsatdummy[data.p$PTQ10 == 1 | data.p$PTQ10 == 2] <- 0
data.p$p.finsatdummy[data.p$PTQ10 == 3 | data.p$PTQ10 == 4] <- 1
# Parent + Child satisfaction
data.p$finsat_tot <- rowMeans(cbind(data.p$p.finsatdummy, data.p$finsatdummy), na.rm = T)

# Welfare attitudes
# YQ21_c: If welfare benefits are too high there is no incentive to find work
# YQ21_f: Everyone should have the right to a minimum income even if they are not working
# 1: Strongly disagree, 4: Strongly agree
data.p[, c("PTQ18_c", "PTQ18_f")] <- data.p[, c("PTQ18_c", "PTQ18_f")] %>% mutate_all(funs(ifelse(. < 0, NA, .)))
data.p$p.welfare <- 5 - data.p$PTQ18_c
data.p$p.mininc <- data.p$PTQ18_f
# Difference
data.p$welfare_diff <- data.p$p.welfare - data.p$welfare
data.p$mininc_diff <- data.p$p.mininc - data.p$mininc

# Survey mode (parent)
data.p$p.mode[data.p$PTIMODE == 1] <- "Online"
data.p$p.mode[data.p$PTIMODE == 2] <- "CATI"
data.p$p.mode[data.p$PTIMODE == 3] <- "CAPI"
data.p$p.mode[data.p$PTIMODE == 4] <- "Paper&Pencil (F2F)"
data.p$p.mode[data.p$PTIMODE == 5] <- "Paper&Pencil (Mail)"

#====================================================================================================
# TABLE 1 - UNWEIGHTED FREQUENCIES (not exporting a table, read from the output)
#====================================================================================================
# N Young Adults by country
with(data, table(cname))
# Gender
round(prop.table(with(data, table(cname, female)), margin = 1)*100)
# Mode
with(data, table(cname, mode))
# N Parents by country
data.p %>% group_by(YRESID) %>%
  mutate(parent = paste0(PARENT, collapse = ",")) %>%
  select(cname, parent) %>%
  ungroup() %>%
  group_by(cname, parent) %>%
  summarize(n = n()) %>%
  spread(parent, n) %>%
  mutate(Both = `Mother,Father`/2) %>% # Obs with both mother and father are counted twice
  select(-`Mother,Father`) 
# Mode Parents
round(prop.table(with(data.p, table(cname, p.mode)), margin = 1)*100)

#====================================================================================================
# FIGURE A1 (employment status)
#====================================================================================================
(data %>%
    select(PWEIGHT_1, cname, work) %>% 
    filter(!is.na(work), !is.na(PWEIGHT_1)) %>%
    as_survey(weights = PWEIGHT_1) %>%
    group_by(cname, work) %>%
    summarize(m = survey_mean()) %>%
    mutate(cname = factor(cname, levels = 
                            unique(cname)[order(m[work == "Employee"] + 
                                                  m[work == "Self-Employed"],
                                                decreasing = T)])) %>%
    ggplot(., aes(x = cname, y = m, group = work)) +
    geom_bar(aes(fill = work), color = "black", alpha = 0.7, stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("black", "gray20", "gray40", "gray60", "gray80"), "") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
    xlab("") + ylab("Share") +
    theme_bw() + 
    theme(legend.position = "bottom")) %>%
  ggsave(filename = "./figures/figure_A1.png", 
         device = "png", dpi = 300, width = 7, height = 3.5)

#====================================================================================================
# FIGURE 1 - ECONOMIC SELF-SUFFICIENCY (4 indicators)
#====================================================================================================
# Income independence
p1 <- (data %>%
  select(PWEIGHT_1, cname, indinc) %>% 
  filter(!is.na(PWEIGHT_1), !is.na(indinc)) %>%
  as_survey(weights = PWEIGHT_1) %>%
  group_by(cname, indinc) %>%
  summarize(m = survey_mean()) %>%
  mutate(m_o = m * c(0, 0, 1)) %>%
  ggplot(., aes(x = reorder(cname, -m_o), y = m, 
                fill = indinc)) +
  geom_bar(col = "black", alpha = 0.7, stat = "identity", position = "stack") +
  scale_fill_manual(values = c("gray20", "gray40", "gray70"), "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent) +
  xlab("") + ylab("Share") +
  theme_bw() +
  ggtitle("Income Independence") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11)))

# Moved out, never moved out, boomeranger
p2 <- (data %>%
  mutate(variable = ifelse(movedout == 1, "Has Moved\nOut",
                           ifelse(boomerang == 1, "Boomeranger", "Has Never\nMoved Out")),
         variable = factor(variable, levels = c("Has Never\nMoved Out", "Boomeranger", "Has Moved\nOut"))) %>%
  select(PWEIGHT_1, cname, variable) %>% 
  filter(!is.na(PWEIGHT_1), !is.na(variable)) %>%
  as_survey(weights = PWEIGHT_1) %>%
  group_by(cname, variable) %>%
  summarize(m = survey_mean()) %>%
  mutate(m_o = m * c(0, 0, 1)) %>%
  ggplot(., aes(x = reorder(cname, -m_o), y = m, 
                fill = variable)) +
  geom_bar(col = "black", alpha = 0.7, stat = "identity", position = "stack") +
  scale_fill_manual(values = c("gray20", "gray40", "gray70"), "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent) +
  xlab("") + ylab("Share") +
  theme_bw() +
  ggtitle("Housing Situation") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11)))

# Self-Assessed Economic Condition
p3 <- (data %>% 
  select(PWEIGHT_1, cname, essextra, essbasic) %>% 
  filter(!is.na(essextra), !is.na(essbasic), !is.na(PWEIGHT_1)) %>%
  gather(variable, value, -c(cname, PWEIGHT_1)) %>%
  mutate(variable = ifelse(variable == "essextra", 
                           "Ability to\nAfford Extras", 
                           "Ability to\nAfford Basics")) %>%
  as_survey(weights = PWEIGHT_1) %>%
  group_by(cname, variable) %>%
  summarize(m = survey_mean(value)) %>%
  ggplot(., aes(x = reorder(cname, -m, mean), y = m, 
                fill = variable)) +
  geom_bar(col = "black", alpha = 0.7, stat = "identity", 
           position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("gray20", "gray70"), "") +
  coord_cartesian(ylim = c(1, 4)) +
  scale_y_continuous(breaks = 1:4, labels = c("1 - Min", "2", "3", "4 - Max")) +
  xlab("") + ylab("Mean") +
  theme_bw() +
  ggtitle("Self-Assessed Economic Condition") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11)))

# Satisfaction with financial situation
colbar <- c("Is Satisfied by Own\nFinancial Situation" = "gray70")
p4 <- (data %>% 
  select(PWEIGHT_1, cname, finsatdummy) %>% 
  filter(!is.na(finsatdummy), !is.na(PWEIGHT_1)) %>%
  as_survey(weights = PWEIGHT_1) %>%
  group_by(cname) %>%
  summarize(m = survey_mean(finsatdummy)) %>%
  ggplot(., aes(x = reorder(cname, -m))) +
  geom_bar(aes(y = m, fill = "Is Satisfied by Own\nFinancial Situation"), 
           col = "black", alpha = 0.7, stat = "identity") +
  scale_fill_manual(name = "", values = colbar) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  xlab("") + ylab("Share") +
  theme_bw() +
  ggtitle("Financial Satisfaction") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11)))

full <- grid.arrange(p1, p2, p3, p4, ncol = 2)
rm(colbar)

ggsave(full, filename = "./figures/figure_1.png", 
       device = "png", dpi = 300, width = 7, height = 7)
rm(p1, p2, p3, p4, full)


#====================================================================================================
# FIGURE 2 - COMPARING PARENTS - CHILDREN
#====================================================================================================
# Self-Assessed Economic Condition
pp.1 <- data.p %>% 
    select(PWEIGHT_1, cname, essextra, essbasic, 
           p.essextra, p.essbasic) %>% 
    filter(!is.na(essextra), !is.na(essbasic), 
           !is.na(p.essextra), !is.na(p.essbasic), 
           !is.na(PWEIGHT_1)) %>%
    mutate(id = 1:n()) %>%
    gather(variable, value, -c(id, cname, PWEIGHT_1)) %>%
    mutate(variable = recode(variable,
                             "essextra" = "c_essextra",
                             "essbasic" = "c_essbasic",
                             "p.essextra" = "p_essextra",
                             "p.essbasic" = "p_essbasic")) %>%
    separate(variable, c("group", "variable"), sep = "_") %>%
    spread(group, value) %>%
    mutate(variable = ifelse(variable == "essextra", 
                             "Ability to Afford Extras", 
                             "Ability to Afford Basics")) %>%
    as_survey(weights = PWEIGHT_1) %>%
    group_by(cname, variable) %>%
    summarize(c = survey_mean(c),
              p = survey_mean(p)) %>%
    group_by(variable) %>%
    mutate(me_pa = median(p),
           me_ch = median(c)) %>%
    ggplot(., aes(x = p, y = c)) +
    geom_vline(aes(xintercept = me_pa), linetype = 3) +
    geom_hline(aes(yintercept = me_ch), linetype = 3) +
    geom_abline(aes(intercept = 0, slope = 1), col = "gray60") +
    geom_point() + geom_text_repel(aes(label = cname), size = 3) +
    facet_wrap(~variable, ncol = 2) +
    scale_y_continuous(limits = c(1, 4), breaks = 1:4, 
                       labels = c("1 - Min", "2", "3", "4 - Max")) +
    scale_x_continuous(limits = c(1, 4), breaks = 1:4, 
                       labels = c("1 - Min", "2", "3", "4 - Max")) +
    xlab("Mean Parents") + ylab("Mean Children") +
    theme_bw() +
    theme(axis.title.y = element_text(vjust = -150),
          axis.text.x = element_text(hjust = c(0.2, 0.5, 0.5, 0.8)))


# Satisfaction with financial situation
pp.2 <- data.p %>%
    select(PWEIGHT_1, cname, finsatdummy, p.finsatdummy) %>% 
    filter(!is.na(PWEIGHT_1), !is.na(finsatdummy), !is.na(p.finsatdummy)) %>%
    as_survey(weights = PWEIGHT_1) %>%
    group_by(cname) %>%
    summarize(m_ch = survey_mean(finsatdummy),
              m_pa = survey_mean(p.finsatdummy)) %>%
    mutate(me_pa = median(m_pa),
           me_ch = median(m_ch),
           title = "Financial Satisfaction") %>%
    ggplot(., aes(x = m_pa, y = m_ch)) +
    geom_vline(aes(xintercept = me_pa), linetype = 3) +
    geom_hline(aes(yintercept = me_ch), linetype = 3) +
    geom_abline(aes(intercept = 0, slope = 1), col = "gray60") +
    geom_point() + geom_text_repel(aes(label = cname), size = 3) +
    facet_wrap(~title) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), 
                       labels = scales::percent) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), 
                       labels = scales::percent) +
    xlab("Share Satisfied Parents") + ylab("Share Satisfied Children") +
    theme_bw()

full <- grid.arrange(pp.1, pp.2, layout_matrix = rbind(c(1, 1, 1), c(NA, 2, 2, 2, NA)))

ggsave(full, filename = "./figures/figure_2.png", 
       device = "png", dpi = 300, width = 7, height = 7)
rm(pp.1, pp.2, full)



#====================================================================================================
# FIGURE 3 - WELFARE ATTITUDES + MINIMUM INCOME - comparison P+C
#====================================================================================================
(data.p %>% 
    select(PWEIGHT_1, cname, welfare, p.welfare, mininc, p.mininc) %>% 
    filter(!is.na(PWEIGHT_1)) %>%
    reshape(., direction = "long", 
            varying = list(c("welfare", "mininc"),
                           c("p.welfare", "p.mininc")),
            timevar = "variable",
            times = c("Welfare Benefits", "Universal Basic Income"),
            v.names = c("child", "paren")) %>%
    as_survey(weights = PWEIGHT_1) %>%
    group_by(cname, variable) %>%
    summarize(child = survey_mean(child, na.rm = T),
              paren = survey_mean(paren, na.rm = T)) %>%
    group_by(variable) %>%
    mutate(m_c = median(child),
           m_p = median(paren)) %>%
    ggplot(., aes(x = paren, y = child)) +
    geom_vline(aes(xintercept = m_p), linetype = 3) +
    geom_hline(aes(yintercept = m_c), linetype = 3) +
    geom_abline(aes(intercept = 0, slope = 1), col = "gray60") +
    geom_point() + geom_text_repel(aes(label = cname), size = 3) +
    facet_wrap(~variable, ncol = 2) +
    scale_y_continuous(limits = c(1, 4), breaks = 1:4, 
                       labels = c("1 - Negative", "2", "3", "4 - Positive")) +
    scale_x_continuous(limits = c(1, 4), breaks = 1:4, 
                       labels = c("1 - Negative", "2", "3", "4 - Positive")) +
    xlab("Mean Parents") + ylab("Mean Children") +
    theme_bw() +
    theme(axis.title.y = element_text(vjust = -200),
          axis.text.x = element_text(hjust = c(0.2, 0.5, 0.5, 0.8)))) %>%
  ggsave(filename = "./figures/figure_3.png", 
         device = "png", dpi = 300, width = 7, height = 3.7)

#====================================================================================================
# FIGURE 6 - CORRELATES of WELFARE ATTITUDES + MINIMUM INCOME
#====================================================================================================
m.1 <- clm(factor(mininc) ~ movedout + factor(indinc) + essbasic + essextra + finsatdummy + 
             p.mininc + p.essbasic + p.essextra + p.finsatdummy +
             factor(cname),
           data = subset(data.p, !is.na(PWEIGHT_1)), weights = PWEIGHT_1)
summary(m.1)

m.2 <- clm(factor(welfare) ~ movedout + factor(indinc) + essbasic + essextra + finsatdummy + 
             p.welfare + p.essbasic + p.essextra + p.finsatdummy +
             factor(cname),
           data = subset(data.p, !is.na(PWEIGHT_1)), weights = PWEIGHT_1)
summary(m.2)



c.names.1 <- c("Has Moved Out", "Partially Dependent\nIncome", "Fully Independent\nIncome",
               "Ability to Afford Basics", "Ability to Afford Extras", 
               "Financial Satisfaction",
               "Attitude Towards\nUniversal Basic Income",
               "Ability to Afford Basics", "Ability to Afford Extras", 
               "Financial Satisfaction")

d.coef.1 <- data.frame(names = c.names.1,
                       beta = coef(m.1)[which(names(coef(m.1)) == "movedout"):
                                          which(names(coef(m.1)) == "p.finsatdummy")],
                       se = sqrt(diag(vcov(m.1)))[which(names(diag(vcov(m.1))) == "movedout"):
                                                    which(names(diag(vcov(m.1))) == "p.finsatdummy")],
                       parent = c(rep("Young Adults Indicators", 6),
                                  rep("Parents Indicators", 4)),
                       attitude = "Attitude Towards\nUniversal Basic Income",
                       stringsAsFactors = F)

c.names.2 <- c("Has Moved Out", "Partially Dependent\nIncome", "Fully Independent\nIncome",
               "Ability to Afford Basics", "Ability to Afford Extras", 
               "Financial Satisfaction",
               "Attitude Towards\nWelfare Benefits",
               "Ability to Afford Basics", "Ability to Afford Extras", 
               "Financial Satisfaction")

d.coef.2 <- data.frame(names = c.names.2,
                       beta = coef(m.2)[which(names(coef(m.2)) == "movedout"):
                                          which(names(coef(m.2)) == "p.finsatdummy")],
                       se = sqrt(diag(vcov(m.2)))[which(names(diag(vcov(m.2))) == "movedout"):
                                                    which(names(diag(vcov(m.2))) == "p.finsatdummy")],
                       parent = c(rep("Young Adults Indicators", 6),
                                  rep("Parents Indicators", 4)),
                       attitude = "Attitude Towards\nWelfare Benefits",
                       stringsAsFactors = F)

d.coef <- bind_rows(d.coef.1, d.coef.2)
d.coef$parent <- factor(d.coef$parent, levels = unique(d.coef$parent))
d.coef$names <- factor(d.coef$names, levels = rev(unique(d.coef$names)))
rm(d.coef.1, d.coef.2)


(ggplot(d.coef, aes(x = names, y = beta, 
                    ymin = beta - 1.96*se, 
                    ymax = beta + 1.96*se)) +
    geom_hline(aes(yintercept = 0), col = "gray60") +
    geom_pointrange(shape = 21, fill = "white", size = 0.3, stroke = 0.5) +
    facet_grid(parent ~ attitude, scales = "free_y", space = "free_y") +
    coord_flip() +
    xlab("Independent Variables") + ylab("Coefficients (95% C.I.)") +
    theme_bw()) %>%
  ggsave(filename = "./figures/figure_4.png", 
         device = "png", dpi = 300, width = 7, height = 5)

#====================================================================================================
# Table A1 - Full regression results
#====================================================================================================

coefs <- list("movedout" = "Has Moved Out", 
              "factor(indinc)Partially\nDependent" = "Partially Dependent Income",
              "factor(indinc)Fully\nIndependent" = "Fully Independent Income",
              "essbasic" = "Ability to Afford Basics",
              "essextra" = "Ability to Afford Extras",
              "finsatdummy" = "Financial Satisfaction",
              "p.mininc" = " Attitude Towards Universal Basic Income",
              "p.welfare" = " Attitude Towards Welfare Benefits",
              "p.essbasic" = " Ability to Afford Basics",
              "p.essextra" = " Ability to Afford Extras",
              "p.finsatdummy" = " Financial Satisfaction",
              "factor(cname)CZ" = "CZ",
              "factor(cname)DK" = "DK",
              "factor(cname)DE" = "DE",
              "factor(cname)GR" = "GR",
              "factor(cname)HU" = "HU",
              "factor(cname)IT" = "IT",
              "factor(cname)ES" = "ES",
              "factor(cname)CH" = "CH",
              "factor(cname)TR" = "TR",
              "factor(cname)UK" = "UK",
              "1|2" = "1|2",
              "2|3" = "2|3",
              "3|4" = "3|4")
              

htmlreg(list(m.1, m.2), 
        file = "./tables/table_A1.doc", 
        custom.coef.map = coefs,
        single.row = TRUE,
        groups = list("Young Adults Indicators" = 1:6,
                      "Parents Indicators" = 7:11,
                      "Country Dummies (baseline: AT)" = 12:21,
                      "Cut Points" = 22:24),
        custom.model.names = c("Universal Basic Income", "Welfare Benefits"),
        caption = "Table A1: Ordinal logistic regression models for attitudes towards 
        universal basic income and welfare benefits. Source: CUPESSE.",
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)
