##### Import Packages ####
library(rio)
library(tidyverse)
library(officer)
library(flextable)
library(poLCA)

# R's default pdf() maps the hyphen to U+2212 (minus) in the embedded font, so
# hyphenated labels such as "data-protection" render with a minus sign. quartz
# keeps U+002D on macOS; cairo_pdf does elsewhere.
pdf_dev <- if (capabilities("aqua")) {
  function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...)
} else grDevices::cairo_pdf

#### Cleaning and basic descriptives ####
Data <- import("Data.xlsx")
table(Data$Finished)
table(Data$Progress)
Data = filter(Data, as.numeric(Progress) > 90) # 470

# Qualtrics exports labels with trailing spaces on some questions
Data <- Data %>% mutate(across(where(is.character), trimws))

Data <- Data %>%
  rename(Job = Q2,
         Career = Q11,
         Gender = Q3,
         Country = Q6,
         Region_expertise = Q7,
         Country_expertise = Q9,
         Disciplinary_background = Q12)

# Job, Career, Gender
table(Data$Job)
353/470 # 75% academic

table(Data$Career)
180/470 # 38% mid-career

table(Data$Gender)
291/470 # 62% men

# Duration of the survey
median(as.numeric(Data$`Duration (in seconds)`), na.rm=T)
1199.5/60 # 20 minutes

# Background
table(Data$Disciplinary_background)
318/470 # 68% social sciences

# Confidence in the answers provided
table(Data$Confidence)
(275+168)/470 # 94% moderately or very confident

# Country and region of expertise
table(Data$Country)
table(Data$Country_expertise)
length(unique(Data$Country_expertise)) # 71 countries


# Region of expertise

table(Data$Region_expertise)

split_regions <- function(x) {
  strsplit(x, ",(?![^()]*\\))", perl = TRUE)[[1]] %>% trimws()
}

df <- tibble(region_expertise = Data$Region_expertise)

result <- df %>%
  filter(!is.na(region_expertise)) %>%
  rowwise() %>%
  mutate(region = list(split_regions(region_expertise))) %>%
  unnest(region) %>%
  group_by(region) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

print(result)
result <- tibble(
  region = c(
    "North America",
    "Western Europe",
    "Latin America and the Caribbean",
    "East and Southeast Asia",
    "Sub-Saharan Africa",
    "Eastern Europe and Central Asia",
    "South Asia",
    "Middle East and North Africa (MENA)",
    "Oceania (Australia, New Zealand, Pacific Islands)"),
  count = c(195, 167, 108, 49, 48, 41, 32, 30, 20))
ft <- flextable(result)
ft <- set_table_properties(ft, width = 0.8, align = "center")
ft <- add_header_row(ft, values = c("Region", "Sum"))
print(ft)
doc <- read_docx()
doc <- doc %>%
  body_add_flextable(value = ft)
print(doc, target = "table_regions.docx")


#### Developed vs developing economies ####
# UN Trade and Development classification, same list as the 2025 report

developed_countries <- c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                         "Denmark", "France", "Germany", "Greece", "Ireland", "Israel",
                         "Italy", "Japan", "Netherlands", "New Zealand", "Norway",
                         "Portugal", "Singapore", "Slovakia", "South Korea",
                         "Spain", "Sweden", "Switzerland", "UK", "USA",
                         "Croatia", "San Marino", "Andorra", "Latvia", "Lithuania",
                         "Estonia", "Malta", "Liechtenstein", "Monaco", "Slovenia",
                         "Cyprus", "Taiwan", "Finland", "Iceland", "Luxembourg")

Data$Country_expertise[Data$Country_expertise == "Ireland {Republic}"] <- "Ireland"
Data$Country_expertise[Data$Country_expertise == "Korea South"] <- "South Korea"
Data$Country_expertise[Data$Country_expertise == "United Kingdom"] <- "UK"
Data$Country_expertise[Data$Country_expertise == "United States"] <- "USA"
Data$Country_expertise[Data$Country_expertise == "Russian Federation"] <- "Russia"

Data$Developed <- ifelse(Data$Country_expertise %in% developed_countries,
                         "Developed", "Developing")
table(Data$Developed)
257/470 # 55% developed, 45% developing


##### Healthy Info Env ####
# How important are each of the following to achieve a good, healthy information environment?

Data_HealthInfoEnv <- gather(Data, Q, Response, Q15_1:Q15_7, factor_key=TRUE)

# % Absolutely essential and % Extremely important or Absolutely essential per item
Data_HealthInfoEnv %>%
  group_by(Q) %>%
  summarise(Essential = mean(Response == "Absolutely essential")*100,
            Extremely_or_Essential = mean(Response %in% c("Extremely important", "Absolutely essential"))*100)

# By economy
Data_HealthInfoEnv %>%
  group_by(Q, Developed) %>%
  summarise(Essential = mean(Response == "Absolutely essential")*100)

Data_HealthInfoEnv <- Data_HealthInfoEnv %>%
  mutate(Response = fct_relevel(Response,
                                "Absolutely essential",
                                "Extremely important",
                                "Very important",
                                "Moderately important",
                                "Not very important",
                                "Not important at all",
                                "I don't know"))%>%
  mutate(Q = fct_relevel(Q,
                         "Q15_7",
                         "Q15_6",
                         "Q15_4",
                         "Q15_5",
                         "Q15_2",
                         "Q15_1",
                         "Q15_3" ))%>%
  mutate(Q = recode(Q,
                    Q15_1 = "Diversity of voices",
                    Q15_2 = "Diversity of media ownership",
                    Q15_3 = "Availability of accurate information",
                    Q15_4 = "Absence of misinformation",
                    Q15_5 = "Absence of hateful content",
                    Q15_6 = "Absence of microtargeted political ads",
                    Q15_7 = "Absence of AI-generated content"))%>%
  group_by(Q, Response) %>%
  summarise(Percentage = n()/nrow(Data)*100)

ggplot(Data_HealthInfoEnv, aes(x=Q, y=Percentage, fill=Response)) +
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","#f7cdcd","#ea8181","#d50e00","#E0E0E0"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 8, 8))

ggsave("../Figures/fig_importance_Q15.pdf", width = 12, height = 7.75, device = pdf_dev)
ggsave("../Figures/fig_importance_Q15.png", width = 12, height = 7.75, dpi = 300, bg = "white")


##### Threats ####
# How significant a threat do you think each of the following poses to the
# information environment in your main country of expertise?

Data_Threats <- gather(Data, Q, Response, Q36_1:Q36_6, factor_key=TRUE)

# % Big threat or An extreme threat per item
Data_Threats %>%
  group_by(Q) %>%
  summarise(Big_or_Extreme = mean(Response %in% c("Big threat", "An extreme threat"))*100)

# By economy
Data_Threats %>%
  group_by(Q, Developed) %>%
  summarise(Big_or_Extreme = mean(Response %in% c("Big threat", "An extreme threat"))*100)

Data_Threats <- Data_Threats %>%
  mutate(Response = fct_relevel(Response,
                                "Not a threat at all",
                                "Very small threat",
                                "Small threat",
                                "Moderate threat",
                                "Big threat",
                                "An extreme threat",
                                "I don't know"))%>%
  mutate(Q = fct_relevel(Q,
                         "Q36_2",
                         "Q36_6",
                         "Q36_4",
                         "Q36_1",
                         "Q36_3",
                         "Q36_5"))%>%
  mutate(Q = recode(Q,
                    Q36_1 = "Polarization",
                    Q36_2 = "Generative AI",
                    Q36_3 = "Mis/disinformation",
                    Q36_4 = "Dominance of a few social media platforms",
                    Q36_5 = "Lack of platform accountability",
                    Q36_6 = "Filter bubbles & echo chambers"))%>%
  group_by(Q, Response) %>%
  summarise(Percentage = n()/nrow(Data)*100)

ggplot(Data_Threats, aes(x=Q, y=Percentage, fill=Response)) +
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","#f7cdcd","#ea8181","#d50e00","#E0E0E0"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 8, 8))

ggsave("../Figures/fig_threats_Q36.pdf", width = 12, height = 6.9, device = pdf_dev)
ggsave("../Figures/fig_threats_Q36.png", width = 12, height = 6.9, dpi = 300, bg = "white")


##### Generative AI in the next 5 years ####
# How do you think generative AI tools will affect the information environment
# in the next 5 years?

Data_AI_future <- gather(Data, Q, Response, Q21_1:Q21_4, factor_key=TRUE)

# % expecting the technology to worsen / improve the information environment
Data_AI_future %>%
  group_by(Q) %>%
  summarise(Worsen = mean(Response %in% c("Slightly worsen", "Somewhat worsen", "Greatly worsen"))*100,
            Improve = mean(Response %in% c("Slightly improve", "Somewhat improve", "Greatly improve"))*100)

Data_AI_future <- Data_AI_future %>%
  mutate(Response = fct_relevel(Response,
                                "Greatly improve",
                                "Somewhat improve",
                                "Slightly improve",
                                "Neither",
                                "I don't know",
                                "Slightly worsen",
                                "Somewhat worsen",
                                "Greatly worsen"))%>%
  mutate(Q = fct_relevel(Q,
                         "Q21_4",
                         "Q21_2",
                         "Q21_3",
                         "Q21_1"))%>%
  mutate(Q = recode(Q,
                    Q21_1 = "AI-generated text",
                    Q21_2 = "AI-generated images",
                    Q21_3 = "AI-generated voices",
                    Q21_4 = "AI-generated videos"))%>%
  group_by(Q, Response) %>%
  summarise(Percentage = n()/nrow(Data)*100)

ggplot(Data_AI_future, aes(x=Q, y=Percentage, fill=Response)) +
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray50","#E0E0E0","#f7cdcd","#ea8181","#d50e00"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 8, 8))

ggsave("../Figures/fig_ai_future_Q21.pdf", width = 12, height = 5.2, device = pdf_dev)
ggsave("../Figures/fig_ai_future_Q21.png", width = 12, height = 5.2, dpi = 300, bg = "white")

# Generative AI by discipline: threat today (Q36_2) vs expectations for the next
# 5 years (mean of the four Q21 items below the scale midpoint)
Data$Discipline3 <- case_when(
  grepl("Social", Data$Disciplinary_background) ~ "Social Sciences",
  grepl("STEM|Science, Technology", Data$Disciplinary_background) ~ "STEM",
  grepl("Humanities", Data$Disciplinary_background) ~ "Humanities",
  TRUE ~ "Other")

Data %>%
  group_by(Discipline3) %>%
  summarise(AI_big_threat = mean(Q36_2 %in% c("Big threat", "An extreme threat"))*100)

q21_levels <- c("Greatly worsen" = 1, "Somewhat worsen" = 2, "Slightly worsen" = 3,
                "Neither" = 4, "Slightly improve" = 5, "Somewhat improve" = 6,
                "Greatly improve" = 7)
Data$Q21_mean <- rowMeans(cbind(q21_levels[Data$Q21_1], q21_levels[Data$Q21_2],
                                q21_levels[Data$Q21_3], q21_levels[Data$Q21_4]), na.rm = TRUE)
Data %>%
  group_by(Discipline3) %>%
  summarise(Net_worse = mean(Q21_mean < 4, na.rm = TRUE)*100)


##### AI summaries in search (new in 2026) ####
# To what extent do you think AI summaries in search engines will improve or
# worsen the following outcomes of a search in your main country of expertise?

Data_AI_summaries <- gather(Data, Q, Response, Q43_1:Q43_5, factor_key=TRUE)

# % expecting improvement / worsening per outcome
Data_AI_summaries %>%
  group_by(Q) %>%
  summarise(Improve = mean(Response %in% c("Improve a little", "Improve", "Improve a lot"))*100,
            Worsen = mean(Response %in% c("Worsen a little", "Worsen", "Worsen a lot"))*100)

# By economy
Data_AI_summaries %>%
  group_by(Q, Developed) %>%
  summarise(Improve = mean(Response %in% c("Improve a little", "Improve", "Improve a lot"))*100)

Data_AI_summaries <- Data_AI_summaries %>%
  mutate(Response = fct_relevel(Response,
                                "Improve a lot",
                                "Improve",
                                "Improve a little",
                                "No change",
                                "I don't know",
                                "Worsen a little",
                                "Worsen",
                                "Worsen a lot"))%>%
  mutate(Q = fct_relevel(Q,
                         "Q43_5",
                         "Q43_3",
                         "Q43_2",
                         "Q43_4",
                         "Q43_1"))%>%
  mutate(Q = recode(Q,
                    Q43_1 = "Speed of finding relevant answers",
                    Q43_2 = "Accuracy of information encountered",
                    Q43_3 = "Diversity of viewpoints encountered",
                    Q43_4 = "Accuracy of conclusions drawn",
                    Q43_5 = "Number of visits to news websites"))%>%
  group_by(Q, Response) %>%
  summarise(Percentage = n()/nrow(Data)*100)

ggplot(Data_AI_summaries, aes(x=Q, y=Percentage, fill=Response)) +
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray50","#E0E0E0","#f7cdcd","#ea8181","#d50e00"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 8, 8))

ggsave("../Figures/fig_ai_summaries_Q43.pdf", width = 12, height = 6.05, device = pdf_dev)
ggsave("../Figures/fig_ai_summaries_Q43.png", width = 12, height = 6.05, dpi = 300, bg = "white")


##### Effects of technologies on society ####
# In your main country of expertise, would you say that the effects of the
# following technologies and phenomena on society have been rather positive or negative?

Data_Tech <- gather(Data, Q, Response, Q46_1:Q46_4, factor_key=TRUE)

# % positive / negative per technology
Data_Tech %>%
  group_by(Q) %>%
  summarise(Positive = mean(Response %in% c("Slightly positive", "Positive", "Very positive"))*100,
            Negative = mean(Response %in% c("Slightly negative", "Negative", "Very negative"))*100)

# By economy
Data_Tech %>%
  group_by(Q, Developed) %>%
  summarise(Positive = mean(Response %in% c("Slightly positive", "Positive", "Very positive"))*100)

Data_Tech <- Data_Tech %>%
  mutate(Response = fct_relevel(Response,
                                "Very positive",
                                "Positive",
                                "Slightly positive",
                                "Neither",
                                "I don't know",
                                "Slightly negative",
                                "Negative",
                                "Very negative"))%>%
  mutate(Q = fct_relevel(Q,
                         "Q46_2",
                         "Q46_3",
                         "Q46_4",
                         "Q46_1"))%>%
  mutate(Q = recode(Q,
                    Q46_1 = "Search engines",
                    Q46_2 = "Recommender systems",
                    Q46_3 = "Generative AI",
                    Q46_4 = "Social media"))%>%
  group_by(Q, Response) %>%
  summarise(Percentage = n()/nrow(Data)*100)

ggplot(Data_Tech, aes(x=Q, y=Percentage, fill=Response)) +
  geom_bar(stat = "identity", width = 0.7)+
  coord_flip()+
  scale_fill_manual(values = c("#458cff", "#82c2ff","#bef7ff","gray50","#E0E0E0","#f7cdcd","#ea8181","#d50e00"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 8, 8))

ggsave("../Figures/fig_tech_effects_Q46.pdf", width = 12, height = 5.2, device = pdf_dev)
ggsave("../Figures/fig_tech_effects_Q46.png", width = 12, height = 5.2, dpi = 300, bg = "white")


##### Future of the information environment ####
# How confident are you that the information environment will improve or worsen
# in the next year in your main country of expertise?

table(Data$Q25)
(171+184)/470 # 75% moderately or very confident it will worsen
(24+43)/470   # 14% expect improvement
41/470        # 9% expect no change

# By economy
Data %>%
  group_by(Developed) %>%
  summarise(Worsen = mean(Q25 %in% c("Very confident it will worsen",
                                     "Moderately confident it will worsen"))*100)

Data_Future <- Data %>%
  mutate(Q25 = ifelse(Q25 == "Believe that it will neither improve nor worsen", "Neither", Q25)) %>%
  mutate(Q25 = fct_relevel(Q25,
                           "Very confident it will worsen",
                           "Moderately confident it will worsen",
                           "Neither",
                           "I don't know/Prefer not to say",
                           "Moderately confident it will improve",
                           "Very confident it will improve"))%>%
  mutate(Q25 = recode(Q25,
                      "Very confident it will worsen" = "Worsen\nVery confident",
                      "Moderately confident it will worsen" = "Worsen\nModerately confident",
                      "I don't know/Prefer not to say" = "I don't know",
                      "Moderately confident it will improve" = "Improve\nModerately confident",
                      "Very confident it will improve" = "Improve\nVery confident"))%>%
  group_by(Q25) %>%
  summarise(Percentage = round(n()/nrow(Data)*100))

ggplot(Data_Future, aes(x=Q25, y=Percentage, fill=Q25)) +
  geom_bar(stat = "identity", width = 0.85)+
  scale_fill_manual(values = c("#b2182b","#ea8181","gray50","#E0E0E0","#82c2ff","#458cff"))+
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.4, size = 6.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 15, color = "black", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 15, color = "black"),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(12, 12, 8, 8))

ggsave("../Figures/fig_future_outlook_Q25.pdf", width = 12, height = 7, device = pdf_dev)
ggsave("../Figures/fig_future_outlook_Q25.png", width = 12, height = 7, dpi = 300, bg = "white")


##### Barriers to research ####
# What barriers or challenges do you currently face in your work on the
# information environment? Select all that apply.

Data$Funding <- as.integer(grepl("Funding opportunities", Data$Q47))
Data$DataAccess <- as.integer(grepl("Data access", Data$Q47))
Data$PoliticalPressures <- as.integer(grepl("Pressures to align current research", Data$Q47))
Data$GovFunding <- as.integer(grepl("Government control over research funding", Data$Q47))
Data$Privacy <- as.integer(grepl("Privacy or data-protection restrictions", Data$Q47))
Data$Censorship <- as.integer(grepl("Government censorship", Data$Q47))
Data$Surveillance <- as.integer(grepl("Government surveillance or monitoring", Data$Q47))
Data$PrivateCompanies <- as.integer(grepl("Pressures from private companies", Data$Q47))

Data_Barriers <- tibble(
  Barrier = c("Funding opportunities",
              "Data access",
              "Pressure to align with a political agenda",
              "Government control of research funding",
              "Privacy or data-protection restrictions",
              "Government censorship",
              "Government surveillance",
              "Pressures from private companies"),
  Percentage = c(mean(Data$Funding),
                 mean(Data$DataAccess),
                 mean(Data$PoliticalPressures),
                 mean(Data$GovFunding),
                 mean(Data$Privacy),
                 mean(Data$Censorship),
                 mean(Data$Surveillance),
                 mean(Data$PrivateCompanies))*100)
Data_Barriers

# By economy
Data %>%
  group_by(Developed) %>%
  summarise(Funding = mean(Funding)*100,
            DataAccess = mean(DataAccess)*100,
            PoliticalPressures = mean(PoliticalPressures)*100,
            GovFunding = mean(GovFunding)*100,
            Privacy = mean(Privacy)*100,
            Censorship = mean(Censorship)*100,
            Surveillance = mean(Surveillance)*100,
            PrivateCompanies = mean(PrivateCompanies)*100)

Data_Barriers <- Data_Barriers %>%
  mutate(Barrier = fct_reorder(Barrier, Percentage))

ggplot(Data_Barriers, aes(x=Barrier, y=Percentage)) +
  geom_bar(stat = "identity", fill = "lightgoldenrod1", color = "black", width = 0.7)+
  geom_text(aes(label = paste0(round(Percentage), "%")), hjust = -0.18, size = 6)+
  coord_flip(clip = "off")+
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = c(0, 0.05)))+
  theme_bw(base_size = 16)+
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 19, color = "black"),
        axis.text.x = element_text(size = 15, color = "black"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 24, 8, 8))

ggsave("../Figures/fig_barriers_Q47.pdf", width = 12, height = 6.5, device = pdf_dev)
ggsave("../Figures/fig_barriers_Q47.png", width = 12, height = 6.5, dpi = 300, bg = "white")


##### Focus on the United States ####

Data$US <- ifelse(Data$Country_expertise == "USA", "US", "Rest")

Data %>%
  group_by(US) %>%
  summarise(GovFunding = mean(GovFunding)*100,          # 50% vs 27%
            PoliticalPressures = mean(PoliticalPressures)*100, # 48% vs 29%
            Worsen = mean(Q25 %in% c("Very confident it will worsen",
                                     "Moderately confident it will worsen"))*100)

# US experts vs experts on other developed economies
Data %>%
  filter(Developed == "Developed") %>%
  group_by(US) %>%
  summarise(Worsen = mean(Q25 %in% c("Very confident it will worsen",
                                     "Moderately confident it will worsen"))*100) # 73% vs 85%

# Experts reporting political or funding pressure are not more pessimistic
Data$Pressured <- as.integer(Data$PoliticalPressures == 1 | Data$GovFunding == 1)
Data$Worsen <- as.integer(Data$Q25 %in% c("Very confident it will worsen",
                                          "Moderately confident it will worsen"))
Data %>%
  group_by(Pressured) %>%
  summarise(Worsen = mean(Worsen)*100) # 77% vs 73%
summary(glm(Worsen ~ Pressured, data = Data, family = binomial)) # p = .32
summary(glm(Worsen ~ Pressured + Developed, data = Data, family = binomial))


##### Country-block items reported in SR2026.3 ####

# How easily can people find diverse viewpoints online regarding political issues?
table(Data$Q_6_1)
(216+142)/470 # 76% at least somewhat easily
Data %>%
  group_by(Developed) %>%
  summarise(Easy = mean(grepl("easy", Q_6_1))*100) # 84% vs 67%

# How much of the population trusts the main news outlets to report accurately about politics?
table(Data$Q_5_1)
(101+31)/470 # 28% more than half
Data %>%
  group_by(Developed) %>%
  summarise(MoreThanHalf = mean(Q_5_1 %in% c("Many, more than half of the population.",
                                             "The vast majority of the population."))*100)

# To what extent does the available information give people the understanding
# they need to take part in civic life?
table(Data$Q_8_2)
(142+30)/470 # 37% to a considerable or very great extent
Data %>%
  group_by(Developed) %>%
  summarise(Adequate = mean(Q_8_2 %in% c("To a considerable extent",
                                         "To a very great extent"))*100) # 47% vs 24%

# How much of the population is confident in their ability to distinguish
# factual information from false information?
table(Data$Q_8_1)
(137+21)/470 # 34% more than half
Data %>%
  group_by(Developed) %>%
  summarise(MoreThanHalf = mean(Q_8_1 %in% c("Many, more than half of the population",
                                             "The vast majority of the population"))*100) # 44% vs 22%

# How much of the population is using AI-powered tools to find information about
# political and social issues?
table(Data$Q_4_1)
(90+11)/470 # 21% more than half
Data %>%
  group_by(Developed) %>%
  summarise(MoreThanHalf = mean(Q_4_1 %in% c("Many, more than half of the population.",
                                             "The vast majority of the population."))*100) # 25% vs 17%


##### Expert typologies (latent class analyses) ####
# Reported in the report as plain-language boxes: three groups of experts on
# threats, three camps on generative AI. Classes from poLCA; responses recoded
# to 3 levels (low / moderate / high), "I don't know" treated as missing.

# Threats (Q36)
recode_threat <- function(x) {
  case_when(x %in% c("Not a threat at all", "Very small threat", "Small threat") ~ 1,
            x == "Moderate threat" ~ 2,
            x %in% c("Big threat", "An extreme threat") ~ 3)
}
Data_LCA_threats <- Data %>%
  transmute(Q36_1 = recode_threat(Q36_1), Q36_2 = recode_threat(Q36_2),
            Q36_3 = recode_threat(Q36_3), Q36_4 = recode_threat(Q36_4),
            Q36_5 = recode_threat(Q36_5), Q36_6 = recode_threat(Q36_6),
            Developed, Discipline3) %>%
  drop_na(Q36_1:Q36_6) # 456

set.seed(42)
lca_threats <- poLCA(cbind(Q36_1, Q36_2, Q36_3, Q36_4, Q36_5, Q36_6) ~ 1,
                     data = Data_LCA_threats, nclass = 3, nrep = 20, verbose = FALSE)
round(lca_threats$P*100) # 52% broadly alarmed, 37% platform/misinfo-focused, 11% broadly unworried
lca_threats$probs

Data_LCA_threats$Class <- lca_threats$predclass
table(Data_LCA_threats$Class, Data_LCA_threats$Developed)
table(Data_LCA_threats$Class, Data_LCA_threats$Discipline3)

# Generative AI (Q21)
recode_ai <- function(x) {
  case_when(x %in% c("Greatly worsen", "Somewhat worsen", "Slightly worsen") ~ 1,
            x == "Neither" ~ 2,
            x %in% c("Slightly improve", "Somewhat improve", "Greatly improve") ~ 3)
}
Data_LCA_ai <- Data %>%
  transmute(Q21_1 = recode_ai(Q21_1), Q21_2 = recode_ai(Q21_2),
            Q21_3 = recode_ai(Q21_3), Q21_4 = recode_ai(Q21_4),
            Discipline3) %>%
  drop_na(Q21_1:Q21_4) # 447

set.seed(42)
lca_ai <- poLCA(cbind(Q21_1, Q21_2, Q21_3, Q21_4) ~ 1,
                data = Data_LCA_ai, nclass = 3, nrep = 20, verbose = FALSE)
round(lca_ai$P*100) # 62% AI-pessimists, 27% AI-optimists, 11% text helps / visual AI harms
lca_ai$probs

Data_LCA_ai$Class <- lca_ai$predclass
table(Data_LCA_ai$Class, Data_LCA_ai$Discipline3)


writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
