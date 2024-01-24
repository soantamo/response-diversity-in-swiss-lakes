library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)
library(mgcViz)
library(DHARMa)
library(readxl)

# gam for divergence, dissimilarity

# lake properties df


lake_info <- read_xlsx("lake_info.xlsx")

eutroph <- lake_info |> 
  select(Lake, Phos_max)

# resp diversity per lake df
resp_div_all <- readRDS("total_models/lakes_all_models/resp_div_all.rds") |> 
  select(temp:fLake)


# dissimilarity and max phosporus

rdiv <- resp_div_all |> 
  select(temp, fLake, rdiv) |>
  rename(Lake = fLake)

eutroph_dissimilarity <- merge(eutroph, rdiv) |> 
  drop_na(Phos_max)#get rid of duplications in rows 

new <- eutroph_dissimilarity[duplicated(eutroph_dissimilarity), ]
# gam testing 
new$fLake <- as.factor(new$Lake)
new$fPhos <- as.factor(new$Phos_max)
str(new)

biel <- new |> 
  filter(fLake == "Biel")

thun <- new |> 
  filter(fLake == "Thun")


model <- gam(data = new, rdiv ~ fPhos + s(temp, by = fPhos, k = 10), method = "REML")
model2 <- gam(data = new, rdiv ~ fLake + s(temp, by = fLake, k = 10), method = "REML")


model3 <- gam(data = new, rdiv ~ interaction(fLake, fPhos) +
                s(temp, k = 3) +
                s(temp, by = interaction(fLake, fPhos)))

summary(model3)$s.table

par(mfrow = c(2, 2))
plot(model3)



model_prediction1 <- predict.gam(model3, type = "response", se.fit = TRUE)
model_prediction2 <- predict.gam(model2, type = "response", se.fit = TRUE)


grid <- expand.grid(temp = seq(
  from = min(eutroph_dissimilarity$temp, na.rm = TRUE),
  to = max(eutroph_dissimilarity$temp, na.rm = TRUE), length = 3800))

model_bind1 <- cbind(model_prediction1, grid)
model_bind2 <- cbind(model_prediction2, grid)

pred_df1 <- model_bind1 |>
  group_by(temp) |>
  mutate(fit = mean(fit)) |>
  mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
  summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
            across(se.fit))

pred_df2 <- model_bind2 |>
  group_by(temp) |>
  mutate(fit = mean(fit)) |>
  mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
  summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
            across(se.fit))

pred_df1 |> 
  ggplot(aes(temp, fit)) +
  geom_line()
  ylim(0,3)

pred_df2 |> 
  ggplot(aes(temp, fit)) +
  geom_line() +
  ylim(0,3)
