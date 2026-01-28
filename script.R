library(tidyverse)
library(TAM)
library(glue)
library(janitor)

base <- read_rds("base-test.rds")

pcm <- tam.mml(resp = base, irtmodel="PCM")

pcm2 <- tam.mml(resp = base, irtmodel="PCM2")

IRT.WrightMap(pcm)
IRT.WrightMap(pcm2)


# Estudio de dificultades:
xsi.pcm <-
  pcm$xsi %>% 
  rownames_to_column("orig") %>% 
  mutate(
    item = str_split_i(orig, "_", 1),
    step = str_split_i(orig, "_", 2)
  ) %>% 
  pivot_wider(
    id_cols = "item",
    names_from = "step",
    values_from = "xsi"
  ) %>% 
  mutate(
    item_total = round(rowMeans(pick(all_of(c("Cat1","Cat2","Cat3","Cat4")))), 4)
  )

xsi.pcm2 <-
  pcm2$xsi %>% 
  rownames_to_column("orig") %>% 
  mutate(
    item = str_split_i(orig, "_", 1),
    step = str_split_i(orig, "_", 2)
  ) %>% 
  pivot_wider(
    id_cols = "item",
    names_from = "step",
    values_from = "xsi"
  ) %>% 
  rename(item_total = "NA") %>% 
  mutate(
    suma = rowSums(pick(all_of(c("step1","step2","step3")))),
    step4 = 0 - suma
  ) %>% 
  select(-suma) %>% 
  mutate(
    item_total_recreado = round(rowMeans(pick(all_of(c("step1","step2","step3","step4")))), 4)
  )

plot.compare <-
  ggplot(mapping = aes(y = item)) +
  geom_point(data = xsi.pcm, aes(x = Cat1), color = "blue") + 
  geom_point(data = xsi.pcm, aes(x = Cat2), color = "blue") + 
  geom_point(data = xsi.pcm, aes(x = Cat3), color = "blue") + 
  geom_point(data = xsi.pcm, aes(x = Cat4), color = "blue") +
  geom_point(data = xsi.pcm2, aes(x = item_total), color = "black") +
  geom_point(data = xsi.pcm2, aes(x = step1), color = "red") +
  geom_point(data = xsi.pcm2, aes(x = step2), color = "red") +
  geom_point(data = xsi.pcm2, aes(x = step3), color = "red") + 
  geom_point(data = xsi.pcm2, aes(x = step4), color = "red")


plot(pcm, type = "items", items = 1)
plot(pcm2, type = "items", items = 1)

# Thurstonian thresholds
th.pcm <- tam.threshold(pcm)
th.pcm2 <- tam.threshold(pcm2)

# Expected score
plot(pcm, type = "expected", items = 1)
plot(pcm, items = 1, type = "items")
plot(pcm, items = 1, type = "trace")
plot(pcm, 
     items = 1, 
     type = "items", 
     package = "lattice"
)

probs <- IRT.irfprob(pcm)
probs1 <- as.data.frame(probs)

probs2 <- 
  probs1["EXT1",] %>% 
  pivot_longer(
    cols = everything(),
    values_to = "value",
    names_to = "name"
  ) %>% 
  mutate(
    cat = str_split_i(name, "\\.", 1),
    n.value = str_split_i(name, "\\.", 2) %>% as.numeric()
  ) %>% 
  arrange(cat, n.value) %>% 
  pivot_wider(
    id_cols = n.value,
    names_from = "cat",
    values_from = "value",
    names_prefix = "Cat"
  )

# --- 1. Define the Theta Grid ---
# You need the actual Theta values (logits) corresponding to 'n.value'.
# TAM's default grid usually has 21 or 30 points, or you might have defined a custom one.
# If you used default settings:
# --- 1. Define Theta (X-axis) ---
# Since n.value is just 1, 2, 3... we need the real logits.
# TAM usually stores the grid in the attributes of the original probability object.
theta_grid <- attr(IRT.irfprob(pcm), "theta")

# Safety check: If theta_grid is NULL, create a standard one
if(is.null(theta_grid)) theta_grid <- seq(-6, 6, length.out = nrow(probs2))

# --- 2. Calculate Cumulative Curves & Plot ---
xd <- probs2 %>%
  mutate(theta = theta_grid) %>%
  pivot_longer(
    cols = -c(n.value, theta),
    names_to = "category",
    values_to = "prob"
  ) %>%
  mutate(score = parse_number(category)) %>%
  group_by(theta) %>%
  arrange(desc(score)) %>%         # Order: Score 4, 3, 2, 1, 0
  mutate(prob_ge_k = cumsum(prob)) %>% # P(>=4)=Prob4; P(>=3)=Prob4+Prob3...
  ungroup() %>%
  filter(score > 0) %>%
  ggplot(aes(x = theta, y = prob_ge_k, color = factor(score))) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Thurstonian Cumulative Probability Curves",
    subtitle = "Intersection with 0.5 = Thurstonian Thresholds",
    y = "Probability P(X >= k)",
    x = "Theta (Logits)",
    color = "Score Category (>= k)"
  ) +
  theme_minimal()
