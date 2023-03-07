library(tidyverse)
load_all()

genes <- example_genes %>%
  filter(molecule %in% c("Genome1", "Genome2", "Genome3", "Genome4")) %>%
  mutate(gstart = ifelse(orientation == 1, start, end)) %>%
  mutate(gend = ifelse(orientation == 1, end, start)) %>%
  select(-start, -end) %>%
  rename(start = gstart, end = gend)

features <- example_features %>%
  filter(molecule %in% c("Genome1", "Genome2", "Genome3", "Genome4"))

ggplot(genes, aes(xmin = start, xmax = end, y = molecule, fill = gene, label = gene)) +
  geom_feature(
    data = features,
    aes(x = position, y = molecule, forward = forward)
  ) +
  geom_feature_label(
    data = features,
    aes(x = position, y = molecule, label = name, forward = forward)
  ) +
  geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
  geom_gene_label() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  theme_genes()

ggsave("figure_1.png", width = 10, height = 4)
