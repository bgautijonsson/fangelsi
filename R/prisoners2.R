library(tidyverse)
library(eurostat)
library(metill)
library(ggh4x)
theme_set(theme_metill())
library(geomtextpath)
library(patchwork)
library(glue)
library(ggtext)


caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fanga og fólksfjölda",
  "\nGögn og kóði: https://github.com/bgautijonsson/fangelsi"
)

litur_island <- "#08306b"
litur_danmork <- "#e41a1c"
litur_finnland <- "#3690c0"
litur_noregur <- "#7f0000"
litur_svithjod <- "#fd8d3c"
litur_annad <- "#737373"

pop <- get_eurostat(
  "migr_pop1ctz",
  filters = list(
    age = "Y15-64",
    citizen = c("NAT", "FOR_STLS"),
    sex = "T"
  )
)

pop <- pop |>
  label_eurostat()

pop <- pop |>
  janitor::remove_constant() |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  rename(pop = values) |>
  mutate(
    citizen = if_else(
      str_detect(citizen, "Foreign country"),
      "Foreign country",
      citizen
    )
  )

prisoners <- get_eurostat(
  "crim_pris_ctz"
)

prisoners <- prisoners |>
  label_eurostat()

plot_dat <- prisoners |>
  filter(
    unit != "Number"
  ) |>
  select(
    -freq,
    -unit
  ) |>
  rename(time = TIME_PERIOD) |>
  pivot_wider(names_from = citizen, values_from = values) |>
  mutate(
    hlutf_erl = `Foreign country` / Total
  ) |>
  pivot_longer(c(-geo, -time, -hlutf_erl), names_to = "citizen", values_to = "values") |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  inner_join(
    pop |>
      mutate(
        pop = zoo::na.approx(pop, na.rm = FALSE),
        .by = c(geo, land, citizen)
      ),
    by = join_by(land, geo, time, citizen)
  ) |>
  mutate(
    per_pop = values / pop * 100000
  ) |>
  select(-values, -pop) |>
  pivot_wider(names_from = citizen, values_from = per_pop) |>
  janitor::clean_names() |>
  mutate(
    diff = foreign_country / reporting_country,
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth)
  ) |>
  pivot_longer(c(foreign_country, reporting_country, diff)) |>
  filter(
    !land %in% c("Liechtenstein", "Grikkland", "Malta"),
    year(time) >= 2009
  )

p1 <- plot_dat |> 
  filter(name != "diff") |> 
  mutate(
    name = fct_recode(
      name,
      "Erlent ríkisfang" = "foreign_country",
      "Innlent ríkisfang" = "reporting_country"
    )
  ) |> 
    drop_na() |> 
  ggplot(aes(time, value)) +
  geom_line(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line(
    data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
    aes(group = land, colour = colour),
    linewidth = 1
  ) +
  geom_textline(
    data = ~ filter(.x, land == "Ísland"),
    aes(group = land, colour = colour, label = land),
    linewidth = 1.8,
    size = 5,
    hjust = 0.8,
    text_smoothing = 20,
    fontface = "bold"
  ) +
  scale_x_date(
    breaks = breaks_width("2 year", offset = "2 year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(NA, NA),
    expand = expansion(c(0.05, 0.05)),
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  facet_wrap("name") +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fjöldi fanga á hverja 100.000 íbúa með viðeigandi ríkisfang"
  )

p1


p2 <- plot_dat |> 
  filter(name == "diff") |> 
  drop_na() |> 
  filter(
    all(value > 1),
    .by = land
  ) |> 
  ggplot(aes(time, value - 1)) +
  geom_line(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line(
    data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
    aes(group = land, colour = colour),
    linewidth = 1
  ) +
  geom_textline(
    data = ~ filter(.x, land == "Ísland"),
    aes(group = land, colour = colour, label = land),
    linewidth = 1.8,
    size = 5,
    hjust = 0.8,
    text_smoothing = 20,
    fontface = "bold"
  ) +
  scale_x_date(
    breaks = breaks_width("2 year", offset = "year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    expand = expansion(c(0, 0.05)),
    breaks = breaks_extended(6),
    labels = \(x) {
      
      out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
      if_else(
        out == "0x fleiri",
        "Jafnmargir",
        out
      )
    },
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Hve mikið fleiri eru fangar með erlent ríkisfang en fangar með innlent ríkisfang (á höfðatölu)?"
  )

p2

p3 <- plot_dat |> 
  distinct(land, time, hlutf_erl, colour, linewidth, size) |> 
  ggplot(aes(time, hlutf_erl)) +
  geom_line(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line(
    data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
    aes(group = land, colour = colour),
    linewidth = 1
  ) +
  geom_textline(
    data = ~ filter(.x, land == "Ísland"),
    aes(group = land, colour = colour, label = land),
    linewidth = 1.8,
    size = 5,
    hjust = 0.8,
    text_smoothing = 20,
    fontface = "bold"
  ) +
  scale_x_date(
    breaks = breaks_width("2 year", offset = "2 year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(c(0, 0.05)),
    breaks = breaks_extended(6),
    labels = label_hlutf(),
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Hlutfall fanga með erlent ríkisfang"
  )

p3

p <- p1 + p2 + p3 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Samanburður á ríkisfangi fanga",
    subtitle = str_c(
      "Hlutfallslegur munur á fjölda fanga eftir ríkisfangi er lágur á Íslandi ",
      "miðað við önnur Evrópulönd", " | ",
      "Önnur\nNorðurlönd sýnd í lit"
    ),
    caption = caption
  )


p


ggsave(
  plot = p,
  filename = "Figures/fangar_rikisfang.png",
  width = 8, height = 1.1 * 8, scale = 1.2
)





library(glue)
library(ggtext)
p <- plot_dat |> 
  filter(
    name == "diff",
    year(time) == 2022,
    value > 1
  ) |> 
  drop_na() |> 
  mutate(
    land = glue("<i style='color:{colour}'>{land}</i>"),
    land = fct_reorder(land, value)
  ) |> 
  ggplot(aes(value - 1, land, col = colour)) +
  geom_segment(
    aes(xend = 0, yend = land),
    linewidth = 0.1, alpha = 1
  ) +
  geom_point(size = 3) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.1)),
    breaks = 0:10,
    labels = \(x) {
      out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
      if_else(
        out == "0x fleiri",
        "Jafnmargir",
        out
      )
    },
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  theme(
    axis.text.y = element_markdown(),
    axis.title.x = element_text(size = 11, face = "plain")
  ) +
  labs(
    x = "Hve mikið fleiri eru fangar með erlent ríkisfang en íslenskir á höfðatölu?",
    y = NULL,
    title = "Á Íslandi eru fangar með erlent ríkisfang lítið fleiri en fangar með íslenskt",
    subtitle = "Reiknað með því að bera saman fjölda fanga eftir ríkisfangi á höfðatölu viðeigandi hóps",
    caption = caption
  )



ggsave(
  plot = p,
  filename = "Figures/fangar_rikisfang_ordered_2022.png",
  width = 8, height = 0.5 * 8, scale = 1.6
)
