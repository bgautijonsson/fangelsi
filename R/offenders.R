library(tidyverse)
library(eurostat)
library(metill)
library(ggh4x)
library(geomtextpath)
library(glue)
library(ggtext)
theme_set(theme_metill())

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



offenders <- get_eurostat(
  "crim_just_ctz"
)

offenders <- offenders |>
  label_eurostat()

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

plot_dat <- offenders |>
  filter(
    unit != "Per hundred thousand inhabitants"
  ) |> 
  select(
    -freq,
    -unit
  ) |> 
  rename(time = TIME_PERIOD) |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  )  |> 
  inner_join(
    pop |>
      mutate(
        pop = zoo::na.approx(pop, na.rm = FALSE),
        .by = c(geo, land, citizen)
      ),
    by = join_by(land, geo, time, citizen)
  )

iceland_2022 <- plot_dat |> 
  filter(
    geo == "Iceland",
    year(time) >= 2022
  ) |> 
  arrange(time) |> 
  mutate_at(
    vars(values),
    \(x) {
      n <- length(x)
      x[(n - 1):n] <- x[n:(n - 1)]
      
      x
    }
  )

p <- plot_dat |> 
  filter(
    (year(time) < 2022) | (geo != "Iceland")
  ) |> 
  bind_rows(
    iceland_2022
  ) |> 
  mutate(
    values = values / pop
  ) |> 
  select(-pop) |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  janitor::clean_names() |> 
  # filter(
  #   leg_stat == "Prosecuted person"
  # ) |> 
  mutate(
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
  mutate(
    p = foreign_country / reporting_country
  ) |> 
  filter(
    # year(time) < 2022,
    leg_stat != "Convicted person"
  ) |>
  mutate(
    leg_stat = fct_recode(
      leg_stat,
      "Saksóttir" = "Prosecuted person",
      "Grunaðir" = "Suspected person"
    )
  ) |> 
  ggplot(aes(time, p)) +
  geom_line(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_textline(
    data = ~ filter(.x, colour != litur_annad),
    aes(group = land, colour = colour, label = land, hjust = land),
    linewidth = 1
  ) +
  scale_x_date(
    breaks = breaks_width("2 year", offset = "1 year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, 8),
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
  scale_hjust_manual(
    values = c(0.3, 0.4, 0.25, 0.2)
  ) +
  coord_cartesian(ylim = c(0, 8)) +
  facet_wrap("leg_stat") +
  theme(
    plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Samanburður á tíðni gruns um glæp og saksóknar meðal innflytjenda og innfæddra",
    subtitle = "Hve mikið fleiri eru grunaðir með erlent ríkisfang en grunaðir með innlent ríkisfang (á höfðatölu)?",
    caption = caption
  )






ggsave(
  plot = p,
  filename = "Figures/grunadir_saksottir.png",
  width = 8, height = 0.5 * 8, scale = 1.7
)


p2 <- plot_dat |> 
  filter(
    (year(time) < 2022) | (geo != "Iceland")
  ) |> 
  bind_rows(
    iceland_2022
  ) |> 
  mutate(
    values = values / pop
  ) |> 
  select(-pop) |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  janitor::clean_names() |> 
  mutate(
    value = foreign_country / reporting_country
  ) |> 
  filter(
    time == max(time),
    .by = land
  ) |> 
  filter(
    value > 1,
    leg_stat == "Suspected person"
  ) |> 
  drop_na() |> 
  mutate(
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
    x = NULL,
    y = NULL,
    subtitle = "Grunaðir um glæp (2022)"
  )


p3 <- plot_dat |> 
  filter(
    (year(time) < 2022) | (geo != "Iceland")
  ) |> 
  bind_rows(
    iceland_2022
  ) |> 
  mutate(
    values = values / pop
  ) |> 
  select(-pop) |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  janitor::clean_names() |> 
  mutate(
    value = foreign_country / reporting_country
  ) |> 
  filter(
    time == max(time),
    .by = land
  ) |> 
  filter(
    value > 1,
    leg_stat == "Prosecuted person"
  ) |> 
  drop_na() |> 
  mutate(
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
    x = NULL,
    y = NULL,
    subtitle = "Saksóttir (2022)"
  )

(p + labs(caption = NULL)) / (p3 + p2)
