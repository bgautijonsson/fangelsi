library(tidyverse)
library(eurostat)
library(metill)
library(ggh4x)
theme_set(theme_metill())
library(geomtextpath)
library(patchwork)



caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fanga og fólksfjölda",
  "\nGögn og kóði: https://github.com/bgautijonsson/fangar"
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
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  janitor::clean_names() |> 
  mutate(
    p = foreign_country / total
  )


p1 <- prisoners |>
  filter(
    unit != "Number"
  ) |> 
  select(
    -freq,
    -unit
  ) |> 
  rename(time = TIME_PERIOD) |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  janitor::clean_names() |> 
  mutate(
    p = foreign_country / total
  ) |> 
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
  ggplot(aes(time, p)) +
  geom_line(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line(
    data = ~ filter(.x, colour != litur_annad),
    aes(group = land, colour = colour),
    linewidth = 1
  ) +
  geom_text(
    data = ~ group_by(.x, land) |> 
      filter(colour != litur_annad, time == max(time)) |> 
      ungroup() |> 
      mutate(
        p = case_when(
          land == "Svíþjóð" ~ p * 1,
          land == "Ísland" ~ p * 1,
          land == "Noregur" ~ p * 1,
          land == "Danmörk" ~ p * 1,
          land == "Finnland" ~ p * 1,
          TRUE ~ p
        )
      ),
    aes(label = land, colour = colour),
    hjust = 0,
    nudge_x = 10,
    size = 3
  ) +
  geom_text(
    data = ~ group_by(.x, land) |> 
      filter(land %in% c("Lúxemborg", "Sviss", "Grikkland"), time == max(time)) |> 
      ungroup(),
    aes(label = land, colour = colour),
    hjust = 0,
    nudge_x = 10,
    size = 3,
    alpha = 0.3,
    col = litur_annad
  ) +
  scale_x_date(
    breaks = breaks_width("2 year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_pretty(),
    labels = label_hlutf(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(),
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  theme(
    plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Hlutfall fanga með erlent ríkisfang"
  )

p1




p2 <- prisoners |>
  filter(
    unit != "Number"
  ) |> 
  select(
    -freq,
    -unit
  ) |> 
  rename(time = TIME_PERIOD) |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  inner_join(
    pop,
    by = join_by(land, geo, time, citizen)
  ) |> 
  mutate(
    per_pop = values / pop * 100000,
    citizen = fct_recode(
      citizen,
      "Íslenskt ríkisfang" = "Reporting country",
      "Erlent ríkisfang" = "Foreign country"
    )
  ) |> 
  filter(land == "Ísland") |> 
  ggplot(aes(time, per_pop)) +
  geom_textline(
    aes(
      lty = citizen, 
      label = citizen,
      hjust = citizen
    ),
    text_smoothing = 30,
    color = litur_island
  ) +
  scale_x_date(
    breaks = breaks_width("2 year", offset = "1 year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.1)),
    guide = guide_axis_truncated()
  ) +
  scale_hjust_manual(
    values = c(0.58, 0.45)
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fjöldi fanga í íslenskum fangelsum á 100.000 íbúa "
  )

p2

title <- "Meðal íbúa með erlent ríkisfang hefur föngum fækkað hlutfallslega"

subtitle <- str_c(
  "Hefur föngum með erlent ríkisfang fjölgað á Íslandi? ",
  "Ef við skoðum tölurnar sem hlutfall allra fanga eða sem fjölda á\n100.000 íbúa með viðeigandi ",
  "ríkisfang virðist svo ekki vera."
)


p <- p1 + p2 +
  plot_annotation(
    title = "Þróun í ríkisfangi fanga á Íslandi og samanburðarlöndum",
    subtitle = subtitle,
    caption = caption
  )

p


ggsave(
  plot = p,
  filename = "Figures/fangar.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)




