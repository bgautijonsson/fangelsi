library(tidyverse)
library(eurostat)
library(metill)
library(ggh4x)
theme_set(theme_metill())

caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga.",
  "\nGögn og kóði: https://github.com/bgautijonsson/fanger"
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

d <- offenders |>
  label_eurostat()


plot_dat <- d |>
  filter(
    unit == "Number"
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


p <- plot_dat |> 
  filter(
    leg_stat == "Convicted person"
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
          land == "Svíþjóð" ~ p * 0.97,
          land == "Ísland" ~ p * 0.94,
          land == "Noregur" ~ p * 0.99,
          land == "Danmörk" ~ p * 1.09,
          land == "Finnland" ~ p * 0.95,
          TRUE ~ p
        )
      ),
    aes(label = land, colour = colour),
    hjust = 0,
    nudge_x = 10
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
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
    title = "Hlutfall fanga með erlent ríkisfang",
    subtitle = "Árleg samantekt Eurostat",
    caption = caption
  )


p



ggsave(
  plot = p,
  filename = "Figures/prisoners.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)





