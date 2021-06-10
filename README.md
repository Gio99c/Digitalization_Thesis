# Premessa

Questo file contiene le visualizzazioni utilizzate all’interno della
tesi e il codice utilizzato per generarle. Tutti i file contenenti i
dati utili alla creazione dei grafici sono presenti all’interno della
directory *dataset*, tutte le fonti sono citate all’interno dei grafici.

### Librerie e variabili utilizzate per la creazione e personalizzazione dei grafici

``` r
library(tidyverse)
library(ggrepel)
library(ggtext)
library(readr)
library(readxl)
library(magrittr)
library(RColorBrewer)
library(patchwork)
Set3 <- brewer.pal(brewer.pal.info["Set3", "maxcolors"], "Set3")
```

## Quote di mercato

``` r
quote <- read_csv("dataset/quote_di_mercato_paesi.csv", col_types = cols(
  stati = col_character(),
  valori = col_double()
))
quote %<>% 
  mutate(stati = factor(stati, levels = c("Altri", "Spagna", "Germania", "Svizzera", "Francia", "Cina", "UK", "USA")))

  quote %>%
  ggplot(aes(x="", y=valori, fill = stati)) +
  ggtitle("Quote di mercato su scala globale 2020") +
    labs(caption = "Fonte: The Art Market 2021, Art Basel and UBS") +
    xlab("") +
    ylab("") +
    scale_fill_brewer(palette="Set3") +
    geom_bar(stat = "identity", width = 1, color = "white", show.legend = F) +
    coord_polar("y") +
    theme_void()
```

![](Graphs_files/figure-markdown_github/Quote%20di%20mercato-1.png)

## Età collezionisti

``` r
collezionisti <- read_csv("dataset/eta_collezionisti.csv", col_types = cols(
  generazione = col_character(),
  valori = col_double()
))

collezionisti %<>% 
  mutate(generazione = reorder(generazione, valori)) 

collezionisti %>%
  ggplot(aes(x="", y=valori, fill = generazione )) +
  ggtitle("Età dei collezionisti intervistati da Art Basel", subtitle = "Campione di 2569 High Net Worth Individuals") +
  labs(caption = "Fonte: The Art Market 2021, Art Basel and UBS") +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("#FCCDE5", "#FFFFB3", "#FDB462", "#80B1D3")) +
  geom_bar(stat = "identity", width = 1, color = "white", show.legend = F) +
  coord_polar("y") +
  theme_void()
```

![](Graphs_files/figure-markdown_github/Età%20collezionisti-1.png)

## Cambiamenti nelle vendite

``` r
vendite <- read_csv("dataset/cambiamenti_nelle_vendite.csv", col_types = cols(
  anni = col_double(),
  change = col_double()
))

vendite %<>% mutate(anni = as.Date(paste(anni, "01", "01", sep = "-")))

vendite %>%
  ggplot(aes(anni, change)) +
  ggtitle("Cambiamento percentuale delle vendite nel mercato dell'arte") +
  labs(caption = "Fonte: The Art Market 2021, Art Basel and UBS") +
  xlab("Anni") +
  ylab("Variazione annuale [%]") +
  geom_line(color = "#88A2BC", size = 0.7) +
  geom_label(aes(label = paste0(vendite$change * 100, "%"), color = change < 0), show.legend = F) +
  scale_color_brewer(palette="Set2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/Cambiamenti%20nelle%20vendite-1.png)

## Cambiamento percentuale vendite online

*Questo grafico non è presente nella versione finale perché i dati
originali non sono affidabili e mostrano incongruenze*

``` r
mercato_online <- read_csv("dataset/valore_vendite_online.csv", col_types = cols(
  anno = col_double(),
  dollari = col_double()
))

percent_value <- function(var) {
  r <- vector()
  for( k in 1:length(var) ) {
    r[k] <- round((var[k+1] - var[k]) / var[k], digits = 2)
  }
  data.table::shift(r, -1)
}

mercato_online %<>% 
  mutate(
    anno = as.Date(paste(anno, "01", "01", sep = "-")),
    crescita = percent_value(dollari)
    )
  

mercato_online %>%
  ggplot(aes(anno, crescita)) +
  ggtitle("Cambiamento percentuale delle vendite nel mercato dell'arte online") +
  labs(caption = "Fonte: Online Art Trade report 2020") +
  xlab("Anni") +
  ylab("Variazione annuale [%]") +
  geom_line() +
  geom_label(aes(label = paste0(mercato_online$crescita * 100, "%"), color = crescita < 0), show.legend = F) +
  scale_color_brewer(palette="Set2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/Cambiamento%20percentuale%20vendite%20online-1.png)

## Cambiamento assoluto vendite online

*Questo grafico non è presente nella versione finale perché i dati
originali non sono affidabili e mostrano incongruenze*

``` r
mercato_online %>%
  ggplot(aes(anno, dollari)) +
  ggtitle("Vendite online di opere d'arte e beni da collezione", subtitle = "Valore delle vendite in milioni di dollari") +
  labs(caption = "Fonte: Arts Economics (2021)") +
  xlab("Anni") +
  ylab("Milioni [$]") +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste0("$",mercato_online$dollari / 1000000)), vjust = 0, nudge_y = 200000, nudge_x = -50) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(v) paste0("$", v / 1000000)) +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/Cambiamento%20assoluto%20vendite%20online-1.png)

## Cambiamento del prezzo di ETH

``` r
eth <- read_csv("dataset/ethereum.csv", col_types = cols(
  Date = col_character(),
  Open = col_double(),
  High = col_double(),
  Low = col_double(),
  Close = col_double(),
  Volume = col_double(),
  `Market Cap` = col_double()
))

eth %<>%
  select(Date, Close) %>%
  mutate(Date = as.Date(Date, format = "%b-%d-%Y"))

values <- eth %>%
  filter(Close == max(Close) | Close == min(Close))

 eth %>%
  ggplot(aes(Date, Close)) +
  geom_line(color = "#88A2BC") +
  geom_point(data = values, aes(color = Close == max(Close)), show.legend = F) +
  geom_label(data = values, aes(label = paste0("$", round(Close, 1), "\n", format(Date, "%b '%y")), color = Close == max(Close)), nudge_x = c(-200, 0), nudge_y = c(-200, 400), show.legend = F) +
  scale_color_brewer(palette="Set2", direction = -1) +
  ggtitle("Andamento del prezzo della criptovaluta ETH (2015-2021)") +
  labs(caption = "Fonte: CoinCodex @ https://coincodex.com/crypto/ethereum/historical-data/ (dati al 03/06/21)") +
  xlab("Anni") +
  ylab("Close value [USD]") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/Cambiamento%20del%20prezzo%20di%20ETH-1.png)

## Andamento del mercato della crypto arte

``` r
crypto_art <- read_csv("dataset/crypto_art_market.csv", col_types = cols(
  data = col_date(),
  valore = col_double(),
  company = col_factor()
))

linee <- crypto_art %>%
  filter(data > as.Date("2018-05-01")) %>%
  group_by(data) %>%
  summarise(totale = sum(valore)) %>%
  ggplot(aes(data, totale)) +
  geom_line(color = "#88A2BC") +
  ggtitle("Andamento complessivo del mercato della crypto art", "Volume di vendita") +
  xlab("Anni") +
  ylab("Dollari [USD]") +
  scale_y_continuous(labels = function(v) ifelse(v == "0", paste0("$", v / 1000000), paste0("$", v / 1000000,"M"))) +
  scale_x_date(date_breaks = "5 months", date_labels = "%b %Y") +
  theme_bw()

barre <- crypto_art %>%
  ggplot(aes(data, valore, group = company, fill = company)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = Set3[Set3 != "#FFFFB3"]) +
  scale_y_continuous(labels = function(v) ifelse(v == "0", paste0("$", v / 1000000), paste0("$", v / 1000000,"M"))) +
  ylab("") +
  xlab("") +
  ggtitle("Dettaglio andamento con suddivisione in quote di mercato") +
  theme_bw() +
  theme(plot.title = element_text(size = 7.5))

barre_log <- crypto_art %>%
  ggplot(aes(data, log10(valore), group = company, fill = company)) +
  geom_col() +
  scale_fill_manual("Marketplace", values = Set3[Set3 != "#FFFFB3"]) +
  scale_y_log10(breaks = scales::trans_breaks('log10', function(x) 10^x), labels = function(v) scales::math_format()((log10(v) / 5))) +
  ylab("") +
  xlab("") +
  labs(caption = "Fonte: CryptoArt @ https://cryptoart.io/data") +
  ggtitle("Dettaglio quote di mercato con scala logaritmica") +
  theme_bw() +
  theme(plot.title = element_text(size = 7.5))

linee / (barre + barre_log)
```

![](Graphs_files/figure-markdown_github/Andamento%20del%20mercato%20della%20crypto%20arte-1.png)

### Dettaglio andamento del mercato della crypto arte

``` r
crypto_art %>%
  filter(data >= as.Date("2021-01-01")) %>%
  group_by(data) %>%
  summarise(totale = sum(valore)) %>%
  ggplot(aes(data, totale)) +
  geom_line(color = "#88A2BC") +
  #geom_point(color = "#88A2BC") +
  geom_label(aes(label = paste0("$", round(totale / 1000000, 1), "M"), color = totale > 0), show.legend = F) +
  scale_color_manual(values = c("#8DD3C7")) +
  ggtitle("Dettaglio volume di vendita del mercato della crypto art", "Periodo di riferimento: Gennaio '21 - Maggio '21") +
  xlab("Anni") +
  ylab("Dollari [USD]") +
  scale_y_continuous(labels = function(v) ifelse(v == "0", paste0("$", v / 1000000), paste0("$", v / 1000000,"M"))) +
  scale_x_date(date_labels = "%b '%y", expand = expansion(mult = c(0.1, 0.1))) +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/Dettaglio%20andamento%20del%20mercato%20della%20crypto%20arte-1.png)

## Distribuzione delle vendite per marketplace

``` r
jitter <- crypto_art %>%
  ggplot(aes(company, valore, color = company)) +
  geom_jitter() +
  scale_color_manual("", values = Set3[Set3 != "#FFFFB3"]) +
  scale_y_log10(breaks = scales::trans_breaks('log10', function(x) 10^x), labels = scales::trans_format('log10', scales::math_format(10^.x))) +
  ggtitle("Distribuzione delle vendite per marketplace", "Vendite mensili nel periodo Aprile 2018 - Maggio 2021") +
  xlab("") +
  ylab("Dollari [USD]") +
  theme_bw()

violin <- crypto_art %>%
  ggplot(aes(company, valore, fill = company, color = company)) +
  geom_violin() +
  scale_color_manual("", values = Set3[Set3 != "#FFFFB3"], aesthetics = c("colour", "fill")) +
  scale_y_log10(breaks = scales::trans_breaks('log10', function(x) 10^x), labels = scales::trans_format('log10', scales::math_format(10^.x))) +
  xlab("") +
  ylab("Dollari [USD]") +
  theme_bw()

box <- crypto_art %>%
  ggplot(aes(company, valore, color = company)) +
  geom_boxplot() +
  scale_color_manual("", values = Set3[Set3 != "#FFFFB3"]) +
  scale_y_log10(breaks = scales::trans_breaks('log10', function(x) 10^x), labels = scales::trans_format('log10', scales::math_format(10^.x))) +
  xlab("Marketplace") +
  ylab("Dollari [USD]") +
  labs(caption = "Fonte: CryptoArt @ https://cryptoart.io/data") +
  theme_bw()

jitter / violin / box
```

![](Graphs_files/figure-markdown_github/Distribuzione%20delle%20vendite%20per%20marketplace-1.png)

## SuperRare Andamento vendite

``` r
sales <- read_csv("dataset/superrare_sales.csv", col_types = cols(
  timestamp = col_datetime(format = ""),
  tokenId = col_double(),
  buyer = col_character(),
  seller = col_character(),
  eth = col_double(),
  rate = col_double(),
  usd = col_double(),
  contract = col_character(),
  transactionId = col_character()
))

freq <- sales %>%
  mutate(month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>%
  group_by(month, year) %>%
  summarise(count = n()) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  ggplot(aes(date, count)) +
  geom_col(fill = "#88A2BC") +
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2021-04-01")), date_breaks = "5 month", date_labels = "%b %Y") +
  ggtitle("Andamento delle vendite di crypto art su SuperRare", "Frequenza mensile delle vendite") +
  xlab("") +
  ylab("Unità vendute") +
  theme_bw()

tot <- sales %>%
  mutate(month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(usd, na.rm = T)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  ggplot(aes(date, total)) +
  geom_line(color = "#88A2BC") +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = function(v) ifelse(v == 0, paste0("$", 0), paste0("$", v / 1000000, "M"))) +
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2021-04-01")), date_breaks = "5 month", date_labels = "%b %Y") +
  ggtitle(NULL, "Volume di vendita") +
  xlab("Anni") +
  ylab("Dollari [USD]") +
  labs(caption = "Fonte: M. Franceschet @ https://www.kaggle.com/franceschet/superrare") +
  theme_bw()

freq / tot
```

![](Graphs_files/figure-markdown_github/SuperRare%20Andamento%20vendite-1.png)

### Dettaglio SuperRare andamento vendite

``` r
freq_d <- sales %>%
  mutate(month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>%
  filter((year == "2020" & month >= "03") | year == "2021") %>%
  group_by(month, year) %>%
  summarise(count = n()) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  ggplot(aes(date, count)) +
  geom_col(fill = "#88A2BC") +
  geom_text(data = ~.x %>% filter(date %in% c(as.Date("2020-04-01"), as.Date("2021-01-01"))), aes(label = count), color = "white", fontface = "bold", vjust = 1, nudge_y = -50) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  ggtitle("Dettaglio andamento delle vendite (Marzo '20 - Marzo '21)", "Frequenza mensile delle vendite") +
  xlab("") +
  ylab("Unità vendute") +
  theme_bw()

tot_d <- sales %>%
  mutate(month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>%
  filter((year == "2020" & month >= "03") | year == "2021") %>%
  group_by(month, year) %>%
  summarise(total = sum(usd, na.rm = T)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  ggplot(aes(date, total)) +
  geom_line(color = "#88A2BC") +
  geom_point(color = "#88A2BC") +
  geom_label(data = ~.x %>% filter(date %in% c(as.Date("2020-04-01"), as.Date("2021-01-01"))), aes(label = ifelse(total < 1e6, paste0("$", round(total / 1e3, 1), "k"), paste0("$", round(total / 1e6, 1), "M"))), nudge_y = c(0, 1e6), color = "#8DD3C7") +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = function(v) ifelse(v == 0, paste0("$", 0), paste0("$", v / 1000000, "M"))) +
  scale_x_date(breaks = c(as.Date("2020-03-01"), as.Date("2020-05-01"), as.Date("2020-07-01"), as.Date("2020-09-01"), as.Date("2020-11-01"), as.Date("2021-01-01"), as.Date("2021-03-01")), date_labels = "%b %Y") +
  ggtitle(NULL, "Volume di vendita") +
  xlab("Anni") +
  ylab("Dollari [USD]") +
  theme_bw()

freq_d / tot_d
```

![](Graphs_files/figure-markdown_github/Dettaglio%20SuperRare%20andamento%20vendite-1.png)

## SuperRare Primario vs Secondario

``` r
p_vs_s <- read_csv("dataset/primario_vs_secondario.csv", col_types = cols(
  data = col_date(),
  Primario = col_double(),
  Secondario = col_double()
))

p_vs_s %>%
  pivot_longer("Primario":"Secondario", names_to = "tipo") %>%
  ggplot(aes(data, value, color = tipo)) +
  geom_line(show.legend = F) +
  geom_text(data = ~.x %>% filter(data == max(data)), aes(data, value, label = tipo), hjust = 0, nudge_x = 3, show.legend = F) +
  geom_point(data = ~.x %>% filter(data == max(data)), aes(data, value), show.legend = F) +
  scale_x_date(expand = expansion(mult = c(0.05, 0.15))) +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = function(v) ifelse(v == 0, paste0("$", v / 1000000), paste0("$", v / 1000000, "M"))) +
  scale_color_manual(values = c("#BEBADA", "#8DD3C7")) +
  ggtitle("Volume di vendita sul mercato primario e secondario") +
  labs(caption = "Fonte: CryptoArt @ https://cryptoart.io/data") +
  ylab("Dollari [USD]") +
  xlab("Anni") +
  theme_bw()
```

![](Graphs_files/figure-markdown_github/SuperRare%20Primario%20vs%20Secondario-1.png)
