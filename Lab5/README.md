# Исследование информации о состоянии беспроводных сетей
QazRT

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение Windows 10 Pro
2.  Visual Studio Code с установленными плагинами для работы с языком R
3.  Интерпретатор языка R 4.5.1

## План:

1.  Загрузить данные и провести действи для приведения в “аккуратный”
    вид
2.  Определить небезопасные точки доступа
3.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах
4.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.
5.  Обнаружить топ-10 самых быстрых точек доступа.
6.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию.
7.  Определить производителя для каждого обнаруженного устройства
8.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес
9.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.
10. Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

## Шаги:

1.  Импорт библиотек

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.1.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
```

1.  Чтение и разделение данных

    ``` r
    lines <- read_lines("P2_wifi_data.csv")
    split_idx <- which(lines == "")[1]

    ap_lines <- lines[3:(split_idx-1)]
    ap_data <- read_csv(paste(ap_lines, collapse = "\n"),
      col_names = c("BSSID", "First_time_seen", "Last_time_seen", "channel", "Speed", 
                    "Privacy", "Cipher", "Authentication", "Power", "#beacons", "#IV", 
                    "LAN_IP", "ID_length", "ESSID", "Key"),
      na = c("", " ", "(not associated)")
    )
    ```

        Rows: 166 Columns: 15
        ── Column specification ────────────────────────────────────────────────────────
        Delimiter: ","
        chr  (6): BSSID, Privacy, Cipher, Authentication, LAN_IP, ESSID
        dbl  (6): channel, Speed, Power, #beacons, #IV, ID_length
        lgl  (1): Key
        dttm (2): First_time_seen, Last_time_seen

        ℹ Use `spec()` to retrieve the full column specification for this data.
        ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ``` r
    sta_lines <- lines[(split_idx+2):length(lines)]
    sta_data <- read_csv(paste(sta_lines, collapse = "\n"),
      col_names = c("Station_MAC", "First_time_seen", "Last_time_seen", 
                    "Power", "Packets", "BSSID", "Probed_ESSIDs"),
      na = c("", " ", "(not associated)")
    )
    ```

        Warning: One or more parsing issues, call `problems()` on your data frame for details,
        e.g.:
          dat <- vroom(...)
          problems(dat)

        Rows: 12081 Columns: 7
        ── Column specification ────────────────────────────────────────────────────────
        Delimiter: ","
        chr  (3): Station_MAC, BSSID, Probed_ESSIDs
        dbl  (2): Power, Packets
        dttm (2): First_time_seen, Last_time_seen

        ℹ Use `spec()` to retrieve the full column specification for this data.
        ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

2.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных

    ``` r
    ap_data
    ```

        # A tibble: 166 × 15
           BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
           <chr>    <dttm>              <dttm>                <dbl> <dbl> <chr>   <chr> 
         1 6E:C7:E… 2023-07-28 09:13:03 2023-07-28 11:55:12       1   130 WPA2    CCMP  
         2 9A:75:A… 2023-07-28 09:13:03 2023-07-28 11:53:31       1   360 WPA2    CCMP  
         3 4A:EC:1… 2023-07-28 09:13:03 2023-07-28 11:04:01       7   360 WPA2    CCMP  
         4 D2:6D:5… 2023-07-28 09:13:03 2023-07-28 10:30:19       6   130 WPA2    CCMP  
         5 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
         6 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:44      11   195 WPA2    CCMP  
         7 0A:C5:E… 2023-07-28 09:13:03 2023-07-28 11:36:31      11   130 WPA2    CCMP  
         8 38:1A:5… 2023-07-28 09:13:03 2023-07-28 10:25:02      11   130 WPA2    CCMP  
         9 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 10:29:21       1   195 WPA2    CCMP  
        10 1E:93:E… 2023-07-28 09:13:04 2023-07-28 11:53:37       6   180 WPA2    CCMP  
        # ℹ 156 more rows
        # ℹ 8 more variables: Authentication <chr>, Power <dbl>, `#beacons` <dbl>,
        #   `#IV` <dbl>, LAN_IP <chr>, ID_length <dbl>, ESSID <chr>, Key <lgl>

    ``` r
    sta_data
    ```

        # A tibble: 12,081 × 7
           Station_MAC       First_time_seen     Last_time_seen      Power Packets BSSID
           <chr>             <dttm>              <dttm>              <dbl>   <dbl> <chr>
         1 CA:66:3B:8F:56:DD 2023-07-28 09:13:03 2023-07-28 10:59:44   -33     858 BE:F…
         2 96:35:2D:3D:85:E6 2023-07-28 09:13:03 2023-07-28 09:13:03   -65       4 <NA> 
         3 5C:3A:45:9E:1A:7B 2023-07-28 09:13:03 2023-07-28 11:51:54   -39     432 BE:F…
         4 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61     958 BE:F…
         5 5E:8E:A6:5E:34:81 2023-07-28 09:13:04 2023-07-28 09:13:04   -53       1 <NA> 
         6 10:51:07:CB:33:E7 2023-07-28 09:13:05 2023-07-28 11:56:06   -43     344 <NA> 
         7 68:54:5A:40:35:9E 2023-07-28 09:13:06 2023-07-28 11:50:50   -31     163 1E:9…
         8 74:4C:A1:70:CE:F7 2023-07-28 09:13:06 2023-07-28 09:20:01   -71       3 E8:2…
         9 8A:A3:5A:33:76:57 2023-07-28 09:13:06 2023-07-28 10:20:27   -74     115 00:2…
        10 CA:54:C4:8B:B5:3A 2023-07-28 09:13:06 2023-07-28 11:55:04   -65     437 00:2…
        # ℹ 12,071 more rows
        # ℹ 1 more variable: Probed_ESSIDs <chr>

    ``` r
    ap_data_clean <- ap_data %>% mutate(
      BSSID = as.character(BSSID),
      First_time_seen = ymd_hms(First_time_seen),
      Last_time_seen = ymd_hms(Last_time_seen),
      channel = as.integer(channel),
      Speed = as.integer(Speed),
      Privacy = as.character(Privacy),
      Cipher = as.character(Cipher),
      Authentication = as.character(Authentication),
      Power = as.integer(Power),
      Beacons = as.integer(`#beacons`),
      IV = as.integer(`#IV`),
      LAN_IP = as.character(LAN_IP),
      ID_length = as.integer(ID_length),
      ESSID = as.character(ESSID)
    )

    sta_data_clean <- sta_data %>% mutate(
      Station_MAC = as.character(Station_MAC),
      First_time_seen = ymd_hms(First_time_seen),
      Last_time_seen = ymd_hms(Last_time_seen),
      Power = as.integer(Power),
      Packets = as.integer(Packets),
      BSSID = as.character(BSSID),
      Probed_ESSIDs = as.character(Probed_ESSIDs)
    )
    ```

3.  Определить небезопасные точки доступа (без шифрования – OPN)

    ``` r
    insecure_aps <- ap_data_clean %>% filter(Privacy == "OPN" | is.na(Privacy))
    insecure_aps 
    ```

        # A tibble: 85 × 17
           BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
           <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
         1 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
         2 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:12       6   130 OPN     <NA>  
         3 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:11       6   130 OPN     <NA>  
         4 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:10       6    -1 OPN     <NA>  
         5 00:25:0… 2023-07-28 09:13:06 2023-07-28 11:56:21      44    -1 OPN     <NA>  
         6 E8:28:C… 2023-07-28 09:13:09 2023-07-28 11:56:05      11   130 OPN     <NA>  
         7 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:27:06       6   130 OPN     <NA>  
         8 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:39:43       6   130 OPN     <NA>  
         9 E8:28:C… 2023-07-28 09:13:17 2023-07-28 11:52:32       1   130 OPN     <NA>  
        10 E8:28:C… 2023-07-28 09:13:50 2023-07-28 11:43:39      11   130 OPN     <NA>  
        # ℹ 75 more rows
        # ℹ 10 more variables: Authentication <chr>, Power <int>, `#beacons` <dbl>,
        #   `#IV` <dbl>, LAN_IP <chr>, ID_length <int>, ESSID <chr>, Key <lgl>,
        #   Beacons <int>, IV <int>

4.  Определить производителя для каждого обнаруженного устройства

    ``` r
    extract_oui <- function(mac_vec) {
      mac_clean <- gsub("[:.-]", "", toupper(mac_vec))
      oui <- ifelse(!is.na(mac_clean) & nchar(mac_clean) >= 6, substr(mac_clean, 1, 6), NA_character_)
      return(oui)
    }

    oui_url <- "https://gitlab.com/wireshark/wireshark/-/raw/release-4.0/manuf"
    oui_raw <- readLines(oui_url)
    oui_clean <- oui_raw[str_detect(oui_raw, regex("^[0-9A-F]{2}:[0-9A-F]{2}:[0-9A-F]{2}", ignore_case = TRUE))]
    oui_df <- tibble(line = oui_clean) %>%
      separate(line, into = c("OUI", "ShortName", "FullName"), sep = "\t", extra = "merge") %>%
      mutate(OUI = toupper(gsub(":", "", OUI))) %>% filter(str_length(OUI) == 6)
    ```

        Warning: Expected 3 pieces. Missing pieces filled with `NA` in 2539 rows [20, 21, 24,
        62, 63, 109, 133, 140, 144, 145, 219, 223, 238, 240, 253, 258, 259, 260, 363,
        365, ...].

    ``` r
    ap_data_clean <- ap_data_clean %>% mutate(OUI = extract_oui(BSSID))
    sta_data_clean <- sta_data_clean %>% mutate(OUI = extract_oui(Station_MAC))

    ap_with_vendor <- ap_data_clean %>% left_join(oui_df, by = "OUI")
    sta_with_vendor <- sta_data_clean %>% left_join(oui_df, by = "OUI")
    ```

5.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах

    ``` r
    wpa3_aps <- ap_data_clean %>% filter(str_detect(Privacy, regex("WPA3", ignore_case = TRUE)))
    wpa3_aps
    ```

        # A tibble: 8 × 18
          BSSID     First_time_seen     Last_time_seen      channel Speed Privacy Cipher
          <chr>     <dttm>              <dttm>                <int> <int> <chr>   <chr> 
        1 26:20:53… 2023-07-28 09:15:45 2023-07-28 09:33:10      44   866 WPA3 W… CCMP  
        2 A2:FE:FF… 2023-07-28 09:41:52 2023-07-28 09:41:52       6   130 WPA3 W… CCMP  
        3 96:FF:FC… 2023-07-28 09:52:54 2023-07-28 10:25:02      44   866 WPA3 W… CCMP  
        4 CE:48:E7… 2023-07-28 09:59:20 2023-07-28 10:04:15      44   866 WPA3 W… CCMP  
        5 8E:1F:94… 2023-07-28 10:08:32 2023-07-28 10:15:27      44   866 WPA3 W… CCMP  
        6 BE:FD:EF… 2023-07-28 10:15:24 2023-07-28 10:15:28       6   130 WPA3 W… CCMP  
        7 3A:DA:00… 2023-07-28 10:27:01 2023-07-28 10:27:10       6   130 WPA3 W… CCMP  
        8 76:C5:A0… 2023-07-28 11:16:36 2023-07-28 11:16:38       6   130 WPA3 W… CCMP  
        # ℹ 11 more variables: Authentication <chr>, Power <int>, `#beacons` <dbl>,
        #   `#IV` <dbl>, LAN_IP <chr>, ID_length <int>, ESSID <chr>, Key <lgl>,
        #   Beacons <int>, IV <int>, OUI <chr>

6.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.

    ``` r
    ap_data_clean <- ap_data_clean %>% mutate(Duration_sec = as.numeric(difftime(Last_time_seen, First_time_seen, units = "secs"))) %>% arrange(desc(Duration_sec))
    ```

7.  Обнаружить топ-10 самых быстрых точек доступа.

    ``` r
    top_speed_aps <- ap_data_clean %>% arrange(desc(Speed)) %>% head(10)
    top_speed_aps
    ```

        # A tibble: 10 × 19
           BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
           <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
         1 96:FF:F… 2023-07-28 09:52:54 2023-07-28 10:25:02      44   866 WPA3 W… CCMP  
         2 26:20:5… 2023-07-28 09:15:45 2023-07-28 09:33:10      44   866 WPA3 W… CCMP  
         3 8E:1F:9… 2023-07-28 10:08:32 2023-07-28 10:15:27      44   866 WPA3 W… CCMP  
         4 CE:48:E… 2023-07-28 09:59:20 2023-07-28 10:04:15      44   866 WPA3 W… CCMP  
         5 9A:75:A… 2023-07-28 09:13:03 2023-07-28 11:53:31       1   360 WPA2    CCMP  
         6 E8:28:C… 2023-07-28 09:18:30 2023-07-28 11:55:10      52   360 OPN     <NA>  
         7 E8:28:C… 2023-07-28 09:18:30 2023-07-28 11:55:10      52   360 OPN     <NA>  
         8 E8:28:C… 2023-07-28 09:18:16 2023-07-28 11:51:48      48   360 OPN     <NA>  
         9 14:EB:B… 2023-07-28 09:25:01 2023-07-28 11:53:36       3   360 WPA2    CCMP  
        10 E8:28:C… 2023-07-28 09:18:30 2023-07-28 11:43:23      48   360 OPN     <NA>  
        # ℹ 12 more variables: Authentication <chr>, Power <int>, `#beacons` <dbl>,
        #   `#IV` <dbl>, LAN_IP <chr>, ID_length <int>, ESSID <chr>, Key <lgl>,
        #   Beacons <int>, IV <int>, OUI <chr>, Duration_sec <dbl>

8.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию.

    ``` r
    beacon_rate <- ap_data_clean %>% mutate(Duration_min = Duration_sec / 60, Beacons_per_min = Beacons / pmax(Duration_min, 1)) %>% arrange(desc(Beacons_per_min))
    beacon_rate
    ```

        # A tibble: 166 × 21
           BSSID    First_time_seen     Last_time_seen      channel Speed Privacy Cipher
           <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
         1 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:44      11   195 WPA2    CCMP  
         2 38:1A:5… 2023-07-28 09:13:03 2023-07-28 10:25:02      11   130 WPA2    CCMP  
         3 0A:C5:E… 2023-07-28 09:13:03 2023-07-28 11:36:31      11   130 WPA2    CCMP  
         4 1E:93:E… 2023-07-28 09:13:04 2023-07-28 11:53:37       6   180 WPA2    CCMP  
         5 D2:6D:5… 2023-07-28 09:13:03 2023-07-28 10:30:19       6   130 WPA2    CCMP  
         6 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 10:29:21       1   195 WPA2    CCMP  
         7 4A:86:7… 2023-07-28 10:33:58 2023-07-28 11:24:06       6   130 WPA2    CCMP  
         8 3A:70:9… 2023-07-28 11:30:10 2023-07-28 11:51:50       6   130 WPA2    CCMP  
         9 F2:30:A… 2023-07-28 10:27:02 2023-07-28 10:27:09       1   130 WPA2    CCMP  
        10 76:70:A… 2023-07-28 09:41:52 2023-07-28 10:27:25      11   180 WPA2    CCMP  
        # ℹ 156 more rows
        # ℹ 14 more variables: Authentication <chr>, Power <int>, `#beacons` <dbl>,
        #   `#IV` <dbl>, LAN_IP <chr>, ID_length <int>, ESSID <chr>, Key <lgl>,
        #   Beacons <int>, IV <int>, OUI <chr>, Duration_sec <dbl>, Duration_min <dbl>,
        #   Beacons_per_min <dbl>

9.  Определить производителя для каждого обнаруженного устройства

    ``` r
    sta_with_vendor
    ```

        # A tibble: 12,081 × 10
           Station_MAC       First_time_seen     Last_time_seen      Power Packets BSSID
           <chr>             <dttm>              <dttm>              <int>   <int> <chr>
         1 CA:66:3B:8F:56:DD 2023-07-28 09:13:03 2023-07-28 10:59:44   -33     858 BE:F…
         2 96:35:2D:3D:85:E6 2023-07-28 09:13:03 2023-07-28 09:13:03   -65       4 <NA> 
         3 5C:3A:45:9E:1A:7B 2023-07-28 09:13:03 2023-07-28 11:51:54   -39     432 BE:F…
         4 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61     958 BE:F…
         5 5E:8E:A6:5E:34:81 2023-07-28 09:13:04 2023-07-28 09:13:04   -53       1 <NA> 
         6 10:51:07:CB:33:E7 2023-07-28 09:13:05 2023-07-28 11:56:06   -43     344 <NA> 
         7 68:54:5A:40:35:9E 2023-07-28 09:13:06 2023-07-28 11:50:50   -31     163 1E:9…
         8 74:4C:A1:70:CE:F7 2023-07-28 09:13:06 2023-07-28 09:20:01   -71       3 E8:2…
         9 8A:A3:5A:33:76:57 2023-07-28 09:13:06 2023-07-28 10:20:27   -74     115 00:2…
        10 CA:54:C4:8B:B5:3A 2023-07-28 09:13:06 2023-07-28 11:55:04   -65     437 00:2…
        # ℹ 12,071 more rows
        # ℹ 4 more variables: Probed_ESSIDs <chr>, OUI <chr>, ShortName <chr>,
        #   FullName <chr>

10. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

    ``` r
    is_global_mac <- function(mac_vec) {
      mac_clean <- gsub("[:.-]", "", toupper(mac_vec))
      first_byte_hex <- substr(mac_clean, 1, 2)
      first_byte_dec <- suppressWarnings(strtoi(first_byte_hex, base = 16))
      is_global <- (first_byte_dec & 2) == 0
      is_global[is.na(is_global)] <- FALSE
      return(is_global)
    }

    sta_data_clean <- sta_data_clean %>% mutate(is_global = is_global_mac(Station_MAC))

    non_randomized_clients <- sta_data_clean %>% group_by(Station_MAC) %>% summarise(
      probed_networks = n_distinct(Probed_ESSIDs[Probed_ESSIDs != "Not Probing"], na.rm = TRUE),
      observation_count = n(),
      first_seen = min(First_time_seen),
      last_seen = max(Last_time_seen),
      total_packets = sum(Packets, na.rm = TRUE),
      .groups = "drop"
    ) %>% filter(probed_networks > 1 | observation_count > 5) %>% arrange(desc(probed_networks), desc(observation_count))

    non_randomized_clients
    ```

        # A tibble: 0 × 6
        # ℹ 6 variables: Station_MAC <chr>, probed_networks <int>,
        #   observation_count <int>, first_seen <dttm>, last_seen <dttm>,
        #   total_packets <int>

11. Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.

    ``` r
    sta_clusters <- sta_data_clean %>% filter(BSSID != "(not associated)" & !is.na(Probed_ESSIDs) & Probed_ESSIDs != "") %>% group_by(BSSID, Probed_ESSIDs) %>% summarise(
      mean_power = mean(Power, na.rm = TRUE),
      sd_power = sd(Power, na.rm = TRUE),
      n = n(),
      first_seen = min(First_time_seen),
      last_seen = max(Last_time_seen),
      .groups = "drop"
    ) %>% arrange(sd_power)

    sta_clusters
    ```

        # A tibble: 53 × 7
           BSSID             Probed_ESSIDs mean_power sd_power     n first_seen         
           <chr>             <chr>              <dbl>    <dbl> <int> <dttm>             
         1 1E:93:E3:1B:3C:F4 Galaxy A71         -48.5    0.707     2 2023-07-28 09:13:13
         2 8E:55:4A:85:5B:01 Vladimir           -51.5    4.12      4 2023-07-28 09:31:57
         3 00:26:99:F2:7A:E2 GIVC               -64.7    4.54      7 2023-07-28 09:13:06
         4 00:26:99:BA:75:80 GIVC               -60.3    5.32      6 2023-07-28 09:39:02
         5 E8:28:C1:DD:04:52 MIREA_HOTSPOT      -69      5.66      2 2023-07-28 10:27:47
         6 E8:28:C1:DC:B2:50 MIREA_GUESTS       -57.7    5.77      3 2023-07-28 10:35:02
         7 AA:F4:3F:EE:49:0B Redmi Note 8…      -49      8.49      2 2023-07-28 09:19:44
         8 E8:28:C1:DC:F0:90 MIREA_GUESTS       -63      8.49      2 2023-07-28 10:30:39
         9 E8:28:C1:DC:B2:52 MIREA_HOTSPOT      -62.3    9.35      6 2023-07-28 09:15:24
        10 E8:28:C1:DE:74:32 MIREA_HOTSPOT      -52.3   12.7       3 2023-07-28 10:28:31
        # ℹ 43 more rows
        # ℹ 1 more variable: last_seen <dttm>

12. Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

    ``` r
    sta_clusters <- sta_clusters %>% mutate(signal_stability = 1 / (sd_power + 1e-6))
    most_stable_cluster <- sta_clusters %>% arrange(desc(signal_stability)) %>% slice(1)
    most_stable_cluster
    ```

        # A tibble: 1 × 8
          BSSID             Probed_ESSIDs mean_power sd_power     n first_seen         
          <chr>             <chr>              <dbl>    <dbl> <int> <dttm>             
        1 1E:93:E3:1B:3C:F4 Galaxy A71         -48.5    0.707     2 2023-07-28 09:13:13
        # ℹ 2 more variables: last_seen <dttm>, signal_stability <dbl>

## Оценка результата

В результате лабораторной работы мы получили и закрепили знания о
методах исследования радиоэлектронной обстановки

## Вывод

Таким образом, мы научились, используя программный пакет tidyverse,
анализировать сетевые дампы с помощью языка программирования R
