# 1. Set Up: --------------------------------------------------------------
##
# library(tidyverse)
# library(stringi)
# library(ISOcodes)
##
iso159_scripts <- dplyr::tibble(ISOcodes::ISO_15924) %>%
  janitor::clean_names() %>%
  dplyr::mutate(numeric = as.numeric(numeric)) %>%
  dplyr::filter(numeric < 900)
lang_iso639_2 <- dplyr::tibble(ISO_639_2) %>% janitor::clean_names()
iso159_scripts <- dplyr::tibble(ISOcodes::ISO_15924) %>%
  janitor::clean_names() %>%
  dplyr::mutate(numeric = as.numeric(numeric)) %>%
  dplyr::filter(numeric < 900)
lang_iso639_2 <- dplyr::tibble(ISO_639_2) %>% janitor::clean_names()
lang_iso639_3 <- dplyr::tibble(ISO_639_3) %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., ""))) %>%
  dplyr::select(id:eng)
codes2 <- natalia::suna(c(lang_iso639_2$alpha_2, lang_iso639_3$part1))
codes3 <- natalia::suna(c(lang_iso639_2$alpha_3_b, lang_iso639_2$alpha_3_t,
                 lang_iso639_3$part2b, lang_iso639_3$part2t, lang_iso639_3$id))
langs <- natalia::suna(c(lang_iso639_2$name, lang_iso639_3$name, lang_iso639_3$eng))
lang_fams <- natalia::suna(ISO_639_5$Id)
scripts <- c(iso159_scripts$alpha_4, iso159_scripts$name, iso159_scripts$pva,
             "Traditional", "Simplified", "Indic") %>%
  natalia::suna() %>% stringr::str_remove_all("_| ")
locales <- expand.grid(codes2, ISO_3166_1$Alpha_2, stringsAsFactors = F) %>%
  dplyr::mutate(x = paste(Var1, Var2, sep = "_")) %>% dplyr::pull(x)

###
txt_translit <- dplyr::tibble(id = stringi::stri_trans_list()) %>%
  tidyr::separate(id, into = c("source", "target"), sep = "-", remove = F) %>%
  dplyr::mutate(
    # General:
    any = dplyr::if_else(source == "Any"| target == "Any", 1, 0),
    generics = dplyr::if_else(target %in% c("Null", "Remove"), 1, 0),
    case = dplyr::if_else(target %in% c("Upper", "Lower", "Title"), 1, 0),
    unicode_norms = dplyr::if_else(target %in% c("NFD", "NFC", "NFKD", "NFKC", "FCD", "FCC"), 1, 0),
    unicode_names = dplyr::if_else(source == "Name"| target == "Name", 1, 0),
    unicode_codes = dplyr::if_else(stringr::str_detect(source, "Hex") | stringr::str_detect(target, "Hex"), 1, 0),
    ascii = dplyr::if_else(source == "ASCII"| target == "ASCII", 1, 0),
    width = dplyr::if_else(stringr::str_detect(source, "width") | stringr::str_detect(target, "width"), 1, 0),
    punctuation = dplyr::if_else(source == "Publishing"| target == "Publishing", 1, 0),
    accents = dplyr::if_else(source == "Accents"| target == "Accents", 1, 0),
    pinyin = dplyr::if_else(stringr::str_detect(source, stringr::regex("Pinyin", ignore_case = T)) |
                       stringr::str_detect(target, stringr::regex("Pinyin", ignore_case = T)), 1, 0),
    # Scripts:
    scripts = dplyr::if_else(source %in% scripts | target %in% scripts |
                        stringr::str_detect(source, "BGN|UNGEGN|Latn|Cyrl|Hrkt") |
                        stringr::str_detect(target, "BGN|UNGEGN|Latn|Cyrl|Hrkt"), 1, 0),
    # ^ https://en.wikipedia.org/wiki/BGN/PCGN_romanization
    # Languages:
    langs = dplyr::if_else(source %in% c(codes2, codes3, langs)|
                      target %in% c(codes2, codes3, langs), 1, 0),
    lang_fam = dplyr::if_else(source %in% lang_fams | target %in% lang_fams, 1, 0),
    # Others:
    phonetics = dplyr::if_else(stringr::str_detect(source, "IPA") | stringr::str_detect(target, "IPA") |
                          stringr::str_detect(source, stringr::regex("XSAMP", ignore_case = T)) |
                          stringr::str_detect(target, stringr::regex("XSAMP", ignore_case = T)), 1, 0),
    locales = dplyr::if_else(source %in% locales | target %in% locales, 1, 0),
    others = dplyr::if_else(source %in% c("Digit", "Tone") | target %in% c("Digit", "Tone"), 1, 0),
  ) %>%
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric))))



# Test! -------------------------------------------------------------------
my_string = "Revista de la Facultad de Ingenier\\xc3" # "_(A%E2%80%93E)"

# Quick checks:
stringi::stri_trans_general(my_string, "latin-ascii")
stringi::stri_trans_general(my_string, "Any-Hex")

##
# Try 1:
x <- txt_translit %>%
  dplyr::mutate(x = generics + case + width + punctuation + accents + pinyin + scripts +
                  unicode_norms + langs + lang_fam + phonetics + locales) %>%
  dplyr::filter(x == 0) %>% janitor::remove_constant(quiet = F) %>% dplyr::pull(id)

purrr::map_chr(x, ~ stringi::stri_trans_general(my_string, .x)) %>% unique()
purrr::map_chr(x, ~ stringi::stri_trans_general(urltools::url_decode(my_string), .x)) %>% unique()

##
# Try 2:
txt_translit %>%
  dplyr::filter(!(id %in% x)) %>%
  dplyr::pull(id) %>%
  purrr::map_chr( ~ stringi::stri_trans_general(my_string, .x)) %>%
  unique()

txt_translit %>%
  dplyr::filter(!(id %in% x)) %>%
  dplyr::pull(id) %>%
  purrr::map_chr( ~ stringi::stri_trans_general(urltools::url_decode(my_string), .x)) %>%
  unique()
