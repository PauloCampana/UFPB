
# brhelper

<!-- badges: start -->
<!-- badges: end -->

O pacote brhelper foi criado com objetivo de facilitar a entrada,
conversão e relação de dados geográficos Brasileiros.

## Instalação

Você pode instalar o pacote brhelper do
[GitHub](https://github.com/PauloCampana/brhelper) através de:

``` r
# install.packages("remotes")
remotes::install_github("PauloCampana/brhelper")
# ou
# install.packages("devtools")
devtools::install_github("PauloCampana/brhelper")
```

## Exemplo

Como fazer um mapa usando os dados inbutidos do brhelper e o pacote
[ggplot2](https://ggplot2.tidyverse.org/):

``` r
library(brhelper)
library(tidyverse)

mapa_estado |> 
    ggplot(aes(x = long, y = lat, group = group, fill = id)) +
    geom_polygon() +
    scale_fill_viridis_c() +
    coord_map() +
    theme_void() +
    theme(legend.position = "none")
```

<img src="man/figures/README-example-1.png" width="100%" />

Como converter uma variável que caracteriza o Estado para ID:

``` r
tribble(
    ~estado,       ~area,
    "Amazonas",     1570.7,
    "Pará",         1247.6,
    "Mato Grosso",  903.3,
    "Minas Gerais", 587.5,
    "Bahia",        564.6
) |> 
  mutate(estado = estado |> converter_estado(de = "nome", para = "id"))
#> # A tibble: 5 × 2
#>   estado  area
#>    <int> <dbl>
#> 1      3 1571.
#> 2      5 1248.
#> 3     25  903.
#> 4     17  588.
#> 5     16  565.
```

Como unir uma variável em ID com os dados do mapa:

``` r
tribble(
    ~estado, ~area,
    3,        1570.7,
    5,        1247.6,
    25,       903.3,
    17,       587.5,
    16,       564.6
) |> 
    right_join(mapa_estado, by = c("estado" = "id")) 
#> # A tibble: 51,746 × 5
#>    estado  area  long   lat group
#>     <dbl> <dbl> <dbl> <dbl> <fct>
#>  1      3 1571. -58.1 -7.34 3.1  
#>  2      3 1571. -58.1 -7.36 3.1  
#>  3      3 1571. -58.1 -7.37 3.1  
#>  4      3 1571. -58.2 -7.38 3.1  
#>  5      3 1571. -58.2 -7.39 3.1  
#>  6      3 1571. -58.2 -7.43 3.1  
#>  7      3 1571. -58.2 -7.46 3.1  
#>  8      3 1571. -58.2 -7.47 3.1  
#>  9      3 1571. -58.2 -7.48 3.1  
#> 10      3 1571. -58.2 -7.49 3.1  
#> # … with 51,736 more rows
```
