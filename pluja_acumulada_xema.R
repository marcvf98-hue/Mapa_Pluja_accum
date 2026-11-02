# Mapa de pluja acumulada (90, 30 i 15 dies) i índex d'humitat per a bolets
# a partir de les dades obertes de la XEMA (Meteocat) publicades a
# analisi.transparenciacatalunya.cat, sobre el mapa base i l'ombrejat de l'ICGC
#
# Requisits:  install.packages(c("httr2", "jsonlite", "dplyr", "leaflet", "htmlwidgets"))
#             per a la interpolació: install.packages(c("sf", "terra", "gstat", "mapSpain"))
#             per a "ked", a més:     install.packages("geodata")   (opcional: "elevatr")
# Ús:         Rscript pluja_acumulada_xema.R   -> genera pluja_acumulada.html

# Interpolació:
#   "ked"     kriging amb l'altitud com a deriva externa (recomanat)
#   "kriging" kriging ordinari
#   "idw"     inversa de la distància
#   "cap"     només punts
INTERPOLACIO <- "ked"
RESOLUCIO_M  <- 1000   # mida de cel·la de la superfície interpolada (metres)
R2_MINIM_KED <- 0.15   # per sota d'aquest R², KED passa a kriging ordinari
MIN_ESTACIONS_INTERPOLAR <- 30  # amb menys estacions, només punts

if (INTERPOLACIO != "cap") {
  # es carreguen abans de dplyr perquè les funcions de dplyr no quedin emmascarades
  library(sf)
  library(terra)
  library(gstat)
  library(mapSpain)
}
library(httr2)
library(jsonlite)
library(dplyr)
library(leaflet)
library(htmlwidgets)

# Si dplyr ja estava carregat a la sessió, library(dplyr) no fa res i paquets
# carregats després (terra té select(), per exemple) poden passar-li per davant.
# Fixem explícitament les funcions de dplyr que fa servir l'script.
for (f in c("select", "filter", "rename", "mutate", "transmute", "summarise",
            "group_by", "arrange", "distinct", "count", "left_join", "inner_join",
            "bind_rows", "coalesce", "case_when", "last", "n")) {
  assign(f, getExportedValue("dplyr", f))
}
rm(f)

BASE       <- "https://analisi.transparenciacatalunya.cat/resource"
DIARIES    <- file.path(BASE, "7bvh-jvq2.json")  # dades diàries XEMA (lleuger)
SEMIHORARI <- file.path(BASE, "nzvn-apee.json")  # dades semihoràries XEMA (molt gran)
ESTACIONS  <- file.path(BASE, "yqwd-vj5e.json")  # metadades de les estacions
VAR_PLUJA_DIARIA <- "1300"  # precipitació acumulada diària (mm)
VAR_PLUJA_30MIN  <- "35"    # precipitació semihorària (mm)
VAR_TMITJ_DIARIA <- "1000"  # temperatura mitjana diària (°C)
VAR_TMIN_DIARIA  <- "1002"  # temperatura mínima diària (°C)

PERIODES <- c("Darrers 90 dies" = 90, "Darrers 30 dies" = 30, "Darrers 15 dies" = 15)

# --- Índex de bolets: humitat del sòl sostinguda --------------------------------
# Índex de precipitació antecedent (API): API_avui = K * API_ahir + pluja_efectiva_avui.
# Fa de "memòria" de la humitat del sòl. Un dia compta com a humit si API >= API_LLINDAR.
# Valors orientatius, no calibrats amb dades de bolets: ajusta'ls amb la teva experiència.
BOLETS             <- TRUE
BOLETS_DIES        <- 21    # finestra d'anàlisi (dies)
BOLETS_K           <- 0.85  # fracció d'humitat que es conserva d'un dia per l'altre
PLUJA_EFECTIVA_MAX <- 40    # mm/dia que el sòl pot absorbir; la resta s'escola
API_LLINDAR        <- 10    # mm: per sobre, el sòl es considera humit
# Pluja continuada: amb una ratxa de més d'aquests dies sense pluja (< 1 mm),
# l'estació no pot passar de "Favorable" encara que el sòl s'hagi mantingut humit.
# Posa-hi Inf per desactivar-ho (premia també un gran episodi seguit de dies secs).
BOLETS_RATXA_SECA_MAX <- 7

# K segons la temperatura: com més calor, més evaporació i el sòl s'asseca abans.
# K = BOLETS_K - BOLETS_K_PENDENT * (Tmitjana - BOLETS_T_REF), limitada a [K_MIN, K_MAX].
# Sense temperatura (o amb el pla B) es fa servir BOLETS_K fixa.
BOLETS_TEMPERATURA <- TRUE
BOLETS_T_REF       <- 12     # °C on K = BOLETS_K
BOLETS_K_PENDENT   <- 0.008  # 25 °C -> K 0,75 ; 5 °C -> K 0,91
BOLETS_K_MIN       <- 0.70
BOLETS_K_MAX       <- 0.95

# Glaçades (temperatura de l'aire a 1,5-2 m: a terra pot glaçar encara que no hi surti)
LLINDAR_GLACADA       <- 0   # °C: Tmin <= 0 compta com a dia de glaçada
LLINDAR_GLACADA_FORTA <- -2  # °C: una sola glaçada així ja frena la fructificació
DIES_GLACADA_FRENEN   <- 3   # o bé aquest nombre de glaçades febles
DIES_GLACADA_RECENTS  <- 10  # només compten les glaçades dels darrers N dies
COBERTURA_MINIMA <- 0.8  # descarta estacions amb menys del 80% de dies amb dada
# opcional: token gratuït del portal per evitar límits (o variable d'entorn SOCRATA_APP_TOKEN)
APP_TOKEN <- if (nzchar(Sys.getenv("SOCRATA_APP_TOKEN"))) Sys.getenv("SOCRATA_APP_TOKEN") else NULL
# fitxer de sortida (a GitHub es desa a public/index.html amb la variable SORTIDA_HTML)
SORTIDA_HTML <- Sys.getenv("SORTIDA_HTML", "pluja_acumulada.html")

soda_get <- function(url, params, timeout = 300) {
  req <- request(url) |>
    req_url_query(!!!params) |>
    req_timeout(timeout) |>
    req_retry(max_tries = 3, retry_on_failure = TRUE,   # també si la connexió falla o fa timeout
              is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
    req_error(is_error = function(resp) FALSE)
  if (!is.null(APP_TOKEN)) req <- req_headers(req, `X-App-Token` = APP_TOKEN)
  resp <- req_perform(req)
  if (resp_status(resp) >= 400) {
    stop(sprintf("HTTP %d: %s", resp_status(resp),
                 substr(resp_body_string(resp), 1, 500)))
  }
  out <- fromJSON(resp_body_string(resp))
  if (length(out) == 0) data.frame() else out
}

obtenir_estacions <- function() {
  soda_get(ESTACIONS, list(`$limit` = 5000)) |>
    filter(nom_estat_ema == "Operativa") |>
    transmute(codi_estacio, nom_estacio, nom_municipi, nom_comarca,
              altitud = suppressWarnings(as.numeric(altitud)),
              latitud = as.numeric(latitud), longitud = as.numeric(longitud)) |>
    filter(!is.na(latitud), !is.na(longitud))
}

.cache <- new.env()

# Font principal: dades diàries (unes 17.000 files per a 90 dies; una sola petició).
# Tenen un o dos dies de retard respecte d'avui.
pluja_diaria <- function(dies_max) {
  inici <- format(Sys.Date() - dies_max - 3, "%Y-%m-%dT00:00:00")
  vars <- c(VAR_PLUJA_DIARIA, if (BOLETS && BOLETS_TEMPERATURA) c(VAR_TMITJ_DIARIA, VAR_TMIN_DIARIA))
  vars_in <- paste0("('", paste(vars, collapse = "','"), "')")
  blocs <- list(); offset <- 0; pagina <- 50000
  repeat {
    bloc <- soda_get(DIARIES, list(
      `$select` = "codi_estacio,data_lectura,codi_variable,nom_variable,valor",
      `$where`  = sprintf("codi_variable IN %s AND data_lectura >= '%s'", vars_in, inici),
      # l'ordre ha de ser únic perquè la paginació no perdi ni repeteixi files
      `$order`  = "data_lectura,codi_estacio,codi_variable",
      `$limit`  = pagina, `$offset` = offset))
    if (nrow(bloc) == 0) break
    blocs[[length(blocs) + 1]] <- bloc
    if (nrow(bloc) < pagina) break
    offset <- offset + pagina
  }
  if (length(blocs) == 0) {
    # ajuda per diagnosticar: quines variables hi ha al conjunt diari?
    v <- soda_get(DIARIES, list(`$select` = "codi_variable,nom_variable",
                                `$order` = "data_lectura DESC", `$limit` = 2000))
    print(unique(v))
    stop("el conjunt diari no té la variable ", VAR_PLUJA_DIARIA,
         " (mira la llista de dalt i canvia VAR_PLUJA_DIARIA)")
  }
  brut <- bind_rows(blocs) |>
    transmute(codi_estacio, codi_variable, nom_variable,
              data = as.Date(substr(data_lectura, 1, 10)),
              valor = suppressWarnings(as.numeric(valor))) |>
    distinct(codi_estacio, data, codi_variable, .keep_all = TRUE)  # per si el portal repeteix registres

  # comprova què és cada codi (els de temperatura no estan verificats)
  noms <- distinct(brut, codi_variable, nom_variable)
  for (i in seq_len(nrow(noms))) message(sprintf("  variable %s: %s", noms$codi_variable[i], noms$nom_variable[i]))
  es_temp <- function(codi) any(noms$codi_variable == codi & grepl("emperatura", noms$nom_variable))
  vol_temp <- BOLETS && BOLETS_TEMPERATURA
  tmitj_ok <- vol_temp && es_temp(VAR_TMITJ_DIARIA)
  tmin_ok  <- vol_temp && es_temp(VAR_TMIN_DIARIA)
  if (vol_temp && !tmitj_ok)
    message("  AVÍS: el codi ", VAR_TMITJ_DIARIA, " no és una temperatura; l'índex de bolets fa servir K fixa.")
  if (vol_temp && !tmin_ok)
    message("  AVÍS: el codi ", VAR_TMIN_DIARIA, " no és una temperatura; no es tenen en compte les glaçades.")

  treu <- function(codi, valid = TRUE) {
    if (!valid) return(brut[0, c("codi_estacio", "data")] |> mutate(v = numeric()))
    filter(brut, codi_variable == codi) |> select(codi_estacio, data, v = valor)
  }
  # base: tots els dies amb alguna variable, perquè la temperatura d'un dia sense
  # dada de pluja no es perdi (la pluja hi queda com a NA)
  distinct(brut, codi_estacio, data) |>
    left_join(treu(VAR_PLUJA_DIARIA)          |> rename(pluja = v), by = c("codi_estacio", "data")) |>
    left_join(treu(VAR_TMITJ_DIARIA, tmitj_ok) |> rename(tmitj = v), by = c("codi_estacio", "data")) |>
    left_join(treu(VAR_TMIN_DIARIA,  tmin_ok)  |> rename(tmin = v),  by = c("codi_estacio", "data"))
}

# Pla B: dades semihoràries, una petició per estació (filtre més selectiu,
# cada consulta és petita i no arriba al límit de temps del servidor).
pluja_semihoraria <- function(dies_max, codis) {
  inici <- format(Sys.Date() - dies_max, "%Y-%m-%dT00:00:00")
  res <- vector("list", length(codis))
  for (i in seq_along(codis)) {
    if (i %% 20 == 1) message(sprintf("  ... estació %d de %d", i, length(codis)))
    res[[i]] <- tryCatch(soda_get(SEMIHORARI, list(
      `$select` = "codi_estacio,data_lectura,valor_lectura",
      `$where`  = sprintf("codi_estacio='%s' AND codi_variable='%s' AND data_lectura >= '%s'",
                          codis[i], VAR_PLUJA_30MIN, inici),
      `$limit`  = 10000), timeout = 120),
      error = function(e) { message("  ", codis[i], ": ", conditionMessage(e)); NULL })
  }
  # agrega a valors diaris (dia UTC) i compta les lectures per dia
  bind_rows(res) |>
    mutate(data = as.Date(substr(data_lectura, 1, 10)),
           v = suppressWarnings(as.numeric(valor_lectura))) |>
    group_by(codi_estacio, data) |>
    summarise(pluja = if (sum(!is.na(v)) >= 40) sum(v, na.rm = TRUE) else NA_real_,
              .groups = "drop") |>   # un dia compta si té almenys 40 de 48 lectures
    mutate(tmitj = NA_real_, tmin = NA_real_)  # el pla B no té temperatures: K fixa
}

# Dies que cal descarregar: el període més llarg, o la finestra de bolets més
# 30 dies perquè l'índex d'humitat (API) tingui en compte la pluja anterior
dies_necessaris <- function() max(PERIODES, if (BOLETS) BOLETS_DIES + 30 else 0)

# Últim dia de l'anàlisi. Un dia és "complet" si en tenen dada almenys el 80%
# de les estacions. El portal de vegades publica els dies recents desordenats
# (p. ex. ja té el 31 però encara no del 25 al 28): per això no n'hi ha prou amb
# l'últim dia complet; cal que la finestra més curta sencera ho sigui. Si no,
# els dies que falten comptarien com a dies secs.
tria_data_fi <- function(d) {
  per_dia <- d |> filter(!is.na(pluja)) |> count(data)
  dies <- seq(min(per_dia$data), max(per_dia$data), by = "day")
  ok <- dies %in% per_dia$data[per_dia$n >= COBERTURA_MINIMA * max(per_dia$n)]
  finestra <- min(PERIODES, if (BOLETS) BOLETS_DIES)
  fi <- NA_integer_
  for (i in rev(seq_along(dies))) {
    if (i >= finestra && all(ok[(i - finestra + 1):i])) { fi <- i; break }
  }
  if (is.na(fi)) {   # cap finestra sencera completa: com a mínim, l'últim dia complet
    fi <- max(which(ok))
    message("  AVÍS: no hi ha cap tram de ", finestra, " dies seguits complets; els acumulats poden quedar curts.")
  }
  posteriors <- dies[seq_along(dies) > fi]
  if (length(posteriors) > 0) {
    incomplets <- posteriors[!ok[seq_along(dies) > fi]]
    message(sprintf("  El portal té dades fins al %s, però hi falten dies (%s): l'anàlisi s'atura al %s.",
                    format(max(dies), "%d/%m"),
                    if (length(incomplets)) paste(format(incomplets, "%d/%m"), collapse = ", ") else "cap",
                    format(dies[fi], "%d/%m/%Y")))
  }
  dies[fi]
}

# Descarrega una vegada les dades diàries del període més llarg
dades_pluja <- function(codis) {
  if (is.null(.cache$pluja)) {
    dies_max <- dies_necessaris()
    message("Descarregant la precipitació diària...")
    .cache$pluja <- tryCatch(pluja_diaria(dies_max), error = function(e) {
      message("Dades diàries no disponibles (", conditionMessage(e),
              ").\nPla B: dades semihoràries estació per estació (pot trigar uns minuts).")
      pluja_semihoraria(dies_max, codis)
    })
    .cache$data_fi <- tria_data_fi(.cache$pluja)
    message(sprintf("  %d valors diaris, últim dia amb dades: %s",
                    nrow(.cache$pluja), format(.cache$data_fi, "%d/%m/%Y")))
  }
  .cache$pluja
}

pluja_acumulada <- function(dies, codis) {
  d <- dades_pluja(codis)
  fi <- .cache$data_fi
  d |>
    filter(data > fi - dies, data <= fi, !is.na(pluja)) |>
    group_by(codi_estacio) |>
    summarise(pluja = round(sum(pluja), 1), n = n(), .groups = "drop") |>
    mutate(cobertura = n / dies) |>
    filter(cobertura >= COBERTURA_MINIMA)
}

# --- Interpolació ------------------------------------------------------------

contorn_catalunya <- function() {
  if (is.null(.cache$contorn)) {
    .cache$contorn <- esp_get_ccaa(ccaa = "Cataluña") |>
      st_transform(25831)  # ETRS89 / UTM 31N, en metres
  }
  .cache$contorn
}

# Model digital del terreny (~1 km) a la mateixa graella que la interpolació.
# Font principal: SRTM 30" via geodata; alternativa: AWS Terrain Tiles via elevatr.
model_terreny <- function(plantilla, contorn) {
  if (is.null(.cache$dem)) {
    message("  carregant el model digital del terreny (es descarrega només la primera vegada)...")
    carpeta <- tools::R_user_dir("pluja_xema", which = "cache")
    dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
    dem <- tryCatch(
      geodata::elevation_30s(country = "ESP", path = carpeta),
      error = function(e) {
        if (!requireNamespace("elevatr", quietly = TRUE)) stop(e)
        message("  geodata no respon; provo amb elevatr.")
        contorn_4326 <- st_transform(contorn, 4326)
        rast(elevatr::get_elev_raster(contorn_4326, z = 7, clip = "bbox"))
      })
    dem <- terra::project(dem, plantilla, method = "bilinear")
    dem <- terra::mask(dem, vect(contorn))
    names(dem) <- "alt"
    .cache$dem <- dem
  }
  .cache$dem
}

ajusta_variograma <- function(formula, d) {
  # distància màxima del variograma: 1/3 de la diagonal de la zona (~120 km a Catalunya)
  diag_m <- sqrt(diff(range(d$X))^2 + diff(range(d$Y))^2)
  cutoff <- diag_m / 3
  # estimador robust de Cressie-Hawkins: menys sensible a aiguats molt localitzats
  v <- variogram(formula, ~X + Y, data = d, cutoff = cutoff, width = cutoff / 15,
                 cressie = TRUE)
  if (is.null(v) || nrow(v) < 3) stop("massa poques parelles d'estacions per al variograma")
  semivar <- max(v$gamma)

  # prova cada model amb valors inicials raonables i es queda amb el millor que
  # convergeixi i tingui un abast creïble (entre 5 km i el doble del cutoff)
  millor <- NULL; millor_sse <- Inf
  for (m in c("Sph", "Exp", "Gau")) {
    convergit <- TRUE
    fv <- withCallingHandlers(
      tryCatch(fit.variogram(v, vgm(psill = 0.8 * semivar, model = m,
                                    range = cutoff / 3, nugget = 0.2 * semivar)),
               error = function(e) NULL),
      warning = function(w) { convergit <<- FALSE; invokeRestart("muffleWarning") })
    if (is.null(fv) || !convergit || attr(fv, "singular")) next
    abast <- fv$range[nrow(fv)]
    if (abast < 5000 || abast > 2 * cutoff || any(fv$psill < 0)) next
    sse <- attr(fv, "SSErr")
    if (sse < millor_sse) { millor <- fv; millor_sse <- sse }
  }
  if (is.null(millor)) stop("cap variograma no convergeix amb un abast raonable")

  n <- nrow(millor)
  message(sprintf("  variograma: %s, abast %.0f km, efecte pepita %.0f%%",
                  as.character(millor$model[n]), millor$range[n] / 1000,
                  100 * millor$psill[1] / sum(millor$psill)))
  millor
}

interpolar <- function(df, metode = INTERPOLACIO, resolucio = RESOLUCIO_M) {
  contorn <- contorn_catalunya()
  pts <- st_as_sf(df, coords = c("longitud", "latitud"), crs = 4326) |>
    st_transform(25831)
  d <- data.frame(st_coordinates(pts), pluja = df$pluja)  # columnes X, Y, pluja
  plantilla <- rast(vect(contorn), resolution = resolucio)

  # IDW: veïns més propers (12), pes = 1 / distància^2 (també és el pla de rescat)
  model_idw <- gstat(formula = pluja ~ 1, locations = ~X + Y, data = d,
                     nmax = 12, set = list(idp = 2))
  model <- model_idw

  if (metode == "ked") {
    model <- tryCatch({
      dem <- model_terreny(plantilla, contorn)
      # altitud a cada estació presa del mateix MDT que la graella (coherència);
      # si cau fora (estació a la vora), es fa servir l'altitud de les metadades
      d$alt <- terra::extract(dem, vect(pts), method = "bilinear")[["alt"]]
      d$alt <- ifelse(is.na(d$alt), df$altitud, d$alt)

      ajust <- lm(pluja ~ alt, data = d)
      r2 <- summary(ajust)$r.squared
      message(sprintf("  relació pluja-altitud: %+.1f mm per cada 100 m, R² = %.2f",
                      100 * coef(ajust)[["alt"]], r2))
      # si l'altitud no explica gairebé res, només afegeix soroll: kriging ordinari
      if (r2 < R2_MINIM_KED) stop(sprintf("R² < %.2f, l'altitud no aporta", R2_MINIM_KED))

      fv <- ajusta_variograma(pluja ~ alt, d)  # variograma dels residus
      plantilla <- dem                          # la graella porta l'altitud com a capa "alt"
      gstat(formula = pluja ~ alt, locations = ~X + Y, data = d, model = fv, nmax = 30)
    }, error = function(e) {
      message("  KED no ha funcionat (", conditionMessage(e), "); provo kriging ordinari.")
      metode <<- "kriging"
      NULL
    })
  }

  if (metode == "kriging") {
    model <- tryCatch({
      fv <- ajusta_variograma(pluja ~ 1, d)
      gstat(formula = pluja ~ 1, locations = ~X + Y, data = d, model = fv, nmax = 30)
    }, error = function(e) {
      message("  El kriging no ha convergit (", conditionMessage(e), "); faig IDW.")
      model_idw
    })
  }

  if (is.null(model)) model <- model_idw
  # amb KED, les cel·les sense altitud (fora de Catalunya) se salten
  r <- terra::interpolate(plantilla, model, debug.level = 0, xyNames = c("X", "Y"),
                   na.rm = (metode == "ked"))[[1]]
  r <- terra::mask(r, vect(contorn))
  terra::clamp(r, lower = 0, values = TRUE)  # el kriging pot donar valors negatius
}

# --- Índex de bolets ----------------------------------------------------------

# API_t = K_t * API_(t-1) + efectiva_t, amb una K diferent cada dia
calcula_api <- function(efectiva, k) {
  api <- numeric(length(efectiva))
  ant <- 0
  for (i in seq_along(efectiva)) { ant <- k[i] * ant + efectiva[i]; api[i] <- ant }
  api
}

ratxa_seca_max <- function(p) {
  r <- rle(p < 1)                          # dies sense pluja (< 1 mm)
  if (any(r$values)) max(r$lengths[r$values]) else 0L
}

index_bolets <- function(codis, finestra = BOLETS_DIES) {
  d  <- dades_pluja(codis)
  fi <- .cache$data_fi
  dies <- seq(fi - dies_necessaris() + 1, fi, by = "day")   # tot el període: l'API "s'escalfa"

  graella <- expand.grid(codi_estacio = unique(d$codi_estacio), data = dies,
                         stringsAsFactors = FALSE)
  graella |>
    left_join(d, by = c("codi_estacio", "data")) |>
    arrange(codi_estacio, data) |>
    group_by(codi_estacio) |>
    mutate(efectiva = pmin(coalesce(pluja, 0), PLUJA_EFECTIVA_MAX),
           k_dia = if (BOLETS_TEMPERATURA)
                     coalesce(pmin(BOLETS_K_MAX, pmax(BOLETS_K_MIN,
                       BOLETS_K - BOLETS_K_PENDENT * (tmitj - BOLETS_T_REF))), BOLETS_K)
                   else rep(BOLETS_K, n()),
           api = calcula_api(efectiva, k_dia)) |>
    filter(data > fi - finestra) |>
    summarise(
      n           = sum(!is.na(pluja)),
      dies_humits = sum(api >= API_LLINDAR),
      dies_pluja  = sum(pluja >= 1, na.rm = TRUE),
      ratxa_seca  = ratxa_seca_max(coalesce(pluja, 0)),
      max_dia     = if (any(!is.na(pluja))) max(pluja, na.rm = TRUE) else NA_real_,
      api_avui    = round(last(api), 1),
      k_mitjana   = mean(k_dia),
      dies_temp   = sum(!is.na(tmitj)),
      tmin_abs    = { tr <- tmin[data > fi - DIES_GLACADA_RECENTS]
                      if (any(!is.na(tr))) min(tr, na.rm = TRUE) else NA_real_ },
      glacades    = sum(tmin[data > fi - DIES_GLACADA_RECENTS] <= LLINDAR_GLACADA, na.rm = TRUE),
      glacada_forta = any(tmin[data > fi - DIES_GLACADA_RECENTS] <= LLINDAR_GLACADA_FORTA, na.rm = TRUE),
      total       = round(sum(pluja, na.rm = TRUE), 1),
      .groups = "drop") |>
    filter(n >= COBERTURA_MINIMA * finestra) |>
    mutate(
      cobertura = n / finestra,
      frenat = glacada_forta | glacades >= DIES_GLACADA_FRENEN,
      valoracio = case_when(
        frenat & dies_humits >= 0.60 * finestra      ~ "Humit, però frenat per glaçades",
        dies_humits >= 0.85 * finestra & total >= 50 &
          ratxa_seca <= BOLETS_RATXA_SECA_MAX        ~ "Molt favorable",
        dies_humits >= 0.85 * finestra & total >= 50 ~ "Favorable (pluja poc continuada)",
        dies_humits >= 0.60 * finestra & total >= 30 ~ "Favorable",
        dies_humits >= 0.30 * finestra               ~ "Marginal",
        TRUE                                         ~ "Desfavorable"))
}

COLOR_GLACADA <- "#1d4ed8"   # contorn de les estacions frenades per glaçades

afegeix_capa_bolets <- function(mapa, est, grups) {
  nom <- sprintf("Bolets: sòl humit (%d dies)", BOLETS_DIES)
  df <- index_bolets(est$codi_estacio) |> inner_join(est, by = "codi_estacio")
  if (nrow(df) == 0) { message("Sense dades per a l'índex de bolets"); return(list(mapa = mapa, grups = grups)) }
  message(sprintf("%s: %d estacions; %d molt favorables, %d favorables", nom, nrow(df),
                  sum(df$valoracio == "Molt favorable"), sum(startsWith(df$valoracio, "Favorable"))))

  pal <- colorNumeric(c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"),
                      domain = c(0, BOLETS_DIES * 1.01), na.color = "transparent")

  if (INTERPOLACIO != "cap" && nrow(df) >= MIN_ESTACIONS_INTERPOLAR) {
    message("  interpolant l'índex (idw)...")
    r <- interpolar(mutate(df, pluja = dies_humits), metode = "idw")
    r <- terra::clamp(r, lower = 0, upper = BOLETS_DIES, values = TRUE)
    mapa <- addRasterImage(mapa, r, colors = pal, opacity = OPACITAT_PLUJA, group = nom,
                           options = gridOptions(pane = "pluja"))
  }

  mapa <- mapa |>
    addCircleMarkers(
      data = df, lng = ~longitud, lat = ~latitud, group = nom,
      radius = ~ifelse(frenat, 5.5, 4), options = pathOptions(pane = "estacions"),
      color = ~ifelse(frenat, COLOR_GLACADA, "#333"), weight = ~ifelse(frenat, 2.5, 0.5),
      opacity = 1, fillColor = ~pal(dies_humits), fillOpacity = 0.9,
      label = ~sprintf("%s: %d de %d dies humits (%s)", nom_estacio, dies_humits,
                       BOLETS_DIES, valoracio),
      popup = ~sprintf(paste0(
        "<b>%s</b><br>%s (%s), %.0f m<br><b>%s</b><hr style='margin:4px 0'>",
        "Dies amb sòl humit: <b>%d de %d</b><br>",
        "Pluja total: %.1f mm en %d dies de pluja<br>",
        "Dia més plujós: %.1f mm<br>",
        "Ratxa seca més llarga: %d dies<br>",
        "Humitat actual (API): %.1f mm<br>",
        "%s"),
        nom_estacio, nom_municipi, nom_comarca, altitud, valoracio,
        dies_humits, BOLETS_DIES, total, dies_pluja, max_dia, ratxa_seca, api_avui,
        paste0(
          ifelse(dies_temp == 0, "<i>Sense temperatura mitjana: K fixa</i>",
                 sprintf("Retenció mitjana (K): %.2f", k_mitjana)),
          "<br>",
          ifelse(is.na(tmin_abs), "<i>Sense temperatura mínima: glaçades no avaluades</i>",
                 sprintf("Glaçades (darrers %d dies): %d, mínima %.1f °C",
                         DIES_GLACADA_RECENTS, glacades, tmin_abs))))) |>
    addLegend("bottomright", pal = pal, values = c(0, BOLETS_DIES),
              className = paste0("info legend llegenda-", length(grups)),
              title = sprintf(paste0(
                "Dies amb sòl humit<br>(darrers %d)<br>",
                "<span style='display:inline-block;width:9px;height:9px;border-radius:50%%;",
                "border:2.5px solid %s;vertical-align:middle'></span> ",
                "<span style='font-weight:normal'>frenat per glaçades</span>"),
                BOLETS_DIES, COLOR_GLACADA))

  list(mapa = mapa, grups = c(grups, nom))
}

# --- Mapa de fons i relleu (ICGC, llicència CC-BY) ----------------------------
# Mapa base: "topografic-gris", "simplificat", "estandard-gris", "topografic-mut-gris"...
# (llista: https://www.icgc.cat/en/Geoinformation-and-Maps/Base-Map-Service)
FONS_ICGC     <- "topografic-gris"
OMBRES        <- TRUE   # ombrejat del relleu a sobre de la pluja (es pot activar/desactivar al mapa)
OPACITAT_OMBRES <- 0.5
OPACITAT_PLUJA  <- 0.7

ICGC_ATRIB <- '&copy; <a href="https://www.icgc.cat">Institut Cartogràfic i Geològic de Catalunya</a>'

afegeix_fons <- function(mapa) {
  mapa |>
    # capes per ordre (z-index): fons 200 < pluja 250 < ombres 300 < estacions 450
    addMapPane("pluja", zIndex = 250) |>
    addMapPane("ombres", zIndex = 300) |>
    addMapPane("estacions", zIndex = 450) |>
    addTiles(
      urlTemplate = sprintf(
        "https://geoserveis.icgc.cat/servei/catalunya/mapa-base/wmts/%s/MON3857NW/{z}/{x}/{y}.png",
        FONS_ICGC),
      attribution = ICGC_ATRIB,
      options = tileOptions(maxZoom = 18))
}

afegeix_ombres <- function(mapa) {
  if (!OMBRES) return(mapa)
  mapa |>
    addWMSTiles(
      "https://geoserveis.icgc.cat/servei/catalunya/elevacions-territorial/wms",
      layers = "model-elevacions-terreny-ombrejat-catalunya-topografic-5m-2009-2018",
      group = "Relleu (ombrejat ICGC)",
      attribution = ICGC_ATRIB,
      options = WMSTileOptions(format = "image/png", transparent = TRUE,
                               version = "1.3.0", opacity = OPACITAT_OMBRES,
                               pane = "ombres")) |>
    # "multiply": el blanc no canvia res i el gris enfosqueix, així el relleu
    # es veu a través dels colors de la pluja sense rentar-los
    prependContent(htmltools::tags$style(
      ".leaflet-ombres-pane { mix-blend-mode: multiply; pointer-events: none; }"))
}

main <- function() {
  .cache$pluja <- NULL; .cache$data_fi <- NULL  # dades noves a cada execució (el MDT es manté)
  est <- obtenir_estacions()

  mapa <- leaflet() |>
    afegeix_fons() |>
    setView(lng = 1.7, lat = 41.7, zoom = 8)

  colors <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b", "#7a0177")
  grups <- character(0)

  for (nom in names(PERIODES)) {
    dies <- PERIODES[[nom]]
    df <- pluja_acumulada(dies, est$codi_estacio) |> inner_join(est, by = "codi_estacio")
    if (nrow(df) == 0) { message("Sense dades per a ", nom); next }
    message(sprintf("%s: %d estacions, màx %.1f mm (%s)", nom, nrow(df),
                    max(df$pluja), df$nom_estacio[which.max(df$pluja)]))

    vmax_color <- max(quantile(df$pluja, 0.98), 1)
    # domini una mica més ample: el reprojectat del ràster pot sortir-se'n per decimals
    pal <- colorNumeric(colors, domain = c(0, vmax_color * 1.01), na.color = "transparent")
    vmax <- max(max(df$pluja), 1)

    if (INTERPOLACIO != "cap" && nrow(df) < MIN_ESTACIONS_INTERPOLAR) {
      message("  només ", nrow(df), " estacions: no s'interpola, només es mostren els punts.")
      df$radi <- 4 + 10 * sqrt(df$pluja / vmax)
    } else if (INTERPOLACIO != "cap") {
      message("  interpolant (", INTERPOLACIO, ")...")
      r <- terra::clamp(interpolar(df), upper = vmax_color, values = TRUE)
      mapa <- addRasterImage(mapa, r, colors = pal, opacity = OPACITAT_PLUJA, group = nom,
                             options = gridOptions(pane = "pluja"))
      df$radi <- 3.5                                  # amb superfície, punts petits
    } else {
      df$radi <- 4 + 10 * sqrt(df$pluja / vmax)       # sense, mida segons la pluja
    }

    mapa <- mapa |>
      addCircleMarkers(
        data = df, lng = ~longitud, lat = ~latitud, group = nom,
        radius = ~radi, options = pathOptions(pane = "estacions"),
        color = "#333", weight = 0.5, opacity = 1,
        fillColor = ~pal(pmin(pluja, vmax_color)), fillOpacity = 0.85,
        label = ~sprintf("%s: %.1f mm", nom_estacio, pluja),
        popup = ~sprintf(
          "<b>%s</b><br>%s (%s)<br>Altitud: %.0f m<br><b>%.1f mm</b> en %d dies<br>Dies amb dades: %.0f%%",
          nom_estacio, nom_municipi, nom_comarca, altitud, pluja, dies, 100 * cobertura)) |>
      addLegend("bottomright", pal = pal, values = c(0, vmax_color),
                className = paste0("info legend llegenda-", length(grups)),
                title = paste0("Pluja (mm)<br>", tolower(nom)))
    grups <- c(grups, nom)
  }

  if (BOLETS) {
    res <- afegeix_capa_bolets(mapa, est, grups)
    mapa <- res$mapa; grups <- res$grups
  }

  titol <- sprintf("<b>Pluja acumulada – XEMA (Meteocat)</b><br>Fins al %s (inclòs)",
                   format(.cache$data_fi, "%d/%m/%Y"))
  mapa <- mapa |>
    afegeix_ombres() |>
    addLayersControl(baseGroups = grups,
                     overlayGroups = if (OMBRES) "Relleu (ombrejat ICGC)",
                     options = layersControlOptions(collapsed = FALSE)) |>
    addControl(titol, position = "topleft")

  # mostra només la llegenda del període seleccionat
  mapa <- onRender(mapa, sprintf("
    function(el, x) {
      var noms = %s;
      function mostra(nom) {
        noms.forEach(function(n, i) {
          var e = el.querySelector('.llegenda-' + i);
          if (e) e.style.display = (n === nom) ? 'block' : 'none';
        });
      }
      mostra(noms[0]);
      // segons la versió, 'this' és el mapa o el widget
      var map = (typeof this.on === 'function') ? this : this.getMap();
      map.on('baselayerchange', function(e) { mostra(e.name); });
    }", toJSON(grups)))

  # es desa a la carpeta actual i després es copia: saveWidget autònom és més
  # fiable així que no pas escrivint directament en una subcarpeta
  tryCatch(
    saveWidget(mapa, "pluja_acumulada.html", selfcontained = TRUE,
               title = "Pluja acumulada i bolets – XEMA"),
    error = function(e) {
      message("No s'ha pogut fer un HTML autònom (", conditionMessage(e),
              "); el deso amb la carpeta pluja_acumulada_files al costat.")
      saveWidget(mapa, "pluja_acumulada.html", selfcontained = FALSE)
    })
  if (normalizePath(SORTIDA_HTML, mustWork = FALSE) !=
      normalizePath("pluja_acumulada.html", mustWork = FALSE)) {
    dir.create(dirname(SORTIDA_HTML), recursive = TRUE, showWarnings = FALSE)
    file.copy("pluja_acumulada.html", SORTIDA_HTML, overwrite = TRUE)
  }
  message("Mapa desat a ", SORTIDA_HTML)
  invisible(mapa)
}

if (sys.nframe() == 0) main()
