## Lokale Änderungen an Skripten und Konfiguration

Das Docker-Image enthält Kopien der R-Skripte (`Main.R`, `config.R`, `R/*.R`)
sowie der `unit_conversion.csv`. Für Entwicklung und Tests können all diese
Dateien lokal geändert werden, **ohne** das Image neu zu bauen: `docker compose`
bindet die lokalen Dateien als Read-only-Volumes in den Container (siehe
`docker-compose.yml`) und überschreibt damit die im Image enthaltenen Kopien.

1. Die gewünschte Datei lokal ändern, z. B. `R/Statistic.R` oder `config.R`.
2. Ohne Rebuild neu starten:

   ```bash
   docker compose run --rm --user "$(id -u):$(id -g)" start_kalium
   ```

Ist eine Änderung am Image selbst nötig (z. B. neue R-Pakete oder eine neue
Version von `CRAN_SNAPSHOT_DATE` im Dockerfile), kann das Image lokal gebaut
und über `KALIUM_DOCKER_IMAGE` in der `.env` verwendet werden:

```bash
docker build -t kalium360:local .
```

```
KALIUM_DOCKER_IMAGE=kalium360:local
```

Startet man das Image ohne `docker compose` direkt mit `docker run`, müssen die
lokalen Dateien sowie die Umgebungsvariablen manuell übergeben werden,
anderenfalls greifen die im Image enthaltenen Kopien bzw. die Standardwerte:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -e KALIUM_DATA_DIR=/data \
  -e KALIUM_OUTPUT_DIR=/output \
  -e KALIUM_WORKING_DIR=/work \
  -e KALIUM_CRTDL=Kalium360_CRTDL_complex \
  -e KALIUM_SITE_PSEUDONYM=mein-pseudonym \
  -v "$(pwd)/Main.R:/app/Main.R:ro" \
  -v "$(pwd)/config.R:/app/config.R:ro" \
  -v "$(pwd)/R:/app/R:ro" \
  -v "$(pwd)/unit_conversion.csv:/app/unit_conversion.csv:ro" \
  -v "$(pwd)/input:/data:ro" \
  -v "$(pwd)/output:/output" \
  -v "$(pwd)/work:/work" \
  ghcr.io/medizininformatik-initiative/dup-kalium360:latest
```

## Releasen

Ein Release entsteht durch einen Git-Tag nach dem Schema `vMAJOR.MINOR.PATCH`,
z. B. `v9.9.9` (die Nummer im folgenden Beispiel ist ein Platzhalter und wird
bewusst nie als echter Tag existieren). Da Commits nur per PR auf `main`
kommen, wird der Tag auf einem kurzen Release-Branch gesetzt und mit dessen
PR gemerged:

```bash
git checkout -b release-v9.9.9
# in docker-compose.yml und README.md das Image-Tag `latest` durch
# `ghcr.io/medizininformatik-initiative/dup-kalium360:v9.9.9` ersetzen, im
# README zudem die Versionsnummer eintragen
git commit -am "prepare release v9.9.9"
git tag -a v9.9.9 -m "Release v9.9.9"
# erst NACH dem Taggen docker-compose.yml und README.md wieder auf `latest`
# zurücksetzen (der Versionspin bleibt so nur im getaggten Commit)
git commit -am "post-release: docker-compose back to latest"
git push origin release-v9.9.9 v9.9.9
# danach PR von release-v9.9.9 nach main öffnen
```

Die CI baut das Image für das Tag `v9.9.9` und aktualisiert das Tag `latest`,
wenn die Version die höchste bisher veröffentlichte ist. Ein Tag mit
unzulässigem Versionsschema lässt den Build fehlschlagen. Das
`docker-compose.yml` des Tags muss das Image exakt auf die Tag-Version pinnen
(die CI prüft das); das README sollte diese Version ebenfalls nennen. Durch
den Post-Release-Commit im PR nutzen `docker-compose.yml` und README nach dem
Merge in `main` wieder `latest`.

Der neueste (noch nicht freigegebene) Stand von `main` wird nach jedem Push
gebaut, sicherheitsgeprüft und unter dem Tag `dev` veröffentlicht. Um ihn zu
testen, kann in `.env` gesetzt werden:

```
KALIUM_DOCKER_IMAGE=ghcr.io/medizininformatik-initiative/dup-kalium360:dev
```
