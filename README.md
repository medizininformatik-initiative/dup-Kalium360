# Kalium360

Kalium360 untersucht wie gut klinisch relevante Aspekte von Kalium-
Laborwerten in FHIR-strukturierten Routinedaten abgebildet sind. Neben den
Laborwerten selbst werden qualitätsbestimmende Faktoren 
(z. B. Hämolyse), Prozeduren (Dialyse) und Medikamente, die Kalium-
werte beeinflussen können betrachtet.

Kalium360 setzt auf eine Datenausleitung mit der DUP-Pipeline und anschließende dezentrale Analyse mit dem hier bereitgestelltem Skript. Es verlassen ausschließlich aggregierte Daten den Standort.

## Fragebogen

Der RedCap-Fragebogen sollte von jedem teilnehmenden Standort einmal ausgefüllt werden. Geben Sie dort ein selbst gewähltes Standort-Pseudonym an. Merken Sie sich ihr Pseudonym und geben sie es später bei der Datenausleitung mit an. Auf diese Weise können wir die Antworten mit den Daten zusammenführen.
Der Fragebogen findet sich unter: [Umfrage Kalium360](https://kurzlinks.de/Kalium360Umfrage)

## Schritt 1: Datenausleitung mit der DUP-Pipeline

Die benötigten Daten werden von dem lokalen FHIR-Server mit Hilfe der DUP-Pipeline ausgeleitet. Benötigt werden dabei die Schritte TORCH und FHIR-flattener. DIMPen ist für das Projekt nicht unbedingt erforderlich wird aber für ein einheitliches Vorgehen empfohlen. Falls Sie diesen Schritt überspringen möchten, können Sie die entsprechenden Stellen in der aether-kalium.yml auskommentieren. 
Eine Konfiguration für aether (aether-kalium.yml) liegt ebenso wie die aktuelle flatteningLookup.json und den für das DIMPen erforderlichen Dateien dem Projekt bei. In der aether-kalium.yml müssen die benötigten services, insbesondere TORCH, konfiguriert werden. 

Es sollten die jeweils aktuellen Versionen genutzt werden:\
AETHER: 1.4.0 https://github.com/medizininformatik-initiative/aether/releases#release-v1.4.0 \
TORCH: 1.0.1 https://github.com/medizininformatik-initiative/torch/releases#release-v1.0.1 \
FHIR-flattener: v0.1.0-alpha.9 https://github.com/medizininformatik-initiative/fhir-flattener/releases#v0.1.0-alpha.9

Die DUP-Pipeline lässt sich für die jeweilige CRTDL mit folgendem Befehl starten:
   ```bash
      aether pipeline start aether-kalium.yml Kalium360_CRTDL_complex_v1.json
   ```
   ```bash
      aether pipeline start aether-kalium.yml Kalium360_CRTDL_basis_v1.json
   ```

### CRTDL Varianten

Diesem Projekt liegen zwei CRTDLs bei. 

- Kalium360_CRTDL_basis: Diese CRTDL funktioniert mit jedem FHIR-Server. Allerdings können MedicationAdministrations mit einer zweifachen Referenz nicht gefunden und ausgeleitet werden. Gemeint sind MedicationAdministrations die auf eine Medication verweisen die wiederrum auf eine weitere Medication verweisen. 

- Kalium360_CRTDL_complex: Diese CRTDL kann auch MedicationAdministrations mit zweifacher Referenz berücksichtigen. Allerdings wird dafür ein Blaze ab Version 1.8 benötigt.

Beide CRTDLs sind für das Projekt geeignet. Wenn die complexe CRTDL an Ihrem Standort ausgeführt werden kann, verwenden Sie bitte diese, ansonsten die basis Version. 

Hinweis: beide CRTDLs enthalten händisch hinzugefügte Filter für Medikation. Beim Hochladen in das FDPG Portal werden diese Filter ohne Fehlermeldung entfernt. Bitte nutzen Sie die CRTDLs so wie sie hier bereitgestellt werden. 

### Output der DUP-Pipeline
Je nachdem welche Daten an ihrem Standort vorliegen, können nach Durchlaufen der DUP-pipeline bis zu 13 verschiedene .csv Dateien entstehen: 
Laborwerte.csv, Kalium Patienten.csv, Bioproben.csv, Fall.csv, Prozeduren.csv, Prozeduren ICU.csv, Haemodialyse.csv, Conditions.csv, Administration Codeableconcept.csv, Administration MedicationCode.csv, Administration Complex.csv, Medikation.csv, ServiceRequest.csv

Lediglich Laborwerte.csv und Kalium Patienten.csv sind Pflicht. Alle anderen Dateien sind optional und werden in vielen Fällen nicht vorliegen. Die ServiceRequest.csv wird nicht benötigt. Sie enthält keine Attribute und wird aus technischen Gründen ausgeleitet um eine evt. vorhandene BasedOn-Referenz in den Observations zu erkennen.  

## Schritt 2: Verteilte Analyse Skript

Da Kalium ein häufig gemessener Laborparameter ist, erwarten wir an einigen Standorten größere Datenmengen. 
Das Skript setzt auf ein datenbanklastiges Design mit einer DuckDB im Hintergrund. Auf diese Weise können auch Datensätze mit mehreren Millionen Observations performant verarbeitet werden. 

### Starten mit Docker

Voraussetzung: Docker ist installiert.

Das Docker-Image wird in der GitHub-Actions-CI gebaut und unter
`ghcr.io/medizininformatik-initiative/dup-kalium360` veröffentlicht: Jeder Push
auf `main` unter dem Tag `dev`, jeder Git-Tag nach dem Schema
`vMAJOR.MINOR.PATCH` (SemVer) unter diesem Versions-Tag, und das Tag `latest`
zeigt auf die höchste veröffentlichte Version. `docker-compose.yml` nutzt
standardmäßig das Tag `latest`. Ein lokales Bauen ist dafür nicht nötig.

1. Repository klonen und in den Ordner wechseln.
2. In `.env` folgende Werte eintragen:
   - KALIUM_DATA_PATH: Input Ordner mit dem DUP-Pipeline output
   - KALIUM_OUTPUT_PATH: Ordner in den das Skript seinen Output schreibt
   - KALIUM_WORKING_PATH: Ordner für DuckDB um temporäre Datenbank-Dateien zu speichern. Nach dem Skriptdurchlauf werden diese Dateien wieder gelöscht.
   - KALIUM_CRTDL: Name der verwendeten CRTDL (Kalium360_CRTDL_basis oder Kalium360_CRTDL_complex)
   - KALIUM_SITE_PSEUDONYM: Ihr selbst gewähltes Pseudonym so wie es auch im Fragebogen angegeben ist
3. Output- und Working-Ordner anlegen, falls sie noch nicht existieren.
   Fehlende Ordner legt Docker mit Besitzer root an, dann fehlen dem Container
   die Schreibrechte.
4. Pipeline starten (das Image wird bei Bedarf automatisch von der GitHub
   Container Registry geladen):

   ```bash
   docker compose run --rm --user "$(id -u):$(id -g)" start_kalium
   ```

   Der Container läuft nicht als root, sondern mit der User- und Gruppen-ID
   des aufrufenden Benutzers. Dadurch darf er in Output- und Working-Ordner
   schreiben, und die Ergebnisdateien gehören diesem Benutzer. Unter Windows
   (PowerShell) gibt es `id` nicht: dort `--user ...` weglassen, der Container
   läuft dann mit der Standard-ID 1000.

Wer eine exakt bestimmte Version nutzen möchte, ruft das Repository am
zugehörigen Git-Tag auf (z. B. `git checkout v1.1.0`): Das `docker-compose.yml`
dieses Tags pinnt das Image auf genau diese Version (siehe Abschnitt „Releasen“).

Die Ergebnisse liegen anschließend im in `.env` hinterlegten Output-Ordner. Jeder Durchlauf erzeugt einen eigenen zeitgestempelten Unterordner. 

Das Skript erzeugt folgende Output-Dateien:

| Datei | Inhalt |
|---|---|
| `Kalium360_*Zeitstempel*.log` | Log-Datei, gehört zum Output; enthält wie die CSV-Dateien nur aggregierte und k-anonymisierte Daten |
| `statisticsPotassiumValues.csv` | Statistiken für jeden vorhandenen Kalium-LOINC, für die Gesamtkohorte sowie getrennt nach Altersgruppen und Geschlecht |
| `descriptionPotassiumObservations.csv` | Übersicht über die vorhandenen Daten, getrennt nach vorhandenen Kalium-LOINCs |
| `countPotassiumValuesPerMonth.csv` | Messwerte pro Monat |
| `distributionPotassiumValues.csv` | Counts der Werte pro Bin, für die gesamte Kohorte sowie getrennt nach LOINC, Geschlecht und Alter |
| `timelineCovariates.csv` | Counts der Covariates pro Monat |
| `potassiumNext6h.csv` | Counts zur Veränderung der Kaliumwerte innerhalb von 6 Stunden |
| `compareSerumBlood.csv` | Statistischer Vergleich eng beieinander gemessener Serum- und Blutwerte. Optional, wird nur bei entsprechend vorhandenen Daten geschrieben |
| `covariatesCounts.csv` | Counts zur Co-occurrence auffälliger Kaliumwerte und Covariaten, getrennt nach unterschiedlichen Gruppen |
| `covariatesRegression.csv` | Ergebnisse der linearen Regressionen zum Zusammenhang von Kaliumwerten und Covariaten |

Die Dateien enthalten alle lediglich k-Anonymisierte Daten. Sollte sich in den .csv Dateien eine Zeile auf weniger als 5 Patienten oder Messwerte (je nach Datei) beziehen, dann werden alle inhaltlichen Auswertungen (Mittelwerte, Percentillen etc) übersprungen. 
Der gesamte Output des Skripts soll an die Datenmanagmentstelle geschickt werden. 

### Alternative: starten ohne Docker

1. R (≥ 4.4) installieren und in diesem Ordner ein Terminal öffnen.
2. Benötigte Pakete installieren:

```bash
   Rscript -e 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-08-01")); install.packages(c("DBI", "duckdb", "glue", "dplyr", "purrr", "lubridate", "tibble", "biglm"))'
```

3. In `.env` die Pfade sowie `KALIUM_CRTDL` und `KALIUM_SITE_PSEUDONYM` anpassen (wie mit Docker, siehe oben). Die Werte in der `config.R` werden nur verwendet, wenn die Umgebungsvariablen nicht gesetzt sind.
4. Pipeline starten:

   ```bash
   Rscript Main.R
   ```

## Entwicklung: lokale Änderungen an Skripten und Konfiguration

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

### Releasen (für Betreuende)

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

## Laufzeit und Speicherbedarf

Zum Vergleich die Laufzeiten am Standort (~250.000 Patienten, 1,4 Mio. Kalium-Observations, ~18 Mio. weitere Ressourcen): DUP-Pipeline 8 Stunden, Skript 20 Minuten (32 GB RAM, Intel Core Ultra 7 265). Der TORCH-Output (.ndjson) umfasste ~50 GB, die finalen .csv-Dateien knapp 40 GB. Der Datenbank-Speicherbedarf blieb minimal.

## Analysebeschreibung

Eine Beschreibung der funktionsweise des Skripts sowie der durchgeführten Analysen findet sich hier: [Analysebeschreibung_Kalium360.md](Analysebeschreibung_Kalium360.md)

## Hinweis zur unit_conversion.csv

Im Skript findet für insgesamt vier der verwendeten Laborparameter bei Bedarf eine Umrechnung in die für dieses Projekt benötigten Einheiten statt. Die unit_conversion.csv dient dabei als Umrechnungstabelle. 
Zu Beginn prüft das Skript ob alle in den Daten vorhandenen Einheiten in dieser .csv vorkommen. Sollte das nicht so sein, bricht es ab mit dem Hinweis diese zu ergänzen. 

Der Aufbau der .csv ist wie folgt: 
| label  | source_unit | target_unit | factor |
| ------ | ----------- | ----------- | ------ |
| kalium | mmol/L      | mmol/L      | 1      |
| glucose| mmol/L      | mg/dL       | 18.02  |


Target_unit muss dabei pro label immer die vom Projekt vorgegebene sein. Factor ist der benötige Umrechnungsfaktor. Als Dezimaltrennzeichen wird hier ein `.` verwendet. Sollte bei Ihnen eine Einheit vorkommen, die sich nicht in die vorgegebene target_unit umrechnen lässt, besteht die Möglichkeit bei target_unit `invalid` einzutragen. Werte mit so markierten Einheiten werden bei den Statistiken für den dritten Teil ausgeschlossen. 

## Hinweis zu Observation.note

Das Skript sucht in Observation.note nach Hinweisen auf hämolytische Proben. Sollte dort etwas gefunden werden, stehen die entsprechenden Freitexte im Log. Diese Passage im Log ist markiert. Zusätzlich steht am Ende ein Hinweis, dass das Log Freitext enthält und vor dem Senden nochmal geprüft werden sollte. Sollte dies bei Ihnen der Fall sein, lesen Sie die entsprechenden Freitexte und zensieren Sie, falls vorhanden, datenschutzrechtlich problematische Stellen. Weitere output Dateien sind davon nicht betroffen.

## Hinweis zum Datenschutz

Die Output Dateien enthalten keinerlei IDs oder andere personenbeziehbare Daten. Zudem werden alle Daten sowohl im log wie auch in den .csvs k-Anonymisiert mit k = 5. Counts unter 5 werden durch <5 ersetzt (0 bleibt erhalten). Zeilen in den .csvs die sich aus zu kleinen Kohorten generieren würden, werden übersprungen.
