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

Die benötigten Daten werden von dem lokalen FHIR-Server mit Hilfe der DUP-Pipeline ausgeleitet. Benötigt werden dabei die Schritte TORCH und FHIR-flattener. Der DIMP Schritt ist optional und für dieses Projekt (verteilte Analyse) nicht erforderlich, kann bei Bedarf aber zusätzlich ausgeführt werden. 

### CRTDL Varianten

Diesem Projekt liegen zwei CRTDLs bei. 

- Kalium360_CRTDL_basis: Diese CRTDL funktioniert mit jedem FHIR-Server. Allerdings können MedicationAdministrations mit einer zweifachen Referenz nicht gefunden und ausgeleitet werden. Gemeint sind MedicationAdministrations die auf eine Medication verweisen die wiederrum auf eine weitere Medication verweisen. 

- Kalium360_CRTDL_complex: Diese CRTDL kann auch MedicationAdministrations mit zweifacher Referenz berücksichtigen. Allerdings wird dafür ein Blaze ab Version 1.8 benötigt.

Beide CRTDLs sind für das Projekt geeignet. Wenn die complexe CRTDL an Ihrem Standort ausgeführt werden kann, verwenden Sie bitte diese, ansonsten die basis Version. 

Hinweis: beide CRTDLs enthalten händisch hinzugefügte Filter für Medikation. Beim Hochladen in das FDPG Portal werden diese Filter ohne Fehlermeldung entfernt. Bitte nutzen Sie die CRTDLs so wie sie hier bereitgestellt werden. 

### Output
Je nachdem welche Daten an ihrem Standort vorliegen, können nach Durchlaufen der DUP-pipeline bis zu 13 verschiedene .csv Dateien entstehen: 
Laborwerte.csv, Kalium Patienten.csv, Bioproben.csv, Fall.csv, Prozeduren.csv, Prozeduren ICU.csv, Haemodialyse.csv, Conditions.csv, Administration Codeableconcept.csv, Administration MedicationCode.csv, Administration Complex.csv, Medikation.csv, ServiceRequest.csv

Lediglich Laborwerte.csv und Kalium Patienten.csv sind Pflicht. Alle anderen Dateien sind optional und werden in vielen Fällen nicht vorliegen. Die ServiceRequest.csv wird nicht benötigt. Sie enthält keine Attribute und wird aus technischen Gründen ausgeleitet um eine evt. vorhandene BasedOn-Referenz in den Observations zu erkennen.  

## Schritt 2: Verteilte Analyse Skript

Da Kalium ein häufig gemessener Laborparameter ist, erwarten wir an einigen Standorten größere Datenmengen. 
Das Skript setzt auf ein datenbanklastiges Design mit einer DuckDB im Hintergrund. Auf diese Weise können auch Datensätze mit mehreren Millionen Observations performant verarbeitet werden. 

## Starten mit Docker

Voraussetzung: Docker ist installiert.

1. Repository klonen und in den Ordner wechseln.
2. In `.env` folgende Pfade definieren: 
   - KALIUM_DATA_PATH: Input Ordner mit dem DUP-Pipeline output
   - KALIUM_OUTPUT_PATH: Ordner in den das Skript seinen Output schreibt
   - KALIUM_WORKING_PATH: Ordner für DuckDB um temporäre Datenbank-Dateien zu speichern. Nach dem Skriptdurchlauf werden diese Dateien wieder gelöscht.
3. In der config.R folgendes eintragen:
   - used_CRTDL: Name der verwendeten CRTDL 
   - site_pseudonym: Ihr selbst gewähltes Pseudonym so wie es auch im Fragebogen angegeben ist

4. Image einmalig bauen:

   ```bash
   docker compose build
   ```

5. Pipeline starten:

   ```bash
   docker compose run --rm start_kalium
   ```

6. Die Ergebnisse liegen anschließend im in `.env` hinterlegten Output-Ordner. Jeder Durchlauf erzeugt einen eigenen zeitgestempelten Unterordner. 
Es werden folgende Output Dateien erzeugt:
   - Kalium360_*Zeitstempel*.log: Das log gehört mit zum output und enthält ebenso wie die .csv Dateien nur aggregierte und k-anonymisierte Daten.
   - statisticsPotassiumValues.csv: enthält für jeden vorhandenen Kalium LOINC Statistiken sowohl für die Gesamtkohorte wie auch getrennt nach Altersgruppen und Geschlecht.
   - descriptionPotassiumObservations.csv: eine Übersicht über die vorhandenen Daten getrennt nach vorhandenen Kalium LOINCs.
   - countPotassiumValuesPerMonth.csv: Messwerte pro Monat
   - distributionPotassiumValues.csv: Counts der Werte pro bin. Sowohl für die gesamte Kohorte wie auch getrennt nach LOINC, Geschlecht und Alter. 
   - timelineCovariates.csv: Counts der Covariates pro Monat
   - compareSerumBlood.csv: statistischer Vergleich eng beeinander gemessener Serum und Blut Werte. Diese Datei ist optional und wird nur bei entsprechend vorhandenen Daten geschrieben. 
   - covariatesCounts.csv: Counts zu der Co-occurrence auffälliger Kaliumwerte und den Covariaten.
   - covariatesRegression: Ergebnisse der linearen Regressionen zu dem Zusammenhang von Kaliumwerten und den Covariaten.


## Alternative: starten ohne Docker

1. R (≥ 4.4) installieren und in diesem Ordner ein Terminal öffnen.
2. Benötigte Pakete installieren:

   ```r
   install.packages(c("DBI", "duckdb", "glue", "dplyr", "purrr",
                       "lubridate", "tibble", "biglm"))
   ```

3. In `.env` die Pfade anpassen sowie die Informationen in der .config angeben (wie mit Docker, siehe oben).
4. Pipeline starten:

   ```bash
   Rscript Main.R
   ```

## Hinweis zur unit_conversion.csv

Im dritten Teil des Skript findet für insgesamt vier der verwendeten Laborparameter bei Bedarf eine Umrechnung in die für dieses Projekt benötigten Einheiten statt. Die unit_conversion.csv dient dabei als Umrechnungstabelle. 
Zu Beginn prüft das Skript ob alle in den Daten vorhandenen Einheiten in dieser .csv vorkommen. Sollte das nicht so sein, bricht es ab mit dem Hinweis diese zu ergänzen. 

Der Aufbau der .csv ist wie folgt: 
| label  | source_unit | target_unit | factor |
| ------ | ----------- | ----------- | ------ |
| kalium | mmol/L      | mmol/L      | 1      |
| glucose| mmol/L      | mg/dL       | 18.02  |


Target_unit muss dabei pro label immer die vom Projekt vorgegebene sein. Factor ist der benötige Umrechnungsfaktor. Als Dezimaltrennzeichen wird hier ein `.` verwendet. Sollte bei Ihnen eine Einheit vorkommen, die sich nicht in die vorgegebene target_unit umrechnen lässt, besteht die Möglichkeit bei target_unit `invalid` einzutragen. Werte mit so markierten Einheiten werden bei den Statistiken für den dritten Teil ausgeschlossen. 

## Hinweis zu Observation.note

Das Skript sucht in Observation.note nach Hinweisen auf hämolytische Proben. Sollte dort etwas gefunden werden, stehen die entsprechenden Freitexte im Log. Diese Passage im Log ist markiert. Zusätzlich steht am Ende ein Hinweis, dass das Log Freitext enthält und vor dem Senden nochmal geprüft werden sollte. Sollte dies bei Ihnen der Fall sein, lesen Sie die entsprechenden Freitexte und zensieren Sie, falls vorhanden, datenschutzrechtlich problematische Stellen. Weitere output Dateien sind davon nicht betroffen.