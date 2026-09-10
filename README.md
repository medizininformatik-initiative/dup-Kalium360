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

Das verteilte Analyse Skript ist noch nicht final. Die aktuelle Version zum testen findet sich im develop branch.
