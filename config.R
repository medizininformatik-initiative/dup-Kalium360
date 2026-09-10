


# please enter the name of the used CRTDL. This generates a note to
# log but has no impact on the code.
used_CRTDL <- "please define"

# please enter the pseudonym you used in the project survey.
site_pseudonym <- "please define"

###############################################################################
###############################################################################

### IF YOU ARE USING THE OUTPUT OF THE DUP-PIPELINE THERE IS NO NEED TO CHANGE
### THE THINGS BELOW THIS LINE

###############################################################################
###############################################################################

# name of the .csv containing the data. Default are the names
# defined in the CRTDL
name_of_lab_csv <- "Laborwerte.csv"
name_of_patient_csv <- "Kalium Patienten.csv"
name_of_specimen_csv <- "Bioproben.csv"
name_of_encounter_csv <- "Fall.csv"
name_of_procedure_csv <- "Prozeduren.csv"
name_of_procedure_icu_csv <- "Prozeduren ICU.csv"
name_of_dauer_dialyse_csv <- "Haemodialyse.csv"
name_of_condition_csv <- "Conditions.csv"

# Medication-files
name_of_medadmCode_csv <- "Administration Codeableconcept.csv"
name_of_medadmMedCode_csv <- "Administration MedicationCode.csv"
name_of_medadmComplex_csv <- "Administration Complex.csv"
name_of_medication_csv <- "Medikation.csv"

# Used column names. Default are the TORCH-column-names
obs_loinc <- "Observation_code_coding_code"
obs_loinc_system <- "Observation_code_coding_system"
obs_value <- "Observation_value_X_Valuequantity_value"
obs_value_unit <- "Observation_value_X_Valuequantity_code"
obs_value_comp <- "Observation_value_X_Valuequantity_comparator"
obs_id <- "id"   # FHIR-ID
obs_reference_high <- "Observation_referenceRange_high_value"
obs_reference_low <- "Observation_referenceRange_low_value"
obs_reference_high_unit <- "Observation_referenceRange_high_code"
obs_reference_low_unit <- "Observation_referenceRange_low_code"
obs_time <- "Observation_effective_X_Effectivedatetime"
obs_issued <- "Observation_issued"
obs_patient <- "patient" # in the format: Patient/id
obs_status <- "Observation_status"
obs_interpretation <- "Observation_interpretation_coding_code"
obs_value_code <- "Observation_value_X_Valuecodeableconcept_coding_code"
obs_value_code_system <- "Observation_value_X_Valuecodeableconcept_coding_system"
obs_specimen <- "Observation_specimen_reference" # Specimen/
obs_start <- "Observation_effective_X_Effectiveperiod_start"
obs_end <- "Observation_effective_X_Effectiveperiod_end"
obs_encounter <- "Observation_encounter_reference"
obs_note <- "Observation_note_text"
obs_basedon <- "Observation_basedOn_reference"
obs_method_code <- "Observation_method_coding_code"
obs_method_system <- "Observation_method_coding_system"

pat_id <- "id"   # FHIR-ID
pat_gebdat <- "Patient_birthDate"
pat_gender <- "Patient_gender"

spec_id <- "id"
spec_patient <- "patient"
spec_type <- "Specimen_type_codingSct_code"
spec_cond <- "Specimen_condition_coding_code"
spec_cond_system <- "Specimen_condition_coding_system"

enc_id <- "id"
enc_patient <- "patient"
enc_class <- "Encounter_class_code"
enc_period_end <- "Encounter_period_end"
enc_period_start <- "Encounter_period_start"
enc_type <- "Encounter_type_coding_code"
#enc_fallnr <- "Encounter_identifierAufnahmenummer_value"
enc_status <- "Encounter_status"
enc_type_system <- "Encounter_type_coding_system"

adm_id <- "id"
adm_patient <- "patient"
adm_status <- "MedicationAdministration_status"
adm_period_end <- "MedicationAdministration_effective_X_Effectiveperiod_end"
adm_period_start <- "MedicationAdministration_effective_X_Effectiveperiod_start"
adm_effective <- "MedicationAdministration_effective_X_Effectivedatetime"
adm_enc <- "MedicationAdministration_context_reference"
adm_med_ref <- "MedicationAdministration_medication_X_Medicationreference_reference"
adm_med_atc <- "MedicationAdministration_medication_X_Medicationcodeableconcept_codingAtcclassde_code"

med_id <- "id"
med_atc <- "Medication_code_codingAtcclassde_code"
med_med_ref <- "Medication_ingredient_item_X_Itemreference_reference"

pro_id <- "id"
pro_patient <- "patient"
pro_status <- "Procedure_status"
#pro_doc_time <- "Procedure_extensionDokumentationsdatum_value_X_Valuedatetime"
pro_ops_code <- "Procedure_code_codingOps_code"
pro_sno_code <- "Procedure_code_codingSct_code"
pro_period_start <- "Procedure_performed_X_Performedperiod_start"
pro_period_end <- "Procedure_performed_X_Performedperiod_end"
pro_performed <- "Procedure_performed_X_Performeddatetime"
pro_encounter <- "Procedure_encounter_reference"

con_id <- "id"
con_patient <- "patient"
con_status <- "Condition_clinicalStatus_coding_code"
con_veri <- "Condition_verificationStatus_coding_code"
con_icd <- "Condition_code_codingIcd10gm_code"
con_recorded <- "Condition_recordedDate"
con_encounter <- "Condition_encounter_reference"



# Cut-off for k-anonymity.
k_value <- 5

# time-range for cohort inclusion
global_min_time <- as.POSIXct("2019-01-01 00:00:00", tz = "UTC")
global_max_time <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")


# default cut-offs
# low and high are default normal ranges in case there is no reference range
# low_ext and high_ext define the cut-off for clinical implausible values
# this cut-offs are (only) used on unit normalized values

kalium_ref <- list(low = 3.5, high = 5.0,
                   low_ext = 1.5, high_ext = 9.0)
glucose_ref <- list(low = 70, high = 110,
                    low_ext = 10, high_ext = 1500)
bicarbonat_ref <- list(low = 21, high = 26,
                       low_ext = 5, high_ext = 45)
crea_ref <- list(low = NA_real_, high = NA_real_,
                 low_ext = 0.1, high_ext = 40)
pH_ref <- list(low = 7.35, high = 7.45,
               low_ext = 6.8, high_ext = 7.8)
GFR_ref <- list(low = 90 , high = NA_real_,
                low_ext = 0, high_ext = 150)

# matching window (hours) for each lab-result.
lab_windows <- c(glucose = 6, bicarbonat = 6, pH = 6, crea = 48, GFR = 48)

# used CODE-Lists
LOINCs_Kalium <- c('12812-4', '12813-2', '2823-3', '29349-8',
                   '32713-0', '39789-3', '39790-1', '41656-0',
                   '51618-7', '6298-4', '75940-7', '77142-8')

LOINCs_Quality <- c('20392-7', '20393-5', '20394-3', '20395-0',
                   '46424-8', '46425-5', '46426-3')

LOINCs_bicarbonat <- c('14151-5','14627-4','19229-4','19230-2','19231-0',
                      '19232-8', '19233-6', '1959-6','1960-4', '1961-2',
                      '1963-8', '28640-1', '28641-9', '39459-3', '39460-1',
                      '48631-6', '54359-5', '69964-5', '97543-3','97544-1')

LOINCs_pH <- c('11558-4', '14873-4', '19213-8', '2744-1', '2745-8', '2746-6',
               '2753-2', '28646-8', '28647-6', '33913-5', '97536-7')

LOINCs_crea <- c('101475-2', '103616-9', '14682-9', '21232-4', '2160-0',
                 '38483-4', '59826-8', '77140-2')

LOINCs_GFR <- c('102097-3', '48642-3', '48643-1', '50044-7', '50210-4',
                '50384-7', '62238-1', '69405-9', '70969-1', '77147-7',
                '78006-4', '88293-6', '88294-4', '94677-2', '96591-3',
                '96592-1', '98979-8', '98980-6')

LOINCs_glucose <- c('100746-7', '104597-0', '104598-8', '104655-6', '14743-9',
                    '14749-6', '15074-8', '2339-0', '2340-8', '2341-6',
                    '2345-7', '32016-8', '39480-9', '39481-7', '41651-1',
                    '41652-9', '41653-7', '47995-6', '51596-5', '72516-8',
                    '74774-1', '77135-2')

LOINCs_kalium_serum <-('2823-3')
LOINCs_kalium_blood <-('6298-4')

OPS_codes <- c('8-853', '8-854', '8-855', '8-857')

# snomed codes that can be used to code hemodialysis
dialyse_snomed <- c('265764009', '341939001', '302497006', '233586004',
                    '233581009', '714749008', '233578004', '233575001',
                    '233588003', '233583007', '182750009', '233579007',
                    '233580005', '708932005', '427053002', '57274006',
                    '233577009', '233576000', '11932001', '698074000',
                    '233589006', '233590002', '708933000', '233587008',
                    '708930002', '233584001', '233585000', '715743002',
                    '233582002', '708934006')


atc_groups <- tribble(
  ~name, ~atc,
  "betabl", "C07",
  "digit", "C01AA",
  "aminos", "B05BB01",
  "osmodi", "B05BC",
  "suxame", "M03AB01",
  "ras", "C09",
  "nsar", "M01A",
  "nsar", "M01B",
  "calcin", "L04AD",
  "hepar", "B01AB",
  "diuretksp", "C03D",
  "diuretksp", "C03E",
  "diuret", "C03A",
  "diuret", "C03B",
  "diuret", "C03C",
  "diuret", "C03X",
  "trimet", "J01EA01",
  "pentam", "P01CX01",
  "kcliv", "B05XA01",
  "kalium", "A12BA",
  "pencg", "J01CE01",
  "blutpr", "B05AX",
  "insul", "A10A",
  "gluc", "B05BA11",
  "inhsym", "R03A",
  "cagluc", "B05XA19",
  "antaci", "A02AH",
  "mcort", "H02AA",
  "mcort", "H02BX20",
  "mcort", "H02BX21",
  "hyperk", "V03AE",
  "laxans", "A06",
  "syssym", "R03C"
)

icd_groups <- tribble(
  ~name,    ~icd,
  "ckd",    "N18",
  "aki",    "N17",
  "dka",    "E10.1",
  "dka",    "E11.1",
  "hypald", "E26.0",
  "hpoald", "E27.1",
  "dm1",    "E10",
  "dm2",    "E11",
  "metaz",  "E87.2",
  "respak", "E87.3",
  "hypomg", "E83.4",
  "hyperk", "E87.5",
  "hypok",  "E87.6",
  "neonk",  "P74.3",
  "leuk",   "C91",
  "leuk",   "C92",
  "thromb", "D47.3",
  "abnmin", "R79.0",
  "gastro", "A09",
  "alkblt", "R78.0",
  "hkpp",   "G72.3"
)

# notes search snippets.
notes_search_snippets <-c('hämol', 'haemol', 'hemol')

