# Extracting and storing assay metadata
Analyte <- metadata$analyte
analyte <- tolower(Analyte)
analyte_abr <- metadata$analyte_abbr
wl <- metadata$wavelength
molar_coef <- metadata$epsilon
final_vol <- metadata$final_vol
sample_vol <- metadata$sample_vol

if (protocol_type == 'std_curve') {
  std_name <- metadata$std_name
  std_abbr <- metadata$std_abbr
  std_min_stock <- metadata$std_min_stock
  std_max_stock <- metadata$std_max_stock
  std_min_final <- metadata$std_min_final
  std_max_final <- metadata$std_max_final
  std_unit <- metadata$std_unit
}