context("pu_")


# setup -------------------------------------------------------------------


#lots of things to test
v_argentinian_provinces = c("Buenos Aires", "Buenos Aires City (DC)", "Catamarca", "Chaco",
                            "Chubut", "Córdoba", "Corrientes", "Entre Ríos", "Formosa", "Jujuy",
                            "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro",
                            "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
                            "Tierra del Fuego", "Tucumán")
v_argentina_abbrev = structure(c("ARG_B", "ARG_C", "ARG_K", "ARG_H", "ARG_U", "ARG_X",
                                 "ARG_W", "ARG_E", "ARG_P", "ARG_Y", "ARG_L", "ARG_F", "ARG_M",
                                 "ARG_N", "ARG_Q", "ARG_R", "ARG_A", "ARG_J", "ARG_D", "ARG_Z",
                                 "ARG_S", "ARG_G", "ARG_V", "ARG_T"), .Names = c("Buenos Aires",
                                                                                 "Buenos Aires City (DC)", "Catamarca", "Chaco", "Chubut", "Córdoba",
                                                                                 "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
                                                                                 "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
                                                                                 "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
                                                                                 "Tierra del Fuego", "Tucumán"))
#translate back names
v_abbrevs = c("DNK", "THA", "USA", "GBR", "SWE", "NOR", "DEU", "VEN", "TUR", "NLD")
v_abbrevs_reversed = c("Denmark", "Thailand", "USA", "United Kingdom", "Sweden", "Norway", "Germany", "Venezuela", "Turkey", "Netherlands")

v_odd_names = c("Danmark", "Viet Nam", "Belgia", "Bolivia, Plurinational State of")
v_nonodd_names = c("Denmark", "Vietnam", "Belgium", "Bolivia")

v_notquiteright = c("USa", "Dnmark", "Viitnam", "Bolgium", "Boliviam")
v_quiteright = c("USA", "Denmark", "Vietnam", "Belgium", "Bolivia")


# tests -------------------------------------------------------------------


test_that("pu_translate", {
  #test argentina results
  expect_true(all(pu_translate(v_argentinian_provinces, superunit = "ARG", messages = 0) == v_argentina_abbrev))

  #reverse
  expect_true(all(pu_translate(v_abbrevs, messages = 0, reverse = T) == v_abbrevs_reversed))

  #italian subunits
  expect_true(all(c("ITA-VEN", "ITA-VEN") == pu_translate(c("veneto", "VENeto"), superunit = "ITA", ad_level = 1, messages = 0)))
  expect_true(all(c("ITA-VE", "ITA-VE") == pu_translate(c("veneziia", "venetia"), superunit = "ITA", ad_level = 2, messages = 0)))

  #standardize odd names by exact match
  expect_true(all(pu_translate(v_odd_names, messages = 0, standardize_name = T) == v_nonodd_names))

  #fuzzy match
  expect_true(all(pu_translate(v_notquiteright, messages = 0) == c("USA", "DNK", "VNM", "BEL", "BOL")))
  expect_true(all(pu_translate(v_notquiteright, messages = 0, standardize_name = T) == v_quiteright))

  #messages when fuzzy
  expect_message(pu_translate(rep("Dunmark", 5)), regexp = "Dunmark -> Denmark with distance 1.00")

  #repeated NAs and countries
  expect_equivalent(pu_translate(c(rep("Denmark", 3), rep(NA, 3), rep("Sweden", 3), "Denmark")), c(rep("DNK", 3), rep(NA, 3), rep("SWE", 3), "DNK"))
})
