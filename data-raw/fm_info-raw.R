## code to prepare `map-raw` dataset goes here

{
  # criando uma matrix circular
  fm <- base::matrix(c(0, 0, 1, 1, 1, 0, 0,
                       0, 1, 1, 1, 1, 1, 0,
                       1, 1, 1, 1, 1, 1, 1,
                       1, 1, 1, 1, 1, 1, 1,
                       1, 1, 1, 1, 1, 1, 1,
                       0, 1, 1, 1, 1, 1, 0,
                       0, 0, 1, 1, 1, 0, 0),
                      ncol = 7, nrow = 7)

  ### salvando os dados no pacote
  usethis::use_data(fm, overwrite = TRUE)


 ###################################################

 ## criando um df com as informações dos códigos dos níveis (lvl_cod),
 #  o nome de cada nível (habitat),
 #  o peso dado para cada nível (q_peso)
 info <- base::data.frame(lvl_cod = c(1:9),
                           habitat = c("Floresta", "Natural n-florestal", "Pastagem",
                                       "Agricultura", "Silvicultura", "N-vegetada",
                                       "Urbano", "Mineiracao", "Agua"),
                            q_peso = c(9:1))

 ### salvando os dados no pacote
 usethis::use_data(info, overwrite = TRUE)

  }
