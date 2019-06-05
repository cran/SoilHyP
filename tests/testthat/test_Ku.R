# test SWC function
test_that('SoilHyP', {
#
data(dataTestthat)
# --------------------------------------------
#  Unimodal van Genuchten
# --------------------------------------------
expect_equal(Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
                par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5),
                modality = 'uni', suc.negativ = FALSE),
             dataTestthat$ku.vgm.uni)
# --------------------------------------------
#  Bimodal van Genuchten
# --------------------------------------------
expect_equal(Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
                par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02,
                n = 1.5, tau = 0.5, w2 = 0.1, alfa2 = 0.1, n2 = 3),
                modality = 'bi', suc.negativ = FALSE),
             dataTestthat$ku.vgm.bi)
# --------------------------------------------
#  Unimodal PDI
# --------------------------------------------
expect_equal(Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
                par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001),
                suc.negativ = FALSE),
             dataTestthat$ku.pdi.uni)
# --------------------------------------------
#  bimodal PDI
# --------------------------------------------
expect_equal(Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
                par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001, w2 = 0.2, alfa2 = 1, n2 = 10),
                suc.negativ = FALSE),
             dataTestthat$ku.pdi.bi)

})


# dataTestthat <- list(th.vgm.uni = SWC(suc = seq(1, 1000, by = 1), par.shp = c(ths = 0.4, thr = 0, alfa = 0.02, n = 1.5),
#                                       FUN.shp = c('vG'), modality = 'uni', suc.negativ = FALSE),
#                      th.vgm.bi  = SWC(suc = seq(1, 1000, by = 1),
#                                       par.shp = c(ths = 0.4, thr = 0, alfa = 0.02, n = 2, w2 = 0.2, alfa2 = 1, n2 = 10),
#                                       FUN.shp  = c('vG'), modality = c('bi'), suc.negativ = FALSE),
#                      th.pdi.uni = SWC(suc = seq(1, 1000, by = 1), par.shp = list(ths = 0.4, thr = 0, alfa = 0.02, n = 1.5),
#                                       FUN.shp = c('pdi'), modality = c('uni'), suc.negativ = FALSE),
#                      th.pdi.bi  = SWC(suc = seq(1, 1000, by = 1),
#                                       par.shp = list(ths = 0.4, thr = 0, alfa = 0.02, n = 2, w2 = 0.2, alfa2 = 1, n2 = 10),
#                                       FUN.shp = c('pdi'), modality = c('bi'), suc.negativ = FALSE),
#                      ku.vgm.uni = Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
#                                      par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5),
#                                      modality = 'uni', suc.negativ = FALSE),
#                      ku.vgm.bi  = Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
#                                      par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02,
#                                                     n = 1.5, tau = 0.5, w2 = 0.1, alfa2 = 0.1, n2 = 3),
#                                      modality = 'bi', suc.negativ = FALSE),
#                      ku.pdi.uni = Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
#                                      par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001),
#                                      suc.negativ = FALSE),
#                      ku.pdi.bi = Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
#                                     par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001, w2 = 0.2, alfa2 = 1, n2 = 10),
#                                     suc.negativ = FALSE)
# )
#
#
# save(dataTestthat, file = 'data/dataTestthat.RData')
#rm(dataTestthat)


