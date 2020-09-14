edos = c("Aguascalientes", "BC", "BCS", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "DF", "Durango", "Guanajuato","Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit", "NL", "Oaxaca", "Puebla", "Queretaro", "QR", "SLP", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
total = c(83998, 277488, 50570, 33829, 168029, 34643, 46064, 216184, 506151, 38483 ,167031, 125866, 71329, 347665, 806060, 150059, 111856, 51823, 265851, 73498, 131447, 93094, 87318, 38268, 201277, 177474, 14879, 197048, 25123, 258036, 58372, 45925)
domestica = c(78264, 142708, 38106, 21534, 135057, 28940, 36373, 110974, 363375, 15667, 130420, 109069, 54296, 274249, 681767, 122457, 97004, 44562, 223210, 60862, 111865, 74282, 51299, 22605, 159286, 136117, 8006, 158018, 21501, 208068, 52444, 38358)
comercial = c(3413, 8078, 4877, 5145, 20341, 3754, 6103, 20813, 88205, 5783, 22601, 11038, 5927, 42931, 87419, 20655, 9345, 6615, 42075, 7240, 9987, 8422, 16826, 10193, 14549, 31462, 3820, 9857, 1896, 42298, 2210, 2627)
industrial = c(512, 12824, 6405, 992, 8416, 1541, 1495, 67288, 32721, 15925, 6764, 2621, 2813, 13086, 12005, 4562, 2711, 559, 105, 2164, 6198, 1823, 8940, 2115, 7205, 9222, 1255, 21592, 358, 3222, 85, 2685)
servicios = c(1629, 113758, 789, 102, 3548, 405, 1990, 9310, 21340, 1019, 5573, 2854, 2672, 15940, 818, 1652, 1953, 69, 461, 2894, 2493, 8075, 8229, 1937, 20121, 319, 1660, 6411, 121, 4268, 3389, 1664)
pipa = c(180, 120, 393, 6056, 667, 3, 103, 7799, 510, 89, 1673, 284, 5621, 1459, 24051, 733, 843, 18, 0, 338, 904, 492, 2024, 1418, 116, 354, 138, 1170, 1247, 180, 244, 591)

png('t1.png', width = 3000, height = 2500, res = 300)
boxplot(total, domestica, comercial, industrial, servicios, pipa, names= column_names, col=(c("coral1", "cyan3", "deepskyblue2", "darkgoldenrod2", "darkgreen", "deeppink3")))
dev.off() 

png('prueba1.png', width = 2100, height = 2600, res = 300)
par(mfrow=c(2,3))
boxplot(total, main = "Total", col = "coral1")
boxplot(domestica, main = "Domestic", col = "cyan3")
boxplot(comercial, main = "Comercial", col = "deepskyblue2")
boxplot(industrial, main = "Industrial", col = "darkgoldenrod2")
boxplot(servicios, main = "Public services", col = "darkgreen")
boxplot(pipa, main = "Pipe", col = "deeppink3")
#boxplot(comercial, industrial,servicios, pipa, names=c("Comercial","Industrial", "Servicios", "Pipa"))
dev.off()

m_total = which.max(total)
m_domestica = which.max(domestica)
m_comercial = which.max(comercial)
m_industrial = which.max(industrial)
m_servicios = which.max(servicios)
m_pipa = which.max(pipa)


column_names=c("Total", "Domestic","Comercial","Industrial", "Public Services", "Pipe")
colors_data = c("coral1", "cyan3", "deepskyblue2", "darkgoldenrod2", "darkgreen", "deeppink3")
png('histo.png', width = 2100, height = 2600, res = 300)
edos_names = c(edos[m_total], edos[m_domestica], edos[m_comercial], edos[m_industrial], edos[m_servicios], edos[m_pipa])
maximos = c(max(total), max(domestica), max(comercial), max(industrial), max(servicios), max(pipa) )
barplot(maximos, names = edos_names, col=colors_data)
legend("topright", legend = column_names, fill = colors_data)
dev.off()

column_names2=c("Incoming water","Total")
colors_data2 = c("darkseagreen3","coral1")
png('histo2.png', width = 2100, height = 2600, res = 300)
edos_names2 = c("Incoming water", "Total")
maximos2 = c(max(water_ncome), max(total))
barplot(maximos2, names = edos_names2, col=colors_data2)
legend("topright", legend = column_names2, fill = colors_data2)
dev.off()

edos[m_total]
edos[m_domestica]
edos[m_comercial]
edos[m_industrial]
edos[m_servicios]
edos[m_pipa]


water_ncome = c(131822, 274954, 112563, 91494, 336211, 110966, 286022, 402681, 686759, 182543, 402165, 363477, 281276, 471436, 1083233, 483935, 599368, 111682, 454882, 355632, 432398, 270532, 192432, 248427, 320085, 394566, 256308, 410445, 119264, 692245, 317557, 126705)
png('water_income.png', width = 2100, height = 3100, res = 300)
boxplot(water_ncome, main = "Income water", col = "darkseagreen3")
dev.off() 

min(total)
mean(total)