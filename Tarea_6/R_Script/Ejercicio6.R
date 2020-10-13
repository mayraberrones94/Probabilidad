edos = c("Aguascalientes", "BC", "BCS", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "DF", "Durango", "Guanajuato","Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit", "NL", "Oaxaca", "Puebla", "Queretaro", "QR", "SLP", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")
total = c(83998, 277488, 50570, 33829, 168029, 34643, 46064, 216184, 506151, 38483 ,167031, 125866, 71329, 347665, 806060, 150059, 111856, 51823, 265851, 73498, 131447, 93094, 87318, 38268, 201277, 177474, 14879, 197048, 25123, 258036, 58372, 45925)
domestica = c(78264, 142708, 38106, 21534, 135057, 28940, 36373, 110974, 363375, 15667, 130420, 109069, 54296, 274249, 681767, 122457, 97004, 44562, 223210, 60862, 111865, 74282, 51299, 22605, 159286, 136117, 8006, 158018, 21501, 208068, 52444, 38358)
comercial = c(3413, 8078, 4877, 5145, 20341, 3754, 6103, 20813, 88205, 5783, 22601, 11038, 5927, 42931, 87419, 20655, 9345, 6615, 42075, 7240, 9987, 8422, 16826, 10193, 14549, 31462, 3820, 9857, 1896, 42298, 2210, 2627)
industrial = c(512, 12824, 6405, 992, 8416, 1541, 1495, 67288, 32721, 15925, 6764, 2621, 2813, 13086, 12005, 4562, 2711, 559, 105, 2164, 6198, 1823, 8940, 2115, 7205, 9222, 1255, 21592, 358, 3222, 85, 2685)
servicios = c(1629, 113758, 789, 102, 3548, 405, 1990, 9310, 21340, 1019, 5573, 2854, 2672, 15940, 818, 1652, 1953, 69, 461, 2894, 2493, 8075, 8229, 1937, 20121, 319, 1660, 6411, 121, 4268, 3389, 1664)
pipa = c(180, 120, 393, 6056, 667, 3, 103, 7799, 510, 89, 1673, 284, 5621, 1459, 24051, 733, 843, 18, 0, 338, 904, 492, 2024, 1418, 116, 354, 138, 1170, 1247, 180, 244, 591)

poblacion =  c(233.7, 46.4, 9.6, 15.6, 19.5, 126.4, 71.2, 14.4, 5967.3, 14.2, 191.3, 55.6, 137.3, 99.8, 724.2, 78.2, 390.2, 42.4, 79.8, 42.3, 179.8, 174.4, 33.6, 44.5, 51.7, 15.9, 96.9, 42.9, 318.4, 113, 53.1, 21)
malo = c(13.33263336, 20.94912467, 17.33160934, 16.973991, 17.38500619, 10.83467732, 8.916364626, 19.61259903, 10.84161621, 14.87235255, 14.83938977, 17.9278914, 10.48780862, 5.180165682, 4.168830823, 12.29467705, 13.89916212, 22.33115698, 17.89841157, 16.75704668, 18.95362678, 12.12067549, 9.996090013, 21.27690443)
regular = c(36.1223806, 32.36231792, 30.77031746, 27.14450176, 23.39351066, 17.9311899, 15.49453566, 19.57272459, 33.35972943, 29.36442499, 35.3178316, 30.7924066, 19.9753758, 12.54575151, 11.89843583, 21.61660139, 36.75068673, 33.04412477, 29.73608228, 26.31486355, 24.17089259, 19.15599309, 16.31239193, 19.10788837)
bueno = c(49.7568826, 45.83327329, 51.48095038, 55.48328246, 45.25348616, 35.7568475, 47.0119972, 47.72185825, 55.4200321, 54.97335023, 49.32022418, 50.672439, 50.44000641, 25.59460433, 46.77597001, 52.3100491, 48.46891993, 43.75455757, 51.97236151, 56.57740627, 44.07392266, 38.06803314, 47.06567655, 46.67837203)
###############################################

#Experimentos tarea 6

################################################
set.seed(100)

png("Ej6_malo.png", width = 1000, height = 2000, res = 300)
hist(malo, xlab = "", main = " ")
dev.off()
png("Ej6_regular.png", width = 1000, height = 2000, res = 300)
hist(regular, xlab = "", main = " ")
dev.off()
png("Ej6_bueno.png", width = 1000, height = 2000, res = 300)
hist(bueno, xlab = "", main = " ")
dev.off()

t.test(malo, mu = 15)
t.test(regular, mu = 25)
t.test(bueno, mu = 45)

wilcox.test(malo, mu=15, conf.int = TRUE)
wilcox.test(regular, mu=25, conf.int = TRUE)
wilcox.test(bueno, mu=45, conf.int = TRUE)

wilcox.test(malo, bueno, paired = TRUE)
wilcox.test(regular, bueno, paired = TRUE)
wilcox.test(malo, regular, paired = TRUE)

shapiro.test(malo)
shapiro.test(regular)
shapiro.test(bueno)

ks.test(malo, bueno)
ks.test(regular, bueno)
ks.test(malo, regular)

var.test(malo, bueno)
var.test(regular, bueno)
var.test(malo, regular)

chisq.test(table(malo, bueno), correct = FALSE)  # Yates continuity correction not applied
#or
summary(table(malo, bueno)) #

cor.test(malo, bueno)
cor.test(regular, bueno)
cor.test(malo, regular)


png('t1.png', width = 3000, height = 2500, res = 300)
boxplot(total, domestica, comercial, industrial, servicios, pipa, names= column_names, col=(c("coral1", "cyan3", "deepskyblue2", "darkgoldenrod2", "darkgreen", "deeppink3")))
dev.off() 