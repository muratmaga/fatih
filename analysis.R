## HGS variables indicate to "Handgrip Strength" which was measured by electronic hand dynamometer in Kg-f
## DGT variables indicate to "2D:4D Digit Ratios" which were calculated from finger length measurements that are collected previously via digital caliper (0.05mm accuracy) in milimeters.

library(geomorph)
setwd("C:/Users/murat/Desktop/Fatih/")

semi.lms = c(45, 46, 47,
49, 50, 51,
36, 37, 38,
40, 41, 42,
33, 34,
30, 31,
63, 64, 65, 66, 67, 68, 69, 70,
53, 54, 55, 56, 57, 58, 59, 60)

fixed.lms = seq(1, 70, 1)[-semi.lms]

sliders =read.table("slider.nts", skip=2, header=F, sep=" ")
colnames(sliders) = c("before", "slide", "after")


left = c(52:60, 20, 21, 22, 26, 33, 27, 34, 35, 4, 15, 12, 11, 13, 14, 5, 44, 45, 46, 47, 48, 49, 50, 51)
right = c(62:70, 18, 17, 16, 24, 30, 23, 31, 32, 3, 8, 7, 6, 10, 9, 2, 39, 38, 37, 36, 43, 42, 41, 40)

maletps <- readland.tps("latestmale72.tps", specID="ID")
gpa_male <- gpagen(maletps, curves = sliders, PrinAxes = FALSE)
male.symm <- bilat.symmetry(gpa_male$coords, 
                            ind = unlist(dimnames(maletps)[3]),
                            replicate = rep(1, dim(maletps)[3]),
                            land.pairs = cbind(left, right),
                            object.sym = TRUE)


femaletps <- readland.tps("latestfemale55.tps", specID = "ID")
gpa_female <- gpagen(femaletps, curves = sliders, PrinAxes = FALSE)
female.symm <- bilat.symmetry(gpa_female$coords, 
                            ind = unlist(dimnames(femaletps)[3]),
                            replicate = rep(1, dim(femaletps)[3]),
                            land.pairs = cbind(left, right),
                            object.sym = TRUE)

#correct centroid sizes for the data

hist(gpa_female$Csize)
hist(gpa_male$Csize)

##generating variables

fem_hgs <- c(20.80,24.10,31.20,32.60,22.10,20.20,24.02,25.40,23.00,32.80,29.30,32.10,29.10,23.60,20.20,26.80,33.70,32.10,27.00,31.40,19.80,24.50,26.30,20.40,20.30,22.10,16.40,26.60,28.20,31.50,19.60,28.30,25.70,23.60,24.60,31.30,25.10,18.10,20.50,23.80,20.60,34.40,27.20,21.10,27.30,24.30,29.30,29.90,24.00,34.30,28.50,21.60,32.70,29.60,27.70)
fem_dgt <- c(0.993785960874569,0.945311942959002,1.004698144233028,1.003748070845888,1.043032061717036,1.050228310502283,0.969716728247482,0.990164171548420,0.945003819179224,1.007542768273717,0.957437375304811,1.026964505168799,0.977220956719818,0.971537926235212,0.974809216863007,0.999653763589779,0.986404416839199,0.955181490717650,1.005927985948478,0.943169811320755,1.058519191777949,1.034693269803455,1.040541623267366,0.996600763094000,1.015849235136414,0.967730669833066,0.994558330227358,1.016495764600981,0.977965864577504,1.002439850898001,1.041459876307551,0.959377640101380,0.981799425245008,0.994420147790680,0.982720939762327,0.975609756097561,1.062577132486388,1.001458316454671,0.983544685990338,0.994115536419089,1.024410457319370,1.011723085460599,0.987709648944116,0.990689655172414,0.992688375871641,1.033969804618117,1.073163089456368,0.964390209091558,1.022450823660249,0.975118966689327,0.997969756370765,1.041394825646794,1.014215870628056,1.019871507545196,0.998096493154697)

male_hgs <- c(39.30,40.50,47.80,55.50,51.80,52.60,51.60,45.70,51.80,41.80,55.60,50.20,44.30,39.90,53.80,54.00,61.20,37.30,54.00,48.20,52.30,37.40,40.30,62.20,64.60,65.00,50.30,48.80,39.30,50.30,50.40,50.30,45.20,33.40,48.10,43.60,36.60,44.00,35.70,41.90,57.00,41.70,35.60,39.20,59.90,57.30,41.60,58.00,38.10,43.80,54.90,35.10,44.60,51.40,45.30,64.50,46.50,49.50,46.70,39.60,49.10,33.30,37.90,42.60,42.60,49.10,44.00,49.60,47.60,37.30,35.00,39.90)
male_dgt <- c(0.970535714285714,0.997248280175109,0.995386845920654,1.020181392275813,1.004210526315789,1.002096152545811,0.966682998530132,0.975171173663462,0.952124554005839,1.020822862066490,0.943031536113937,0.983698296836983,0.971503235440518,1.011190508291762,0.968668915412558,0.947167956751444,0.936949547973170,1.003119151590767,1.028776978417266,0.975000000000000,1.021276595744681,1.028457974851092,0.962447001817080,1.030244530244530,1.000597014925373,1.004626569729015,0.902173913043478,0.998742138364780,0.968674698795181,0.963178294573643,0.971275885103540,0.981028151774786,0.970572569906791,0.959045876191077,0.966328725038402,0.983594286070738,1.001989347365719,1.007470288624788,0.959028139656071,1.043449733906831,1.020685080207454,1.016434153823680,1.010238907849829,0.988527594867966,0.975516449885233,0.929381155918974,1.005608772372070,0.992311477426498,1.025106057444885,0.991258278145695,1.019593849347953,0.997851851851852,0.972531406626310,0.980591145502981,1.023470937456855,0.994648003293536,1.003788349273382,0.982813902783122,0.980674128058303,0.996288548485053,0.967612164189282,0.927169515067335,0.999785023289144,1.014076289110933,0.966497394974609,0.967036430102105,0.960147556845670,1.000969994826695,1.042028615227389,0.979215116279070,0.994198238501328,1.025969405905372)


#create geomorph data from for linear models
male.gdf = geomorph.data.frame(Size = gpa_male$Csize, 
                               Coords = gpa_male$coords,
                               HGS = male_hgs, 
                               DGT = male_dgt)


#regression with centroid size

model1 <- procD.lm(Coords ~ Size + HGS + DGT, data = male.gdf )
#model1 <- procD.lm(male$coords.coords ~ cs.male + malehgs + maledgt)

summary(model1)


# generating a "random" variable

random <- c(2, 5, 7, 1, 3, 7, 4, 1, 5, 6, 1, 2, 11, 6, 8, 10, 13, 15, 1, 9, 7, 3, 5, 6, 12, 9, 3, 8, 5, 1, 4, 2, 9, 1, 2, 13, 16, 4, 5, 15, 11, 4, 3, 7, 6, 9, 2, 12, 3, 2, 3, 5, 2, 8, 9, 11, 5, 2, 9, 1, 4, 2, 13, 15, 3, 6, 13, 1, 2, 9, 2, 1)

# Regression with male Handgrip Strength variable

fithgsmale <- procD.lm(gpa_male$coords ~ male_hgs)

summary(fithgsmale)

# Significant results with 0.041 p vaule
# Now creating Regression Scores

RS_MaleHGS <- plot(fithgsmale, type = "regression", reg.type = "RegScore", predictor = male_hgs)

# Repeating regression with RScores as dependent variable

fithgsmale_rs <- procD.lm(RS_MaleHGS$RegScore ~ male_hgs)

summary(fithgsmale_rs)

# Significant results with 0.001 p value
# Now using my random generated variable as independent for the same process

fitrandommale <- procD.lm(gpa_male$coords ~ random)

summary(fitrandommale)

# Insignificant results with 0.808 p vaule
# Now continue with RegScores

RS_MaleRandom <- plot(fitrandommale, type = "regression", reg.type = "RegScore", predictor = random)

fitrandommale_rs <- procD.lm(RS_MaleRandom$RegScore ~ random)

summary(fitrandommale_rs)

# Significant Results With 0.024 p value
