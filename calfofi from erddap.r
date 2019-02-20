setwd("R:/NOAA projects/all ichthyoplankton data")


library(ggplot2)
library(reshape2)
library(ggmap)
library(plyr)

## pull in data taken from erddap
## this file came from the link: CalCOFI Larvae Counts, Scientific Names SB to SC
# see: https://coastwatch.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=CalCOFI+Larvae+Counts
raw1 <- read.csv("erdCalCOFIlrvcntSBtoSC_5e28_d9ef_0ecc.csv")
raw2 <- raw1[-1,]
head(raw2)
####
raw2$line.station <- with(raw2, paste(line, station))

### select just the core stations
raw3 <- subset(raw2, line.station %in% c("76.7 49", "76.7 51", "76.7 55", "76.7 60","76.7 70","76.7 80","76.7 90", "76.7 100","80 51", 
											"80 55","80 60","80 70","80 80", "80 90","80 100",
											"81.8 46.9","83.3 40.6", "83.3 42", "83.3 51", "83.3 55", "83.3 60","83.3 70","83.3 80", "83.3 90", "83.3 100","83.3 110", 
							"86.7 33", "86.7 35", "86.7 40", "86.7 45", "86.7 50", "86.7 55","86.7 60","86.7 70", "86.7 80", "86.7 90", "86.7 100", "86.7 110",
							"90 28", "90 30", "90 35","90 37", "90 45", "90 53", "90 60", "90 70", "90 80", "90 90", "90 100", "90 110", "90 120",
							"93.3 26.7", "93.3 28", "93.3 30", "93.3 35", "93.3 40","93.3 45", "93.3 50","93.3 60","93.3 70", "93.3 80", "93.3 90", "93.3 100","93.3 110","93.3 120"))

### select only starboard samples from the bongo nets
raw4 <- subset(raw3, net_location == "starboard")

## select cruises so that there is only 1 cruise per season with mostly complete covereage of the sample frame
raw4$cruise_shipcode <- with(raw4, paste(cruise, ship_code))
head(raw4)

raw5 <- subset(raw4,cruise_shipcode %in% c("201504 NH","201404 OS","201402 SH","201304 SH","201307 NH","201301 SH",
"201210 NH","201203 SH","201207 OS","201202 NH","201110 NH","201104 SH","201108 NH","201101 NH","201011 NH","201004 MF",
"201008 NH","201001 NH","200911 NH","200903 JD","200907 M2","200901 NH","200810 NH","200804 JD","200808 NH","200801 JD",
"200711 NH","200704 JD","200707 NH","200701 JD","200610 RR","200604 NH","200607 NH","200602 JD","200511 NH","200504 NH",
"200507 NH","200501 NH","200411 RR","200404 NH","200407 JD","200401 JD","200310 NH","200304 RR","200307 NH","200302 JD",
"200211 NH","200204 JD","200207 NH","200201 JD","200110 NH","200104 JD","200107 NH","200101 JD","200010 NH","200004 JD",
"200007 NH","200001 NH","199910 NH","199904 JD","199908 NH","199901 RR","199809 NH","199804 JD","199807 NH","199802 JD",
"199709 NH","199704 NH","199707 JD","199702 JD","199610 RR","199604 JD","199608 NH","199602 JD","199510 NH","199504 NH",
"199507 JD","199501 JD","199410 NH","199403 JD","199408 NH","199401 JD","199310 NH","199304 JD","199308 NH","199301 JD",
"199210 NH","199204 JD","199207 JD","199202 JD","199110 NH","199103 JD","199108 JD","199101 JD","199011 NH","199003 JD",
"199008 NH","198911 NH","198904 JD","198907 NH","198901 JD","198810 NH","198805 JD","198808 NH","198801 JD","198711 NH",
"198705 JD","198709 NH","198703 JD","198611 NH","198605 JD","198609 NH","198602 JD","198511 NH","198505 JD","198508 NH",
"198502 M4","198410 JD","198410 NH","198404 JD","198404 NH","198407 JD","198407 NH","198401 JD","198401 NH","198303 EK",
"198104 JD","198107 JD","198101 JD","197803 JD","197807 JD","197801 JD","197511 JD","197505 JD","197507 JD","197501 AX",
"197501 JD","197207 AL","197207 AX","197207 JD","197202 AX","197202 AB","197202 AL","196910 JD","196904 JD","196907 JD",
"196901 JD","196806 JD","196801 HO","196706 EB","196610 JD","196604 AL","196607 JD","196602 AX","196509 BD","196504 AX",
"196507 AX","196507 BD","196501 AX","196501 BD","196410 BD","196404 AX","196407 AX","196407 BD","196401 AX","196401 BD",
"196310 BD","196304 AX","196304 BD","196307 BD","196301 AX","196301 BD","196210 AX","196210 BD","196203 HO","196203 BD",
"196207 BD","196201 HO","196201 BD","196110 HO","196110 BD","196104 HO","196104 BD","196107 HO","196107 BD","196101 HO",
"196101 BD","196010 BD","196010 SB","196004 HO","196004 HS","196007 HO","196007 BD","196002 BD","195910 SB","195910 OR",
"195904 OR","195907 OR","195907 PT","195901 BD","195901 OR","195810 ST","195804 SB","195804 BD","195804 PT","195807 PT",
"195807 OR","195710 PT","195704 HO","195707 ST","195707 PT","195604 ST","195607 OR","195607 PT","195504 CR","195507 CR",
"195507 PT","195404 HO","195404 ES","195406 HO","195304 HO","195304 PT","195304 CR","195306 CR","195306 ES","195306 YE",
"195306 PT","195204 HO","195204 CR","195206 SB","195206 BD","195206 YE","195104 BD","195104 CR","195106 BD","195106 YE",
"195101 47","195101 CR"))

### get year and months for each cruise
head(raw5)
year_month <- data.frame(transform(raw5$cruise, Year = substr(raw5$cruise, 1, 4), Month = substr(raw5$cruise, 5, 6)))
head(year_month)
raw6 <- data.frame(raw5, year_month[,c(2,3)])
head(raw6)

### select the columns that you need for the next analysis
data.frame(colnames(raw6))
raw7 <- raw6[,c(1,3,4,6,8,9,10,11,15,16,17,18,12,13)]
head(raw7)
# substitude a period for a space in the species names
raw7$scientific_name <- gsub(" ", "\\.", raw7$scientific_name)
unique(raw7$larvae_10m2)

### for some reason it is seeing larvae_10m2 as a factor.  save it as a csv and pull it back in so that it sees it as a number
write.csv(raw7, "raw7.csv")
raw8 <- read.csv("raw7.csv")[,-1]
str(raw8)

## transform data frame from long to wide format
data1 <- dcast(raw8, cruise + ship_code + order_occupied + net_type + latitude + longitude + station + line.station + cruise_shipcode + Year + Month ~ scientific_name, sum)
head(data1)

## in 1978 some stations were samplee twice with a ring and bongo net.  Remove the ring net from 1978 samples
data1$code <- with(data1, paste(cruise_shipcode, line.station, net_type))

data2 <- subset(data1, !code %in% c("197801 JD 76.7 60 C1","197801 JD 80 60 C1","197801 JD 80 90 C1","197801 JD 83.3 60 C1","197801 JD 86.7 60 C1",
										"197801 JD 90 30 C1","197801 JD 90 60 C1","197801 JD 90 90 C1","197801 JD 93.3 60 C1", "197807 JD 80 60 C1",
										"197807 JD 80 90 C1","197807 JD 86.7 60 C1","197807 JD 90 30 C1","197807 JD 90 60 C1","197807 JD 90 90 C1","197807 JD 93.3 60 C1"))
										
head(data2)										
										
##### Samples from 1965 to the present have been identified to current species standards.  Prior to that some were identified only to genus.  To be able to compare
#### samples pre and post 1965 it is necessary to group some species into coarser categories
										
# pull out squid and incomplete species
#Combine species into family groups, delete incomplete taxa
## squid and lobster (palinuridae)
data2 <- data2[setdiff(names(data2),
                       c('Abraliopsis', 'Abraliopsis.felis','Argonauta','Berryteuthis','Chiroteuthis.calyx', 'Chiroteuthis.spp.','Chtenopteryx.sicula','Cranchia.scabra','Cranchiidae',
                         'Doryteuthis.opalescens','Enoploteuthidae',
                         'Galiteuthis', 'Galiteuthis.phyllura','Gonatidae','Gonatopsis.borealis','Gonatus','Gonatus.onyx','Gonatus.pyros','Gonichthys.tenuiculus',
						 'Histioteuthis.heteropsis','Haliphron.atlanticus','Japetella.diaphana','Leachia.pacifica',
						 'Octopodidae','Octopoteuthidae','Octopoteuthis','Octopoteuthis.deletron','Octopus','Ocythoe.tuberculata','Ommastrephes.bartramii','Ommastrephidae',
                         'Onychoteuthis.borealijaponicus','Pterygioteuthis','Pterygioteuthis.gemmata','Pterygioteuthis.giardi','Palinuridae',
                         'Pyroteuthidae','Pyroteuthis.addolux', 'Teuthida','Taoniinae'))]

data2 <- data2[setdiff(names(data2), c('Unidentified', 'Disintegrated.fish.larvae','Perciformes','Aulorhynchus.flavidus'))]	
						 
					 
data2$Anguilliformes1 <- with(data2, Cyema.atrum+Saccopharynx.lavenbergi)
data2 <- data2[setdiff(names(data2), c('Anguilliformes', 'Zoarcidae' , 'Zoarcidae', 'Leptocephalus.giganteus','Ophichthus.zophochir','Congridae','Cyematidae','Cyema.atrum','Leptocephalus.holti','Saccopharynx.lavenbergi','Eurypharynx.pelecanoides'))]					 
						 
data2 <- data2[setdiff(names(data2), c('Clupeiformes', 'Etrumeus.teres'))]
data2 <- data2[setdiff(names(data2), c('Bathylagidae', 'Bathylagus'))]					 
data2 <- data2[setdiff(names(data2), c('Nansenia'))]	
data2 <- data2[setdiff(names(data2), c('Dolichopteryx.longipes'))]	

data2$Gonostomatidae1 <- with(data2, Cyclothone+Cyclothone.acclinidens+Cyclothone.signata)
data2 <- data2[setdiff(names(data2), c('Gonostomatidae','Cyclothone','Cyclothone.acclinidens','Cyclothone.pseudopallida','Cyclothone.signata','Diplophos.taenia','Gonostoma','Gonostoma.atlanticum','Sigmops.ebelingi','Sigmops.elongatum'))]

data2$Sternoptychidae1 <- with(data2, Sternoptyx+Argyropelecus+Argyropelecus.affinis+Argyropelecus.hemigymnus+Argyropelecus.lychnus+Argyropelecus.sladeni+Danaphos.oculatus)
data2 <- data2[setdiff(names(data2), c('Sternoptychidae','Sternoptyx','Argyropelecus','Argyropelecus.affinis','Argyropelecus.hemigymnus','Argyropelecus.lychnus','Argyropelecus.sladeni','Danaphos.oculatus'))]

data2 <- data2[setdiff(names(data2), c('Valenciennellus.tripunctulatus'))]	

data2$Vinciguerria1 <- with(data2,Ichthyococcus.irregularis+Vinciguerria.lucetia+Vinciguerria.poweriae)
data2 <- data2[setdiff(names(data2), c('Ichthyococcus.irregularis','Vinciguerria','Vinciguerria.lucetia','Vinciguerria.poweriae'))]
	
data2 <- data2[setdiff(names(data2), c('Stomiidae', 'Stomiiformes', 'Stomiinae'))]

data2 <- data2[setdiff(names(data2), c('Melanostomiinae', 'Bathophilus', 'Eustomias','Photonectes','Bathophilus.filifer'))]
	
data2$Scopelarchidae1 <- with(data2,Scopelarchidae+Benthalbella+Benthalbella.dentata+Rosenblattichthys.volucris+Scopelarchus.analis+Scopelarchus.guentheri+Scopelosaurus)
data2 <- data2[setdiff(names(data2), c('Scopelarchidae','Benthalbella','Benthalbella.dentata','Rosenblattichthys.volucris','Scopelarchus','Scopelarchus.analis','Scopelarchus.guentheri','Scopelosaurus'))]	
	
data2 <- data2[setdiff(names(data2), c('Synodus.lucioceps'))]		

	
data2$Paralepididae1 <- with(data2, Paralepididae+Arctozenus.risso + Lestidiops+Lestidiops.ringens)					 
data2 <- data2[setdiff(names(data2),c('Paralepididae', 'Arctozenus.risso','Lestidiops','Lestidiops.ringens','Lestidiops.pacificus','Magnisudis.atlantica','Stemonosudis.macrura'))]	
	
data2 <- data2[setdiff(names(data2), c('Stomiidae', 'Stomiiformes', 'Stomiinae'))]	
	
data2$Myctophum1 <- with(data2, Myctophum.nitidulum)					 
data2 <- data2[setdiff(names(data2),c('Myctophum','Myctophum.lychnobium','Myctophum.nitidulum'))]	

data2$Diaphus1 <- with(data2, Diaphus+Diaphus.theta)					 
data2 <- data2[setdiff(names(data2),c('Diaphus','Diaphus.theta'))]

data2 <- data2[setdiff(names(data2),c('Lampanyctus.omostigma','Lampanyctus.acanthurus','Lampanyctus.steinbecki','Lampanyctus.tenuiformis'))]

data2$Nannobrachium1 <- with(data2, Nannobrachium+Nannobrachium.bristori+Nannobrachium.hawaiiensis+Nannobrachium.regale+Nannobrachium.ritteri)					 
data2 <- data2[setdiff(names(data2),c('Nannobrachium','Nannobrachium.bristori','Nannobrachium.hawaiiensis','Nannobrachium.idostigma','Nannobrachium.regale','Nannobrachium.ritteri'))]

data2 <- data2[setdiff(names(data2),c('Parvilux.ingens','Stenobrachius.nannochir'))]

data2 <- data2[setdiff(names(data2),c('Taaningichthys.minimus'))]
	 
data2 <- data2[setdiff(names(data2),c('Triphoturus','Triphoturus.nigrescens'))]

data2 <- data2[setdiff(names(data2),c('Centrobranchus.nigroocellatus'))]
data2 <- data2[setdiff(names(data2),c('Diogenichthys'))]

data2$Hygophum1 <- with(data2,Hygophum.reinhardtii)					 
data2 <- data2[setdiff(names(data2),c('Hygophum','Hygophum.atratum','Hygophum.reinhardtii'))]

data2 <- data2[setdiff(names(data2),c('Protomyctophum','Protomyctophum.thompsoni'))]

data2$Macrouridae1 <- with(data2,Nezumia)					 
data2 <- data2[setdiff(names(data2),c('Macrouridae','Coryphaenoides.acrolepis','Nezumia','Coryphaenoides','Coryphaenoides.leptolepis','Coryphaenoides.pectoralis'))]	
	
data2 <- data2[setdiff(names(data2),c('Merlucciidae'))]	
	
data2$Ophidiiformes1 <- with(data2,Ophidion.scrippsae+Chilara.taylori)
data2 <- data2[setdiff(names(data2),c('Ophidiiformes','Ophidion.scrippsae','Chilara.taylori'))]	
	
data2 <- data2[setdiff(names(data2),c('Bythitidae'))]	
	
data2 <- data2[setdiff(names(data2), c('Caulophryne', 'Dolopichthys','Gigantactis','Oneirodes','Oneirodidae','Cryptopsaras.couesii','Gigantactinidae','Gobiesox.maeandricus','Atherinopsidae','Atherinops.affinis','Atherinopsis.californiensis','Leuresthes.tenuis'))]	
	
data2 <- data2[setdiff(names(data2),c('Cheilopogon.heterurus','Cheilopogon.pinnatibarbatus'))]		
	
data2$Lampridiformes1 <- with(data2,Trachipterus.altivelis)
data2 <- data2[setdiff(names(data2),c('Radiicephalus.elongatus','Trachipteridae','Trachipterus','Desmodema.lorum','Trachipterus.altivelis'))]		
	
data2$Melamphaidae1 <- with(data2,Melamphaes+Melamphaes.lugubris+Melamphaes.parvus+Poromitra.crassiceps+Scopelogadus.mizolepis.bispinosus)
data2 <- data2[setdiff(names(data2),c('Melamphaidae','Melamphaes','Melamphaes.lugubris','Melamphaes.parvus','Melamphaes.simus','Poromitra.crassiceps','Poromitra.megalops','Poromitra.megalops','Scopeloberyx.robustus','Scopelogadus.mizolepis.bispinosus'))]	
	
data2 <- data2[setdiff(names(data2),c('Eutaeniophorus','Eutaeniophorus.festivus','Macroramphosus.gracilis','Cosmocampus.arctus'))]		

data2$Syngnathus1 <- with(data2,Syngnathus.californiensis)
data2 <- data2[setdiff(names(data2),c('Syngnathus','Syngnathus.californiensis'))]	

data2$Sebastes1 <- with(data2,Sebastes+Sebastes.macdonaldi)
data2 <- data2[setdiff(names(data2),c('Sebastes','Sebastes.macdonaldi','Sebastes.melanostomus'))]	

data2$Scorpaena1 <- with(data2,Scorpaena.guttata)
data2 <- data2[setdiff(names(data2),c('Scorpaena','Scorpaena.guttata', 'Scorpaenodes.xyris'))]

data2$Sebastolobus1 <- with(data2,Sebastolobus+Sebastolobus.altivelis)
data2 <- data2[setdiff(names(data2),c('Sebastolobus','Sebastolobus.alascanus', 'Sebastolobus.altivelis'))]

data2 <- data2[setdiff(names(data2),c('Hexagrammidae','Hexagrammos.decagrammus', 'Zaniolepididae','Zaniolepis','Zaniolepis.frenata','Zaniolepis.latipinnis'))]

data2$Cottidae1 <- with(data2,Ruscarius.creaseri+Leptocottus.armatus+Artedius.harringtoni+Artedius.lateralis+Oligocottus.maculosus+Chitonotus.pugetensis+Icelinus.quadriseriatus+Radulinus.asprellus)
						
						
data2 <- data2[setdiff(names(data2),c('Cottidae','Ruscarius.creaseri','Ruscarius.meanyi','Icelinus,Hemilepidotus.spinosus','Leptocottus.armatus','Artedius','Artedius.fenestralis','Artedius.harringtoni','Artedius.lateralis','Clinocottus.analis',
						'Oligocottus','Oligocottus.maculosus','Ascelichthys.rhodorus','Chitonotus.pugetensis','Enophrys.bison','Icelinus.quadriseriatus','Orthonopias.triacis','Paricelinus.hopliticus','Radulinus.asprellus'))]	
	
data2$Agonidae1 <- with(data2,Odontopyxis.trispinosa + Xeneretmus.latifrons)
data2 <- data2[setdiff(names(data2), c('Agonidae', 'Agonopsis.sterletus','Bathyagonus.pentacanthus','Bothragonus.swani','Odontopyxis.trispinosa', 'Xeneretmus.leiops','Xeneretmus.latifrons'))]

data2$Cyclopteridae1 <- with(data2, Liparis.mucosus)
data2 <- data2[setdiff(names(data2), c('Cyclopteridae','Liparis','Liparis.fucensis','Liparis.mucosus'))]

data2$Paralabrax1 <- with(data2, Paralabrax)
data2 <- data2[setdiff(names(data2), c('Paralabrax','Paralabrax.clathratus','Paralabrax.nebulifer'))]
data2 <- data2[setdiff(names(data2), c('Serranidae'))]

data2 <- data2[setdiff(names(data2), c('Coryphaena.equiselis',	'Coryphaena.hippurus',	'Coryphaenoides',	'Coryphaenoides.acrolepis',	'Coryphaenoides.leptolepis',	'Coryphaenoides.pectoralis'))]	
	
data2 <- data2[setdiff(names(data2), c('Bramidae','Brama.japonica'))]
data2 <- data2[setdiff(names(data2), c('Caristiidae'))]

data2 <- data2[setdiff(names(data2), c('Caristius.maderensis','Caulolatilus.princeps','Haemulidae','Anisotremus.davidsoni','Xenistius.californiensis'))]
	
data2$Sciaenidae1 <- with(data2, Atractoscion.nobilis+Cheilotrema.saturnum+Genyonemus.lineatus+Menticirrhus.undulatus+Roncador.stearnsii+Seriphus.politus+Umbrina.roncador)
data2 <- data2[setdiff(names(data2), c('Sciaenidae','Atractoscion.nobilis','Cheilotrema.saturnum','Genyonemus.lineatus','Menticirrhus.undulatus','Roncador.stearnsii','Seriphus.politus','Umbrina.roncador'))]	
	
data2 <- data2[setdiff(names(data2), c('Hermosilla.azurea','Pomacentridae'))]	

data2 <- data2[setdiff(names(data2), c('Howella','Howella.pammelas'))]

data2 <- data2[setdiff(names(data2), c('Mugil.cephalus'))]	

data2$Labridae1 <- with(data2, Halichoeres.semicinctus+Oxyjulis.californica+Semicossyphus.pulcher)
data2 <- data2[setdiff(names(data2), c('Labridae','Halichoeres.semicinctus','Oxyjulis.californica','Semicossyphus.pulcher'))]

data2 <- data2[setdiff(names(data2), c('Ronquilus.jordani','Rathbunella','Rathbunella.alleni','Stichaeidae','Anoplarchus.purpurescens','Plectobranchus.evides','Pholidae'))]

data2$Chiasmodon1 <- with(data2, Chiasmodon.subniger)
data2 <- data2[setdiff(names(data2), c('Chiasmodon','Kali','Chiasmodon.subniger'))]
	
data2 <- data2[setdiff(names(data2), c('Cryptotrema.corallinum'))]	
	
data2$Clinidae1 <- with(data2, Gibbonsia.spp.)
data2 <- data2[setdiff(names(data2), c('Clinidae','Gibbonsia.spp.'))]	
	
data2 <- data2[setdiff(names(data2), c('Chaenopsis.alepidota','Neoclinus','Neoclinus.blanchardi','Neoclinus.stephensae','Blennioidei'))]		
	
data2$Hypsoblennius1 <- with(data2, Hypsoblennius.spp.+Hypsoblennius.gilberti+Hypsoblennius.jenkinsi)
data2 <- data2[setdiff(names(data2), c('Hypsoblennius.spp.','Hypsoblennius.gentilis','Hypsoblennius.gilberti','Hypsoblennius.jenkinsi'))]
	
data2 <- data2[setdiff(names(data2), c('Icosteus.aenigmaticus'))]
	
data2$Gobiidae1 <- with(data2, Clevelandia.ios+Rhinogobiops.nicholsii+Lepidogobius.lepidus+Lythrypnus.zebra+Typhlogobius.californiensis)
data2 <- data2[setdiff(names(data2), c('Gobiidae','Clevelandia.ios','Rhinogobiops.nicholsii','Ilypnus.gilberti','Lepidogobius.lepidus','Lythrypnus','Lythrypnus.dalli','Lythrypnus.zebra','Typhlogobius.californiensis'))]	
	
data2 <- data2[setdiff(names(data2), c('Diplospinus.multistriatus','Lepidocybium.flavobrunneum','Nealotus.tripes','Scombridae','Sarda.chiliensis','Lepidopus.fitchi'))]		

data2 <- data2[setdiff(names(data2), c('Cubiceps.baxteri','Psenes.pellucidus'))]	

data2 <- data2[setdiff(names(data2), c('Paralichthyidae','Paralichthys','Xystreurys.liolepis','Perissias.taeniopterus','Pleuronectidae','Pleuronectiformes','Hippoglossus.stenolepis','
Embassichthys.bathybius','Eopsetta.jordani','Hypsopsetta.guttulata','Isopsetta.isolepis','Lepidopsetta.bilineata','Trichiuridae'
))]	

data2$Citharichthys1 <- with(data2, Citharichthys+Citharichthys.sordidus+Citharichthys.stigmaeus+Citharichthys.xanthostigma)
data2 <- data2[setdiff(names(data2), c('Citharichthys','Citharichthys.fragilis','Citharichthys.sordidus','Citharichthys.stigmaeus','Citharichthys.xanthostigma'))]	

data2$Pleuronichthys1 <- with(data2, Pleuronichthys+Pleuronichthys.ritteri)
data2 <- data2[setdiff(names(data2), c('Pleuronichthys','Pleuronichthys.ritteri'))]	

data2 <- data2[setdiff(names(data2), c('Anoplopoma.fimbria'))]	

data2 <- data2[setdiff(names(data2), c('Poromitra', 'Prionotus'))]	
data2 <- data2[setdiff(names(data2), c('Myctophidae'))]	
data2 <- data2[setdiff(names(data2), c('Icelinus'))]	
data2 <- data2[setdiff(names(data2), c('Carangidae', 'Cataetyx.rubrirostris', 'Ceratioidei'))]	
data2 <- data2[setdiff(names(data2), c('Benthosema'))]

head(data2) 
