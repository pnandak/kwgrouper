## dependencies

## library(dplyr)
## library(WriteXLS)
## library(reshape2)


## keywords laden

## keywords < read.csv("bfgl.csv", sep=",") %>% select(keyword)

## processing code

keywordsProcessed <- keywords %>%
        
        mutate(
                
                ## Nissewaard
                Nissewaard=grepl("nissewaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wijdemeren
                Wijdemeren=grepl("Wijdemeren",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dongen
                Dongen=grepl("dongen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hardenberg
                Hardenberg=grepl("hardenberg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Schagen
                Schagen=grepl("schagen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Eindhoven
                Eindhoven=grepl("Eindhoven",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haaren
                Haaren=grepl("Haaren",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Houten
                Houten=grepl("Houten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Coevorden
                Coevorden=grepl("Coevorden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leeuwarderadeel
                Leeuwarderadeel=grepl("Leeuwarderadeel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rijswijk
                Rijswijk=grepl("Rijswijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Steenbergen
                Steenbergen=grepl("Steenbergen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Elburg
                Elburg=grepl("Elburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Renkum
                Renkum=grepl("Renkum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Littenseradiel
                Littenseradiel=grepl("Littenseradiel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Peel en Maas
                PeelMaas=grepl("(?=.*peel)(?=.*maas)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Boxmeer
                Boxmeer=grepl("Boxmeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hulst
                Hulst=grepl("Hulst",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tilburg
                Tilburg=grepl("Tilburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Laarbeek
                Laarbeek=grepl("Laarbeek",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Berkelland
                Berkelland=grepl("Berkelland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leek
                Leek=grepl("Leek",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Epe
                Epe=grepl("^Epe | Epe | Epe$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Eemsmond
                Eemsmond=grepl("Eemsmond",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bergen
                Bergen=grepl("Bergen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dinkelland
                Dinkelland=grepl("Dinkelland|(?=.*dinkel)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oldebroek
                Oldebroek=grepl("Oldebroek",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Woensdrecht
                Woensdrecht=grepl("Woensdrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hoogeveen
                Hoogeveen=grepl("Hoogeveen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Putten
                Putten=grepl("Putten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oost Gelre
                OostGelre=grepl("(?=.*oost)(?=.*gelre)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## EijsdenMargraten
                EijsdenMargraten=grepl("(?=.*Eijsden)(?=.*Margraten)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dongeradeel
                Dongeradeel=grepl("Dongeradeel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## De Marne
                DeMarne=grepl("(?=.*de)(?=.*marne)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hollands Kroon
                HollandsKroon=grepl("(?=.*hollands)(?=.*kroon)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Aalten
                Aalten=grepl("Aalten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Borsele
                Borsele=grepl("Borsele",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oldambt
                Oldambt=grepl("Oldambt|(?=.*old)(?=.*ambt)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nijkerk
                Nijkerk=grepl("Nijkerk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## MiddenDelfland
                MiddenDelfland=grepl("Delfland|(?=.*midden)(?=.*delfland)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tytsjerksteradiel
                Tytsjerksteradiel=grepl("Tytsjerksteradiel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## AlphenChaam
                AlphenChaam=grepl("(?=.*alphen)(?=.*chaam)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hattem
                Hattem=grepl("Hattem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nijmegen
                Nijmegen=grepl("Nijmegen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Werkendam
                Werkendam=grepl("Werkendam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Loppersum
                Loppersum=grepl("Loppersum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Almelo
                Almelo=grepl("Almelo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Veere
                Veere=grepl("^Veere | Veere | Veere$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nuth
                Nuth=grepl("^Nuth | Nuth | Nuth$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Gennep
                Epe=grepl("^Gennep | Gennep | Gennep$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Aalsmeer
                Aalsmeer=grepl("Aalsmeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haarlemmermeer
                Haarlemmermeer=grepl("Haarlemmermeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Buren
                Buren=grepl("^Buren | Buren | Buren$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Waalre
                Waalre=grepl("Waalre",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zaltbommel
                Zaltbommel=grepl("Zaltbommel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zuidhorn
                Zuidhorn=grepl("Zuidhorn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Overbetuwe
                Overbetuwe=grepl("Overbetuwe",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## EchtSusteren
                EchtSusteren=grepl("^Echt | Echt | Echt$|^Susteren | Susteren | Susteren$|(?=.*echt)(?=.*susteren)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Roerdalen
                Roerdalen=grepl("Roerdalen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leudal
                Leudal=grepl("Leudal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Meerssen
                Meerssen=grepl("Meerssen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Roermond
                Roermond=grepl("Roermond",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Alphen aan den Rijn
                AlphenAanDeRijn=grepl("(?=.*alphen)(?=.*rijn)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Best
                Best=grepl("^Best | Best | Best$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Opmeer
                Opmeer=grepl("Opmeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## SúdwestFryslân
                ## SudwestFryslan=grepl("(?=.*(Súdwest|Sudwest|Zuidwest))(?=.*(Fryslân|Fryslan|Friesland))",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Medemblik
                Medemblik=grepl("Medemblik",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Goes
                Goes=grepl("^Goes | Goes | Goes$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leeuwarden
                Leeuwarden=grepl("Leeuwarden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Winsum
                Winsum=grepl("Winsum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## De Ronde Venen
                DeRondeVenen=grepl("(?=.*ronde)(?=.*venen)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## SittardGeleen
                SittardGeleen=grepl("Sittard|Geleen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Delft
                Delft=grepl("Delft",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Franekeradeel
                Franekeradeel=grepl("Franekeradeel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Asten
                Asten=grepl("Asten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Krimpenerwaard
                Krimpenerwaard=grepl("Krimpenerwaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Someren
                Someren=grepl("Someren",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## HoogezandSappemeer
                HoogezandSappemeer=grepl("Hoogezand|Hogezand|Sappemeer|Sappermeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Moerdijk
                Moerdijk=grepl("Moerdijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zederik
                Zederik=grepl("Zederik",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bronckhorst
                Bronckhorst=grepl("Bronckhorst|Bronkhorst",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hof van Twente
                HofVanTwente=grepl("(?=.*hof)(?=.*twente)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zwijndrecht
                Empty=grepl("Zwijndrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Valkenswaard
                Valkenswaard=grepl("Valkenswaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oss
                Oss=grepl("^Oss | Oss | Oss$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## ReuselDe Mierden
                ReuselDeMierden=grepl("Reusel|Mierden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Schijndel
                Schijndel=grepl("Schijndel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Breda
                Breda=grepl("Breda",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ten Boer
                TenBoer=grepl("(?=.*ten)(?=.*boer)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leusden
                Leusden=grepl("Leusden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## GoereeOverflakkee
                GoereeOverflakkee=grepl("goeree|overflakkee|flakkee",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zundert
                Zundert=grepl("Zundert",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nieuwkoop
                Nieuwkoop=grepl("Nieuwkoop",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Geldermalsen
                Geldermalsen=grepl("Geldermalsen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ooststellingwerf
                Ooststellingwerf=grepl("Ooststellingwerf|stellingwerf",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rijnwaarden
                Rijnwaarden=grepl("Rijnwaarden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bloemendaal
                Bloemendaal=grepl("Bloemendaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Druten
                Druten=grepl("Druten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vlagtwedde
                Vlagtwedde=grepl("Vlagtwedde",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Castricum
                Castricum=grepl("Castricum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Roosendaal
                Roosendaal=grepl("Roosendaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heerenveen
                Heerenveen=grepl("Heerenveen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## De Friese Meren
                DeFrieseMeren=grepl("(?=.*friese)(?=.*meren)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tubbergen
                Tubbergen=grepl("Tubbergen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Alblasserdam
                Alblasserdam=grepl("Alblasserdam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Kollumerland en Nieuwkruisland
                KollumerlandEnNieuwkruisland=grepl("Kollum|kruisland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Maasdriel
                Maasdriel=grepl("Maasdriel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Utrecht
                Utrecht=grepl("Utrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Alkmaar
                Alkmaar=grepl("Alkmaar",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lochem
                Lochem=grepl("Lochem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Almere
                Almere=grepl("Almere",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Woudrichem
                Woudrichem=grepl("Woudrichem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Noordenveld
                Noordenveld=grepl("Noordenveld",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## De Wolden
                DeWolden=grepl("(?=.*de)(?=.*wolden)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## West Maas en Waal
                WestMaasEnWaal=grepl("(?=.*west)(?=.*maas)(?=.*waal)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Stadskanaal
                Stadskanaal=grepl("Stadskanaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## MiddenDrenthe
                MiddenDrenthe=grepl("(?=.*midden)(?=.*(drenthe|drente))",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Weert
                Weert=grepl("Weert",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wijchen
                Wijchen=grepl("Wijchen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nuenen, Gerwen en Nederwetten
                NuenenGerwenNederwetten=grepl("Nuenen|Gerwen|Nederwetten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Aa en Hunze
                AaEnHunze=grepl("^Aa | Aa | Aa$|hunze",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Horst aan de Maas
                HorstAanDeMaas=grepl("(?=.*horst)(?=.*maas)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Utrechtse Heuvelrug
                UtrechtseHeuvelrug=grepl("(?=.*utrecht)(?=.*heuvelrug)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Amersfoort
                Amersfoort=grepl("Amersfoort",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Amstelveen
                Amstelveen=grepl("Amstelveen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Schinnen
                Schinnen=grepl("Schinnen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Amsterdam
                Amsterdam=grepl("Amsterdam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## OuderAmstel
                OuderAmstel=grepl("(?=.*oude)(?=.*amstel)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Delfzijl
                Delfzijl=grepl("Delfzijl|Delftzijl",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lingewaard
                Lingewaard=grepl("Lingewaard|Lingenwaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zevenaar
                Zevenaar=grepl("Zevenaar",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dalfsen
                Dalfsen=grepl("Dalfsen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tholen
                Tholen=grepl("Tholen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Assen
                Assen=grepl("Assen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Apeldoorn
                Apeldoorn=grepl("Apeldoorn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Deventer
                Deventer=grepl("Deventer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Voorst
                Voorst=grepl("Voorst",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Appingedam
                Appingedam=grepl("Appingedam|Appingendam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Venlo
                Venlo=grepl("Venlo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ommen
                Ommen=grepl("^Ommen | Ommen | Ommen$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Boekel
                Boekel=grepl("Boekel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Giessenlanden
                Giessenlanden=grepl("Giessenlanden|Giesenlanden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Middelburg
                Middelburg=grepl("Middelburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Arnhem
                Arnhem=grepl("Arnhem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hillegom
                Hillegom=grepl("Hillegom",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lingewaal
                Lingewaal=grepl("Lingewaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zaanstad
                Zaanstad=grepl("Zaanstad",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Achtkarspelen
                Achtkarspelen=grepl("Achtkarspelen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zeist
                Zeist=grepl("Zeist",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Koggenland
                Koggenland=grepl("Koggenland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Terneuzen
                Terneuzen=grepl("Terneuzen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Montferland
                Montferland=grepl("Montferland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Terschelling
                Terschelling=grepl("Terschelling",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Simpelveld
                Simpelveld=grepl("Simpelveld",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Westland
                Westland=grepl("Westland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## BaarleNassau
                BaarleNassau=grepl("Baarle|Nassau",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Steenwijkerland
                Steenwijkerland=grepl("Steenwijkerland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zwartewaterland
                Zwartewaterland=grepl("Zwartewaterland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Baarn
                Baarn=grepl("Baarn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Deurne
                Deurne=grepl("Deurne",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hilvarenbeek
                Hilvarenbeek=grepl("Hilvarenbeek",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oosterhout
                Oosterhout=grepl("Oosterhout",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Maasgouw
                Maasgouw=grepl("Maasgouw",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Aalburg
                Aalburg=grepl("Aalburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## GemertBakel
                GemertBakel=grepl("Gemert|^Bakel |Bakel | Bakel$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Marum
                Marum=grepl("^Marum | Marum | Marum$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Opsterland
                Opsterland=grepl("Opsterland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ameland
                Ameland=grepl("Ameland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Noordoostpolder
                Noordoostpolder=grepl("(?=.*oost)(?=.*polder)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Barendrecht
                Barendrecht=grepl("Barendrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Emmen
                Emmen=grepl("^Emmen | Emmen | Emmen$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Barneveld
                Barneveld=grepl("Barneveld",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Voerendaal
                Voerendaal=grepl("Voerendaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ferwerderadiel
                Ferwerderadiel=grepl("Ferwerderadiel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Reimerswaal
                Reimerswaal=grepl("Reimerswaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hengelo
                Hengelo=grepl("Hengelo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Uden
                Uden=grepl("^Uden | Uden | Uden$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bedum
                Bedum=grepl("^Bedum | Bedum | Bedum$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Beek
                Beek=grepl("^Beek | Beek | Beek$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Groesbeek
                Groesbeek=grepl("Groesbeek",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Venray
                Venray=grepl("Venray",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Cuijk
                Cuijk=grepl("Cuijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## GulpenWittem
                GulpenWittem=grepl("Gulpen|Wittem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Beesel
                Beesel=grepl("Beesel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Menameradiel
                Menameradiel=grepl("Menameradiel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zeevang
                Zeevang=grepl("Zeevang",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Doesburg
                Doesburg=grepl("Doesburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## GeldropMierlo
                GeldropMierlo=grepl("Geldrop|Gelrop|Mierlo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## SchouwenDuiveland
                SchouwenDuiveland=grepl("(?=.*Schouwe)(?=.*Duive)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bellingwedde
                Bellingwedde=grepl("Bellingwedde",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Menterwolde
                Menterwolde=grepl("Menterwolde",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ede
                Ede=grepl("^Ede | Ede | Ede$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lopik
                Lopik=grepl("Lopik",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zandvoort
                Zandvoort=grepl("Zandvoort",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heerlen
                Heerlen=grepl("Heerlen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Helmond
                Helmond=grepl("Helmond",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Cranendonck
                Cranendonck=grepl("Cranendonck",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Valkenburg aan de Geul
                ValkenburgAanDeGeul=grepl("valkenburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Stein
                Stein=grepl("^Stein | Stein | Stein$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tiel
                Tiel=grepl("^Tiel | Tiel | Tiel$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bergen op Zoom
                BergenOpZoom=grepl("(?=.*bergen)(?=.*zoom)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vught
                Vught=grepl("Vught",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lansingerland
                Lansingerland=grepl("Lansingerland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bernheze
                Bernheze=grepl("Bernheze",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Veldhoven
                Veldhoven=grepl("Veldhoven",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## SintMichielsgestel
                SintMichielsgestel=grepl("Michielsgestel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Veghel
                Veghel=grepl("Veghel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Beuningen
                Beuningen=grepl("Beuningen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Losser
                Losser=grepl("Losser",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## RijssenHolten
                RijssenHolten=grepl("Rijssen|Holten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Beverwijk
                Beverwijk=grepl("Beverwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dronten
                Dronten=grepl("Dronten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Gilze en Rijen
                GilzeEnRijen=grepl("Gilze|Rijen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Kapelle
                Kapelle=grepl("Kapelle",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bladel
                Bladel=grepl("Bladel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Eersel
                Eersel=grepl("Eersel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Kaag en Braassem
                KaagEnBraassem=grepl("^Kaag | Kaag | Kaag$|Braassem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## De Bilt
                DeBilt=grepl("^Bilt | Bilt | Bilt$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Onderbanken
                Onderbanken=grepl("Onderbanken",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Drimmelen
                Drimmelen=grepl("Drimmelen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Kampen
                Kampen=grepl("Kampen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Mook en Middelaar
                MookEnMiddelaar=grepl("^Mook | Mook | Mook$|Middelaar",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Binnenmaas
                Binnenmaas=grepl("Binnenmaas",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heumen
                Heumen=grepl("Heumen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Blaricum
                Blaricum=grepl("Blaricum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Sint Anthonis
                SintAnthonis=grepl("Anthonis",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Weststellingwerf
                Weststellingwerf=grepl("Weststellingwerf",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Slochteren
                Slochteren=grepl("Slochteren",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## BodegravenReeuwijk
                BodegravenReeuwijk=grepl("Bodegraven|Reeuwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Enschede
                Enschede=grepl("Enschede",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## OlstWijhe
                OlstWijhe=grepl("^Olst | Olst | Olst$|Wijhe",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## BorgerOdoorn
                BorgerOdoorn=grepl("Borger|Odoorn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## 'sHertogenbosch
                sHertogenbosch=grepl("Hertogenbosch|(?=.*den)(?=.*bosch)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Cromstrijen
                Cromstrijen=grepl("Cromstrijen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Smallingerland
                Smallingerland=grepl("Smallingerland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Veendam
                Veendam=grepl("Veendam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Maastricht
                Maastricht=grepl("Maastricht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Borne
                Borne=grepl("^Borne | Borne | Borne$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Mill en Sint Hubert
                MillEnSintHubert=grepl("^Mill | Mill | Mill$|(?=.*sint)(?=.*hubert)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bergeijk
                Bergeijk=grepl("Bergeijk|Bergijk|Bergeik",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## HeezeLeende
                HeezeLeende=grepl("^Heeze | Heeze | Heeze$|Leende",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## SintOedenrode
                SintOedenrode=grepl("Oedenrode",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Halderberge
                Halderberge=grepl("Halderberge",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nederweert
                Nederweert=grepl("Nederweert",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Westerveld
                Westerveld=grepl("Westerveld",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oirschot
                Oirschot=grepl("Oirschot",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Pekela
                Pekela=grepl("Pekela",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## HardinxveldGiessendam
                HardinxveldGiessendam=grepl("Hardinxveld|Hardinksveld|Giessendam|Giesendam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Stede Broec
                StedeBroec=grepl("$Stede | Stede | Stede$|Broec",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Boxtel
                Boxtel=grepl("Boxtel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Landerd
                Landerd=grepl("Landerd",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Molenwaard
                Molenwaard=grepl("Molenwaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oude IJsselstreek
                OudeIJsselstreek=grepl("(?=.*oud)(?=.*(ijsselstreek|ijselstreek))",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Son en Breugel
                SonEnBreugel=grepl("^Son | Son | Son$|Bruegel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Stichtse Vecht
                StichtseVecht=grepl("(?=.*stichtse)(?=.*vecht)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Brielle
                Brielle=grepl("Brielle",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Waterland
                Waterland=grepl("Waterland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Langedijk
                Langedijk=grepl("Langedijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Meppel
                Meppel=grepl("Meppel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Raalte
                Raalte=grepl("Raalte",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dantumadiel
                Dantumadiel=grepl("Dantumadiel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oisterwijk
                Oisterwijk=grepl("Oisterwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Twenterand
                Twenterand=grepl("Twenterand",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Brummen
                Brummen=grepl("Brummen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Brunssum
                Brunssum=grepl("Brunssum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Tynaarlo
                Tynaarlo=grepl("Tynaarlo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bunnik
                Bunnik=grepl("Bunnik",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bunschoten
                Bunschoten=grepl("Bunschoten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Bussum
                Bussum=grepl("Bussum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haaksbergen
                Haaksbergen=grepl("Haaksbergen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vaals
                Vaals=grepl("^Vaals | Vaals | Vaals$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Waalwijk
                Waalwijk=grepl("Waalwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Capelle aan den IJssel
                CapelleAanDenIJssel=grepl("(?=.*(Capelle|kapelle))(?=.*(ijssel|ijsel))",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## NoordBeveland
                NoordBeveland=grepl("(?=.*noord)(?=.*beveland)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wijk bij Duurstede
                WijkBijDuurstede=grepl("Duurstede",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Neerijnen
                Neerijnen=grepl("Neerijnen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Culemborg
                Culemborg=grepl("Culemborg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hellendoorn
                Hellendoorn=grepl("Hellendoorn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Gorinchem
                Gorinchem=grepl("Gorinchem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ermelo
                Ermelo=grepl("Ermelo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rucphen
                Rucphen=grepl("Rucphen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Harlingen
                Harlingen=grepl("Harlingen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Grave
                Grave=grepl("^grave | grave | grave$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Texel
                Texel=grepl("Texel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lisse
                Lisse=grepl("Lisse",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heusden
                Heusden=grepl("Heusden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Grootegast
                Grootegast=grepl("Grootegast",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Noordwijk
                Noordwijk=grepl("Noordwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Strijen
                Strijen=grepl("Strijen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wierden
                Wierden=grepl("Wierden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Uithoorn
                Uithoorn=grepl("Uithoorn",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Geertruidenberg
                Geertruidenberg=grepl("Geertruidenberg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Loon op zand
                LoonOpZand=grepl("(?=.*loon)(?=.*zand)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rheden
                Rheden=grepl("Rheden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Noordwijkerhout
                Noordwijkerhout=grepl("Noordwijkerhout",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## PijnackerNootdorp
                PijnackerNootdorp=grepl("Pijnacker|Pijnakker|Nootdorp|Nooddorp",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## NederBetuwe
                NederBetuwe=grepl("(?=.*neder)(?=.*betuwe)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wassenaar
                Wassenaar=grepl("Wassenaar",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Den Haag
                DenHaag=grepl("Gravenhaege|Gravenhage|(?=.*den)(?=.*haag)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Den Helder
                DenHelder=grepl("(?=.*den)(?=.*helder)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Staphorst
                Staphorst=grepl("Staphorst",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Landsmeer
                Landsmeer=grepl("Landsmeer",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leerdam
                Leerdam=grepl("Leerdam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Diemen
                Diemen=grepl("Diemen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Doetinchem
                Doetinchem=grepl("Doetinchem",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Dordrecht
                Dordrecht=grepl("Dordrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Groningen
                Groningen=grepl("Groningen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Velsen
                Velsen=grepl("Velsen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Harderwijk
                Harderwijk=grepl("Harderwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## EdamVolendam
                EdamVolendam=grepl("^Edam | Edam | Edam$|Volendam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Eemnes
                Eemnes=grepl("Eemnes",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nunspeet
                Nunspeet=grepl("Nunspeet",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rhenen
                Rhenen=grepl("Rhenen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Enkhuizen
                Enkhuizen=grepl("Enkhuizen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haren
                Haren=grepl("^Haren | Haren | Haren$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## EttenLeur
                EttenLeur=grepl("^Etten | Etten | Etten$|^Leur | Leur | Leur$|Ettenleur|(?=.*etten)(?=.*leur)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vianen
                Vianen=grepl("Vianen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Kerkrade
                Kerkrade=grepl("Kerkrade",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zwolle
                Zwolle=grepl("Zwolle",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zoeterwoude
                Zoeterwoude=grepl("Zoeterwoude",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Gouda
                Gouda=grepl("Gouda",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Korendijk
                Korendijk=grepl("Korendijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## OudBeijerland
                OudBeijerland=grepl("Beijerland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zuidplas
                Zuidplas=grepl("Zuidplas",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vlissingen
                Vlissingen=grepl("Vlissingen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haarlem
                Haarlem=grepl("^Haarlem | Haarlem | Haarlem$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Haarlemmerliede en Spaarnwoude
                HaarlemmerliedeEnSpaarnwoude=grepl("haarlemliede|spaarnwoude",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Woerden
                Woerden=grepl("Woerden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heemskerk
                Heemskerk=grepl("Heemskerk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heemstede
                Heemstede=grepl("Heemstede",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heerde
                Heerde=grepl("^Heerde | Heerde | Heerde$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heerhugowaard
                Heerhugowaard=grepl("Heerhugowaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Heiloo
                Heiloo=grepl("Heiloo",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oudewater
                Oudewater=grepl("Oudewater",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Westvoorne
                Westvoorne=grepl("Westvoorne",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hellevoetsluis
                Hellevoetsluis=grepl("Hellevoetsluis",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Drechterland
                Drechterland=grepl("Drechterland|^Drecht | Drecht | Drecht$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## HendrikIdoAmbacht
                HendrikIdoAmbacht=grepl("(?=.*hendrik)(?=.*ido)(?=.*ambacht)",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hilversum
                Hilversum=grepl("Hilversum",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rotterdam
                Rotterdam=grepl("Rotterdam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Hoorn
                Hoorn=grepl("^Hoorn | Hoorn | Hoorn$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Huizen
                Huizen=grepl("^Huizen | Huizen | Huizen$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## IJsselstein
                IJsselstein=grepl("IJsselstein",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wormerland
                Wormerland=grepl("Wormerland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Katwijk
                Katwijk=grepl("Katwijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Schiedam
                Schiedam=grepl("Schiedam",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Albrandswaard
                Albrandswaard=grepl("Albrandswaard",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Krimpen aan den IJssel
                KrimpenAanDenIJssel=grepl("(?=.*krimpen)(?=.*(ijssel|ijsel))",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Laren
                Laren=grepl("^Laren | Laren | Laren$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leiden
                Leiden=grepl("^Leiden | Leiden | Leiden$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Leiderdorp
                Leiderdorp=grepl("Leiderdorp",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## LeidschendamVoorburg
                LeidschendamVoorburg=grepl("Leidschendam|Voorburg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Lelystad
                Lelystad=grepl("Lelystad",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Montfoort
                Montfoort=grepl("Montfoort",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Maassluis
                Maassluis=grepl("Maassluis",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Beemster
                Beemster=grepl("Beemster",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## het Bildt
                HetBildt=grepl("Bildt",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Muiden
                Muiden=grepl("^Muiden | Muiden | Muiden$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Naarden
                Naarden=grepl("Naarden",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Nieuwegein
                Nieuwegein=grepl("Nieuwegein",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Landgraaf
                Landgraaf=grepl("Landgraaf",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oegstgeest
                Oegstgeest=grepl("Oegstgeest",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oldenzaal
                Oldenzaal=grepl("Oldenzaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vlieland
                Vlieland=grepl("Vlieland",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Papendrecht
                Papendrecht=grepl("Papendrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Oostzaan
                Oostzaan=grepl("Oostzaan",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Purmerend
                Purmerend=grepl("Purmerend",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Renswoude
                Renswoude=grepl("Renswoude",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Ridderkerk
                Ridderkerk=grepl("Ridderkerk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Rozendaal
                Rozendaal=grepl("Rozendaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Teylingen
                Teylingen=grepl("Teylingen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Westervoort
                Westervoort=grepl("Westervoort",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Scherpenzeel
                Scherpenzeel=grepl("Scherpenzeel",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Schiermonnikoog
                Schiermonnikoog=grepl("Schiermonnikoog",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Sliedrecht
                Sliedrecht=grepl("Sliedrecht",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Soest
                Soest=grepl("Soest",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Uitgeest
                Uitgeest=grepl("Uitgeest",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Urk
                Urk=grepl("^Urk | Urk | Urk$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Veenendaal
                Veenendaal=grepl("Veenendaal",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Vlaardingen
                Vlaardingen=grepl("Vlaardingen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Voorschoten
                Voorschoten=grepl("Voorschoten",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Waddinxveen
                Waddinxveen=grepl("Waddinxveen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Wageningen
                Wageningen=grepl("Wageningen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zutphen
                Zutphen=grepl("Zutphen",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Weesp
                Weesp=grepl("^Weesp | Weesp | Weesp$",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Winterswijk
                Winterswijk=grepl("Winterswijk",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Woudenberg
                Woudenberg=grepl("Woudenberg",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zeewolde
                Zeewolde=grepl("Zeewolde",keyword,ignore.case=TRUE,perl=TRUE),
                
                ## Zoetermeer
                Zoetermeer=grepl("Zoetermeer",keyword,ignore.case=TRUE,perl=TRUE)
                          
        )
