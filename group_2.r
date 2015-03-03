## dependencies

library(dplyr)
library(WriteXLS)
library(reshape2)


## keywords laden

keywords <- read.csv("bfgl-kwpo.csv", sep=";") %>% select(keyword)


## frequentietabel

woorden <- paste(keywords$keyword, collapse=" ")
uniekeWoorden <- strsplit(woorden, " ")[[1]]
woordFreq <- as.data.frame(table(uniekeWoorden)) %>%
  arrange(desc(Freq))

rm(uniekeWoorden, woorden)

write.csv(woordFreq, "woordFreq.csv")


## processed

keywordsProcessed <- keywords %>%
  
  mutate(
    
    ## 1. transport excl. transporteur
    transport=grepl("(?=.*transport)((?!transporte).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## 2. Container
    ltl=grepl("container",keyword,ignore.case=TRUE),
    
    ## 3. logistic
    logistics=grepl("logistic",keyword,ignore.case=TRUE),
    
    ## 4. tracking
    track=grepl("(?=.*track)((?!trace).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 5. track and trace
    TrackAndTrace=grepl("(?=.*track)(?=.*trace)",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 6. terminal
    terminal=grepl("terminal",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 7. Bedrijf
    bedrijf=grepl("bedrijf|bedrijven",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 8. Douane
    douane=grepl("douane",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 9. shipping
    shipping=grepl("shipping",keyword,ignore.case=TRUE),
    
    ## 10. shipment
    shipment=grepl("shipment",keyword,ignore.case=TRUE),
    
    ## 11. ship
    ship=grepl("^ship | ship | ship$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 12. logistiek
    ship=grepl("logistiek",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 13. schiphol
    schiphol=grepl("schiphol",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 14. ECT Delta
    ECTDelta=grepl("^ect | ect | ect$|(?=.*(^ect | ect | ect$))(?=.*(^delta | delta | delta$))",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 15. internationaal
    internationaal=grepl("internationaal|international",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 16. maasvlakte
    maasvlakte=grepl("maasvlakte",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 17. schedule
    schedule=grepl("schedule",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 18. haven
    schedule=grepl("(?=.*haven)((?!lucht).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 19. koeltransport
    koeltransport=grepl("(?=.*koel)(?=.*transport)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 20. Sailing
    sailing=grepl("sail",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 21. Sailing
    sailing=grepl("sailing",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 22. naar
    sailing=grepl("^naar | naar | naar$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 23. vervoer
    vervoer=grepl("vervoer",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 24. Delta (non-ect)
    delta=grepl("(?=.*delta)((?!ect).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 25. APM
    apm=grepl("^apm | apm | apm$",keyword,ignore.case=TRUE,perl=TRUE), 
    
    ## 26. Groupage
    groupage=grepl("groupage",keyword,ignore.case=TRUE,perl=TRUE), 
    
    ## 27. Lines
    lines=grepl("lines",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 28. Vessel
    vessel=grepl("vessel",keyword,ignore.case=TRUE,perl=TRUE), 
    
    ## 29. Waalhaven
    waalhaven=grepl("waalhaven",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 30. Declarant
    declarant=grepl("declarant",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 31. Euromax
    euromax=grepl("euromax",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 32. Beurs
    beurs=grepl("beurs",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 33. Transporteurs
    transporteur=grepl("transporteur",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 33. Auto
    auto=grepl("auto",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 34. Depot
    depot=grepl("depot",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 35. Freight
    freight=grepl("freight",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 36. Kramer
    kramer=grepl("kramer",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 37. Reefer
    reefer=grepl("reefer",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 38. Cargo
    cargo=grepl("cargo",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 39. Company
    company=grepl("company|companies",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 40. global
    global=grepl("global",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 41. Service
    service=grepl("service",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 42. Solutions
    solutions=grepl("solution",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 43. Speciaal
    speciaal=grepl("speciaal",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 44. Trucking
    trucking=grepl("trucking",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 45. Expediteur
    expediteur=grepl("expediteur",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 46. Expeditie
    expeditie=grepl("expediti",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 47. Overslag
    overslag=grepl("overslag",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 48. Feet
    feet=grepl("5ft |0ft | ft$|feet|foot",keyword,ignore.case=TRUE,perl=TRUE),
      
    ## 49. 10
    tien=grepl("10| 10",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 50. 20
    twintig=grepl("20| 20",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 51. 40
    veertig=grepl("40| 40",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 52. 45
    vijfenveertig=grepl("45| 45",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 53. Adres
    adres=grepl("adres",keyword,ignore.case=TRUE,perl=TRUE),
    
## Groeperen van landen    
    
    ## Nederland
    Nederland=grepl("nederland|netherland",keyword,ignore.case=TRUE,perl=TRUE),

    ## Duitsland
    Duitsland=grepl("duitsland|germany",keyword,ignore.case=TRUE,perl=TRUE),

    ## China
    China=grepl("china",keyword,ignore.case=TRUE,perl=TRUE),

    ## Frankrijk
    Frankrijk=grepl("frankrijk|france",keyword,ignore.case=TRUE,perl=TRUE),

    ## Belgie
    Belgie=grepl("belgie|belgium",keyword,ignore.case=TRUE,perl=TRUE),

    ## Spanje
    Spanje=grepl("spanje|spain",keyword,ignore.case=TRUE,perl=TRUE),

    ## Engeland
    Engeland=grepl("Engeland|England",keyword,ignore.case=TRUE,perl=TRUE),

## Groeperen van buitenlandse steden

    ## Antwerpen
    antwerpen=grepl("antwerp",keyword,ignore.case=TRUE,perl=TRUE),

    ## Zeebrugge
    zeebrugge=grepl("zeebrugge",keyword,ignore.case=TRUE,perl=TRUE),

## Groeperen van Transporteurs

    ## H.J Bakker Transport Roosendaal
    HJBakkerTransport=grepl("(?=.*bakker)(?=.*(roosendaal|hj|h.j.))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Koeltransport
    BakkerInternationaalKoeltransport=grepl("(?=.*bakker)(?=.*(bovenkarspel|koel))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Transport Giethoorn
    BakkerTransportGiethoorn=grepl("(?=.*bakker)(?=.*giethoorn)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Logistiek Huybrechts
    BakkerLogistiekHuybrechts=grepl("(?=.*bakker)(?=.*(tilburg|huybrechts))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Jan de Bakker Transport
    JanDeBakkerBV=grepl("(?=.*bakker)(?=.*(jan|bommel))",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## Bakker en Schilder Transport
    BakkerEnSchilderTransport=grepl("(?=.*bakker)(?=.*(schilder|zand))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Logistiek
    BakkerLogistiekZeewolde=grepl("(?=.*bakker)(?=.*(zeewolde|logistics|logistiek))((?!(schilder|zand|jan|bommel|tilburg|huybrechts|giethoorn|bovenkarspel|roosendaal|hj|h.j.)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker en Brouwer
    BakkerEnBrouwer=grepl("(?=.*bakker)(?=.*(brouwer|vuren))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Logistiek
    BakkerLogistiekDeventer=grepl("(?=.*bakker)(?=.*deventer)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Transport Vianen
    BakkerTransportVianen=grepl("(?=.*bakker)(?=.*vianen)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Transportbedrijf Bakker
    BakkerTransportbedrijfReduzum=grepl("(?=.*bakker)(?=.*(reduzum|transportbedrijf))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bakker Transport Warehousing
    BakkerTransportWarehousing=grepl("(?=.*bakker)(?=.*(internatio|transport|heerenveen|warehous))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Jan Bakker Transport
    JanBakkerTransport=grepl("(?=.*bakker)(?=.*(jan|oldebroek))",keyword,ignore.case=TRUE,perl=TRUE),

    ## AB Texel Transport
    ABTexel=grepl("(?=.*(AB|bakker))(?=.*texel)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## Wim Bosman Group
    WimBosmanGroup=grepl("(?=.*bosman)(?=.*(heerenberg|wim|logist))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Wim Bosman Group
    GBosmanNisse=grepl("(?=.*bosman)(?=.*(nisse|^g |frank|transport))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Transportbedrijf Bosman
    TransportbedrijfBosman=grepl("(?=.*bosman)(?=.*(transportbedrijf|gaanderen))",keyword,ignore.case=TRUE,perl=TRUE),

    ## DSV
    DSV=grepl("^dsv | dsv | dsv$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Rhenus
    Rhenus=grepl("^rhenus | rhenus | rhenus$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Vos Logistics Oss
    VosLogistics=grepl("(?=.*^vos | vos | vos$)(?=.*(logistics|oss|wim))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Vos Deventer
    VosDeventer=grepl("(?=.*^vos | vos | vos$)(?=.*(deventer|b.v.))",keyword,ignore.case=TRUE,perl=TRUE),

    ## R Vos Schaijk
    RVosSchaijk=grepl("(?=.*^vos | vos | vos$)(?=.*(schaijk|^r | r | r$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## De Vos Transport
    DeVosTransport=grepl("(?=.*(^vos | vos | vos$|de vos))(?=.*(transportbedrijf|transportonderneming|nederhemert|zaltbommel))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Mol Cargo
    MolCargo=grepl("(?=.*^mol | mol | mol$)(?=.*(cargo|tiel|transport))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Mol Power
    MolPower=grepl("(?=.*^mol | mol | mol$)(?=.*(power|track|ship|mitsui|tilburg|line))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Gheys Transport Mol
    GheysTransport=grepl("gheys|(?=.*^mol | mol | mol$)(?=.*gheys)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Deuveren Transport
    VanDeuverenTransport=grepl("(?=.*deuveren)((?!(dedemsvaart|ijsselmuiden|nunspeet|hout|kampen)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## MSC
    MSC=grepl("^msc | msc | msc$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Versteijnen
    Versteijnen=grepl("versteijnen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Cosco
    Cosco=grepl("cosco",keyword,ignore.case=TRUE,perl=TRUE),

    ## Geodis
    Geodis=grepl("geodis",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Heijden Transport
    VDHTransport=grepl("(?=.*heijden)(?=.*(transport|logistiek))((?!(ridderkerk|^g | g |container|tiel)).)*$|(?=.*heijden)(?=.*(hapert|bladel))((?!(container|tiel)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Heijden Transport
    GvdHeijdenTransport=grepl("(?=.*(^g | g ))(?=.*heijden)((?!(container)).)*$|(?=.*heijden)(?=.*ridderkerk)((?!(container|tiel)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Maersk
    Maersk=grepl("^maersk | maersk | maersk$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Schenker
    Schenker=grepl("^schenker | schenker | schenker$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van den Brink Transport
    VanDenBrinkTransport=grepl("(?=.*^brink | brink | brink$)(?=.*transport)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van den Brink Transport
    VanDenBrinkTransport=grepl("(?=.*^cma | cma | cma$)",keyword,ignore.case=TRUE,perl=TRUE),

    ## De Jongh Transport
    GebrDeJongh=grepl("jongh",keyword,ignore.case=TRUE,perl=TRUE),

    ## De Jong Transport
    DeJongTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*transport)((?!(^r | r | r$|^g | g | g$|^leo | leo | leo$|grauss|leerbroek|^a | a | a$)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## R de Jong Transport
    RDeJongTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*(^r | r | r$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## G de Jong Transport
    GDeJongTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*(^g | g | g$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Leo de Jong Transport
    LeoDeJongTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*(^leo | leo | leo$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## De Jong-Grauss Transport
    DeJongGraussTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*grauss)",keyword,ignore.case=TRUE,perl=TRUE),

    ## A de Jong Transport
    ADeJongTransport=grepl("(?=.*(^jong | jong | jong$))(?=.*(^a | a | a$))|(?=.*(^jong | jong | jong$))(?=.*leerbroek)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Jan de Rijk Transport
    JanDeRijk=grepl("(?=.*rijk)(?=.*(^j | j | j$|jan))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Sluis Transport
    VanDerSluis=grepl("(?=.*^sluis | sluis | sluis$)(?=.*(van|vs|transport))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Visser Transport
    VisserTransport=grepl("(?=.*visser)(?=.*transport)((?!(vuren|brant|maasdijk)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Brant Visser Transport
    BrantVisserTransport=grepl("(?=.*visser)(?=.*brant)((?!(vuren|maasdijk|aalsmeer)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Visser Transport Maasdijk
    VisserTransportMaasdijk=grepl("(?=.*visser)(?=.*transport)(?=.*maasdijk)((?!(vuren|brant|aalsmeer)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## P Visser Transport
    PVisserTransport=grepl("(?=.*visser)(?=.*transport)(?=.*(^p | p | p$))((?!(vuren|brant|aalsmeer|maasdijk)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## VCK
    VCK=grepl("^vck | vck | vck$",keyword,ignore.case=TRUE,perl=TRUE),

    ## ANL
    ANL=grepl("^anl | anl | anl$",keyword,ignore.case=TRUE,perl=TRUE),

    ## CGM
    CGM=grepl("^cgm | cgm | cgm$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Evergreen
    Evergreen=grepl("evergreen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Goes
    Goes=grepl("goes",keyword,ignore.case=TRUE,perl=TRUE),

    ## Heezik
    Heezik=grepl("heezik",keyword,ignore.case=TRUE,perl=TRUE),

    ## Hyundai
    Hyundai=grepl("hyundai",keyword,ignore.case=TRUE,perl=TRUE),

    ## Maas Transport
    MaasTransport=grepl("(?=.*maas)(?=.*(transport|frans))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Yang Ming Tracking
    YangMingTracking=grepl("(?=.*yang)(?=.*(ming))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Simon Post transport
    SimonPostTransport=grepl("(?=.*simon)(?=.*(^post | post | post$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Marcel Post Transport
    MarcelPostTransport=grepl("(?=.*marcel)(?=.*(^post | post | post$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## HN Post Transport
    HNPostTransport=grepl("(?=.*(^hn | hn | hn$))(?=.*(^post | post | post$))|(?=.*^post | post | post$)(?=.*transport)((?!(simon|marcel|kogeko|sneek)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Post Kogeko Transport
    PostKogekoTransport=grepl("(?=.*kogeko)(?=.*(^post | post | post$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Post Transport Sneek
    PostTransportSneek=grepl("(?=.*sneek)(?=.*(^post | post | post$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Redder Transport
    RedderTransport=grepl("(?=.*redder)(?=.*transport)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Rotra
    Rotra=grepl("rotra",keyword,ignore.case=TRUE,perl=TRUE),

    ## A van Veen
    AVanVeen=grepl("(?=.*veen)(?=.*(^a | a | a$|helder))",keyword,ignore.case=TRUE,perl=TRUE),

    ## M van Veen
    MVanVeen=grepl("(?=.*veen)(?=.*(^m | m | m$|marjo))",keyword,ignore.case=TRUE,perl=TRUE),

    ## In t Veen
    InTVeen=grepl("(?=.*veen)(?=.*(^in | in | in$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Veen
    VanDerVeen=grepl("(?=.*veen)(?=.*van)(?=.*(^der | der | der$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Verhoeven Logistics
    VerhoevenLogistics=grepl("(?=.*verhoeven)(?=.*(logistics|uden))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Verhoeven Transport
    VerhoevenTransport=grepl("(?=.*verhoeven)(?=.*(transport|ridderkerk))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Verhoeven Wijnexpediteurs
    VerhoevenWijnexpediteurs=grepl("(?=.*verhoeven)(?=.*(expediteur|tilburg))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Verhoeven Waalre
    VerhoevenWaalre=grepl("(?=.*verhoeven)(?=.*waalre)",keyword,ignore.case=TRUE,perl=TRUE),

    ## VlastuinTransport
    VlastuinTransport=grepl("vlastuin",keyword,ignore.case=TRUE,perl=TRUE),

    ## VanVlietTransport
    VanVlietTransport=grepl("^vliet | vliet | vliet$",keyword,ignore.case=TRUE,perl=TRUE),

    ## C Vreugdenhil
    CVreugdenhil=grepl("(?=.*(^vreugdenhil | vreugdenhil | vreugdenhil$))(?=.*(^c | c | c$|maasdijk))|(?=.*(^vreugdenhil | vreugdenhil | vreugdenhil$))(?=.*(^transport | transport | transport$))((?!(^m | m | m$|^t | t | t$|lier)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## M Vreugdenhil
    MVreugdenhil=grepl("(?=.*(^vreugdenhil | vreugdenhil | vreugdenhil$))(?=.*(^m | m | m$))",keyword,ignore.case=TRUE,perl=TRUE),

    ## T Vreugdenhil
    TVreugdenhil=grepl("(?=.*(^vreugdenhil | vreugdenhil | vreugdenhil$))(?=.*(^t | t | t$|lier))",keyword,ignore.case=TRUE,perl=TRUE),

    ## Yusen Logistics
    YusenLogistics=grepl("yusen",keyword,ignore.case=TRUE,perl=TRUE),


## Groeperen van provincies
    
## Groeperen van gemeentes
    
    ## einde
    
    ## irrelevant
        
    irrelevant=grepl("^opleiding | opleiding | opleiding$|(?=.*deuveren)(?=.*(dedemsvaart|ijsselmuiden|nunspeet|hout|kampen))|houthandel|makelaar|(?=.*(brandsma|brink|bennink))(?=.*roden)|verhui|failli|vacature|huys",keyword,ignore.case=TRUE, perl=TRUE)
    
    )

## (?=.*faster)(?=.*than)(?=.*light) --> 101

## (?=.*ftl)(?=.*(advanced|edition|android|mod|cheat|review|multi|player|game|guide|ipad|stasis|pod|save|stuck|screen|error|loadout|editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|achiev|favorite|tier|type|secret|play|ware|shipping|download|ship|wiki)) --> 908

## (?=.*ftl)(?=.*advanced|edition|android|mod|cheat|review|multi|player|game|guide|ipad|stasis|pod|save|stuck|screen|error|loadout|editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|achiev|favorite|tier|type|secret|play|ware|shipping|download|ship|wiki) --> 140

## (?=.*editors)(?=.*light|lyrics|love|end|start|wiki|download) --> 13
## (?=.*editors)(?=.*(light|lyrics|love|end|start|wiki|download)) --> 93

## mp3|crack|no sound but wind|young scooter|sheeran|edith piaf|transport tycoon|18 wheels of steel|lyrics|truckload of trouble|playstation|xbox|game|play|^tlc | tlc | tlc$|acorn|ps4|gta5|sale|edison intel|transporter 3|html editors|eddie murphy|tattoo|edison|slug|wikileaks|zoo|vvd|edith|series|transporter 2|transporter 4 --> 2517





## reshape

## irrelevante wooren eruit halen
keywordsProcessed2 <- filter(keywordsProcessed, irrelevant==FALSE)

kwmelt <- melt(keywordsProcessed,id.vars=c("keyword"),measure.vars=names(keywordsProcessed[,2:ncol(keywordsProcessed)]),variable.name="tag",value.name="yesno")

cleanmelt <- filter(kwmelt, yesno==TRUE)

cleanmelt <- select(cleanmelt, keyword, tag)


## long naar wide
data.wide <- dcast(cleanmelt, keyword ~ tag, value.var="tag")


## export unique words
write.csv(unique(cleanmelt$keyword), "taggedwordsunique.csv", row.names=FALSE, col.names=FALSE)



