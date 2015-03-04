

## dependencies

library(dplyr)
library(WriteXLS)
library(reshape2)


## keywords laden

keywords <- read.csv("bfgl-kwpo.csv", sep=";") %>%
  filter(relevant!="nee") %>%
  select(keyword,volume)



## frequentietabel maken

woorden <- paste(keywords$keyword, collapse=" ")
uniekeWoorden <- strsplit(woorden, " ")[[1]]
woordFreq <- as.data.frame(table(uniekeWoorden)) %>%
  arrange(desc(Freq))

write.csv(woordFreq, "woordFreq.csv")

rm(uniekeWoorden, woorden, woordFreq)


## Keywords processem

keywordsProcessed <- keywords %>%
  
  mutate(
    
    ## 1. transport excl. transporteur
    transport=grepl("(?=.*transport)((?!transporte).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## 2. Container
    container=grepl("container",keyword,ignore.case=TRUE),
    
    ## 3. logistic
    logistic=grepl("logistic",keyword,ignore.case=TRUE),
    
    ## 4. tracking
    track=grepl("(?=.*track)((?!trace).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 5. track and trace
    trackAndTrace=grepl("(?=.*track)(?=.*trace)",keyword,ignore.case=TRUE, perl=TRUE),
    
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
    logistiek=grepl("logistiek",keyword,ignore.case=TRUE, perl=TRUE),
    
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
    haven=grepl("(?=.*haven)((?!lucht).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 19. koeltransport
    koeltransport=grepl("(?=.*koel)(?=.*transport)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 20. Sail
    sail=grepl("^sail | sail | sail$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 21. Sailing
    sailing=grepl("sailing",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 22. naar
    naar=grepl("^naar | naar | naar$|^to | to | to$",keyword,ignore.case=TRUE,perl=TRUE),
    
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
    
    ## 54. Cube
    cube=grepl("cube",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 55. Forwarding
    forwarding=grepl("forwarding",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 56. High
    high=grepl("high",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 57. Management
    management=grepl("management",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 58. Online
    ## online=grepl("online",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 59. Schedule
    schedule=grepl("schedule",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 60. Tracing
    tracing=grepl("tracing",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 61. Vracht
    vracht=grepl("^vracht | vracht | vracht$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 62. Vrachtwagen
    vrachtwagen=grepl("vrachtwagen",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 63. Weg
    weg=grepl(" weg|^weg",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 64. zwaar
    zwaar=grepl("zwaar",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 65. dieplader
    dieplader=grepl("dieplader",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 66. Fleet
    fleet=grepl("^fleet | fleet | fleet$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 67. Goederen
    goederen=grepl("goederen",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 68. HC
    HC=grepl("^hc | hc | hc$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 68. Intermodaal
    intermodaal=grepl("intermodaal",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 69. opslag
    opslag=grepl("opslag",keyword,ignore.case=TRUE,perl=TRUE),
      
    ## 70. Pallet
    pallet=grepl("pallet",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 71. Scheepvaart
    scheepvaart=grepl("scheepvaart",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 72. Specialist
    specialist=grepl("specialist",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 73. Tanktransport
    tanktransport=grepl("tanktransport",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 74. world
    world=grepl("^world | world | world$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 75. zeecontainer
    zeecontainer=grepl("zeecontainer",keyword,ignore.case=TRUE,perl=TRUE),
    
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

    ## Denemarken
    Denemarken=grepl("Denemarken|Denmark",keyword,ignore.case=TRUE,perl=TRUE),

    ## Scandinavie
    Scandinavie=grepl("Scandinavi",keyword,ignore.case=TRUE,perl=TRUE),

    ## Zweden
    Zweden=grepl("Zweden|Sweden",keyword,ignore.case=TRUE,perl=TRUE),

    ## Zwitserland
    Zwitserland=grepl("Zwitser|Swiss|Switser",keyword,ignore.case=TRUE,perl=TRUE),

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

    ## Jo van Beek
    JoVanBeek=grepl("(?=.*beek)(?=.*jo)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Beek Sneltransport
    VanBeekSneltransport=grepl("(?=.*beek)(?=.*snel)(?=.*transport)",keyword,ignore.case=TRUE,perl=TRUE),

    ## AC van Beek
    ACVanBeek=grepl("(?=.*(^AC | AC | AC$))(?=.*beek)|(?=.*(^beek | beek | beek$))(?=.*transport)((?!(snel|jo|uden)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Bentum Transport
    VanBentumTransport=grepl("(?=.*bentum)((?!barneveld).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## J van Bentum Transport
    JVanBentumTransport=grepl("(?=.*bentum)(?=.*barneveld)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Bos Logistics
    BosLogistics=grepl("(?=.*(^bos | bos | bos$))(?=.*schiphol)",keyword,ignore.case=TRUE,perl=TRUE),

    ## EuserTransport
    EuserTransport=grepl("^euser | euser | euser$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Greving Transport
    GrevingTransport=grepl("greving",keyword,ignore.case=TRUE,perl=TRUE),

    ## Breewel Transport
    GrevingTransport=grepl("breewel",keyword,ignore.case=TRUE,perl=TRUE),

    ## Hereijgers Transport
    HereijgersTransport=grepl("hereijgers",keyword,ignore.case=TRUE,perl=TRUE),

    ## Mitsui
    Mitsui=grepl("mitsui",keyword,ignore.case=TRUE,perl=TRUE),

    ## De Rijke Transport
    DeRijkeTransport=grepl("rijke",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Wal Transport
    VanDerWal=grepl("^wal | wal | wal$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van der Werff Transport
    VanDerWerff=grepl(" werff|^werff",keyword,ignore.case=TRUE,perl=TRUE),

    ## Wezenberg Transport
    WezenbergTransport=grepl("wezenberg",keyword,ignore.case=TRUE,perl=TRUE),

    ## Alblas Transport
    AlblasTransport=grepl("alblas",keyword,ignore.case=TRUE,perl=TRUE),

    ## Boers Transport
    BoersTransport=grepl("^boers | boers | boers$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Brouwer Transport
    BrouwerTransport=grepl("^boers | boers | boers$",keyword,ignore.case=TRUE,perl=TRUE),

    ## CSAV
    CSAV=grepl("csav",keyword,ignore.case=TRUE,perl=TRUE),

    ## Derix Transport
    DerixTransport=grepl("derix",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Dijk Transport
    VanDijkTransport=grepl("(?=.*van)(?=.*dijk)",keyword,ignore.case=TRUE,perl=TRUE),

    ## Essers Transport
    EssersTransport=grepl("essers",keyword,ignore.case=TRUE,perl=TRUE),

    ## VanDerGaag Transport
    VanDerGaagTransport=grepl("gaag",keyword,ignore.case=TRUE,perl=TRUE),

    ## Getru Transport
    GetruKoeltransport=grepl("getru",keyword,ignore.case=TRUE,perl=TRUE),

    ## Groenenboom Transport
    GroenenboomKoeltransport=grepl("groenenboom",keyword,ignore.case=TRUE,perl=TRUE),

    ## GVT
    GVT=grepl("gvt",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van de Kamp
    VanDeKampTransport=grepl("^kamp | kamp | kamp$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Opzeeland Transport
    OpzeelandTransport=grepl("opzeeland",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Reenen Transport
    VanReenenTransport=grepl("Reenen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Schouten Transport
    SchoutenTransport=grepl("schouten",keyword,ignore.case=TRUE,perl=TRUE),

    ## Speksnijder Transport
    SpeksnijderTransport=grepl("speksnijder",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Spreuwel Transport
    VanSpreuwelTransport=grepl("spreuwel",keyword,ignore.case=TRUE,perl=TRUE),

    ## Stam Transport
    StamTransport=grepl("stam",keyword,ignore.case=TRUE,perl=TRUE),

    ## Steinweg Transport
    SteinwegTransport=grepl("steinweg",keyword,ignore.case=TRUE,perl=TRUE),

    ## Veldhuizen Transport
    VeldhuizenTransport=grepl("veldhuizen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Vermeer Transport
    VermeerTransport=grepl("^vermeer | vermeer | vermeer$",keyword,ignore.case=TRUE,perl=TRUE),

    ## Van Zandbergen Transport
    VanZandbergenTransport=grepl("zandbergen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Zielman Transport
    ZielmanTransport=grepl("zandbergen",keyword,ignore.case=TRUE,perl=TRUE),

    ## Zijderhand Transport
    ZijderhandTransport=grepl("zijderhand",keyword,ignore.case=TRUE,perl=TRUE),

## Groeperen van provincies

## 1. Zuid-Holland
zuidholland=grepl("(?=.*zuid)(?=.*holland)",keyword,ignore.case=TRUE,perl=TRUE),

## 2. Noord-Holland
noordholland=grepl("(?=.*noord)(?=.*holland)",keyword,ignore.case=TRUE,perl=TRUE),

## 3. Noord-Brabant
noordbrabant=grepl("brabant",keyword,ignore.case=TRUE,perl=TRUE),

## 4. Overijssel
overijssel=grepl("(?=.*over)(?=.*(ijssel|ijsel))",keyword,ignore.case=TRUE,perl=TRUE),

## 5. Utrecht
utrecht=grepl("utrecht",keyword,ignore.case=TRUE,perl=TRUE),

## 6. Drenthe
drenthe=grepl("drente|drenthe",keyword,ignore.case=TRUE,perl=TRUE),

## 7. Friesland
friesland=grepl("(?=.*(frys|fries))(?=.*(lân|land))",keyword,ignore.case=TRUE,perl=TRUE),

## 8. Gelderland
gelderland=grepl("(?=.*gelder)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),

## 9. Limburg
limburg=grepl("limburg",keyword,ignore.case=TRUE,perl=TRUE),

## 10. Zeeland
zeeland=grepl("^zeeland | zeeland | zeeland$",keyword,ignore.case=TRUE,perl=TRUE),

## 11. Groningen
groningen=grepl("groningen",keyword,ignore.case=TRUE,perl=TRUE),

## 12. Flevoland
flevoland=grepl("(?=.*flevo)(?=.*land)",keyword,ignore.case=TRUE,perl=TRUE),
    
## Groeperen van gemeentes

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
Laarbeek=grepl("Laarbeek|(?=.*beek)(?=.donk)",keyword,ignore.case=TRUE,perl=TRUE),

## Berkelland
Berkelland=grepl("Berkelland",keyword,ignore.case=TRUE,perl=TRUE),

## Leek
Leek=grepl("Leek",keyword,ignore.case=TRUE,perl=TRUE),

## Epe
Epe=grepl("^Epe | Epe | Epe$",keyword,ignore.case=TRUE,perl=TRUE),

## Eemsmond
Eemsmond=grepl("Eemsmond",keyword,ignore.case=TRUE,perl=TRUE),

## Bergen
Bergen=grepl("(?=.*(^Bergen | bergen | bergen$))((?!zoom).)*$",keyword,ignore.case=TRUE,perl=TRUE),

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

## S?dwestFrysl?n
## SudwestFryslan=grepl("(?=.*(S?dwest|Sudwest|Zuidwest))(?=.*(Frysl?n|Fryslan|Friesland))",keyword,ignore.case=TRUE,perl=TRUE),

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
Zundert=grepl("Zundert|hazeldonk",keyword,ignore.case=TRUE,perl=TRUE),

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
Montferland=grepl("Montferland|heerenberg",keyword,ignore.case=TRUE,perl=TRUE),

## Terschelling
Terschelling=grepl("Terschelling",keyword,ignore.case=TRUE,perl=TRUE),

## Simpelveld
Simpelveld=grepl("Simpelveld",keyword,ignore.case=TRUE,perl=TRUE),

## Westland
Westland=grepl("Westland|maasdijk|de lier|poeldijk",keyword,ignore.case=TRUE,perl=TRUE),

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
Beek=grepl("(?=.*(^Beek | Beek | Beek$))((?!(^van | van | van$)).)*$",keyword,ignore.case=TRUE,perl=TRUE),

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
Lansingerland=grepl("Lansingerland|Bleiswijk",keyword,ignore.case=TRUE,perl=TRUE),

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
Kampen=grepl("Kampen|ijsselmuiden",keyword,ignore.case=TRUE,perl=TRUE),

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
Smallingerland=grepl("Smallingerland|drachten",keyword,ignore.case=TRUE,perl=TRUE),

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
Zoetermeer=grepl("Zoetermeer",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## irrelevant
        
    irrelevant=grepl("^opleiding | opleiding | opleiding$|(?=.*deuveren)(?=.*(dedemsvaart|ijsselmuiden|nunspeet|hout|kampen))|houthandel|makelaar|(?=.*(brandsma|brink|bennink))(?=.*roden)|verhui|failli|vacature|huys|college|cursus|mbo",keyword,ignore.case=TRUE, perl=TRUE)

)


## Data reshapen

keywordsLong <- keywordsProcessed %>%
  filter(irrelevant==FALSE) %>%
  melt(id.vars=c("keyword","volume"),
  measure.vars=names(keywordsProcessed[,3:ncol(keywordsProcessed)]),
  variable.name="tag",value.name="yesno") %>%
  filter(yesno==TRUE) %>%
  select(keyword,volume,tag)

write.csv(keywordsLong,"tableau.csv")

## frequentietabel maken

as.data.frame(table(keywordsLong$tag)) %>%
  arrange(desc(Freq)) %>%
  write.csv("tagFreq.csv")

## Dimensies

## dimensies zijn voor deze klus handmatig gedaan in Excel.

tagsdims <- read.csv("tags-dims.csv", sep=";")

wordtagsdims <- merge(keywordsLong,tagsdims,by.x="tag", by.y="Tag")

## Types

## types zijn voor deze klus handmatig gedaan in Excel

dimstypes <- read.csv("dims-types.csv", sep=";")

wordtagsdimstypes <- merge(wordtagsdims,dimstypes,by.x="Dimensie", by.y="Dimensie")

## productwoorden ontdoen van verkeerde tags
