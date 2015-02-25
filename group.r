## dependencies

library(dplyr)
library(WriteXLS)
library(reshape2)


## keywords laden

keywords <- read.csv("bfgl.csv", sep=",") %>% select(keyword)


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

    ## 2. freight
    freight=grepl("freight",keyword,ignore.case=TRUE), 
    
    ## 3. vracht excl. vrachtwagen
    vracht=grepl("vracht |(?=.*vracht)((?!wagen).)*$",keyword,ignore.case=TRUE, perl=TRUE),    
    
    ## 4. logistic
    logistics=grepl("logistic|logistiek",keyword,ignore.case=TRUE),
    
    ## 5. companies
    company=grepl("compan",keyword,ignore.case=TRUE),
    
    ## 6. trucking
    trucking=grepl("trucking",keyword,ignore.case=TRUE),
    
    ## 7. shipping
    shipping=grepl("shipping",keyword,ignore.case=TRUE),
    
    ## 8. shipment
    shipment=grepl("shipment",keyword,ignore.case=TRUE),
    
    ## 9. ship
    ship=grepl("(?=.*ship)((?!ftl|shipper).)*$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 10. truck met trucking, trucker en load excluded
    truck=grepl("(?=.*truck)((?!trucking|trucker|load).)*$",keyword,
                ignore.case=TRUE, perl=TRUE),
    
    ## 11. ltl incl. less than truck load
    ltl=grepl("ltl|(?=.*less)(?=.*load)(?=.*truck)",keyword,ignore.case=TRUE, 
              perl=TRUE),
    
    ## 12. ftl incl. full truck load excl. android, cheats, ships, tips, ships, 
    ## review, multiplayer, advanced edition, game, guide, ipad, stasis, pod, 
    ## save, best, stuck, screen, error, loadout, editor, layout, cruiser, slug,
    ## mobile, strategy,creat,lock,zoltan,achiev,favorite,tier,type
    ftl=grepl("(?=.*full)(?=.*load)(?=.*truck)|(?=.*ftl)((?!android|tips|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play).)*$",keyword,ignore.case=TRUE, 
              perl=TRUE),
    
    ## 13. Container
    ltl=grepl("container",keyword,ignore.case=TRUE),
    
    ## 14. International
    international=grepl("internationa",keyword,ignore.case=TRUE),
    
    ## 15. load
    load=grepl("(?=.*load)((?!truck|less|full|ltl|ftl|loader).)*$",keyword,
               ignore.case=TRUE, perl=TRUE),
    
    ## 16. track
    track=grepl("track",keyword,ignore.case=TRUE),

    ## 17. service
    services=grepl("services",keyword,ignore.case=TRUE),
    
    ## 18.cargo
    cargo=grepl("cargo",keyword,ignore.case=TRUE),
    
    ## 19.cargo
    global=grepl("global",keyword,ignore.case=TRUE),
    
    ## 20.ltd
    ltd=grepl("ltd",keyword,ignore.case=TRUE),
    
    ## 21. trace
    trace=grepl("trace",keyword,ignore.case=TRUE),
    
    ## 22. track and trace
    trackandtrace=grepl("(?=.*track)(?=.*trace)",keyword,ignore.case=TRUE, 
              perl=TRUE),
    
    ## 22. jobs
    jobs=grepl("job",keyword,ignore.case=TRUE),
    
    ## 23. truckload
    truckload=grepl("(?=.*truckload|truck load)((?!loader).)*$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 24. forwarding
    forwarding=grepl("(?=.*forward|forwarding)((?!forwarder).)*$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 25. forwarder
    forwarder=grepl("(?=.*forwarder)((?!forwarding).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 26. transporter
    transporter=grepl("(?=.*transporter|transporteur)((?!android|tips|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 27. trucker
    trucker=grepl("(?=.*trucker)((?!android|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 28. shipper
    shipper=grepl("(?=.*shipper)((?!android|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 29. loader
    loader=grepl("(?=.*loader)((?!android|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 30. haulage
    haulage=grepl("haulage",keyword,ignore.case=TRUE),
    
    ## 31. hauler
    hauler=grepl("hauler|haulier",keyword,ignore.case=TRUE),
    
    ## 32. hauler
    canada=grepl("canada",keyword,ignore.case=TRUE),
    
    ## 33. UK
    uk=grepl("uk | uk|(?=.*united)(?=.*kingdom)",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 34. How to
    howto=grepl("how to|howto|(?=.*how)(?=.*do)((?!much|long).)*$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 35. How much
    howmuch=grepl("how much|(?=.*how)(?=.*much)",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 36. How long
    howlong=grepl("how long|(?=.*how)(?=.*long)",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 37. Free
    free=grepl("(?=.*free)((?!android|wiki|
               mods|cheat|ships|review|multi|player|advanced|edition|game|
               guide|ipad|stasis|pod|save|stuck|screen|error|loadout|
               editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|
               achiev|favorite|tier|type|secret|play|ware|shipping|download).)*$",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 38. Management
    management=grepl("management",keyword,ignore.case=TRUE, perl=TRUE),
    
    ## 39. Top
    top=grepl("(?=.*top | top)((?!rtop|ktop|stop|etop|topic).)*$",keyword,ignore.case=TRUE, perl=TRUE),  
    
    ## 40. Carrier
    carrier=grepl("(?=.*carrier)((?!carriere).)*$",keyword,ignore.case=TRUE, perl=TRUE), 
    
    ## 40. EDI (Electronic data interchange)
    edi=grepl("edi$|edi |(?=.*elec)(?=.*data)(?=.*inter)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 41. India
    india=grepl("(?=.*india| india|india )((?!indiana|indialinx).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 42. air
    air=grepl("(?=.*air| air|air )((?!repair|prairie|cairns|hair|horaires|commissionair|nairaland|nairobi).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 43. sea
    sea=grepl("(?=.*sea| sea|sea )((?!season|search|seattle|reseau|sears|sealing|disease|search|lease|seals|searcy|seat|chelsea).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 44. Europe
    europe=grepl("(?=.*europe)((?!european|europese).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 45. China
    china=grepl("(?=.*china)((?!chinashipping).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 46. European
    european=grepl("european",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 47. Europa
    europa=grepl("(?=.*europa)((?!europapark|europapalette|europallet|ec.|intereuropa|europatransport).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 48. Airport
    airport=grepl("airport",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 49. Airline
    airline=grepl("airline",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 50. Road
    road=grepl("(?=.*road)((?!roadrunner|railroad|abroad|roadie).)*$",keyword,ignore.case=TRUE,perl=TRUE),

    ## 51. Group
    group=grepl("(?=.*group)((?!groupage|groupon).)*$",keyword,ignore.case=TRUE,perl=TRUE),  
    
    ## 52. Groupage
    groupage=grepl("groupage",keyword,ignore.case=TRUE,perl=TRUE), 

    ## 53. Online
    groupage=grepl("online",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 54. Rotterdam
    rotterdam=grepl("rotterdam",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 55. feet
    feet=grepl("(?=.*ft | ft|feet)((?!ftl).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 56. rates
    rates=grepl("(?=.*rates| rate|rate )((?!refrigerated|strategy|integrated|rated|emirates|corporate|operate|strategic|accelerates|crate|celebrate|directorate).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 57. inc
    inc=grepl("(?=.*inc |inc. | inc | inc. | inc$| inc.$)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 58. list
    list=grepl("(?=.*^list | list | list$)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 59. usa
    usa=grepl("^usa | usa | usa$|(?=.*united)(?=.*states)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 60. nederland
    nederland=grepl("nederland",keyword,ignore.case=TRUE),
    
    ## 61. fedex
    fedex=grepl("fedex",keyword,ignore.case=TRUE),
    
    ## 62. vacature
    vacature=grepl("vacature",keyword,ignore.case=TRUE),
    
    ## 63. software
    software=grepl("software",keyword,ignore.case=TRUE),
    
    ## 64. broker
    broker=grepl("(?=.*broker)((?!brokerage|gebroken).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 65. brokerage
    broker=grepl("brokerage",keyword,ignore.case=TRUE),
    
    ## 66. industry
    industry=grepl("industry|industries",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 67. lcl
    lcl=grepl("^lcl | lcl | lcl$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 68. best
    best=grepl("(?=.*best)((?!besturing|bestellen|bestway|cargobest).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 69. board
    board=grepl("(?=.*board)((?!boarding|loadboard|onboard|adboard|longboard|scoreboard|nordboard).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 70. loadboard
    load=grepl("load board|loadboard",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 71. opleiding
    opleiding=grepl("opleiding",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 72. dimension
    dimension=grepl("dimension",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 73. system
    system=grepl("^system | system | system$|^systems | systems | systems$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 74. vessel
    system=grepl("vessel|vessels",keyword,ignore.case=TRUE,perl=TRUE),

    ## 75. business
    business=grepl("business",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 76. find
    find=grepl("^find | find | find$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 77. what is...
    whatis=grepl("(?=.*what)(?=.*is|are|mean|was|does|do|were)",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 78. association
    association=grepl("association",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 79. agent
    agent=grepl("^agent | agent | agent$",keyword,ignore.case=TRUE,perl=TRUE),

    ## 80. line
    line=grepl("(?=.*line)((?!kline|k-line|guideline|liner|infoline|lineup|linkline|stateline|ultraline|coxlines|discipline|foodline|euroline|hotline|loadline|mainline|mponline|online|crystalline|outline|riverline|ecu-line|uline).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 81. south
    south=grepl("(?=.*south)((?!hampton|east|north|west|southall|southland|southport|southwind|africa).)*$",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 82. south africa
    southafrica=grepl("south africa|south-africa",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## 83. flatbed
    flatbed=grepl("flatbed",keyword,ignore.case=TRUE,perl=TRUE),
    
    ## einde
    
    ## irrelevant
    
    irrelevant=grepl("sale|(?=.*faster)(?=.*than)(?=.*light)|(?=.*ftl)(?=.*android|wiki|mods|cheat|ships|review|multi|player|advanced|edition|game|guide|ipad|stasis|pod|save|stuck|screen|error|loadout| editor|layout|cruiser|slug|mobile|strategy|creat|lock|zoltan|achiev|favorite|tier|type|secret|play|ware|shipping|download| ^tlc | tlc | tlc$)",keyword,ignore.case=TRUE, perl=TRUE)
    
    )



## syntax voor bevat wel + bevat niet
"(?=.*transport)((?!compan).)*$"




## reshape

kwmelt <- melt(keywordsProcessed,id.vars=c("keyword"),measure.vars=names(keywordsProcessed[,2:ncol(keywordsProcessed)]),variable.name="keyword",value.name="tags")