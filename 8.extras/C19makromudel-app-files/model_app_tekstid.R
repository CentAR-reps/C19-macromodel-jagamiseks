

sissejuhatus <- '

RITA-C19-makromudel on projekti "COVID-19ga seotud tegevuspiirangute sotsiaalse 
ja majandusliku mõju ja riigi kriisimeetmete tõhususe hindamine elanikkonna 
sotsiaalmajandusliku heaolu maksimeerimiseks" käigus välja töötatud simulatsioonimudel, 
mis hindab koroonaviiruse või mõne muu šoki/riigipoolse sekkumise tagajärjel 
lõppkasutamises toimunud muutuse mõju Eesti majandusele.

'



kasutamine <- '

# Mudeli kasutamine

## Üldine lähenemine

Mudeli kasutamiseks tuleb läbi käia järgmised etapid: 

1. Valmista ette simulatsioonistsenaarium(id) ehk otsusta, milliste tegevusalade 
toodangu/teenuste ja millist tüüpi lõppkasutamine muutub, või milline maksumäär muutub.

2. Kui hindame maksumuudatuste mõju kodumajapidamiste lõpptarbimisele, siis vali 
milliseid elastsusi tuleks kasutada:

    -   "Nullid" - maksudest tulenev hinnatõus kodumajapidamiste tarbimist ei mõjuta;
    
    -   "Miinusühed" - iga maksutõususust tulenev 1% hinnatõus toob kaasa 1% 
    lõpptarbimise vähenemise;
    
    -   "Ekspert" - Eesti andmete ja kirjanduse analüüsi põhjal kokku pandud hinnaelastsused, 
    eksperteadmiste põhjal loodud nn "parim pakkumine".
    
3. Viimase sisulise valikuna tuleb otsustada, kas kasutada Leontiefi I või II tüüpi 
multiplikaatoreid:

    -   "I" - esimest tüüpi multiplikaatorid. Sisaldavad otseseid ja kaudseid efekte.
    Konservatiivne hinnang muudatuste mõjule. Võtab arvesse vaid seda, et lõppkasutamisse minevate 
    toodete/teenuste tootmiseks on vaja ka vahetarbimist. 

    -   "II" - teist tüüpi multiplikaatorid. Need võtavad arvesse nii otseseid, 
    kaudseid kui tingitud efekte (kui otseste ja kaudsete efektide tulemusena 
    saavad inimesed tööd ja nende tarbimisvõimekus suureneb, siis II tüüpi 
    multiplikaator püüab arvesse võtta ka seda, et see lisandunud tarbimisvõimekus realiseeritakse. 
    Selle multiplikaatori tüübi valimisel tuleks sisestada säästumäär. 
    Seejärel suunatakse sellega korrigeeritud netopalgad tarbimisse ja arvutatakse 
    selle abil tingitud efektid (tulemus on otseste, kaudsete ja tingitud efektide summa).
    
4. Mudeli jooksutamiseks tuleb vajutada *Arvuta tulemused* nuppu. Kasutatud 
simulatsioonistsenaariumi ning mudeli tulemused saab vajadusel alla laadida.




## Simulatsioonistsenaariumi(te) ettevalmistamine


#### Eeldefineeritud stsenaariumid

Mudeliga on kaasas mõned eelnevalt defineeritud stsenaariumid, millega suurendatakse 
100 000 euro võrra:  

- "Kodumajapidamiste lõpptarbimist"  
- "Valitsussektori lõpptarbimist"  
- "Investeeringuid"  
- "Eksporti"  

Lisaks on valik "Kõiki eelpool mainituid koos", millega on võimalik kõik eelnevad 
stsenaariumid korraga läbi jooksutada. 

Kõikide eeldefineeritud stsenaariumide puhul on selle nimele vastavas lõppkasutamise komponendis 
suurendatud lõppkasutamist 100 000 euro võrra (proportsionaalselt majandustegevusalade 
suurusele).  

Lisaks stsenaariumi valimisele on vajalik määratleda kasutatavad elastsused ning 
multiplikaatorite tüüp (vt *Üldine loogika*).

Eeldefineeritud stsenaariumid on mõeldud eelkõige näidistena, mis aitavad kasutajal 
endal vajaduspõhiseid stsenaariume koostada. Näidisstsenaariumeid 
on võimalik alla laadida ning kasutada neid alusena enda stsenaariumite loomisel 
(valik *Lae oma stsenaarium*).


#### Defineeri stsenaarium

Valik võimaldab hinnata konkreetse tegevusala lõppkasutamise või hõive muutusega 
seonduvat mõju majandusele. Lisaks (lõppkasutamisest ja hõivest 
eraldiseisvalt) on võimalik hinnata maksumuudatustega seonduvat mõju.

- Lõppkasutamise muutus:
    - Vali tegevusala, millele lõppkasutamise muutusega kaasnevat mõju 
    soovid hinnata.  
    - Vali lõppkasutamise komponent, mida soovid muuta.  
    - Vali muutuse summa (eurodes).  
- Hõive muutus:
    - Vali tegevusala, millele hõive muutusega kaasnevat mõju 
    soovid hinnata.    
    - Vali hõive muutus (mitu inimest lisandub või lahkub).
- Maksumäärade muutus:
    - Sisesta uued maksumäärad.


Lisaks stsenaariumi defineerimisele on vajalik määratleda 
multiplikaatorite tüüp (vt *Üldine loogika*) ning maksumäärade muutuse korral ka
kasutatavad elastsused.

Valiku abil on võimalik luua väga kitsa fookusega stsenaariume (ühe konkreetse 
majandustegevusala hindamiseks). Keerulisemad stsenaariumid tuleks koostada Exceli 
failina ning kasutada valikut *Lae oma stsenaarium*.


#### Lae üles oma stsenaarium

Keerulisemate stsenaariumite hindamisel, mille puhul soovitakse muuta mitme tegevusala 
lõppkasutamist (ja/või hõivet) või mitut lõppkasutamise komponenti samaaegselt, 
on otstarbekas defineerida stsenaarium Exceli failina ning see mudeli jooksutamiseks 
üles laadida. Tuleb tähele panna, et üles laetava stsenaariumi fail oleks korrektse struktuuriga. 
Seetõttu on mõistlik kasutada stsenaariumi alusena allalaetud eeldefineeritud 
stsenaariumit (vt *Eeldefineeritud stsenaariumid*). Stsenaariumi faili puhul on 
oluline silmas pidada järgmisi asjaolusid:  

- Iga tulp sisendandmete failis on eelnevast ja järgnevast sõltumatu, seega saab 
ühe faili abil sisestada korraga ka mitu erinevat stsenaariumi (need peaks paiknema 
erinevates tulpades). Samas võib tulpades esitada ka ühte simulatsioonistsenaariumit, 
mis on jaotatud üle aja (sellisel juhul tähistaks iga tulp kas kuud või aastat 
ning lahtrites oleks sellel perioodil planeeritud muutused lõppkasutuses/maksumäärades). 
Oluline on siiski silmas pidada, et tulpades olevat infot käsitletakse ka siis 
ikkagi sõltumatuna - kui simuleerida nt üle 2 aasta kestvat programmi, siis 
esimese aasta tulemused meie mudelis teist aastat kuidagi ei mõjuta.

- Stsenaariumi kirjelduses peavad olema kõik näidisfailis toodud lehed, seda isegi 
siis, kui sisuline info on ainult ühel lehel - ülejäänud peavad olema olemas, 
need tuleb lihtsalt tühjaks jätta.

- Ka tulpade päised peavad olema kõigil lehtedel ühesugused. See, mida tulpade 
päistesse kirjutada, on kasutaja otsustada - see võiks olla miski, mis aitab 
simulatsioonistsenaariumeid tulemusi kajastavatel joonistel hästi eristada.

- Üks stsenaarium võib sisaldada muutusi erinevates lõppkasutamise komponentides 
(ehk siis samas tulbas võivad olla lõppkasutamise muutused nii kodumajapidamiste 
tarbimise lehel kui nt investeeringute lehel). Samas võib olla selguse huvides 
ikkagi parem analüüsida erinevate sekkumiste mõjusid erinevate stsenaariumitena 
(ehk siis erinevates tulpades) - nii tekib parem ettekujutus, kui suur osa mõjust 
tuleb nt investeeringute ja kui suur ekspordi või kodumajapidamiste tarbimise 
muutustest.


Pärast faili üleslaadimist ja enne mudeli jooksutamist tuleb jällegi 
määratleda multiplikaatorite tüüp ning (makusmäärade muutuse korral) kasutatavad 
elastsused (vt *Üldine loogika*).




#### Ekspordikanalite mõju

Valik võimaldab konstrueerida stsenaariumi, mis lähtub mingi ekspordikanali muutusest. 
Ekspordi muutust saab defineerida lähtuvalt konkreetse majandustegevusala konkreetse
ekspordisihtkoha nõudluse muutusest või mõne riigi (või riigi majandustegevusala) 
koguimpordimahu muutusest. Esimene võimalus mõjutab peaasjalikult valitud sektori lõpptarbimist, teine 
kõiki määratud riiki (või riigi majandustegevusalasse) eksportivaid tegevusalasid.

Ekspordi baastasemeks on 2019. aasta.

Stsenaariumi tüübid: 

- *Eesti ekspordi muutus* - võimaldab defineerida muutusi konkreetse Eesti
tegevusala ekspordis konkreetsesse riiki. Eeldab tegevusala ja sihtriigi valikut.
    
- *Sihtriigi impordi muutus* - võimaldab defineerida muutusi konkreetse sihtriigi või 
sihtriigi majandustegevusala nõudluses, mõjutades seeläbi kõiki sinna eksportivaid 
tegevusalasid. Eeldab sihtturu taseme (sihtriigi koguimport või sihtriigi kindla 
tegevusala toodete/teenuste import) valikut ning sihtriigi (ja juhul kui tase on 
riik-majandusharu, siis ka majandusharu) valikut.

Võimalik on määrata *väliskaubanduse tüübina* eraldi kaubad, teenused või mõlemad
koos.

*Sisendi tüübina* on võimalik kasutada suhtelist muutust (baastaseme ehk 2019. aasta suhtes), 
absoluutset muutust (baastaseme ehk 2019. aasta suhtes) või absoluuttaset. Suhteline
muutus tuleb sisestada osakaaluna (17% muutuse puhul 0.17).


Juhul, kui lisaks ekspordi lõppkasutamise muutusele soovitakse hinnata ka muude
lõppkasutamise komponentide muutusest tingitud mõjusid, on võimalik defineeritud 
ekspordistsenaarium alla laadida ning pärast selle täiendamist see *Lae oma stsenaarium* 
valiku kaudu uuesti hindamiseks üles laadida.

Kui teisi lõppkasutamise komponente muuta vaja ei ole, saab defineeritud stsenaariumi
ka koheselt mudelist läbi jooksutada (eelnevalt tuleb määrata kasutatavate 
multiplikaatorite tüüp).


# Metoodika lühikirjeldus


Lühidalt kokku võttes läbib mudel järgmised sammud:

1. Kasutaja defineerib stsenaariumi, mis sisaldab kas maksumäärade (sotsiaalmaks, 
töötuskindlustusmaksed või käibemaks) muutusi, muutusi lõppkasutamises või muutusi 
töötajate arvus.

2. Kui stsenaariumi sisaldab maksumuudatusi, siis leitakse esmalt maksumuudatuste 
mõju lõppkasutamisele. Selleks:

    - Leitakse, kui suur on nominaalsete maksumäärade protsentuaalne suurenemine.
    
    - Eeldatakse, et sama palju suureneb ka maksutulu.
    
    - Leitakse maksutulu suurenemise osakaal kogutoodangus ning seda käsitletakse 
    maksude suurenemisest tingitud hinnatõusuna (eeldades, et kogu maksukoormuse 
    muutus kantakse otse üle).
    
    - Seejärel leitakse kodumajapidamiste nõudluse hinnaelastsuste abil muutus 
    kodumajapidamiste lõpptarbimises.
    
    - Käibemaksu puhul on lähenemine veidi teistsugune (vt täpsemalt 
    [maksumooduli kirjeldusest](https://raportid.centar.ee/2022_RITAC19_metoodikaraport.html#9_Maksumoodul)).

3. Maksumuudatustest tulenev lõppkasutamise muutus liidetakse stsenaariumi 
kirjelduses ette antud lõppkasutamise muutustele (eeldusel, et stsenaariumi 
kirjeldus üldse mingit täiendavat lõppkasutamise muutust ette näeb, üldjuhul 
soovitame samaaegselt lõppkasutamise muutust ja maksumuudatusi mitte modelleerida).

4. Lõppkasutamise muutustest võetakse maha käibemaks (erandiks on eksport, mis 
on käibemaksu vaba) ja impordi osa lõppkasutamises

5. Alles jäänud muudatused lõppkasutamises korrutatakse läbi tootmise 
multiplikaatoritega, saades mõju kogutoodangule.

6. Lisandväärtuse, tööjõukulu ja kasumi mõju saadakse, võttes arvesse vastavate 
näitajate osakaalu kogutoodangus (majandustegevusalade lõikes).

7. Mõju maksutulule leitakse iga maksu maksubaasi ning efektiivsete maksumäärade 
korrutisena (käibemaksu puhul on siin jällegi erandid). Kui muutusid maksumäärad, 
siis avaldub selle mõju läbi kahe kanali:

    - Maksumäära muutusest tulenev lõpptarbimise muutus - seda juba eelnevalt 
    kirjeldati. Maksutõus toob meie mudelis kaasa lõpptarbimise vähenemise ning 
    see mõjutab maksude laekumist negatiivselt.
    
    - Maksumäära muutusest tulenev otsene maksutulu muutus - kui nominaalne 
    maksumäär muutub, siis muutub protsentuaalselt sama palju ka maksutulu. 
    Näiteks, kui maksumäär suureneb 10%, siis suureneb ka kogu maksutulu 10%.
    
    - Lõplik mõju maksude laekumisele on nende efektide summa.
    

Täpsemalt saab mudeli kohta lugeda [mudeli metoodikaraportist](https://raportid.centar.ee/2022_RITAC19_metoodikaraport.html). 
Ekspordistsenaariumi defineerimise kohta saab täpsemat teavet 
[välisnõudluse raportist](https://raportid.centar.ee/2022_RITAC19_valisnoudlus.html).

'
