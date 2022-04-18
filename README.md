# RITA-C19-makromudel

## Sissejuhatus ja lühike metoodika kirjeldus

RITA-C19-makromudel on projekti "COVID-19ga seotud tegevuspiirangute sotsiaalse ja majandusliku mõju ja riigi kriisimeetmete tõhususe hindamine elanikkonna sotsiaalmajandusliku heaolu maksimeerimiseks" käigus välja töötatud simulatsioonimudel, mis hindab koroonaviiruse või mõne muu šoki/riigipoolse sekkumise tagajärjel lõppkasutamises toimunud muutuse mõju Eesti majandusele.

Lõppkasutamise muutus võib olla otse mudelisse sisestatud muutus ühes järgnevas lõppkasutamise komponendis:

-   kodumajapidamiste lõpptarbimine

-   investeeringud

-   valitsussektori kulutused

-   eksport

Lisaks sellele on käesolevas mudelis võimalik muutusi lõppkasutamises esile kutsuda ka läbi maksumäärade muutmise. Sellisel juhul arvutatakse mõju läbi kodumajapidamiste lõpptarbimise nõudluse hinnaelastsuste. Teisi lõppkasutamise komponente maksumuudatused käesolevas mudelis ei mõjuta. Muuta on võimalik järgmiste maksude määrasid:

-   käibemaks tavamääras

-   käibemaks erimääras

-   sotsiaalmaks

-   töötuskindlustusmaksed (tööandja ja töötaja makse koondsummana)

Mudeli arvutab lõppkasutamise mõju järgmistele väljundnäitajatele:

-   kogutoodang

-   lisandväärtus

-   tööjõukulu

-   kasum

-   maksutulu (maksuliikide lõikes)

Metoodiliselt põhineb mudel sisend-väljundraamistikul, täpsemalt pakkumise ja kasutamise tabelite (Statistikaameti andmed aastast 2017) põhjal koostatud **tegevusala-tegevusala** sisendväljundtabelil ning sellelt arvutatud tootmise multiplikaatoritel. Simulatsiooni sisendiks olevad stsenaariumid esitatakse majandustegevusalade lõikes (väljundid on toores olekus samuti majandustegevusalade lõikes, kuid enamasti on neid mõistlik agregeerida (seda standardvaated ka teevad)).

Peamised sammud, mida mudeli arvutused läbivad, on järgmised:

-   Esmalt leitakse maksumuudatuste mõju lõppkasutamisele (eeldusel, et stsenaarium sisaldab maksumuudatusi) (NB! Siin ja edaspidi toimub kõik tegevusalade lõikes). Selleks:

    -   Leitakse, kui suur on nominaalsete maksumäärade protsentuaalne suurenemine

    -   Eeldatakse, et sama palju suureneb ka maksutulu

    -   Leitakse maksutulu suurenemise osakaal kogutoodangus ning seda käsitletakse maksude suurenemisest tingitud hinnatõusuna (eeldades, et kogu maksukoormuse muutus kantakse otse hinda üle)

    -   Seejärel leitakse kodumajapidamiste nõudluse hinnaelastsuste abil muutus kodumajapidamiste lõpptarbimises

    -   Käibemaksu puhul on lähenemine veidi teistsugune (vt täpsemalt maksumooduli kirjeldusest)

-   Maksumuudatustest tulenev lõppkasutamise muutus liidetakse stsenaariumiga ette antud lõppkasutamise muutustele (eeldusel, et selliseid on)

-   Lõppkasutamise muutustest võetakse maha käibemaks (erandiks on eksport, mis on käibemaksu vaba) ja impordi osa lõpptarbimises (NB! Impordi leke toimub ka vahetarbimises)

-   Järele jäänud muudatused lõpptarbimiseks korrutatakse läbi tootmise multiplikaatoritega saades mõju kogutoodangule

-   Lisandväärtuse, tööjõukulu ja kasumi mõju saadakse, võttes arvesse vastavate näitajate osakaalu kogutoodangus (jällegi sektorite lõikes)

-   Mõju maksutulule leitakse iga maksu maksubaasi ning efektiivsete maksumäärade korrutisena (käibemaksu puhul on siin jällegi erandid). Kui muutusid maksumäärad, siis avaldub selle mõju läbi kahe kanali:

    -   Maksumäära muutusest tulenev lõpptarbimise muutus - seda juba eelnevalt kirjeldati. Näiteks toob maksutõus meie mudelis kaasa lõpptarbimise vähenemise ning see mõjutab maksude laekumist negatiivselt.

    -   Maksumäära muutusest tulenev maksutulu muutus - kui nominaalne maksumäär muutub, siis muutub protsentuaalselt sama palju ka maksutulu. Näiteks, kui maksumäär suureneb 10%, siis suureneb ka maksutulu 10%.

    -   Lõplik mõju maksude laekumisele tuleb nende kahe efekti summast.

## Mudeli kasutamine

Mudeli kasutamine käib nii:

-   **Lae mudel Githubist alla ja vaata, et kõik vajalikud paketid (loetletud failis "1.skripts/packages.R") on installitud.**

-   **Valmista ette simulatsioonistsenaarium(id) ehk otsusta, milliste tegevusalade toodangu/teenuste ja millist tüüpi lõppkasutamine muutub, või milline maksumäär muutub.**

    -   Simulatsioonistsenaariumid sisestatakse mudelisse MS Excel formaadis oleva failina, näidised leiad kataloogist "4.user_input_examples".

    -   Iga tulp sisendandmete failis on eelnevast ja järgnevast sõltumatu, seega saab ühe faili abil sisestada korraga ka mitu erinevat stsenaariumi (need peaks paiknema erinevates tulpades). Samas võib tulpades esitada ka ühte simulatsioonistsenaariumit, mis on jaotatud üle aja (sellisel juhul tähistaks iga tulp kas kuud või aastat ning lahtrites oleks sellel perioodil planeeritud muutused lõppkasutuses/maksumäärades). Oluline on siiski silmas pidada, et tulpades olevat infot käsitletakse ka siis ikkagi sõltumatuna - kui simuleerida nt üle 2 aasta kestvat programmi, siis esimese aasta tulemused meie mudelis teist aastat kuidagi ei mõjuta.

    -   Tehnilise poole pealt olgu mainitud, et:

        -   Stsenaariumi kirjelduses peavad olema kõik näidisfailis toodud lehed, seda isegi siis, kui sisuline info on ainult ühel lehel - ülejäänud peavad olema olemas, need tuleb lihtsalt tühjaks jätta.

        -   Ka tulpade päised peavad olema kõigil lehtedel ühesugused. See mida tulpade päistesse kirjutada on kasutaja otsustada - see võiks olla miski, mis aitab simulatsioonistsenaariumeid tulemusi kajastavatel joonistel hästi eristada.

        -   Üks stsenaarium võib sisaldada muutusi erinevates lõppkasutamise komponentides (ehk siis samas tulbas võivad olla lõppkasutamise muutused nii kodumajapidamiste tarbimise lehel kui nt investeeringute lehel). Samas võib olla selguse huvides ikkagi parem analüüsida erinevate sekkumiste mõjusid erinevate stsenaariumitena (ehk siis erinevates tulpades) - nii tekib parem ettekujutus, kui suur osa mõjust tuleb nt investeeringute ja kui suur ekspordi või kodumajapidamiste tarbimise muutustest.

-   **Ava fail RUN.R, vajuta klahvikombinatsiooni Ctrl+A ja seejärel Ctrl+Enter. Selle tulemusena avaneb dialoogide jada, mis päädib mudeli käivitamisega.**

    -   Esmalt küsitakse mudeli kasutajalt simulatsioonistsenaariumit sisaldava faili asukohta

    -   Seejärel palutakse täpsustada, kas tulpades esitatud andmed on kuised või aastased (eeskätt mõjutab töötajate arvu arvutusi, mis tugineb ühe kogutoodangu ühiku tööjõusisaldusel ning on vaikimisi väljendatud tööaastates). Valikud:

        -   "aasta" - kui tegemist on aastaste andmetega või tegemist pole üldse aegreaga (tulpades on erinevad stsenaariumid)

        -   "kuu" - kui tegemist on kuiste andmetega

    -   Järgmine dialoog täpsustab, milliseid elastsusi tuleks kasutada, kui hindame maksumuudatuste mõju kodumajapidamiste lõpptarbimisele. Valikud:

        -   "nullid" - maksudest tulenev hinnatõus kodumajapidamiste tarbimist ei mõjuta

        -   "miinusühed" - iga maksutõususust tulenev 1% hinnatõus toob kaasa 1% lõpptarbimise vähenemise)

        -   "ekspert" - Eesti andmete ja kirjanduse analüüsi põhjal kokku pandud hinnaelastsused, nö analüütikute parim pakkumine

    -   Viimase sisulise valikuna tuleb otsustada, kas kasutada Leontiefi I või II tüüpi multiplikaatoreid. Valikud:

        -   "I" - esimest tüüpi multiplikaatorid. Sisaldavad otseseid ja kaudseid efekte. Soovitame kasutada neid, võtab arvesse vaid seda, et lõppkasutamisse minevate toodete/teenuste tootmiseks on vaja ka vahetarbimist. Konservatiivne hinnang muudatuste mõjule.

        -   "II" - teist tüüpi multiplikaatorid. Need võtavad arvesse nii otseseid, kaudseid kui tingitud efekte (kui otseste ja kaudsete efektide tulemusena saavad inimesed tööd ja nende tarbimisvõimekus suureneb, siis II tüüpi multiplikaator püüab arvesse võtta ka seda, et see tarbimisvõimekus realiseeritakse. Selle multiplikaatori tüübi valimisel tuleks sisestada säästumäär. Seejärel suunatakse sellega korrigeeritud netopalgad tarbimisse ja arvutatakse selle abil tingitud efektid (tulemus on otseste, kaudsete ja tingitud efektide summa).

-   **Tutvu simulatsiooni tulemustega. Simulatsiooni tulemused kirjutatakse objekti nimega "tulemused". See on list, millel on järgnevad elemendid:**

    -   tulemused$Joonised - element, mis sisaldab valmis kujul joonised stsenaariumi mõju kohta järgmistele näitajatele (nimed kõnelevad enamasti iseenda eest, viimane joonis jagab koondmaksutulu maksuliike lõikes laiali) (joonistel on majandustegevusalad agregeeritud):

        -   tulemused$Joonised$Koondtulemused

        -   tulemused$Joonised$Kogutoodang

        -   tulemused$Joonised$Lisandväärtus

        -   tulemused$Joonised$Tööjõukulu

        -   tulemused$Joonised$Kasum

        -   tulemused$Joonised$Hõive

        -   tulemused$Joonised$Maksutulu

        -   tulemused$Joonised$\`Maksutulu-detailne\`

    -   tulemused$Andmed- element, mis sisaldab mudeli väljundeid andmete kujul. Kuna eelmises punktis loetletud joonised on mõeldud eeskätt tulemustest kiire ülevaate saamiseks, siis pole nende kujundusega väga palju vaeva nähtud. Seega peaks andmed olema mudeli põhiväljundiks, mis annab kasutajale võimaluse esitada tulemusi just sellisel kujul nagu ta ise soovib.

        -   tulemused$Andmed$total_production - kogutoodang

        -   tulemused$Andmed$value_added - lisandväärtus

        -   tulemused$Andmed$labour_cost - tööjõukulu

        -   tulemused$Andmed$profits - kasumid

        -   tulemused$Andmed$\`FD of imports\` - lõpptarbimise impordi sisaldus

        -   tulemused$Andmed$taxes - maksud

        -   tulemused$Andmed$employment - hõive

Simulatsiooni andmete töötlemisel võib olla abi funktsioonist *aggregate_stuff* (vt täpsemalt failist "1.skripts/support_functions.R", mille abil on lihtne viia mudeli väljundid ggplot2 joonistega paremini sobivale pikale kujule ning valida ka seda, kas summeerida andmed üle sektorite kokku (nagu on tehtud listis tulemused$Joonised esitatud joonistel) või esitada neid tegevusalade lõikes.

Eeldatavasti leiab mudel kasutamist ka R markdown dokumentides. Seetõttu on see üles ehitatud nii, et mudelile saab ette anda kõik eelpool kirjeldatud parameetrid ka käsurealt (vt allpool toodud näidet).

``` r
# Kataloogide asukohad
wd <<- getwd()
script_loc <<- file.path(wd, "1.scripts")  
data_loc <<- file.path(wd, "2.data")
dict_loc <<- file.path(wd, "3.dictionaries")
model_loc <<- file.path(wd, "6.model_modules")

# Loeme sisse skriptid
source(file.path(script_loc,"packages.R"), encoding = "UTF-8")
source(file.path(script_loc,"IO_functions.R"), encoding = "UTF-8")
source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")

# Loeme sisse mudeli moodulid
source(file.path(model_loc,"0.launch_the_model.R"), encoding = "UTF-8")
source(file.path(model_loc,"1.input_data_domestic.R"), encoding = "UTF-8")
source(file.path(model_loc,"2.impact_of_tax_change_on_prices.R"), encoding = "UTF-8")
source(file.path(model_loc,"3.impact_of_price_change_on_final_demand.R"), encoding = "UTF-8")
source(file.path(model_loc,"4.demand_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"5.supply_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"6.impact_on_VA_LC_PROF.R"), encoding = "UTF-8")
source(file.path(model_loc,"8.impact_on_taxes.R"), encoding = "UTF-8")
source(file.path(model_loc,"9.demand_side_impact_on_employment.R"), encoding = "UTF-8")

# Jooniste üldiseks kujunduseks kasutame paketi "ggthemr" teemat "fresh".
# Selle paigaldamine on veidi keerulisem (googelda :-)), nende jaoks, kes
# ei soovi sellega tegeleda on järgmine rida välja kommenteeritud.
# ggthemr('fresh')

# Käivitame mudeli ja salvestame tulemused objekti "tulemused"
# Mängime läbi kaks stsenaariumi, mis on kirjeldatud failis "Andmete sisestamise vormi näidis-stsenaariumid.xlsx" (paikneb "4.user_input_examples" kataloogis). Neist esimeses jagatakse kodumajapidamistele 1 mln euro jagu transporditoetusi, teises sama summa restoranide külastamiseks.

tulemused = run(andmed = "./4.user_input_examples/Andmete sisestamise vormi näidis-stsenaariumid.xlsx",
                intervall = "aasta", 
                elastsused = "ekspert", 
                multiplikaatorid = "II", 
                saastumaar = 0.14, 
                label_raha = "eurot", 
                label_inimesed = "inimest", 
                murra_y = 30, 
                murra_f = 20,
                f_axes_y = T, 
                vaikselt = T, 
                hoive = "ETU")

# Vaatame tulemusi 
tulemused$Joonised$Koondtulemused
```

Näite testimiseks võib olla vaja asendada andmete asukoht kettal asukoha täispika nimega (alates ketta nimest).

Nagu näha, on käsurealt võimalik sisestada kõiki eelpool mainitud parameetreid, ning ka mõned täiendavad parameetrid, mida dialoogi vormis ei küsita:

-   andmed - stsenaariumi(te) kirjeldust sisaldavate andmete asukoht

-   elastsused - info selle kohta, milliseid elastsusi kasutatakse

-   multiplikaatorid - info selle kohta, milliseid multiplikaatoreid kasutatakse

-   saastumaar - info selle kohta, millist säästumäära tuleks kasutada (vajalik vaid siis, kui kasutatakse II tüüpi multiplikaatoreid)

-   label_raha - andmesilt, mida kasutada telgedel, mis kuvavad rahas mõõdetavaid tulemusi (kui sisestate andmeid miljonites eurodes, siis võte eelistada nimetust "mln eurot"

-   label_inimesed - andmesilt, mida kasutada telgedel, mis kuvavad hõivenäitajaid

-   murra_y - tähemärkide arv, mille järgi murtakse y-teljel kuvatavaid andmesilte

-   murra_f - tähemärkide arv, mille järgi murtakse fassetil kuvatavaid andmesilte

-   f_axes_y - võimalus valida, kas fassetid paigutatakse x või y teljele (TRUE tähendab, et y-teljele)

-   vaikselt - võimaldab mudelipoolsed teavitustekstid välja lülitada (kasulik Markdown raportite loomisel) (TRUE lülitab tekstid välja)

-   hoive - võimaldab öelda, milline andmestik tuleks võtta aluseks hõive näitajate arvutamisel, kas "ETU" (Eesti tööjõu-uuring) või "EMTA" (Eesti Maksu- ja Tolliamet) (soovitame kasutada esimest)
