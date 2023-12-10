(ns t.core
  (:gen-class)
  (:require
    [clojure.string :as str]
    [clojure.set :as cljset]))



(def input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def myinput "tsgbzmgbonethreedrqzbhxjkvcnm3
7qlpsnhbmnconeeight78
prbqsfzqn57
ctrv3hmvjphrffktwothree
9six9qbgcvljfvccdjslspprgonenine
eighteight9dnvcqznjvfpreight
one5sevenfour3
gjgfiveonekzssz9c
one54
rtsevenfourfive1rqhslone
xkxfdhk25fourtwot8
kjgcsncmeightdxdgkxfzqmmkg1351
7qpnldcvgs
reightwotz5
nineqrtrsfsreightthreetworvrphxptlrbczxsix1
1svfvgsfcssjpvhzr
1seven174ndseight2
56lrkxdfive
2jvrgmmlninejf583
fvsspqpt31
3threemhjhvrvgkseven
eightoneoneeightttlpjqgnkndbnmppjsdhzdbhkhl12
vxjgghc18dzbsssnssh
one2cgxdfbfhl3
8ssfour997six6
8onesrtcvqmxpzeighttwo6
3twoptpvz5cbgpdrggdtwoseven
35smnsnzxmdjtsns6sevenonethree
fivetwoeightfdjpscncb41
flkdknzdxtgdxcfqkn4mbh4tqvgdq
one3threethree
sevenfourgdnxhsqq5one1
kkrjgpm2sixeight24
krrhdhjqgmc5dv4
cvlh16nineeight
495five2nine1
tzmcxvdtvxpmrcsevenfour15one
2qvbrsxqsix45threeeight
pfmktwo29onensbfgdddfm2
7xqjzxstvsthree
37ninetwo3
5vhtctxvjrvftwoone2six
hbhxhjjl5twoqxmlprccr5
ninefour2foureight69
1sevenmggmcxfcfdxptktsgcgsc
fnkzsdjh3kqxpdvmkmvqqjcjn
twotcspvbvppqqntwo1hlcvppcpnv5
xbfhpnkqbnxdfgbgtm5
84tpzgonelnine6bzkx
bgslbmdkfpbx6
lqpfphxdtfourxbqdhrbcz2cmhtddsnkpgln
7fourfzthreezhrjdjlvsix4lrrfv
2seven3
fourtdggtgsdm5
1pbphhpsxbnqmfbxzone
tskfvdph7mftvqpnvcj2oneninenine2
3fiveeighttwothreeninenine
seven687two1pdjbbprztwo
eightrslvddpmonejfive4hhqbdq
xbhcqzx52eight52seventwo
fbgpj8fourthreecqngmbbvkktlbss
plckgsixeight6eight95bnrfonetwonej
1nine97rtrtxhqnone2
cntfouronerdsnn4lfcmhvlrq1five
hrh34sixfourqqng7eightwot
sixlfldl7qjsclxqfive15
khdfour17
frmcpx65
five6bjlbxkp7qthreethreecc3
5four6qlmpsrdeightfour
9lxdxgfrnlk4lsgbjnz9two
one7five2fvdqfqqn92xsxnblxvhdfcrz
hmhmxvqljdgvvvdfour5one68zgm
718
3onefive43
xgpqs5four267six
pqseven17nineninephpszgqzsixldhxmhzgf
ncxxlsqdkvc8fiverslzqtzhzltcmbkthreelkjjckxsvljvs
nine6sixkrs
5kcqpgfms
1znbcrpxmjcsixeight
sixtwo28eight59fql
8nfcseven
8five864sixfive
8fzsvsrjmjxmc
kftvseven6dcsfqtmtjvmgbrninefvv6
twoseven65
nine58sjshtgtqlz
lhjfzvlbtrfffour9eighteight73
9crhmmqkrrfour
threeklhhtlmczn6thfkvdd
dvjxkdgsnlds44kxs
9gtvhrcphvnzbmckkeightfour
fouronejtvrscxj9onethreelgslth
86four
4hvnqzfthree
8qone9
lnkngkxsflqsvm3threenine2seven3
65five
8529xdd5threeonexq
sevenlthree1
5sevennineflfivevgp
6pmzqczjtlnfvgg1sevensix
sixtwoone2
kqrxsgtrrjsix9
2twonebs
fs9hmjmdmbhkskqvdb
293
9sevenbprbbnzqjbhfpcgnxjmflxgkvqgvc
rkd58six
5mbgfldv7rskkllhm
fcjdzfjpvptdcxfltdddm7twornkskrnqnqv
bkqzf51two8twoeight8
2fivefour4777six8
mzq67threetwonrqpcrptwo
7bmkeightm8
z6twoeight5threerfckddtc
threeninesixmtdc7xtjhvksjjc
5fxffvhqzgtdlfdxdhsixnmjtvlq2five
nqpsvzgql1
six3qljkjbfeight
sgddglfjrtwoseven3
rxzhvffr1rfxqpr
7zjv5
3tcsxlpxhsmnmg27
lbqkvfxp8sevendcjlrfour7
cxlvqldcjhhlzcvsix6crleight6
jhchtninehrvlsfvkr7two
48ninetwo
45fivefiveone2
hthchfc1
oneninetwobfk7two
ninesevenkfnlhrznqseventhree9
2fzbgjdvbnktwoonefivefiveqhz
seven28mxjxnzb
gqrznvtjeight9six277five
four3three
1kpqmrjctwolngk8
49zks
threesevensix9fivergmlzrblg
eight9sixeightlfvrbvfhd5one
4sevensevenonegm6
six11one3lrztzsjczlnhddxbn
eightsevenpqdsxzmblmvdmjpeight9
71seven
cjqcqjlkgffoursvfcqxfkkszjhjl73
dgndjkhcplztmkjrdn53
1eight91vqjhmxrq9nine6
bthlrtwo54hxppxfourdsnxnlnpvhmrpqtqzlr
1bhdmvdrjvzthreenine6four6
threerknmhjtfourtwoeight65eight7
1hbgvsdxtrsixmsfzxrlzskkkgh
4hsjt
twothreefxhlr1bfngthreemthqqvgfpxeight
nineeight4hp5nhksqdvrhcn22
hdcfdknjkhsxnk9
pshjqxmqcbkhqkc5onekzfgkbeight
five3ninektmsrxhsdrseveneight1onenine
p5five
2sixrgznrrvmxbbvnxhqtpeightthree77
threehmpjclsbhjlbxj1
982
vcjxvjvbmmzmrvv97one6rdxrdk
four8one9xpgfjgvjmdsevennineeight5
five7jmcrkbnine2
kmbhc8threessdnfmsix
214sixfkxkllldfive6
dbhbfprsqkpmnfxgkgcone4qfgrgghqtqhxqn
1plplklqjqjdsix
ztzgqsixqsevenseven4
3threedjnbdbvxseven3qpsjone
dnccnninetwomkzk52sixcgq
94rdhfgjjr1gzl
8three2ljbjjfvx
8sdklzklt
onexvrsttmhfninetftbrhtwo1gkpbninekh
vsbfour5six
2phhz85
two73fourfive
2twonineninesjkljpdgx
2h
tghzldzcj38foursix
9srzlsv9seven
twojzssixfive53
85eight35foursix
rzx2lb
ghfnqk396nxqgvvgsmnine9hhc
6sixnine746zlrksxcglg58
1nbfivelvpcjnsddt3eight4fourblrzckjvf
sixninethreednqkk9zzspxbcfjmfive
16tsvqdplfnbnh
l13tkbfldqcdfzjsdvtfttphmcmsix7
rcgmlmnine94foureightnine
lpvmrmtkthdqll5
gjjprdvggvtjvqsnbg7xjvnp3eightninebhrdrq
lzzmsmbpqnineninemnine6two4
52qldxmrfive36ninetwo
four6fourhrsgf29
9zb97kffl
eighttwonine5fourkqtjsjthree
two21
two6fourtjjn147
lxnzlpcvzkbd9pxtr6qhbfmcmblbhmncljqd
zccjlmzhm9
11zdnjgj
one5fourone4onerttltxrzgf3
eight2nine83onepxjhx
38fourllqpbclqnlmfourthree
treight9threesevenqgj
jb7threezfvnine28seven
kmnkbmmnine53lddseven3
3jxgfdzkgznine
72lzbqmclqtwo94nkt5
threejdqnineeight4seven9two9
sevenqnpjbznp6sevenfrnghcmvlfbns
oneone23oneeightone
52qngdcmftmchfzt
two8hnine1qsseven1three
seventjlsdnmx25pnxvtxtsix4
3veightjkgnbmcssevenpjrszdxqthree
kkhxtnqzvz4jdqmtwoeight
4eight15niner
kreightpprcklznlcstjvqhn36ggfkphxdhm
four53ngjzlmhg
ztwoneoneshxnb2sevenxcrgjbckmfiveeight
3five5221ddgprnvgpnj
2hdkdzcvcmmtcrbsvfsevenone
dtkjdncq73threechgcccdgqqsixthreehlfroneightn
vtfvj4nine23seven2two
phcvhvlq4kjnjcdkjrgninefourthreethree57
twopxngznkbrmrmrz7135one
one3bqdzone9sevenc2
69threesix
6jcmfivenine6fivefivethreetr
six1jpxjrqkpgllkqldf
rjz4two59gjjgs2five
joneight9gvqknfour7sevenfhzjsrldj
9one4onesixhvvkqtpleightwovmh
seven4threekpstdppxhvtwo7
flxzfrxp1
seven57eightthreeeightseveneight1
6sevengmkvrvone
five1eightfour19
nqxnqn7five3txrngheightnine
rdfkkkthree7lnpmdgdb9
jhhvznhlsevenninefourcmkrbtfive3thtfbjvgvdnv
2seven7eight8jrjkf4fourhv
oneone7hmdfjpzr57vfz
7vqsllrhznvrdccrdrnjvmzmmknp4
412sevenflfcvksjzk
7eight43mq354oneightv
sevenseven7eightoneightvvj
twotwosix2ninefour9three
threefourtwotwothree1lj
jpqccxztdninepncqqmnlxninexjfgvgccrm1
5cr
jlhrrqmtlgtqfqpmkrhqjmpmxjshshblthreefournine2
four93kssrxr
bztqzsjkeight6two18four
sevenoneone587ghfbdmjpqxnpbh
823onesevenseven1
tt3qmcsbcm1eight
prkptfivefourtwo8
4six8836gmmgvlrz
qklnpxjqhpczsc775
phblxtj9
jgz8lrghthree813eightone
vppt4eightthreezpgqt6dmxdtllksgrtmcbpfjh
nine9lgsgzkjvbcmtfourr
7three61ninefqgsfbpvjrx
98ninenfzfh
two2one8seventwo3hpdbstnhvkthree
fourstrljqf8nine
5kzgf5rspljfnsljrfrhg2mdsh
jkbjkbjhvfoureightgfnnxxnqh826z
3ksgl
3vfivehntm
threeeightone64krfskdqlxsfive
oneseventwo8vqzvnine9ninenq
7fivesrkfkfthree8qmxmdrvhl
447lhctbzpphthreebkmrthreetwo
two92seven
95sevenonecgj
kcgsqqdpfhrpxzceight88
9c4six
rdkqfzkdlggfq661tfmqlgkcfmfcvpqsmr82
1zzhzhz
six2ndsdxkfxsfthreeonesevenzlp
eight5threeoneightmxp
sixfour4hdktht3two3seven
9zdphltlnrfg9225
6eightfourplxbgjfnnzfk2sevendcpbl
threenineeightsltsrkvpl28six8nine
lmbqmrmjldlgb7
2sixninedxnhmdfxh4five66
5mllgtrxzkgxskkqxvpjttninefour3
2hjl6two
h5
eightthreethree3hcsrt
htjlplnfxgfqkm77seven
lpmggpmcq943lcsvrhdljsqqdlgrphlnnine
7twoonecgtdnfvlrsixrhnineseven
lxszmkhj983five
four9fourninefourtwol
eight58sixsix94
2sixthreefkbvtdgjq
1rnc
86seven13
one6f3jkrlrtf
two4nine72twojtfive
fourtdtwo27
two6eight
nxhzvln3five5
six7lbgqhnzkntwo
hsfjgnjxjvzskjninezfndhtwofghpstbqzcseven1four
6q
ffszknxdbqzgvmg2p
cvl52
nmfmp8six5threenhnxs4
9fdknpvztworqfdpjrk36eight9
brmqzbhx84
two72nvrlnhkjnineseven
1spsmjscthone8
sevenphkpdzvgzcjvlskdzbhqgpknrzdpsn39
gfmxvrrvk8
8zkpvrq7ggjskkr
7ninemvm41sbftf6
nine2sevenfiveonetlnhbbxv
rtwone881998
eight5threegthree
fhlnkzgqh191
48fourcmfivecz
5mdvbgmdhqbctxclonedhx1sevenqczd
12one
sixsixeightjmnbqzfive49bk
nine2seventhreehvrfvm11
gnsixb3three2
one3hrqzfsc3one8threehhjbzkmln
vvxcbrrone95
nine7twosnpv2threeeighttgg
8onetwobqnxkkvhk5
qrphvhfqninemcnzfrhfbvdtsnhst3
2eightj
eightmdjmnqd1seven
seven5sixtwoeightcglpqvlzxdfive32
nine3scvsffour6
3onefourseven
cfqfkxv4five
2dbq8fnzpzp
fivekbblqgnjt12pxnvjxrssm
sixfive16r8dqkgspmlb
ninexjhfsgn9ninezv
lcfourtwofiveffrl6gfhxsrzmdl
8oneshvxjn
8threeeight8sixqbfnpxhr
smdjbhvsxlnhcqnxxgqcxfive5
threeonesevenglblr56lsntrdsjhp
seven1ninevqpssj
58seven
lghxjdhpdbqs19
lkcttrkrcpfourjtkfxmxcfive4kxnchspbqvqrfv
7mpgnvdrmsqbcnfzrhtpjhz
threezkjqk5fourthree8four3
7398
xpmrcfffmhqpnmxtqxsblcd2fourq6eightdqvj
6onedvdxtv9
sevenzzvgmlqk5phcldhgghvshclp8
xpddfttzsix63one3
threem5eightwomp
2crrxnvrcjklzvdxk1slmrkq2
5fivesix8mstzprjlvqhcrn
rzkhdgfvkqonexhblnnine32
5rseight377fivepnlsngxvmthree
onesixseven9
9dgr98
6jjcb1hlmlchvtqhpp36nine
lrxhvzs538seven76mkt
six48fivexqxqxdzzhggvcb1
94seven7two7
five2bqxzjllksix4
21three2eight
9rbsrdjbgmgln2fivesixninejfvk1
9foursixsevennine1
9zjxdzqbxjlbbsnine
8zfslsdlhdqmptwozzndjcskhptwo68
9fiveone3fourpncrvfive9
tbslbmgkqv936sixgsqzkfcjpl
fournxfsgdthq4bsmp38sevenfivegclrlpgxt
1kzql
7onehfzqjdnlzcxflzrlxdh8four9mzf
1sevensfivevgxeight9frcbzhpn
four1seven8njj8
74dtgcn
sevennine8twothreeztstwoklxhgfbgnnb
7seven3bdrbpnzcr
2twooneggtzrfqcsqqq
rfxnrxkk5twocjddcmqbrqjvprtbdjtpngl99
pspkmpxzmn9cndplpjskthree7klc
eighthcnhhvvjxc295rxgfour
74nine
jk17mxgfhjveight8twonine
2six8seven2nine4sevenkrgmk
6k96
5jseveneight6eightnpfqt
ntwone1dkfrhmfourzngslfournine
nine64
njeightwo52
15mhnqxqcdgcqbqtdp
hlpgsrxf429four
tncmkps6bninergvfkrv649
bqqlgvbdp3
7sixpzpbrxbhqjgvgns
849zhtmsevenfour77
three15seven9fourthreejfmxhllseven
ksixeight4three4fqkjthreemjqhxqgk
four4seven4eightkgsg
sxgrghvnfknfrsixnkflvngkmsznhc8
6dzxmxkvvpnkp6fnzdfkjsevendlqrsgdztone
eighttwo81cbrtrrfour9fourqkkbk
hvnjpcxhlb9skfjsixseven9two9
pmt113onertkj4
seveneightdftwotttone6
fnsmzpthreexcssct42
5eight4threefgllrpsgqhdtsqbqvzxtpgpdgnvctt
rfddtbdpftrvf641five2x
1three8clseventjtwodlhggr
fivelxhrninej8fjngzdskseveneight
ninexxlhmhzdj7tfzds3khvctlrxmhsxdnbm
bggglk31threefivenineonepqcdzczjm
223seveneightfp
mkvsblkv76
7three86
xkmxnbmtzrpdp2threesixfjkpbhpzb
9four2
7tqqg
three4five
zndhqdmkntwo6
rgblqcvrxzv5clzjdmfkph5
skcvbd2seven5
eight8xnlmzm
9twovfnvrmcfhn2gm187
seven8sevenjshjndsdgrnczlsevensix7djrk
bt2one4jgztwo
one126eightnzpxkgfj
blrlnfnnvbhkltmgqqsixsix2bdnprfp2rpqtnvbtwonenpx
prffzmfndlqfqjncvskgdtwovmbqnxm8twojttxpv2
6qhjdzs5bm
76lcmfntskmxddthreesixone23
bstcghjbhkrlpxf1six
7ptpcjchcfffivenineoneone
7ninethreezstzpdr1two
szxlzxpn2fiverhjrp
5625
two6ghnhfvrzlqdtqflvfgldfjj37gcxnxf
one3scnfcdpsn2five3ninejbv
eight569nffztptqgd1mqbkssdcppjg
fbsvjlln4cdshlvmxpmmc4threemfqnseven
nljzcfzfive5ninen
gmhlb2sixfive55cvnkgknrpbdtnrd
47six
skkhbxfour6trmh7nineseven
three9five
12ninexcc
eight6kdtbvfourb
425threetwo4hxnxseven
five4jbbftjkcmjqxrdls3nlhvcgeightrjn3
knnjdj5tgtsrgnmjqmjjrnht1
nine3mhxxsix39
fivefourseven2drz3eight
215xnm
6seven3
77kqzfcbklvznhgvnpfivefivejx5
8seven6sevengjssbvngxzbgcmm1
mqqxqgmzdvzdgjrlkntdjhgfouronegrkddl14
5threetxbscpfxdgfive
two3bdhxhkqnd87cmrkzxf5
two7twobgnxklp
9sdnnzrkms8rbbzfqms8one
2hxdxxstwo
foursrb7smqxmkt
14xfhdkfmh
84trqvtltpqvdtzdlch
19hjjhfljxnine
1lbkvjcjbbeightngnqdvz8nddkmqmvvb8seven
mlnpptcflthdprj4fiveseven92sdhxchdcj
cdmbfivepgdbgdfvbklz8six7
cbqvbtpljv8skzgmbzbfgrbffmmb
one9ninev4
nzvjsqx7fivebgptkrxsx65sixfivesczbl
42bhjthree1
twoqnsxllxrsfive64vgtpcmxc9
sixbzflbdv7
54threehf9vhctwofour
4pmkmlpnnine3
3sk88qglhbsjxdsdcnqrnpdz
79szleighttwofivef
vbctcrttwo43oneone
qjhdlqzqftfgxgnbbscsbznfgslbltd6szdppxfpm
fivenbbsgfthree9
hjqmr9rqsrhspcfjmfj379
lxjeightwo6four8
45twonkxhqq
ggcvxfz6lfpbcspvzsqztjllrrpd
qmlvh9bmlkfive83fivepbtnine
1qqqbrct45sevenx
ttgtbnzrn1dprs6four
nine1rcstjdlhb6hsrnvsjxeight1
gtlhk3gjhqvtwotv
six278kd
vlgzjxlmfs6
8d51
seven6lzjgcc
sz7sixctstdkhlb89
27three
6two3msmszdzcrfgxj5
8dbvzcrqv
tftbmjrjd831gbzrhscthree6jvvfour
254sevensixzzkjsq1jmmbvdbhtmfjsp
ghqgzfkqhnqntwo4nfj1
sixfourninetwoxjtsxcr3
five2niner2
22blmbkts7
one2cbkvqstone5threekzvskbzdt5
48lgvmpjvhlfsixone6seventwo9
1five5pzglqpqltn
8dc6blpnqcjndthree5
lncn3sevenptdrlpqsbseven
9mdzt
flnr3sevensevenpjgrgdbrvdgkhtlzzljdnppp
lpljgkpqdgqsevenseven8foureight5
xzrkml7
127
hgxdcdprhg4qkxtwo1rlc
nine9five5g
three1lgdctzmqpgc4zbmc6ttsls
onethree495three
eightfblxhxptkdnhk6npbbsxgxlhoneights
fourm6sixeightmrztnbjptqfv
twoone76
6dsbcqrjj
pfkoneightmv8bdrmhlnhmninektnbdtvtwo6twod
qttsnrppd6s
76three5
thrxlfmbmtsxjzsx5fivelvknrgctp5vlmpf
nine7ninethreeztmlsfch
18eightthreetwo978seven
bseightwovtlxsb5sevensrqnrklqvzstncmr6zg
3zthdrzlqcjbdsfgqphkzqeight
mvfnine6sevenrkbxbhlld35five
cddpxjhdgt9fsfmkxmvtm3dnqndzsnm
vrhjfsfkt3sevenqcsxvkdrfndtfljvkxzckctwoonednpklkm
hqjfnbh9threexqfive
mrpdsqbhzjzplrlqf4fjstqq
6htcxzghbx
nine6dflrcbjl65
bpclpbkbbmq9threeonetwonejn
eight5fivedcsqtsnfkthreefour9six
4lvl
fourq2
four388rrkjtfjzrceight8
7lznsixfh41pdlvhbh9tr
9nineseven438
tgcqntlpqonevmcqninezcxfxbbone6
seven2gg
two1one65threexsix
1one6lrgflmsixfivefour
sevenone46
61crvnncdxmkseventzqvdsmfkx
khztxzpnl6nine
threeonehjxpgj3nine6threehcsix
twotwo5mpbgbtfjpcxbksix49two
62
kssevengjcdtrcmklxdgftslsz7kfclxsdtc
9mgpqjhlfivesix34sevenone
89711
3tgkpfrvvcpxfs8jhmdcxz392
6ghbglhfive469x
9ctfqkhbgprf38
cshsctcqr6tcxzjgrzsx
chn5onethreevqdhhdv74
one9fmlbplqeightfthchbzjhzgkbv
fourtwopkllbslsix94bcshtp2
rvmpsxkmcqninetwovhjzvhm3
4eightbcdvjjr5ninethree6fiveone
6three6eightdqkcg
rsix7seven6fdpzpgvmkp
eightrvlmbdq91six
x978bsdxxnknine
81five3
6bmmvdflqfxcmhtsixjprt
five6two4five37seven7
nine6hldzbzxhzdvnptjeight4ninejxlhvtkpzd
eightfone9
4vknnxxj6192q6nnfbcm
twolzt6two23two1
84lltlrz4vsl3two
rnfour39cpcggzone
hgfx464
6tdldmclkhqkkdzxmhvtthree8
9sevensix
pfgb55
mqnqnineplccphpctvxs3sldkl
cblchzhrsone4fcbvfxkjjbcptp8oneightv
5mndn
pptwone8
fivejvzddb3bcvvskzxstfive5
k7sevenninefivefiveqzbxlsgcmoneone
gxqthreejzntslggzhxnsevensix1onecphllcxc
6slttzsdvnkfgbqldqvlqrszlpsixtwo3
7lqlkstbxpt87xcbssmn
fourtwoeightjnflkkbksn566
dkjpckbtnninejcxg3
6713ninetwo
9six3rskfprrbqhbstmp9
4onepfqshseven4seven
1d
zdht4sksmztjpcrjthreenine
four29one2csmk
93lmdlxrrj4threeeight
zkbckninedlzxpmdneight4
gjdshxctnszcrlxj8
4nqfvtshhk
hvjvrh37threehznzlrprtptwo4one
eightghbtgpbjfdprqnpvhhcbdtrzsjgsix2
sixmgpzcmxq3sixsixxpdrfpzdsmqsr
onefivegphhdggf6kdrmnvgfqj
mvtj2six
6jhtqngl7jtphdsvtkbvscm1fivehv
632onefiveeight2
9ninezfbzdplrfourthree3eight8
164three7fbznchn8
fsklrdzbzv8threefour8sevensix
twosixtwo4sixsevenfivethree
foursevensix4fourxhfjzjbkhsix
eight26foursixseven
8frxqhzqstxzmlsqpqtcsevenfive9
mgck4815plnnmrvhfvhbsln
3fxpccfplrfivetwobbngonephjs9four
4gbbgpbqnpbqjzgvjrgpprtwoqvjrrn
four17two93eightthree
twoseven3
one5vqpmvkpgtwo88seven
sssmblvsixseven2
6fsdxfxgdhnqkcpzmbljlfkfsjztdp94h
kx99pbk6
nineone7knpqj
2fourzgseightseven5
sixninekdbvrrllrl8sevencvrdhnine
8jtqlpz2
hgblktnine92htpggp22
8l3
xfxb82srqlldfonelqjhssmsrfkbmldjqt2klnq
321one42seventhreenlrtm
jponefoureight6nine
3lsljdntnsjrseven
two1rvc9fiveonerkdnrnfd1
1twoclddjfsixtwovhhczl
4two48krlhhbvzhq
78fourncglqhghzxhbrb98threeglzklqgkg
threetmzvnrfnqq23
9five5four5ones8
six4onefive6
45lfmdrgsrvkkkzglnqmrlt
64onevthree
onefour9zvkgndsv2
dmrk46vlrztxbjfsbll
2four8oneeighteight
pqtvvlbrgninethreeone1
6four733d
seven1qhxhgxsx8fzlbkm9hpmnqkknjpseven
6cspj21fivetwo
seven3fllzkctljxlcqljbxfrcleightwov
73bkkkdnptddqqxhjteight
jjdmnfourp1threextxpmdnnnine
five2lvhh4kfg3
rbconeightthree8
59lqvpccqlbmqqtzfourseven547
vdvsbhcptvxbgd46three6
xtrbzgtwofour9kq9
2nineeight
29nlcdzgrkh7
onehglhgkcksm2onemzpnqmbxccvpxxssg1gcpvhl
sevenhhhgntcldxlfbrpks27threeeightwoh
2kccbsjdblthree
nine6threenine
nkbcdbseven8cqfour
bfour3
nineeighttneight833
3glgsvfhkz7fivefivefive
zzlxqlfour3mcjjp
93eight28
1threeffive4c5oneightgdg
five4six
gpzjjxthree4fmvbzmzbjf31six
bcbdtbjrtn3five11
kbtnkjvkmdh6
fpjsqbxv5
3twofive
9vplnvkkmhpdfxckm2js64
4eightsixxgmbxzkkcsevennvdgprqcgvsqct
eightsix1five8one5sevenvqjmrkdjq
9seven7
ngmnllfj49dlqdmnthree
sixthreeqbkmzvmt7sbgsscqsg
5sjbrdqvbksss7gnfcp6vjpbfcgps
nhssblqsixtwo57
fourbthree7four
6foursix
pfcbeightfvtnlrmld273
fivefhthree8189mvhf
nine5qzvxjvj1nkljhvdpnf9
tgrllhmcmpmpeightzjd7
vnblzqbhx4four6
5qb6six2one9
sevenprjnnznl9nvbfour28sixnpxttwonemcn
4sixsix
7two9
7pzzdv2fourgxxczzqtdxfjgtxjztbvjt
218jxfvzrfournine
4sevennine
662onecvzlsixzj7six
bpnmf95seven2
clfcfivetwo9mkxqnthnnhkpxcm
klgfmtjlctstwo9
1zlqhkkhmdvszg2three4nljlbrzz
five6threesevensnprfgsggs1nqqjtsqxpdtjlfr
nhkvqk89four
rtgzhbhjcjcznp5cxbprzxfoureighteight4lng
xdvgfzxbvzfdkff5dthree
1two465sixnine
2six327mflsr
qpeightwo4sixfivetwokvzhkg
4seven9jmbnjrhrtthree
4flchcfh1four
94fourldjgvg7drlcxqtnv2
99sevenfourmfhnjg9
32fivesix275ninelpmkponeightnp
9xzgfxcmplrzjsbnlkgcbnpchth
htoneighttntmj6ninethreen7
64one9sevenghcqjjrseven
8nvkfqkbbpzseven63zpnbbnine62
fivefoursmdnvczb6
mmrqfrtwo2hhpk7eightthree
dnrrfklrpxstln57vqzkrxlqnmsvmgdbthreeoneightc
19rqmvljqfrmfoureightseven
twofive9vm1csqlrvngtfiveeight
zeightwofivefour6gcmh97
7pzts
42ninecnbthreedlgglgp
3kqthqfprdntgbfqqc
sixdrrqxxdgl8dhf4
51three
39qpctcjfnh7
9bfour4
onefourseventwohbqpxn73
jzbqdcsevenseven8seven8mmbktn
xlzeightwo3648threegrfourrjzvz
eighttmmtcrqhlpnrjjlchrvvthree6
5five3one69eightthf
bctwonelppl122bkfivetwo
foursix2ddxqqz
hbmcnbvdhpkhp3sv88
csdp3onebjxxlckthreetwoplskdphhmp
rvkdmmgfvsfive8ggvcvxgjgrsix
7zjsdzbeightfour7
four3qfsvmbhhfv276
krdnine4bnneightbpxfdvl9pcf
scdklgdh46fivefiveseven24seven
9rmmsthreethrlxjtjkb
7fivesevenstfourfivecddbzmxrrqeight
fivethreeninexgkpmpbvsix7
sixvqpdpfk43rhmd9
2zvkhxtfourpdfnsbsnbvone
four7lrcvtsczdp2sixvvlkzrzzk9gcxtmx7
61pzlq74cmlkjvseven
qcjzncctvv237dsjvqm2three
89threezgd2eight8
nxll6fivev6kt4rmtfkrnkfive
lvxxvnpzlrk8sbmbnhtmlds
sevensevenjhcxcninesevendlghdvfj5mkmdb
vfq4
gxkcbkknqg8187eight
six79tcdlv
bsntwone3six8
8nd
qdlhkvvpxrtgbrzpmcfourfivegpjqjfbfmngnnbpshfkdpsdgj2
threetjbcjmzlcvnchp34nineseven
zjpxzkdlmhcbzkrflvfour5mdqldvgrnx5sixlmgjpqx
cjnvmsdrpmq8
threeqr8xrxgv87
rcfsntqrlonezlgl7dhlflqckc9vhd21
9three7three
ptkqnrtmrnsixfiveseven67
44fourfive9
4fstrhjjctsvrz7cpbr6gjpkkxglx
6rjqcmpfbp
731
fivexdfkzmjs3146five6kvdsnvsxsj
dhhz6
sltpllcs4seven8771
1fourkjfhfcfxbeightseven8three
sevengrzlghdkg8three
sixsixnkgfcfivekqfzsj59
4qbqggqlmjkrgnrxthree9dxgssjkrseven
xhlmbone3one8
3t
8six37
fivethree3onegthree9thhfzp
9xdmvkx7fourfxpgsvnine77
coneightfivedfkqrfjcckghzsrtrc9sevenone1
five4ninevxldttfxxdfxfhttbxkvjkfour23
cpfklzcpp2eightfjqcrsmhlnxckxmtdchbzqblqsqqp
34six1
5fivex4
fivefivepjfrpzdvcq2
onelnppnxczthreelldqndvt3
nine699ninexjnine
16one74
8jdkblqbq59zlkhj97one
ckrft6sixeight
fourldmq24
87zvt
threefiveone75hxnqqsjroneeight
lsxlzvssshlvbr5vtvgsznrxhkp73
threeknpqxxgg84
oneone3mvtlxjhfiveksgpnms9kzkf
thxrqsrninefourfive31kbcp57
fivesixnine6oneqgxckbjqfszpsvhjgms
eightsix83
ttwone9qqhg7
7fhvffiveftkbfgxsdfourthree1
hbkst6foursixdndhmtwosix
mhgxpvgmndjgsftqjkkxkfzxmhgtdpxxmbztwo6six
two6ljpjc8ffgmblpxrcsevenvbsm
xqbmzh85mqchftctnfourfkeight7
xlbltdqcsx2sixmqdzvxhqj
eight468
nine2rcrgfpx5
eight5tjoneseven
hncnnjvbdfdfcthree1
sixzfllx24threesevensix
7eighteight
8652sslmpr1
m88ktthbnh5
six1ninethree
sevengtb6
9eightzpdkqzp3
tplshfhqsshs4
4twothreefpj
48threetwockbzxdhqp4eight
eightmkeightjdgmvxdqfx7
six99
69one
twodmnzphhsb2
three3zhqckdqeight1
2x9xjzgtdgk5ninefourseven
4vrpseven7tq
86cjqrqp
one8four67
three7two7six2kxl
bhnbkthreetwofqchzvbxjqqm4seven4five
onehncctk2qnphtkjns
6stmbmtj1ms
mdhmknhftkdtnrgkbmh7six1six
six4three5
fnccxltjdn4gteightfourone3
twofoureight5seven4
four7twosix8three
pgvjtszlkx4
threeseven438two5
six78qhspmhqtkc
56sevenjdfvfbzvsgthreemrbmcqctgonesixppg
fivesevenlsixvgxfkxtbclkbsqt35
sixeightfiveseven7three
frmgthreefour3szxtf1
5eightrvflvtqzq8sevenj
five5dbtfxqxzfour52mxfzmkfks
2ggjtsdmdr6
cpszsjthree4
rrd8rrfcvzhcdr651rfc6h
sfnfsc89eighttmsc888t
46eight
992sevenzsxkzxxppnkxlxkpbz
seven93tzgthree
952sszqqqrc1seven
fivetwo3sevenfive4
pfgdhlvv23onesixtwoznfrlcndlvr8
mhcvnl1three5
3twofour3pgjtjp2five1
nfcvxxcltwo49khnmggcdlr5
8frh21n
6p477xtfrhvclvnxpbdmftrgd2
sevenbqsdxlceight3
seven2nine86jx
44cqv
7bfgmrc
88twoonethree4m5oneightn
94eightthreefourdqdrnx8threetwonepbt
onegvkdxllnd76
onechljqc4mvhljvcgbthree
2ninenine1two
threenineone8ggnbmnfcnfteightsix1
eight6fivermqnm94onessnplzpkzjm
7fivezcnlhtfournine
pfourvpqfqz7foursix
sevenonedcs9
threebfspz4b6sevennzkznbd7
tbg56jzgmkgmxnbncvtgrpk4
337sdxdlbqd2
4tcvrpvqkkhhthmcjzggqgmpdonektzmrrszs
twocfgqtp7
sevenfcmgkblzh4mvgjmktn4
ptnqxxf1two
fqftxjznine186three
6foursevenonenjkbdjfbhhtvdthreethreeeight
4two3
zhronethree2six3five7
3fxbqvvkh6twoneftm
zf2two4xndhkmb93
fivefivefour9rhcjxxtbtd63nxtk
five3qpvgdcqcvcvbn6
1qjckjqkbqvbtqkrbvkxp
2gnglzbllksix6fgfjrrltrhrspl5seven
four6five8gpktxzxcdtvqsevenqlbsnine
seven6lgkknlfouronegxnine2
9ninefour8fourpd2threetwo
358lmjlsevensjrlhkbspkqxcfourmktxqrhjz
mbqnmgjcbninenine82djlmzk38
four6lgd66eighteight8
three566
czeightwoninefive7one
threecmkmzszq337rsctbvvpgvlfvreight
btwone91blrcbjvpfl
cgfddzmnmz3
seven3sixsixnqt9
xsthfourtwo1nine5dpvjpnine
97fpjzktsixtzpdseven5
seven6fivefive
two16
nddxvnqnggdzcnhthree2
1sixseven
three1nvpdzhrndgbvfiveeight1
134
threeeightlvbp6
qmrmqcjkdfoursixfive8915six
2jrsix78twoninefour
5vgtsevenfoureighttmntdgptbvxonez
2npzxnpmdtxctgeightrbvrgdkqthree5fg
hxtvnvqffivekgzcl6dtmplhqprfdbzdoneninesix
two384zlqs8351
2sevensgnxgzkqsixtwo
nkone877fivegtr
ldr43
six3nrstcpvfive6
one799onenineseven
4onetwofourfourghpzmgjthree
one53two8tnmlpcxvvqvrtjthreefour
49nine
sgnxr3two
threeblnc1six3hvdlj1
8sskxtzkl35hknine8one
keightcqbkbz9gsbxdqshzngxcbjqtqn
7foursixnine
sbhv3lvkgsgk
dmvthree152cjh
two6nzdd7rrltcngdbshfzlhsix
1nxthreetchfsleightq6v
5threevrc9four6st
b2threeeightonesix44hnxzcm
2threefourfournine
1fxbbfszjhseven
lrxhhqdzx2sixsixtwo7eight6nine
2eightmzzcgszpxr
7nine7sevenzndthknx1
kkstmkvmfeight92ninelpxhsssk9
1zjpvbmzdxxc
ninevtqgtzxtprbst1bxxsrrrztnhbjrdqvlheightvbhncs
7seven8threeeight
mfzkktfoneseven8pvqpszgqjszx
lr9kddzcxmmxc253lxfournine
three29gpbvjthree6hcnxzqcdcj
sixljtcnxbq8vnbrjgd9qsqonethreenzdnvjjg
vpeightwo15
lctthree6four14nine
eightfiveone47sevenhdxtlninegffpzhjmh
foursixhtx9nineseven8
seven94four
9h6nine
fivemxkfxvbpneight8hqlmmrmeight6
five5bhrghlfiveseventhreeoneseven
eight79nbmspstpkxhttvrq
xqcmqxnine6
prcnjkshkvlcgsixfiveone6
btbcs2rsrcrshzp8six89
5tg578fldlcxponefourtwonet
v4
gqrnpz5sth
xcsmcfour3eightts
eight691seven8cxdbveightzv
onenjhcd9")


(def input2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn sol1
  [input]
  (apply +
         (map
          (fn [l] (Integer. (str (re-find #"\d" l) (re-find #"\d" (str/reverse l)))))
          (str/split-lines input))))


(defn maptonumber
  [t]
  ({"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
    "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
    "eno" 1 "owt" 2 "eerht" 3 "ruof" 4 "evif" 5 "xis" 6 "neves" 7 "thgie" 8 "enin" 9} t))

(defn sol2
  [input]
  (apply +
         (map
          (fn [l] (parse-long (str
                               (maptonumber (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" l))
                               (maptonumber (re-find #"\d|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin" (str/reverse l))))))
          (str/split-lines input))))

;; Integer. : bad idea
;;    capital letter = java class 
;;    CapitalLetter + "." = contructor of the java class
;;    - creates new object
;;    - Integer : entier en 32bits  ==> max is 2.10^9 : not that much with advent of code and no warning/error
;; 
;; better solve : integer 64bit
;; Long (don't use "Long." because again new object)
;;    parse-long


(defn mainD1
  []
  (println "Hello, World!")
  (println "Part 1")
  (println input)
  (println (sol1 input))
  (println "With my input:")
  (println (sol1 myinput))
  (println "\nPart 2")
  (println input2)
  (println (sol2 input2))
  (println "With my input:")
  (println (sol2 myinput))
  (println (sol2 (slurp "input/day1.txt"))))

  ;; (println "2two1sevenine")
  ;; (println (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" "2two1sevenine"))
  ;; (println (map maptonumber 
  ;;               [(re-find #"\d|one|two|three|four|five|six|seven|eight|nine" "2two1sevenine") (re-find #"\d|eno}owt|eerht|ruof|evif|xis|neves|thgie|enin" (str/reverse "2two1sevenine"))]))


;; 55427 too high

  ;; (println "pqr3stu8vwx")
  ;; (println ((fn [l] (Integer. (str (re-find #"\d" l) (re-find #"\d" (str/reverse l))))) "pqr3stu8vwx"))
  ;; (println (slurp ))


(def inputd2
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn iscolorok?
  [color numb cubes]
  (reduce (fn [a b] (and a b)) true
          (map (fn [b] (<= (parse-long (re-find #"\d+" b)) numb))
               (re-seq (re-pattern (str "\\d+ " color)) cubes))))
;; reduce needs a function of two elements
;; `and` is _not_ a function so we have to define one
;; we set `true` as a starting point in case the sequence has only one element

(defn d2part1
  [input]
  (apply +
         (map (fn [[g c]] (if
                           (and
                            (iscolorok? "red" 12 c)
                            (iscolorok? "green" 13 c)
                            (iscolorok? "blue" 14 c))
                            (parse-long (re-find #"\d+" g))
                            0))
              (map (fn [l] (str/split l #":" 2)) (str/split-lines input)))))
;; use destructuring 
;; fn [input]
;; input is [game color]
;; so : fn[[game color]]


(defn mincolor
  [color cubes]
  (reduce (fn [a b] (if (> a b) a b)) 0
          (map (fn [b] (parse-long (re-find #"\d+" b)))
               (re-seq (re-pattern (str "\\d+ " color)) cubes))))
;; note : max is a function that exists ^^

(defn d2part2
  [input]
  (apply +
         (map (fn [ll] (* (mincolor "red" (last ll))
                          (mincolor "green" (last ll))
                          (mincolor "blue" (last ll))))
              (map (fn [l] (str/split l #":" 2)) (str/split-lines input)))))

(def cubegame4
  "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")

(defn mainD2
  []
  (println "Day 2 - cube game")
  (println "~~~ part1 ~~~")
  (println (d2part1 inputd2))
  ;; (println (iscolorok? "blue" 14 "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"))
  (println (d2part1 (slurp "input/day2.txt")))
  (println "~~~ part2 ~~~")
  ;; (println "Game 4: " cubegame4)
  ;; (println "blue : " (mincolor "blue" cubegame4))
  ;; (println "green : " (mincolor "green" cubegame4))
  ;; (println "Game 4 power : " (* (mincolor "blue" cubegame4) (mincolor "green" cubegame4) (mincolor "red" cubegame4)))
  (println (d2part2 inputd2))
  (println (d2part2 (slurp "input/day2.txt"))))

(defn mainD0
  []
  (println "no day 0!"))



(def day3sample
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn get-all-num
  ([idx-line line]
   (let [num (re-find #"\d+" line)]
     (if num
       (get-all-num
        idx-line
        (seq (vector [idx-line (str/index-of line num) (+ (str/index-of line num) (count num) -1) (parse-long num)]))
        (str/replace-first line (re-pattern num) (str/join "" (for [_ (range (count num))] "."))))
       [(seq ()) line])))
  ([idx-line seq-of-num line]
   (let [num (re-find #"\d+" line)]
     (if num
       (get-all-num
        idx-line
        (conj seq-of-num [idx-line (str/index-of line num) (+ (str/index-of line num) (count num) -1) (parse-long num)])
        (str/replace-first line (re-pattern num) (str/join "" (for [_ (range (count num))] "."))))
       [seq-of-num line]))))

(defn get-symbols
  ([idx-line line]
   (let [sym (re-find #"[^\.]" line)]
     (if sym
       (get-symbols
        idx-line
        (seq (vector [idx-line (str/index-of line sym)]))
        (str/replace-first line sym "."))
       (seq ()))))
  ([idx-line seq-sym line]
   (let [sym (re-find #"[^\.]" line)]
     (if sym
       (get-symbols
        idx-line
        (conj seq-sym [idx-line (str/index-of line sym)])
        (str/replace-first line sym "."))
       seq-sym))))


(defn get-stars
  ([idx-line line]
   (let [sym (re-find #"\*" line)]
     (if sym
       (get-stars
        idx-line
        (seq (vector [idx-line (str/index-of line sym)]))
        (str/replace-first line sym "."))
       (seq ()))))
  ([idx-line seq-sym line]
   (let [sym (re-find #"\*" line)]
     (if sym
       (get-stars
        idx-line
        (conj seq-sym [idx-line (str/index-of line sym)])
        (str/replace-first line sym "."))
       seq-sym))))

(defn day3part1
  [input]
  (let [lines (str/split-lines input)
        all-num-and-sym (map-indexed
                         (fn [idx-line line] ((fn [[nums line]] [nums (get-symbols idx-line line)]) (get-all-num idx-line line)))
                         lines)
        nums (apply concat (apply conj [] (map first all-num-and-sym)))
        syms (apply concat (apply conj [] (map last all-num-and-sym)))]
    ;; (println nums)
    ;; (println syms)
    ;; (println (map
    ;;           (fn [[idx-line xmin xmax value]]
    ;;             (vector value (filter (fn [[i-line-s x-s]]
    ;;                                     (and
    ;;                                      (or (= i-line-s idx-line) (= (+ i-line-s 1) idx-line) (= (- i-line-s 1) idx-line))
    ;;                                      (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
    ;;                                   syms)))
    ;;           nums))
    (apply + (map
              (fn [[idx-line xmin xmax value]]
                (if (> (count (filter (fn [[i-line-s x-s]]
                                        (and
                                         (or (= i-line-s idx-line) (= (+ i-line-s 1) idx-line) (= (- i-line-s 1) idx-line))
                                         (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
                                      syms)) 0)
                  value 0))
              nums))))



(defn day3part2
  [input]
  (let [lines (str/split-lines input)
        all-num-and-sym (map-indexed
                         (fn [idx-line line] ((fn [[nums line]] [nums (get-stars idx-line line)]) (get-all-num idx-line line)))
                         lines)
        nums (apply concat (apply conj [] (map first all-num-and-sym)))
        syms (apply concat (apply conj [] (map last all-num-and-sym)))]
    ;; (println nums)
    ;; (println syms)
    (apply + (map last
                  (filter
                   (fn [[n _]] (= n 2))
                   (map (fn [[idx-line x-s]]
                          (let [neighbors (filter
                                           (fn [[i-line xmin xmax value]]
                                             (and (or (= idx-line i-line) (= idx-line (+ i-line 1)) (= idx-line (- i-line 1)))
                                                  (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
                                           nums)]
                            (vector (count neighbors) (apply * (map last neighbors)))))
                        syms))))))

(defn mainD3
  []
  (println "Day 3 - engine schematics")
  (println day3sample)
  (println "part 1")
  (println (day3part1 day3sample))
  (println (day3part1 (slurp "input/day3.txt")))
  (println "part 2 - gear power")
  (println (day3part2 day3sample))
  (println (day3part2 (slurp "input/day3.txt"))))

(def d4sample
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn cardvalue
  [value n-match]
  (if (and n-match (> n-match 0))
    (if (= value 0)
      (cardvalue 1 (- n-match 1))
      (cardvalue (* value 2) (- n-match 1)))
    value))

(defn count-matchs
  [[win have]]
  (apply +
         (map (fn [h] (if (nil? (some #(= h %) win)) 0 1)) have)))

(defn d4part1
  [input]
  (let [lines (str/split-lines input)]
    (apply +
           (map (fn [l] (let [[win have] (str/split (last (str/split l #"\:")) #"\|")]
                          (cardvalue 0 (count-matchs (vector (re-seq #"\d+" win) (re-seq #"\d+" have)))))) lines))))

(defn get-card-values
  [line]
  (let [[card values] (str/split line #"\:")
        card-id (parse-long (re-find #"\d+" card))
        [win have] (map (fn [s] (re-seq #"\d+" s)) (str/split values #"\|"))]
    {:card-id card-id  :win win :have have}))


(defn count-cards
  ([dict-cards]
   (count-cards
    0 ;; N-cards
    (range 1 (+ (count dict-cards) 1)) ;; list-cards
    dict-cards))
  ([N-cards list-cards dict-cards]
   (if (> (count list-cards) 0)
     (let [card-id (apply min list-cards)
           new-list-cards (filter #(> % card-id) list-cards)
           n-cards (- (count list-cards) (count new-list-cards))
           card (first (get dict-cards card-id))
           N-matchs (count-matchs (vector (:win card) (:have card)))]

       (println card-id n-cards
                ;;    card N-matchs
                ;; (concat 
                ;;   (apply concat (repeat n-cards (filter #(<= % (count dict-cards)) (range (+ 1 card-id) (+ 1 card-id N-matchs)))))
                ;;   new-list-cards))
                )
       (count-cards
        (+ N-cards n-cards)
        (concat
         (apply concat (repeat n-cards (filter #(<= % (count dict-cards)) (range (+ 1 card-id) (+ 1 card-id N-matchs)))))
         new-list-cards)
        dict-cards))
     N-cards)))

;; could have try to use SWAP! to keep track of the number of each cards
;; nope ! seems swap is more complexe than that or you need to work with atoms
;; Care (update map key function(with previous value)) --> does not update the map but creates a new one

(defn d4part2
  [input]
  (let [lines (str/split-lines input)
        dict-cards (group-by :card-id (map get-card-values lines))]
    (count-cards dict-cards)))

(defn mainD4
  []
  (println "Day 4 - scratch cards")
  (println d4sample)
  (println (d4part1 d4sample))
  (println (d4part1 (slurp "input/day4.txt")))
  (println "part 2 - count cards")
  (println (d4part2 d4sample))
  (println (d4part2 (slurp "input/day4.txt"))))


(def d8sample1 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def d8sample2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn dir->fct
  [dir]
  (map (fn [d] (cond
                 (= d \R) last
                 (= d \L) first
                 :else (println "\ta letter is not L or R!"))) dir))

;; \R is not the same as "R" but should be use when comparing letters (?)

(defn dict-of-nodes
  [nodes]
  (apply
   sorted-map
   (mapcat
    (fn [l] (let [[k v] (str/split l #" = ")]
              [k (re-seq #"\w{3}" v)]))
    (str/split-lines nodes))))
;; this gives a map from nodes to nodes. the keys of the maps are 'symbol' (not 'keyword')
;;      might need to use (keyword XXX) to access the values

(defn d8part1
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)]
    (newline)
    (println (first dir))
    (loop
     [step 0 node "AAA"]
      (when (= (mod step 1000) 0)
        (newline)
        (println step "," node "-->" (get nodesmap node))
        (println "               " ((nth (apply concat (repeat dirfct)) step) ["L" "R"])))
      (if (= node "ZZZ")
        step
        (recur (inc step) ((nth (apply concat (repeat dirfct)) step) (get nodesmap node)))))))


(def d8sample3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
")

(defn d8part2bruteforce
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)
        startnodes (filter #(= \A (last %)) (keys nodesmap))
        ;; endnodes (filter #(= \Z (last %)) (keys nodesmap))
        ]
    (loop
      [step 0 nodes startnodes]
      (when (= (mod step 1000) 0)
        (newline)
        (println step "," nodes))
      (if (every? #(= \Z (last %)) nodes)
          step
          (recur (inc step) (map (fn [node] ((nth dirfct (mod step (count dirfct))) (get nodesmap node))) nodes))))
  ))


(defn path-to-next-Z
  [node tot-steps nodesmap dirfct]
  (loop [step 0 n node]
    (if (and (> step 0) (= \Z (last n)))
      [n step]
      (recur (inc step) ((nth dirfct (mod (+ step tot-steps) (count dirfct))) (get nodesmap n))))))


;; Taken from here : https://rosettacode.org/wiki/Least_common_multiple
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))


(defn d8part2
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)
        startnodes (filter #(= \A (last %)) (keys nodesmap))
        ;; endnodes (filter #(= \Z (last %)) (keys nodesmap))
        Ndir (count dirfct)
        ;; one-node (nth startnodes 3)
        ]
    (apply lcmv (for [one-node startnodes]
    ;; (println one-node "-" 0 "-" 0)
      (loop
        [iter 0
         node one-node 
         tot-steps 0
         all-paths {}]
        (let [[new-node add-steps] (path-to-next-Z node tot-steps nodesmap dirfct)
              new-tot-steps (+ tot-steps add-steps)]
          (println new-node "-" (mod new-tot-steps Ndir) "-" new-tot-steps)
          (cond
            (> iter 10) (println "too many iter")
            (and (= node new-node) (= (mod new-tot-steps Ndir) (mod tot-steps Ndir))) add-steps #_(println "min:" tot-steps "- step:" add-steps)
            :else (recur (inc iter) new-node new-tot-steps all-paths))))))))


(defn mainD8
    []
    (println "Day 8 - sandstorm")
  ;; (println d8sample1)
  ;; (println (d8part1 d8sample1))
  ;; (newline)
  ;; (println d8sample2)
  ;; (println (d8part1 d8sample2))
  ;; (newline)
  ;; (println (d8part1 (slurp "input/day8.txt")))
    (newline)
    (println "part 2 - multi time-space ghost walking")
    ;; (println d8sample3)
    ;; (println (d8part2 d8sample3))
    (newline)
    (println (d8part2 (slurp "input/day8.txt"))))


(def d5sample1
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse-one-map
  [[txt & nums]]
  (let [[start end] (str/split (str/replace txt " map:" "") #"-to-")]
    [start [end (map #(map parse-long (re-seq #"-?\d+" %)) nums)]]))

(defn parse-input
  [input]
  (let [groups (str/split input #"\n\n")
        seeds (map parse-long (re-seq #"\d+" (first groups)))]
    [seeds (reduce #(assoc %1 (first %2) (last %2)) {} (map #(parse-one-map (str/split-lines %)) (rest groups)))]
  ))


(defn sold5p1-allsteps
  [value value-name maps all-steps]
  #_(println value value-name (get maps value-name))
  (if (= value-name "location")
    all-steps
    (let [[next-value-name nums] (get maps value-name)
          this-map (first (filter #(and (>= value (second %) ) (< (- value (second %)) (last %))) nums))]
      (if this-map
        (let [new-value (+ (first this-map) (- value (second this-map)))]
          (sold5p1-allsteps new-value next-value-name maps (conj all-steps new-value)))
        (sold5p1-allsteps value next-value-name maps (conj all-steps value))))
  ))

(defn sold5p1
  [value value-name maps ]
  #_(println value value-name (get maps value-name))
  (if (= value-name "location")
    value
    (let [[next-value-name nums] (get maps value-name)
          this-map (first (filter #(and (>= value (second %)) (< (- value (second %)) (last %))) nums))]
      (if this-map
        (let [new-value (+ (first this-map) (- value (second this-map)))]
          (sold5p1 new-value next-value-name maps))
        (sold5p1 value next-value-name maps)))))


(defn d5part1
  [input]
  (let [[seeds maps] (parse-input input)]
    (apply min (map #(sold5p1 % "seed" maps) seeds))))


(defn range-transform-with-one-map
  [this-range this-map]
  #_(println "     " this-range  this-map)
  (let [[start size] this-range
        [map-start-new map-start-old map-size] this-map
        end (+ start size -1)
        map-end-old (+ map-start-old map-size -1)]
      (cond
        (< end map-start-old) [[this-range] []] ;; outside of this range
        (> start map-end-old) [[this-range] []] ;; also outside of this range
        (< start map-start-old) ;; it starts before the map but end in of after
          (map conj 
               [[] []]
               [[start (+ map-start-old (- start))] []]
               (let [l (range-transform-with-one-map [map-start-old (- size (+ map-start-old (- start)))] this-map)] [(first (first l)) (last (last l))]))
        (<= end map-end-old) ;; all the range in the map
          [[] [[(+ map-start-new (- start map-start-old)) size]]]
        :else  ;; start in range, ends outside
          (let [size-in-map (inc (- map-end-old start))]
            [[[(inc map-end-old) (- size size-in-map)]] [[(+ map-start-new (- start map-start-old)) size-in-map]]])

        )))

(defn range-transf-for-reduce
  [old-and-new-ranges this-map]
  (let [old-ranges (filter #(> (count %) 0) (first old-and-new-ranges))
        new-ranges (filter #(> (count %) 0) (second old-and-new-ranges))]
    (println "   " old-ranges new-ranges)
    (loop [to-do-old-ranges old-ranges
           done-old-ranges []
           new-ranges new-ranges]
      (if (zero? (count to-do-old-ranges))
        [done-old-ranges new-ranges]
        (let [[output-old output-new] (range-transform-with-one-map (first to-do-old-ranges) this-map)]
          (recur (rest to-do-old-ranges) (concat done-old-ranges output-old) (concat new-ranges output-new))))
    )))



(defn d5part2
  [input]
  (let [[seeds maps] (parse-input input)
        seed-ranges (partition 2 seeds)]
    (apply min (map #(first %)
      (loop [this-ranges seed-ranges
             this-stage "seed"]
        (println " " this-stage this-ranges)
        (println "  maps" (get maps this-stage))
        (if (= this-stage "location")
          this-ranges
          (let [one-map (get maps this-stage)
                next-stage (first one-map)
                this-stage-maps (last one-map)]
            (recur (apply concat (reduce range-transf-for-reduce [this-ranges []] this-stage-maps)) next-stage)))
    )))))

;; I neede the prints to avoid "StackOverFlowError"
;; because otherwise a lot of stuff is lazy : it is stored as a stack of fucntions rather than in memory
;; calling the prints force it out of lazy form into memory and the stack (which is relatively speaking small) is not overflowed
;; forcing out of lazy can also be done with (vec )


(defn mainD5 
  []
  (println "Day 5 - from seed to soil to location")
  (println "part 1 - closest location")
  (println d5sample1)
  (newline)
  (println (d5part1 d5sample1))
  (newline)
  #_(println (d5part1 (slurp "input/day5.txt")))
  (println "part 2 - all the seeds because range")
  (println (d5part2 d5sample1))
  ;; (println (range-transform-with-one-map '(97 2) '(50 98 2)))
  ;; (println (reduce range-transf-for-reduce [['(15 2) '(95 20)] []] ['(50 98 2) '(0 102 2)]))
  (println (d5part2 (slurp "input/day5.txt")))
  )




(def d9sample1 "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn get-diffs 
  [nums max-values]
  (if (every? zero? nums)
    max-values
    (get-diffs (map - (rest nums) (drop-last 1 nums)) (conj max-values (last nums)) )))

(defn d9part1
  [input]
  (let [lines (str/split-lines input)]
    (apply + (map (fn [l] (apply + (get-diffs (map parse-long (re-seq #"-?\d+" l)) []))) lines))))


(defn get-diffs-part2
  [nums max-values step-num]
  (if (every? zero? nums)
    max-values
    (get-diffs-part2 (map - (rest nums) (drop-last 1 nums))
                     (conj max-values (if (even? step-num) (first nums) (* (first nums) -1)))
                     (inc step-num))))

(defn d9part2
  [input]
  (let [lines (str/split-lines input)]
    (apply + (map (fn [l] (apply + (get-diffs-part2 (map parse-long (re-seq #"-?\d+" l)) [] 0))) lines))))

(defn mainD9
  []
  (println "Day 9")
  (println d9sample1)
  (println (d9part1 d9sample1))
  (newline)
  #_(println (d9part1 (slurp "input/day9.txt")))
  (println "part2")
  (println (d9part2 d9sample1))
  (println (d9part2 (slurp "input/day9.txt"))))

;;  1861136893 wrong => was missing "-" in number detection

(def d10sample1
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def day10sample2
"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(defn d10-parse-lines
  [lines]
  (map-indexed (fn [y l] (map-indexed (fn [x c] [x y c]) l)) lines))

(defn connected-tiles
  [[x y s]]
  (cond
    (= s \|) [[x (inc y)] [x (dec y)]]
    (= s \-) [[(inc x) y] [(dec x) y]]
    (= s \L) [[x (dec y)] [(inc x)  y]]
    (= s \J) [[x (dec y)] [(dec x) y]]
    (= s \7) [[(dec x) y] [x (inc y)]]
    (= s \F) [[(inc x) y] [x (inc y)]]
  ))


(defn get-tile
  [[x y] all-points]
  (first (filter #(= [x y] [(first %) (second %)]) all-points)))

(defn around-S
  [all-points]
  (let [[xs ys _] (first (filter #(= \S (last %)) all-points))
        options (map (fn [pos] (get-tile pos all-points)) [[xs (inc ys)] [xs (dec ys)] [(inc xs) ys] [(dec xs) ys]])
        connected (map connected-tiles options)
        ]
    (map 
      first 
      (filter 
        (fn [[_ c]] (some true? c))
        (map (fn [o c] [o (map #(= % [xs ys]) c)]) options connected)))))

(defn get-next-tile
  [[xp yp _] tile all-points]
  (let [[c1 c2] (connected-tiles tile)]
    (cond
      (= c1 [xp yp]) (get-tile c2 all-points)
      (= c2 [xp yp]) (get-tile c1 all-points)))
  )

(defn d10p1steps
  [step prevtiles tiles all-points]
  (let [[p1 p2] prevtiles
        [t1 t2] tiles]
    (when (= 0 (mod step 1000)) (prn step tiles))
    (if
      (or (= t1 t2) (= p1 t1) (= p1 t2) (= p2 t1) (= p2 t2))
      step
      (recur (inc step) tiles [(get-next-tile p1 t1 all-points) (get-next-tile p2 t2 all-points)] all-points))))

(defn d10part1
  [input]
  (let [lines (str/split-lines input)
        all-points (apply concat (d10-parse-lines lines))
        S (first (filter #(= \S (last %)) all-points))
        S-connected-tiles (around-S all-points)]
    #_(prn all-points)
    (prn S)
    (prn S-connected-tiles)
    (d10p1steps 1 [S S] S-connected-tiles all-points)))

(def d10sample3 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
")

;; only left going pipe can be at the right of S
(defn S-is-actually
  [[xs ys _] [x1 y1 _] [x2 y2 _]]
  (let [cand-right (when (or (= [x1 y1] [(inc xs) ys]) (= [x2 y2] [(inc xs) ys])) (set [\- \L \F]))
        cand-top (when (or (= [x1 y1] [xs (dec ys)]) (= [x2 y2] [xs (dec ys)])) (set [\| \L \J]))
        cand-left (when (or (= [x1 y1] [(dec xs) ys]) (= [x2 y2] [(dec xs) ys])) (set [\- \J \7]))
        cand-bot (when (or (= [x1 y1] [xs (inc ys)]) (= [x2 y2] [xs (inc ys)])) (set [\| \7 \F]))
        ]
    (first (apply clojure.set/intersection (keep  identity [cand-right cand-top cand-left cand-bot])))
))

(defn d10findloop
  [prevtile tile all-tiles S all-points]
  (let [next-tile (get-next-tile prevtile tile all-points)]
    (when (= 0 (mod (count all-tiles) 1000)) (prn (count all-tiles) tile))
    (if
      (= next-tile S)
      all-tiles
      (recur tile next-tile (conj all-tiles next-tile) S all-points))))


(defn all-inside-candidate-col
  [[list-candidates open previous-y] [x y s]]
  (prn y list-candidates open previous-y s)
  (cond
    (= s \|) [list-candidates open y]
    (or (= s \L) (= s \J)) [list-candidates true y]
    (or (= s \F) (= s \7)) (if open [(concat list-candidates (map #(vector % x) (range (inc previous-y) y))) false y] [list-candidates false y])
    (= s \-) (if open [(concat list-candidates (map #(vector x %) (range (inc previous-y) y))) false y] [list-candidates true y])
    ))

#_(defn all-inside-candidate-lig
  [[list-candidates open previous-x previous-s] [x y s]]
  ;; (prn y list-candidates open previous-y s)
  (if 
    open
    (cond
      (= s \-) [list-candidates open x previous-s] ;; I need  to propagate the information about how I "open" the first time
      (= s \|) [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) false x s]
      (or (= s \F) (= s \L)) [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) true x s]
      )
    (cond
      (= s \-) [list-candidates open x previous-s]
      (= s \|) [list-candidates true x s]
      (or (= s \F) (= s \L) (= s \|)) [list-candidates true x s]
      (= s \J) []
      (or (= s \7) ) [list-candidates true x]
       (if open [(concat list-candidates (map #(vector % x) (range (inc previous-y) y))) false y] [list-candidates false y])
      (= s \|) (if open [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) false x] [list-candidates true x]))))

(defn transform-path-y
  [path-y clean-path-y]
  (if
    (= (count path-y) 0)
    clean-path-y
    (let [[x1 _ s1] (first path-y)
          path-y (rest path-y)]
      (if
        (= s1 \|)
        (recur path-y (conj clean-path-y [s1 x1 x1]))
        (let [[x2 _ s2] (first path-y)
              path-y (rest path-y)]
          (if
            (or (and (= s1 \L) (= s2 \J)) (and (= s1 \F) (= s2 \7)))
            (recur path-y clean-path-y)
            (recur path-y (conj clean-path-y [(str s1 s2) x1 x2]))
            ))
      )
      ))
  )

(defn xy-inside
  [y elems]
  (if 
    (zero? (count elems))
    []
    (let [eles (partition 2 (sort-by second elems))]
      (map (fn [x] [x y]) (mapcat (fn [[[_ start _][_ _ end]]] (range (inc start) end)) eles)))
    ))

(defn d10part2
  [input]
  (let [lines (str/split-lines input)
        all-points (apply concat (d10-parse-lines lines))
        S (first (filter #(= \S (last %)) all-points))
        S-connected-tiles (around-S all-points)
        s-symb (S-is-actually S (first S-connected-tiles) (second S-connected-tiles))
        all-points (map (fn [[x y s]] [x y (if (= s \S) s-symb s)]) all-points)
        S [(first S) (second S) s-symb]
        all-path-points (d10findloop S (first S-connected-tiles) [S (first S-connected-tiles)] S all-points)
        ;; path-by-y (group-by second (d10findloop S (first S-connected-tiles) [S (first S-connected-tiles)] S all-points))
        transformed-paths (apply sorted-map (mapcat (fn [[k v]] [k (transform-path-y (sort-by first (filter #(not= (last %) \-) v)) [])]) (group-by second all-path-points)))
        candidates (set (mapcat (fn [[k v]] (xy-inside k v)) transformed-paths))]
    (prn S)
    ;; (prn all-path-points)
    ;; ;; (prn (sort-by first (map (fn [[k v]] [k (reduce count-inside [0 false -1] (sort-by second v))]) path)))
    ;; ;; (prn transformed-paths)
    ;; ;; (prn (xy-inside 6 (get transformed-paths 6) ))
    ;; (prn candidates)
    ;; (prn (reduce count-inside [0 false -1] (sort-by second (get path 16))))
    ;; (apply + (map (fn [[_ v]] (first (reduce count-inside [0 false -1] (sort-by second v)))) path))
    (count (cljset/difference candidates (set (map (fn [[x y _]] [x y]) all-path-points))))
  ))

(def d10sample4
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(defn mainD10
  []
  (println "Day 10")
  ;; (println d10sample1)
  ;; (println (d10part1 d10sample1))
  ;; (newline)
  ;; (println (d10part1 day10sample2))
  ;; (newline)
  ;; (println (d10part1 (slurp "input/day10.txt")))
  (println "part2")
  (println d10sample3)
  (prn (d10part2 d10sample3))
  (println d10sample4)
  (prn (d10part2 d10sample4))
  (newline)
  (println (d10part2 (slurp "input/day10.txt"))))



(def d11sample1 "")

(defn d11part1
  [input]
  (let [lines (str/split-lines input)]
    lines))

(defn mainD11
  []
  (println "Day 11")
  (println d11sample1)
  (println (d11part1 d11sample1)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [available-days {0 mainD0
                        1 mainD1
                        2 mainD2
                        3 mainD3
                        4 mainD4
                        5 mainD5
                        8 mainD8
                        9 mainD9
                        10 mainD10
                        11 mainD11}

        this-day (parse-long (first args))]
    (if
      (contains? available-days this-day)
      ((get available-days this-day))
      ((get available-days (apply max (keys available-days)))))))
