module Countries exposing (Countries, Country, GeoCoords, Scale(..),
    HighlightMethod(..), init, next)

import Dict exposing (Dict)

import Random exposing (Generator)

type alias GeoCoords = { latitude : Float, longitude : Float }

type Scale = Small | Medium | Large

type HighlightMethod = Fill | Dot | SmallDots (List GeoCoords)

type alias Country =
    { code : String
    , latitude : Float
    , longitude : Float
    , scale : Scale
    , highlightMethod : HighlightMethod
    }

type Countries = Countries { recents : List (Int, Country) }

geoCoords : Float -> Float -> GeoCoords
geoCoords latitude longitude =
    { latitude = latitude, longitude = longitude }

maxRecentsLength : Int
maxRecentsLength = 10

init : Countries
init = Countries { recents = [] }

next : Countries -> Generator (Maybe Country, Countries)
next (Countries { recents }) =
    let
        candidates = Dict.diff countries (Dict.fromList recents)
        fillHoles random = List.filter (Tuple.first >> (>=) random) recents
            |> List.length
            |> (+) random
        toCountries random =
            let
                index = fillHoles random
            in case Dict.get index countries of
                Just country ->
                    ( Just country
                    , Countries
                        { recents = (index, country) :: recents
                            |> List.take maxRecentsLength
                        }
                    )
                Nothing -> ( Nothing, Countries { recents = recents } )
    in Random.int 0 (Dict.size candidates - 1)
        |> Random.map toCountries

countries : Dict Int Country
countries = Dict.fromList <| List.indexedMap Tuple.pair
    [ { code = "af"
      , latitude = 33.924150466918945
      , longitude = 67.6884994506836
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "am"
      , latitude = 40.079999923706055
      , longitude = 45.012001037597656
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "az"
      , latitude = 40.14484977722168
      , longitude = 47.56700134277344
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "bh"
      , latitude = 26.026599884033203
      , longitude = 50.53450012207031
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "bd"
      , latitude = 23.68094825744629
      , longitude = 90.32749938964844
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bt"
      , latitude = 27.506399154663086
      , longitude = 90.4110107421875
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "bn"
      , latitude = 4.523200988769531
      , longitude = 114.69549560546875
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "kh"
      , latitude = 12.558151245117188
      , longitude = 104.96250915527344
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "cn"
      , latitude = 35.88694953918457
      , longitude = 104.17950439453125
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "cy"
      , latitude = 35.1158504486084
      , longitude = 33.428497314453125
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "ge"
      , latitude = 42.32000160217285
      , longitude = 43.32550048828125
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "in"
      , latitude = 21.12230110168457
      , longitude = 82.7544937133789
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "id"
      , latitude = -2.5015029907226562
      , longitude = 118.09150695800781
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ir"
      , latitude = 32.43534851074219
      , longitude = 53.66399383544922
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "iq"
      , latitude = 33.21780014038086
      , longitude = 43.660003662109375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "il"
      , latitude = 31.44649887084961
      , longitude = 35.066497802734375
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "jp"
      , latitude = 37.875450134277344
      , longitude = 137.24099731445312
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "jo"
      , latitude = 31.281349182128906
      , longitude = 37.12200164794922
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "kz"
      , latitude = 47.99909973144531
      , longitude = 66.96599578857422
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "kp"
      , latitude = 40.358551025390625
      , longitude = 127.51800537109375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "kr"
      , latitude = 36.45989990234375
      , longitude = 127.79049682617188
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "kw"
      , latitude = 29.315250396728516
      , longitude = 47.48650360107422
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "kg"
      , latitude = 41.22394943237305
      , longitude = 74.73750305175781
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "la"
      , latitude = 18.208251953125
      , longitude = 103.88400268554688
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "lb"
      , latitude = 33.87719917297363
      , longitude = 35.84700012207031
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "my"
      , latitude = 4.106849670410156
      , longitude = 109.45599365234375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "mv"
      , latitude = 3.7385520935058594
      , longitude = 73.45500183105469
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords 4.201450347900391 73.50050354003906
            , geoCoords 3.259601593017578 73.4124984741211
            ]
      }
    , { code = "mn"
      , latitude = 46.85639953613281
      , longitude = 103.82051086425781
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mm"
      , latitude = 19.196197509765625
      , longitude = 96.66349792480469
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "np"
      , latitude = 28.373899459838867
      , longitude = 84.10699462890625
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "om"
      , latitude = 21.502349853515625
      , longitude = 55.907997131347656
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "pk"
      , latitude = 30.395051956176758
      , longitude = 68.94600677490234
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ps"
      , latitude = 31.87135124206543
      , longitude = 34.88500213623047
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "ph"
      , latitude = 12.950752258300781
      , longitude = 121.781494140625
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "qa"
      , latitude = 25.358949661254883
      , longitude = 51.18199920654297
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "sa"
      , latitude = 24.248151779174805
      , longitude = 45.128501892089844
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sg"
      , latitude = 1.3562507629394531
      , longitude = 103.822998046875
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "lk"
      , latitude = 7.881050109863281
      , longitude = 80.79251098632812
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sy"
      , latitude = 34.80729866027832
      , longitude = 39.061500549316406
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tw"
      , latitude = 23.6009521484375
      , longitude = 121.00048828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tj"
      , latitude = 38.859548568725586
      , longitude = 71.23450469970703
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "th"
      , latitude = 13.030601501464844
      , longitude = 101.50749206542969
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tl"
      , latitude = -8.913799285888672
      , longitude = 126.10549926757812
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tr"
      , latitude = 38.962350845336914
      , longitude = 35.243003845214844
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tm"
      , latitude = 38.97464942932129
      , longitude = 59.561500549316406
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ae"
      , latitude = 24.3448486328125
      , longitude = 53.977996826171875
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "uz"
      , latitude = 41.363800048828125
      , longitude = 64.5564956665039
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "vn"
      , latitude = 15.964248657226562
      , longitude = 105.7860107421875
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ye"
      , latitude = 15.801898956298828
      , longitude = 47.81749725341797
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "al"
      , latitude = 41.150699615478516
      , longitude = 20.17650604248047
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "ad"
      , latitude = 42.53860092163086
      , longitude = 1.5774993896484375
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "at"
      , latitude = 47.70039939880371
      , longitude = 13.337501525878906
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "by"
      , latitude = 53.705400466918945
      , longitude = 27.9425048828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "be"
      , latitude = 50.500999450683594
      , longitude = 4.444496154785156
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "ba"
      , latitude = 43.91814994812012
      , longitude = 17.660499572753906
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "bg"
      , latitude = 42.740699768066406
      , longitude = 25.464500427246094
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "hr"
      , latitude = 44.48374938964844
      , longitude = 16.458999633789062
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "cz"
      , latitude = 49.80699920654297
      , longitude = 15.46099853515625
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "dk"
      , latitude = 56.182899475097656
      , longitude = 11.628997802734375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ee"
      , latitude = 58.58225059509277
      , longitude = 25.00250244140625
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "fi"
      , latitude = 64.94040012359619
      , longitude = 25.527999877929688
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "fr"
      , latitude = 46.24099922180176
      , longitude = 2.397003173828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "de"
      , latitude = 51.16875076293945
      , longitude = 10.4375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gr"
      , latitude = 38.33915138244629
      , longitude = 23.938995361328125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "va"
      , latitude = 41.901899337768555
      , longitude = 12.433494567871094
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "hu"
      , latitude = 47.153249740600586
      , longitude = 19.485000610351562
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "is"
      , latitude = 64.96640014648438
      , longitude = -19.01599884033203
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ie"
      , latitude = 53.41975021362305
      , longitude = -8.20849609375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "it"
      , latitude = 41.88495063781738
      , longitude = 12.556999206542969
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "lv"
      , latitude = 56.86544990539551
      , longitude = 24.608497619628906
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "li"
      , latitude = 47.164100646972656
      , longitude = 9.544998168945312
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "lt"
      , latitude = 55.15210151672363
      , longitude = 23.837997436523438
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "lu"
      , latitude = 49.80634880065918
      , longitude = 6.109504699707031
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "mt"
      , latitude = 35.948001861572266
      , longitude = 14.372993469238281
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "md"
      , latitude = 46.96405029296875
      , longitude = 28.375
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "mc"
      , latitude = 43.75130081176758
      , longitude = 7.408500671386719
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "me"
      , latitude = 42.7056999206543
      , longitude = 19.392005920410156
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "nl"
      , latitude = 52.187950134277344
      , longitude = 5.273506164550781
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "mk"
      , latitude = 41.60404968261719
      , longitude = 21.727500915527344
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "no"
      , latitude = 64.58150005340576
      , longitude = 17.87999725341797
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "pl"
      , latitude = 51.929500579833984
      , longitude = 19.11750030517578
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "pt"
      , latitude = 39.571401596069336
      , longitude = -7.846000671386719
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ro"
      , latitude = 45.96714973449707
      , longitude = 24.9739990234375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ru"
      , latitude = 61.526750564575195
      , longitude = 104.93740177154541
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "sm"
      , latitude = 43.9419002532959
      , longitude = 12.456001281738281
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "rs"
      , latitude = 44.01150131225586
      , longitude = 20.908004760742188
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sk"
      , latitude = 48.68054962158203
      , longitude = 19.701004028320312
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "si"
      , latitude = 46.14584922790527
      , longitude = 14.947006225585938
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "es"
      , latitude = 39.89525032043457
      , longitude = -2.9644927978515625
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "se"
      , latitude = 62.191650390625
      , longitude = 17.6510009765625
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ch"
      , latitude = 46.80280113220215
      , longitude = 8.212501525878906
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "ua"
      , latitude = 48.79384994506836
      , longitude = 31.1300048828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gb"
      , latitude = 54.335700035095215
      , longitude = -3.199005126953125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "dz"
      , latitude = 28.039501190185547
      , longitude = 1.6425018310546875
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ao"
      , latitude = -11.224449157714844
      , longitude = 17.894996643066406
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bj"
      , latitude = 9.300296783447266
      , longitude = 2.2985000610351562
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "bw"
      , latitude = -22.320999145507812
      , longitude = 24.671005249023438
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bf"
      , latitude = 12.251300811767578
      , longitude = -1.5674972534179688
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bi"
      , latitude = -3.3844528198242188
      , longitude = 29.912506103515625
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "cv"
      , latitude = 16.005950927734375
      , longitude = -24.012001037597656
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords 16.99394989013672 -25.11449432373047
            , geoCoords 14.92824935913086 -24.40650177001953
            , geoCoords 16.585453033447266 -24.215499877929688
            , geoCoords 15.122299194335938 -23.45050048828125
            , geoCoords 16.725799560546875 -22.939498901367188
            , geoCoords 16.111648559570312 -22.82050323486328
            ]
      }
    , { code = "cm"
      , latitude = 7.377349853515625
      , longitude = 12.358001708984375
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "cf"
      , latitude = 6.633152008056641
      , longitude = 20.91699981689453
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "td"
      , latitude = 15.460250854492188
      , longitude = 18.715499877929688
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "km"
      , latitude = -11.867996215820312
      , longitude = 43.87699890136719
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords -11.641498565673828 43.35950469970703
            , geoCoords -12.307498931884766 43.7449951171875
            , geoCoords -12.214000701904297 44.373497009277344
            ]
      }
    , { code = "cg"
      , latitude = -0.6585006713867188
      , longitude = 14.875999450683594
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "cd"
      , latitude = -4.070949554443359
      , longitude = 21.738502502441406
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ci"
      , latitude = 7.537700653076172
      , longitude = -5.555000305175781
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "dj"
      , latitude = 11.82480239868164
      , longitude = 42.587501525878906
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "eg"
      , latitude = 26.824951171875
      , longitude = 30.787002563476562
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "gq"
      , latitude = 2.3591995239257812
      , longitude = 9.884506225585938
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "er"
      , latitude = 15.190849304199219
      , longitude = 39.772003173828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sz"
      , latitude = -26.52649688720703
      , longitude = 31.45050048828125
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "et"
      , latitude = 9.15420150756836
      , longitude = 40.488494873046875
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ga"
      , latitude = -0.8070487976074219
      , longitude = 11.592002868652344
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gm"
      , latitude = 13.438152313232422
      , longitude = -15.325996398925781
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "gh"
      , latitude = 7.964698791503906
      , longitude = -1.02850341796875
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gn"
      , latitude = 9.944896697998047
      , longitude = -11.365997314453125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gw"
      , latitude = 11.810001373291016
      , longitude = -15.193000793457031
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ke"
      , latitude = 0.3999519348144531
      , longitude = 37.92900085449219
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ls"
      , latitude = -29.61199951171875
      , longitude = 28.22150421142578
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "lr"
      , latitude = 6.444499969482422
      , longitude = -9.453994750976562
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ly"
      , latitude = 26.339248657226562
      , longitude = 17.229995727539062
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mg"
      , latitude = -18.82550048828125
      , longitude = 46.87000274658203
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mw"
      , latitude = -13.262996673583984
      , longitude = 34.28150177001953
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ml"
      , latitude = 17.56945037841797
      , longitude = -4.023002624511719
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mr"
      , latitude = 21.015649795532227
      , longitude = -10.943496704101562
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mu"
      , latitude = -20.25149917602539
      , longitude = 57.55500030517578
      , scale = Medium
      , highlightMethod = Dot
      }
    , { code = "ma"
      , latitude = 31.792800903320312
      , longitude = -7.120994567871094
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "mz"
      , latitude = -18.66299819946289
      , longitude = 35.53350067138672
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "na"
      , latitude = -22.953502655029297
      , longitude = 18.490501403808594
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ne"
      , latitude = 17.607097625732422
      , longitude = 8.063499450683594
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ng"
      , latitude = 9.075149536132812
      , longitude = 8.656501770019531
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "rw"
      , latitude = -1.93585205078125
      , longitude = 29.86750030517578
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "st"
      , latitude = 0.8732490539550781
      , longitude = 6.959999084472656
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords 1.6203498840332031 7.3914947509765625
            , geoCoords 0.22590255737304688 6.609001159667969
            ]
      }
    , { code = "sn"
      , latitude = 14.50345230102539
      , longitude = -14.458999633789062
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sc"
      , latitude = -4.672149658203125
      , longitude = 55.46299743652344
      , scale = Medium
      , highlightMethod = Dot
      }
    , { code = "sl"
      , latitude = 8.451499938964844
      , longitude = -11.788002014160156
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "so"
      , latitude = 5.144203186035156
      , longitude = 46.177001953125
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "za"
      , latitude = -28.46600341796875
      , longitude = 24.66699981689453
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ss"
      , latitude = 7.856899261474609
      , longitude = 29.707504272460938
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "sd"
      , latitude = 15.434001922607422
      , longitude = 30.216995239257812
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "tz"
      , latitude = -6.3554534912109375
      , longitude = 35.032005310058594
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tg"
      , latitude = 8.602500915527344
      , longitude = 0.8440017700195312
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "tn"
      , latitude = 33.7849006652832
      , longitude = 9.515998840332031
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ug"
      , latitude = 1.3751487731933594
      , longitude = 32.269996643066406
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "zm"
      , latitude = -13.131050109863281
      , longitude = 27.82050323486328
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "zw"
      , latitude = -19.022499084472656
      , longitude = 29.115501403808594
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "us"
      , latitude = 37.25150108337402
      , longitude = -95.8484992980957
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ag"
      , latitude = 17.35565185546875
      , longitude = -61.7864990234375
      , scale = Small
      , highlightMethod = SmallDots
            [ geoCoords 17.63140106201172 -61.80800247192383
            , geoCoords 17.083049774169922 -61.7864990234375
            ]
      }
    , { code = "ar"
      , latitude = -38.41749954223633
      , longitude = -63.62249755859375
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "bs"
      , latitude = 23.938751220703125
      , longitude = -75.86650085449219
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bb"
      , latitude = 13.189952850341797
      , longitude = -59.53750228881836
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "bz"
      , latitude = 17.185501098632812
      , longitude = -88.5130500793457
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "bo"
      , latitude = -16.30120086669922
      , longitude = -63.57100296020508
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "br"
      , latitude = -14.241996765136719
      , longitude = -54.40349578857422
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ca"
      , latitude = 62.425954818725586
      , longitude = -96.82805252075195
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "cl"
      , latitude = -36.698997497558594
      , longitude = -71.0719985961914
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "co"
      , latitude = 4.099250793457031
      , longitude = -72.95050048828125
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "cr"
      , latitude = 9.63010025024414
      , longitude = -84.23579788208008
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "cu"
      , latitude = 21.52294921875
      , longitude = -79.51210021972656
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "dm"
      , latitude = 15.43020248413086
      , longitude = -61.36600112915039
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "do"
      , latitude = 18.774799346923828
      , longitude = -70.16949844360352
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ec"
      , latitude = -1.7676010131835938
      , longitude = -78.10639953613281
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "sv"
      , latitude = 13.797550201416016
      , longitude = -88.91060256958008
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gd"
      , latitude = 12.122699737548828
      , longitude = -61.69449996948242
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "gt"
      , latitude = 15.776451110839844
      , longitude = -90.23175048828125
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "gy"
      , latitude = 4.875251770019531
      , longitude = -58.9370002746582
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ht"
      , latitude = 19.066448211669922
      , longitude = -73.06149673461914
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "hn"
      , latitude = 14.746601104736328
      , longitude = -86.26005172729492
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "jm"
      , latitude = 18.118549346923828
      , longitude = -77.27549743652344
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "mx"
      , latitude = 23.630352020263672
      , longitude = -102.54884910583496
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ni"
      , latitude = 12.871749877929688
      , longitude = -85.41384887695312
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "pa"
      , latitude = 8.409000396728516
      , longitude = -80.11164855957031
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "py"
      , latitude = -23.420001983642578
      , longitude = -58.44649887084961
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "pe"
      , latitude = -9.193851470947266
      , longitude = -75.01079940795898
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "kn"
      , latitude = 17.251602172851562
      , longitude = -62.685997009277344
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "lc"
      , latitude = 13.905498504638672
      , longitude = -60.97999954223633
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "vc"
      , latitude = 13.026702880859375
      , longitude = -61.23899841308594
      , scale = Small
      , highlightMethod = Dot
      }
    , { code = "sr"
      , latitude = 3.9178504943847656
      , longitude = -56.02199935913086
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "tt"
      , latitude = 10.694999694824219
      , longitude = -61.21549987792969
      , scale = Small
      , highlightMethod = Fill
      }
    , { code = "uy"
      , latitude = -32.516998291015625
      , longitude = -55.80350112915039
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "ve"
      , latitude = 6.432952880859375
      , longitude = -66.59749984741211
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "fm"
      , latitude = 7.435249328613281
      , longitude = 150.5275115966797
      , scale = Medium
      , highlightMethod = SmallDots
          [ geoCoords 9.506149291992188 138.13800048828125
          , geoCoords 7.400051116943359 151.7415008544922
          , geoCoords 6.884349822998047 158.2314910888672
          , geoCoords 5.306098937988281 162.95700073242188
          ] 
      }
    , { code = "vu"
      , latitude = -16.97549819946289
      , longitude = 168.21099853515625
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "fj"
      , latitude = -17.09149932861328
      , longitude = 178.16794282197952
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "nz"
      , latitude = -40.84650421142578
      , longitude = 172.5070037841797
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "mh"
      , latitude = 8.484249114990234
      , longitude = 169.30099487304688
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords 11.157451629638672 166.8719940185547
            , geoCoords 7.314899444580078 168.7524871826172
            , geoCoords 5.9069976806640625 169.66299438476562
            , geoCoords 7.07244873046875 171.39649963378906
            ]
      }
    , { code = "nr"
      , latitude = -0.5200996398925781
      , longitude = 166.93251037597656
      , scale = Medium
      , highlightMethod = Dot
      }
    , { code = "to"
      , latitude = -20.007999420166016
      , longitude = -174.64211010932922
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords -18.631999969482422 -173.9955050945282
            , geoCoords -21.259498596191406 -175.1377398967743
            ]
      }
    , { code = "pg"
      , latitude = -6.492099761962891
      , longitude = 148.41000366210938
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "sb"
      , latitude = -9.220451354980469
      , longitude = 161.30349731445312
      , scale = Medium
      , highlightMethod = Fill
      }
    , { code = "ws"
      , latitude = -13.755996704101562
      , longitude = -172.11404013633728
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords -13.634498596191406 -172.47768998146057
            , geoCoords -13.926998138427734 -171.74773001670837
            ]
      }
    , { code = "au"
      , latitude = -27.163002014160156
      , longitude = 133.4010009765625
      , scale = Large
      , highlightMethod = Fill
      }
    , { code = "pw"
      , latitude = 5.366996765136719
      , longitude = 132.8975067138672
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords 3.041248321533203 131.1614990234375
            , geoCoords 7.536350250244141 134.5830078125
            ]
      }
    , { code = "tv"
      , latitude = -8.500652313232422
      , longitude = 179.20651245117188
      , scale = Medium
      , highlightMethod = Dot
      }
    , { code = "ki"
      , latitude = -3.7667503356933594
      , longitude = -171.12979459762573
      , scale = Medium
      , highlightMethod = SmallDots
            [ geoCoords -4.676002502441406 -174.52092504501343
            , geoCoords -4.508049011230469 -172.2046401500702
            , geoCoords -2.952350616455078 -171.40432500839233
            , geoCoords -4.4548492431640625 -171.24692010879517
            , geoCoords 3.8600502014160156 -159.33415031433105
            , geoCoords 1.8804969787597656 -157.3774003982544
            , geoCoords -5.619647979736328 -155.89550018310547
            , geoCoords -4.062450408935547 -154.97919940948486
            , geoCoords -11.42449951171875 -151.80084991455078
            , geoCoords -0.8614501953125 169.53900146484375
            , geoCoords 3.0806503295898438 172.86000061035156
            , geoCoords 1.3895530700683594 173.05250549316406
            , geoCoords -0.9275970458984375 174.5800018310547
            ]
      }
    ]
