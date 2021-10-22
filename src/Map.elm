module Map exposing (..)

type alias Location =
    { latitude : Float
    , longitude : Float
    , name : String
    , id : Int
    }

type alias Route =
    List Location

locationToRadians : Location -> (Float, Float)
locationToRadians loc =
    let rLat = degrees loc.latitude
        rLong = degrees loc.longitude
    in (rLat, rLong)

distance : Location -> Location -> Float
distance coordsA coordsB =
    let haversine x = sin (x / 2) ^ 2
        earthRadius = 6372.8
        (rLatA, rLongA) = locationToRadians coordsA
        (rLatB, rLongB) = locationToRadians coordsB
        hLat = haversine <| rLatB - rLatA
        hLong = haversine <| rLongB - rLongA
        root = sqrt <| hLat + cos rLatA * cos rLatB * hLong
    in earthRadius * 2 * asin (min 1 root)

routeDistance : Route -> Float
routeDistance route =
    case route of
        a :: b :: rest -> distance a b + routeDistance (b :: rest)
        [_] -> 0
        [] -> 0

type alias TwoOptState =
    { i : Int
    , k : Int
    , route : Route
    , distance : Float
    , swapped : Bool
    }

twoOptSwap : Route -> Int -> Int -> Route
twoOptSwap route i k =
    List.concat [ List.take i route
                , List.reverse <| List.drop i << List.take k <| route
                , List.drop k route
                ]

outerTwoOpt : TwoOptState -> TwoOptState
outerTwoOpt state =
    if state.i < List.length state.route
    then innerTwoOpt { state | k = state.i + 1 }
    else state

innerTwoOpt : TwoOptState -> TwoOptState
innerTwoOpt state =
    if state.k < List.length state.route
    then let newRoute = twoOptSwap state.route state.i state.k
             newDistance = routeDistance newRoute
         in if newDistance < state.distance
            then outerTwoOpt { state | route = newRoute, distance = newDistance, i = 1, k = 0 }
            else innerTwoOpt { state | k = state.k + 1 }
    else outerTwoOpt { state | i = state.i + 1 }

twoOpt : Route -> Route
twoOpt route =
    let helper state =
            if state.swapped
            then helper <| outerTwoOpt { state | swapped = False }
            else state
        initialRoute =
            case route of
                x :: _ -> route ++ [x]
                [] -> []
        init = { i = 1
               , k = 0
               , distance = routeDistance initialRoute
               , route = initialRoute
               , swapped = True
               }
        result = outerTwoOpt init
    in result.route

location : Int -> String -> Float -> Float -> Location
location id name latitude longitude =
    { id = id
    , name = name
    , latitude = latitude
    , longitude = longitude
    }
