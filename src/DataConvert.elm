module DataConvert exposing (..)

import EdoSolver as Edo

type alias AxisFuncMaybe data = data -> Maybe Float
    
type alias DatumTS1 = {t : Float, x1 : Float }
type alias DatumTS1E1R1U1 = {t : Float, x1 : Float, e1 : Float, r1 : Float, u1 : Float }
type alias DatumTS1E1R1U4 = {t : Float, x1 : Float, e1 : Float, r1 : Float, u1 : Float, u2 : Float, u3 : Float, u4 : Float }
type alias DatumTS2E1R1U4 = {t : Float, x1 : Float, x2 : Float, e1 : Float, r1 : Float, u1 : Float, u2 : Float, u3 : Float, u4 : Float }
    
-- type alias DatumT2S = {t : Float, x1 : Float, x2 : Float }
-- type alias DatumT3S = {t : Float, x1 : Float, x2 : Float, x3 : Float }
    
-- Adicionar os data types à medida em que for criando
type ChartDatum
    = TS1 DatumTS1
    | TS1E1R1U1 DatumTS1E1R1U1
    | TS1E1R1U4 DatumTS1E1R1U4
    | TS2E1R1U4 DatumTS2E1R1U4
      
    -- | TS11R1U DatumTS11R1U

type alias ChartData = List ChartDatum
 
-- type ChartData
--     = Nodata
--     | TS1 DataTS1
--     | T2S DataT2S
--     | T3S DataT3S
--     | T4S DataT4S
--     | T5S DataT5S
    
                      
toChartDatumTS1 : Edo.Datum -> Maybe ChartDatum
toChartDatumTS1 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                x::xs -> Just <| TS1 {t = tempo, x1 = x}
                [] -> Nothing

toChartDatumTS1E1R1U1 : Edo.Datum -> Maybe ChartDatum
toChartDatumTS1E1R1U1 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                (x::e::r::u::xs) -> Just <| TS1E1R1U1 {t = tempo, x1 = x, e1 = e, r1 = r, u1 = u}
                _ -> Nothing
                     
toChartDatumTS1E1R1U4 : Edo.Datum -> Maybe ChartDatum
toChartDatumTS1E1R1U4 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                (x::e::r::u1::u2::u3::u4::xs) -> Just <| TS1E1R1U4 {t = tempo, x1 = x, e1 = e, r1 = r, u1 = u1, u2 = u2, u3 = u3, u4 = u4}
                _ -> Nothing
                     
toChartDatumTS2E1R1U4 : Edo.Datum -> Maybe ChartDatum
toChartDatumTS2E1R1U4 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                (x1::x2::e::r::u1::u2::u3::u4::xs) -> Just <| TS2E1R1U4 {t = tempo, x1 = x1, x2 = x2, e1 = e, r1 = r, u1 = u1, u2 = u2, u3 = u3, u4 = u4}
                _ -> Nothing
                     
-- toChartDatumT2S : Edo.Datum -> ChartDatum
-- toChartDatumT2S edoDatum =
--     case edoDatum of
--         (t, sist) ->
--             case sist of 
--                 x1::x2::xs -> T2S <| DatumT2S t x1 x2
--                 _ -> T2S <| DatumT2S t 0.0 0.0
                     
-- toChartDatumT3S : Edo.Datum -> ChartDatum
-- toChartDatumT3S edoDatum =
--     case edoDatum of
--         (t, sist) ->
--             case sist of 
--                 x1::x2::x3::xs -> T3S <| DatumT3S t x1 x2 x3
--                 _ -> T3S <| DatumT3S t 0.0 0.0 0.0
            
                     
toChartDataTS1 : Edo.Data -> ChartData
toChartDataTS1 =
    List.filterMap toChartDatumTS1 
        
toChartDataTS1E1R1U1 : Edo.Data -> ChartData
toChartDataTS1E1R1U1 =
    List.filterMap toChartDatumTS1E1R1U1 
        
toChartDataTS1E1R1U4 : Edo.Data -> ChartData
toChartDataTS1E1R1U4 =
    List.filterMap toChartDatumTS1E1R1U4
        
toChartDataTS2E1R1U4 : Edo.Data -> ChartData
toChartDataTS2E1R1U4 =
    List.filterMap toChartDatumTS2E1R1U4
        
-- toChartDataT2S : Edo.Data -> ChartData
-- toChartDataT2S =
--     List.map toChartDatumT2S 
                     
-- toChartDataT3S : Edo.Data -> ChartData
-- toChartDataT3S =
--     List.map toChartDatumT3S 
        
                     
stringToAxisFunc : String -> Maybe (AxisFuncMaybe ChartDatum)
stringToAxisFunc str =
    case str of
        "t" -> Just ft
        "x1" -> Just fx1
        "x2" -> Just fx2
        "e1" -> Just fe1
        "r1" -> Just fr1
        "u1" -> Just fu1
        "u2" -> Just fu2
        "u3" -> Just fu3
        "u4" -> Just fu4
        _ -> Nothing
             
funcMaybeToMaybeFunc2 : ChartData -> (ChartDatum -> Maybe Float) -> Maybe (ChartDatum -> Float)
funcMaybeToMaybeFunc2 data func =
    case data of
        (x::xs) ->
            funcMaybeToMaybeFunc x func    
        _ -> Nothing

funcMaybeToMaybeFunc : a -> (a -> Maybe b) -> Maybe (a -> b)
funcMaybeToMaybeFunc datum func =
    case func datum of
        Just val -> Just (\s -> Maybe.withDefault val (func s))
        Nothing -> Nothing
                   
fx1 : ChartDatum -> Maybe Float
fx1 chartDatum = 
    case chartDatum of
        TS1 datum -> Just datum.x1
        TS1E1R1U1 datum -> Just datum.x1
        TS1E1R1U4 datum -> Just datum.x1
        TS2E1R1U4 datum -> Just datum.x1
                           
fx2 : ChartDatum -> Maybe Float
fx2 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Nothing
        TS1E1R1U4 datum -> Nothing
        TS2E1R1U4 datum -> Just datum.x2

ft : ChartDatum -> Maybe Float
ft chartDatum = 
    case chartDatum of
        TS1 datum -> Just datum.t
        TS1E1R1U1 datum -> Just datum.t
        TS1E1R1U4 datum -> Just datum.t
        TS2E1R1U4 datum -> Just datum.t
                                
fe1 : ChartDatum -> Maybe Float
fe1 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Just datum.e1
        TS1E1R1U4 datum -> Just datum.e1
        TS2E1R1U4 datum -> Just datum.e1
                     
fr1 : ChartDatum -> Maybe Float
fr1 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Just datum.r1
        TS1E1R1U4 datum -> Just datum.r1
        TS2E1R1U4 datum -> Just datum.r1
                                
fu1 : ChartDatum -> Maybe Float
fu1 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Just datum.u1
        TS1E1R1U4 datum -> Just datum.u1
        TS2E1R1U4 datum -> Just datum.u1
                                
fu2 : ChartDatum -> Maybe Float
fu2 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Nothing
        TS1E1R1U4 datum -> Just datum.u2
        TS2E1R1U4 datum -> Just datum.u2
                           
fu3 : ChartDatum -> Maybe Float
fu3 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Nothing
        TS1E1R1U4 datum -> Just datum.u3
        TS2E1R1U4 datum -> Just datum.u3
                           
fu4 : ChartDatum -> Maybe Float
fu4 chartDatum = 
    case chartDatum of
        TS1 datum -> Nothing
        TS1E1R1U1 datum -> Nothing
        TS1E1R1U4 datum -> Just datum.u4
        TS2E1R1U4 datum -> Just datum.u4


xsFromDatum : ChartDatum -> Edo.State
xsFromDatum chartDatum = 
    case chartDatum of
        TS1 datum -> 
            [(.x1 datum)]
        TS1E1R1U1 datum -> 
            [(.x1 datum)]
        TS1E1R1U4 datum ->
            [(.x1 datum)]
        TS2E1R1U4 datum ->
            [(.x1 datum), (.x2 datum)]

usFromDatum : ChartDatum -> Edo.ControlEffort
usFromDatum chartDatum = 
    case chartDatum of
        TS1 datum -> 
            []
        TS1E1R1U1 datum -> 
            [(.u1 datum)]
        TS1E1R1U4 datum ->
            [(.u1 datum),(.u2 datum),(.u3 datum),(.u4 datum)]
        TS2E1R1U4 datum ->
            [(.u1 datum),(.u2 datum),(.u3 datum),(.u4 datum)]

rsFromDatum : ChartDatum -> Edo.Ref
rsFromDatum chartDatum =
    case chartDatum of
        TS1 datum -> 
            []
        TS1E1R1U1 datum -> 
            [(.r1 datum)]
        TS1E1R1U4 datum ->
            [(.r1 datum)]
        TS2E1R1U4 datum ->
            [(.r1 datum)]
