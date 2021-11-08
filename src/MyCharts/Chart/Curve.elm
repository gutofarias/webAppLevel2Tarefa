module MyCharts.Chart.Curve exposing (..)

import Chart as C
import Chart.Attributes as CA
import DataConvert as DC 
import Html as H
import Html.Attributes as HA


------------------------------------------------
-- Model
------------------------------------------------

type alias AxesString = (String,String)
type alias CurveID = Int
    
type alias Model =
    { curveID : CurveID
    , axesString : AxesString
    }

    
------------------------------------------------
-- init
------------------------------------------------

init : Maybe Model -> Model 
init maybeLastCurve =
    case maybeLastCurve of
        Nothing ->
            {curveID = 1, axesString = ("t","x1")}
                
        Just lastCurve ->
            let
                lastCurveID = .curveID lastCurve
                curveID = lastCurveID + 1
            in
                {curveID = curveID, axesString = ("t","x1")}

                    
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = XAxis String
    | YAxis String
      
      
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model 
update msg model =
    let
       (xAxis, yAxis) = .axesString model 
       otherCurveID = .curveID model
    in
        case msg of
            XAxis axisString ->
                {model | axesString = (axisString,yAxis)}
            YAxis axisString ->
                {model | axesString = (xAxis,axisString)}
                
                        
------------------------------------------------
-- view
------------------------------------------------
                
view : DC.ChartData -> String -> List (H.Html msg)
view chartData selStr =
    case chartData of
        [] -> []
        chartDatum :: ls ->
            case chartDatum of
                DC.TS1 datum -> 
                    [ chartAxisOptionView2 chartDatum "t" selStr 
                    , chartAxisOptionView2 chartDatum "x1" selStr
                    ]
                DC.TS1E1R1U1 datum ->
                    [ chartAxisOptionView2 chartDatum "t"  selStr 
                    , chartAxisOptionView2 chartDatum "x1" selStr
                    , chartAxisOptionView2 chartDatum "e1" selStr
                    , chartAxisOptionView2 chartDatum "r1" selStr
                    , chartAxisOptionView2 chartDatum "u1" selStr
                    ] 
                DC.TS1E1R1U4 datum ->
                    [ chartAxisOptionView2 chartDatum "t"  selStr 
                    , chartAxisOptionView2 chartDatum "x1" selStr
                    , chartAxisOptionView2 chartDatum "e1" selStr
                    , chartAxisOptionView2 chartDatum "r1" selStr
                    , chartAxisOptionView2 chartDatum "u1" selStr
                    , chartAxisOptionView2 chartDatum "u2" selStr
                    , chartAxisOptionView2 chartDatum "u3" selStr
                    , chartAxisOptionView2 chartDatum "u4" selStr
                    ]
                DC.TS2E1R1U4 datum ->
                    [ chartAxisOptionView2 chartDatum "t"  selStr 
                    , chartAxisOptionView2 chartDatum "x1" selStr
                    , chartAxisOptionView2 chartDatum "x2" selStr
                    , chartAxisOptionView2 chartDatum "e1" selStr
                    , chartAxisOptionView2 chartDatum "r1" selStr
                    , chartAxisOptionView2 chartDatum "u1" selStr
                    , chartAxisOptionView2 chartDatum "u2" selStr
                    , chartAxisOptionView2 chartDatum "u3" selStr
                    , chartAxisOptionView2 chartDatum "u4" selStr
                    ]


optionToNameCurve : DC.ChartDatum -> String -> String
optionToNameCurve chartDatum optStr =
    case chartDatum of
        DC.TS1 datum -> 
            case optStr of
                "t" -> "t"
                "x1" -> "x"
                _ -> "erro"
        DC.TS1E1R1U1 datum -> 
            case optStr of
                "t" -> "t"
                "x1" -> "x"
                "e1" -> "e" 
                "r1" -> "r"
                "u1"-> "u"
                _ -> "erro"
                     
        DC.TS1E1R1U4 datum -> 
            case optStr of
                "t" -> "t"
                "x1" -> "x"
                "e1" -> "e" 
                "r1" -> "r"
                "u1"-> "u"
                "u2"-> "up"
                "u3"-> "ui"
                "u4"-> "ud"
                _ -> "erro"
                     
        DC.TS2E1R1U4 datum -> 
            case optStr of
                "t" -> "t"
                "x1" -> "x1"
                "x2" -> "x2"
                "e1" -> "e" 
                "r1" -> "r"
                "u1"-> "u"
                "u2"-> "up"
                "u3"-> "ui"
                "u4"-> "ud"
                _ -> "erro"
                     
chartAxisOptionView : String -> String -> String -> H.Html msg
chartAxisOptionView val txt selStr =
        if (selStr == val) then
            H.option [HA.value val, HA.selected True] [H.text txt]
            -- option [value val] [text txt]
        else
            H.option [HA.value val] [H.text txt]

chartAxisOptionView2 : DC.ChartDatum -> String -> String -> H.Html msg
chartAxisOptionView2 chartDatum optStr selStr =
    chartAxisOptionView optStr (optionToNameCurve chartDatum optStr) selStr

------------------------------------------------
-- ChartSeries
------------------------------------------------
                
curveToChartSeries : DC.ChartDatum -> DC.ChartData -> Model -> Maybe (C.Element DC.ChartDatum msg)
curveToChartSeries chartDatum chartData model = 
    let 
        (xstr,ystr) = .axesString model
        name = (optionToNameCurve chartDatum xstr) ++ " – " ++ (optionToNameCurve chartDatum ystr)
                      
        -- stringToAxisFunc me retorna um Maybe (AxisFuncMaybe ChartDatum) 
        -- type alias AxisFuncMaybe data = data -> Maybe Float
        maybeXfuncMaybeFloat = DC.stringToAxisFunc xstr
        maybeYfuncMaybeFloat = DC.stringToAxisFunc ystr
                               
        -- Maybe (ChartDatum -> Maybe Float)
        -- Dois maybes, o de fora é pq eu nao sei se o string retorna uma funcao valida, o de dentro é porque eu não sei se o chartDatum permite que eu aplique a função. Posso ter um TS1 e pedir u1, por exemplo
    in
        case (maybeXfuncMaybeFloat, maybeYfuncMaybeFloat) of

            -- (ChartDatum -> Maybe Float)
            -- Analisa se existe a função para a dada string
            (Just xfuncMaybeFloat, Just yfuncMaybeFloat) ->
                let
                    -- Maybe (ChartDatum -> Float)
                    -- Analisa se o dado é suportado pela funcao usando o chartDatum 
                    maybeXfunc = DC.funcMaybeToMaybeFunc chartDatum xfuncMaybeFloat
                    maybeYfunc = DC.funcMaybeToMaybeFunc chartDatum yfuncMaybeFloat
                in
                    case (maybeXfunc, maybeYfunc) of
                        -- Caso tudo esteja ok consegue pegar a funcao e retornar o Just
                        (Just xfunc, Just yfunc) ->
                            Just  
                            (C.series xfunc
                            [ C.named name <|
                                C.interpolated yfunc
                                  [ CA.width 2
                                  ]
                                  [] 
                            ]

                            chartData)

                        -- caso contrario retorna nothing
                        _ -> Nothing
            -- caso contrario retorna nothing
            _ -> Nothing
