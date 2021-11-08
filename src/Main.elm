module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import ModelSystem as MS
import Controller as Control 
import Reference as Ref

import Browser
import Browser.Events
import Html -- exposing (Html, button, div, text, pre, input, label, select, option, span, section)
import Html.Attributes as HA -- exposing (style, placeholder, value, for, name, selected)
import Html.Events -- exposing (onClick, onInput)

import Chart as C
import Chart.Attributes as CA
import MyCharts as MC
import MyCharts.Chart as MCC

import Element as E
import Element.Input as EI
import Element.Border as EB

import UI 


------------------------------------------------
-- main
------------------------------------------------

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

      
      
------------------------------------------------
-- BigModel
------------------------------------------------

type BigModel
    = SolvingEdo Model
    | Animation Model Edo.EdoParam Edo.State

modelFromBigModel : BigModel -> Model
modelFromBigModel bigModel = 
    case bigModel of
        SolvingEdo model -> model
        Animation model _ _ -> model

updatingBigModelFromModel : BigModel -> Model -> BigModel
updatingBigModelFromModel bigModel newModel = 
    case bigModel of
        SolvingEdo _ -> SolvingEdo newModel
        Animation _ animatingEdoParam xs -> Animation newModel animatingEdoParam xs

                                            
------------------------------------------------
-- Model
------------------------------------------------

type alias Model =
    { chartData : DC.ChartData
    , modelSystemType : MS.Type
    , modelSystemModel : MS.Model
    , edoParam : Edo.EdoParam
    , edoIStates : Edo.EdoIStates
    , chartsModel : MC.Model
    , controlModel : Control.Model
    , refModel : Ref.Model
    }

    
------------------------------------------------
-- init
------------------------------------------------

init : () -> (BigModel, Cmd Msg) 
init () =
    let
        modelSystemType = MS.Level2
        edoParam = MS.initEdoParam modelSystemType
        edoIStates = Edo.edoIStatesFromEdoParam edoParam
    in
            (SolvingEdo
            { chartData = []
            , modelSystemType = modelSystemType
            , modelSystemModel = MS.init modelSystemType
            , edoParam = edoParam
            , edoIStates = edoIStates
            , chartsModel = MC.init
            -- , controlModel = Control.init Control.PID
            , controlModel = MS.control modelSystemType
            -- , refModel = Ref.init Ref.Step1
            , refModel = MS.ref modelSystemType
            }, Cmd.none)
              

------------------------------------------------
-- Subscriptions
------------------------------------------------

subscriptions : BigModel -> Sub Msg
subscriptions bigModel =
    case bigModel of
        SolvingEdo model -> 
            -- Sub.none
            MC.subscriptions model.chartsModel MyChartsMsg
        Animation _ _ _ -> 
            Browser.Events.onAnimationFrameDelta (Tick)
        
        
------------------------------------------------
-- Msg
------------------------------------------------
            
type Msg
    = EdoMsg Edo.Msg
    | UpdateEdoParam 
    | ModelSystemMsg MS.Msg
    | MyChartsMsg MC.Msg
    | ControlMsg Control.Msg
    | RefMsg Ref.Msg
    | RunEdo
    | RunAnimation
    | Tick Float

      
------------------------------------------------
-- Msg
------------------------------------------------

update : Msg -> BigModel -> (BigModel, Cmd Msg)
update msg bigModel =
    
  let
      model = modelFromBigModel bigModel
  in
  case msg of
      
-- EdoMsg --------------------------------------

    EdoMsg edoMsg ->
        let
            edoIStates = .edoIStates model
            edoIStatesNew = Edo.updateEdoIStates edoMsg edoIStates 
            newModel = {model | edoIStates = edoIStatesNew}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)

                
-- UpdateEdoParam ----------------------------------------

    UpdateEdoParam ->
        let
            edoIStates = .edoIStates model

            edoParam = .edoParam model
            edoParamNew = Edo.updateEdoParam edoParam edoIStates
            newModel = {model | edoParam = edoParamNew}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)
                
                    
-- ModelSystemMsg -----------------------------------

    ModelSystemMsg modelSystemMsg ->
        let
            modelSystemModel = .modelSystemModel model
            modelSystemModelNew = MS.update modelSystemMsg modelSystemModel
            newModel = {model | modelSystemModel = modelSystemModelNew}
        in 
            (updatingBigModelFromModel bigModel newModel, Cmd.none)

    ControlMsg controlMsg ->
        let 
            controlModel = .controlModel model
            controlModelNew = Control.update controlMsg controlModel
            newModel = {model | controlModel = controlModelNew}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)

                
-- RefMsg ----------------------------------------

    RefMsg refMsg ->
        let
            refModel = .refModel model
            refModelNew = Ref.update refMsg refModel 
            newModel = {model | refModel = refModelNew}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)

                
-- MyChartsMsg ----------------------------------------

    MyChartsMsg chartsMsg ->
        let
            chartsModel = .chartsModel model
            newChartsModel = MC.update chartsMsg chartsModel
            newModel = {model | chartsModel = newChartsModel}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)


-- RunEdo --------------------------------------

    RunEdo -> 
        let  
            newModel = modelFromBigModel <| Tuple.first <| update UpdateEdoParam bigModel
            edoParam = .edoParam newModel 
            modelSystemModel = .modelSystemModel newModel
            controlModel = .controlModel newModel
            refModel = .refModel newModel

            controller = Control.controllerFromModel controlModel
            refFunc = Ref.refFunctionFromModel refModel 

            controlMem = []
            newEdoParam = {edoParam | controlMemory = controlMem}

            data = Tuple.first <| MS.runEdo modelSystemModel newEdoParam refFunc controller
        in
            ( SolvingEdo { newModel | chartData = data, edoParam = newEdoParam }, Cmd.none)

                
-- RunAnimation ----------------------------------------

    RunAnimation ->
        let
            updatedModel = modelFromBigModel <| Tuple.first <| update UpdateEdoParam bigModel

            edoParam = .edoParam updatedModel
            modelSystemModel = .modelSystemModel updatedModel

            controlMem = []
            tiniAnimation = .tempo edoParam
            -- Coloca no tfim pra sincronizar o tempo inicial e o tfim vai ser incrementado aos poucos por Tick
            animatingEdoParam = {edoParam | tfim = tiniAnimation, controlMemory = controlMem}
            chartData = []

            newModel = {updatedModel | chartData = chartData}
            xs = MS.xsFromModel modelSystemModel
        in
        (Animation newModel animatingEdoParam xs, Cmd.none)

        
-- Tick ----------------------------------------

    Tick dTime ->
        case bigModel of
            SolvingEdo _ ->
                (bigModel, Cmd.none)
            Animation modelAnimation animatingEdoParam xs ->
                let
                    modelEdoParam = .edoParam modelAnimation
                    tfimAnimation = .tfim modelEdoParam

                    tempo = .tempo animatingEdoParam
                in
                if (Edo.checkEndTimeEps tempo tfimAnimation) then
                    (SolvingEdo modelAnimation, Cmd.none)

                else
                let
                    dTimeSec = dTime/1000.0

                    tfimStep = .tfim animatingEdoParam
                    newTfimStep = min tfimAnimation (tfimStep + dTimeSec)

                    animatingEdoParam2 = {animatingEdoParam | tfim = newTfimStep}

                    modelData = .chartData modelAnimation
                    modelSystemModel = .modelSystemModel modelAnimation

                    controlModel = .controlModel modelAnimation
                    refModel = .refModel modelAnimation

                    controller = Control.controllerFromModel controlModel
                    refFunc = Ref.refFunctionFromModel refModel 

                    stateUpdatedModel = MS.updateModelFromXs xs modelSystemModel

                    (data,newAnimationEdoParam) = MS.runEdo stateUpdatedModel animatingEdoParam2 refFunc controller

                    newXs = case data of
                                (d::ds) -> DC.xsFromDatum d
                                _ -> []

                    newData = data ++ modelData
                    newModel = { modelAnimation | chartData = newData}
                in
                (Animation newModel newAnimationEdoParam newXs, Cmd.none)
                    
                    
------------------------------------------------
-- View
------------------------------------------------
        
view : BigModel -> Html.Html Msg
view bigModel =
  let
    model = modelFromBigModel bigModel
            
    edoIStates = .edoIStates model
    edoParam = .edoParam model
                 
    chartData = .chartData model
    chartsModel = .chartsModel model
    modelSystemModel = .modelSystemModel model
                 
    refModel = .refModel model
    controlModel = .controlModel model
                   
    refFunc = Ref.refFunctionFromModel refModel
    controller = Control.controllerFromModel controlModel
           
    -- Ficou bom porque a animação mantém o final a partir dos dados
    (xs,rs,us) = case chartData of
             -- Caso tenha os dados pelo chartData
             (cd::cdlist) -> 
                 (DC.xsFromDatum cd, DC.rsFromDatum cd, DC.usFromDatum cd)
             _ ->
                -- Se não pega os dados pelo modelo
                case bigModel of
                    SolvingEdo _ ->
                        let 
                            xlist = MS.xsFromModel modelSystemModel 
                            (rlist,ulist) = Edo.getRsUs xlist edoParam (MS.output model.modelSystemType)refFunc controller
                        in
                            (xlist,rlist,ulist)
                    Animation _ animationEdoParam xsAnimation ->
                        let
                            (rlist,ulist) = Edo.getRsUs xsAnimation animationEdoParam (MS.output model.modelSystemType) refFunc controller
                        in 
                            (xsAnimation,rlist,ulist)

    chartsTuple = MC.chartsTuple chartsModel chartData (MyChartsMsg)
                
  in
      UI.view <|
              [ E.row [ E.alignTop
                      -- , E.spacing 50
                      -- , E.explain Debug.todo
                      , E.alignLeft
                      , E.width 
                            (E.fill
                            |> E.minimum 880
                            |> E.maximum 1000)
                      ]
                    
                    [ 
                      E.column [E.spacing 0, E.alignTop, E.padding 0, E.alignLeft] 
                          
                        [ MS.view modelSystemModel
                            (ModelSystemMsg)  
                              
                        , Control.view controlModel
                            (ControlMsg)
                                
                        , Ref.view refModel
                            (RefMsg)
                                
                        , E.row [E.centerX, E.spacing 40, E.moveDown 5]
                            [ Edo.view2 edoIStates
                                  (EdoMsg) 
                            , UI.button "RunODE" (Just RunEdo)
                            , UI.button "Animation" (Just RunAnimation)
                            ]
                        ]
                          
                    , E.el [ E.alignRight
                           -- , E.width E.fill
                           , E.centerY
                           , E.padding 10
                           -- , E.explain Debug.todo
                           , E.width 
                                 (E.shrink
                                 |> E.minimum 400)
                           , E.height 
                                 (E.fill
                                 |> E.minimum 400
                                 |> E.maximum 400)
                           ]  
                               (E.html <| MS.simulation xs rs us modelSystemModel)
                    ] 
              ]
                ++
                    (List.map
                         (\(a,b) -> 
                              E.row [
                                      E.width 
                                          (E.fill
                                          |> E.minimum 880
                                          |> E.maximum 1000)
                                    ]
                              [E.el [E.alignLeft] a, E.el [E.alignRight] b])
                         chartsTuple)
      
fcomposition23 : (a -> b) -> (c -> d -> a) -> c -> d -> b 
fcomposition23 f2 f3 c = 
    f2 << (f3 c)

