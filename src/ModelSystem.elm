module ModelSystem exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Html 

import Reference as Ref
import Controller as Control 

import Element as E

import ModelSystem.Level as Level
import ModelSystem.Level2 as Level2


------------------------------------------------
-- Model
------------------------------------------------

type Type
    = Level
    | Level2

type Model 
    = LevelModel Level.Model
    | Level2Model Level2.Model
    
      
      
------------------------------------------------
-- init
------------------------------------------------

init : Type -> Model
init modelType =
    case modelType of
        Level -> LevelModel Level.init
        Level2 -> Level2Model Level2.init

                 
------------------------------------------------
-- Msg
------------------------------------------------
                        
type Msg
    = LevelMsg Level.Msg
    | Level2Msg Level2.Msg

      
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model
update msg model =
    case msg of
        LevelMsg levelMsg ->
            case model of
                LevelModel levelModel ->
                    LevelModel <| Level.update levelMsg levelModel 
                _ -> model
                        
        Level2Msg level2Msg ->
            case model of
                Level2Model level2Model ->
                    Level2Model <| Level2.update level2Msg level2Model 
                _ -> model
      
------------------------------------------------
-- view
------------------------------------------------
                        
view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg = 
    case model of
        LevelModel levelModel ->
            Level.view levelModel (msgToMainMsg << LevelMsg)
                
        Level2Model level2Model ->
            Level2.view level2Model (msgToMainMsg << Level2Msg)
                
------------------------------------------------
-- xsFromModel
------------------------------------------------

xsFromModel : Model -> Edo.State
xsFromModel model = 
    case model of
        LevelModel levelModel ->
            Level.xsFromModel levelModel

        Level2Model level2Model ->
            Level2.xsFromModel level2Model
                
------------------------------------------------
-- updateModelFromXs
------------------------------------------------

updateModelFromXs : Edo.State -> Model -> Model
updateModelFromXs xs model =
    case model of
        LevelModel levelModel ->
            LevelModel <| Level.updateModelFromXs xs levelModel
                       
        Level2Model level2Model ->
            Level2Model <| Level2.updateModelFromXs xs level2Model
                
------------------------------------------------
-- edoParam
------------------------------------------------

initEdoParam : Type -> Edo.EdoParam
initEdoParam modelType = 
    case modelType of
        Level -> Level.initEdoParam
        Level2 -> Level2.initEdoParam

------------------------------------------------
-- control
------------------------------------------------

control : Type -> Control.Model
control modelType = 
    case modelType of
        Level -> Level.control
        Level2 -> Level2.control

------------------------------------------------
-- ref
------------------------------------------------

ref : Type -> Ref.Model
ref modelType = 
    case modelType of
        Level -> Level.ref
        Level2 -> Level2.ref

------------------------------------------------
-- output
------------------------------------------------

output : Type -> Edo.OutputFunction
output modelType = 
    case modelType of
        Level -> Level.output
        Level2 -> Level2.output

------------------------------------------------
-- runEdo
------------------------------------------------

runEdo : Model -> Edo.EdoParam -> Edo.RefFunction -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
runEdo model edoParam refFunc controller =
     case model of
         LevelModel levelModel ->
             Level.runEdo levelModel edoParam refFunc controller
                 
         Level2Model level2Model ->
             Level2.runEdo level2Model edoParam refFunc controller
                 
------------------------------------------------
-- simulation
------------------------------------------------

simulation : Edo.State -> Edo.Ref -> Edo.ControlEffort -> Model -> Html.Html msg
simulation xs rs us model = 
    case model of
        LevelModel levelModel ->
            Level.simulation xs rs us levelModel
                
        Level2Model level2Model ->
            Level2.simulation xs rs us level2Model

                 
                 
