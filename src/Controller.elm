module Controller exposing (..)

import EdoSolver as Edo
import Element as E
import Controller.PID as PID
import Controller.PIDeq as PIDeq


------------------------------------------------
------------------------------------------------
-- Controller
------------------------------------------------
------------------------------------------------
------------------------------------------------
-- Model
------------------------------------------------

type Type
    = PID
    | PIDeq

type Model
    = PidModel PID.Model
    | PideqModel PIDeq.Model
      
init : Type -> Model
init controlType =
    case controlType of
        PID -> PidModel PID.init
        PIDeq -> PideqModel PIDeq.init

controllerFromModel : Model -> Edo.Controller
controllerFromModel model =
    case model of
        PidModel pidModel ->
            PID.controllerFromModel pidModel
        PideqModel pideqModel ->
            PIDeq.controllerFromModel pideqModel

                
update : Msg -> Model -> Model
update msg model =
    case msg of
        PidMsg pidMsg ->
            case model of
                PidModel pidModel ->
                    PidModel <| PID.update pidMsg pidModel
                _ -> model
        PideqMsg pideqMsg ->
            case model of
                PideqModel pideqModel ->
                    PideqModel <| PIDeq.update pideqMsg pideqModel
                _ -> model
                        
                    

------------------------------------------------
-- Msg
------------------------------------------------


type Msg
    = PidMsg PID.Msg
    | PideqMsg PIDeq.Msg



------------------------------------------------
-- ControlView
------------------------------------------------

view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg =
    case model of
        PidModel pidModel ->
            PID.view pidModel (msgToMainMsg << PidMsg)

        PideqModel pideqModel ->
            PIDeq.view pideqModel (msgToMainMsg << PideqMsg)
