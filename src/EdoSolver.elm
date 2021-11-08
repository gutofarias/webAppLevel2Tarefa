module EdoSolver exposing (..)

import List
import Html exposing (Html,div,text,input,span)
import Html.Attributes exposing (style, placeholder, value, type_)
import Html.Events exposing (onInput)
import Element as E
import UI

------------------------------------------------
-- Variables
------------------------------------------------
eps : Float
eps = 1e-8

checkEndTimeEps : Tempo -> Tempo -> Bool
checkEndTimeEps tempo tfim = 
    if (abs(tempo - tfim) <= eps) then
        True
    else
        False

------------------------------------------------
-- Data Types Edo
------------------------------------------------

type alias Tempo = Float
type alias Passo = Float
type alias PassoSaida = Int

type alias State = List Float
type alias DState = List Float
type alias ControlEffort = List Float
type alias ControlMemory = List Float
type alias Ref = List Float
type alias Output = List Float
type alias Error = List Float
  
type EdoSist
    = Uncontrolled FuncSistUncontrolled
    | Controlled {outputFunc:OutputFunction, refFunc:RefFunction, controller:Controller, sistFunc:FuncSistControlled}
      
type alias FuncSistControlled = ControlEffort -> Tempo -> State -> DState
type alias FuncSistUncontrolled = Tempo -> State -> DState
type alias Controller = ControlMemory -> Error -> Passo -> Tempo -> State -> (ControlEffort, ControlMemory)
type alias RefFunction = Tempo -> Output -> Ref
type alias OutputFunction = Tempo -> State -> Output
    
type alias Solver = FuncSistUncontrolled -> Passo -> Tempo -> State -> State
type alias Datum = (Tempo , State)
type alias Data = List Datum

------------------------------------------------
-- EdoParam
------------------------------------------------

type alias EdoParam =  {
                 tempo : Tempo,
                 tfim : Tempo,
                 passo : Passo,
                 relPassoSaida : PassoSaida,
                 controlMemory : ControlMemory,
                 solver : Solver} 

    
initEdoParamAndIStates : (EdoParam,EdoIStates)
initEdoParamAndIStates =
    ({ tempo = 0.0
    , tfim = 10.0
    , passo = 0.001
    , relPassoSaida = 100
    , controlMemory = []
    , solver = rungeKutta } ,
    { tiniStr = "0"
    , tfimStr = "10" })
    
updateEdoParam : EdoParam -> EdoIStates -> EdoParam
updateEdoParam edoParam edoIStates =
    let
        tiniStr = .tiniStr edoIStates
        tfimStr = .tfimStr edoIStates
        listStr = [tiniStr,tfimStr]
        listValues = List.filterMap String.toFloat listStr
    in
        case listValues of
            (tini::tfim::[]) ->
                {edoParam | tempo = tini, tfim = tfim}
            _ -> 
                edoParam
    
edoIStatesFromEdoParam : EdoParam -> EdoIStates
edoIStatesFromEdoParam edoParam = 
    let
        tiniStr = String.fromFloat (.tempo edoParam)
        tfimStr = String.fromFloat (.tfim edoParam)
    in
        {tiniStr = tiniStr, tfimStr = tfimStr}
        

------------------------------------------------
-- EdoIStates
------------------------------------------------
    
type alias EdoIStates = { tiniStr:String, tfimStr:String }

    
updateEdoIStates : Msg -> EdoIStates -> EdoIStates
updateEdoIStates msg edoIStates =
    case msg of
        Tini valueStr -> {edoIStates | tiniStr = valueStr}
        Tfim valueStr -> {edoIStates | tfimStr = valueStr}


------------------------------------------------
-- EdoInteract
------------------------------------------------

type Msg
    = Tini String
    | Tfim String
    
    
------------------------------------------------
-- viewEdo
------------------------------------------------

view : EdoIStates -> (Msg -> msg) -> E.Element msg
view edoIStates msgToMainMsg =
  let
    tiniStr = .tiniStr edoIStates
    tfimStr = .tfimStr edoIStates
  in
    E.column [E.spacing 10, E.alignTop]
        [ UI.heading "ODE"
        , UI.textField tiniStr "Tini " <| msgToMainMsg << Tini
        , UI.textField tfimStr "Tfin" <| msgToMainMsg << Tfim ]
        
view2 : EdoIStates -> (Msg -> msg) -> E.Element msg
view2 edoIStates msgToMainMsg =
  let
    tiniStr = .tiniStr edoIStates
    tfimStr = .tfimStr edoIStates
  in
    E.column [E.spacing 0, E.alignTop, E.moveUp 5]
        [ UI.textField tiniStr "Tini " <| msgToMainMsg << Tini
        , UI.textField tfimStr "Tfin" <| msgToMainMsg << Tfim ]
    
------------------------------------------------
-- EdoSolver Functions
------------------------------------------------
    
edoStep : EdoParam -> FuncSistUncontrolled -> State -> State
edoStep param fsist xs =
    let
        solver = .solver param
        passo =  .passo param
        tempo =  .tempo param
    in
        solver fsist passo tempo xs
                    
                        
-- Unoptimized for TCO
edoSolverOld : EdoParam -> EdoSist -> State -> (Data, EdoParam)
edoSolverOld param edoSist xs =
    let
        tempo = .tempo param
        tfim = .tfim param
    in
        if (tempo >= tfim) then
            ((tempo, xs)::[], param)
        else
            let
                ((tempo1, xs1), param1) = integrator param edoSist 1 xs
                -- Nao sei porque mas se comentar a linha de baixo para de funcionar 
                -- O valor de param1.tempo é pra ser o mesmo de tempo1
                -- logo param1b == param1 mas se usar so param1 da erro de call stack
                param1b = { param1 | tempo = tempo1} 
                (data, param2) = edoSolverOld param1b edoSist xs1
            in
                -- (tempo, xs) :: edoSolver param1 edoSist xs1  
                ((tempo,xs)::data, param2)
                    
edoSolverReversed : EdoParam -> EdoSist -> State -> (Data, EdoParam)
edoSolverReversed param edoSist xs =
    let
        tempo = .tempo param
        xsAndMaybeUR = calcXsAndMaybeUR param edoSist xs
        (reversedData,paramFinal) = edoSolverAcc param edoSist xs ((tempo,xsAndMaybeUR)::[],param)
        -- data = List.reverse reversedData
    in
        -- (data,paramFinal)
        (reversedData,paramFinal)

edoSolver : EdoParam -> EdoSist -> State -> (Data, EdoParam)
edoSolver param edoSist xs =
    let
        (reversedData,paramFinal) = edoSolverReversed param edoSist xs
        data = List.reverse reversedData
    in
        (data,paramFinal)
        -- (reversedData,paramFinal)
            
edoSolverAcc : EdoParam -> EdoSist -> State -> (Data,EdoParam) -> (Data,EdoParam)
edoSolverAcc param edoSist xs (data,param2) =
    let
        tempo = .tempo param
        tfim = .tfim param
    in
        if (tempo >= tfim) then
            (data, param2)
        else
            let
                ((tempo1, xs1), param1) = integrator param edoSist 1 xs
                param1b = { param1 | tempo = tempo1} 
                xsAndMaybeUR = calcXsAndMaybeUR param1b edoSist xs1
            in
                edoSolverAcc param1b edoSist xs1 ((tempo1,xsAndMaybeUR)::data, param1b)


calcXsAndMaybeUR : EdoParam -> EdoSist -> State -> State
calcXsAndMaybeUR param edoSist xs =
    case edoSist of
        Uncontrolled sistFunc ->
            xs

        Controlled functions ->
            let
                tempo = .tempo param
                passo = .passo param
                controlMem = .controlMemory param

                outputFunc = .outputFunc functions
                output = outputFunc tempo xs
                             
                refFunc = .refFunc functions
                ref = refFunc tempo output

                error = zipWith (-) ref output
                        
                controller = .controller functions
                (controlEffort,newControlMem) = 
                    controller controlMem error passo tempo xs
            in
                List.foldr (++) [] [xs,error,ref,controlEffort,output]
                    
calcFsist : EdoParam -> EdoSist -> State -> (FuncSistUncontrolled, EdoParam)
calcFsist param edoSist xs =
    case edoSist of
        Uncontrolled sistFunc ->
            (sistFunc,param)

        Controlled functions ->
            let
                tempo = .tempo param
                passo = .passo param
                controlMem = .controlMemory param
                             
                outputFunc = .outputFunc functions
                output = outputFunc tempo xs
                         
                refFunc = .refFunc functions
                ref = refFunc tempo output

                error = zipWith (-) ref output
                      
                controller = .controller functions
                             
                (controlEffort,newControlMem) = 
                    controller controlMem error passo tempo xs

                sistFunc = .sistFunc functions
                fsist = sistFunc controlEffort

                newParam = { param | controlMemory = newControlMem }
            in
                (fsist,newParam)
                    
integrator : EdoParam -> EdoSist -> PassoSaida -> State -> (Datum, EdoParam)
integrator param edoSist saidaCount xs =
    let
        tfim = .tfim param
        tempo = .tempo param
        passo = .passo param 
        relPassoSaida = .relPassoSaida param
        controlMem = .controlMemory param
    in
        if (tfim - tempo <= passo) then
            
            if (abs (tfim-tempo) <= eps) then    
                let
                    paramFinal = {param | tempo = tfim}
                in
                    ((tfim,xs), paramFinal)
            else
                let 
                    newParam = { param | passo = tfim - tempo }
                    (fsist,param2) = calcFsist newParam edoSist xs
                    xsfinal = edoStep param2 fsist xs
                    -- Mudei pra o programa não finalizar com o passo errado
                    paramfinal = {param2 | passo = passo}
                in
                    ((tfim, xsfinal),paramfinal)
        else
            let 
                (fsist,newParam) = calcFsist param edoSist xs
                xs1 = edoStep newParam fsist xs
                tempo1 = tempo + passo
                param1 = { newParam | tempo = tempo1 } 
            in
                if (saidaCount == relPassoSaida) then
                    ((tempo1, xs1),param1)
                else
                    integrator param1 edoSist (saidaCount + 1) xs1

                        
rungeKutta : Solver
rungeKutta fsist passo tempo xs = 
    let
        xps1 = fsist tempo xs
        k1 = List.map ((*) passo) xps1
             
        tempo2 = tempo + 0.5*passo
        xs2 = zipWith (\a b -> a + 0.5*b) xs k1 
        xps2 = fsist tempo2 xs2
        k2 = List.map ((*) passo) xps2
             
        tempo3 = tempo2
        xs3 = zipWith (\a b -> a + 0.5*b) xs k2
        xps3 = fsist tempo3 xs3
        k3 = List.map ((*) passo) xps3

        tempo4 = tempo + passo
        xs4 = zipWith (+) k3 xs 
        xps4 = fsist tempo4 xs4
        k4 = List.map ((*) passo) xps4
    in
        zipWith (\a b -> 2.0*a + b) k3 k4 
        |> zipWith (\a b -> 2.0*a + b) k2
        |> zipWith (+) k1
        |> zipWith (\a b -> a + b/6.0) xs
            

eulerSolver : Solver
eulerSolver fsist passo tempo xs =
    let 
        xps = fsist tempo xs
        xps2 = List.map ((*) passo) xps
    in
        zipWith (+) xs xps2
                        
            
funcSist : FuncSistUncontrolled
funcSist t xs =
    case xs of
        (x::[]) -> t*x :: [] 
        _ -> xs

           
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith func l1 l2 =
    case l1 of
       [] -> [] 
       x::[] -> 
           case l2 of
               [] -> []
               y::[] -> (func x y)::[]
               y::ys -> (func x y)::[]
       x::xs ->
           case l2 of
               [] -> []
               y::[] -> (func x y)::[]
               y::ys -> (func x y):: zipWith func xs ys


printState : State -> String
printState list =
    case list of
        [] -> ""
        x::[] -> String.fromFloat x
        x::xs -> (String.fromFloat x) ++ "\t" ++ printState xs

printData : Data -> String
printData data = 
    case data of
        [] -> ""
        (t,xs) :: [] -> (String.fromFloat t) ++ "\t" ++ printState xs
        (t,xs) :: dt -> (String.fromFloat t) ++ "\t" ++ printState xs ++ "\n" ++ printData dt

getRsUs : State -> EdoParam -> OutputFunction -> RefFunction -> Controller -> (Ref, ControlEffort)
getRsUs xs edoParam outFunc refFunc controller =
    let
        tempo = .tempo edoParam
        passo = .passo edoParam
        controlMemory = .controlMemory edoParam

        ys = outFunc tempo xs
        rs = refFunc tempo ys
        es = zipWith (-) rs ys
        (us,controlMemory2) = controller controlMemory es passo tempo xs
    in
        (rs,us)

------------------------------------------------
-- Variables (teste)
------------------------------------------------

xs_ = 3.0 :: []
tini_ = 0.0 
tfim_ = 4.0
passo_ = 0.01
relSaida_ = 10
param_ = EdoParam tini_ tfim_ passo_ relSaida_ [] eulerSolver


