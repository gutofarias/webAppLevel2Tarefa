module ModelSystem.Level exposing (..)

import Html 
import Element as E
import UI

import DataConvert as DC
import EdoSolver as Edo

import Reference as Ref
import Controller as Control

import Drawing2d
import Pixels exposing (pixels)
import Point2d
import Rectangle2d
import Polygon2d
import Circle2d
import Color
import Angle
import Arc2d
import Polyline2d

------------------------------------------------
------------------------------------------------
-- Nível
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Model
------------------------------------------------

type alias Model =
    { h0 : Float , ag : Float , ap : Float
    , h0Str : String, agStr : String, apStr : String }
    
    
------------------------------------------------
-- init
------------------------------------------------
    
init : Model
init =
    { h0 = 10.0
    , ag = 1.0
    , ap = 0.1
    , h0Str = "10"
    , agStr = "1"
    , apStr = "0.1" }
    
    
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = H0 String
    | Ag String
    | Ap String
    
    
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model
update msg model =
    case msg of
        H0 valueStr -> 
            let
                h0 = .h0 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault h0 maybeVal
            in
            { model | h0Str = valueStr, h0 = val }
        Ag valueStr ->
            let
                ag = .ag model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ag maybeVal
            in
            { model | agStr = valueStr, ag = val }
        Ap valueStr -> 
            let
                ap = .ap model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ap maybeVal
            in
            { model | apStr = valueStr, ap = val }


------------------------------------------------
-- view
------------------------------------------------
    
view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg = 
    let 
        h0Str = .h0Str model
        agStr = .agStr model
        apStr = .apStr model
    in 
        
    E.column [E.spacing 10, E.alignTop]
        [ UI.heading "Level" 
        , E.row [E.spacing 30]
                [ E.column [E.alignTop, E.spacing 10]
                    [ UI.textField h0Str "Hini" (msgToMainMsg << H0)
                    ]
                , E.column [E.alignTop, E.spacing 10]
                    [ UI.textField agStr "A " (msgToMainMsg << Ag)
                    , UI.textField apStr "a " (msgToMainMsg << Ap)
                    ]
                ]
        ]
        
        
------------------------------------------------
-- xsFromModel
------------------------------------------------

xsFromModel : Model -> Edo.State
xsFromModel model =
    [(.h0 model)]
                    
        
------------------------------------------------
-- updateModelFromXs
------------------------------------------------

updateModelFromXs : Edo.State -> Model -> Model
updateModelFromXs xs model = 
    let
        h0 = .h0 model
        newH0 = Maybe.withDefault h0 <| List.head xs
    in
        {model | h0 = newH0}
    
            
------------------------------------------------
-- edoParam
------------------------------------------------

initEdoParam : Edo.EdoParam
initEdoParam = 
    { tempo = 0.0
        , tfim = 10.0
        , passo = 0.001
        , relPassoSaida = 100
        , controlMemory = []
        , solver = Edo.rungeKutta }
    
        
------------------------------------------------
-- control
------------------------------------------------

control : Control.Model
control = 
    Control.init Control.PID

                 
------------------------------------------------
-- ref
------------------------------------------------

ref : Ref.Model
ref = 
    Ref.init Ref.Step1

------------------------------------------------
-- output
------------------------------------------------

output : Edo.Tempo -> Edo.State -> Edo.Output
output tempo xs =
    case xs of
        (x::ls) -> [x]
        _ -> xs

             
------------------------------------------------
-- runEdo
------------------------------------------------

runEdo : Model -> Edo.EdoParam -> Edo.RefFunction -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
runEdo model edoParam refFunc controller =
        let
            initState = (.h0 model) :: []
            edoSist = Edo.Controlled
                      { refFunc = refFunc
                      , outputFunc = output
                      , controller = controller
                      , sistFunc = (system model)}
            
            (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
        in
            (DC.toChartDataTS1E1R1U4 edoData, edoParamNew)
                
                
------------------------------------------------
-- system equation
------------------------------------------------
        
system : Model -> Edo.ControlEffort -> Edo.Tempo -> Edo.State -> Edo.DState
system model us t state =
    let
       ag = .ag model
       ap = .ap model
       g = 9.81
       uAux = Maybe.withDefault 0.0 (List.head us)
       u = if (uAux<0.0) then 0.0 else uAux
    in
       case state of
           h::ls -> 
               let
                   hn = if h >= 0.0 then h else 0.0
                   -- hnlog = Debug.log "sist" (hn,u)
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) + (u/ag) :: []
           _ -> 0.0 :: []
      

------------------------------------------------
-- simulation
------------------------------------------------

simulation : Edo.State -> Edo.Ref -> Edo.ControlEffort -> Model -> Html.Html msg
simulation xs rs us model = 
    let
        viewBox =
            Rectangle2d.from Point2d.origin (Point2d.pixels 800 450)
        
        level = Maybe.withDefault 0.0 <| List.head xs
        input = Maybe.withDefault 0.0 <| List.head us
        reference = Maybe.withDefault 0.0 <| List.head rs
        ag = .ag model
        ap = .ap model
        lbase = 200.0
        l = lbase*sqrt(ag)
        hbase = 300.0
        h = hbase
        e = 10.0    
        esc = 30.0
        abbase = 20.0
        ab = 3*abbase*sqrt(ap)
        xini = 100.0
        yini = 100.0

        pa = Point2d.pixels xini yini
        pb = Point2d.pixels (xini+l) yini
        pc = Point2d.pixels xini (yini+h)
        pd = Point2d.pixels (xini+l) (yini+h)
             
        p1 = extendP2d pb (esc+e) 0.0
        p2 = extendP2d p1 0.0 (-e)
        p3 = extendP2d pa (-e) (-e)
        p4 = extendP2d pc (-e) (-e-ab)
        p5 = extendP2d p4 (-esc) (0.0)
        p6 = extendP2d p5 0.0 e
        p7 = extendP2d p6 (e+esc) 0.0

        p8 = extendP2d pc (-e-esc) 0.0
        p9 = extendP2d p8 0.0 e
        p10 = extendP2d pd e e
        p11 = extendP2d pb e (ab+e)
        p12 = extendP2d p11 (esc) 0.0
        p13 = extendP2d p12 0.0 (-e)
        p14 = extendP2d pb 0.0 (ab)

                 
        poly1 = Polygon2d.singleLoop [pa,p1,p2,p3,p4,p5,p6,p7]
        poly2 = Polygon2d.singleLoop [pc,p8,p9,p10,p11,p12,p13,p14,pd]
                
        levelmax = 20.0
        levelPixelAux = (level/levelmax)*h
        levelPixel = min levelPixelAux h
        prect1 = pa
        prect2 = extendP2d prect1 l levelPixel
        rect = Rectangle2d.from prect1 prect2

        hrb = Maybe.withDefault 0.0 <| List.minimum [levelPixel,ab]
        prb1 = pb 
        prb2 = extendP2d prb1 (e+esc) hrb
        rectb = Rectangle2d.from prb1 prb2
        
        input2 = max input 0.0
        -- qo = input2*10.0
        qo = input2/ap
        -- qo = 10.0
        hqo = if (qo > ab) then ab
              else if (qo < 0.0) then 0.0
              else qo
                  
        pra11 = p6
        pra12 = extendP2d pra11 (esc+e) (hqo)
        recta1 = Rectangle2d.from pra11 pra12

        pra21 = extendP2d pra11 (esc+e+hqo) 0.0
        pra22 = pa
        recta2 = Rectangle2d.from pra21 pra22

        parca1 = pra12
        -- parca1 = Point2d.pixels 200.0 200.0
        parca2 = pra21
        -- parca2 = Point2d.pixels 250.0 250.0
        anglea = Angle.degrees 90.0
        arca = Arc2d.from parca2 parca1 anglea

        nSegs = 15
        arcAsPolylineSegsA = Arc2d.segments nSegs arca
        polya = Polygon2d.singleLoop ((Arc2d.centerPoint arca)::(Polyline2d.vertices arcAsPolylineSegsA))
        
        waterFall = 50.0
        prc1 = p1
        prc2 = extendP2d prc1 (hrb) (-waterFall)
        rectc = Rectangle2d.from prc1 prc2
                
        parcb1 = extendP2d p1 0.0 hrb
        parcb2 = extendP2d p1 hrb 0.0
        angleb = Angle.degrees 90.0
        arcb = Arc2d.from parcb2 parcb1 angleb
               
        arcAsPolylineSegsB = Arc2d.segments nSegs arcb
        polyb = Polygon2d.singleLoop ((Arc2d.centerPoint arcb)::(Polyline2d.vertices arcAsPolylineSegsB))
                
        rectBB = Rectangle2d.from prc2 p9 
        boundingBox = Rectangle2d.boundingBox rectBB
        rectBB2 = Rectangle2d.fromBoundingBox boundingBox

        points = [pa,pb,pc,pd,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,parca1,parca2,prc1,prc2,parcb1,parcb2]
    in 
    Drawing2d.draw
        { viewBox = rectBB2
        , entities = [
              Drawing2d.group [Drawing2d.fillColor Color.lightBlue, Drawing2d.strokeWidth <| Pixels.float 0.0]
                [ Drawing2d.rectangle [] rect
                , Drawing2d.rectangle [] rectb
                , Drawing2d.rectangle [] recta1
                , Drawing2d.rectangle [] recta2
                , Drawing2d.rectangle [] rectc
                -- , Drawing2d.arc [Drawing2d.strokeWidth <| Pixels.float 1.0] arca
                , Drawing2d.polygon [] polya
                -- , Drawing2d.arc [Drawing2d.strokeWidth <| Pixels.float 1.0] arcb
                , Drawing2d.polygon [] polyb
                ]
            , Drawing2d.group [Drawing2d.fillColor Color.grey]
                [ Drawing2d.polygon [] poly1        
                , Drawing2d.polygon [] poly2
                ]
            ] 
            -- ++ List.map dot points
        }

        
extendP2d : Point2d.Point2d Pixels.Pixels coordinates -> Float -> Float -> Point2d.Point2d Pixels.Pixels coordinates
extendP2d p dx dy =
   let 
       px = Pixels.toFloat (Point2d.xCoordinate p)
       py = Pixels.toFloat (Point2d.yCoordinate p)
   in
       Point2d.pixels (px + dx) (py + dy)
           
dot : Point2d.Point2d Pixels.Pixels coordinates -> Drawing2d.Entity Pixels.Pixels coordinates msg
dot point =
    Drawing2d.circle
        [ Drawing2d.blackStroke
        , Drawing2d.whiteFill
        , Drawing2d.strokeWidth (pixels 1)
        ]
        (Circle2d.withRadius (pixels 4) point)
