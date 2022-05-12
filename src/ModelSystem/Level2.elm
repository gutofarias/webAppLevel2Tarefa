module ModelSystem.Level2 exposing (..)

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
    { h01 : Float , ag1 : Float , ap1 : Float
    , h01Str : String, ag1Str : String, ap1Str : String
    , h02 : Float , ag2 : Float , ap2 : Float
    , h02Str : String, ag2Str : String, ap2Str : String }
    
    
------------------------------------------------
-- init
------------------------------------------------
    
init : Model
init =
    { h01 = 10.0
    , ag1 = 1.0
    , ap1 = 0.1
    , h01Str = "10"
    , ag1Str = "1"
    , ap1Str = "0.1" 
    , h02 = 10.0
    , ag2 = 1.0
    , ap2 = 0.1
    , h02Str = "10"
    , ag2Str = "1"
    , ap2Str = "0.1" }
    
    
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = H01 String
    | Ag1 String
    | Ap1 String
    | H02 String
    | Ag2 String
    | Ap2 String
      
    
    
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model
update msg model =
    case msg of
        H01 valueStr -> 
            let
                h01 = .h01 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault h01 maybeVal
            in
            { model | h01Str = valueStr, h01 = val }
        Ag1 valueStr ->
            let
                ag1 = .ag1 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ag1 maybeVal
            in
            { model | ag1Str = valueStr, ag1 = val }
        Ap1 valueStr -> 
            let
                ap1 = .ap1 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ap1 maybeVal
            in
            { model | ap1Str = valueStr, ap1 = val }

        H02 valueStr -> 
            let
                h02 = .h02 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault h02 maybeVal
            in
            { model | h02Str = valueStr, h02 = val }
        Ag2 valueStr ->
            let
                ag2 = .ag2 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ag2 maybeVal
            in
            { model | ag2Str = valueStr, ag2 = val }
        Ap2 valueStr -> 
            let
                ap2 = .ap2 model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ap2 maybeVal
            in
            { model | ap2Str = valueStr, ap2 = val }
                
------------------------------------------------
-- view
------------------------------------------------
    
view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg = 
    let 
        h01Str = .h01Str model
        ag1Str = .ag1Str model
        ap1Str = .ap1Str model
        h02Str = .h02Str model
        ag2Str = .ag2Str model
        ap2Str = .ap2Str model
    in 
        
    E.column [E.spacing 10, E.padding 20, E.centerX]
        [ UI.heading "Level 2"
        , E.row [E.spacing 35]
                [ UI.textField h01Str "H1ini" (msgToMainMsg << H01)
                , UI.textField ag1Str "A1 " (msgToMainMsg << Ag1)
                , UI.textField ap1Str "a1 " (msgToMainMsg << Ap1)
                ]
        , E.row [E.spacing 35]
                [ UI.textField h02Str "H2ini" (msgToMainMsg << H02)
                , UI.textField ag2Str "A2 " (msgToMainMsg << Ag2)
                , UI.textField ap2Str "a2 " (msgToMainMsg << Ap2)
                ]
        ]
        
        
------------------------------------------------
-- xsFromModel
------------------------------------------------

xsFromModel : Model -> Edo.State
xsFromModel model =
    [(.h01 model), (.h02 model)]
                    
        
------------------------------------------------
-- updateModelFromXs
------------------------------------------------

updateModelFromXs : Edo.State -> Model -> Model
updateModelFromXs xs model = 
    let
        h01 = .h01 model
        h02 = .h02 model
        (newH01, newH02) = 
            case xs of
                [xs1,xs2] -> (xs1,xs2)
                _ -> (0.0, 0.0)
    in
        {model | h01 = newH01, h02 = newH02}
    
            
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
        (x1::x2::ls) -> [x2]
        _ -> xs

             
------------------------------------------------
-- runEdo
------------------------------------------------

runEdo : Model -> Edo.EdoParam -> Edo.RefFunction -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
runEdo model edoParam refFunc controller =
        let
            initState = (.h01 model) :: (.h02 model) :: []
            edoSist = Edo.Controlled
                      { refFunc = refFunc
                      , outputFunc = output
                      , controller = controller
                      , sistFunc = (system model)}
            
            (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
        in
            (DC.toChartDataTS2E1R1U4 edoData, edoParamNew)
                
------------------------------------------------
-- system equation
------------------------------------------------
        
system : Model -> Edo.ControlEffort -> Edo.Tempo -> Edo.State -> Edo.DState
system model us t state =
    let
       ag1 = .ag1 model
       ap1 = .ap1 model
       ag2 = .ag2 model
       ap2 = .ap2 model
       g = 9.81
       uAux = Maybe.withDefault 0.0 (List.head us)
       u = if (uAux<0.0) then 0.0 else uAux
    in
       case state of
           h1::h2::ls -> 
               let
                   h1n = if h1 >= 0.0 then h1 else 0.0
                   h2n = if h2 >= 0.0 then h2 else 0.0
                   qi1 = u
                   qo1 = ap1*sqrt(2.0*g)*sqrt(h1n)
                   qi2 = qo1
                   qo2 = ap2*sqrt(2.0*g)*sqrt(h2n)
                   -- hnlog = Debug.log "sist" (hn,u)
               in 
                   (qi1/ag1) - (qo1/ag1) ::
                   (qi2/ag2) - (qo2/ag2) :: []
           _ -> 0.0 :: []
      

------------------------------------------------
-- simulation
------------------------------------------------

simulation : Edo.State -> Edo.Ref -> Edo.ControlEffort -> Model -> Html.Html msg
simulation xs rs us model = 
    let
        newOrigin = Point2d.pixels 0 -400
        viewBox =
            Rectangle2d.from newOrigin (Point2d.pixels 800 450)
        -- boundingBox = Rectangle2d.boundingBox rectBB
        -- rectBB2 = Rectangle2d.fromBoundingBox boundingBox
        
        -- level = Maybe.withDefault 0.0 <| List.head xs
        (level1, level2) =
            case xs of
                x1::x2::ls -> (x1,x2)
                _ -> (0.0,0.0)

        input = Maybe.withDefault 0.0 <| List.head us
        reference = Maybe.withDefault 0.0 <| List.head rs
        ag1 = .ag1 model
        ap1 = .ap1 model
        lbase = 200.0
        l1 = lbase*sqrt(ag1)
        hbase = 300.0
        h = hbase
        e = 10.0    
        esc = 30.0
        abbase = 20.0
        ab1 = 3*abbase*sqrt(ap1)
        xini = 100.0
        yini = 100.0

        pa = Point2d.pixels xini yini
        pb = Point2d.pixels (xini+l1) yini
        pc = Point2d.pixels xini (yini+h)
        pd = Point2d.pixels (xini+l1) (yini+h)
             
        p1 = extendP2d pb (esc+e) 0.0
        p2 = extendP2d p1 0.0 (-e)
        p3 = extendP2d pa (-e) (-e)
        p4 = extendP2d pc (-e) (-e-ab1)
        p5 = extendP2d p4 (-esc) (0.0)
        p6 = extendP2d p5 0.0 e
        p7 = extendP2d p6 (e+esc) 0.0

        p8 = extendP2d pc (-e-esc) 0.0
        p9 = extendP2d p8 0.0 e
        p10 = extendP2d pd e e
        p11 = extendP2d pb e (ab1+e)
        p12 = extendP2d p11 (esc) 0.0
        p13 = extendP2d p12 0.0 (-e)
        p14 = extendP2d pb 0.0 (ab1)

                 
        poly1 = Polygon2d.singleLoop [pa,p1,p2,p3,p4,p5,p6,p7]
        poly2 = Polygon2d.singleLoop [pc,p8,p9,p10,p11,p12,p13,p14,pd]
                
        levelmax = 20.0
        level1PixelAux = (level1/levelmax)*h
        level1Pixel = min level1PixelAux h
        prect1 = pa
        prect2 = extendP2d prect1 l1 level1Pixel
        rect = Rectangle2d.from prect1 prect2

        hrb1 = Maybe.withDefault 0.0 <| List.minimum [level1Pixel,ab1]
        prb1 = pb 
        prb2 = extendP2d prb1 (e+esc) hrb1
        rectb = Rectangle2d.from prb1 prb2
        
        input2 = max input 0.0
        -- qo = input2*10.0
        qo1 = input2/ap1
        -- qo = 10.0
        hqo1 = if (qo1 > ab1) then ab1
              else if (qo1 < 0.0) then 0.0
              else qo1
                  
        pra11 = p6
        pra12 = extendP2d pra11 (esc+e) (hqo1)
        recta1 = Rectangle2d.from pra11 pra12

        pra21 = extendP2d pra11 (esc+e+hqo1) 0.0
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
        
        waterFall = 350.0
        prc1 = p1
        prc2 = extendP2d prc1 (hrb1) (-waterFall)
        rectc = Rectangle2d.from prc1 prc2
                
        parcb1 = extendP2d p1 0.0 hrb1
        parcb2 = extendP2d p1 hrb1 0.0
        angleb = Angle.degrees 90.0
        arcb = Arc2d.from parcb2 parcb1 angleb
               
        arcAsPolylineSegsB = Arc2d.segments nSegs arcb
        polyb = Polygon2d.singleLoop ((Arc2d.centerPoint arcb)::(Polyline2d.vertices arcAsPolylineSegsB))
                
        rectBB = Rectangle2d.from prc2 p9 
        boundingBox = Rectangle2d.boundingBox rectBB
        rectBB2 = Rectangle2d.fromBoundingBox boundingBox

        points = [pa,pb,pc,pd,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,parca1,parca2,prc1,prc2,parcb1,parcb2]

        -- Segundo tanque
        ag2 = .ag2 model
        ap2 = .ap2 model
        l2 = lbase*sqrt(ag2)
        ab2 = 3*abbase*sqrt(ap2)
        zpixels = Point2d.toPixels p1
              
        xini2 = zpixels.x - 30
        yini2 = zpixels.y - 40
                
               
        zc = Point2d.pixels xini2 yini2
        zd = Point2d.pixels (xini2+l2) yini2
        za = Point2d.pixels xini2 (yini2-h)
        zb = Point2d.pixels (xini2+l2) (yini2-h)
             
        z1 = extendP2d zb (esc+e) 0.0
        z2 = extendP2d z1 0.0 (-e)
        z3 = extendP2d za (-e) (-e)
        z4 = extendP2d zc (-e) (e)
        z7 = extendP2d z4 (e) 0.0

        z9 = extendP2d zd 0.0 e 
        z10 = extendP2d zd e e
        z11 = extendP2d zb e (ab2+e)
        z12 = extendP2d z11 (esc) 0.0
        z13 = extendP2d z12 0.0 (-e)
        z14 = extendP2d zb 0.0 (ab2)
              
        zoly1 = Polygon2d.singleLoop [za,z1,z2,z3,z4,z7]
        zoly2 = Polygon2d.singleLoop [z9,z10,z11,z12,z13,z14,z9]
              
        level2PixelAux = (level2/levelmax)*h
        level2Pixel = min level2PixelAux h
        zrect1 = za
        zrect2 = extendP2d zrect1 l2 level2Pixel
        zrect = Rectangle2d.from zrect1 zrect2

        zhrb1 = Maybe.withDefault 0.0 <| List.minimum [level2Pixel,ab2]
        zrb1 = zb 
        zrb2 = extendP2d zrb1 (e+esc) zhrb1
        zrectb = Rectangle2d.from zrb1 zrb2
                 
        newWaterFall = 50
        zrc1 = z1
        zrc2 = extendP2d zrc1 (zhrb1) (-newWaterFall)
        zrectc = Rectangle2d.from zrc1 zrc2
                
        zarcb1 = extendP2d z1 0.0 zhrb1
        zarcb2 = extendP2d z1 zhrb1 0.0
        zangleb = Angle.degrees 90.0
        zarcb = Arc2d.from zarcb2 zarcb1 zangleb
               
        zarcAsPolylineSegsB = Arc2d.segments nSegs zarcb
        zolyb = Polygon2d.singleLoop ((Arc2d.centerPoint zarcb)::(Polyline2d.vertices zarcAsPolylineSegsB))
                
        zrectBB = Rectangle2d.from zrc2 p9 
        zboundingBox = Rectangle2d.boundingBox zrectBB
        zrectBB2 = Rectangle2d.fromBoundingBox zboundingBox
                
                
        points2 =
           [za,zb,zc,zd,z1,z2,z3,z4,z7,z9,z10,z11,z12,z13,z14,zrb1,zrb2]
            -- [za,zb,zc,zd]

    in 
    Drawing2d.draw
        { viewBox = zrectBB2
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
            , Drawing2d.group [Drawing2d.fillColor Color.lightBlue, Drawing2d.strokeWidth <| Pixels.float 0.0]
            [ Drawing2d.rectangle [] zrect
            , Drawing2d.rectangle [] zrectb
            , Drawing2d.rectangle [] zrectc
            -- , Drawing2d.rectangle [] zrectBB
            , Drawing2d.polygon [] zolyb
            ]
            , Drawing2d.group [Drawing2d.fillColor Color.grey]
                [ Drawing2d.polygon [] zoly1        
                , Drawing2d.polygon [] zoly2
                ]
            ] 
            -- ++ List.map dot points2
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
