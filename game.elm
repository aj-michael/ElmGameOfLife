import Graphics.Collage (collage, rect, filled, move, Form)
import Graphics.Element (Element)
import Signal (foldp,Signal,(~),(<~),merge)
import Signal
import Color (darkGrey)
import Time (fps)
import Window
import Mouse
import List
import Dict
import Set

type Input = TimeDelta Float | MouseClick (Int,Int)

render : (Int,Int) -> List (Int,Int) -> Element
render (w',h') squares =
  let (w,h) = (toFloat w', toFloat h')
  in List.map draw squares
     |> collage w' h'

draw : (Int,Int) -> Form
draw (x,y) =
  let (x',y') = (20*(toFloat x),20*(toFloat y))
  in rect 20 20
  |> filled darkGrey
  |> move (x',y')

initial : List (Int,Int)
initial =
  (toad 10 20)
  ++ (pulsar -10 15)
  ++ (unix 10 0)
  ++ (gun -20 -10)

glider : Int -> Int -> List (Int,Int)
glider a b =
  let g = [(10,10),(11,10),(12,10),(12,11),(11,12)]
  in g

toad : Int -> Int -> List (Int,Int)
toad a b =
  let t = [(0,0),(1,0),(2,0),(1,1),(2,1),(3,1)]
  in List.map (\(x,y) -> (x+a,y+b)) t

pulsar : Int -> Int -> List (Int,Int)
pulsar a b = 
  let p = [(2,1),(3,1),(4,1),(6,2),(6,3),(6,4),(1,2),(1,3),(1,4),(2,6),(3,6),(4,6)]
  in List.concatMap (\(x,y) -> [(x+a,y+b),(-x+a,y+b),(x+a,-y+b),(-x+a,-y+b)]) p

unix : Int -> Int -> List (Int,Int)
unix a b =
  let u = [(0,2),(0,3),(1,4),(1,6),(1,7),(2,0),(2,3),(2,6),(2,7),(3,0),(3,2),(4,1),(6,1),(6,2),(7,1),(7,2)]
  in List.map (\(x,y) -> (x+a,y+b)) u

gun : Int -> Int -> List (Int,Int)
gun a b =
  let g = [(0,3),(0,4),(1,3),(1,4),(10,2),(10,3),(10,4),(11,1),(11,5),(12,0),(12,6),(13,0),(13,6),(14,3),(15,1),(15,5),(16,2),(16,3),(16,4),(17,3),(20,4),(20,5),(20,6),(21,4),(21,5),(21,6),(22,3),(22,7),(24,2),(24,3),(24,7),(24,8),(34,5),(34,6),(35,5),(35,6)]
  in List.map (\(x,y) -> (x+a,y+b)) g

update : Input -> List (Int,Int) -> List (Int,Int)
update input squares = case input of
  TimeDelta _ ->
    let filtered = List.filter (alive squares) squares
        spawned = reborn squares     
    in filtered ++ spawned
       |> Set.fromList
       |> Set.toList
  MouseClick (x,y) -> (x//20,y//20) :: squares

reborn : List (Int,Int) -> List (Int,Int)
reborn list =
  let allNeighbors = List.concatMap neighborhood list
      counter = List.foldr (\k d -> Dict.update k incr d) Dict.empty allNeighbors
      filtered = Dict.filter (\k v -> v == 3) counter
  in Dict.keys filtered

incr : Maybe Int -> Maybe Int
incr m = case m of
  Just v -> Just (v+1)
  Nothing -> Just 1

alive : List (Int,Int) -> (Int,Int) -> Bool
alive list (tx,ty) =
  let onScreen = abs tx < 2000 && abs ty < 2000
      liveNeighbors = List.filter (\t -> List.member t (neighborhood (tx,ty))) list
      count = List.length(liveNeighbors)
  in (count == 2 || count == 3) && onScreen

neighborhood : (Int,Int) -> List (Int,Int)
neighborhood (x,y) = List.map (\(m,n) -> (x+m,y+n)) [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

state : Signal (List (Int,Int))
state = foldp update initial input

input : Signal Input
input = merge
  (Signal.map TimeDelta (fps 30))
  (Signal.map MouseClick (Signal.map2 adjust (Signal.sampleOn Mouse.clicks Mouse.position) Window.dimensions))

adjust : (Int,Int) -> (Int,Int) -> (Int,Int)
adjust (clickX,clickY) (dimX,dimY) = (clickX-dimX//2,dimY//2-clickY)

main : Signal Element
main = render <~ Window.dimensions ~ state