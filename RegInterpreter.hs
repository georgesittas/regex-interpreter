module RegInterpreter (makeNfa, nfaToDfa, regexFullMatch, regexPartMatch) where

import RegParser
import Data.List

type StateId = Int
type TransChar = Char
type Inputs = [TransChar]
type Transition = (StateId, StateId, TransChar)
type States = [StateId]
type Transitions = [Transition]
type FirstState = StateId
type LastStates = States
type Fsa = (States, Inputs, Transitions, FirstState, LastStates)

-- Auxiliary functions

my_group :: Eq t => [t] -> [[t]]
my_group [] = []
my_group (x:xs) = groupx [x] x xs

groupx :: Eq t => [t] -> t -> [t] -> [[t]]
groupx r _ [] = [r]
groupx r x (y:ys) = if y == x then groupx (x:r) x ys else r : (groupx [y] y ys)

filt_dups = map head . my_group . sort

-- Core functions

makeNfa :: [Char] -> Fsa
makeNfa regex = snd $ makeNfa' (parseRegexpr regex) 1

makeNfa' :: RegExpr -> StateId -> (StateId, Fsa)
makeNfa' EmptyChar sid = (sid+2, ([sid,sid+1], [], [(sid,sid+1,'_')], sid, [sid+1]))
makeNfa' AnyLetter sid = (sid+2, ([sid,sid+1], [], [(sid,sid+1,'.')], sid, [sid+1]))
makeNfa' (Letter(x)) sid = (sid+2, ([sid,sid+1], [x], [(sid,sid+1,x)], sid, [sid+1]))
makeNfa' (Union(x,y)) sid =
  let (sid1, (s1,i1,t1,f1,[l1])) = makeNfa' x sid
      (sid2, (s2,i2,t2,f2,[l2])) = makeNfa' y sid1
      i = i1 ++ [c | c <- i2, c `notElem` i1]
      t = t1 ++ t2 ++ [(sid2,f1,'_'),(sid2,f2,'_'),(l1,sid2+1,'_'),(l2,sid2+1,'_')]
  in (sid2+2, (s1 ++ s2 ++ [sid2,sid2+1], i, t, sid2, [sid2+1]))
makeNfa' (Concat(x,y)) sid =
  let (sid1, (s1,i1,t1,f1,[l1])) = makeNfa' x sid
      (sid2, (s2,i2,t2,f2,[l2])) = makeNfa' y sid1
      i = i1 ++ [c | c <- i2, c `notElem` i1]
      t = t1 ++ t2 ++ [(l1,f2,'_')]
  in (sid2, (s1 ++ s2, i, t, f1, [l2]))
makeNfa' (Kleene(x)) sid =
  let (sid', (s',i',t',f',[l'])) = makeNfa' x sid
      t = t' ++ [(sid',f','_'),(l',sid'+1,'_'),(sid',sid'+1,'_'),(l',f','_')]
  in (sid'+2, (s' ++ [sid',sid'+1], i', t, sid', [sid'+1]))

nfaToDfa :: Fsa -> Fsa
nfaToDfa (_, inputs, transitions, firstState, lastStates) =
    ([1..numStates], inputs, finalTransitions, 1, finalStates)
  where
    beginState = filt_dups (firstState : (getNextStates '_' [firstState] transitions))
    newTransitions = findTransitions beginState transitions ('.' : inputs) ('.' : inputs) []
    (finalTransitions, finalStates, numStates) =
      makeDfa newTransitions lastStates [(beginState, 1)] 1

getNextStates :: TransChar -> States -> Transitions -> States
getNextStates _ [] _ = []
getNextStates ch (state:rest) transitions =
  dfsStates ++ (getNextStates ch rest transitions)
  where
    (dfsStates, visitedStates) = (getDfsStates ch transitions transitions state [] [state])

getDfsStates :: TransChar -> Transitions -> Transitions -> StateId -> States -> States -> (States, States)
getDfsStates _ [] _ _ resultStates visited = (resultStates, visited)
getDfsStates ch ((sState,tState,tch) : transitions) allTransitions curState resultStates visited
  | (sState == curState && ch == tch && (tState `notElem` restStates))
              = getDfsStates '_' allTransitions allTransitions tState (tState : restStates) restVisited
  | (sState == curState && ch /= '_' && tch == '_' && (tState `notElem` restVisited))
              = getDfsStates ch allTransitions allTransitions tState restStates (tState:restVisited)
  | otherwise = (restStates, restVisited)
  where
    (restStates, restVisited) = getDfsStates ch transitions allTransitions curState resultStates visited

makeDfa :: [(States, States, Char)]
        -> States
        -> [(States, Int)]
        -> Int
        -> (Transitions, States, Int)
makeDfa [] lastStates statesMap counterStates = ([], [], counterStates)
makeDfa ((curStates, nextStates, curIn):newTransitions) lastStates statesMap counterStates
    = (finalTransitions, finalStates2, numStates)
  where
    retId = (findId curStates statesMap)
    a = if retId == -1 then 1 else 0
    id1 = if a == 1 then counterStates + 1 else retId
    newStatesMap = if a == 1 then (curStates, id1) : statesMap else statesMap

    retId2 = (findId nextStates newStatesMap)
    b = if retId2 == -1 then 1 else 0
    id2 = if b == 1 then counterStates + a + 1 else retId2
    newStatesMap2 = if b == 1 then ((nextStates, id2):newStatesMap) else newStatesMap

    newCounterStates = counterStates + a + b

    (nextFinalTransitions, nextFinalStates, numStates) =
      makeDfa newTransitions lastStates newStatesMap2 newCounterStates

    finalStates =
      if checkFinalStates curStates lastStates && id1 `notElem` nextFinalStates then
        (id1:nextFinalStates)
      else
        nextFinalStates

    finalStates2 =
      if checkFinalStates nextStates lastStates && id2 `notElem` finalStates then
        (id2:finalStates)
      else
        finalStates

    finalTransitions = ((id1, id2, curIn):nextFinalTransitions)

checkFinalStates :: States -> States -> Bool
checkFinalStates [] lastStates = False
checkFinalStates (x:curStates) lastStates =
  if x `elem` lastStates then True else checkFinalStates curStates lastStates

findId :: States -> [(States,Int)] -> Int
findId curStates [] = -1
findId curStates ((states,id1) : statesMap) =
  if curStates == states then id1 else findId curStates statesMap

checkNextStates :: States -> [(States, States, Char)] -> Bool
checkNextStates x [] = False
checkNextStates x ((curStates, nextStates, curIn) : xs) =
  if curStates == x then True else checkNextStates x xs

findTransitions :: States
                -> Transitions
                -> Inputs
                -> Inputs
                -> [(States, States, Char)]
                -> [(States, States, Char)]
findTransitions [] transitions inputs oInputs resultTransitions = resultTransitions
findTransitions curStates transitions [] oInputs resultTransitions = resultTransitions
findTransitions curStates transitions (curIn:inputs) oInputs resultTransitions = newTransitions
  where
    nextStates = (filt_dups (getNextStates curIn curStates transitions))
    nextNewTransitions = findTransitions curStates transitions inputs oInputs resultTransitions

    transit =
      if (curStates, nextStates, curIn) `elem` nextNewTransitions || nextStates == [] then
        nextNewTransitions
      else
        ((curStates, nextStates, curIn):nextNewTransitions)

    tmp =
      if (checkNextStates nextStates nextNewTransitions) || nextStates == [] then
        transit
      else
        (findTransitions nextStates transitions oInputs oInputs transit)

    newTransitions = tmp

regexFullMatch :: ([Char], [Char]) -> Bool
regexFullMatch (regex,input) = regexFullMatch' dfa input first_state
  where dfa@(_,_,_,first_state,_) = nfaToDfa (makeNfa regex)

regexFullMatch' :: Fsa -> [Char] -> StateId -> Bool
regexFullMatch' (_,_,_,_,l) [] sid = sid `elem` l
regexFullMatch' dfa@(_,_,t,_,_) (x:xs) sid =
  let next_states = [y | (k,y,c) <- t, k == sid, (c == x || c == '.')]
  in  if null next_states then False else checkPossibleStates dfa next_states xs

checkPossibleStates :: Fsa -> States -> [Char] -> Bool
checkPossibleStates _ [] _ = False
checkPossibleStates dfa (state : rest) xs =
  if regexFullMatch' dfa xs state then True else checkPossibleStates dfa rest xs

regexPartMatch :: ([Char], [Char]) -> [[Char]]
regexPartMatch (regex, input) =
  [take i input | i <- [1..(length input)], regexFullMatch (regex, take i input)]
