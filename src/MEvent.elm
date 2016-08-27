module MEvent exposing (..)

{-|


-}

import Maybe exposing (withDefault)
import Ratio exposing (Rational, over, fromInt, divide)
import Ratio.Infix exposing (..)
import Music exposing (..)
import MoreMusic exposing (..)



type alias MEvent = 
   { eTime    : PTime
   , eInst    : InstrumentName
   , ePitch   : AbsPitch
   , eDur     : DurT
   , eVol     : Volume
   , eParams  : List Float
   }

type alias Performance = List MEvent

type alias PTime = Rational
type alias DurT = Rational



type alias MContext  = 
   { mcTime    : PTime
   , mcInst    : InstrumentName
   , mcDur     : DurT
   , mcVol     : Volume
   }


merge : Performance -> Performance -> Performance
merge es1 es2 =
  let
    hes1 = List.head es1
    hes2 = List.head es2
    tailEs1 = List.tail es1
            |> withDefault []
    tailEs2 = List.tail es2
            |> withDefault []
  in
    case (hes1, hes2) of
      (Nothing, _) ->
         es2
      (_, Nothing) ->
         es1
      (Just h1, Just h2) ->
         if Ratio.lt (.eTime h1) (.eTime h2) then
           h1 :: merge tailEs1 es2
         else
           h2 :: merge es1 tailEs2

{-
perform : Music a -> Performance
perform = 
  perform1 . toMusic1

perform1 : Music1 -> Performance
perform1 = 
  fst . perform1Dur
-}


perform1Dur : Music1 -> (Performance, DurT)
perform1Dur m = 
  let
    defCon  = { mcTime = (fromInt 0), mcInst = AcousticGrandPiano, mcDur = (metro 120 qn), mcVol = 127 }
    metro : Int -> Dur -> DurT
    metro setting dur  = 60 /| (setting *| dur)
    applyControls : Music1 -> Music1
    applyControls m =
      case m of
        (Modify (Tempo r) m) ->
          scaleDurations r <| applyControls m
        (Modify (Transpose k) m) -> 
          shiftPitches1 k <| applyControls m
        Seq m1 m2 -> 
          Seq (applyControls m1) (applyControls m2) 
        Par m1 m2 -> 
          Par (applyControls m1) (applyControls m2) 
        _ -> m
  in
    musicToMEvents defCon (applyControls m)

musicToMEvents : MContext -> Music1 -> (Performance, DurT)
musicToMEvents c m =
  let
    t  = .mcTime c
    dt = .mcDur c
  in
    case m of
      Prim (Note d p) ->
       ([noteToMEvent c d p], d |*| dt)
      Prim (Rest d) ->
        ([], d |*| dt)
      Seq m1 m2 -> 
        let 
          (evs1, d1) = musicToMEvents c m1
          (evs2, d2) = musicToMEvents { c | mcTime = t |+| d1} m2
        in  
          (evs1 ++ evs2, d1 |+| d2)
      Par m1 m2 -> 
        let 
          (evs1, d1) = musicToMEvents c m1
          (evs2, d2) = musicToMEvents { c | mcTime = t |+| d1} m2
        in  
          (merge evs1 evs2, Ratio.max d1 d2)
      Modify (Instrument i) m ->
        musicToMEvents {c | mcInst=i} m
      Modify (Phrase pas) m -> 
        phraseToMEvents c pas m
      Modify x m -> 
        musicToMEvents c m -- Transpose a

noteToMEvent : MContext -> Dur -> (Pitch, (List NoteAttribute)) -> MEvent
noteToMEvent c d (p, nas) = 
   let 
     e0 = {eTime=c.mcTime, eInst=c.mcInst, ePitch=absPitch p, eDur= (d |*| c.mcDur), eVol=c.mcVol, eParams=[]}
     nasFun : NoteAttribute -> MEvent -> MEvent
     nasFun na ev =
       case na of
          Volume v ->
            {ev  | eVol = v }
          Params pms ->
            {ev | eParams = pms}
          _ ->
            ev 
   in  
     List.foldr nasFun e0 nas

phraseToMEvents : MContext -> List PhraseAttribute -> Music1 -> (Performance, DurT)
phraseToMEvents c pAtts m =
  let
    t = .mcTime c
    i = .mcInst c
    dt = .mcDur c
  in case pAtts of
    pa :: pas ->
      -- this case not finished!!
      musicToMEvents c m
    _ -> 
      musicToMEvents c m

{-
> phraseToMEvents c [] m = musicToMEvents c m
> phraseToMEvents c@MContext{mcTime=t, mcInst=i, mcDur=dt} (pa:pas) m =
>  let  pfd@(pf,dur)  =  phraseToMEvents c pas m
>       loud x        =  phraseToMEvents c (Dyn (Loudness x) : pas) m
>       stretch x     =  let  t0 = eTime (head pf);  r  = x/dur
>                             upd (e@MEvent {eTime = t, eDur = d}) = 
>                               let  dt  = t-t0
>                                    t'  = (1+dt*r)*dt + t0
>                                    d'  = (1+(2*dt+d)*r)*d
>                               in e {eTime = t', eDur = d'}
>                        in (map upd pf, (1+x)*dur)
>       inflate x     =  let  t0  = eTime (head pf);  
>                             r   = x/dur
>                             upd (e@MEvent {eTime = t, eVol = v}) = 
>                                 e {eVol =  round ( (1+(t-t0)*r) * 
>                                            fromIntegral v)}
>                        in (map upd pf, dur)
-}






