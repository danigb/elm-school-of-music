module MEvent exposing (..)

{-|


-}

import Maybe exposing (withDefault)
import Tuple exposing (first, second)
import Ratio exposing (Rational, over, fromInt, divide, toFloat, negate)
import Ratio.Infix exposing (..)
import Music exposing (..)
import MoreMusic exposing (..)
import Music1 exposing (..)


type alias MEvent =
    { eTime : PTime
    , eInst : InstrumentName
    , ePitch : AbsPitch
    , eDur : DurT
    , eVol : Volume
    , eParams : List Float
    }


type alias Performance =
    List MEvent


type alias PTime =
    Rational


type alias DurT =
    Rational


type alias MContext =
    { mcTime : PTime
    , mcInst : InstrumentName
    , mcDur : DurT
    , mcVol : Volume
    }


nullEvent : MEvent
nullEvent =
    { eTime = over 0 1
    , eInst = AcousticGrandPiano
    , ePitch = 0
    , eDur = over 0 1
    , eVol = 0
    , eParams = []
    }


merge : Performance -> Performance -> Performance
merge es1 es2 =
    let
        hes1 =
            List.head es1

        hes2 =
            List.head es2

        tailEs1 =
            List.tail es1
                |> withDefault []

        tailEs2 =
            List.tail es2
                |> withDefault []
    in
        case ( hes1, hes2 ) of
            ( Nothing, _ ) ->
                es2

            ( _, Nothing ) ->
                es1

            ( Just h1, Just h2 ) ->
                if Ratio.lt (.eTime h1) (.eTime h2) then
                    h1 :: merge tailEs1 es2
                else
                    h2 :: merge es1 tailEs2



{-
   toMusic1 is a polymorphic function in the Haskell implemented using
   type classes (which don't exist in Elm).  We need an unobtrusive way
   of somehow emulating this.

   Bottom line is perhaps just to have a bunch of functions with slightly different names
   or one function with an extra parameter that disambiguates the types.  Both sordid.


   perform : Music a -> Performance
   perform =
     perform1 << toMusic1
-}


perform1 : Music1 -> Performance
perform1 =
    first << perform1Dur



-- other instances - I think that this is the best we can do, given that we are denied true abstractio


performP : Music Pitch -> Performance
performP =
    (makeMusic pMusicMaker)
        >> (first << perform1Dur)


performPV : Music ( Pitch, Volume ) -> Performance
performPV =
    (makeMusic pvMusicMaker)
        >> (first << perform1Dur)


performAP : Music AbsPitch -> Performance
performAP =
    (makeMusic apMusicMaker)
        >> (first << perform1Dur)



-- end of other instances


perform1Dur : Music1 -> ( Performance, DurT )
perform1Dur m =
    let
        defCon =
            { mcTime = (fromInt 0), mcInst = AcousticGrandPiano, mcDur = (metro 120 qn), mcVol = 127 }

        metro : Int -> Dur -> DurT
        metro setting dur =
            60 /| (setting *| dur)

        applyControls : Music1 -> Music1
        applyControls m =
            case m of
                Modify (Tempo r) m ->
                    scaleDurations r <| applyControls m

                Modify (Transpose k) m ->
                    shiftPitches1 k <| applyControls m

                Seq m1 m2 ->
                    Seq (applyControls m1) (applyControls m2)

                Par m1 m2 ->
                    Par (applyControls m1) (applyControls m2)

                _ ->
                    m
    in
        musicToMEvents defCon (applyControls m)


musicToMEvents : MContext -> Music1 -> ( Performance, DurT )
musicToMEvents c m =
    let
        t =
            .mcTime c

        dt =
            .mcDur c
    in
        case m of
            Prim (Note d p) ->
                ( [ noteToMEvent c d p ], d |*| dt )

            Prim (Rest d) ->
                ( [], d |*| dt )

            Seq m1 m2 ->
                let
                    ( evs1, d1 ) =
                        musicToMEvents c m1

                    ( evs2, d2 ) =
                        musicToMEvents { c | mcTime = t |+| d1 } m2
                in
                    ( evs1 ++ evs2, d1 |+| d2 )

            Par m1 m2 ->
                let
                    ( evs1, d1 ) =
                        musicToMEvents c m1

                    ( evs2, d2 ) =
                        musicToMEvents { c | mcTime = t |+| d1 } m2
                in
                    ( merge evs1 evs2, Ratio.max d1 d2 )

            Modify (Instrument i) m ->
                musicToMEvents { c | mcInst = i } m

            Modify (Phrase pas) m ->
                phraseToMEvents c pas m

            Modify x m ->
                musicToMEvents c m



-- Transpose a


noteToMEvent : MContext -> Dur -> ( Pitch, List NoteAttribute ) -> MEvent
noteToMEvent c d ( p, nas ) =
    let
        e0 =
            { eTime = c.mcTime, eInst = c.mcInst, ePitch = absPitch p, eDur = (d |*| c.mcDur), eVol = c.mcVol, eParams = [] }

        nasFun : NoteAttribute -> MEvent -> MEvent
        nasFun na ev =
            case na of
                Volume v ->
                    { ev | eVol = v }

                Params pms ->
                    { ev | eParams = pms }

                _ ->
                    ev
    in
        List.foldr nasFun e0 nas


{-| This function took almost a whole day to port from Haskell to Elm
    There may well be errors....
-}
phraseToMEvents : MContext -> List PhraseAttribute -> Music1 -> ( Performance, DurT )
phraseToMEvents c pAtts m =
    let
        t =
            .mcTime c

        i =
            .mcInst c

        dt =
            .mcDur c
    in
        case pAtts of
            pa :: pas ->
                let
                    ( pf, dur ) =
                        -- as pfd!!
                        phraseToMEvents c pas m

                    loud x =
                        phraseToMEvents c (Dyn (Loudness (fromInt x)) :: pas) m

                    stretch x =
                        let
                            t0 =
                                .eTime
                                    (List.head pf
                                        |> withDefault nullEvent
                                    )

                            r =
                                x |/| dur

                            -- e = {eTime = t, eDur = d}
                            upd : MEvent -> MEvent
                            upd e =
                                let
                                    dt =
                                        t |-| t0

                                    t1 =
                                        (1 +| dt |*| r) |*| dt |+| t0

                                    d =
                                        e.eDur

                                    d1 =
                                        (1 +| (2 *| dt |+| d) |*| r) |*| d
                                in
                                    { e | eTime = t1, eDur = d1 }
                        in
                            ( List.map upd pf, (1 +| x) |*| dur )

                    inflate x =
                        let
                            t0 =
                                .eTime
                                    (List.head pf
                                        |> withDefault nullEvent
                                    )

                            r =
                                x |/| dur

                            -- e   = {eTime = t, eVol = v}
                            upd : MEvent -> MEvent
                            upd e =
                                let
                                    t =
                                        e.eTime

                                    v =
                                        e.eVol

                                    eVol =
                                        ((1 +| (t |-| t0) |*| r) |* v)
                                            |> Ratio.round
                                in
                                    { e | eVol = eVol }
                        in
                            ( List.map upd pf, dur )
                in
                    case pa of
                        Dyn (Accent x) ->
                            ( List.map (\e -> { e | eVol = Ratio.round (x |* e.eVol) }) pf, dur )

                        Dyn (StdLoudness l) ->
                            case l of
                                PPP ->
                                    loud 40

                                PP ->
                                    loud 50

                                P ->
                                    loud 60

                                MP ->
                                    loud 70

                                SF ->
                                    loud 80

                                MF ->
                                    loud 90

                                NF ->
                                    loud 100

                                FF ->
                                    loud 110

                                FFF ->
                                    loud 120

                        Dyn (Loudness x) ->
                            phraseToMEvents { c | mcVol = Ratio.round x } pas m

                        Dyn (Crescendo x) ->
                            inflate x

                        Dyn (Diminuendo x) ->
                            inflate (Ratio.negate x)

                        Tmp (Ritardando x) ->
                            stretch x

                        Tmp (Accelerando x) ->
                            stretch (Ratio.negate x)

                        Art (Staccato x) ->
                            ( List.map (\e -> { e | eDur = x |*| e.eDur }) pf, dur )

                        Art (Legato x) ->
                            ( List.map (\e -> { e | eDur = x |*| e.eDur }) pf, dur )

                        Art (Slurred x) ->
                            let
                                lastStartTime =
                                    List.foldr (\e t -> Ratio.max (e.eTime) t) (over 0 1) pf

                                setDur e =
                                    if e.eTime |<| lastStartTime then
                                        { e | eDur = x |*| e.eDur }
                                    else
                                        e
                            in
                                ( List.map setDur pf, dur )

                        Art _ ->
                            ( pf, dur )

                        -- not supported
                        Orn _ ->
                            ( pf, dur )

            -- not supported
            _ ->
                musicToMEvents c m
