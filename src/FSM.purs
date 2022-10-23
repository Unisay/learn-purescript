module FSM where

import Prelude

import Control.Monad.State (State)
import Custom.Prelude (Maybe, (<<$>>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Identity (Identity)

newtype FSMT i m o = FSMT (i → m (Step i m o))

data Step i m o = Halt | Next o (FSMT i m o)

-- instance MonadTrans (FSMT i) where
--   lift ∷ ∀ m a. Monad m ⇒ m a → FSMT i m a
--   lift m = FSMT \_ → Next m (lift m)

instance Functor m ⇒ Functor (Step i m) where
  map f = case _ of
    Halt → Halt
    Next o fsmt → Next (f o) (map f fsmt)

instance Monad m ⇒ Apply (Step i m) where
  apply fab fa = case fab, fa of
    Halt, _ → Halt
    _, Halt → Halt
    Next ab fsmtab, Next a fsmta → Next (ab a) (apply fsmtab fsmta)

instance Monad m ⇒ Applicative (Step i m) where
  pure a = Next a halt

instance Functor m ⇒ Functor (FSMT i m) where
  map f (FSMT v) = FSMT (map f <<$>> v)

instance Monad m ⇒ Apply (FSMT i m) where
  apply = ap

instance Monad m ⇒ Applicative (FSMT i m) where
  pure = pure >>> pure >>> pure >>> FSMT

instance Monad m ⇒ Bind (FSMT i m) where
  bind ∷ ∀ a b. FSMT i m a → (a → FSMT i m b) → FSMT i m b
  bind (FSMT ia) f = FSMT \i → ia i >>= case _ of
    Halt → pure Halt
    Next a h | FSMT ib ← f a →
      ib i <#> case _ of
        Halt → Halt
        Next b g → Next b (?h <> ?g)

instance Monad m ⇒ Monad (FSMT i m)

-- instance MonadState s m ⇒ MonadState s (FSMT i m) where
--   state f = lift (state f)

halt ∷ ∀ i m o. Applicative m ⇒ FSMT i m o
halt = FSMT $ const $ pure Halt

stepFSMT ∷ ∀ i m o. FSMT i m o → i → m (Step i m o)
stepFSMT (FSMT k) = k

type FSM i o = FSMT i Identity o

------------------------------------------------------------------------------
-- ⏪⏩▶⏸⏹

type RecordPlayer = FSMT RPInput (State RPState) RPOutput

data RPInput = Forward | Rewind | Play | Pause | Stop

type RPState =
  { nowPlaying ∷ Maybe Song
  , playlist ∷ Playlist
  }

type RPOutput = { nowPlaying ∷ Maybe Song, isPaused ∷ Boolean }

type Song = { author ∷ String, songName ∷ String }

type Playlist = NonEmptyArray Song
-- recordPlayerPlaying ∷ RecordPlayer
-- recordPlayerPlaying = FSMT case _ of
--   Forward → flip Tuple recordPlayerPlaying $
--     gets \{ nowPlaying, playlist } →
--       { isPaused: false
--       , nowPlaying: do
--           s ← nowPlaying
--           idx ← NEA.elemIndex s playlist
--           if idx == NEA.length playlist - 1 then Just (NEA.head playlist)
--           else NEA.index playlist (idx + 1)
--       }

--   Rewind → flip Tuple recordPlayerPlaying $
--     gets \{ nowPlaying, playlist } →
--       { isPaused: false
--       , nowPlaying: do
--           s ← nowPlaying
--           idx ← NEA.elemIndex s playlist
--           if idx == 0 then Just (NEA.last playlist)
--           else NEA.index playlist (idx - 1)
--       }

--   Play → Tuple
--     (gets \{ nowPlaying } → { nowPlaying: nowPlaying, isPaused: false })
--     recordPlayerPlaying

--   Pause → flip Tuple recordPlayerPaused $
--     gets \{ nowPlaying } → { nowPlaying: nowPlaying, isPaused: true }

--   Stop → flip Tuple recordPlayerPaused $ pure
--     { nowPlaying: Nothing, isPaused: true }

-- recordPlayerPaused ∷ RecordPlayer
-- recordPlayerPaused = FSMT case _ of
--   Forward → flip Tuple recordPlayerPlaying $
--     gets \{ nowPlaying, playlist } →
--       { isPaused: true
--       , nowPlaying: do
--           s ← nowPlaying
--           idx ← NEA.elemIndex s playlist
--           if idx == NEA.length playlist - 1 then Just (NEA.head playlist)
--           else NEA.index playlist (idx + 1)
--       }

--   Rewind → flip Tuple recordPlayerPlaying $
--     gets \{ nowPlaying, playlist } →
--       { isPaused: true
--       , nowPlaying: do
--           s ← nowPlaying
--           idx ← NEA.elemIndex s playlist
--           if idx == 0 then Just (NEA.last playlist)
--           else NEA.index playlist (idx - 1)
--       }

--   Play → flip Tuple recordPlayerPlaying $
--     gets \{ nowPlaying, playlist } → case nowPlaying of
--       Nothing → { nowPlaying: Just $ NEA.head playlist, isPaused: false }
--       song → { nowPlaying: song, isPaused: false }

--   Pause → Tuple
--     (gets \{ nowPlaying } → { nowPlaying: nowPlaying, isPaused: true })
--     recordPlayerPaused

--   Stop → Tuple (pure { nowPlaying: Nothing, isPaused: true }) recordPlayerPaused

{-

  i0 -> h0 -> a0    ->   i0 -> g0 -> b0
        |                      |
  i1 -> h1 -> a1    ->   i1 -> g1 -> b1
        |                      |

        ?      ?               ?     ?







  i2 -> ?h2 -> a2    ->   i2 -> ?g2 -> b2
        |                        
  i3 -> Halt 


-}