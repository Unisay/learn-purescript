module Effects.Eff where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT, withReaderT)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Prim.Row (class Cons, class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

data Eff fx a
  = Ef0 (ReaderT (Record fx) Effect a)
  | Ef1 (ReaderT (Record fx) (Eff fx) a)

instance Functor (Eff fx) where
  map f (Ef1 r) = Ef1 (map f r)
  map f (Ef0 r) = Ef0 (map f r)

instance Apply (Eff fx) where
  apply = ap

instance Applicative (Eff fx) where
  pure r = Ef0 (pure r)

instance Bind (Eff fx) where
  bind = bindEff

bindEff ∷ ∀ fx a b. Eff fx a → (a → Eff fx b) → Eff fx b
bindEff = case _ of
  Ef0 m → \f → Ef0 $ ReaderT \r → runReaderT m r >>= \a → case f a of
    Ef0 mb → runReaderT mb r
    me → runEff r me
  Ef1 e → \f → Ef1 $ ReaderT \r → runReaderT e r >>= f

instance Monad (Eff fx)

instance MonadEffect (Eff r) where
  liftEffect = Ef0 <<< liftEffect

newtype H (f ∷ Type → Type) g = H (f ~> g)

infixr 4 type H as ~~>

type HE f = f ~~> Effect

sendHE
  ∷ ∀ x s f rTail r
  . IsSymbol s
  ⇒ Lacks s rTail
  ⇒ Cons s (f ~~> Effect) rTail r
  ⇒ Proxy s
  → f x
  → Eff r x
sendHE s fx = Ef0 $ ReaderT \r → let H h = R.get s r in h fx

send
  ∷ ∀ x s f rTail r
  . IsSymbol s
  ⇒ Lacks s rTail
  ⇒ Cons s (f ~~> Eff rTail) rTail r
  ⇒ Proxy s
  → f x
  → Eff r x
send s fx = Ef0 $ ReaderT \(r ∷ Record r) → do
  let (H h ∷ f ~~> Eff rTail) = R.get s r
  let (rTail ∷ Record rTail) = R.delete s r
  runEff rTail (h fx)

runEff ∷ ∀ fx a. Record fx → Eff fx a → Effect a
runEff r (Ef0 rm) = runReaderT rm r
runEff r (Ef1 re) = runEff r $ runReaderT re r

runEff0 ∷ ∀ a. Eff () a → Effect a
runEff0 = runEff {}

contramapEff ∷ ∀ r1 r2 a. (Record r2 → Record r1) → Eff r1 a → Eff r2 a
contramapEff f (Ef0 m) = Ef0 $ withReaderT f m
contramapEff f (Ef1 e) = Ef1 $ ReaderT \r → contramapEff f (runReaderT e (f r))

{-----------------------------= Log Effect =-----------------------------------}

data Log a = Log String a

type LOG = HE Log

log ∷ ∀ r. Lacks "log" r ⇒ String → Eff (log ∷ LOG | r) Unit
log msg = sendHE (Proxy ∷ Proxy "log") (Log msg unit)

runNoLog ∷ ∀ a r. Eff (log ∷ LOG | r) a → Eff r a
runNoLog = contramapEff $ R.union { log: H \(Log _msg a) → pure a }

hLogConsole ∷ LOG
hLogConsole = H \(Log msg a) → Console.log ("LOG: " <> msg) $> a

runLogConsole ∷ ∀ a r. Eff (log ∷ LOG | r) a → Eff r a
runLogConsole = contramapEff $ R.union { log: hLogConsole }

{-----------------------------= Trace Effect =---------------------------------}

data Trace t a = Trace t a

type TRACE t = Trace t ~~> Eff (log ∷ LOG)

type STRACE = TRACE String

trace_ = Proxy ∷ Proxy "trace"

traced
  ∷ ∀ a r. Lacks "trace" r ⇒ Show a ⇒ a → Eff (trace ∷ STRACE | r) a
traced a = send trace_ (Trace (show a) a)

trace ∷ ∀ r. Lacks "trace" r ⇒ String → Eff (trace ∷ STRACE | r) Unit
trace s = send trace_ (Trace s unit)

hTraceConsole ∷ TRACE String
hTraceConsole = H \(Trace msg a) → log msg $> a

runTraceLog
  ∷ ∀ a r. Eff (trace ∷ STRACE, log ∷ LOG | r) a → Eff (log ∷ LOG | r) a
runTraceLog = contramapEff $ R.union { trace: hTraceConsole }

runNoTrace ∷ ∀ a r t. Eff (trace ∷ TRACE t | r) a → Eff r a
runNoTrace = contramapEff $ R.union { trace: H \(Trace _msg a) → pure a }

-- runTraceCollect
--   ∷ ∀ a r t. Eff (trace ∷ TRACE t | r) a → Eff r (Tuple (Array t) a)
-- runTraceCollect eff = ReaderT \r → do
--   st ← Ref.new []
--   let
--     r' = R.union
--       { trace: H \(Trace msg a) →
--           Ref.modify_ (\msgs → Array.snoc msgs msg) st $> a
--       }
--       r
--   a ← runReaderT eff r'
--   tracess ← Ref.read st
--   pure $ Tuple tracess a

{-----------------------------= State Effect =---------------------------------}

-- data State s a = Get (s → a) | Put s a

-- type STATE s fx = H (State s) (Eff fx)

-- state_ ∷ Proxy "state"
-- state_ = Proxy

-- get ∷ ∀ s r. Eff (state ∷ STATE s | r) s
-- get = send state_ (Get identity)

-- put
--   ∷ ∀ l s r r'. Lacks l r ⇒ Cons l (H (State s) (Eff r)) r r' ⇒ s → Eff r' Unit
-- put s = send state_ (Put s unit)

-- runState
--   ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r (Tuple s a)
-- runState s eff = ReaderT \h → do
--   r ← Ref.new s
--   a ← runReaderT eff $ R.union
--     { state: H case _ of
--         Get k → Ref.read r <#> k
--         Put w a → Ref.write w r $> a
--     }
--     h
--   s' ← Ref.read r
--   pure $ Tuple s' a

-- evalState ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r a
-- evalState s eff = snd <$> runState s eff

-- execState ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r s
-- execState s eff = fst <$> runState s eff

{----------------------------= Random Effect =---------------------------------}

-- data Rnd a = RndInt (Int → a)

-- type RND fx = H Rnd (Eff fx)

-- randomInt ∷ ∀ r. Lacks "rnd" r ⇒ Eff (rnd ∷ RND r | r) Int
-- randomInt = send (Proxy ∷ _ "rnd") (RndInt identity)

-- hRandomInt ∷ H Rnd
-- hRandomInt = H \(RndInt k) → Rnd.randomInt bottom top <#> k

-- runRnd ∷ ∀ a r. Eff (rnd ∷ RND | r) a → Eff r a
-- runRnd = withReaderT $ R.union { rnd: hRandomInt }

-- runPseudoRnd
--   ∷ ∀ a r
--   . Seed
--   → Eff (rnd ∷ RND, state ∷ STATE Seed | r) a
--   → Eff (state ∷ STATE Seed | r) a
-- runPseudoRnd seed eff = do
--   put seed
--   v ← hs
--   withReaderT (R.union { rnd: H \(RndInt k) → pure $ k v }) eff
--   where
--   hs ∷ Eff (state ∷ STATE Seed | r) Int
--   hs = do
--     { newSeed, newVal } ← PR.random <$> get
--     put newSeed
--     pure newVal

{--------------------------------= Example =-----------------------------------}

-- test ∷ Eff (log ∷ LOG, state ∷ STATE Int, trace ∷ STRACE, rnd ∷ RND) Int
-- test = do
--   trace "start"
--   put =<< traced 42
--   log "Saved state"
--   s ← get
--   d ← randomInt
--   r ← traced $ s * d
--   put r
--   log "Updated state"
--   s' ← get
--   log "Retrieved state"
--   trace "finish"
--   traced s'

test ∷ Effect Unit
test = trace "ok" # runTraceLog # runLogConsole # runEff0
