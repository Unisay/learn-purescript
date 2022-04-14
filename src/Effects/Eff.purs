module Effects.Eff where

import Prelude

import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Data.Array as Array
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Record as R
import Type.Proxy (Proxy(..))

type Eff fx = ReaderT (Record fx) Effect

newtype H f = H (f ~> Effect)

send
  ∷ ∀ x s f r r'. IsSymbol s ⇒ Cons s (H f) r r' ⇒ Proxy s → f x → Eff r' x
send s fx = liftEffect =<< asks (R.get s >>> \(H h) → h fx)

runEff ∷ ∀ a. Eff () a → Effect a
runEff eff = runReaderT eff {}

{-----------------------------= Log Effect =-----------------------------------}

data Log a = Log String a

type LOG = H Log

log_ ∷ Proxy "log"
log_ = Proxy

log ∷ ∀ r. String → Eff (log ∷ LOG | r) Unit
log msg = send log_ $ Log msg unit

runNoLog ∷ ∀ a r. Eff (log ∷ LOG | r) a → Eff r a
runNoLog eff =
  ReaderT $ runReaderT eff <<< R.union { log: H \(Log _msg a) → pure a }

logHconsole ∷ LOG
logHconsole = H \(Log msg a) → Console.log msg $> a

runLogConsole ∷ ∀ a r. Eff (log ∷ LOG | r) a → Eff r a
runLogConsole eff = ReaderT $ runReaderT eff <<< R.union { log: logHconsole }

{-----------------------------= Trace Effect =---------------------------------}

data Trace t a = Trace t a

type TRACE t = H (Trace t)

type STRACE = TRACE String

trace_ ∷ Proxy "trace"
trace_ = Proxy

traced ∷ ∀ a r. Show a ⇒ a → Eff (trace ∷ STRACE | r) a
traced a = send trace_ (Trace (show a) a)

trace ∷ ∀ r. String → Eff (trace ∷ STRACE | r) Unit
trace s = send trace_ (Trace s unit)

runTraceConsole ∷ ∀ a r. Eff (trace ∷ STRACE | r) a → Eff r a
runTraceConsole eff = ReaderT $ runReaderT eff <<< R.union
  { trace: H \(Trace msg a) → Console.log msg $> a }

runNoTrace ∷ ∀ a r t. Eff (trace ∷ TRACE t | r) a → Eff r a
runNoTrace eff =
  ReaderT $ runReaderT eff <<< R.union { trace: H \(Trace _msg a) → pure a }

runTraceCollect
  ∷ ∀ a r t. Eff (trace ∷ TRACE t | r) a → Eff r (Tuple a (Array t))
runTraceCollect eff = ReaderT \r → do
  st ← Ref.new []
  let
    r' = R.union
      { trace: H \(Trace msg a) →
          Ref.modify_ (\msgs → Array.snoc msgs msg) st $> a
      }
      r
  Tuple <$> runReaderT eff r' <*> Ref.read st

{-----------------------------= State Effect =---------------------------------}

data State s a = Get (s → a) | Put s a

type STATE s = H (State s)

state_ ∷ Proxy "state"
state_ = Proxy

get ∷ ∀ s r. Eff (state ∷ STATE s | r) s
get = send state_ (Get identity)

put ∷ ∀ s r. s → Eff (state ∷ STATE s | r) Unit
put s = send state_ (Put s unit)

runState
  ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r (Tuple s a)
runState s eff = ReaderT \h → do
  r ← Ref.new s
  a ← runReaderT eff $ R.union
    { state: H case _ of
        Get k → Ref.read r <#> k
        Put w a → Ref.write w r $> a
    }
    h
  s' ← Ref.read r
  pure $ Tuple s' a

evalState ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r a
evalState s eff = snd <$> runState s eff

execState ∷ ∀ a r s. s → Eff (state ∷ STATE s | r) a → Eff r s
execState s eff = fst <$> runState s eff

{--------------------------------= Example =-----------------------------------}

test ∷ Eff (log ∷ LOG, state ∷ STATE Int, trace ∷ STRACE) Int
test = do
  trace "start"
  put =<< traced 42
  log "Saved state"
  s ← get
  r ← traced $ s * 2
  put r
  log "Updated state"
  s' ← get
  log "Retrieved state"
  trace "finish"
  traced s'
