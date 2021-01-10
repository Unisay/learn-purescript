module Effects where

import Prelude
import Effect (Effect)
import Effects.Runtime as Runtime

{-

Глоссарий:

Чистые (Pure) выражения (вычисления) — 
  результат функции зависит только от её аргументов и не способен влиять на 
  окружающий мир: получать и отправлять данные за пределы функции (ввод/вывод, IO)
  Такие функции ещё называются ссылочно прозрачными (referrentially transparent)

  Примеры чистых функций и выражений:
  \a b -> a + b
  \_ -> 42
  "Здравствуй, Мир!" :: String
  foldr Cons Nil xs


Эффекты ввода вывода:
  Типы данных и их значения, описывающие действия над "окружающей средой", например:
  — Считать строку с терминала
  — Напечатать текст на терминале
  — Отправить запрос по сети
  — Удалить файл

Чужие (Foreign) функции:
  Тело (имплементация) функции определено за пределами программы Purescript.
  Программа Purescript может такие функции использовать.

-}
data Eff
  = PrintLine String
  | AskForInput String (String -> Eff)
  | ClearScreen
  | Compose Eff Eff

runEff :: Eff -> Unit
runEff = case _ of
  PrintLine s -> Runtime.printLine s
  AskForInput question continueWith ->
    runEff
      $ continueWith
      $ Runtime.askForInput question
  ClearScreen -> Runtime.clearScreen
  Compose effect1 effect2 ->
    let
      _ = runEff effect1

      _ = runEff effect2
    in
      unit

effectHello :: Eff
effectHello = PrintLine "Hello, my name is Robo."

effectAskName :: (String -> Eff) -> Eff
effectAskName continueWith = AskForInput "What is your name? " continueWith

effectClear :: Eff
effectClear = ClearScreen

program :: Eff
program =
  Compose effectClear
    ( Compose effectHello
        ( effectAskName \name ->
            PrintLine ("Your name is: " <> name)
        )
    )

-- | Program entry point, should be run with 
-- | ```shell
-- | spago run -m Effects
-- | ```
main :: Effect Unit
main = pure $ runEff program
