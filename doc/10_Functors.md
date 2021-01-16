# Функторы (Functors)

К классу `Functor` относятся любые типы данных второго подрядка (kind `Type -> Type`) 

```purescript
forall f a. f a
```
(примеры: `List`, `Maybe`, `Either String`, `data MyPhantomType a = Empty`, `(->) a`)

для которых можно определить функцию такого типа:

```purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b  
```

Если между любыми двумя типами `a` и `b` существует отношение `a -> b`, 
то значит существует и отношение между типами `f a -> f b`, которое подчинаяется 
следующим законам:

1. Закон идентичности (Identity)
  ```purescript

  identity :: forall a. a -> a
  identity a = a

  map identity = identity
  ```

2. Закон композиции (Composition)

```purescript
map (g <<< f) = map g <<< map f 
```

## Интуиция

Вот несколько конкретных примеров-метафор отражающих идею класса типов-функторов
и дающих интуицию для её понимания:

### Функтор как контейнер

тип `f a` где переменная типа `f` это тип контейнера, на котором написано, 
что внутри него находится значение типа `a` 

Допустим у нас определён такой тип
```purescript
data Container a = Container a
```

тогда в применении к этому типу функция `map` класса `Functor` 
определяется следующим образом:

```purescript
map :: forall a. (a -> b) -> (Container a -> Container b)
map ab (Container a) = 
--     ^^^^^^^^^^^^ разобрать контейнер, 
  let b = ab a
--    ^^^^^^^^ поменять а на b, 
  in Container b
--   ^^^^^^^^^^^ положить b в новый контейнек
```

и её можно использовать:

```purescript
box :: Container Int
box = Container 42

map (_ + 1) box == Container 43
```

|Название переменной типа|f|a|
|--|-|-|
|Название конкретного типа|Container|Int|

В данном примере переменной типа `f` соответствует тип `Container` 
храняший значение типа `Int`. 

### Функтор как вычисление

### Функтор как эффект

