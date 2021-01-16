## Что такое данные? 

Это информация, которую мы храним в памяти компьютера и которой оперируют программы.
Такую информацию мы записываем в тексте программы с помощью специальных обозначений.

Например примитивные (скалярные) данные и то, как они обозначаются в программе:

* Числа:  
  * `(-1)`
  * `42`
  * `13.3333`  
  ...

* Символы:  
  * `'c'`
  * `'∀'`  
  ...

* Строки:
  * `"hello"`
  * `""`
  * ```purescript
    "это строка содержит символ \
    \перевода каретки"
    ```

* Логические "правда" или "ложь"
  * `true`
  * `false`

* Просто нечто бесполезное (или универсальное?):
  * `unit`

Такие данные можно просто создать написав их обозначение в тексте программы.
Для их создания ничего больше не требуется.


## Что такое Типы (данных)?

Интуитивно понятно, что данные могут быть похожими или отличаться друг от друга.
Если сгрупирровать данные по некоторым признакам и дать группам имена, то получатся Типы Данных.

Например:

`Int`   
  ➥ группа, к которой относятся все (ну почти все) целые числа (включая отрицательные).

`Number`   
  ➥ группа, к которой относятся все (ну почти все) дробные числа (включая отрицательные)

`Char`  
  ➥ группа, к которой относятся все возможные символы или таблицы Unicode.

`String`  
  ➥ группа, к которой относятся все возможные строки, включая пустую строку, 
  не содержащую ни одного символа.

`Boolean`  
  ➥ к этой группе относятся только два примера (экземпляра) данных: логические концепции "правда" или "ложь"

`Unit`   
  ➥ группа, к которой относится только один абстрактный объект
  под названием `unit`. Этот объект обозначает что угодно и не имеет конкретного смысла.


> Кстати, на языке PureScript фраза _"42 относится к группе Int"_ 
> (или как ещё говорят _"42 имеет тип Int"_) записывается так:
> ```purescript
> 42 :: Int
> ```

## "Простые" типы данных

Называются простыми, т.к. не состоят из других типов, являются примитивными, атомарными.

> Такие типы ещё называют _скалярными_

Вот список простых типов данных, предопределеных авторами языка PureScript.
* `Int`   
* `Number`   
* `Char`  
* `String`  
* `Boolean`  
* `Unit`   

Дополнительно, авторы программ могут придумывать и объявлять свои собственные данные и их типы.

Для того, чтобы дать имя новому типу данных используется ключевое слово `data`.

Пример:
```purescript
data Author = Yuriy 
```

в этом примере `Author` это название типа, а `Yuriy` это единственно возможные данные имеющие этот тип.

(_названия типов всегда начинаются с заглавных латинских букв_)

```purescript
Yuriy :: Author
```

## "Сложные" (алгебраические) типы данных

... сложены из "простых" типов данных. 

Если взять данные простых типов и каким-то образом их скомбинировать (сложить, обьеденить), получим данные сложного типа.

Как можно объеденять данные простых типов в данные сложных типов?  
Если говорить абстрактно, то существует два способа:

* "__И__"  
  Например: 
  - Целое число __И__ строка
  - Символ __И__ Дробное число
* "__ИЛИ__"  
  Например: 
  - Целое число __ИЛИ__ строка
  - Символ __ИЛИ__ Дробное число

### Обьединение данных способом "__И__"

Давайте рассмотрим конкретный пример. 

Для указания точек на координатной плоскости требуется два числа. В нашем примере пусть это будут числа `11` и `42` :

```purescript
11 :: Int -- соответствует значению по оси X
42 :: Int -- соответствует значению по оси Y 
```

> В языке PureScript последовательность символов `--` обозначает начало комментария.   

> Комментарий включает в себя эту последовательность и все символы, находящиеся справа и до самого конца строки (символа перевода каретки.  

> Компиллятор игнорирует комментарии, они не являются частью выполнимой программы и авторы программ используют их для своих заметок.

Мы хотим объеденить эти данные, так чтоб в памяти компьютера они хранились в одной условной ячейке. Мы также хотим дать имя типу, обозначающему обьединение данных таких типов (`Int` "__И__" `Int`)


```purescript
data Point -- Такая запись называется "Обьявление типа Point"
```

и для того, чтобы указать данные каких типов обьединяются (в данном случае способом "__И__") мы дописываем

```purescript
data Point = MakePoint Int Int 
```

`MakePoint` это функция-конструктор, название которой мы придумали вместе с названием типа и которую будем использовать для создания данных типа `Point` содержащих в себе два обьекта данных типа `Int`. Функции-конструкторы мы рассмотрим позже.

Именно перечисление обьединяемых типов через пробел соответствует способу обьединения данных "__И__". Иными словами "__И__" не записывается, а подразумевается.

Тип обьявлен. 

Теперь для создания данных этого типа, нам сперва потребуется создать данные вложенных типов (т.е. просто записать в программе конкретные числа)
через пробел после названия функции-конструктора:

```purescript
MakePoint 11 42
```

у нас получилось выражение (фрагмент программы), создающее данные обьявленного нами сложного типа `Point` и состоящих из данных простых типов `Int` и `Int`.

Благодаря тому, что мы использовали функцию-конструктор `MakePoint` обьявленную вместе с обьявлением типа, компилятор догадается, что всё выражение целиком 
имеет тип `Point`:

```purescript
MakePoint 11 42 :: Point
```

Если мы хотим использовать это выражение в программе более одного раза, 
но не хотим каждый раз повторять его запись полностью, мы можем дать этому выражению 
имя и потом ссылаться на него по имени (т.е. записть имени выражения эквивалентна записи самого выражения). Дадим ему имя `myPoint` (имена выражений начинаются с маленькой латинской буквы):

```purescript
myPoint = MakePoint 11 42
```

Ещё пример:
```purescript
center = MakePoint 0 0 -- центр координатной плоскости
```

Не смотря на то, что компилятор знает тип выражений `myPoint` и `center`
по использованной функции-конструктору, считается хорошим тоном записывать
типы явно, хотя это не обязательно:

```purescript
myPoint :: Point
myPoint = MakePoint 11 42

center :: Point
center = MakePoint 0 0
```

В памяти компьютера выражения имеющие тип `Point` хрянятся как данные всех 
обьединённых типов (`Int` и `Int`) вместе:

<table>
  <thead>
    <th colspan="2">Point</th>
  </thead>
  <tbody>
  <tr>
    <td>
      <table>
        <thead><th>Int</th></thead>
        <tbody><tr><td>11</td></tr></tbody> 
      </table>
    </td>
    <td>
      <table>
        <thead><th>Int</th></thead>
        <tbody><tr><td>42</td></tr></tbody> 
      </table>
    </td>
  </tr>
  </tbody>
</table>

Стоит отметить, что способом "__И__" обьединять можно более одного типа.

Например, в программе:

```purescript
data Person = Int String String Number Point

rating :: Number
rating = 98.02

location :: Point
location = MakePoint 110 32

ivanIvanov :: Person
ivanIvanov = Person 1 "Ivan" "Ivanov" rating location
```

выражение `ivanIvanov` эквивалентно выражению

```purescript
Person 1 "Ivan" "Ivanov" 98.02 (MakePoint 110 32)
```

и в памяти компьютера представлено следующим образом:

<table>
  <thead><th colspan="5">Person</th></thead>
  <tbody>
  <tr>
    <td>
      <table>
        <thead><th>Int</th></thead>
        <tbody><tr><td>100</td></tr></tbody> 
      </table>
    </td>
    <td>
      <table>
        <thead><th>String</th></thead>
        <tbody><tr><td>"Ivan"</td></tr></tbody> 
      </table>
    </td> 
    <td>
      <table>
        <thead><th>String</th></thead>
        <tbody><tr><td>"Ivanov"</td></tr></tbody> 
      </table>
    </td> 
    <td>
      <table>
        <thead><th>Number</th></thead>
        <tbody><tr><td>98.02</td></tr></tbody> 
      </table>
    </td> 
    <td>
      <table>
        <thead><th colspan="2">Point</th></thead>
        <tbody>
        <tr>
          <td>
            <table>
              <thead><th>Int</th></thead>
              <tbody><tr><td>110</td></tr></tbody> 
            </table>
          </td>
          <td>
            <table>
              <thead><th>Int</th></thead>
              <tbody><tr><td>32</td></tr></tbody> 
            </table>
          </td>
        </tr>
        </tbody>
      </table>        
    </td> 
  </tr>
  </tbody>
</table>

### Обьединение данных способом "__ИЛИ__"

Позволяет объединить в один тип данные других типов взаимоисключающим образом.

Рассмотрим на примере.

Допустим, у нас есть такая палитра цветов:

| Индекс     | Название            | Односимвольное обозначение | Цвет    |
|------------|---------------------|----------------------------|---------|
| `1 :: Int` | `"white" :: String` |        `'w' :: Char`       | Белый   |
| `2 :: Int` | `"red" :: String`   |        `'r' :: Char`       | Красный |
| `3 :: Int` | `"green" :: String` |        `'g' :: Char`       | Зелёный |
| `4 :: Int` | `"blue" :: String`  |        `'b' :: Char`       | Синий   |

Любой цвет из этой таблицы можно обозначить 
в программе одним из трёх способов на выбор:

- С помощью уникального индекса.  
  Например данные `3` обозначают только зелёный цвет.  
- С помощью уникального названия.  
  Например строка `"blue"` обозначает синий цвет.  
- С помощью уникального символа.  
  Например символ `'w'` означает белый цвет.

Объявим соответствующий тип `Color`, который может хранить данные обозначающие цвет
одним из доступных способов:

```purescript
data Color = ColorByIndex Int | ColorByName String | ColorByChar Char
```

`Color` - название нового данных.

`Int`, `String`, `Char` - названия объединяемых типов данных.

`|` - символ, обозначающий обьединение способом "__ИЛИ__"

`ColorByIndex` - название функции, 
конструирующей данные типа `Color` из данных типа `Int`:
  ```purescript
  emeraldGreen :: Color
  emeraldGreen = ColorByIndex 3
  ```

`ColorByName` - название функции, 
конструирующей данные типа `Color` из данных типа `String`:
  ```purescript
  deepBlue :: Color
  deepBlue = ColorByName "blue"
  ```

`ColorByChar` - название функции, 
конструирующей данные типа `Color` из данных типа `Char`:
  ```purescript
  snowWhite :: Color
  snowWhite = ColorByChar 'w'
  ```

Такие выражения представлены в памяти так:


<table>
  <thead><th colspan="2">Color</th></thead>
  <tbody>
  <tr>
    <td>ColorByIndex</td>
    <td>3</td>
  </tr>
  </tbody>
</table>  

или

<table>
  <thead><th colspan="2">Color</th></thead>
  <tbody>
  <tr>
    <td>ColorByName</td>
    <td>"blue"</td>
  </tr>
  </tbody>
</table>  

или

<table>
  <thead><th colspan="2">Color</th></thead>
  <tbody>
  <tr>
    <td>ColorByChar</td>
    <td>'w'</td>
  </tr>
  </tbody>
</table>  



## Почему сложные типы ещё называются алгебраическими?

TODO

## Синонимы типов

TODO

## Типы типов