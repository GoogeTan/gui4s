# Gui4s
[English](README.md) | [Русский](README.ru.md)

Gui4s — это библиотека для создания графических интерфейсов, фокусирующаяся на выразительности и поддерживаемости.

## Функциональность
* Простой и расширяемы API
* Управление состоянием в стиле Elm/MVI.
* Поддержка windows, macOS, linux, android(в активной разработке)
* Построение интерфейса чистыми функциями(ни макросов, ни плагинов на компилятор!)
* Cats effect для асинхронных задач
* Анимации

## Установка
### sbt
```
libraryDependencies ++= List(
	"todo" % "todo" % "1.0.0"
)
```

### Mill
```
mvnDeps:
- todo::todo:1.0.0
```

## Примеры
### Аватарка
```scala 3
def userProfile[Event](picture : Url, userName : String, userStatus : String) : Widget[Event] =
  row(	    
    imageUrl(url = picture)
      .maxSize(width = 100, height = 100)
      .clip(Shape.circle),
    column(
      text(text = userName, style = TextStyle(size = 24)),
      text(text = userStatus, style = TextStyle(size = 12, color = Color.Gray)),
    )
  )
```
### Условия, циклы и всё-всё-всё
Так как построение интерфейса является чистой функцией, можно использовать любые конструкции языка, коллекции и библиотеки:
```scala 3
def images[Event](images : List[Url]) : Widget[Event] =	  
  column(	    
    items = images.map(
      url =>
        imageUrl(url)
          .clip(Shape.roundedCorners(5))	
    ),
    gap = 10
  )
```

```scala 3
def loadingUserProfile[Event](maybeUser : Option[User]) : Widget[Event] =
  maybeUser match	
    case Some(User(picture, name, status)) =>
      userProfile(picture, name, status)
    case None =>
      loadingAnimation
```
### Управление состоянием
```scala 3
statefulWidget[AppIO][Int, ApplicationRequest, Unit](
  name = "state_name",
  initialState = 0,
  eventHandler = (state, _, _) => (state + 1).pure,
  body = state =>
    onClick(())(
      textWidget(
        "Count: " + state.toString,
        SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
      )
    )
)
```
### Большие примеры
#### [Десктоп](https://github.com/GoogeTan/gui4s/tree/master/desktop/example/)
Для запуска используйте 
```bash 
./mill desktop.example.run
```
#### [Андроид](https://github.com/GoogeTan/gui4s/tree/master/android/example)
```bash 
./mill android.example.startAndroidEmulator
./mill android.example.androidInstall    
```

## Ссылки на документацию

## Контрибьюция