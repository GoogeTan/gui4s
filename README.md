# Gui4s
[English](README.md) | [Русский](README.ru.md)
Gui4s is cross-platform declarative GUI library focused on expressiveness and maintainability.

## Features
* Simple and extendable API
* State management in Elm/MVI style.
* Support for windows, macOS, linux, android(в активной разработке)
* Pure functions to build UI(macro-free and compiler plugin-free!)
* Cats effect for asynchronous tasks
* Animations

## Installation
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

## Examples
### User profile
```scala 3
def userProfile[Event](picture : Url, userName : String, userStatus : String) : Widget[Event] =
  row(	    
    imageUrl(url = picture)
      .size(width = 100.px, height = 100.px)
      .clip(Shape.circle),
    column(
      text(text = userName, style = TextStyle(size = 24.sp)),
      text(text = userStatus, style = TextStyle(size = 12.sp, color = Color.Gray)),
    )
  )
```
### Conditions, collections and other stuff
As Ui is built using pure functions, you can use any language means to achieve your goals:
```scala 3
def images[Event](images : List[Url]) : Widget[Event] =	  
  column(	    
    items = images.map(
      url =>
        imageUrl(url)
          .clip(Shape.roundedCorners(5.px))	
    ),
    gap = 10.px
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
### States
State management in Gui4s is Elm inspired:
```scala 3
final case class UserFormState(name : String, email: String, password : String)

def registrationForm[Event](formIsComplete : UserFormState => Event) : Widget[Event] =
  stateful(
    name = "user_form",
    initialState = UserFormState("", "", ""),
    eventHandler = {
      case (state, NameInput(newName)) => state.copy(name = newName).pure
      case (state, EmailInput(newEmail)) => state.copy(email = newEmail).pure
      case (state, PasswordInput(newPassword)) => state.copy(name = newPassword).pure
      case (state, Submit) => formIsComplete(state).raiseEvent
    },
    body = {
      case UserFormState(currentName, currentEmail, currentPassword) =>
        column(
          textInput(title = "Имя", currentValue = currentName, onChange = NameInput(_)),
          emailInput(title = "Почта", currentValue = currentEmail, onChange = EmailInput(_)),
          passwordInput(title = "Пароль", currentValue = currentPassword, onChange = PasswordInput(_)),
          submitButton(onClick = Submit)
        )
    }
  )
```
### Bigger examples
#### [Deskop](https://github.com/GoogeTan/gui4s/tree/master/desktop/example/)
To run the example execute:
```bash
./mill desktop.example.run
```
#### [Desktop](https://github.com/GoogeTan/gui4s/tree/master/android/example)
To run the example execute:
```bash
./mill android.example.startAndroidEmulator
./mill android.example.androidInstall
```

## Documentation

## Contribution