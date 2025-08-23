# Gui4s
Gui4s - это библиотека для создания графических интерфейсов, фокусирующаяся на гибкости, переносимости, стабильности и тестируемости. Вам не потребутся плагины компилятора, рефлексию, мутабельность,
анотации и другие трюки для построения интерфейсов: библиотека на 99% состоит из чистых функций, поэтому код будет работать, как вы ожидаете. 
# Установка
'''
libraryDependencies ++== List(
	"todo" % "todo" % "1.0.0"	
)
'''
# Примеры
## Аватарка
'''scala
def userProfile[Event](picture : Url, userName : String, userStatus : String) : Widget[Event] =
	row(
		imageUrl(url = picture)
			.size(width = 100.px, height = 100.px)
			.clip(Shapes.circle),
		column(
			text(text = userName, style = TextStyle(size = 24.sp)),
			text(text = userStatus, style = TextStyle(size = 12.sp, color = Color.Gray)),
		)
	)
'''
## Пример с коллекциями, ифами и всем-всем-всем
Так как построение интерфейса является чистой функцией, можно использовать любые конструкции языка, коллекции и библиотеки:
'''scala
def images[Event](images : List[Url]) : Widget[Event] =
	column(
		items = images.map(
			url =>
				imageUrl(url)
					.clip(Shape.roundedCorners(5.px))	
		),
		gap = 10.px
	)
'''

'''scala
def loadingUserProfile[Event](maybeUser : Option[User]) : Widget[Event] =
	maybeUser match
		case Some(User(picture, name, status)) =>
			userProfile(picture, name, status)
		case None =>
			loadingAnimation
'''
## Виджет с внутренним состоянием
'''
final case class UserFormState(name : String, email: String, password : String)
def registrationForm[Event](formIsComplete : UserFormState => Event) : Widget[Event] =
	stateful(
		initialState = UserFormState("", "", ""),
		eventHandler = {
			case (state, NameInput(newName)) => state.copy(name = newName).eventResult
			case (state, EmailInput(newEmail)) => state.copy(email = newEmail).eventResult
			case (state, PasswordInput(newPassowrd)) => state.copy(name = newPassword).eventResult
			case (state, submit) => formIsSomplete(state).raiseEvent
		},
		body = {
		case UserFormState(currentName, currentEmail, currentPassowrd) =>
			column(
				textInput(title = "Имя", currentValue = currentName, onChange = NameInput(_)),
				emailInput(title = "Почта", currentValue = currentEmail, onChange = EmailInput(_)),
				passwordInput(title = "Пароль", currentValue = currentPassword, onChange = PasswordInput(_)),
				subminButton(onClick = Submit)
			)
		}
	)
'''
## Большие примеры
TODO Ссылки на репозитории с крупными примерами.
