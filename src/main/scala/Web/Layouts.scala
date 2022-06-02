package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import Data.{MessageService, AccountService, SessionService, Session}


/**
  * Assembles the method used to layout ScalaTags
  */
object Layouts:

  /**
    * Create the equivalent to the <head> element for the HTML pages.
    * @return Returns a head element with the css and js include.
    */
  private def headElem() = head(
    script(src := "/static/js/main.js"),
    tag("link")(href := "/static/css/main.css", rel := "stylesheet")
  )

  /**
    * Create the "header" used in each page.
    * @param linkTitle Displayed name for the top right link.
    * @param linkHref Destination for the top right link.
    * @param session Session, to know if logged or not.
    * @return A nav element with a "Name" and a login/logout link.
    */
  private def navElem(linkTitle: String, linkHref: String, session: Session) = tag("nav")(
    a(
      cls := "nav-brand"
    )(
      "Bot-Tender"
    ),
    div(
      session.getCurrentUser.map(u => "Hello " + u + "! ")
             .getOrElse(""),
      cls := "nav-item"
    )(
      a(href := linkHref)(linkTitle)
    )
  )

  /**
    * Since all the forms have same pattern and the same number of input, their are created here.
    * @param idForm Identifier of the form
    * @param actionForm Action attribute value.
    * @param methodForm Method attribute value.
    * @param jsSubmitFunction Function to use onSubmit.
    * @param label Label to display near the input text.
    * @param errorId Identifier of the error text.
    * @param errorText Error Text to display.
    * @param inputId Identifier of the input text.
    * @return Return a complete form with label, input and button.
    */
  private def singleInputForm(idForm: String,
                              actionForm: Option[String],
                              methodForm:  Option[String],
                              jsSubmitFunction: Option[String],
                              label: String,
                              errorId: String,
                              errorText: Option[String],
                              inputId: String) = form(id := idForm,
                                                 onsubmit := jsSubmitFunction.getOrElse(""),
                                                 action:= actionForm.getOrElse(""),
                                                 method:=methodForm.getOrElse(""))(
    div(id := errorId, cls := "errorMsg", if !errorText.isDefined then display := "none")(errorText.getOrElse("")),
    tag("label")(attr("for") := inputId)(label),
    input(id := inputId, tpe := "text", name:=inputId),
    input(tpe := "submit")
  )

  /**
    * Create a message to be displayed in the message box.
    * @param author Author of the message
    * @param content Message content.
    * @return Return a div with a span containing the author and a spawn containing the content.
    */
  def message(author : String, content : Frag) : Frag =
    div(cls:="msg")(
      tag("span")(cls:="author")(author),
      content
    )

  /**
    * Create the content of a message without the author.
    * @param message Content of the message without the author.
    * @return Return the span of the message.
    */
  def messageContent(message : String) : Frag =
    tag("span")(cls:="msg-content")(message)

  /**
    * Creates a placeholder to display when their is no message in the message box.
    * @param message Message to display
    * @return Return a dic centered with a message.
    */
  def placeholder(message : String) = div(cls := "msg", textAlign.center)(message)

  /**
    * Create the index page.
    * @param session Session to get connected user.
    * @return Return a complete <html> element
    */
  def indexPage()(session: Session) =
    html(
      headElem(),
      body(
        session.getCurrentUser.map(u => navElem("Logout", "/logout", session))
          .getOrElse(navElem("Login", "/login", session)),
        div(
          cls := "content"
        )(
          div(
            id := "boardMessage"
          )(
            placeholder("Please wait! the messages are loading!")
          ),
          singleInputForm(
            "msgForm",
            None,
            None,
            Some("submitMessageForm(); return false;"),
            "Your message:",
            "errorDiv",
            None,
            "messageInput"),
        )
      )
    )

  /**
    * Create the login/registration page.
    * @param errorLogin Error to display if login fails
    * @param errorRegister Error todisplay if register fails.
    * @param session Session to get the current user.
    * @return Return a complete <html> element.
    */
  def loginPage(errorLogin: Option[String], errorRegister: Option[String])(session: Session) =
    html(
      headElem(),
      body(
        navElem("Go to the message board", "/", session),
        div(
          cls := "content"
        )(
          h1("Login"),
          singleInputForm(
            "loginForm",
            Some("/login"),
            Some("post"),
            None,
            "Username:",
            "errorLogin",
            errorLogin,
            "loginInput"),
          h1("Register"),
          singleInputForm(
            "registerForm",
            Some("/register"),
            Some("post"),
            None,
            "Username:",
            "errorRegister",
            errorRegister,
            "registerInput")
        )
      )
    )

  /**
    * Create a success page to confirm actions.
    * @param action Name of the action.
    * @param session Session to get the user.
    * @return Return a complete <html> element.
    */
  def successPage(action: String)(session: Session) =
    html(
      headElem(),
      body(
        navElem("Go to the message board", "/", session),
        div(
          cls := "content"
        )(
          div(cls := "msg", textAlign.center)(s"You ${action} successfully!")
        )
      )
    )

end Layouts
