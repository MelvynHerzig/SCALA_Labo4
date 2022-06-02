package Web

import Data.{AccountService, Session, SessionService}

/**
  * Assembles the routes dealing with the users:
  * - One route to display the login form and register form page
  * - One route to process the login form and display the login success page
  * - One route to process the register form and display the register success page
  * - One route to logout and display the logout success page
  *
  * The username of the current session user is stored inside a cookie called `username`.
  */
class UsersRoutes(accountSvc: AccountService,
                  sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:

    import Decorators.getSession

    /**
      *  Display a login form and register form page for the following URL: `/login`.
      * @param session Current session.
      * @return Login page.
      */
    @getSession(sessionSvc)
    @cask.get("/login")
    def loginPage()(session: Session) =
        Layouts.loginPage(None, None)(session)
    end loginPage

    /**
      * Part 3 Step 3b: Process the login information sent by the form with POST to `/login`,
      * set the user in the provided session (if the user exists) and display a successful.
      * @param loginInput Login value
      * @param session Current sessions.
      * @return The login page with an error or a success page.
      */
    @getSession(sessionSvc)
    @cask.postForm("/login")
    def login(loginInput : String)(session: Session) =
    // If input is empty display error
        if loginInput.isEmpty then Layouts.loginPage(Some("Incorrect username"), None)(session)
        // If account exists.
        else if accountSvc.isAccountExisting(loginInput) then
            session.setCurrentUser(loginInput)
            Layouts.successPage("logged in")(session)
        // Otherwise
        else Layouts.loginPage(Some("User not found"), None)(session)
    end login

    /**
      * Process the register information sent by the form with POST to `/register`,
      * create the user, set the user in the provided session and display a successful register page.
      * @param registerInput Registration input.
      * @param session Current session.
      * @return The login page with an error or a success page.
      */
    @getSession(sessionSvc)
    @cask.postForm("/register")
    def register(registerInput : String)(session: Session) =
    // If input is empty display error
        if registerInput.isEmpty then Layouts.loginPage(None, Some("Incorrect username"))(session)
        // If the account does not already exist
        else if !accountSvc.isAccountExisting(registerInput) then
            accountSvc.addAccount(registerInput, 30)
            session.setCurrentUser(registerInput)
            Layouts.successPage("registered")(session)
        // otherwise
        else Layouts.loginPage(None, Some("User already exist"))(session)
    end register

    /**
      * Reset the current session and display a successful logout page.
      * @param session
      * @return
      */
    @getSession(sessionSvc)
    @cask.get("/logout")
    def logout()(session: Session) =
        session.reset()
        Layouts.successPage("logged out")(session)
    end logout

    initialize()
end UsersRoutes
