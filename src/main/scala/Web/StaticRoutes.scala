package Web

/**
  * Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes :

    @cask.staticResources("/static/css")
    def staticCssFile() = "./css"

    @cask.staticResources("/static/js")
    def staticJsFile() = "./js"

    initialize()

end StaticRoutes
