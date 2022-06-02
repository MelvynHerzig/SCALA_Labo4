package Web

import Chat.ExprTree.{Command, Identification}
import Chat.{AnalyzerService, ExprTree, Parser, TokenizerService}
import Data.{AccountService, MessageService, Session, SessionService}
import cask.WsChannelActor
import io.undertow.websockets.WebSocketConnectionCallback
import castor.Context.Simple.global
import sourcecode.Text.generate

import scala.collection.mutable.*

/**
  * Assembles the routes dealing with the message board:
  * - One route to display the home page
  * - One route to send the new messages as JSON
  * - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(tokenizerSvc: TokenizerService,
                     analyzerSvc: AnalyzerService,
                     msgSvc: MessageService,
                     accountSvc: AccountService,
                     sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    import Decorators.getSession

    // Channels to every clients.
    val subscribers =  ListBuffer[cask.endpoints.WsChannelActor]()

    /**
      * Display the home page (with the message board and the form to send new messages)
      * @param session
      * @return Index page
      */
    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.get("/")
    def index()(session: Session) =
        Layouts.indexPage()(session)
    end index

    /**
      * Clear the message history.
      * @return Redirection to index.
      */
    @cask.get("/clearHistory")
    def clearHistory() =
        msgSvc.deleteHistory()
        cask.Redirect("/")
    end clearHistory

    /**
      *  Process the new messages sent as JSON object to `/send`. The JSON looks
      *  like this: `{ "msg" : "The content of the message" }`.
      *
      *  A JSON object is returned. If an error occurred , it looks like this:
      *  `{ "success" : false, "err" : "An error message that will be displayed" }`.
      *  Otherwise (no error), it looks like this:
      *  `{ "success" : true, "err" : "" }`
      *
      *  The following are treated as error:
      *  - No user is logged in
      *  - The message is empty
      *
      *  If no error occurred, every other user is notified with the last 20 messages
      * @param msg
      * @param session
      * @return
      */
    @getSession(sessionSvc) // This decorator fills the `(session: Session)` part of the `index` method.
    @cask.postJson("/send")
    def sendMessage(msg: String)(session: Session) =

        if msg.isEmpty then
            createResponse(false, Some("A message cannot be empty"))
        else if session.getCurrentUser.isEmpty then
            createResponse(false, Some("You must be logged in to send messages"))
        else
            val error = handleMessage(msg)(session)
            createResponse(error.isEmpty, error)
        end if
    end sendMessage

    /**
      * Process and store the new websocket connection made to `/subscribe`
      * @return
      */
    @cask.websocket("/subscribe")
    def subscribe() : cask.WebsocketResult =
        cask.WsHandler { channel =>

            // Save channel and send the latest messages
            subscribers += channel
            sendMessageToClient(channel, latestMessagesAsString(20))

            // Remove channel on disconnect.
            cask.WsActor {
                case cask.Ws.Close(_, _) =>  subscribers -= channel
            }
        }
    end subscribe

    /**
      * Create the json response
      * @param success The operation was a success
      * @param error Error message to add in the response
      * @return Json of the response
      */
    private def createResponse(success: Boolean, error : Option[String]) = ujson.Obj("success" -> success, "err" -> error.getOrElse(""))

    /**
      * Get the mention in message.
      * @param msg Message to look for mention.
      * @return Return None if no mention else Some with the username.
      */
    private def getMention(msg: String) : Option[String] =
        if msg.charAt(0) == '@' then Some(msg.substring(1, msg.indexOf(" ")))
        else None
    end getMention

    /**
      * Handle a message and send answer.
      * @param message Message to handle.
      * @param session Current session.
      */
    private def handleMessage(message : String)(session : Session) : Option[String] =

        // Get mention
        val mention = getMention(message)

        // If a message for bot
        if mention.isDefined && mention.get == "bot" then

            // Get tokens
            val tokenized = tokenizerSvc.tokenize(message.substring(message.indexOf(" ")))

            // Try to parse
            try {

                val expr = new Parser(tokenized).parsePhrases()

                expr match
                    case Identification(_) => // Do not analyze Identification, user has been identified on login/register.
                        botAnswersToUser(message, mention, Some(expr), "Bonjour")(session)

                    case Command(products) => 
                        

                    case _ => // Other actions.
                        val answer = s"@${session.getCurrentUser.get} ${analyzerSvc.reply(session)(expr)}"
                        botAnswersToUser(message, mention, Some(expr), answer)(session)

            } catch {
                case e: Chat.UnexpectedTokenException => return Some(e.getMessage)
            }
        // Message not for bot.
        else msgSvc.add(session.getCurrentUser.get, Layouts.messageContent(message), mention)

        // Update each other client.
        val response = latestMessagesAsString(20)
        subscribers.foreach(sendMessageToClient(_, response))
        None
    end handleMessage

    /**
      * Compute the bot answer to a message.
      * @param message User message.
      * @param mention Mention (bot).
      * @param expr Expression tree.
      * @param botAnswer Bot response.
      * @param session Current session.
      */
    private def botAnswersToUser(message: String, mention: Option[String], expr: Option[ExprTree], botAnswer: String)(session : Session) =
        val id = msgSvc.add(session.getCurrentUser.get, Layouts.messageContent(message), mention, expr)
        msgSvc.add("bot", Layouts.messageContent(botAnswer), session.getCurrentUser, None, Some(id))
    end botAnswersToUser

    /**
      * Send a message to a client.
      * @param channel Client's channel.
      * @param message Message to send.
      */
    private def sendMessageToClient(channel : WsChannelActor, message : String) =
        channel.send(cask.Ws.Text(message))
    end sendMessageToClient

    /**
      * Get the nth latest message and return a string that represent them.
      * @param number Number of message to retrieve.
      * @return The messages as string.
      */
    private def latestMessagesAsString(number : Int) =
        if msgSvc.getLatestMessages(number).isEmpty then Layouts.placeholder("No messages have been sent yet").toString
        else msgSvc.getLatestMessages(number)
          .reverse
          .map((author, content) => Layouts.message(author, content).toString)
          .reduceLeft(_ + _)

    initialize()
end MessagesRoutes
