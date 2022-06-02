package Data

import Chat.ExprTree
import Data.MessageService.{MsgContent, Username}
import scalatags.Text.Frag


object MessageService:
    type Username = String
    type MsgContent = Frag

trait MessageService:
    /**
      * Retrieve the latest N added messages
      * @param n The number of message to retrieve
      * @return The content of the messages and their senders
      */
    def getLatestMessages(n: Int): Seq[(Username, MsgContent)]

    /**
      * Add a message to the history
      * @param sender The username of the sender
      * @param msg The content of the message
      * @param mention The name if it exists of the user mentioned at the start of the message with an '@'. For example the message "@Julian Hello" mentions "Julian"
      * @param exprType If the message is to the bot, the type of the query
      * @param replyToId If the message is a reply to another message, the id of the other message. Used for example, when the bot answers to a query
      * @return the unique id of the added message (generated by a method of your choice)
      */
    def add(sender: Username, msg: MsgContent, mention: Option[Username] = None, exprType: Option[ExprTree] = None, replyToId: Option[Long] = None): Long

    /**
      * Deletes all the stored messages
      */
    def deleteHistory(): Unit

class MessageImpl extends MessageService:


    // List of memorized messages.
    private var messages : List[Message] = Nil

    // Next message identifier.
    private var nextUid : Long = 0

    override def add(sender: Username, msg: MsgContent, mention: Option[Username] = None, exprType: Option[ExprTree] = None, replyToId: Option[Long] = None): Long =
        messages = Message(sender, msg, mention, exprType, replyToId) +: messages
        val messageUid = nextUid
        nextUid += 1
        messageUid
    end add

    override def getLatestMessages(n: Int) : Seq[(Username, MsgContent)] =
        messages.take(n).map(m => (m.sender, m.msg))
    end getLatestMessages

    override def deleteHistory(): Unit =
        messages = Nil
    end deleteHistory

/**
  * Message case class
  * @param sender The username of the sender
  * @param msg The content of the message
  * @param mention The name if it exists of the user mentioned at the start of the message with an '@'. For example the message "@Julian Hello" mentions "Julian"
  * @param exprType If the message is to the bot, the type of the query
  * @param replyToId If the message is a reply to another message, the id of the other message. Used for example, when the bot answers to a query
  */
case class Message(sender: Username, msg: MsgContent, mention: Option[Username], exprType: Option[ExprTree], replyToId: Option[Long])