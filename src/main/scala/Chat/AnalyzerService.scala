package Chat
import Chat.ExprTree.{And, Balance, Command, Identification, Or, Price, Product}
import Data.{AccountService, ProductService, Session}

import scala.concurrent.Future
import scala.util.{Success, Failure, Try}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  def computePrice(t: ExprTree): Double = t match
    case Command(products) => computePrice(products)
    case Price(exp) => computePrice(exp)
    case Product(name, brand, quantity) => quantity * productSvc.getPrice(name, brand)
    case And(leftExp, rightExp) => computePrice(leftExp) + computePrice(rightExp)
    case Or(leftExp, rightExp) => Math.min(computePrice(leftExp), computePrice(rightExp))
    case _ => 0
  end computePrice


  def prepareCommand(t: ExprTree) : Future[ExprTree] =
    t match

      case Product(name, brand, quantity) =>
        productSvc.prepare(name, brand).flatMap(_ => Future.successful(Product(name, brand, quantity)))

      case And(lExp, rExp) =>

        val listOfFuturesOfExprtree = List(prepareCommand(lExp), prepareCommand(rExp)) // List[Future[ExprTree]]
        val listOfFutureTrysOfExprtree = listOfFuturesOfExprtree.map(futureToFutureTry) // List[Future[Try[ExprTree]]]

        val futureListOfTrys = Future.sequence(listOfFutureTrysOfExprtree) // Future[List[Try[ExprTree]]]
        val futureListOfSuccesses = futureListOfTrys.map(_.filter(_.isSuccess))

        futureListOfSuccesses.flatMap(listOfTries => {
          listOfTries.size match {
            case 0 => Future.failed(null)
            case 1 => Future.successful(listOfTries.head.get)
            case 2 => Future.successful(And(listOfTries.head.get, listOfTries.last.get))
          }
        })

      case Or(lExp, rExp) => if computePrice(lExp) <= computePrice(rExp) then prepareCommand(lExp) else prepareCommand(rExp)
  end prepareCommand

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): (String, Option[Future[String]]) =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => (String, Option[Future[String]]) = reply(session)
    val user = session.getCurrentUser

    t match
      // Example cases
      case Thirsty() => ("Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !", None)
      case Hungry() => ("Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !", None)

      case Identification(pseudo) =>
        session.setCurrentUser(pseudo)
        if !accountSvc.isAccountExisting(pseudo) then accountSvc.addAccount(pseudo, 30)
        (s"Bonjour $pseudo !", None)

      case Command(products) =>
        if user.isDefined && accountSvc.isAccountExisting(user.get) then
          val price = computePrice(products)
          if price > accountSvc.getAccountBalance(user.get) then
            ("Le montant actuel de votre solde est insuffisant", None)
          else
            val preparationAnswer = prepareCommand(products).map(t => {
              val realPrice = computePrice(t)
              accountSvc.purchase(user.get, realPrice)
              if realPrice == price then
                s"La commande de ${inner(products)} est prête. Cela coute ${realPrice}"
              else
                s"La commande de ${inner(products)} est partiellement prête. Voici ${inner(t)}. Cela coute ${realPrice}."
            }).recover(_ => s"La commande de ${inner(products)} ne peut pas être délivrée")
            //accountSvc.purchase(user.get, price)
            (s"Votre commande est en cours de préparation : ${inner(products)._1}", Some(preparationAnswer)) // TODO : add a future here
            //s"Voici donc ${inner(products)} ! Cela coûte $price et votre nouveau solde est de ${accountSvc.getAccountBalance(user.get)}"
        else (s"Veuillez d'abord vous identifier", None)

      case Balance() =>
        if user.isDefined && accountSvc.isAccountExisting(user.get) then
          (s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user.get)}", None)
        else (s"Veuillez d'abord vous identifier", None)

      case Price(products) => (s"Cela coûte CHF ${computePrice(products)}.", None)

      case Product(name, brand, quantity) => (s"$quantity $name ${brand.getOrElse("")}", None)

      case Or(lExp, rExp) => (if computePrice(lExp) <= computePrice(rExp) then s"${inner(lExp)}" else s"${inner(rExp)}", None)
      case And(lExp, rExp) => (s"${inner(lExp)} et ${inner(rExp)}", None)
  end reply

  private def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover { case x => Failure(x)}
  end futureToFutureTry

end AnalyzerService
