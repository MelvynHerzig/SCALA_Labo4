<h1> SCALA bot tender labo 4</h1>

<h2> Auteurs </h2>

* Forestier Quentin
* Herzig Melvyn

<h2> Points d'attention </h2>

<h3> ProductService </h3>

La première modification est dans ProductService. Le but est d'ajouter les informations de préparation à chaque produit.

Pour rappel, dans les laboratoires précédents, nous avions un objet productInformation pour les bières et les croissants. 

```
  val beer: ProductInformation = ProductInformation("boxer", Map(
    "farmer" -> 1.0,
    "boxer" -> 1.0,
     [...]
  ))
```

Cela nous permettait pour chaque type de produits, de connaître sa marque par défaut ainsi que les détails (prix) de chaque marque.

Nous avons étendu cette structure pour y inclure le temps de préparation ainsi que le taux de succès de chaque marque.

```
  val beer: ProductInformation = ProductInformation("boxer", Map(
    "farmer" -> (1.0, DeliveryInformation(Duration(1, SECONDS), Duration(0.5, SECONDS), 0.5)),
    "boxer" -> (1.0, DeliveryInformation(Duration(1.5, SECONDS), Duration(0.5, SECONDS), 0.5)),
    [...]
  ))
```

En conséquence dans productImpl, nous avons ajouté une méthode prepare qui permet de lancer la préparation d'un produit donné.

```
  /**
    * Launch the preparation of a given product.
    * @param product Product to prepare.
    * @param brand Product's brand (or default brand if empty).
    * @return Return a future that will be successful if the preparation went well
    *         or a future failure if the preparation failed.
    */
  def prepare(product: ProductName, brand: Option[BrandName]): Future[Unit] =
    val deliveryInfo = getProductInformations(product).getDeliveryInformation(brand)
    randomSchedule(deliveryInfo.mean, deliveryInfo.std, deliveryInfo.successRate)
  end prepare
```

<h3> AnalyzerService </h3>

La majeure partie de la logique a été effectuée dans ce service.

La première étape a été d'implémenter la possibilité de lancer la préparation du contenu d'une commande.
Pour ce faire, nous avons créé la méthode prepareCommand:

```
  /**
    * For an exprTree (products) of a command, prepare it (them).
    * @param t Command exprTree (product)
    * @return If some products succeeded, return a future successful with a resulting
    *         exprTree. If all the products fail, return a future failure.
    */  
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
```

Cette méthode itère à travers les OR et les AND pour lancer la préparation des produits. Cette méthode à l'avantage de retourner un Future[ExprTree] ce qui nous permet d'utiliser les méthodes "reply" et "computePrice" une fois le futur complété. La difficulté principale a été de traiter les AND, car ils se comportent différemment en fonction du nombre de produits qui ont été préparés avec succès.

Ensuite, nous avons modifié le type de retour de la méthode reply. Avant elle renvoyait un "String", désormais elle retourne un "(String, Option[Future[String]])". La première valeur du tuple est la réponse du bot qui doit être envoyé directement à l'utilisateur. La seconde valeur est une réponse optionnelle délayée pour le client. Elle est optionnelle, car elle est utilisée uniquement pour le traitement d'une commande.

La dernière modification a lieu dans la gestion d'une commande dans reply.

Dans le laboratoire 3, la commande était préparée séquentiellement après les vérifications d'usage (utilisateur authentifié et solde suffisant).

Dans cette version, le processus est sensiblement différent.

```
  [...]
  case Command(products) =>
    if user.isDefined && accountSvc.isAccountExisting(user.get) then
      val price = computePrice(products)
      if price > accountSvc.getAccountBalance(user.get) then
        ("Le montant actuel de votre solde est insuffisant", None)
      else
        val preparationAnswer = prepareCommand(products).map(t => {
          val realPrice = computePrice(t)
          try{
            accountSvc.purchase(user.get, realPrice)
            if realPrice == price then
              s"La commande de ${inner(products)._1} est prête. Cela coute $realPrice"
            else
              s"La commande de ${inner(products)._1} est partiellement prête. Voici ${inner(t)._1}. Cela coute $realPrice."
          }catch{
            case e: IllegalArgumentException => "Le montant actuel de votre solde est insuffisant"
          }

        }).recover(_ => s"La commande de ${inner(products)._1} ne peut pas être délivrée")

        (s"Votre commande est en cours de préparation : ${inner(products)._1}", Some(preparationAnswer))

    else (s"Veuillez d'abord vous identifier", None)

  [...]  
```

La première étape consiste à vérifier que le montant soit apriori suffisant pour traiter la commande (si l'utilisateur est authentifié). Si et seulement si le solde le permet, la préparation commence. Une fois le futur complété, il y a trois cas de figure possibles:

* Le futur est un succès avec l'intégralité de la commande qui a pu être préparé
* Le futur est un succès avec une partie de la commande qui a pu être préparé
* Le futur est un échec, car rien n'a pu être préparé

Dans chacun de ces cas, nous préparons la string qui va être utilisée pour le futur de la seconde valeur du tuple de réponse de reply.
Il y a une spécificité, dans le cas où le futur est un succès (et donc qu'une partie de la commande peut être achetée) nous effectuons l'exécution dans un try catch. En effet, la méthode purchase a été modifiée pour retourner une exception si le montant à débiter est supérieur au solde. Cette modification a été faite, car nous n'avons aucune garantie que le solde n'a pas été modifié entre le moment ou nous vérifions que l'utilisateur a assez de crédit (avant la préparation) et le moment où il est réellement débité (après la préparation). Nous aurions pu nous contenter d'abandonner le "if" préalable, mais cela ne nous semblait pas logique de potentiellement lancer la préparation de commandes trop couteuses.

<h3> MessagesRoutes </h3>

Nous avons modifié notre méthode handleMessage dans le cas où le bot est mentionné. Avant, tout était traité de la même manière mise à part les identifications qui étaient ignorées. Désormais, nous avons différencié le traitement des commandes, car cela nécessite de gérer une réponse différée. 

```
[...]
case Command(products) => // Command received

  // Get the replies
  val replyResult = analyzerSvc.reply(session)(Command(products))

  // Extract the one to answer directly
  val directAnswer = replyResult._1
  val idRequest = botAnswersToUser(message, mention, Some(expr), directAnswer)(session)

  // Extract the one to send when the preparation is over.
  val futureAnswer = replyResult._2
  // For the "completion" of the future we are using a map (for some reasons ?¿?¿) because
  // the instructions of the laboratory tell to not use "onComplete" which would have
  // been appropriated in the current case (or at least we don't see any reason
  // for it to be unappropriated).
  futureAnswer.get.map(products => {
      msgSvc.add("bot", Layouts.messageContent(products), session.getCurrentUser, None, Some(idRequest))

      val response = latestMessagesAsString(20)
      subscribers.foreach(sendMessageToClient(_, response))
  })
[...]
```

Nous récupérons les réponses de la méthode "reply" de l'analyzerService. Nous séparons les deux réponses: la directe et la délayée. Nous envoyons la directe puis lorsque la réponse délayée est prête (futur complété ) nous envoyons le statut final du traitement de la commande.





