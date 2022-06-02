package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // States
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree

  // Action - Identification, Command, Balance, Price
  case class Identification(pseudo: String) extends ExprTree
  case class Command(products: ExprTree) extends ExprTree
  case class Balance() extends ExprTree
  case class Price(products: ExprTree) extends ExprTree

  // Product - Beer and croissant
  case class Product(name: String, brand: Option[String], quantity: Int) extends ExprTree

  // Operators - exprTree are Product possibly combined with other Operators.
  case class And(leftExp: ExprTree, rightExp: ExprTree) extends ExprTree
  case class Or(leftExp: ExprTree, rightExp: ExprTree) extends ExprTree
end ExprTree