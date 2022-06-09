package Data

import scala.concurrent.duration.*
import scala.concurrent.Future
import Utils.FutureOps.randomSchedule

import java.lang

/**
  * Custom exception class used when an invalid brand is used for a given product.
  *
  * @param msg Message to be displayed.
  */
class InvalidBrandException(msg: String) extends Exception(msg) {}

trait ProductService:
  /**
    * Alias, a brand name is a String
    */
  type BrandName = String

  /**
    * Alias, a product name is a string
    */
  type ProductName = String

  /**
    * Gets the price of a given product.
    *
    * @param product Product name.
    * @param brand   Brand name.
    * @return Returns the price of the product.
    */
  def getPrice(product: ProductName, brand: Option[BrandName]): Double

  /**
    * Gets the default brand of a given product.
    *
    * @param product Product name.
    * @return Returns the default brand name of the product.
    */
  def getDefaultBrand(product: ProductName): BrandName

  def prepare(product: ProductName, brand: Option[BrandName]): Future[Unit]

end ProductService


class ProductImpl extends ProductService :

  /**
    * Default brand and prices map for beers.
    */
  val beer: ProductInformation = ProductInformation("boxer", Map(
    "farmer" -> (1.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2)),
    "boxer" -> (1.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2)),
    "wittekop" -> (2.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2)),
    "punkipa" -> (3.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2)),
    "jackhammer" -> (3.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2)),
    "tenebreuse" -> (4.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 0.2))
  ))

  /**
    * Default brand and prices map for croissants
    */
  val croissant: ProductInformation = ProductInformation("maison", Map(
    "maison" -> (2.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 3)),
    "cailler" -> (2.0, DeliveryInformation(Duration(1, SECONDS), Duration(2, SECONDS), 3))
  ))

  /**
    * For a given product gets the information associated.
    *
    * @param product Product name.
    * @return Returns the default brand and the map of price.
    */
  private def getProductInformations(product: ProductName): ProductInformation =
    product match
      case "croissant" => croissant
      case "biere" => beer
      case _ => throw new IllegalArgumentException("Unknown product")
  end getProductInformations

  def getPrice(product: ProductName, brand: Option[String]): Double =
    getProductInformations(product).getPrice(brand)
  end getPrice

  def getDefaultBrand(product: ProductName): BrandName =
    getProductInformations(product).getDefaultBrand
  end getDefaultBrand

  def prepare(product: ProductName, brand: Option[BrandName]): Future[Unit] =
    val deliveryInfo = getProductInformations(product).getDeliveryInformation(brand)
    randomSchedule(deliveryInfo.mean, deliveryInfo.std, deliveryInfo.successRate)
  end prepare


end ProductImpl

/**
  * Class in charge of storing product information.
  *
  * @param defaultBrand Default brand name of the product.
  * @param informations Default prices and deliveryInformation for each brand of the product.
  */
case class ProductInformation(private val defaultBrand: String, private val informations: Map[String, (Double, DeliveryInformation)]):

  /**
    * Alias, a brand name is a String
    */
  type BrandName = String

  /**
    * Gets the price of the product for the given brand. If the brand is "", default brand is used.
    *
    * @param brand Brand name.
    * @return Returns the price of the product for the given brand.
    * @throws InvalidBrandException when the brand doesn't exist for the product.
    */
  def getPrice(brand: Option[BrandName]): Double =
    if brand.isEmpty then
      informations(getDefaultBrand)._1
    else if !informations.contains(brand.get) then
      throw new InvalidBrandException("Invalid brand name for this product")
    else informations(brand.get)._1
  end getPrice

  /**
    * Gets the default brand of a product.
    *
    * @return Returns the brand name.
    */
  def getDefaultBrand: BrandName = defaultBrand

  def getDeliveryInformation(brand: Option[BrandName]): DeliveryInformation =
    if brand.isEmpty then
      informations(getDefaultBrand)._2
    else if !informations.contains(brand.get) then
      throw new InvalidBrandException("Invalid brand name for this product")
    else informations(brand.get)._2
  end getDeliveryInformation

end ProductInformation


case class DeliveryInformation(mean: Duration, std: Duration, successRate: Double)