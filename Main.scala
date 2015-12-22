import monocle.Lens
import monocle.macros.{Lenses, GenLens}

//case class Street(name: String)     // ... means it contains other fields
//case class Address(street: Street)
//case class Company(address: Address)
//case class Employee(company: Company)

@Lenses("_") case class Employee(company: Company)
@Lenses("_") case class Company(address: Address)
@Lenses("_") case class Address(street: Street)
@Lenses("_") case class Street(name: String)

object Main extends App {
  def vanillaScala: Unit = {
    println("Vanilla Scala:")
    val employee: Employee = Employee(Company(Address(Street("vanila scala"))))
    val copy = employee.copy(
      company = employee.company.copy(
        address = employee.company.address.copy(
          street = employee.company.address.street.copy(
            name = employee.company.address.street.name.capitalize // luckily capitalize exists
          )
        )
      )
    )
    println(copy)
  }

  def withMonocle: Unit = {
    println("with Monocle:")
    val employee: Employee = Employee(Company(Address(Street("monocle"))))
    val _name = Lens[Street, String](_.name)(str => s => s.copy(name = str))
    val _street = Lens[Address, Street](_.street)(str => e => e.copy(street = str))
    val _address = Lens[Company , Address](_.address)(add => e => e.copy(address = add))
    val _company = Lens[Employee, Company](_.company)(comp => e => e.copy(company = comp))

    val copy = (_company composeLens _address composeLens _street composeLens _name).modify(_.capitalize)(employee)
    println(copy)

    // you can achieve the same result with less characters using symbolic syntax
    val copy2 = (_company ^|-> _address ^|-> _street ^|-> _name).modify(_.capitalize)(employee)
    println(copy2)
  }

  def genLens: Unit = {
    println("genLens:")
    val employee: Employee = Employee(Company(Address(Street("genlens"))))
    val _name = GenLens[Street](_.name)
    val _street = GenLens[Address](_.street)
    val _address = GenLens[Company](_.address)
    val _company = GenLens[Employee](_.company)

    val copy = (_company ^|-> _address ^|-> _street ^|-> _name).modify(_.capitalize)(employee)
    println(copy)
  }

  vanillaScala

  withMonocle

  genLens
}
