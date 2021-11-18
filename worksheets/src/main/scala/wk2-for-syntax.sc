case class Contact(name:String, phoneNumbers:List[String])

val nasm = (contacts: List[Contact]) =>
  contacts.flatMap {
    contact => contact.phoneNumbers
      .filter(phoneNumber => phoneNumber.startsWith("+41"))
      .map(phoneNumber => (contact.name, phoneNumber))
  }

val nasm2 = (contacts: List[Contact]) =>
  for
    contact <- contacts
    phoneNumber <- contact.phoneNumbers
    if phoneNumber.startsWith("+41")
  yield (contact.name, phoneNumber)


val cs = List( Contact("Jeff",List("+13035551313", "+4112345")), Contact("Ellie", List("+13035551212")))

nasm(cs)
nasm2(cs)

/*

for (x <- e1 if f; s) yield e2
same as
for (x <- e1.withFilter( x => f); s yield e2


*/