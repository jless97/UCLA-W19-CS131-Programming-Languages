fun <T> everyNth(L: List<T>, N: Int) : List<T> {
  if (N < 1 || N > L.size) return emptyList()
  return L.filterIndexed { index, _ -> (index + 1) % N == 0 }
}

fun main(args: Array<String>) {
  val emptyList = listOf<String>()
  val intList = listOf(1, 2, 3, 4, 5, 6)
  val stringList = listOf("a", "b", "c", "d", "e", "f")
  val byteList = listOf(0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF)

  /* Test Empty */
  val rsEmpty1 = everyNth(emptyList, 1)
  if (rsEmpty1.isEmpty()) println("TEST 1 SUCCESS")
  else println("TEST 1 FAILURE")

  /* Test Out-of-bounds N */
  val rsN1 = everyNth(intList, -1)
  if (rsN1.isEmpty()) println("TEST 2 SUCCESS")
  else println("TEST 2 FAILURE")
  val rsN2 = everyNth(intList, 0)
  if (rsN2.isEmpty()) println("TEST 3 SUCCESS")
  else println("TEST 3 FAILURE")
  val rsN3 = everyNth(intList, 7)
  if (rsN3.isEmpty()) println("TEST 4 SUCCESS")
  else println("TEST 4 FAILURE")

  /* Test Int */
  val rsInt = everyNth(intList, 2)
  if (rsInt == listOf(2, 4, 6)) println("TEST 5 SUCCESS")
  else println("TEST 5 FAILURE")

  /* Test String */
  val rsString = everyNth(stringList, 2)
  if (rsString == listOf("b", "d", "f")) println("TEST 6 SUCCESS")
  else println("TEST 6 FAILURE")

  /* Test Byte */
  val rsByte = everyNth(byteList, 2)
  if (rsByte == listOf(0xFB, 0xFD, 0xFF)) println("TEST 7 SUCCESS")
  else println("TEST 7 FAILURE")
}