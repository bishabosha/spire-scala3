import spire.syntax.cfor._

def foo: Int = {
  cforRange(-10 to -1) { i =>
    if i < 0 then return i
  }
  return 0
}
