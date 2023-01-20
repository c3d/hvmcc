class Foo {
  constructor(x, y) {
    this.x = x;
    this.y = y;
    this.test = 1 + x + y;
  }
}

class Bar {
  constructor(z) {
    this.z = z
  }
}

var a = Bar(1)

switch (true) {
case a instanceof Bar && b instanceof Foo:
  return (1 + d) * 3
case b instanceof Foo:
  return e
case c instanceof Foo && a instanceof Bar:
  return f
}
