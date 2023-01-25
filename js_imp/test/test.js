class Foo {
  constructor(x, y) {
    this.x = x;
    this.y = y;
    this.test = 1 + x + y;
  }
}

class Bar {
  constructor(z) {
    this.z = z;
  }
}

let a = new Bar(1);

{
  let a = a + 1;
  let b = 2;
  {
    let b = a;
    let a = a + 2;
    console.log(a);
  }
  console.log(a);
}

console.log(a);
