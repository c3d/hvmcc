class Null {}

var nil = new Null();

class Leaf {
  constructor(value) {
    this.value = value;
  }
}

class Node {
  constructor(left, right) {
    this.left = left;
    this.right = right;
  }
}

class Free {}

var free = new Free();

class Used {}

var used = new Used();

class Both {
  constructor(left, right) {
    this.left = left;
    this.right = right;
  }
}

function toMap(t) {
  switch (true) {
  case t instanceof Null:
    return free;
  case t instanceof Leaf:
    return radix(t.value);
  case t instanceof Node:
    return merge(toMap(t.left), toMap(t.right));

  }
};

function toArr(x, m) {
  switch (true) {
  case m instanceof Free:
    return nil;
  case m instanceof Used:
    return new Leaf(x);
  case m instanceof Both:
    return new Node(toArr(x * 2 + 0, m.left), toArr(x * 2 + 1, m.right));
  }
};

// sort :: Arr -> Arr
function sort(t) {
  return toArr(0, toMap(t));
};

// merge :: Map -> Map -> Map
function merge(a, b) {

  switch (true) {
  case a instanceof Free:
    return b;
  case b instanceof Free:
    return a;
  case a instanceof Used:
    return b;
  case b instanceof Used:
    return a;
  case a instanceof Both && b instanceof Both:
    return new Both(merge(a.left, b.left), merge(a.right, b.right));
  }
};

// radix :: Word64 -> Map
function radix(n) {
  var r0 = used;
  var r1 = u60_swap(n & 1, r0, free);
  var r2 = u60_swap(n & 2, r1, free);
  var r3 = u60_swap(n & 4, r2, free);
  var r4 = u60_swap(n & 8, r3, free);
  var r5 = u60_swap(n & 16, r4, free);
  var r6 = u60_swap(n & 32, r5, free);
  var r7 = u60_swap(n & 64, r6, free);
  var r8 = u60_swap(n & 128, r7, free);
  var r9 = u60_swap(n & 256, r8, free);
  var rA = u60_swap(n & 512, r9, free);
  var rB = u60_swap(n & 1024, rA, free);
  var rC = u60_swap(n & 2048, rB, free);
  var rD = u60_swap(n & 4096, rC, free);
  var rE = u60_swap(n & 8192, rD, free);
  var rF = u60_swap(n & 16384, rE, free);
  var rG = u60_swap(n & 32768, rF, free);
  var rH = u60_swap(n & 65536, rG, free);
  var rI = u60_swap(n & 131072, rH, free);
  var rJ = u60_swap(n & 262144, rI, free);
  var rK = u60_swap(n & 524288, rJ, free);
  var rL = u60_swap(n & 1048576, rK, free);
  var rM = u60_swap(n & 2097152, rL, free);
  var rN = u60_swap(n & 4194304, rM, free);
  var rO = u60_swap(n & 8388608, rN, free);
  return rO;
};

// u60_swap :: Word64 -> Map -> Map -> Map
function u60_swap(n, a, b) {
  if (n === 0) {
    return new Both(a, b);
  } else {
    return new Both(b, a);
  }
};

// reverse' :: Arr -> Arr
function reverse(t) {
  switch (true) {
  case t instanceof Null:
    return nil;
  case t instanceof Leaf:
    return new Leaf(t.value)
  case t instanceof Node:
    return new Node(reverse(t.right), reverse(t.left));
  }
};

// sum' :: Arr -> Word64
function sum(t) {
  switch (true) {
  case t instanceof Null:
    return 0;
  case t instanceof Leaf:
    return t.value;
  case t instanceof Node:
    return sum(t.left) + sum(t.right);    
  }
};

function gen_go(n, x) {
  if (n === 0) {
    return new Leaf(x);
  } else {
    var x_ = x * 2;
    var y_ = x_ + 1;
    var n_ = n - 1;
    return new Node(gen_go(n_, x_), gen_go(n_, y_));
  }
};

function gen(n) {
  return gen_go(n, 0);
};

console.log(sum(sort(reverse(gen(21)))));
