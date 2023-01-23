class Null {}

let nil = new Null();

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

let free = new Free();

class Used {}

let used = new Used();

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
  let r0 = used;
  let r1 = u60_swap(n & 1, r0, free);
  let r2 = u60_swap(n & 2, r1, free);
  let r3 = u60_swap(n & 4, r2, free);
  let r4 = u60_swap(n & 8, r3, free);
  let r5 = u60_swap(n & 16, r4, free);
  let r6 = u60_swap(n & 32, r5, free);
  let r7 = u60_swap(n & 64, r6, free);
  let r8 = u60_swap(n & 128, r7, free);
  let r9 = u60_swap(n & 256, r8, free);
  let rA = u60_swap(n & 512, r9, free);
  let rB = u60_swap(n & 1024, rA, free);
  let rC = u60_swap(n & 2048, rB, free);
  let rD = u60_swap(n & 4096, rC, free);
  let rE = u60_swap(n & 8192, rD, free);
  let rF = u60_swap(n & 16384, rE, free);
  let rG = u60_swap(n & 32768, rF, free);
  let rH = u60_swap(n & 65536, rG, free);
  let rI = u60_swap(n & 131072, rH, free);
  let rJ = u60_swap(n & 262144, rI, free);
  let rK = u60_swap(n & 524288, rJ, free);
  let rL = u60_swap(n & 1048576, rK, free);
  let rM = u60_swap(n & 2097152, rL, free);
  let rN = u60_swap(n & 4194304, rM, free);
  let rO = u60_swap(n & 8388608, rN, free);
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
    let x_ = x * 2;
    let y_ = x_ + 1;
    let n_ = n - 1;
    return new Node(gen_go(n_, x_), gen_go(n_, y_));
  }
};

function gen(n) {
  return gen_go(n, 0);
};

console.log(sum(sort(reverse(gen(21)))));
