// A sequence can reach covered code through a new jump without covering any
// new instruction: once f(true) ran, f(false) only adds the edge from the
// `if` straight to `y = 2`, which f(true) reached by falling through.
contract EdgeOnly {
  uint x;
  uint y;
  function f(bool p) public { if (p) { x = 1; } y = 2; }
  function g(bool p) public { if (p) { x = 3; } y = 4; }
  function h(bool p) public { if (p) { x = 5; } y = 6; }
  function echidna_true() public returns (bool) { return true; }
}
