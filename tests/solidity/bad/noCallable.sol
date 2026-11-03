contract NoCallable {
  // Returns one STOP byte as runtime code: the deployment has code, but none
  // that matches this contract's ABI, so Echidna has nothing to call.
  constructor() public { assembly { mstore8(0, 0) return(0, 1) } }
  function f() public {}
  function echidna_f() public returns (bool) { return true; }
}
