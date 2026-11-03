contract EmptyRuntime {
  // Returns no runtime code, so the deployed contract has nothing to call.
  constructor() public { assembly { return(0, 0) } }
  function f() public {}
  function echidna_f() public returns (bool) { return true; }
}
