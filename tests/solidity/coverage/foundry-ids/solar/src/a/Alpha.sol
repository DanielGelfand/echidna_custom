// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
import "../z/Zlib.sol";
contract Alpha { uint public x; function bump() external { x = Zlib.inc(x); } }
