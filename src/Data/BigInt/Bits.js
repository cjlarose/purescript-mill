// module Data.BigInt.Bits
"use strict";

exports.and = function(a) {
  return function(b) {
    return a.and(b);
  };
};

exports.or = function(a) {
  return function(b) {
    return a.or(b);
  };
};

exports.xor = function(a) {
  return function(b) {
    return a.xor(b);
  };
};

exports.not = function(a) {
  return a.not();
};

exports.shiftLeft = function(a) {
  return function(n) {
    return a.shiftLeft(n);
  };
};

exports.shiftRight = function(a) {
  return function(n) {
    return a.shiftRight(n);
  };
};
