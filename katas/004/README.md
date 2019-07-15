# Roman numbers converter

- It is not permitted to subtract an "auxiliary" symbol. (CML, not LM = 950;
  XLV not VL, = 45).
- Can't do two subtractions in a row, thus LIVX is illegal.
- Additions must decrease, as you go from left to right. Thus, each symbol
added must have a value equal or less than the last symbol which was added.
Thus, LIIX is wrong, cause we added L, added I, subtracted I, then try to
add X.

## Implementations

- [TypeScript](./ts)
