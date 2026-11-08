/-!
# SHA-256 (FIPS 180-4) in pure Lean

Used by `Leanllm.Replay` to key recorded responses by request hash. Not constant-time; do not use
for secrets.
-/

namespace Leanllm.Sha256

private def k : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

private def h0 : Array UInt32 := #[
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

@[inline] private def rotr (x : UInt32) (n : UInt32) : UInt32 :=
  (x >>> n) ||| (x <<< (32 - n))

/-- Message padding: append `0x80`, zeros, then the 64-bit big-endian bit length. -/
private def pad (msg : ByteArray) : ByteArray := Id.run do
  let bitLen : UInt64 := msg.size.toUInt64 * 8
  let mut out := msg.push 0x80
  while out.size % 64 != 56 do
    out := out.push 0
  for i in [0:8] do
    out := out.push (bitLen >>> (8 * (7 - i)).toUInt64).toUInt8
  return out

/-- Process one 64-byte block starting at `off`. -/
private def compress (h : Array UInt32) (b : ByteArray) (off : Nat) : Array UInt32 := Id.run do
  let mut w : Array UInt32 := Array.replicate 64 0
  for t in [0:16] do
    let i := off + 4 * t
    w := w.set! t ((b.get! i).toUInt32 <<< 24 ||| (b.get! (i+1)).toUInt32 <<< 16 |||
                   (b.get! (i+2)).toUInt32 <<< 8 ||| (b.get! (i+3)).toUInt32)
  for t in [16:64] do
    let w15 := w[t-15]!
    let w2  := w[t-2]!
    let s0 := rotr w15 7 ^^^ rotr w15 18 ^^^ (w15 >>> 3)
    let s1 := rotr w2 17 ^^^ rotr w2 19 ^^^ (w2 >>> 10)
    w := w.set! t (w[t-16]! + s0 + w[t-7]! + s1)
  let mut a := h[0]!
  let mut bb := h[1]!
  let mut c := h[2]!
  let mut d := h[3]!
  let mut e := h[4]!
  let mut f := h[5]!
  let mut g := h[6]!
  let mut hh := h[7]!
  for t in [0:64] do
    let s1 := rotr e 6 ^^^ rotr e 11 ^^^ rotr e 25
    let ch := (e &&& f) ^^^ ((~~~ e) &&& g)
    let t1 := hh + s1 + ch + k[t]! + w[t]!
    let s0 := rotr a 2 ^^^ rotr a 13 ^^^ rotr a 22
    let maj := (a &&& bb) ^^^ (a &&& c) ^^^ (bb &&& c)
    let t2 := s0 + maj
    hh := g
    g := f
    f := e
    e := d + t1
    d := c
    c := bb
    bb := a
    a := t1 + t2
  return #[h[0]! + a, h[1]! + bb, h[2]! + c, h[3]! + d,
           h[4]! + e, h[5]! + f, h[6]! + g, h[7]! + hh]

/-- SHA-256 digest of `msg` (32 bytes). -/
def hash (msg : ByteArray) : ByteArray := Id.run do
  let padded := pad msg
  let mut h := h0
  for blk in [0:padded.size / 64] do
    h := compress h padded (64 * blk)
  let mut out := ByteArray.emptyWithCapacity 32
  for word in h do
    out := out.push (word >>> 24).toUInt8
    out := out.push (word >>> 16).toUInt8
    out := out.push (word >>> 8).toUInt8
    out := out.push word.toUInt8
  return out

private def hexDigit (n : UInt8) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n.toNat) else Char.ofNat ('a'.toNat + n.toNat - 10)

/-- Lowercase hex encoding of a byte array. -/
def toHex (bytes : ByteArray) : String := Id.run do
  let mut s := ""
  for b in bytes do
    s := s.push (hexDigit (b >>> 4))
    s := s.push (hexDigit (b &&& 0x0f))
  return s

/-- Lowercase hex SHA-256 of the UTF-8 bytes of `s`. -/
def hexOfString (s : String) : String :=
  toHex (hash s.toUTF8)

end Leanllm.Sha256
