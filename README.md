# Huffman

This is a bare minimum Huffman encoding / decoding tool written in Haskell. It has no real purpose other than learning.  

In the future I want to
1. Refactor to use recursion schemes for all the tree utils. Honestly anywhere I use explicit recursion I would like to refactor.
2. Handle the byte stuff mre correctly. Like properly define everything as bytestream operations.
3. Refactor to use a conduit instead of reading the entire file into memory. If compression is generally used on *large* things, it follows that one would not want to load the entire compression target into memory.
4. Add a cli interface to the whole thing. Maybe use opt parse or something idk.
