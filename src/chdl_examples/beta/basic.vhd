ENTITY xorer IS
    PORT(inSig0 : in BIT;
    inSig1 : in BIT;
    inSig2 : in BIT;
    inSig3 : in BIT;
    inSig4 : in BIT;
    inSig5 : in BIT;
    inSig6 : in BIT;
    inSig7 : in BIT;
    outSig0 : out BIT;
    outSig1 : out BIT;
    outSig2 : out BIT;
    outSig3 : out BIT);
BEGIN
END ENTITY xorer;


ARCHITECTURE arch OF xorer IS
BEGIN
    outSig0 <= (inSig0 xor inSig1);
    outSig1 <= (inSig2 xor inSig3);
    outSig2 <= (inSig4 xor inSig5);
    outSig3 <= (inSig6 xor inSig7);
END ARCHITECTURE arch;
