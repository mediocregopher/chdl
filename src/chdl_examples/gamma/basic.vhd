ENTITY fullAdder IS
    PORT(a : in bit;
    b : in bit;
    cin : in bit;
    s : out bit;
    cout : out bit);
BEGIN
END ENTITY fullAdder;

ARCHITECTURE ARCH OF fullAdder IS
    SIGNAL axorb : bit;
BEGIN
    axorb <= (a xor b);
    s <= (axorb xor cin);
    cout <= ((axorb and cin) or (a and b));
END ARCHITECTURE fullAdder;

ENTITY main IS
    PORT(a : in ARRAY(10#7# DOWNTO 10#0#) OF BIT;
    b : in ARRAY(10#7# DOWNTO 10#0#) OF BIT;
    s : out ARRAY(10#7# DOWNTO 10#0#) OF BIT;
    cout : out BIT);
BEGIN
END ENTITY main;

ARCHITECTURE ARCH OF main IS
    SIGNAL tmp : ARRAY(10#8# DOWNTO 10#0#) OF BIT;
BEGIN
    tmp(0) <= '0';
    cout <= tmp(8);
    G__7999 : ENTITY fullAdder(ARCH) PORT MAP(a => a(0)
    , b => b(0)
    , cin => tmp(0)
    , s => s(0)
    , cout => tmp(1)
    );
    G__8000 : ENTITY fullAdder(ARCH) PORT MAP(a => a(1)
    , b => b(1)
    , cin => tmp(1)
    , s => s(1)
    , cout => tmp(2)
    );
    G__8001 : ENTITY fullAdder(ARCH) PORT MAP(a => a(2)
    , b => b(2)
    , cin => tmp(2)
    , s => s(2)
    , cout => tmp(3)
    );
    G__8002 : ENTITY fullAdder(ARCH) PORT MAP(a => a(3)
    , b => b(3)
    , cin => tmp(3)
    , s => s(3)
    , cout => tmp(4)
    );
    G__8003 : ENTITY fullAdder(ARCH) PORT MAP(a => a(4)
    , b => b(4)
    , cin => tmp(4)
    , s => s(4)
    , cout => tmp(5)
    );
    G__8004 : ENTITY fullAdder(ARCH) PORT MAP(a => a(5)
    , b => b(5)
    , cin => tmp(5)
    , s => s(5)
    , cout => tmp(6)
    );
    G__8005 : ENTITY fullAdder(ARCH) PORT MAP(a => a(6)
    , b => b(6)
    , cin => tmp(6)
    , s => s(6)
    , cout => tmp(7)
    );
    G__8006 : ENTITY fullAdder(ARCH) PORT MAP(a => a(7)
    , b => b(7)
    , cin => tmp(7)
    , s => s(7)
    , cout => tmp(8)
    );
END ARCHITECTURE main;

