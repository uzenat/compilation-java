def nil (n) =
    val bk0 = new[1] in
    bk0[0] := 0;
    bk0 end

def cons(x,l) =
    val bk = new[3] in
    bk[0]:=1;
    bk[1]:=x;
    bk[2]:=l;
    bk end

def leaf(a) =
    val bk = new [2] in
    bk[0]:=0;
    bk[1]:=a;
    bk end

def node(g,d) =
    val bk = new[3] in
    bk[0]:=1;
    bk[1]:=g;
    bk[2]:=d;
    bk end

def concat (l1,l2) =
    if l1[0] = 0 then l2
    else cons(l1[1], concat(l1[2],l2))
    end

def tolist(t) =
    if t[0] = 0 then cons(t[1],nil(0))
    else concat(tolist(t[1]), tolist(t[2]))
    end

val xx = concat(tolist(leaf(42)), tolist(node(leaf(23),leaf(24)))) [1]
/* resultat: 42 */