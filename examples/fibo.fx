def fibo(n) =
    if n < 2 then n else
    fibo(n-1) + fibo(n-2)
    end

val resultat = fibo(15)
/* reponse : 610 */