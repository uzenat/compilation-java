def pow(x, n) =
    if n = 0 then 1 else x * pow(x, n-1) end
val res = pow(6, 9)
/* resultat : 10077696 */    	 	     