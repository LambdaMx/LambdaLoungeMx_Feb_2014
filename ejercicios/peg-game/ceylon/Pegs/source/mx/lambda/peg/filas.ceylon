
"Toma un iterable de iterables y devuelve un iterable
 plano con los elementos de los iterables internos."
{T*} plancha<T>({{T*}*} nested) =>
	{ for (a in nested) for (e in a) e };

"Calcula y devuelve la lista de columnas donde puede
 terminar la ficha si se arroja desde cierta columna
 en el tablero indicado."
[Integer*] calculaDestinos(Tablero board)(Integer ini) {
	{Integer*} dests({Integer*} cols, [Row*] rows) {
		function destinos(Integer col, Row row) {
			if (exists z=row[col], !z) {
				return { col };
			}
			function isOpen(Integer col) {
				if (exists c = row[col]) {
					return !c;
				}
				return false;
			}
			return [col-1,col+1].filter(isOpen);
		}
		if (nonempty rows) {
			value d0 = plancha { for (c in cols) destinos(c, rows.first) };
			return dests(d0, rows.rest);
		}
		return cols;
	}
	value d0 = dests({ini*2+1}, board.rest);
	return [for (i in d0) (i-1)/2];
}
