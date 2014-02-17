"Genera el tablero como una secuencia de filas, cada fila
 contiene booleans indicando si hay pija o no en esa posición."
shared Tablero generaTablero(Integer rows, Integer cols,
		"La lista de coordenadas donde deben faltar pijas"
		[[Integer, Integer]*] missings) {
	"Función interna para generar una fila."
	Row row(Integer r, {Integer*} skips) {
		{[Boolean,Boolean]+} f;
		Boolean last;
		if (r%2==0) {
			f = { for (c in 0..cols-2)
					[!skips.contains(c), false] };
			last = !skips.contains(cols-1);
		} else {
			f = { for (c in 0..cols-2)
					[c==0, !skips.contains(c)] };
			last = true;
		}
		return plancha(f).sequence.withTrailing(last);
	}
	return [ for ( r in 0..rows-1) row(r,
		{ for (e in missings) if (e[0]==r) e[1] }) ];
}

"Imprime el tablero y la posición de la meta."
shared void printBoard(Tablero pegs, Integer goal) {
	for (r in pegs) {
		for (c in r) {
			process.write(c then "x" else ".");
		}
		process.writeLine();
	}
	process.write(" ");
	process.write("  ".repeat(goal));
	print("G");
}
