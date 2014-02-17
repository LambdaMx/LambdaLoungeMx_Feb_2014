shared alias Row => [Boolean+];
shared alias Tablero => [Row+];

"Run the module `mx.lambda.peg`."
by("chochos")
shared void run() {
	value argc = process.arguments.size;
    if (argc < 3) {
        print("Debes indicar al menos el número de filas y columnas, y la columna a donde hay que llegar.");
    } else if (argc % 2 != 1) {
        print("El número de argumentos debe ser impar: filas columnas meta [fila columna]...");
    } else {
        value rows = parseInteger(process.arguments[0] else "0") else 0;
        value cols = parseInteger(process.arguments[1] else "0") else 0;
        value goal = parseInteger(process.arguments[2] else "0") else 0;
        value missings = argc < 4 then {} else [
	        for (i in (3..argc-1).by(2)) [
        		parseInteger(process.arguments[i] else "0") else 0,
        		parseInteger(process.arguments[i+1] else "0") else 0
    		]
        ];
		"El tablero debe tener al menos dos filas"
		assert(rows>1);
		"El tablero debe tener al menos dos columnas"
		assert(cols>1);
		"La meta debe estar dentro del rango de columnas"
		assert(goal>=0 && goal<cols);
		print("Generando tablero de ``rows`` y ``cols`` columnas");
        value tablero = generaTablero(rows, cols, missings);
        printBoard(tablero, goal);
        "Los posibles destinos de cada columna inicial"
        value outcomes = (0..cols-2).map(calculaDestinos(tablero));
        print("Los posibles destinos de cada origen: ``outcomes``");
        "Recorremos los posibles destinos de cada columna inicial,
         buscando la meta entre ellos. Si la meta está, obtenemos
         el tamaño del conjunto, de lo contrario un número muy grande
         para descartarlo fácilmente."
        value probs = [for (s in outcomes)
        				goal in s then s.count((Integer x) => x==goal)*1.0/s.size*1.0 else 0.0];
        print("Las probabilidades de cada origen de caer en la meta: ``probs``");
        "Buscamos el origen con menos destinos posibles"
        value mp = max(probs);
        "La posición óptima es la que tiene menos destinos posibles,
         pero que contenga a la meta entre ellos."
        value optima = probs.indexes(mp.equals).first;
        assert(exists optima);
        print("La posición óptima es ``optima``");
    }
}