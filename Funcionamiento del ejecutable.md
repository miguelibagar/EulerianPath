# Funcionamiento del archivo `EulerianPath.exe`

El archivo ejecutable `EulerianPath.exe` contiene el buscador de caminos eulerianos
en un grafo. Su código puede encontrarse en el archivo `EulerianPath.hs` de la carpeta `codigos`.
Para usarlo deben seguirse las instrucciones descritas en la interfaz.

+ En el menú `EXAMPLE` se trabajará con aquellos grafos de la batería de ejemplos del proyecto
  (puede consultarse en la memoria, así como sus representaciones gráficas. Debe escribirse el
  nombre del mismo (gX, siendo X un número entre 1 y 17 o la palabra "Fallo").
+ En el menú `CREATE` el usuario podrá introducir su propio grafo, en forma de lista de pares. 
  Sólo hay que escribir cada arista una vez (no hay que introducir la opuesta).
+ En el menú `PROPS` pueden comprobarse con QuickCheck los dos teoremas fundamentales sobre caminos
  eulerianos. 
