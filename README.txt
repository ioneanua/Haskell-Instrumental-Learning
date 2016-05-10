Ioneanu Andrei
324 CA
23.04.2016

TEMA 2 PP

HASKELL: Invatare Instrumentala

TASK 1: Generarea cailor  
		Am avut de generat un cale aleatoare infinita pe baza unui generator, functie care truncheaza acea cale, 
		o infinitate de cai infinite - toate esxplicate in comentarii

TASK 2: Estimarea utilitatilor fara diminuarea ratei de invatare
		Cu ajutorul functilor de generare de cai infinite am vut de contruit trei array-uri , unul cu consecintele
		fiecarei stari , celalalt cu valorile initiale alea starilor si al 3-lwa cu utilitatiile provenite dintr-o estimare.
		Am avut de afisat corect o estimare si de actualizat o estimare in urma parcurgerii unei cai si functii care obtin
		fluxuri inifinite de estimarei pe baza unui anumit flux de cai, fucntie pentru vecinul cu cea mai mare vlaore estimata.
		dar si functie ce contruieste o cale inceputa in strea initiala.

TASK 3: Estimarea utilitatilor cu diminuarea ratei de invatare
		Pentru a imbunatatii estiamrile realizate de algoritm trebuie redusa rata de invatare in fucntie de timp.
		De aceea ma creat o functie care reproduce fluxul inifint al ratelor de invatare scalate in fucntie de exemplu
		si am initializat campul de date StateInfo ce contine o valoare de tip float si un nr intreg de vizitari, modificand mai apoi
		fucntiile implementate mai sus pentru a tine cont de aceste aspecte.