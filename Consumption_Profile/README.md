# Agrupamento de curvas de carga

Experimentos realizados com três medidas de dissimilaridade (correlação, distância euclidiana e DTW) para encontrar qual medida é ideal para representar a semelhança entre curvas de carga (perfis de consumo de energia elétrica) de residências.

## O que contém em cada pasta?

### Database

Curvas de carga de 148 residências (sintéticas) com observações a cada minuto, em um período de 6 meses.

### Results

Resultados dos experimentos.

### Scripts

* Scripts R:
	* Agrupamento de 148 curvas de carga, variando a medida de dissimilaridade entre as curvas.
	* São feitas 30 execuções para cada experimento e calculado o Intervalo de Confiança (IC).

