# Agrupamento de curvas de carga da base de dados REFIT

Experimentos realizados com três medidas de dissimilaridade (correlação, distância euclidiana e DTW) para encontrar qual medida é ideal para representar a semelhança entre curvas de carga (perfis de consumo de energia elétrica) de residências.

Foi utilizada a base de dados chamada [REFIT] (https://pure.strath.ac.uk/portal/en/datasets/refit-electrical-load-measurements-cleaned(9ab14b0e-19ac-4279-938f-27f643078cec).html) que contém dados de 1 mês de consumo de 20 residências do Reino Unido com observações menores que 1 hora.

## O que contém em cada pasta?

### Database

Curvas de carga de 20 residências da base REFIT.

### Results

Resultados dos experimentos.

### Scripts

* Scripts R:
	* Agrupamento de 20 curvas de carga, variando a medida de dissimilaridade entre as curvas.
	* Agrupamentos com diferentes granularidades: 24 e 144 observações em cada perfil de consumo.
	* Métricas de validação: Silhueta Width e MAE (Mean Absolute Error).



