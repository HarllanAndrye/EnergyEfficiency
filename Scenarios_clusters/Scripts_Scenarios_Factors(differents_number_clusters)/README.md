# Cenários - Fatores

Experimentos realizados para o agrupamento de consumidores residenciais utilizando 4 fatores extraídos da base de dados.

Os fatores utilizados e os cenários criados são:

Fatores:
* Características diversas: localização geográfica e valor do consumo;
* Feedback: relacionado as recomendações passadas para o consumidor;
* NIALM: informação dos 9 aparelhos;
* Perfil: curvas de carga sintéticas mapeadas com os usuários do questionário.

Cenários:
1. Características diversas;
2. Características diversas + Feedback (Adaptativo);
3. Características diversas + Feedback + NIALM (Adaptativo);
4. Características diversas + Feedback + Perfil (Adaptativo);
5. Características diversas + Feedback + NIALM + Perfil (Adaptativo);
6. Características diversas + NIALM;
7. Características diversas + Perfil;
8. Características diversas + NIALM + Perfil;
9. Feedback (Adaptativo);
10. Feedback + NIALM (Adaptativo);
11. Feedback + Perfil (Adaptativo);
12. Feedback + NIALM + Perfil (Adaptativo);
13. NIALM;
14. NIALM + Perfil;
15. Perfil.


## O que contém em cada pasta?

### Database

Informações sobre os consumidores: consumo mensal, posse de aparelhos, ações que estão dispostos a fazer para reduzir o consumo de energia elétrica, ...

### Results

Resultados dos experimentos.

### Scripts

* Scripts R:
	* Agrupamento de consumidores utilizando diferentes fatores para a formação dos grupos.
	* Os scripts utilizados para gerar os resultados são os que possuem o nome "cenárioX.R", onde X é um número entre 1 e 15.



