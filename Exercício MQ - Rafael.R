######################## Exercício MQ - Modelos de Regressão ################
############################# Rafael Both #########################
################################# Exercício 3 #################################


# Este script foi feito com auxílio da ferramenta ClaudeAI Sonnet 5
# e Gemini 3.6 Flash. Fui adicionando algumas explicações e as mantive no script
# para poder retomar algumas explicações e estudar novamente quando necessário.

# Os dados selecionados para a análise são dados ordinais da onda 7 do WVS no Brasil.

# Os dados podem ser baixados em: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

# Foram escolhidas 2 variáveis:

# Variavel dependente (Y): Q250 - Importancia da democracia (1 a 10)
# Variavel independente (X): Q49 - Satisfacao com a vida (1 a 10)

# Ou seja, pretende-se avaliar a partir de um modelo de REGRESSAO LOGISTICA ORDINAL
# se existe uma relação entre a satisfação com a vida e uma avaliação pessoal do 
# respondente a respeito da importância da democracia. Com isso, busca-se verificar se existe
# uma relação pragmática: quanto mais satisfeito com a vida, mais uma pessoa
# tende a apoiar a democracia no Brasil.


# ------------------------------------------------------------
# PASSO 1: Carregar pacotes e banco de dados com as variáveis de interesse.
# ------------------------------------------------------------

# Primeiro, iremos carregar os pacotes sugeridos pela ferramenta de IA.
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "MASS", "car", 
  "DescTools", "generalhoslem", "Hmisc"))

library(readxl) # para ler o arquivo .xlsx
library(dplyr)   # para selecionar/manipular dados
library(tidyr)    # pivot_longer() para reorganizar dados p/ grafico
library(ggplot2)   # para os graficos (histogramas)
library(MASS)       # contem a funcao polr(), usada para regressao ordinal
library(car)         # contem a funcao vif(), usada para multicolinearidade
library(DescTools)    # PseudoR2() - varias medidas de pseudo-R2
library(generalhoslem) # logitgof() - Hosmer-Lemeshow para modelo ORDINAL
library(Hmisc)          # rcorr.cens() - estatistica c (discriminacao)


# Agora, iremos importar todos dados da Onda 7 e criar um objeto só com as variáveis de
# interesse a serem utilizadas: a Q250 e a Q49

dados_completos <- read_excel("LOCALDOARQUIVO")

# Para dar uma olhada nos dados carregados:
View(dados_completos)

# Seleção apenas duas colunas serão utilizadas.
# Os nomes originais no banco sao longos, por isso usamos crase (`) para
# conseguir referenciar nomes de coluna que tem espaco/dois-pontos.
dados <- dados_completos %>%
  dplyr::select(`Q250: Importance of democracy`, `Q49: Satisfaction with your life`)


# Agora: renomear as variaveis para Q250 e Q49
names(dados) <- c("Q250", "Q49")


# Conforme o documento do questionário do WVS, os códigos negativos representam respostas
# inválidas. Assim, transformaremos estas respostas negativas (<0) em NA, pois não devem ser tratadas
# como valores reais da escala.
dados$Q250[dados$Q250 < 0] <- NA
dados$Q49[dados$Q49 < 0] <- NA


# ------------------------------------------------------------
# PASSO 2: Estatística descritiva (média, desvio padrao e histogramas)
# ------------------------------------------------------------

# Primeiro, iremos calcular a média e o desvio padrão.
# O argumento na.rm = TRUE ignora os NAs no calculo)
media_Q250 <- mean(dados$Q250, na.rm = TRUE)
dp_Q250    <- sd(dados$Q250, na.rm = TRUE)

media_Q49  <- mean(dados$Q49, na.rm = TRUE)
dp_Q49     <- sd(dados$Q49, na.rm = TRUE)

cat("Q250 - Media:", media_Q250, " | Desvio padrao:", dp_Q250, "\n")
cat("Q49  - Media:", media_Q49,  " | Desvio padrao:", dp_Q49,  "\n")

# Contagem de valores, incluindo os NAs (para sabermos quantos "sumiram")
table(dados$Q250, useNA = "ifany")
table(dados$Q49,  useNA = "ifany")

# Agora, para facilitar a visualização, serão gerados
# histogramas incluindo a categoria de NA.

# Como hist() normalmente ignora NAs, criamos uma versao "fator" que
# mostra o NA como uma barra a mais no grafico de barras.
ggplot(dados, aes(x = factor(Q250, exclude = NULL))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuicao de Q250 (Importancia da democracia) - incluindo NA",
       x = "Q250", y = "Frequencia")

ggplot(dados, aes(x = factor(Q49, exclude = NULL))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribuicao de Q49 (Satisfacao com a vida) - incluindo NA",
       x = "Q49", y = "Frequencia")


# Sobre os resultados descritivos:
# Nota-se nos histogramas uma distribuição não paramétrica.
# Isso não é um problema na rodagem do modelo de regressão que será utilizado.
# Na Q250, a média é 8,17 e o DP 2,61.
# Na Q49, a média é 7,56 e o DP é 2,32.
# Há, na Q250, 9% de missing data (161 casos); na Q49, apenas 8 casos faltantes. 


# ------------------------------------------------------------
# PASSO 3: Checagem dos pressupostos
# ------------------------------------------------------------

# Serão checados os pressupostos a partir do documento recomendado
# durante o curso: Super Tutorial de Regressão Logística em R, do Prof. Dalson.
# Antes de checar os pressupostos, removemos as linhas com NA em Q250 ou Q49.
# Isso é necessario porque varias funcoes de diagnostico nao aceitam NA.
dados_completo_caso <- dados %>%
  filter(!is.na(Q250), !is.na(Q49))

n_final <- nrow(dados_completo_caso)
cat("Numero de casos validos (sem NA) para a analise:", n_final, "\n")

# O número de casos válidos é 1599.

# Partindo para os pressupostos:
# --- a) Multicolinearidade (VIF) ----------------------------
# O VIF (Variance Inflation Factor) mede se os preditores sao
# redundantes entre si (informacao repetida). Como neste modelo
# so temos um preditor (Q49), nao ha como haver colinearidade entre
# preditores - o VIF so faz sentido com 2 ou mais variaveis independentes.

# Se fosse utilizado, o código seria:
modelo_auxiliar_vif <- lm(as.numeric(Q250) ~ Q49, data = dados_completo_caso)

# --- b) Outliers (Distancia de Cook) -----
# Usamos um modelo linear auxiliar (nao é o modelo final, é só para
# gerar a distancia de Cook) para identificar observacoes que
# influenciam desproporcionalmente o resultado.
cooksd <- cooks.distance(modelo_auxiliar_vif)

plot(cooksd, type = "h", main = "Distancia de Cook por observacao",
     ylab = "Distancia de Cook")
# Regra pratica: valores muito acima de 4/n merecem atencao
abline(h = 4 / n_final, col = "red", lty = 2)

pontos_influentes <- which(cooksd > 4 / n_final)
cat("Numero de possiveis pontos influentes (Cook > 4/n):", length(pontos_influentes), "\n")

# Foram identificados 113 possíveis pontos influentes, comum para uma amostra de 
# dados de opinião pública. Conclui-se que não existem outliers influentes
# capazes de distorcer as estimativas do modelo."


# --- c) Linearidade no logit (termo quadratico) ---------------
# Para variavel dependente ORDINAL, testamos se a relacao com Q49 é
# realmente linear no logit adicionando um termo quadratico (Q49^2)
# ao modelo. Se o termo quadratico NAO for significativo, isso apoia
# a hipotese de linearidade (o modelo simples, sem o termo quadratico,
# é adequado).

dados_completo_caso$Q250_fator <- factor(dados_completo_caso$Q250, ordered = TRUE)

modelo_linearidade <- polr(Q250_fator ~ Q49 + I(Q49^2),
                            data = dados_completo_caso, Hess = TRUE)

# Para obter o p-valor do termo quadratico, calculamos a partir do teste t (Wald)
tabela_coef <- coef(summary(modelo_linearidade))
p_valores <- pnorm(abs(tabela_coef[, "t value"]), lower.tail = FALSE) * 2
tabela_coef <- cbind(tabela_coef, "p_valor" = p_valores)
print(tabela_coef)

# O teste serve para responder à seguinte pergunta:
# A curva explica a realidade significativamente melhor do que a linha reta?
# Se p_valor < 0.05 (Significativo): A curva faz diferença! A linha reta simples não é suficiente,
# indicando que a relação é curvilínea (não-linear).
# Se p_valor > 0.05 (NÃO Significativo): A curva não traz nenhuma melhoria
# relevante em relação à linha reta. Ou seja, a linha reta simples já dá conta do recado perfeitamente.
# De acordo com a linha "I(Q49^2)": o p_valor > 0.05; ou seja, o termo quadratico NÃO é significativo,
# então, a suposicao de linearidade no logit é razoavel.


# --- d) Tamanho da amostra --------------------------------------
# Regra pratica para regressao ordinal: pelo menos
# ~10 a 20 observacoes por parametro estimado.
n_categorias <- length(unique(dados_completo_caso$Q250_fator))
n_parametros <- (n_categorias - 1) + 1  # interceptos + 1 preditor (Q49)
casos_por_parametro <- n_final / n_parametros

cat("Categorias de Q250:", n_categorias, "\n")
cat("Parametros no modelo (interceptos + preditores):", n_parametros, "\n")
cat("Casos por parametro:", round(casos_por_parametro, 1),
    " (recomendado: >= 10-20)\n")

# Foram identificados 159.9 casos por parâmetro, o adequado seria pelo menos 20.



# Interpretacao conjunta dos pressupostos:
# ------------------------------------------------------------
# a) VIF: nao se aplica com um unico preditor -> sem problema de
#    multicolinearidade neste modelo.

# b) Cook: A partir dos pontos acima da linha vermelha, constatou-se que não existem outliers
# influentes capazes de distorcer as estimativas do modelo.

# c) Linearidade no logit: a relacao pode ser tratada como linear no logit.

# d) Tamanho da amostra: amostra considerada adequada para o modelo.

# Assim, os quatro pontos acima estão OK. A partir disso, o modelo ordinal simples
# é adequado e pode ser rodado normalmente.


# ------------------------------------------------------------
# PASSO 4: Modelo final de regressao logistica ordinal
# ------------------------------------------------------------
# Utilizaremos polr() = "Proportional Odds Logistic Regression", funcao do pacote MASS
# usada para regressao logistica ORDINAL. Hess = TRUE pede para a funcao
# calcular a matriz Hessiana, necessaria para obtermos os erros-padrao
# (e, com eles, os p-valores) dos coeficientes.

modelo_final <- polr(Q250_fator ~ Q49, data = dados_completo_caso,
                      Hess = TRUE)

summary(modelo_final)

# Agora, faz-se o cálculo dos p-valores dos coeficientes (o polr nao mostra por padrao)
tabela_final <- coef(summary(modelo_final))
p_final <- pnorm(abs(tabela_final[, "t value"]), lower.tail = FALSE) * 2
tabela_final <- cbind(tabela_final, "p_valor" = p_final)
print(tabela_final)

# Agora, calculam-se as Odds ratios (como apresentado no curso, são mais faceis de
# interpretar do que os coeficientes em log-odds)
odds_ratios <- exp(coef(modelo_final))
print(odds_ratios)

# ------------------------------------------------------------
# PASSO 5: Ajuste do modelo e capacidade preditiva
# ------------------------------------------------------------

# Agora, vamos à avaliação do modelo construído.

#  a) Modelo vs. nulo - por razao de verossimilhanca
# Comparamos o modelo com Q49 contra um modelo "nulo" (só com os
# interceptos, sem nenhum preditor). Se o modelo com Q49 for
# significativamente melhor, isso confirma que Q49 ajuda a explicar Q250.

modelo_nulo <- polr(Q250_fator ~ 1, data = dados_completo_caso, Hess = TRUE)

teste_rv <- anova(modelo_nulo, modelo_final)
print(teste_rv)
# Na coluna "Pr(Chi)" se lê um resultado < 0.05, o que indica que o modelo com Q49 é
# significativamente melhor do que o modelo sem nenhum preditor.


# b) Qualidade do ajuste
# O teste de Hosmer-Lemeshow tradicional é feito para regressao
# logistica BINARIA. Para modelos ORDINAIS, como este, usamos a versao generalizada
# implementada no pacote "generalhoslem" (funcao logitgof, com ord = TRUE),
# que agrupa os casos em "g" grupos (aqui, g = 10) e compara os valores
# observados com os previstos pelo modelo.

probs_previstas_hl <- predict(modelo_final, type = "probs")

teste_hl <- logitgof(dados_completo_caso$Q250_fator, probs_previstas_hl,
                      g = 10, ord = TRUE)
print(teste_hl)

# Aqui a logica é OPOSTA à maioria dos testes: um p-valor ALTO (> 0.05)
# indica que NAO ha evidencia de mau ajuste, ou seja, o modelo se ajusta
# bem aos dados. Um p-valor baixo (< 0.05) sugeriria mau ajuste.

# Neste caso, foi identificado um mau ajuste, com p-value = 1.679e-11.


# c) Poder explicativo - pseudo-R2
# Como a regressao logistica nao tem um R2 "verdadeiro" (como na
# regressao linear), usamos pseudo-R2. O pacote DescTools calcula
# varias versoes de uma vez (McFadden, Nagelkerke, Cox-Snell etc.).
# McFadden costuma ser a mais citada; valores entre 0.2 e 0.4 ja sao
# considerados um bom ajuste nesse tipo de indicador (esta escala nao é
# comparavel diretamente ao R2 da regressao linear).

pseudo_r2 <- PseudoR2(modelo_final, which = "all")
print(pseudo_r2)

# O que se teve foi um valor muito baixo, indicando um poder explicativo modesto.
# Na literatura de estatística e ciências sociais, valores de McFadden entre 0,2 e 0,4 são
# considerados indicativos de um excelente ajuste. O valor de 0,0044 mostra que, embora a relação
# possa ser estatisticamente significante, a Q49 (Satisfação com a vida) sozinha
# não é capaz de prever bem a Q250 (Importância da democracia).


# d) Erro do modelo - (-2 log verossimilhanca)
# É uma medida de erro/desvio do modelo (quanto menor, melhor o ajuste
# aos dados).

menos2LL <- -2 * as.numeric(logLik(modelo_final))
cat("-2 log verossimilhanca do modelo final:", menos2LL, "\n")

# e) Classificacao - acuracia
# Comparamos a categoria de Q250 PREVISTA pelo modelo (a de maior
# probabilidade) com a categoria REALMENTE observada em cada pessoa.
# Como Q250 tem 10 categorias, é normal a acuracia bruta ser
# relativamente baixa: acertar a categoria exata entre 10 opcoes é
# muito mais dificil do que, por exemplo, acertar entre 2 opções.

predito_classe <- predict(modelo_final, type = "class")

matriz_confusao <- table(Observado = dados_completo_caso$Q250_fator,
                          Previsto  = predito_classe)
print(matriz_confusao)

acuracia <- mean(as.character(predito_classe) == as.character(dados_completo_caso$Q250_fator))
cat("Acuracia do modelo:", round(acuracia * 100, 1), "%\n")

# De acordo com o Gemini: devido à forte assimetria na distribuição da variável dependente e ao
# baixo poder explicativo isolado do preditor, o modelo de classificação por máxima probabilidade
# atribui todas as observações à categoria majoritária, não obtendo ganho discriminativo
# em relação a um classificador nulo.


# f) Discriminacao - estatistica c
# A estatistica C (relacionada a area sob a curva ROC) mede a
# capacidade do modelo de ORDENAR corretamente os casos (se uma pessoa
# com Q49 mais alto tende mesmo a ter Q250 mais alto).
# C = 0.5 indica que o modelo nao discrimina melhor do que o acaso;
# C = 1 indica discriminacao perfeita.
# Usamos o preditor linear do modelo (a combinacao dos preditores antes de
# virar probabilidade) comparado com a variavel observada.

preditor_linear <- as.numeric(dados_completo_caso$Q49) * coef(modelo_final)["Q49"]

estat_c <- rcorr.cens(preditor_linear, as.numeric(dados_completo_caso$Q250_fator))
print(estat_c)
cat("Estatistica C (discriminacao):", round(estat_c["C Index"], 3), "\n")

# O modelo obteve um C-index de 0,5584, indicando que, embora a capacidade de ordenar
# corretamente os pares de observações seja estatisticamente superior ao acaso, a capacidade
# preditiva/discriminativa isolada da variável Q49 é modesta.

# ------------------------------------------------------------
# PASSO 6: Visualizacao e comunicação dos resultados
# ------------------------------------------------------------

# a) Traduzir coeficientes em figura (odds ratio com IC 95%)

coefs   <- coef(modelo_final)
erros_padrao <- sqrt(diag(vcov(modelo_final)))[names(coefs)]

tabela_or <- data.frame(
  Variavel = names(coefs),
  OR       = exp(coefs),
  IC_baixo = exp(coefs - 1.96 * erros_padrao),
  IC_alto  = exp(coefs + 1.96 * erros_padrao)
)
print(tabela_or)

ggplot(tabela_or, aes(x = OR, y = Variavel)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = IC_baixo, xmax = IC_alto), height = 0.1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Odds Ratio de Q49 sobre Q250 (com IC 95%)",
       x = "Odds Ratio", y = "") +
  theme_minimal()

# A linha vermelha tracejada em OR = 1 indica "nenhum efeito".
# Como o intervalo de confianca (as barras horizontais) nao cruzam essa linha,
# o efeito de Q49 é estatisticamente significativo.


# b) Curvas ajustadas por categoria de Q250, ao longo de Q49 -----
# Mostra como a probabilidade prevista de cada categoria de Q250 muda
# conforme Q49 aumenta - e a forma mais direta de "ver" o modelo ajustado.

grade_Q49 <- data.frame(
  Q49 = seq(min(dados_completo_caso$Q49, na.rm = TRUE),
            max(dados_completo_caso$Q49, na.rm = TRUE),
            length.out = 100)
)

probs_previstas <- predict(modelo_final, newdata = grade_Q49, type = "probs")
probs_previstas <- cbind(grade_Q49, probs_previstas)

probs_longo <- probs_previstas %>%
  pivot_longer(cols = -Q49, names_to = "Categoria_Q250", values_to = "Probabilidade")

ggplot(probs_longo, aes(x = Q49, y = Probabilidade, color = Categoria_Q250)) +
  geom_line(linewidth = 1) +
  labs(title = "Probabilidade prevista de cada categoria de Q250, conforme Q49",
       x = "Q49 (satisfacao com a vida)",
       y = "Probabilidade prevista",
       color = "Categoria\nde Q250") +
  theme_minimal()


## Distribuicao observada vs. prevista (tipo "histograma combinado")
# Compara, para cada categoria de Q250, a proporcao REALMENTE observada
# na amostra com a proporcao MEDIA prevista pelo modelo. Barras muito
# parecidas entre "Observado" e "Previsto" indicam bom ajuste global.

dist_observada <- prop.table(table(dados_completo_caso$Q250_fator))
dist_prevista  <- colMeans(predict(modelo_final, type = "probs"))

comparacao <- data.frame(
  Categoria = names(dist_observada),
  Observado = as.numeric(dist_observada),
  Previsto  = as.numeric(dist_prevista)
) %>%
  pivot_longer(cols = c(Observado, Previsto), names_to = "Tipo", values_to = "Proporcao")

ggplot(comparacao, aes(x = Categoria, y = Proporcao, fill = Tipo)) +
  geom_col(position = "dodge") +
  labs(title = "Distribuicao observada vs. prevista pelo modelo (Q250)",
       x = "Categoria de Q250", y = "Proporcao", fill = "") +
  theme_minimal()


# ------------------------------------------------------------
# PASSO 7: Sintese - diagnostico, qualidade do ajuste e tamanho da amostra
# ------------------------------------------------------------
# Este passo apenas reune, em um unico lugar, os numeros
# para interpretar e decidir se o modelo final é satisfatorio.

# Rode e leia os comentarios ao lado de cada linha.

cat("\n===== RESUMO DO MODELO FINAL =====\n")
cat("N de casos usados no modelo:", nrow(dados_completo_caso), "\n")
cat("Razao de verossimilhanca (modelo vs. nulo) - p-valor:",
    signif(teste_rv$`Pr(Chi)`[2], 3),
    "-> menor que 0.05 indica que Q49 melhora o modelo\n")
cat("Hosmer-Lemeshow (ordinal) - p-valor:",
    signif(teste_hl$p.value, 3),
    "-> maior que 0.05 indica bom ajuste (sem evidencia de mau ajuste)\n")
cat("Pseudo-R2 (McFadden):", round(pseudo_r2["McFadden"], 3), "\n")
cat("-2 log verossimilhanca:", round(menos2LL, 1), "\n")
cat("Acuracia de classificacao:", round(acuracia * 100, 1), "%\n")
cat("Estatistica C (discriminacao):", round(estat_c["C Index"], 3),
    "-> quanto mais proximo de 1, melhor a discriminacao (0.5 = acaso)\n")
cat("===================================\n")


# Nao existe um unico numero que "decide" se o modelo esta bom; olha-se
# o conjunto: significancia (RV e p-valor de Q49), ajuste (Hosmer-
# Lemeshow), poder explicativo (pseudo-R2) e discriminacao (estat. C).

# Com apenas 1 preditor (Q49), é esperado que o pseudo-R2 e a
# estatistica C nao sejam muito altos - isso NAO invalida o modelo,
# apenas mostra que Q49 sozinho explica uma parte (nao tudo) da
# variacao em Q250.

# O tamanho da amostra ja foi validado anteriormente;
# como o N aqui é grande (milhares de casos), a amostra é mais do que
# suficiente para os parametros estimados neste modelo simples.

# Tendo como referência a significância estatística,
# a hipótese foi confirmada porque existe uma associação positiva e
# significativa entre satisfação com a vida e importância da democracia.
# O valor indicava que, para cada aumento de 1 unidade na variável de
# satisfação com a vida, a chance do respondente avaliar a importância da
# democracia aumenta em aproximadamente 10%. Porém, a relevância
# substantiva dos resultados indica que o efeito é bastante pequeno e o
# poder de explicação do modelo é muito baixo.