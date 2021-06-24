# PerformanceAnalytics

Como configurar um projeto integrando o Rstudio ao github

# PRIMEIRO PASSO

Abrir uma conta no https://github.com/

# SEGUNDO PASSO

Instalar o software de controle de versão "git" ná maquina: https://git-scm.com/downloads

# TERCEIRO PASSO

Configuração da conexão do Rstudio com o github (é necessário cirar um novo projeto no Rstudio)
Existem duas formas para se configurar, manualmente (interfade gráfica) ou via código. O canal "RLadies São Paulo" tem tuorias
mostrando as duas formas:

## Configuração via código (pacote usethis): https://youtu.be/2gmofUthjKk

install.packages("usethis")  # instalação do pacote usethis
 
usethis::use_git_config(# Seu nome
        user.name = "nome de usuário", 
        # Seu email
        user.email = "email da conta do github") # Informar para o Git seu nome, e email.

usethis::edit_r_environ() # Abrir o arquivo ambiente R

usethis::create_github_token() # gerar token e digitar no arquivo ambiente R: GITHUB_PAT=ghp_MIjmPdvQB ... " ( ver RLadies São Paulo)

usethis::use_git()  # comando para usar o git

usethis::use_github() # comando para usar o github

## Configuração manual: https://youtu.be/seWbPrPoKag

A configuração manual é intuitiva: cria-se um novo projeto com controle de versão do git e partir daí é so digitar o 
caminho do github (por exemplo:https://github.com/marcosperroni/PerformanceAnalytics) e confirmar.



observação: na configuração manual tive que dar o seguitne comando no terminal do Rstudio: git push origin master 
(na interface gráfica não estava conseguindo dar um push até digitar esse comando).
