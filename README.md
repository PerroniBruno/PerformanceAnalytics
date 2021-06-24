# PerformanceAnalytics

Como configurar um projeto integrando o Rstudio ao github

PRIMEIRO PASSO

Abrir uma conta no https://github.com/

SEGUNDO PASSO

Instalar o software de controle de versão "git" ná maquina: https://git-scm.com/downloads

TERCEIRO PASSO

Configuração da conexão do Rstudio com o github (é necessário cirar um novo projeto no Rstudio)
Existem duas formas para se configurar, manualmente (interfade gráfica) ou via código. O canal "RLadies São Paulo" tem tuorias
mostrando as duas formas:

Configuração via código (pacote usethis): https://youtu.be/2gmofUthjKk

install.packages("usethis")  # instalação do pacote usethis

# Informar para o Git seu nome, e email.
usethis::use_git_config(# Seu nome
        user.name = "nome de usuário", 
        # Seu email
        user.email = "email da conta do github")

# Abrir o r
usethis::edit_r_environ()

usethis::create_github_token()

usethis::use_git()

usethis::use_github()


Configuração manual: https://youtu.be/seWbPrPoKag

observação: na configuração manual tive que dar o seguitne comando no terminal do Rstudio: git push origin master 
(na interface gráfica não estava conseguindo dar um push até digitar esse comando).
