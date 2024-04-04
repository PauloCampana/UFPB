print("Questão 1.")
# Utilizando f-string obtenha a saída abaixo:
# Nome: [João da Silva+++++++], Idade: [0038], Salário: [***12500.370***]

nome = "João da Silva"
idade = 38
salario = 12500.37

print(f"Nome: [{nome:+<20}], Idade: [{idade:0>4}], Salário: [{salario:*^15.3f}]")

# Questão 2.
# Escreva o que são ambientes virtuais em Python,
# como criá-los, como ativá-los e como desativá-los.
# Explique também como criar uma lista dos pacotes instalados em um # arquivo .txt
# e como utilizar essa lista para instalar pacotes em um novo ambiente.

# Ambientes virtuais são formas de trabalhar em um projeto python de maneira
# padronizada, utilizando a mesma versão do interpretador e pacotes sem
# causar conflito com outras versões instaladas do sistema.

# Um ambiente virtual pode ser criado com `python3 -m venv nome_do_env`,
# ativado com `source nome_do_end/bin/activate`
# e desativado simplesmente com `deactivate`.

# O comando `pip freeze` produz uma lista dos pacotes instalados
# no ambiente atual, essa lista pode ser passada pra um arquivo com um pipe `>`:
# `pip freeze > nome_do_arquivo.txt`.
# Arquivos de texto podem ser usados para instalar pacotes
# em um novo ambiente com `pip install -r nome_do_arquivo.txt`.

print("\nQuestão 3.")
# A função deve:
# * Verificar se o Pokémon existe no dicionário pokemons;
# * Se o Pokémon existir, retornar um dicionário com as chaves “tipo” e “ataques”;
# * Se o Pokémon não existir, retornar uma string vazia.

pokemons = {
    "pikachu": {
        "tipo": "elétrico",
        "ataques": ["Choque do Trovão", "Raio", "Investida"]
    },
    "charizard": {
        "tipo": "fogo/voador",
        "ataques": ["Lança-chamas", "Fogo Fátuo", "Ataque Aéreo"]
    },
    "blastoise": {
        "tipo": "água",
        "ataques": ["Jato d'Água", "Giro Rápido", "hidro bomba"]
    },
}

def get_pokemon(dictionary, key):
    if key not in dictionary:
        return ""
    return dictionary[key]

print(get_pokemon(pokemons, "blastoise"));
print(get_pokemon(pokemons, "digimon"));
print(get_pokemon(pokemons, "pikachu"));

# Questão 4.
# Ver pasta `app`.

print("\nQuestão 5.")
# Crie os decoradores @par e @impar que dado uma
# lista em Python ele irá retornar uma sublista
# contendo os valores pares e ímpares, respectivamente.

def par(fun):
    def wrapper(x):
        complete = fun(x)
        even_only = []
        for c in complete:
            if c % 2 == 0:
                even_only.append(c)
        return even_only
    return wrapper

def impar(fun):
    def wrapper(x):
        complete = fun(x)
        odd_only = []
        for c in complete:
            if c % 2 == 1:
                odd_only.append(c)
        return odd_only
    return wrapper

@par
def identity1(x):
    return x
@impar
def identity2(x):
    return x

numeros = [3,3,2,3,3,5,0,9,7,0,4,4,7,8,4,2]
print(identity1(numeros))
print(identity2(numeros))
