#----- Testes: -----
#Remover os "#" antes dos "#'''" (com ctrl+r) para suprimir os inputs() e poder rodar os testes.

#           padrao                     , tipo, comeco, razao, limite, n, rep, cumu, ponta, centro, ponta_n, centro_n
testes = [["Sequência"                 , "PA"       , 0, 0, "u", 4, "-", "Sim" , "-", "-", "-", "-"],
          ["Sequência"                 , "PA"       , 5, 0, "u", 4, "-", "Não" , "-", "-", "-", "-"],
          ["Sequência"                 , "PA"       , 0, 0, "t", 4, "-", "Não" , "-", "-", "-", "-"],
          ["Sequência"                 , "PA"       , 0, 0, "t", 4, "-", "Sim" , "-", "-", "-", "-"],
          
          ["Sequência"                 , "PG"       , 0, 2, "u", 9, "-", "Sim", "-", "-", "-", "-"],
          ["Sequência"                 , "PG"       , 0, 2, "t", 9, "-", "Sim", "-", "-", "-", "-"],
          ["Sequência"                 , "PG"       , 10, 2, "u", 9, "-", "Sim", "-", "-", "-", "-"],
          
          ["Sequência"                 , "Fibonatti", "-", "-", "u", 9, "-", "Não", "-", "-", "-", "-"],
          
          ["Sequência repetida"        , "PA"       , 0, 1, "t", 4, 3, "Não", "-", "-", "-", "-"], 
          ["Sequência repetida"        , "PG"       , 1, 4, "u", 9, 2, "Sim", "-", "-", "-", "-"], 
          ["Sequência repetida"        , "Fibonatti", "-", "-", "t", 6, "Não", 1, "-", "-", "-", "-"],
          
          ["Espelho"                   , "-"        , "-", "-", "-", -1, "-", "Não", 3, 1, 4, 5], 
          ["Sequência espelho"         , "PG"       , 1, 2, "u", 9, 0, "Não", "-", 0, "-", 0], 
          ["Sequência espelho repetida", "Fibonatti", "-", "-","t", 6, 2, "Não", "-", 1, "-", 5]]


j = 6 #Qual "linha" da "matriz" de testes deve ser executada.
names = ["padrao", "tipo", "comeco", "razao", "limite", "n", "rep", "cumu", "ponta", "centro", "ponta_n", "centro_n"]
padrao=tipo=comeco=razao=limite=n=rep=cumu=ponta=centro=ponta_n=centro_n=1
#Como definimos as variáveis no loop abaixo, o spyder não reconhece que elas foram definidas então essa linha suprime as mensagens de erro

for i in range(len(names)):
    globals()[names[i]] = testes[j][i]
    



#----- Funções aplicáveis em vários padrões: -----
def output(seq):
    seq = "".join(str(i) for i in seq)
    print("\nSua sequência: ", seq)
    
    
    
def sequencia():
    #'''
    tipo = input("Insira a sequência desejada, dentre as opções: PA, PG e Fibonatti: ")
    #'''
    
    if tipo in ["PA","PG"]:
        #'''
        comeco = int(input("Insira o primeiro termo de sua sequência: "))
        razao = int(input("Insira a razão de sua sequência: "))
        limite = input("Como limite, deseja declarar o tamanho da sequência (digite t), ou seu último termo (digite u)? ")
        n = int(input("Insira o limite: "))
        #'''
        
        if limite == "t":
            seq = [0]*n
            
            for i in range(n):
                if tipo == "PA":
                    seq[i] = comeco + razao*i
                elif tipo == "PG":
                    seq[i] = comeco * razao**i
                    
        elif limite == "u":
            print("\nObs: o real último valor pode ser menor se o inserido menos o primeiro não for múltiplo da razão.")
            if comeco > n: print("\nERRO: começo maior que o limite.")
            seq = [0]
    
            i = 0
            while seq[i] <= n:
                if tipo == "PA":
                    seq.append(comeco + razao*i)
                    if razao == 0:
                        seq[i-1] = n+1
                        print("\nERRO: A razão não pode ser 0 se não foi definido o tamanho da sequência (opção "u").")
                        
                elif tipo == "PG":
                    seq.append(comeco * razao**i)
                    if razao == 0 or comeco == 0:
                        seq[i-1] = n+1
                        print("\nERRO: A razão e/ou o começo não podem ser 0 se não foi definido o tamanho da sequência (opção "u").")
               
                elif tipo == "Fibonatti":
                    seq.append(seq[i] + seq[i+1])
                i = i + 1
                
            del seq[0]
            if seq[-1] > n: del seq[-1]
    
    
    elif tipo == "Fibonatti":
        #'''
        limite = input("Como limite, deseja declarar o tamanho da sequência (digite t), ou seu último termo (digite u)? ")
        n = int(input("Insira o limite: "))
        #'''
        
        if limite == "t":
            seq = [1,1] + [0]*(n-2)
            for i in range(n-2):
                seq[i+2] = seq[i+1] + seq[i]
        
        elif limite == "u":
            seq = [1,1]
            i = 0
            while seq[i+1] <= n:
                seq.append(seq[i] + seq[i+1])
                i = i + 1
            
            if seq[-1] > n: del seq[-1]
    
    if cumu == "Sim":
        seq2 = list(seq)
        for i in range(len(seq)):
            seq[i] = "".join(str(j) for j in seq2[0:i+1])
        
    return seq




#----- Programa: -----
print("##############################################################")
print("Bem vindo à impressora de padrões! Os padrões disponíveis são: \n")

padroes = ["Sequência: 12345..., 1 12 123...;",
           "Sequência repetida: 123123123123..., 11 1212 123123...;",
           "Espelho: 111101111, 3332222333;",
           "Sequência espelho: 3210123;",
           "Sequência espelho repetida: 3213210123123;"]

for i in range(len(padroes)):
    print(padroes[i])

print("\nObs: todas as respostas numéricas devem ser números inteiros e positivos, afora quando indicado. Recomendamos não fazer sequências com números de mais de um algarismo.")

#'''
padrao = input("\nInsira o padrão desejado (pelos nomes definidos acima): ")
#'''


if padrao in ["Sequência", "Sequência repetida"]:
    #'''
    cumu = input("Você deseja que sua sequência seja cumulativa, i.e. 1 12 123... (Sim) ou Não, i.e. 123... (Não)? ")
    #'''
    print("\nSobre a sequência a ser repetida: ")
    
    subseq = sequencia()
    
    if padrao == "Sequência repetida":
        #'''
        rep = int(input("Insira o número de repetições: "))
        #'''
        1 #só pra marcar a indentação no "modo teste"
    else:
        rep = 1
    
    if cumu == "Sim":
        for i in range(len(subseq)):
            subseq[i] = str(subseq[i])*rep
        seq = subseq
        
        seq = " ".join(str(i) for i in seq)
        print("\nSua sequência: ", seq)
    
    else:    
        seq = (subseq * rep)
        output(seq)




if padrao == "Espelho":
    #'''
    ponta = int(input("Insira o termo da ponta de sua sequência: "))
    centro = int(input("Insira o termo do centro de sua sequência: "))
    #'''
    
    print("Você pode informar o tamanho da ponta, do centro, ou apenas um dos dois + o tamanho total da sequência.")
    #'''
    ponta_n = int(input("Insira o tamanho de cada ponta (-1 para ignorar): "))
    centro_n = int(input("Insira o tamanho do centro (-1 para ignorar, 0 para não ter centro): "))
    n = int(input("Digite o tamanho da sequência (-1 para ignorar): "))
    #'''
    
    if (ponta_n == -1) + (centro_n == -1)  + (n == -1) > 1:
        print("\nERRO: apenas um valor pode ser ignorado.")
    
    if centro_n == -1: centro_n = n - ponta_n*2
    if ponta_n == -1: ponta_n = (n-centro)/2
    
    seq = [ponta]*ponta_n + [centro]*centro_n + [ponta]*ponta_n
    output(seq)




if padrao in ["Sequência espelho", "Sequência espelho repetida"]:
    #'''
    centro = int(input("Insira o termo do centro de sua sequência: "))
    centro_n = int(input("Insira o tamanho do centro (0 para não ter centro): "))
    #'''
    
    print("\nSobre a sequência a ser repetida: ")
    
    cumu = "Não"
    subseq = sequencia()
    
    if padrao == "Sequência espelho repetida":
        #'''
        rep = int(input("Insira o número de repetições: "))
        #'''
        1 #só pra marcar a indentação no "modo teste"
    else:
        rep = 1
    
    seq = subseq * rep + [centro]*centro_n + list(reversed(subseq)) * rep
    output(seq)
    
else: print("\nERRO: o padrão deve ser ""Sequência"", ""Sequência repetida"", etc...")