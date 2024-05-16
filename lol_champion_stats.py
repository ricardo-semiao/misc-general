from requests import get
from bs4 import BeautifulSoup
import pandas as pd
import matplotlib.pyplot as plt


#Obter a tabela do site:
link = 'https://leagueoflegends.fandom.com/wiki/List_of_champions/Base_statistics'
site = get(link)
site = BeautifulSoup(site.content, 'html.parser')

table = site.find('table')

head = table.find('tr').findAll('th')
headtext = [i.text for i in head]

body = table.find('tbody').findAll('tr')
bodytext = []
for i in body:
    line = i.findAll('td')
    bodytext.append([j.text for j in line])
    
df = pd.DataFrame(bodytext, columns=headtext)


#Selecionar apenas os campeões e stats importantes:
header = ['Champions', 'HP', 'HP+', 'AR', 'AR+', 'MR', 'MR+']
champs = [' Riven', ' Irelia', ' Camille', ' Fiora', ' Kled & Skaarl', ' Renekton', ' Pantheon', ' Jax', ' Sylas', ' Sett', ' Wukong']

dfsub = df.loc[df['Champions'].isin(champs)][header]
for i in header[1:len(header)]:
    dfsub[i] = pd.to_numeric(dfsub[i])


#Visualização para comparar:
lv = [*range(1,19)]
stats = ['HP', 'AR', 'MR']
ratio = 0.6

#Adicionar EHP no DF:
for i in [0,17]:
    EHP = []
    for j in stats:
         EHP.append(dfsub[j] + dfsub[j+'+']*i)
    value = (1+EHP[1]/100)*EHP[0]*ratio + (1+EHP[2]/100)*EHP[0]*(1-ratio)
    dfsub['EHP@'+str(i+1)] = [int(i) for i in value]

#Gráficos:
for j in stats:
    for i in champs[0:4]:
        index = dfsub['Champions'] == i
        stat = [dfsub[j].loc[index].values + dfsub[j+'+'].loc[index].values*(k-1) for k in lv]
        plt.plot(lv, stat, label=i)
    plt.legend()
    plt.xlabel('Level')
    plt.ylabel(j)
    plt.title(j+' per level')
    plt.show()

#Adicionar EHP nos gráficos:
for i in champs[0:4]:
    index = dfsub['Champions'] == i
    j = stats[0]
    HP = [dfsub[j].loc[index].values + dfsub[j+'+'].loc[index].values*(k-1) for k in lv]
    j = stats[1]
    AR = [dfsub[j].loc[index].values + dfsub[j+'+'].loc[index].values*(k-1) for k in lv]
    stat = [(1+AR[k]/100)*HP[k] for k in range(len(lv))]
    plt.plot(lv, stat, label=i)
plt.legend()
plt.xlabel('Level')
plt.ylabel('EHP')
plt.title('EHP per level')
plt.show()


dfsub = dfsub.sort_values(by='EHP@18', ascending=False)
b = [dfsub['EHP@18'].values[i] - dfsub['EHP@18'].values[i+1] for i in range(len(dfsub)-1)]
b.insert(0, 0)

dfsub['EHPdiff'] = b

print(dfsub)


