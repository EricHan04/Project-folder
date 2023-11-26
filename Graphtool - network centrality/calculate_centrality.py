#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np 
from more_itertools import unique_everseen
import graph_tool.all as gt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

#trim the original frame
def trim_raw(year, continent):
    
    dtype = {
    'OverlapYearStart': str,
    'OverlapYearEnd': str,
    'DirBrdID': str,
    'DirectorID': str
    }

    dfo = pd.read_csv('{}_{}.csv.gz'.format(year, continent),
                      encoding="ISO-8859-1",
                      na_filter=False,
                      dtype=dtype, 
                      error_bad_lines=False,
                      warn_bad_lines=False)

    dfo = dfo.drop(labels=[
        'DirBrdName', 'CompanyName', 'DirectorName', 'OverlapYearStart',
        'OverlapYearEnd', 'RoleTitle', 'RoleBoardPosition', 'RoleEDFlag',
        'AssociatedRoleTitle', 'AssociatedRoleBoardPosition',
        'AssociatedRoleEDFlag'
    ],
                   axis=1)

    return dfo

#clean it up
def df_to_nodes_edges(df):


    df = df[['DirBrdID', 'DirectorID']]

    edges = list(df.itertuples(index=False, name=None))
    edges = list(dict.fromkeys(edges))
    edges = list(unique_everseen(edges, key=frozenset))

    d = list(df['DirBrdID']) + list(df['DirectorID'])

    nodes = list(dict.fromkeys(d))

    return edges, nodes

#clean it up
def cleanup1(comp_detail, dfo, country):

    l1 = list(comp_detail[comp_detail['HOCountryName'] == '{}'.format(country)]
              ['BoardID'].unique())
    df = dfo[dfo['CompanyID'].isin(l1)]
    
    #all organization types

    df1 = df.dropna(subset=['Role', 'AssociatedRole'])
    df1.reset_index(inplace=True, drop=True)
    df_all = df1[['DirBrdID', 'DirectorID']]
    
    e_all, n_all = df_to_nodes_edges(df_all)

    #Profressional network (included private, etc.)
    pro = df[(df['OrgType'] == 'Quoted') | (df['OrgType'] == 'Private') |
             (df['OrgType'] == 'Government')]

    profess = pro[(pro['AssociationType'] == 'Listed Org') |
                  (pro['AssociationType'] == 'Unlisted Org') |
                  (pro['AssociationType'] == 'NFP')]
    profess = profess.dropna(subset=['Role', 'AssociatedRole'])
    profess.reset_index(inplace=True, drop=True)
    df_profess = profess[['DirBrdID', 'DirectorID']]
    
    e_profess, n_profess = df_to_nodes_edges(df_profess)

    #Profressional network (Listed firms only)
    quote = df[df['OrgType'] == 'Quoted']
    listed = quote[quote['AssociationType'] == 'Listed Org']
    listed = listed.dropna(subset=['Role', 'AssociatedRole'])
    listed.reset_index(inplace=True, drop=True)
    df_listed = listed[['DirBrdID', 'DirectorID']]
    
    e_listed, n_listed = df_to_nodes_edges(df_listed)

    #non-professional
    nonprofess = df[(df['OrgType'] == 'Universities') | (df['OrgType'] == 'Clubs')
                 | (df['OrgType'] == 'Charities') |
                 (df['OrgType'] == 'Sporting')]

    nonprofess = nonprofess.dropna(subset=['Role', 'AssociatedRole'])
    nonprofess.reset_index(inplace=True, drop=True)
    df_nonprofess = nonprofess[['DirBrdID', 'DirectorID']]
    
    e_nonprofess, n_nonprofess = df_to_nodes_edges(df_nonprofess)

    return e_all, n_all, e_profess, n_profess, e_listed, n_listed, e_nonprofess, n_nonprofess

#calculate centrality
def cal_centrality(nodes, edges):
    
    idxs = dict(zip(nodes, range(len(nodes))))
    iedges = [(idxs[e[0]],idxs[e[1]]) for e in edges]

    g = gt.Graph(directed=False)
    g.add_edge_list(iedges)

    #10 create empty df to export later
    final = pd.DataFrame({'ID_source' : list(idxs.keys()) , 'ID' : list(idxs.values()) })

    #11 create empty df for betweenness and closeness
    df = pd.DataFrame(columns=['ID', 'bet_val'])

    #12 calculate degree and add to final csv file
    degree = list(g.get_out_degrees(list(g.get_vertices()), eweight=None))
    se = pd.Series(degree)
    final['degree'] = se.values

    #13 calculate eigenvector and add to final csv file
    eigen_val = list(gt.eigenvector(g)[1])
    se = pd.Series(eigen_val)
    final['eigenvector'] = se.values

    #14 seperate disconnected subgraph by labeling component of subgrah 
    comp = gt.label_components(g, attractors=False)[0]
    a = max(list(comp))
    N = len(nodes)
#     print(N)

    #15 calculate betweenness centrality for each component graph and save to df

    for i in range(0,a+1):
        #filter subgraph base on label component
        u = gt.GraphView(g, vfilt=comp.a==i)

        #15.1 betweenness
        vb, eb = gt.betweenness(u)
        bet_val = list(vb)

        #nodes list of subgraph
        nl = list(u.get_vertices())
        nu = len(nl) #length of nodes list

        #15.2 closeness
        clo_val = list(gt.closeness(u))
        clo_val = [i*nu/N for i in clo_val]

        df1 = pd.DataFrame({'ID':nl, 'bet_val':bet_val, 'clo_val':clo_val})
        df = df.append(df1, ignore_index=True)

    #16 add df to final
    final = pd.merge(final,df,how='left',on='ID')
    
    return final

#rank centrality

def perrank(df):
    
    df['pct_degree'] = df['degree'].rank(pct=True)
    df['pct_bet'] = df['bet_val'].rank(pct=True)
    df['pct_clo'] = df['clo_val'].rank(pct=True)
    df['pct_eigen'] = df['eigenvector'].rank(pct=True)
    
    return df

#pca
def pca(df):
    
    test1 = df[['degree', 'eigenvector', 'bet_val', 'clo_val']]
    scaler = StandardScaler() # nothing need to insert here
    scaler.fit(test1)
    scaled_data = scaler.transform(test1)
    pca = PCA(n_components=1)
    pca.fit(scaled_data)
    x_pca = pca.transform(scaled_data)
    df = df.assign(fa=x_pca)
    
    return df


# In[ ]:




