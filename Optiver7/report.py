import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import pickle
import numpy as np

"""
Helpers
"""
def compute_mean_importance(importance):
    res = importance[0].copy()
    res['Importance'] = np.mean(np.array(
        [df['importance'].values for df in importance]
    ), axis=0)
    
    res = res.drop(['importance'], axis=1)
    
    # reformat for plot
    return pd.DataFrame(
        {'Features':[f for f in res.index], 
         'Importance': res['Importance']}
    ).reset_index(drop=True)


def plot_importance(importance, title='', save_to_file=None, top=None):    
    importance = importance.sort_values(
        ['Importance'], ascending=False
    )[:top]#.sort_values(['Importance'])
    sns.set(font_scale=1)

    plt.figure(figsize=(10, 8))
    #importance.plot.barh(ax=ax)
    sns.barplot(x="Importance", y="Features", data=importance.sort_values(by="Importance",ascending=False))
    
    if title:
        plt.title(title)
    plt.tight_layout()
    
    plt.show()
    plt.close()


"""
Report Functions
"""

# Final Ensemble Comparison
def compare_models():
    master = pd.read_csv("final-ensemble.csv")
    sns.boxplot(data=master[["rmspe-weighted",'rmspe-average','rmspe-poly',
                                'rmspe-ext', 'rmspe-simp', 'rmspe-norm']], orient="h", showfliers=False).set(
        xlabel="RMSPE score",
        ylabel="Models",
        title="Boxplot Comparison of Model Approaches")

    plt.show()
    plt.close()

# Feature Importance
def feature_importance_example():
    
    split = pd.read_feather("split_norm.fth")

    plot_importance(split, title='LGBM Model on Normalized Data: Top 40 Features by Split', top=40)
    

# compare time chaining models
def compare_timechains():
    
    tukey = pickle.load(open("tukey.pickle", "rb"))
    return tukey.summary()
