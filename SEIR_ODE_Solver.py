import argparse
import datetime as dt
from time import time
import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt
from scipy import optimize
from pprint import pprint



from sklearn.metrics import mean_squared_error
from sklearn.model_selection import ParameterGrid

from pandas.compat.pickle_compat import _class_locations_map

debug = False

_class_locations_map.update({
    ('pandas.core.internals.managers', 'BlockManager'): ('pandas.core.internals', 'BlockManager')
})

# Parameters
MAXITER = 1500
OFFSET_DAY = 7
MODEL = ['Euler 2 step', 'Euler'][0]

X0_template = [0.75, 0.25, 0.75, 1e-4]

param_template = {'beta':0.75,
                  'gamma':0.25,
                  'sigma':0.75,
                  'zeta':1e-4}

ode_task_descriptor = {'Countries': ["Austria", "Germany"],
                   'Start Date': "2020-03-01",
                   'End Date': "2020-07-01",
                   'nLevels': 5}

bla = []
#ODE_Model = {'Country': "Austria",
#             'Start Date': "2020-03-10",
#             'End Date': "2020-06-18",
#             'Stringency Levels': [{'Start Date': "2020-03-10", 'End Date': "2020-03-11", 'Level': 0},
#                                   {'Start Date': "2020-03-11", 'End Date': "2020-06-18", 'Level': 1}],
#             'Level Parameters': {0: {'alpha':0, 'beta':1}},
##             'Ratio Parameter': 1
#             }

# Euler one-step method
# alpha: correlation between number of positive tests and "real" number of infected
# SEIR parameter, beta, gamma, sigma, zeta
def model_euler(df_model, beta, gamma, sigma, zeta, h=1):
    for i in range(1, len(df_model)):
        S_p = df_model['S'].iloc[i-1]
        E_p = df_model['E'].iloc[i-1]
        I_p = df_model['I'].iloc[i-1]
        R_p = df_model['R'].iloc[i-1]
        
        df_model .iloc[i] = [S_p + h*f_S(beta, zeta, S_p, I_p, R_p),
                      E_p + h*f_E(beta, sigma, S_p, I_p, E_p),
                      I_p + h*f_I(sigma, gamma, E_p, I_p),
                      R_p + h*f_R(gamma, zeta, I_p, R_p)]
    return df_model

# h ... step width
# df_model ... a Dataframe containing a date index and S, E, I, R columns
# beta, gamma, sigma, zeta ... ODE parameters

def model_euler_2step(df_model, beta, gamma, sigma, zeta, h=1):    
    for i in range(1, len(df_model)):
        S_p = df_model['S'].iloc[i-1]
        E_p = df_model['E'].iloc[i-1]
        I_p = df_model['I'].iloc[i-1]
        R_p = df_model['R'].iloc[i-1]
        
        S_s = S_p + h*f_S(beta, zeta, S_p, I_p, R_p)
        E_s = E_p + h*f_E(beta, sigma, S_p, I_p, E_p)
        I_s = I_p + h*f_I(sigma, gamma, E_p, I_p)
        R_s = R_p + h*f_R(gamma, zeta, I_p, R_p)
        df_model.iloc[i] = [S_p + 0.5*h*(f_S(beta, zeta, S_p, I_p, R_p) + f_S(beta, zeta, S_s, I_s, R_s)),
                      E_p + 0.5*h*(f_E(beta, sigma, S_p, I_p, E_p) + f_E(beta, sigma, S_s, I_s, E_s)),
                      I_p + 0.5*h*(f_I(sigma, gamma, E_p, I_p) + f_I(sigma, gamma, E_s, I_s)),
                      R_p + 0.5*h*(f_R(gamma, zeta, I_p, R_p) + f_R(gamma, zeta, I_s, R_s))]
    return df_model

def model_euler_2step_np(df_model, beta, gamma, sigma, zeta, h=1):   
    S_p = np.zeros(len(df_model))
    E_p = np.zeros(len(df_model))
    I_p = np.zeros(len(df_model))
    R_p = np.zeros(len(df_model))
    
    S_p[0] = df_model.iloc[0]['S']
    E_p[0] = df_model.iloc[0]['E']
    I_p[0] = df_model.iloc[0]['I']
    R_p[0] = df_model.iloc[0]['R']
    
    for i in range(1, len(df_model)):
        S_pp = S_p[i-1]
        E_pp = E_p[i-1]
        I_pp = I_p[i-1]
        R_pp = R_p[i-1]
        
        S_s = S_pp + h*f_S(beta, zeta, S_pp, I_pp, R_pp)
        E_s = E_pp + h*f_E(beta, sigma, S_pp, I_pp, E_pp)
        I_s = I_pp + h*f_I(sigma, gamma, E_pp, I_pp)
        R_s = R_pp + h*f_R(gamma, zeta, I_pp, R_pp)
        S_p[i] = S_pp + 0.5*h*(f_S(beta, zeta, S_pp, I_pp, R_pp) + f_S(beta, zeta, S_s, I_s, R_s))
        E_p[i] = E_pp + 0.5*h*(f_E(beta, sigma, S_pp, I_pp, E_pp) + f_E(beta, sigma, S_s, I_s, E_s))
        I_p[i] = I_pp + 0.5*h*(f_I(sigma, gamma, E_pp, I_pp) + f_I(sigma, gamma, E_s, I_s))
        R_p[i] = R_pp + 0.5*h*(f_R(gamma, zeta, I_pp, R_pp) + f_R(gamma, zeta, I_s, R_s))
    df_model['S'] = S_p
    df_model['E'] = E_p
    df_model['I'] = I_p
    df_model['R'] = R_p
    return df_model

# DGLs
def f_S(beta, zeta, S, I, R):
    return -beta*S*I + zeta*R

def f_E(beta, sigma, S, I, E):
    return +beta*S*I - sigma*E

def f_I(sigma, gamma, E, I):
    return sigma*E - gamma*I

def f_R(gamma, zeta, I, R):
    return gamma*I - zeta*R

def calculate_series(ODE_Model, df_model, dgl_model=model_euler_2step):
    for idx, sl in ODE_Model['Stringency Levels'].iterrows():
        df_part = df_model[sl['Start Date']:sl['End Date']]
        if len(df_part) > 1:
            df_model.loc[sl['Start Date']:sl['End Date']] = np.array(dgl_model(df_part.copy(), **ODE_Model['Level Parameters'][sl['Level']]))
            if debug:
                print(df_model)
    return df_model

def error_func_rbgsz(X0, ODE_Model, df_orig_p, dgl_model=model_euler_2step):
    # punish parameters < 0 and > 1
    if (min(X0) < 0) or (max(X0) > 1):
        return 1e3
    # first parameter gives the ratio between positive tests and truly infected
    r = X0[0]
    X0 = X0[1:]
    start_date = ODE_Model['Start Date']
    
    # initialize model data
    df_model = pd.DataFrame(index = df_orig_p.index, columns=['S', 'E', 'I', 'R'])
    df_model.loc[start_date, 'I'] = df_orig_p[start_date]*r
    df_model.loc[start_date, 'E'] = df_orig_p[start_date]*r
    df_model.loc[start_date, 'R'] = 0
    df_model.loc[start_date, 'S'] = 1 - df_model.loc[start_date, ['I', 'E', 'R']].sum()
    
    # overwrite model parameters with parameters from optimizer
    ODE_Model['Level Parameters'] = {}
    for k in ODE_Model['Stringency Levels']['Level'].unique():
        ODE_Model['Level Parameters'][k] = {'beta': X0[0], 'gamma': X0[1], 'sigma': X0[2], 'zeta': X0[3]}
        X0 = X0[4:]
    
    df_model = calculate_series(ODE_Model, df_model, dgl_model)
    # punish model values < 0
    if df_model.min().min() < 0:
        return 1e3
            
    mse = mean_squared_error(df_orig_p*r, (df_model['I']+df_model['R']))
    print(mse)
    return mse
    
class ODE_Task:
    def __init__(self, task_descriptor, modelname=MODEL):
        if modelname == 'Euler 2 step':
            self.model = model_euler_2step_np
        if modelname == 'Euler':
            self.model = model_euler
        self.countries = task_descriptor['Countries']
        self.start_date = task_descriptor['Start Date']
        self.end_date = task_descriptor['End Date']
        self.nLevels = task_descriptor['nLevels']
        self.ODE_Models = {}
        for country in self.countries:
            self.ODE_Models[country] = self.init_model(country)

    def init_model(self, country):
        ODE_Model = {}
        ODE_Model['Country'] = country
        ODE_Model['Start Date'] = self.start_date
        ODE_Model['End Date'] = self.end_date
        stringency_interval = extract_stringency_levels(country, self.start_date, self.end_date, self.nLevels)
        stringency_interval['End Date'] = stringency_interval['Start Date'].shift(periods=-1, fill_value=pd.Timestamp(self.end_date))
        ODE_Model['Stringency Levels'] = stringency_interval
        ODE_Model['Level Parameters'] = {}

        return ODE_Model
    
    def calculate_series(self, country):
        df = load_country_from_cvs(country, self.start_date, self.end_date)
        ODE_Model = self.ODE_Models[country]

        pop = load_population_from_cvs(country)
        
        df_model = pd.DataFrame(index = df.index, columns=['S', 'E', 'I', 'R'])
        df_model.loc[self.start_date, 'I'] = df[self.start_date]*ODE_Model['Ratio Parameter']/pop
        df_model.loc[self.start_date, 'E'] = df[self.start_date]*ODE_Model['Ratio Parameter']/pop
        df_model.loc[self.start_date, 'R'] = 0
        df_model.loc[self.start_date, 'S'] = 1 - df_model.loc[self.start_date, ['I', 'E', 'R']].sum()

        return calculate_series(ODE_Model, df_model)

    def calculate_tests(self, country):
        df_model = self.calculate_series(country)
        ODE_Model = self.ODE_Models[country]
        pop = load_population_from_cvs(country)
        df_tested = pd.DataFrame()
        df_tested['predicted'] = df_model.loc[:, ['R', 'I']].sum(axis=1) *pop/ODE_Model['Ratio Parameter']
        df_tested['orig'] = load_country_from_cvs(country, self.start_date, self.end_date)
        return df_tested
        
    
    def error_function(self, params, df, I_0, E_0, R_0, S_0=None, r=None):
        beta = params[0]
        gamma = params[1]
        sigma = params[2]
        zeta = params[3]
        
        # if r is given as optimization parameter
        if (len(params) == 5) and (r is None):
            r = params[4]
            
        # if r is given, renormalize
#        if not r is None:
            I_0 = I_0/r
            E_0 = E_0/r
            R_0 = R_0/r
            if not S_0 is None:
                S_0 = S_0/r
        
        # if S_0 is not given, we expect everything to sum up to 1                
        if S_0 is None:
            S_0 = 1 - I_0 - E_0 - R_0
            
        # send a warning (for now) if we leaf the expected parameter grid
#        if ((min(I_0, E_0, R_0, S_0) < 0) or
#            (max(I_0, E_0, R_0, S_0) > 1) or
#            (S_0 + I_0 + E_0 + R_0) > 1) and debug:
#            print("Warning !!!! unexpected values!!!")
#        if debug:
#            print(S_0, I_0, E_0, R_0)
#            print(df)

        df_model = pd.DataFrame(index=df.index, columns=['S', 'E', 'I', 'R'])
        df_model.iloc[0] = [S_0, E_0, I_0, R_0]
            
#        df_model = self.calculate_series(ODE_Model, df_model)
        df_model = self.model(df_model, beta, gamma, sigma, zeta)
        
        if r is None:
            r = 1
        mse = mean_squared_error(df/r, (df_model['I']+df_model['R']))
        if debug:
#            print(df*r)
#            print((df_model['I']+df_model['R']))
            print("params: ", params, " mse: ", mse)
        return mse
    
    def get_model(self, country):
        return self.ODE_Models[country]
    
    def get_models(self):
        return self.ODE_Models
            
    def estimate_parameters(self, country):
        df = load_country_from_cvs(country, self.start_date, self.end_date)
        ODE_Model = self.ODE_Models[country]

        pop = load_population_from_cvs(country)
        
        levels = ODE_Model['Stringency Levels']['Level'].unique()
        
        # set first parameter r
        X0 = [0.1]
        for _ in levels:
            X0 += X0_template.copy()      
        
        opt = optimize.minimize(error_func_rbgsz, X0, method='Nelder-Mead', options={'maxiter':MAXITER}, args=(ODE_Model, df/pop, self.model))
        # overwrite model paramaters with optimized parameters
        X = opt.x.copy()
        ODE_Model['Ratio Parameter'] = X[0]
        X = X[1:]
        ODE_Model['Level Parameters'] = {}
        for k in ODE_Model['Stringency Levels']['Level'].unique():
            ODE_Model['Level Parameters'][k] = {'beta': X[0], 'gamma': X[1], 'sigma': X[2], 'zeta': X[3]}
            X = X[4:]
        return

def load_country_from_cvs(country, start_date=None, end_date=None):
    df = pd.read_csv('../01_Data/01_Raw/github/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
    # remove rows containing Province/State information
    df = df[df['Province/State'].isna()]
    df = df.set_index('Country/Region')
    # remove all columns that do not look like a date string
    df.drop(labels=[c for c in df.columns if c.count('/') != 2], axis=1, inplace=True)
    # Transpose dataframe
    df = df.T
    df.reset_index(inplace=True)
    df['Date']=df.reset_index()['index'].apply(lambda x: dt.datetime.strptime(x, "%m/%d/%y"))
    df.drop('index', axis=1, inplace=True)
    df.set_index('Date', inplace=True)
    df = df[country]
    if not start_date is None:
        df = df[df.index >= start_date]
    if not end_date is None:
        df = df[df.index <= end_date]    
    return df

def load_population_from_cvs(country):
    df = pd.read_csv('../01_Data/01_Raw/github/COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv')
    # remove rows containing Province/State information
    df = df[df['Province_State'].isna()]
    df = df.set_index('Country_Region')
    return df.loc[country, 'Population']

def extract_stringency_levels(country, start_date=None, end_date=None, number_of_levels=None, offset_day=OFFSET_DAY):
    df_result= pd.DataFrame()
    df = pd.read_csv('../01_Data/01_Raw/github/covid-policy-tracker/data/OxCGRT_latest.csv', dtype={'Date':'str'})
    df['Date'] = df['Date'].apply(lambda x: dt.datetime.strptime(x, "%Y%m%d"))
    
    if not ((start_date is None) or (end_date is None)):
        df= df[(df['Date'] >= start_date) & (df['Date'] <= end_date)]
    elif not start_date is None:
            df= df[df['Date'] >= start_date]
    elif not end_date is None:
            df= df[df['Date'] <= end_date]
            
    df_result=df[df['CountryName'] == country]
    df_result = df_result[['Date', 'StringencyIndexForDisplay']]
    df_result.rename(columns={'StringencyIndexForDisplay':'Level'}, inplace=True)

    if not number_of_levels is None:
        df_result['Level'] = df_result['Level'].map(lambda x: int(np.floor(x*number_of_levels/100)))
    df_result.rename({'Date':'Start Date'}, inplace=True, axis=1)
    df_changes = df_result[df_result['Level']!=df_result['Level'].shift()]
    if offset_day != 0:
        df_changes.loc[df_changes['Start Date'] != start_date, 'Start Date'] +=dt.timedelta(days=offset_day)
        df_changes = df_changes.loc[df_changes['Start Date'] < end_date]
    
    return df_changes

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
    description='Calculate parameters for a SEIR model')
    parser.add_argument(
        '-d', '--debug',
        dest='debug',
        action='store_true',
        help="Debug mode")
    args = parser.parse_args()
    
    debug=args.debug
    country = 'Germany'

    ode_task = ODE_Task(ode_task_descriptor)

    timer_on = time()
    ode_task.estimate_parameters(country)
    print("Elapsed time: {}s".format(time() - timer_on))
    for item in ode_task.get_model(country).items():
        pprint(item)
    df_model = ode_task.calculate_series(country)
    df = ode_task.calculate_tests(country)
    df.plot()
    