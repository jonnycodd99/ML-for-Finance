import pandas as pd
import numpy as np
import random 

from sklearn.metrics import r2_score, mean_squared_error
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score

import torch
import torch.nn as nn
import torch.optim as optim

def lag_features(df, features, lags):
    df_ = df.copy()
    
    if isinstance(lags, int):
        for feature in features:
            for lag in range(1, lags + 1):
                df_[f'{feature}_lag{lag}'] = df[feature].shift(lag)
    elif isinstance(lags, list):
        if len(features) != len(lags):
            raise ValueError("Length of 'features' and 'lags' must match when 'lags' is a list.")
        for feature, lag in zip(features, lags):
            for l in range(1, lag + 1):
                df_[f'{feature}_lag{l}'] = df[feature].shift(l)
    else:
        raise TypeError("Lags must be either an integer or a list of integers.")
    return df_

def compute_reg_metrics(y, y_pred):
    R2 = np.round(r2_score(y, y_pred), 5)
    RMSE = np.round(mean_squared_error(y, y_pred), 6)
    return R2, RMSE

def regression_metrics(y_train, y_train_pred, y_test, y_test_pred):
    r2_train, rmse_train = compute_reg_metrics(y_train, y_train_pred)
    r2_test, rmse_test = compute_reg_metrics(y_test, y_test_pred)

    print(' --- TRAIN --- ')
    print(f' R2: {r2_train}')
    print(f' RMSE: {rmse_train}')

    print(' --- TEST --- ')
    print(f' R2: {r2_test}')
    print(f' RMSE: {rmse_test}')
    print(f' Gap RMSE: {rmse_test - rmse_train}')

def compute_class_metrics(y, y_pred):
    acc = np.round(accuracy_score(y, y_pred), 5)
    prec = np.round(precision_score(y, y_pred), 5)
    recall = np.round(recall_score(y, y_pred), 5)
    f1score = np.round(f1_score(y, y_pred), 5)
    return acc, prec, recall, f1score

def classification_metrics(y_train, y_train_pred, y_test, y_test_pred):
    acc, prec, recall, f1score = compute_class_metrics(y_train, y_train_pred)
    acc_, prec_, recall_, f1score_ = compute_class_metrics(y_test, y_test_pred)

    print(' --- TRAIN --- ')
    print(f' Accuracy: {acc}')
    print(f' Precision: {prec}')
    print(f' Recall: {recall}')
    print(f' F1-score: {f1score}')

    print(' --- TEST --- ')
    print(f' Accuracy: {acc_}')
    print(f' Precision: {prec_}')
    print(f' Recall: {recall_}')
    print(f' F1-score: {f1score_}')

def set_seed(seed):
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    np.random.seed(seed)
    random.seed(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False
    
