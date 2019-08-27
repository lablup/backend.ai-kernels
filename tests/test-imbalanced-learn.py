# Authors: Guillaume Lemaitre <g.lemaitre58@gmail.com>
# License: MIT

import numpy as np
import matplotlib.pyplot as plt

from sklearn.datasets import make_moons, make_blobs
from sklearn.ensemble import IsolationForest
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report

from imblearn import FunctionSampler
from imblearn.pipeline import make_pipeline

print(__doc__)

rng = np.random.RandomState(42)


def plot_scatter(X, y, title):
    """Function to plot some data as a scatter plot."""
    plt.figure()
    plt.scatter(X[y == 1, 0], X[y == 1, 1], label='Class #1')
    plt.scatter(X[y == 0, 0], X[y == 0, 1], label='Class #0')
    plt.legend()
    plt.title(title)
    
    
moons, _ = make_moons(n_samples=500, noise=0.05)
blobs, _ = make_blobs(n_samples=500, centers=[(-0.75, 2.25),
                                              (1.0, 2.0)],
                      cluster_std=0.25)
outliers = rng.uniform(low=-3, high=3, size=(500, 2))
X_train = np.vstack([moons, blobs, outliers])
y_train = np.hstack([np.ones(moons.shape[0], dtype=np.int8),
                     np.zeros(blobs.shape[0], dtype=np.int8),
                     rng.randint(0, 2, size=outliers.shape[0],
                                 dtype=np.int8)])

plot_scatter(X_train, y_train, 'Training dataset')