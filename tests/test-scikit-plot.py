import matplotlib.pyplot as plt
import scikitplot as skplt
from sklearn.datasets import load_digits
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_val_predict

X, y = load_digits(return_X_y=True)

random_forest_clf = RandomForestClassifier(n_estimators=5, max_depth=5, random_state=1)
predictions = cross_val_predict(random_forest_clf, X, y)

skplt.metrics.plot_confusion_matrix(y, predictions, normalize=True)
plt.show()
