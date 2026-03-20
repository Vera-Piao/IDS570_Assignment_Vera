from pathlib import Path
import json

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    confusion_matrix,
    classification_report,
    roc_auc_score,
)

DATA_DIR = Path("data")

with open(DATA_DIR / "train_core_vs_neg.json", "r", encoding="utf-8") as f:
    train_data = json.load(f)

with open(DATA_DIR / "test_core_vs_neg.json", "r", encoding="utf-8") as f:
    test_data = json.load(f)

X_train_texts = [t for (t, y) in train_data]
y_train = [y for (t, y) in train_data]

X_test_texts = [t for (t, y) in test_data]
y_test = [y for (t, y) in test_data]

vectorizer = TfidfVectorizer(
    lowercase=True,
    min_df=5,
    max_df=0.9
)

X_train = vectorizer.fit_transform(X_train_texts)
X_test = vectorizer.transform(X_test_texts)

clf = LogisticRegression(
    max_iter=1000,
    n_jobs=1
)

clf.fit(X_train, y_train)

y_pred = clf.predict(X_test)
y_proba = clf.predict_proba(X_test)[:, 1]

print("Confusion matrix:")
print(confusion_matrix(y_test, y_pred))

print("\nClassification report:")
print(classification_report(y_test, y_pred))

print("ROC AUC:", roc_auc_score(y_test, y_proba))

coef = clf.coef_[0]
nonzero = (coef != 0).sum()
total = len(coef)

print("\nNon-zero coefficients:", nonzero)
print("Total coefficients:", total)
print("Sparsity ratio:", round(nonzero / total, 4))

feature_names = vectorizer.get_feature_names_out()

top_pos_idx = coef.argsort()[-15:][::-1]
top_neg_idx = coef.argsort()[:15]

print("\nTop 15 positive-weight words (CORE=1):")
for i in top_pos_idx:
    print(f"{feature_names[i]:20s} {coef[i]:.6f}")

print("\nTop 15 negative-weight words (NEG=0):")
for i in top_neg_idx:
    print(f"{feature_names[i]:20s} {coef[i]:.6f}")