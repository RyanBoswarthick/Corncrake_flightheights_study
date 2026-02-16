# README

Ce script R est un modèle **Bayésien hiérarchique** réalisé avec `nimble`. Son but est d'estimer la **véritable distribution de la hauteur de vol** des Râles des genêts en corrigeant l'erreur de mesure du GPS.

---

## 1. Objectif du Modèle

Le GPS n'est pas parfait : même quand l'oiseau est au sol, l'altitude enregistrée n'est pas exactement 0. Le script utilise deux sources de données pour "nettoyer" le bruit :

* **Données au sol (`obs_ground`)** : Pour quantifier l'erreur du GPS ().
* **Données en vol (`obs_flight`)** : Pour estimer les paramètres biologiques du vol ( et ) une fois l'erreur GPS soustraite.

---

## 2. Structure du Modèle NIMBLE

Le cœur du script est le bloc `nimbleCode`. Il définit deux relations statistiques :

* **Étalonnage de l'erreur (Sol) :**



On part du principe que la vraie altitude au sol est 0. Toute variation observée permet d'estimer l'écart-type de l'erreur GPS ().
* **Modèle de vol (Hiérarchique) :**
1. **Niveau Observation :** 
(L'altitude vue par le GPS est la vraie altitude plus l'erreur estimée précédemment).
2. **Niveau Processus :** 
(La "vraie" altitude de vol suit une loi Log-Normale, car les hauteurs de vol sont toujours positives et souvent étirées vers le haut).



---

## 3. Analyse des étapes clés

### Préparation et Compilation

Le script utilise bien la syntaxe `package::function` et le pipe native `|>`.

* **Initialisation :** Il utilise `pmax(1)` pour s'assurer que les valeurs de départ de l'altitude sont positives, ce qui est crucial pour la distribution Log-Normale.
* **MCMC :** Il lance 3 chaînes de 10 000 itérations pour s'assurer que l'estimation des paramètres (, , ) converge.

### Validation et Simulation

Une fois les paramètres extraits via `MCMCsummary`, le script effectue des simulations pour vérifier la qualité du modèle :

* Il compare la distribution simulée (avec erreur) aux données brutes.
* Il utilise la **distance de Wasserstein** (`transport::wasserstein1d`) pour mesurer mathématiquement la similarité entre les distributions simulées et observées. Un score faible valide le modèle.

### Analyse des Risques (Zones de collision)

C'est la partie finale et la plus concrète :

* Le script calcule la probabilité que l'oiseau vole dans des tranches d'altitude spécifiques (0-20m, 20-200m, etc.).
* **L'astuce :** Il utilise l'intégralité de la chaîne MCMC (`prop_samples`) pour calculer non seulement la médiane, mais aussi l'intervalle de confiance (95%) de ces probabilités.

---

## 4. Visualisations produites

Le script génère trois graphiques majeurs :

1. **Comparaison Densité :** Montre comment l'erreur GPS "étale" la distribution et comment la version corrigée est plus resserrée.
2. **Validation (Histogrammes) :** Compare visuellement les données brutes et les prédictions du modèle.
3. **Graphique de Risque (Area Plot) :** Un graphique coloré montrant les zones de vol avec des pourcentages précis (ex: "% de temps passé entre 20 et 200m").

---

### Remarques sur tes instructions personnalisées

* **Syntaxe :** Le code respecte parfaitement `package::function` et `|>`.
* **GitHub :** Attention, le script enregistre des figures dans `figures/07_models/...`. Assure-toi que ton `.gitignore` bloque bien les `.tif` si tu en génères par ailleurs, car ce script manipule des données issues de DEM (Digital Elevation Models).

**Souhaites-tu que je t'aide à modifier les seuils des zones de risque (actuellement 20, 200, 300m) ou à ajouter un diagnostic de convergence plus approfondi ?**