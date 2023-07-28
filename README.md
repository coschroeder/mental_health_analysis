# “How is your thesis going?” –  Ph.D. students’ perspectives on mental health and stress in academia
Julian Friedrich, Anna Bareis, Moritz Bross, Zoé Bürger, Álvaro Cortés Rodríguez, Nina Effenberger, Markus Kleinhansl, Fabienne Kremer, Cornelius Schröder

all: University of Tübingen, Germany, and sustainAbility Ph.D. initiative at the University of Tübingen, Germany

## Analysis Code for the Mental Health Study at the University Tübingen

This work is now published at PLOSOne: <https://doi.org/10.1371/journal.pone.0288103>

(Link to the preprint: <https://doi.org/10.31234/osf.io/uq9w5>)

Data:
  - The coarsed quantitative data can be found in the `data` folder.
  - The complete dataset, including the responses to the open questions, can be found here:  https://doi.org/10.23668/psycharchives.12914 

## Folder structure:
- `preprocessing`: preprocessing file to get from the raw to the preprocessed data (this is dependent on raw data, which can not be shared).
- `data_analysis`: data analyis pipeline (dependent on preprocessed data). Apart from the faculty-wise comparison, all analysis can be run on the coarsed data.  For the reported sociodemographic statistics the results may slightly differ due to coarsing.
- `notebooks`: R notebook and HTML file explaining all preprocessed variables. The HTML file can be viewed [here](https://htmlpreview.github.io/?https://github.com/coschroeder/mental_health_analysis/blob/main/notebooks/data_view.nb.html) directly.

- `questionnaire`: raw questionnaire as exported from sosci survey.
