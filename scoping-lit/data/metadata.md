# Metadata for Scoping Literature Data

## `scoping_search.csv`
- **Description**: This file contains the results of the pilot search exported from Web of Science. It includes metadata for articles identified during the scoping phase of the systematic review.
- **Columns**:
  - `article_id`: Unique identifier for each article.
  - `title`: Title of the article.
  - `authors`: List of authors for the article.
  - `journal`: Journal where the article was published.
  - `year`: Year of publication.
  - `keywords`: Keywords associated with the article.
  - `abstract`: Abstract of the article.
  - `screening_status`: Status of the article after initial screening (e.g., included, excluded).

## Notes
- The `scoping_search.csv` file is used as input for scripts in the `scoping-lit` folder to generate word clouds, clean data, and filter articles for full-text screening.
- Ensure that the file is updated with the latest search results before running the analysis scripts.