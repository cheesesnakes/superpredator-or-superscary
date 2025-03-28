# Metadata for Data Files in meta-analysis/data

1. `animal_traits.csv`:
   - Description: Contains information on animal traits such as size, trophic level, and feeding guild.
   - Columns: `species`, `size`, `trophic_level`, `feeding_guild`

2. `authors.csv`:
   - Description: Exported list of authors and citations from the literature review.
   - Columns: `author_name`, `affiliation`, `citation_count`

3. `chapter-1_covariates.csv`:
   - Description: Contains information on covariates used in the analysis, such as location and habitat.
   - Columns: `study_id`, `location`, `habitat`, `covariate_type`

4. `chapter-1_effects.csv`:
   - Description: Contains information on effect sizes calculated for each study.
   - Columns: `study_id`, `effect_size`, `variance`, `confidence_interval`

5. `chapter-1_full-text-screening.csv`:
   - Description: Contains information on articles that passed the full-text screening process.
   - Columns: `article_id`, `title`, `screening_status`

6. `chapter-1_study-info.csv`:
   - Description: Contains information on study characteristics such as study design, sampling methods, focal species, and human interaction type.
   - Columns: `study_id`, `design`, `sampling_method`, `focal_species`, `interaction_type`

7. `clean_data.csv`:
   - Description: The final exported tidy dataframe used in the meta-analysis.
   - Columns: `study_id`, `species`, `effect_size`, `covariates`

8. `effect-size.csv`:
   - Description: Contains the computed effect sizes for each study.
   - Columns: `study_id`, `effect_size`, `standard_error`

9. `populations.csv`:
   - Description: A cleaned list of focal species along with information on trophic level and feeding guild.
   - Columns: `species`, `trophic_level`, `feeding_guild`

10. `size.csv`:
    - Description: Contains information on the size of the focal species.
    - Columns: `species`, `size`