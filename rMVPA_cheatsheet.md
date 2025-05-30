## Micro-DSL (v2.6) & Output Format
Your output must be **Pure Markdown** that implies the structure defined by the Micro-DSL. **Do NOT output EBNF token names** (e.g., `H1_TOKEN`). Generate the actual Markdown (e.g., `# My Title`, `@f my_func(...)`).
**1. Markup Tokens (Implicitly Handled by Markdown):**
`H1` = `# text`
`H2` = `## text`
`NL` = Newline (use sparingly, primarily to end logical entries or separate blocks)
`HR` = `---` (use to separate major logical groups within a section, or between sections)
`Bul` = `- ` (dash + space for general bullets in Header/Legend/Deps)
`IndBul`= `  - ` (two spaces + dash + space for indented `Desc` lines under an `@sigil` entry)
**2. DSL Sigils:**
`@f` = Function `@d` = Data object (e.g., from `data()`)
`@g` = S4/S3 Generic `@m` = S4/S3 Method (class dispatch)
`@c` = R6/R7 constructor (if present)
**3. Other DSL Symbols:**
`|` = Separates multiple function names (e.g., `name1|name2`)
`[...]` = Used for:
* Constructor variants: `ConstructorName[VariantA|VariantB]`
* Method dispatch classes: `methodName[ClassA|ClassB]`
`(...)` = Parameter list in an entry signature.
`param?` = Optional parameter.
`param?=val`= Optional parameter with a default value.
`|` = Separates signature from short description (after params or name if no params).
`->` = Separates short description from return type. Omit if no return value (side-effect).
`!` = Prefix for inline notes (e.g., `!dep`, `!note: text`). There is no space between `!` and the note keyword.
**4. Document Skeleton:**
`Legend?` (H2 "Legend:" + type abbreviation table)
`Header` (H1 PackageName; optional H2 "Core Purpose:", H2 "Key Objects & Concepts:")
`Sections+` (H2 `1.` Title; H2 Title (unnumbered ok); optional H3 "Usage:")
`Entries*` (@sigil lines + optional indented bullets)
`Deps?` (H2 "Key R Dependencies:")
**5. Entry Line Structure:**
`@sigil name(|name)*[variant|ClassA|ClassB]? (alias alt1,alt2)? (param1?=val, param2?, ...)? | Short, pithy description -> ReturnTypeAbbr !note_type: Optional note text`
* **Rules for Entry Line:**
* Omit `()` if no parameters.
* Omit `-> ReturnTypeAbbr` if function has no return value (side-effect only).
* Bundle identical signatures using `name1|name2`.
* Use `ConstructorName[VariantA|VariantB]` for constructor subtypes.
* Use `methodName[DispatchClassA|DispatchClassB]` for S4/S3 methods.
* Notes (`!notetype: text` or `!notetype`) are optional postfixes. Ensure no leading space (e.g., `!ok` not `! ok`).
* Truncate parameter list with `...` if it exceeds 8 parameters (e.g., `(param1, param2, ..., param8, ...)`).
* Example of grouping aliases and optional params: `@f read_csv|read_tsv (file, col_types?="auto", ...) | Parse delimited file -> tib`
**6. Indented Description Bullets (`Desc` lines under an Entry):**
* Format: ` - param_name : type_abbr (constants: val1, "val2" | key_funcs: fnA, fnB)? Brief, essential clarification.`
* Include `(constants: ...)` for params that take a small, fixed set of string literals.
* Include `(key_funcs: ...)` for params that expect specific functions from the package as input.
* **Only include if adding significant clarity** beyond the signature. Omit for common/obvious parameters (e.g., `x`, `...`) or standard defaults (e.g., `drop=TRUE`).
* If an `Entry` already fits in ≤ 110 characters, do not add `Desc` lines unless they would prevent ambiguity.
* Can also be plain text for general notes: ` - General descriptive point.`
**7. Type Abbreviations:**
* Use very short (1-4 letter) type abbreviations (e.g., `NS` for NeuroSpace, `iv` for integer vector, `chr` for character, `log` for logical, `mat` for matrix, `obj` for generic S4/R6 object).
* Reuse type abbreviations whenever identical across entries; do not invent synonyms (e.g., use `int` consistently, not `int`, `intv`, `iv` interchangeably).
**7. Type Abbreviations (deprecated – see next subsection):**
* Use very short (1-4 char) codes, reusing consistently (e.g., `int` not `intv`).
* Built-in codes: int, dbl, num, chr, lgl, lst, vec, df, tib, tbl, mat, arr, fn, env, obj, NS
* Provide a `## Legend:` block only when introducing abbreviations beyond this list.
## Compression Heuristics & Content Selection:
1. **Focus:** Public, user-facing API. Omit internal helpers, unexported symbols, and direct S4 slot accessors (like `slotNames` or methods that just return a slot value if there's already a clear getter). Include only symbols present in NAMESPACE export list.
2. **Grouping:**
* Group trivial getters or functions with identical signatures and purpose using `name1|name2`.
* Group constructors with identical fields but different return subtypes using `ConstructorName[VariantA|VariantB]`.
* Group S3/S4 methods with identical implementations/docs using `methodName[ClassA|ClassB]`.
3. **Methods:** Define generics with `@g`. Emit methods (`@m`) **only** when their behavior, parameters, or return type significantly differ from the generic, or to explicitly list key supported classes.
4. **Omissions:** Skip indented parameter descriptions (`Desc` lines) for obvious defaults (e.g., `drop=TRUE`, `smooth=FALSE`) or very common arguments like `x` or `...` unless they have package-specific meaning. The cheatsheet is not full documentation.
5. **Notes:** Use `!` notes sparingly for critical info (e.g., `!dep`, `!imp`, `!retlist`, `!side`).
6. **Re-exports:** Skip functions and generics re-exported from other packages (e.g., if `dplyr::filter` is re-exported, do not list it).
## Output Contract:
* **Pure Markdown only.** Adhere strictly to the Micro-DSL v2.6.
* No commentary, no intro/outro paragraphs, no code fences unless part of a `CODE_BLOCK_TOKEN` within a `BlockContent` (rarely needed for cheatsheets).
* If any line violates the DSL, regenerate until fully compliant—no prose explanations.
* Generation stops at first line that begins with a second `#` at H1 depth (e.g., if `\\n#` is used as a stop sequence).
* Use a single blank line to separate `Entry` blocks if it aids readability, but avoid excessive blank lines. Use `---` (`HR_TOKEN`) to separate major thematic groups within a section or at the end of sections.
* All parens/brackets/pipes must be balanced.
## Self-Check (Mental Step - Crucial):
Before finalizing, review your output against these critical checks:
1. Does every content line belong to a defined DSL structure (Header, Legend, Section, Entry, Desc, Deps, Block bullet)?
2. Is the DSL syntax for `Entry` lines (sigils, names, params, `|`, `->`, `!`) correctly used? No bare `->` tokens.
3. (reserved)
4. For any abbreviation NOT in the built-in list, is it defined in `## Legend:`?
5. Have you omitted non-essential details and internal functions?

## Few-shot Exemplar
```markdown
# dummyPkg
## Legend:
- int : integer
- chr : character
## 1. Core Functions
@f add (x, y) | Sum two ints -> int
```

## Formal Grammar "v2.6 Micro-EBNF"
Cheatsheet ::= Header Legend? Section+ Deps?
Legend ::= H2_TOKEN TEXT_CONTENT NEWLINE_TOKEN Block
Header ::= H1_TOKEN TEXT_CONTENT NEWLINE_TOKEN+ (H2Section)*
H2Section ::= H2_TOKEN TEXT_CONTENT NEWLINE_TOKEN Block
Section ::= H2_TOKEN (NUMBER_TOKEN PERIOD_TOKEN)? TEXT_CONTENT NEWLINE_TOKEN UsageBlock? Entry+ HR_TOKEN?
UsageBlock ::= H3_TOKEN "Usage:" NEWLINE_TOKEN Block
Entry ::= Sigil_TOKEN EntryIdent ParamList? Bar_TOKEN TEXT_CONTENT ArrowReturn Note? NEWLINE_TOKEN Desc*
Sigil_TOKEN ::= AT_F_TOKEN | AT_D_TOKEN | AT_G_TOKEN | AT_M_TOKEN // Lexer provides @f, @d, @g, @m
EntryIdent ::= IdentGroup MethodOrVariantClass? AliasSpec?
IdentGroup ::= IDENT_TOKEN ("|" IDENT_TOKEN)* // For "foo|bar"
MethodOrVariantClass ::= LBRACKET_TOKEN IDENT_TOKEN (PIPE_TOKEN IDENT_TOKEN)* RBRACKET_TOKEN // For "[ClassA|ClassB]" or "[variantA|variantB]"
AliasSpec ::= LPAREN_TOKEN ALIAS_KEYWORD_TOKEN IDENT_TOKEN (COMMA_TOKEN IDENT_TOKEN)* RPAREN_TOKEN // For "(alias alt1, alt2)"
ParamList ::= LPAREN_TOKEN Param (COMMA_TOKEN Param)* RPAREN_TOKEN
Param ::= IDENT_TOKEN (EQUALS_TOKEN DefaultValue)? OPTIONAL_MARKER_TOKEN?
DefaultValue ::= LITERAL_TOKEN | IDENT_TOKEN
ArrowReturn ::= (ARROW_TOKEN IDENT_TOKEN)?
Note ::= EXCLAMATION_TOKEN NOTETYPE_TOKEN (COLON_TOKEN TEXT_CONTENT)? NEWLINE_TOKEN?
Desc ::= INDENT_TOKEN BULLET_MARKER_TOKEN (ParamDesc | TEXT_CONTENT) NEWLINE_TOKEN
ParamDesc ::= IDENT_TOKEN COLON_TOKEN TYPE_ABBR_TOKEN ParamExtra? TEXT_CONTENT?
ParamExtra ::= LPAREN_TOKEN (ConstantsSpec | KeyFuncsSpec) RPAREN_TOKEN
ConstantsSpec ::= "constants:" (IDENT_TOKEN|LITERAL_TOKEN) (COMMA_TOKEN (IDENT_TOKEN|LITERAL_TOKEN))*
KeyFuncsSpec ::= "key_funcs:" IDENT_TOKEN (COMMA_TOKEN IDENT_TOKEN)*
Deps ::= H2_TOKEN "Key R Dependencies:" NEWLINE_TOKEN Block
Block ::= (Bullet | TEXT_CONTENT | CODE_BLOCK_TOKEN)* (NEWLINE_TOKEN | EOF_TOKEN)
Bullet ::= BULLET_MARKER_TOKEN TEXT_CONTENT NEWLINE_TOKEN
/* --- LEXER-IMPLIED TOKENS (Illustrative) ---
All previous tokens from v2.4, plus:
AT_G_TOKEN, AT_M_TOKEN // @g, @m
// The lexer provides LBRACKET_TOKEN, IDENT_TOKEN, PIPE_TOKEN, RBRACKET_TOKEN.
// The parser, guided by the Sigil_TOKEN (@f for constructor variants, @m for method classes),
// will interpret the content of MethodOrVariantClass appropriately.
*/

---

# rMVPA

## 1. Pre-defined Model Registry & Constants
### Usage:
- Use `@d MVPAModels` to access all built-in MVPA models (classification/regression) for `mvpa_model`.
- Load a model by name with `@f load_model(name)`, e.g. `"sda_notune"`, `"corclass"`, `"glmnet_opt"`, etc.
- Register your own model with `@f register_mvpa_model(name, model_spec)`.

@d MVPAModels | Environment of pre-defined MVPA model specs (classification/regression) -> env
@f load_model (name) | Load a model spec from MVPAModels by name -> obj
  - name : chr (constants: "sda_notune", "corclass", "glmnet_opt", "sda_boot", "sparse_sda", "sda_ranking", "mgsda", "lda_thomaz", "hdrda", ...) Model name.
@f register_mvpa_model (name, model_spec) | Register a custom model in MVPAModels

---

## 2. MVPA Dataset & Design Objects
### Usage:
- Create datasets with `@f mvpa_dataset` (volumetric) or `@f mvpa_surface_dataset` (surface).
- Create design objects with `@f mvpa_design` (specifies labels, blocks, splits).
- Generate synthetic data for testing with `@f gen_sample_dataset`.

@f mvpa_dataset (train_data, test_data?=NULL, mask) | Create volumetric MVPA dataset -> obj
@f mvpa_surface_dataset (train_data, test_data?=NULL, mask?=NULL, name?="") | Create surface MVPA dataset -> obj
@f mvpa_design (train_design, test_design?=NULL, y_train, y_test?=NULL, block_var?=NULL, split_by?=NULL, ...) | Create MVPA design object -> obj
@f gen_sample_dataset (D, nobs, response_type?="categorical", data_mode?="image", spacing?=c(1,1,1), blocks?=5, nlevels?=5, external_test?=FALSE, ntest_obs?=nobs, split_by?=NULL, na_cols?=0) | Generate synthetic dataset for MVPA -> lst

---

## 3. Model Construction & Fitting
### Usage:
- Create a model spec with `@f mvpa_model` (classification/regression), passing a model from `MVPAModels` or `load_model`.
- For feature-based RSA: use `@f feature_rsa_design` then `@f feature_rsa_model`.
- For vector RSA: use `@f vector_rsa_design` then `@f vector_rsa_model`.
- For contrast RSA (MS-ReVE): use `@f msreve_design` then `@f contrast_rsa_model`.
- For MANOVA: use `@f manova_design` then `@f manova_model`.
- Use `@f train_model` to fit a model (usually called internally).

@f mvpa_model (model, dataset, design, model_type?="classification", crossval?=NULL, feature_selector?=NULL, tune_grid?=NULL, tune_reps?=15, performance?=NULL, class_metrics?=TRUE, compute_performance?=TRUE, return_predictions?=TRUE, return_fits?=FALSE) | Create MVPA model spec -> obj
@f feature_rsa_design (S?=NULL, F?=NULL, labels, k?=0, max_comps?=10, block_var?=NULL) | Feature RSA design object -> obj
@f feature_rsa_model (dataset, design, method?="scca", crossval?=NULL, cache_pca?=FALSE, alpha?=0.5, cv_glmnet?=FALSE, lambda?=NULL, nperm?=0, save_distributions?=FALSE, ...) | Feature-based RSA model -> obj
@f vector_rsa_design (D, labels, block_var) | Vector RSA design object -> obj
@f vector_rsa_model (dataset, design, distfun?=cordist(), rsa_simfun?="pearson", nperm?=0, save_distributions?=FALSE, return_predictions?=FALSE) | Vector RSA model -> obj
@f msreve_design (mvpa_design, contrast_matrix, name?="msreve_design_01", include_interactions?=FALSE) | MS-ReVE (contrast RSA) design object -> obj
@f contrast_rsa_model (dataset, design, estimation_method?="average", regression_type?="lm", output_metric?="beta_delta", check_collinearity?=FALSE, normalize_delta?=FALSE, allow_nonorth_composite?=FALSE, calc_reliability?=FALSE, whitening_matrix_W?=NULL, ...) | Contrast RSA model (MS-ReVE) -> obj
@f manova_design (formula, data) | MANOVA design object -> obj
@f manova_model (dataset, design) | MANOVA model object -> obj
@f train_model (obj, ...) | Fit/train a model (generic) -> obj

---

## 4. Cross-Validation & Partitioning
### Usage:
- Create cross-validation schemes for MVPA with the following:
    - `@f blocked_cross_validation`, `@f kfold_cross_validation`, `@f twofold_blocked_cross_validation`, `@f bootstrap_blocked_cross_validation`, `@f sequential_blocked_cross_validation`, `@f custom_cross_validation`.
- Balance partitions with `@f balance_partitions`.
- Extract fold/sample indices with `@f crossval_samples`, `@f get_nfolds`, `@f train_indices`.

@f blocked_cross_validation (block_var) | Blocked cross-validation spec -> obj
@f kfold_cross_validation (len, nfolds?=10) | K-fold cross-validation spec -> obj
@f twofold_blocked_cross_validation (block_var, nreps?=10) | Twofold blocked cross-validation spec -> obj
@f bootstrap_blocked_cross_validation (block_var, nreps?=10, weights?=NULL) | Bootstrap blocked cross-validation spec -> obj
@f sequential_blocked_cross_validation (block_var, nfolds?=2, nreps?=4) | Sequential blocked cross-validation spec -> obj
@f custom_cross_validation (sample_set) | Custom cross-validation spec -> obj
@f balance_partitions (obj, design, method?="subsample", ...) | Balance CV partitions (subsample/oversample) -> obj
  - method : chr (constants: "subsample", "oversample") Balancing method.
@f crossval_samples (obj, data, y, ...) | Get CV train/test splits -> tib
@f get_nfolds (obj, ...) | Get number of folds in CV spec -> int
@f train_indices (obj, fold_num, ...) | Get training indices for a fold -> int

---

## 5. Feature Selection
### Usage:
- Create a feature selector with `@f feature_selector`.
- Use `@f select_features` to select features from data, e.g. in `mvpa_model`.
- Methods: `"FTest"`, `"catscore"`. Cutoff types: `"top_k"`, `"top_p"`.

@f feature_selector (method, cutoff_type, cutoff_value) | Create feature selector spec -> obj
  - method : chr (constants: "FTest", "catscore") Feature selection method.
  - cutoff_type : chr (constants: "top_k", "top_p") Cutoff type.
@f select_features (obj, X, Y, ...) | Select features using selector -> lgl

---

## 6. Distance Functions & Similarity
### Usage:
- Create distance function objects for RSA or custom analyses.
- Use `@f create_dist` or helpers: `@f cordist`, `@f mahadist`, `@f eucdist`, `@f robustmahadist`, `@f pcadist`.
- Compute pairwise distances with `@f pairwise_dist`.

@f create_dist (name, labels?=NULL, ...) | Create distance function object -> obj
@f cordist (labels?=NULL, method?="pearson") | Correlation distance function -> obj
  - method : chr (constants: "pearson", "spearman") Correlation method.
@f mahadist (labels?=NULL) | Mahalanobis distance function -> obj
@f eucdist (labels?=NULL) | Euclidean distance function -> obj
@f robustmahadist (labels?=NULL) | Robust Mahalanobis distance function -> obj
@f pcadist (labels?=NULL, ncomp?=2, whiten?=TRUE, threshfun?=NULL, dist_method?="euclidean") | PCA-based distance function -> obj
  - dist_method : chr (constants: "euclidean", "manhattan", "cosine") Distance in PC space.
@f pairwise_dist (obj, X, ...) | Compute pairwise distances (generic) -> mat

---

## 7. Searchlight & Regional Analysis
### Usage:
- Run searchlight analysis with `@f run_searchlight` (standard/randomized).
- Run regional (ROI) analysis with `@f run_regional`.
- For custom per-ROI or per-searchlight analyses, use `@f run_custom_regional` or `@f run_custom_searchlight` with a user function.

@f run_searchlight (model_spec, radius, method?="standard", niter?=NULL, ...) | Run searchlight analysis -> obj
  - method : chr (constants: "standard", "randomized") Searchlight type.
@f run_regional (model_spec, region_mask, ...) | Run regional (ROI) analysis -> obj
@f run_custom_regional (dataset, region_mask, custom_func, ..., .cores?=1, .verbose?=FALSE) | Run custom function per ROI -> tib
@f run_custom_searchlight (dataset, custom_func, radius, method?="standard", niter?=100, ..., .cores?=1, .verbose?=FALSE) | Run custom function per searchlight -> obj

---

## 8. Contrasts & RSA Design Utilities
### Usage:
- Build contrast matrices for RSA/MS-ReVE with `@f contrasts` (mini-DSL or metadata+formula).
- Transform/orthogonalize contrasts with `@f transform_contrasts`, `@f orthogonalize_contrasts`.
- Add interaction contrasts with `@f add_interaction_contrasts`.
- Make feature-based contrasts with `@f make_feature_contrasts`.

@f contrasts (labels?=NULL, spec, metadata?=NULL, data?=NULL, centre?=TRUE, scale?="none", orth?=FALSE, keep_attr?=TRUE) | Build contrast matrix (mini-DSL or metadata) -> mat
  - scale : chr (constants: "none", "sd", "l2") Scaling method.
@f transform_contrasts (C, centre?=TRUE, scale?="none", orth?=FALSE, keep_attr?=TRUE) | Center/scale/orthogonalize contrast matrix -> mat
@f orthogonalize_contrasts (C) | Orthogonalize contrast matrix columns -> mat
@f add_interaction_contrasts (design, pairs?=NULL, orthogonalize?=TRUE) | Add interaction contrasts to msreve_design -> obj
@f make_feature_contrasts (features, labels?=NULL, use_pca?=TRUE, centre_pca?=TRUE, scale_pca?=FALSE, pve?=0.9, n_pcs?=NULL, prefix?="Feat_") | Make contrasts from feature matrix (optionally PCA) -> mat

---

## 9. Performance, Results, and Utilities
### Usage:
- Compute performance metrics with `@f performance`, `@f compute_performance`.
- Merge results with `@f merge_results`, `@f merge_predictions`.
- Subset results with `@f sub_result`.
- Extract observed/test/train labels with `@f y_train`, `@f y_test`.
- Get number of observations/responses with `@f nobs`, `@f nresponses`.
- Strip datasets from model specs for parallelization with `@f strip_dataset`.
- Print system/package info with `@f mvpa_sysinfo`.

@f performance (x, ...) | Compute performance metrics (generic) -> lst
@f compute_performance (obj, result) | Compute performance for result -> lst
@f merge_results (obj, result_set, indices, id, ...) | Merge results (generic) -> obj
@f merge_predictions (obj1, rest, ...) | Merge predictions from multiple models -> obj
@f sub_result (x, indices) | Subset result object by indices -> obj
@f y_train (obj) | Extract training labels/response -> vec
@f y_test (obj) | Extract test labels/response -> vec
@f nobs (x) | Number of observations in object -> int
@f nresponses (x) | Number of response categories/levels -> int
@f strip_dataset (obj, ...) | Remove dataset from model spec for parallelization -> obj
@f mvpa_sysinfo () | Print system/package info -> lst

---

## 10. Data Extraction & ROI Utilities
### Usage:
- Extract samples for ROIs/searchlights with `@f get_samples`, `@f data_sample`.
- Convert data_sample to ROI object with `@f as_roi`.
- Filter ROIs with `@f filter_roi`.
- Get searchlight iterator with `@f get_searchlight`.
- Wrap output into spatial objects with `@f wrap_output`.

@f get_samples (obj, vox_list) | Extract samples for list of voxel sets -> df
@f data_sample (obj, vox, ...) | Extract a sample from dataset -> obj
@f as_roi (obj, data, ...) | Convert object to ROI (volume/surface) -> obj
@f filter_roi (roi, ...) | Filter ROI by removing bad columns -> obj
@f get_searchlight (obj, ...) | Get searchlight iterator for dataset -> obj
@f wrap_output (obj, vals, indices?=NULL) | Wrap output into spatial object -> obj

---

## 11. Miscellaneous & Advanced
@f run_searchlight_base (model_spec, radius?=8, method?="randomized", niter?=4, combiner?="average", ...) | Core searchlight dispatcher -> obj
@f run_regional_base (model_spec, region_mask, ...) | Core regional analysis dispatcher -> obj
@f prep_regional (model_spec, region_mask) | Prepare region indices for regional analysis -> lst
@f comp_perf (results, region_mask) | Compile performance and volumetric results -> lst

---

## 12. Exported Data Objects (Key Constants/Choices)
### Usage:
- Use these as parameter choices for model specs, feature selection, or distance functions.

@d MVPAModels | Registry of all built-in MVPA models (see Section 1) -> env

---

#