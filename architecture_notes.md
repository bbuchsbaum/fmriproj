# fmriproj-rMVPA Integration Architecture

## Overview

The integration between fmriproj and rMVPA has been redesigned to use a more elegant and maintainable architecture that avoids the anti-pattern of giant switch statements.

## Key Components

### 1. ProjectedDataSource (R6 Proxy Class)

Located in: `R/projected_data_source.R`

This is the cornerstone of the new architecture. It's an R6 class that acts as a transparent proxy between rMVPA models and the projected data.

**Key features:**
- Lazy evaluation: projection happens only when data is accessed
- Caching: computed projections are cached for performance
- Transparent interface: implements S3 methods like `as.matrix()`, `[`, `dim()` etc.
- Supports both matrix and function-based projections
- Preserves metadata (row/column names, attributes)

**Why it's elegant:**
- rMVPA models are completely unaware they're working with projected data
- No modification to rMVPA code required
- Memory efficient through lazy evaluation

### 2. S3 Dispatch Model Factory

Located in: `R/model_factory.R`

Replaces the giant switch statement with elegant S3 method dispatch.

**Key functions:**
- `model_spec()`: Creates a model specification with appropriate class
- `create_projected_model()`: S3 generic that dispatches based on model type
- Individual S3 methods for each model type (classification, regression, RSA variants)

**Why it's better:**
- Open/Closed Principle: Easy to add new model types without modifying existing code
- Each model type is handled by its own method
- Clear separation of concerns
- Type-safe through S3 class system

### 3. Integration Flow

```
User creates model_spec
    ↓
create_projected_model (S3 dispatch)
    ↓
create_projection_proxy (creates ProjectedDataSource)
    ↓
build_rmvpa_model (calls actual rMVPA constructor)
    ↓
rMVPA model with transparent projection
```

## Design Principles

1. **Transparency**: rMVPA should be unaware of projection
2. **Lazy Evaluation**: Don't compute until necessary
3. **Extensibility**: Easy to add new model types or projection methods
4. **Type Safety**: Use R's S3 system for dispatch
5. **Memory Efficiency**: Cache when beneficial, compute on-demand when not

## Adding New Model Types

To add support for a new rMVPA model type:

1. Add the type to `model_spec()` function's type argument
2. Create a new S3 method: `create_projected_model.newtype()`
3. Add the corresponding case in `build_rmvpa_model()`
4. Export the S3 method in NAMESPACE

That's it! No modification to existing code required.

## Testing Strategy

The architecture is tested at multiple levels:

1. **Unit tests for ProjectedDataSource** (`test-projected-data-source.R`)
   - Proxy behavior
   - Lazy evaluation
   - Caching
   - Metadata preservation

2. **Integration tests for model factory** (`test-model-factory.R`)
   - S3 dispatch
   - Model creation
   - Projection application

3. **End-to-end tests with rMVPA** (`test-rmvpa-all-models.R`)
   - Actual integration with rMVPA models
   - All model types
   - Various projection scenarios

## Benefits Over Previous Architecture

1. **Maintainability**: No more giant switch statements
2. **Extensibility**: Adding new models is trivial
3. **Performance**: Lazy evaluation and caching
4. **Elegance**: Clean separation of concerns
5. **Robustness**: Type-safe S3 dispatch
6. **Transparency**: rMVPA remains blissfully unaware

## Memory Management Considerations

### Caching Strategy

The `ProjectedDataSource` implements lazy evaluation with caching:

1. **Initial state**: No projection computed, minimal memory footprint
2. **First access**: Projection computed and cached internally
3. **Subsequent access**: Returns cached result (fast, but uses memory)

### Memory Implications

**Potential Issue**: If users keep multiple model objects with large cached projections, memory usage can accumulate:

```r
# Each model holds its cached projection in memory
model1 <- create_projected_model(spec1, proj1)  # Caches projection 1
model2 <- create_projected_model(spec2, proj2)  # Caches projection 2
# Both projections remain in memory
```

### Managing Memory

The `clear_cache()` method allows explicit memory management:

```r
# After analysis is complete
model$dataset$clear_cache()  # Frees the cached projection

# Or in a pipeline
result <- model %>%
  run_analysis() %>%
  extract_results()
  
model$dataset$clear_cache()  # Clean up after use
```

### Best Practices

1. **For single analyses**: Let caching work automatically
2. **For multiple analyses**: Consider clearing cache between runs
3. **For memory-constrained environments**: Clear cache immediately after extracting results
4. **For production pipelines**: Implement automatic cache clearing in wrapper functions

## Implementation Details

### Key Audit Findings

From auditing rMVPA's codebase, the following matrix operations are used:
- Basic: `dim()`, `nrow()`, `ncol()`, `length()`
- Names: `rownames()`, `colnames()`, `dimnames()`
- Type checks: `is.matrix()`, `is.numeric()`, `class()`
- Access: `[` subsetting, `head()`, `str()`
- Special: `t()` for transpose (used in RSA models)

All these are now implemented as S3 methods for `ProjectedDataSource`.

### Data Orientation Confirmation

The analysis confirmed that ALL rMVPA models expect data in the same orientation:
- **Input**: samples (trials) × features (voxels)
- **RSA note**: The `t(train_dat)` in standard RSA is for computing trial-by-trial correlations, not a different input format

This means our proxy works consistently across all model types.

## Future Improvements

1. **Streaming projections**: For very large datasets, implement streaming/chunked projections
2. **GPU acceleration**: ProjectedDataSource could use GPU for matrix multiplication
3. **Distributed computing**: Proxy could distribute projection across nodes
4. **Smart caching**: Implement LRU cache for multiple projections
5. **Memory monitoring**: Add methods to report cache size and memory usage

The architecture is designed to accommodate these improvements without breaking changes.