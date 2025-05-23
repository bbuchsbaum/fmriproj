Absolutely. An appendix detailing the mathematics will be crucial for clarity, reproducibility, and for others to build upon this work. Here's a sketch of what that mathematical appendix for the `fmriproj` proposal could look like, fleshing out the operations at each key stage.

---

**Appendix: Mathematical Formulation of the `fmriproj` Pipeline**

This appendix details the mathematical operations underlying each layer of the `fmriproj` pipeline.

**Notation:**

*   \(Y \in \mathbb{R}^{T \times V}\): The BOLD fMRI data matrix for a single run, where \(T\) is the number of time points (scans) and \(V\) is the total number of voxels in the brain (or mask).
*   \(Y_{sl} \in \mathbb{R}^{T \times V_{sl}}\): The BOLD fMRI data matrix for a specific searchlight (sl), where \(V_{sl}\) is the number of voxels in that searchlight.
*   \(S \in \mathbb{R}^{T \times N}\): The "stick function" matrix, where \(N\) is the number of trials/events in the run. Each column \(S_n\) is a vector of zeros with a 1 at the onset time of trial \(n\). Parametric modulators can be incorporated by scaling these sticks.
*   \(\theta\): A vector of global HRF shape parameters (e.g., time-to-peak, dispersion width scaling).
*   \(B(\theta) \in \mathbb{R}^{L \times K}\): The HRF basis matrix, where \(L\) is the length of the HRF in samples, and \(K\) is the number of HRF basis functions (e.g., \(K=3\) for canonical + time + dispersion derivatives). Its shape depends on \(\theta\).
*   \(X(\theta) \in \mathbb{R}^{T \times (N \cdot K)}\): The full trial-wise design matrix. It's formed by convolving each trial's stick function with each of the \(K\) basis functions in \(B(\theta)\). If \(S_n\) is the stick for trial \(n\) and \(B_k(\theta)\) is the \(k\)-th HRF basis vector (length \(L\)), then the columns of \(X(\theta)\) corresponding to trial \(n\) are \([S_n * B_1(\theta), S_n * B_2(\theta), \ldots, S_n * B_K(\theta)]\), where \(*\) denotes discrete convolution (typically implemented as matrix multiplication with a Toeplitz matrix representation of \(B_k(\theta)\)).
*   \(Q(\theta) \in \mathbb{R}^{T \times M}\), \(R(\theta) \in \mathbb{R}^{M \times M}\): Matrices from the thin QR decomposition of \(X(\theta)\), such that \(X(\theta) = Q(\theta)R(\theta)\), where \(Q(\theta)^T Q(\theta) = I_M\) and \(R(\theta)\) is upper triangular. \(M = N \cdot K\) is the number of columns in \(X(\theta)\).
*   \(\lambda_g \ge 0\): Global ridge regularization parameter.
*   \(\lambda_{sl} \ge 0\): Searchlight-specific adaptive ridge regularization parameter.
*   \(Z_{sl} \in \mathbb{R}^{M \times V_{sl}}\): Matrix of projected raw "beta-like" coefficients for the searchlight, before HRF basis collapse, where \(M = N \cdot K\).
*   \(w_{sl} \in \mathbb{R}^{K}\) or \(\mathbb{R}^{K \times V_{sl}}\): HRF basis collapse weights for the searchlight.
*   \(A_{sl} \in \mathbb{R}^{N \times V_{sl}}\): Final trial-specific feature patterns for the searchlight.
*   \(y \in \{1, \ldots, C\}^{N}\): Trial labels for \(C\) classes.
*   \(v_{sl} \in \mathbb{R}^{V_{sl}}\): Classifier weights for the searchlight (for a linear classifier).

---

**Layer 1: Sparse Trialwise Design Matrix \(X(\theta)\)**

*   **Input:** `fmrireg_event_model` (providing onsets, parametric modulators, durations for \(S\)), `hrf_basis_func`, `theta_params` (\(\theta\)).
*   **Operation:**
    1.  Generate \(B(\theta)\) using `hrf_basis_func(theta_params, time_vector)`.
    2.  For each trial \(n = 1, \ldots, N\):
        *   Create its stick function \(S_n\) (potentially scaled by parametric modulators and accounting for duration if block-like).
        *   For each HRF basis \(k = 1, \ldots, K\):
            *   Compute \(X_{n,k} = S_n * B_k(\theta)\).
    3.  Assemble \(X(\theta) = [X_{1,1}, \ldots, X_{1,K}, X_{2,1}, \ldots, X_{N,K}]\) as a sparse matrix.
*   **Output:** \(X(\theta) \in \mathbb{R}^{T \times (N \cdot K)}\).

**Layer 2: Global Projector Construction & Adaptive Ridge Projection**

**2.1 Global Projector Components (once per \(\theta\) update):**

*   **Input:** \(X(\theta)\) from Layer 1, global ridge `lambda_g`.
*   **Operation (via `fmriproj::build_projector`):**
    1.  Compute thin QR decomposition: \(X(\theta) = Q(\theta)R(\theta)\).
        *   \(Q_g = Q(\theta)\)
        *   \(R_g = R(\theta)\)
    2.  If \(\lambda_g > 0\):
        *   The global projector \(K_g = (R_g^T R_g + \lambda_g I_M)^{-1} R_g^T Q_g^T\).
        This can be computed efficiently as \(K_g = \text{solve}(R_g^T R_g + \lambda_g I_M, R_g^T Q_g^T)\).
    3.  If \(\lambda_g = 0\):
        *   The global projector \(K_g = Q_g^T\). (This simplifies to `solve(R_g, Q_g^T Y_sl)` later if only \(Q_g^T Y_{sl}\) is projected without `R_g^{-1}`).
*   **Output:** `list(Qt_g = Q_g^T, R_g = R_g, K_g = K_g)` (where `K_g` might just be `Qt_g`).

**2.2 Searchlight-Specific Adaptive Projection (per searchlight):**

*   **Input:** `Y_sl`, global components `Qt_g`, `R_g`, `lambda_adaptive_method`, `lambda_floor_global`, `X_theta_for_EB_residuals`.
*   **Operation (via `fmriproj::adaptive_ridge_projector`):**
    1.  **Determine effective lambda \(\lambda_{eff,sl}\):**
        *   If `lambda_adaptive_method == "none"`: \(\lambda_{eff,sl} = \lambda_{floor\_global}\).
        *   If `lambda_adaptive_method == "EB"` (Empirical Bayes):
            *   OLS estimate for this SL (using unregularized global \(Q_g, R_g\)):
                \(\hat{\beta}_{ols,sl} = R_g^{-1} (Q_g^T Y_{sl})\). (Computed efficiently).
            *   Residuals for this SL: \(E_{sl} = Y_{sl} - X(\theta) \hat{\beta}_{ols,sl}\). Requires `X_theta_for_EB_residuals`.
            *   Noise variance estimate: \(s_{n,sl}^2 = \frac{1}{(T - M) \cdot V_{sl}} \|E_{sl}\|_F^2\).
            *   "Signal" variance estimate (simplified): \(s_{b,sl}^2 = \frac{1}{M \cdot V_{sl}} \|\hat{\beta}_{ols,sl}\|_F^2\). (This part of EB can vary; a more sophisticated EB approach might use diagonal elements of \((R_g^T R_g)^{-1}\) to account for parameter uncertainties).
            *   Searchlight lambda: \(\lambda_{sl,EB} = \frac{s_{n,sl}^2}{s_{b,sl}^2}\) (the exact scaling may vary; some formulations include additional factors like \(\frac{M}{T-M}\) or other corrections).
            *   \(\lambda_{eff,sl} = \max(\lambda_{floor\_global}, \lambda_{sl,EB})\).
        *   If `lambda_adaptive_method == "LOOcv_local"`: Perform internal K-fold CV within the trials relevant to `Y_sl` to choose \(\lambda_{sl,CV}\) from a small grid, minimizing prediction error on \(Y_{sl}\).
            *   \(\lambda_{eff,sl} = \max(\lambda_{floor\_global}, \lambda_{sl,CV})\).
    2.  **Construct local projector \(K_{sl}\):**
        *   \(K_{sl} = (R_g^T R_g + \lambda_{eff,sl} I_M)^{-1} R_g^T Q_g^T\).
        **Note on regularization interaction:** The `projector_components` contains `Qt_g` and `R_g` from the unregularized QR decomposition of \(X(\theta)\). The `lambda_floor_global` provides a baseline regularization level, while `λ_sl_adaptive` allows for additional searchlight-specific regularization. The effective regularization is \(\lambda_{eff,sl} = \max(\lambda_{floor\_global}, \lambda_{sl,adaptive})\), ensuring that the adaptive method can only increase (never decrease) the regularization beyond the global floor.
    3.  **Project:** \(Z_{sl\_raw} = K_{sl} Y_{sl}\).
*   **Output:** `list(Z_sl_raw \in \mathbb{R}^{(N \cdot K) \times V_{sl}}, diag_data = list(lambda_sl_chosen = \lambda_{eff,sl}))`.

**Layer 3: HRF \(\beta\) Collapse & Optional Local \(w_{sl}\) Optimization**

*   **Input:** \(Z_{sl\_raw}\) from Layer 2, `N_trials`, `K_hrf_bases`, `method` (`"rss"`, `"pc"`, `"optim"`), `labels_for_w_optim`, `classifier_for_w_optim`.
*   **Operation (via `fmriproj::collapse_beta`):**
    1.  Reshape \(Z_{sl\_raw}\) to \(Z'_{sl} \in \mathbb{R}^{N \times K \times V_{sl}}\). (Conceptually, data is stored as \(N \times (K \cdot V_{sl})\) or similar for efficiency).
    2.  **Determine collapse weights \(w_{sl} \in \mathbb{R}^K\):**
        *   If `method == "rss"`: The "collapse" is \(A_{sl,nv} = \sqrt{\sum_{k=1}^{K} (Z'_{sl,nkv})^2}\). The weights \(w_{sl}\) are implicit and computed dynamically as \(w_{sl,k}^{(n,v)} = \frac{Z'_{sl,nkv}}{A_{sl,nv}}\) for each trial-voxel pair.
        *   If `method == "pc"`:
            *   Reshape/stack to \( (N \cdot V_{sl}) \times K \) matrix.
            *   Compute \(K \times K\) covariance matrix \(C_Z\).
            *   \(w_{sl}\) = principal eigenvector of \(C_Z\).
        *   If `method == "optim"`:
            *   Initialize \(w_{sl}\) (e.g., \([1, 0, \ldots, 0]^T\)).
            *   Iteratively update \(w_{sl}\) using L-BFGS (e.g., 3-5 steps):
                *   Current features for classifier: \(A_{sl}^{(iter)}[n,v] = \sum_{k=1}^{K} Z'_{sl,nkv} \cdot w_{sl,k}^{(iter)}\).
                *   Classifier loss: \(\mathcal{L}(v_{sl}, w_{sl}^{(iter)}) = \text{Loss}(y, \text{classifier}(A_{sl}^{(iter)}, v_{sl}))\). (Classifier weights \(v_{sl}\) are held fixed from main MVPA loop or re-estimated quickly).
                *   Gradient: \(\nabla_{w_{sl}} \mathcal{L}\).
                *   Update \(w_{sl}^{(iter+1)}\) and re-normalize \(\|w_{sl}^{(iter+1)}\|_2 = 1\).
    3.  **Collapse:** For each trial \(n\) and voxel \(v\):
        \(A_{sl,nv} = \sum_{k=1}^{K} Z'_{sl,nkv} \cdot w_{sl,k}\) (for `"pc"` and `"optim"` methods).
        For `"rss"` method: \(A_{sl,nv} = \sqrt{\sum_{k=1}^{K} (Z'_{sl,nkv})^2}\).
*   **Output:** `list(A_sl \in \mathbb{R}^{N \times V_{sl}}, w_sl_final, diag_data = list(beta_components_sl = Z'_{sl}, ...))`.

**Layer 4: Progressive Projection Pursuit (PP)**

*   **Input:** `A_sl_train \in \mathbb{R}^{N_{train} \times V_{sl}}`, `labels_train`, `method` (`"LDA"`, `"PLS-DA"`), `dims`.
*   **Operation (via `fmriproj::fit_pp` within `rMVPA` CV fold):**
    1.  If `method == "LDA"`:
        *   Compute within-class scatter \(S_W\) and between-class scatter \(S_B\) from `A_sl_train`.
        *   Solve generalized eigenvalue problem \(S_B W_z = \Lambda S_W W_z\).
        *   \(W_z \in \mathbb{R}^{V_{sl} \times dims}\) are the eigenvectors corresponding to the largest eigenvalues.
    2.  If `method == "PLS-DA"`:
        *   Perform PLS regression predicting class indicator matrix from `A_sl_train`.
        *   \(W_z\) are the PLS weight vectors.
*   **Output:** `W_z \in \mathbb{R}^{V_{sl} \times dims}`. Prediction is `A_sl_reduced = A_{sl} W_z \in \mathbb{R}^{N \times dims}`.

**Layer 5: `rMVPA` Classifier**

*   **Input:** `A_sl` (or `A_sl_reduced`) from Layer 3 (or 4), trial `labels`.
*   **Operation:** Standard `rMVPA` classifier training (e.g., SVM, correlation-based RSA) and prediction within its cross-validation framework.
    *   Example Linear SVM: \(\min_{v_{sl}, b} \frac{1}{2} \|v_{sl}\|^2 + C \sum \xi_i\) s.t. \(y_i(v_{sl}^T A_{sl,i} + b) \ge 1 - \xi_i, \xi_i \ge 0\).
*   **Output:** Classifier accuracy, decision values, etc.

**Global \(\theta\) Optimization (via `fmriproj::optimize_joint_hrf_mvpa`)**

*   **Objective:** \(\min_{\theta} \sum_{folds} \text{CrossValidatedLoss}(\text{Classifier}(A_{sl}(\theta, w_{sl}(\theta)), v_{sl}(\theta)), y_{test\_fold})\).
*   **Mechanism:**
    1.  Outer loop iterates `stats::optim` on \(\theta\).
    2.  For each \(\theta\) candidate:
        *   `make_trialwise_X(..., theta_params = theta)` (Layer 1).
        *   `build_projector(X_theta, ...)` (Layer 2 global).
        *   Inner loop (e.g., `future_lapply` over searchlights):
            *   `Y_sl = grab_block(...)`.
            *   `adaptive_ridge_projector(Y_sl, Qt, R, ...)` -> `Z_sl_raw`.
            *   `collapse_beta(Z_sl_raw, ...)` -> `A_sl`, `w_sl`.
            *   `rMVPA` internal cross-validation on `A_sl` using `classifier_for_loss_fn` to get loss for this searchlight.
        *   Aggregate loss across searchlights.
    3.  `optim` updates \(\theta\) based on total loss (and gradient if available).
        *   Numerical gradient for \(\theta\): \(\frac{\text{Loss}(\theta + \delta) - \text{Loss}(\theta - \delta)}{2\delta}\).
        *   Analytic gradient (TMB): Requires HRF basis function and entire pipeline to be TMB-differentiable. Cost involves chain rule through QR and subsequent steps. Re-factorization of \(X(\theta)\) is often a practical approximation for \(\nabla_\theta Q(\theta)\).

---

This appendix should provide the necessary mathematical rigor. The details of the Empirical Bayes for \(\lambda_{sl}\) and the exact form of the gradient for supervised \(w_{sl}\) optimization would be further elaborated with specific equations in a full paper.