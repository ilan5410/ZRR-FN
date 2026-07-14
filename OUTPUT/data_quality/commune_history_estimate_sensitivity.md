# Commune-History Exclusion Sensitivity

This exercise excludes communes flagged by the official INSEE movement-history audit from the preferred adjusted first-difference specification. It is deliberately conservative: it does not impute a one-to-many historical correspondence or reallocate commune outcomes.

```text
                         sample observations communes_excluded_by_history_flag
            Full audited sample        19212                                 0
 Exclude official-history flags        19165                              1860
     estimate standard_error      p_value
 -0.006716998    0.001483732 6.016092e-06
 -0.006716385    0.001485748 6.205311e-06
```

Interpretation: stability under this exclusion is evidence against a result driven solely by the flagged codes, but is not proof that every historical merge is harmonized.
