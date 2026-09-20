# statcheck-ml port kit

This kit holds the rules and the trained model a statcheck-ml port needs: the prefilter, the character vocabulary, the p-value constants, and one or more shipped models. The mother repository, statcheck-ml, writes it.

Do not edit a file in this kit here. Change it in the mother repository and export the kit again, or this copy will drift from the other ports.

Kit version: 2.0.0
Mother repository: https://github.com/rasoulnorouzi/ml-statcheck
Mother commit: 49f64536fb66a5952e59be2cefa35a2788f298f0

## Models

| config | dev F1 | holdout F1 | holdout 95% CI |
|---|---|---|---|
| gru-crf | 0.927 | 0.904 | [0.871, 0.934] |

## Verification

`manifest.json` lists every other file in this kit with its sha256 hash. A port checks each hash before it trusts the kit. A text file is hashed with its line endings folded to LF first, so the hash does not depend on the platform that checked the file out.

Verify from the mother repository:

    python pipeline/08_port_kit.py --verify <kit_dir>

## Regeneration

    python pipeline/08_port_kit.py <target_dir> --config gru-crf --name statcheck-ml-r
