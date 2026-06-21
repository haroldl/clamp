MYPY = False
if not MYPY:
    BranchAlias: object = set[int] | None
