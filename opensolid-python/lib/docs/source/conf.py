project = "OpenSolid"
copyright = "2024, Ian Mackenzie"
author = "Ian Mackenzie"

# Support automatically generating documentation for
# all classes in a package, all methods in a class etc.
extensions = ["sphinx.ext.autodoc"]

# Use Read The Docs theme
html_theme = "sphinx_rtd_theme"

# Don't include __init__ signature in class declaration,
# since that's not meant for public use
autodoc_class_signature = "separated"

autodoc_default_options = {
    # Don't show __init__,
    # since it's not meant for public use
    "exclude-members": "__init__",
    # Follow the order that things are declared in the source,
    # since that should be meaningful/useful
    "member-order": "bysource",
    # Include 'special' function in docs,
    # since it's useful to be able to see which ones exist
    # and what overloads they have
    "special-members": ",".join(
        [
            "__eq__",
            "__lt__",
            "__le__",
            "__gt__",
            "__ge__",
            "__add__",
            "__radd__",
            "__sub__",
            "__rsub__",
            "__mul__",
            "__rmul__",
            "__truediv__",
            "__rtruediv__",
            "__floordiv__",
            "__rfloordiv__",
            "__mod__",
            "__rmod__",
        ]
    ),
}
