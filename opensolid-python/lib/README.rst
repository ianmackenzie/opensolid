OpenSolid
=========

`OpenSolid <https://github.com/ianmackenzie/opensolid>`_ is an under-development 2D/3D CAD library.
This package contains its official Python bindings. The bindings (and OpenSolid itself) are still
under active development, but you're welcome to try them out!

The package contains a single ``opensolid`` module containing a variety of classes representing
geometric values and quantities, such as ``Length``, ``Angle``, ``Point2d``, and ``Direction2d``.
Each class and function has a brief docstring, but here's a brief sampling of some of the
operations you can do::

    >>> from opensolid import *

    # Define some Length values
    >>> width = Length.meters(5)
    >>> height = Length.centimeters(10)

    # Multiply those to get an Area value
    >>> width * height
    Area.square_meters(0.5)

    # All values are stored in SI units internally,
    # so you can mix and match units freely
    >>> Length.centimeters(100) + Length.inches(10)
    Length.meters(1.254)

    # You can request that any length be converted into
    # a value in specific units (giving back a plain 'float')
    >>> Length.meters(2).in_inches()
    78.74015748031496

    # Define a couple of points in 2D from their
    # X and Y coordinates, given in centimeters
    >>> p1 = Point2d.centimeters(50, 60)
    >>> p2 = Point2d.centimeters(100, 80)

    # Measure the distance between the points
    >>> p1.distance_to(p2)
    Length.meters(0.5385164807134505)

    # Build up a parametric curve (expression);
    # 'Curve.t' is the curve parameter, which
    # ranges from 0 to 1, so here x will range
    # from 0 to 5.
    >>> x = 5 * Curve.t
    >>> y = x.squared() - 5 * x + 1

    # Evaluate the curve at t=0.5
    >>> y.evaluate(0.5)
    -5.25

    # Find the values of t for which y=0. For this, we need
    # to set up a tolerance value. (A tolerance is needed
    # for cases where e.g. the curve decreases to a tiny
    # value like 1e-9 before increasing again; should that
    # be considered a solution or not? Similarly, if the
    # curve decreases to -1e9 and then increases again,
    # should that be considered two zeros or just one? See
    # the documentation of Curve.zeros for more info.)
    >>> with Tolerance(1e-6):
    ...     # Get the location of each zero, i.e. the
    ...     # value of t (not x!) for which y is zero
    ...     roots = [zero.location() for zero in y.zeros()]
    >>> roots
    [0.041742430504416, 0.9582575694955838]

    # Find the values of x at which y is zero,
    # by evaluating x at those roots
    >>> [x.evaluate(t) for t in roots]
    [0.20871215252208, 4.7912878474779195]

    # Check that y is in fact zero at those roots
    >>> [y.evaluate(root) for root in roots]
    [0.0, 0.0]

    # Construct a displacement (vector with length units)
    # from its components in meters
    >>> d = Displacement2d.meters(1, 2)

    # To get the direction of a vector, we also need to
    # define a tolerance (any vector with magnitude
    # smaller than this value will be considered 'zero'
    # and therefore have no direction, so attempting to
    # get its direction will raise an exception).
    >>> with Tolerance(Length.nanometer):
    ...     d.direction()
    Direction2d.degrees(63.434948822922)
