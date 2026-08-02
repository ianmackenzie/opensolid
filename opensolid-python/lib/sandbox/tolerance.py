from opensolid import Length

small = Length.meters(1e-6)
tiny = Length.meters(1e-12)


def check_zero() -> None:
    print("small:", small.is_zero())
    print("tiny:", tiny.is_zero())


print("With default tolerance:")
check_zero()
with Length.tolerance(Length.meters(1e-3)):
    print("With custom tolerance:")
    check_zero()
