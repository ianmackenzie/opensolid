from setuptools import setup
from setuptools.command.bdist_wheel import bdist_wheel as _bdist_wheel


class bdist_wheel(_bdist_wheel):
    def get_tag(self) -> tuple[str, str, str]:
        """Override get_tag to set ABI tag to 'none'.

        The OpenSolid native library is accessed only by ctypes calls,
        and so is independent of any Python ABI.
        """
        impl_tag, abi_tag, plat_tag = _bdist_wheel.get_tag(self)
        return impl_tag, "none", plat_tag


setup(
    packages=["opensolid"],
    package_dir={"opensolid": "src"},
    package_data={
        "opensolid": ["*.so", "*.dylib", "*.dll"],
    },
    cmdclass={"bdist_wheel": bdist_wheel},
    has_ext_modules=lambda: True,
)
