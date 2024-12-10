from setuptools import setup
from setuptools.command.bdist_wheel import bdist_wheel


class custom_bdist_wheel(bdist_wheel):
    def get_tag(self) -> tuple[str, str, str]:
        """Override get_tag to set ABI tag to 'none'.

        The OpenSolid native library is accessed only by ctypes calls,
        and so is independent of the Python ABI.
        """
        impl_tag, abi_tag, plat_tag = bdist_wheel.get_tag(self)
        return impl_tag, "none", plat_tag


setup(
    packages=["opensolid"],
    package_dir={"": "src"},
    package_data={
        "opensolid": ["*.so", "*.dylib", "*.dll"],
    },
    cmdclass={"bdist_wheel": custom_bdist_wheel},
    has_ext_modules=lambda: True,
)
