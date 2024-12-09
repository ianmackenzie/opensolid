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
    name="opensolid",
    version="0.1.0",
    author="Ian Mackenzie",
    author_email="ian.e.mackenzie@gmail.com",
    description="A collection of classes for 2D/3D geometric modelling",
    python_requires=">=3.12",
    project_urls={"source": "https://github.com/ianmackenzie/opensolid"},
    package_dir={"": "src"},
    packages=["opensolid"],
    package_data={
        "opensolid": ["*.so", "*.dylib", "*.dll"],
    },
    cmdclass={"bdist_wheel": bdist_wheel},
    has_ext_modules=lambda: True,
)
