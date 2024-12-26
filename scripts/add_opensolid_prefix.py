import glob
import shutil
from pathlib import Path

for old_module_path in glob.glob("opensolid/src/*.hs"):
    old_module_name = old_module_path.replace("opensolid/src/", "").replace(".hs", "")
    new_module_name = "OpenSolid." + old_module_name

    old_import_qualified = "import " + old_module_name + " qualified"
    new_import_qualified = (
        "import " + new_module_name + " qualified as " + old_module_name
    )

    old_import_qualified_source = (
        "import {-# SOURCE #-} " + old_module_name + " qualified"
    )
    new_import_qualified_source = (
        "import {-# SOURCE #-} " + new_module_name + " qualified as " + old_module_name
    )

    old_import_members = "import " + old_module_name + " ("
    new_import_members = "import " + new_module_name + " ("

    old_import_members_source = "import {-# SOURCE #-} " + old_module_name + " ("
    new_import_members_source = "import {-# SOURCE #-} " + new_module_name + " ("

    for other_module_path in glob.glob("opensolid/**/*.hs*", recursive=True):
        with open(other_module_path, "r") as other_module_file:
            old_content = other_module_file.read()
        new_content = (
            old_content.replace(old_import_qualified, new_import_qualified)
            .replace(old_import_qualified_source, new_import_qualified_source)
            .replace(old_import_members, new_import_members)
            .replace(old_import_members_source, new_import_members_source)
        )
        with open(other_module_path, "w") as other_module_file:
            other_module_file.write(new_content)


for old_module_path in glob.glob("opensolid/src/*.hs"):
    old_module_name = old_module_path.replace("opensolid/src/", "").replace(".hs", "")
    new_module_name = "OpenSolid." + old_module_name

    new_module_path = old_module_path.replace(
        "opensolid/src/", "opensolid/src/OpenSolid/"
    )
    with open(old_module_path, "r") as old_module_file:
        old_module_content = old_module_file.read()
    shutil.move(old_module_path, new_module_path)
    new_module_content = old_module_content.replace(
        "module " + old_module_name, "module " + new_module_name
    )
    with open(new_module_path, "w") as new_module_file:
        new_module_file.write(new_module_content)

    old_boot_module_path = old_module_path + "-boot"
    if Path(old_boot_module_path).is_file():
        with open(old_boot_module_path, "r") as old_boot_module_file:
            old_boot_module_content = old_boot_module_file.read()
        new_boot_module_content = old_boot_module_content.replace(
            "module " + old_module_name, "module " + new_module_name
        )
        new_boot_module_path = old_boot_module_path.replace(
            "opensolid/src/", "opensolid/src/OpenSolid/"
        )
        shutil.move(old_boot_module_path, new_boot_module_path)
        with open(new_boot_module_path, "w") as new_boot_module_file:
            new_boot_module_file.write(new_boot_module_content)
