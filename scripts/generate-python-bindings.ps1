# Run the Python code generator executable to create __init__.py
cabal run opensolid-python
# Generate the opensolid-ffi.def file with all API functions listed
# (necessary for Windows builds)
cabal run generate-def-file
# Build the native shared library
# (on Windows, this uses the opensolid-ffi.def file generated above)
cabal build opensolid-ffi
# Copy the native shared library to beside __init__.py
# so that it can be loaded properly
Copy-Item -Path .\opensolid-rs\target\release\opensolid_rs.dll -Destination .\opensolid-python\opensolid\
Get-ChildItem -Path .\dist-newstyle -Filter opensolid-ffi.dll -Recurse | Copy-Item -Destination .\opensolid-python\opensolid\
