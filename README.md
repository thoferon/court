# Plugins

A plugin is a simple executable running continuously in the background checking when a project should be built. It is taking no arguments other than the ones specified in the `plugin-map` file.

A build is triggered every time a plugin prints a tab-separated list containing the arguments to pass to the build process on one line. **IMPORTANT:** A plugin needs to flush the lines everytime it prints something to prevent blocking the `court` process.

`Court` tries to read a formatted string if possible. For instance, `"example\n"` will be read like `example` followed by a newline while `example` will still be valid and read as it is.

# PluginMap

TODO: description and example
TODO: relative paths

# Build

TODO: project-name/build
TODO: exit code
TODO: stderr ignored