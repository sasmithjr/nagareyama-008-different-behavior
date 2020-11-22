module.exports = {
    entry: resolve("./StuffTests.fsproj"),
    outDir: resolve("./output"),
    babel: {
        plugins: ["transform-es2015-modules-commonjs"],
    }
};

function resolve(path) {
    return require("path").join(__dirname, path);
}
