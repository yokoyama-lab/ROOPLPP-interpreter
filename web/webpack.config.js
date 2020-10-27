const path = require('path');
module.exports = {
    entry: './src/index.tsx',
    output: {
        path: __dirname+"/dist",
        filename: '[name].bundle.js',
    },
    resolve: {
        extensions:['.ts','.tsx','.js', '.jsx']
    },
    devServer: {
        contentBase: path.join(__dirname,'dist')
    },
    module: {
        rules: [
            {
                test:/\.tsx$/,
                exclude: /node_modules/,
                loader:'ts-loader'
            }
        ]
    }
}