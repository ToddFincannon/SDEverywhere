import fs from 'fs-extra'
import path from 'path'
import sh from 'shelljs'
import B from 'bufx'
import antlr4 from 'antlr4'
import { ModelLexer, ModelParser } from 'antlr4-vensim'
import { codeGenerator } from './CodeGen.js'
import { preprocessModel } from './Preprocessor.js'
import {
  canonicalName,
  modelPathProps,
  buildDir,
  webDir,
  linkCSourceFiles,
  filesExcept,
  execCmd,
  readDat,
  readXlsx
} from './Helpers.js'
import { initConfig, makeModelSpec, makeModelConfig, makeChartData, readModelConfig } from './MakeConfig.js'
import Model from './Model.js'
import { printSubscripts, yamlSubsList } from './Subscript.js'

// Set true to retain generated source files during development.
const RETAIN_GENERATED_SOURCE_FILES = false
// A custom CSS file may be provided to override built-in styles.
const CUSTOM_CSS = 'custom.css'

export let command = 'generate [options] <model>'
export let describe = 'generate model code'
export let builder = {
  genc: {
    describe: 'generate C code for the model',
    type: 'boolean'
  },
  genhtml: {
    describe: 'generate an HTML UI for the model',
    type: 'boolean'
  },
  list: {
    describe: 'list model variables',
    type: 'boolean',
    alias: 'l'
  },
  preprocess: {
    describe: 'write a preprocessed model that runs in Vensim',
    type: 'boolean',
    alias: 'p'
  },
  analysis: {
    describe: 'write a nonexecutable preprocessed model for analysis',
    type: 'boolean',
    alias: 'a'
  },
  spec: {
    describe: 'pathname of the I/O specification JSON file',
    type: 'string',
    alias: 's'
  },
  builddir: {
    describe: 'build directory',
    type: 'string',
    alias: 'b'
  },
  refidtest: {
    describe: 'test reference ids',
    type: 'boolean',
    alias: 'r'
  }
}
export let handler = argv => {
  generate(argv.model, argv)
}

export let generate = async (model, opts) => {
  // Get the model name and directory from the model argument.
  let { modelDirname, modelName, modelPathname } = modelPathProps(model)
  // Ensure the build directory exists.
  let buildDirname = buildDir(opts.builddir, modelDirname)
  // The web directory is only used for the --genhtml option.
  let webDirname = ''
  // Generate a spec file from the config files for web apps.
  // This overrides the --spec argument if present.
  if (opts.genhtml) {
    webDirname = webDir(buildDirname)
    initConfig(modelDirname, webDirname)
    opts.spec = makeModelSpec()
  }
  // Preprocess model text into parser input. Stop now if that's all we're doing.
  let spec = parseSpec(opts.spec)
  // Read time series from external DAT files into a single object.
  // externalDatfiles is an array of either filenames or objects
  // giving a variable name prefix as the key and a filename as the value.
  let extData = new Map()
  if (spec.externalDatfiles) {
    for (let datfile of spec.externalDatfiles) {
      let prefix = ''
      let filename = ''
      if (typeof datfile === 'object') {
        prefix = Object.keys(datfile)[0]
        filename = datfile[prefix]
      } else {
        filename = datfile
      }
      let pathname = path.join(modelDirname, filename)
      let data = await readDat(pathname, prefix)
      extData = new Map([...extData, ...data])
    }
  }
  // Attach Excel workbook data to directData entries by file name.
  let directData = new Map()
  if (spec.directData) {
    for (let [file, xlsxFilename] of Object.entries(spec.directData)) {
      let pathname = path.join(modelDirname, xlsxFilename)
      directData.set(file, readXlsx(pathname))
    }
  }
  // Produce a runnable model with the "genc" and "preprocess" options.
  let profile = opts.analysis ? 'analysis' : 'genc'
  // Write the preprocessed model and removals if the option is "analysis" or "preprocess".
  let writeFiles = opts.analysis || opts.preprocess
  let input = preprocessModel(modelPathname, spec, profile, writeFiles)
  if (writeFiles) {
    let outputPathname = path.join(buildDirname, `${modelName}.mdl`)
    writeOutput(outputPathname, input)
    process.exit(0)
  }
  // Parse the model and generate code. If no operation is specified, the code generator will
  // read the model and do nothing else. This is required for the list operation.
  let operation = ''
  if (opts.genc || opts.genhtml) {
    operation = 'generateC'
  } else if (opts.list) {
    operation = 'printVarList'
  } else if (opts.refidtest) {
    operation = 'printRefIdTest'
  }
  let parseTree = parseModel(input)
  let code = codeGenerator(parseTree, { spec, operation, extData, directData }).generate()
  if (opts.genc || opts.genhtml) {
    let outputPathname = path.join(buildDirname, `${modelName}.c`)
    writeOutput(outputPathname, code)
  }
  if (opts.list) {
    let outputPathname, outputText
    // Write variables to a text file.
    outputPathname = path.join(buildDirname, `${modelName}_vars.txt`)
    outputText = Model.printVarList()
    writeOutput(outputPathname, outputText)
    // Write subscripts to a text file.
    outputPathname = path.join(buildDirname, `${modelName}_subs.txt`)
    outputText = printSubscripts()
    writeOutput(outputPathname, outputText)
    // Write variables to a YAML file.
    outputPathname = path.join(buildDirname, `${modelName}_vars.yaml`)
    outputText = Model.yamlVarList()
    writeOutput(outputPathname, outputText)
    // Write subscripts to a YAML file.
    outputPathname = path.join(buildDirname, `${modelName}_subs.yaml`)
    outputText = yamlSubsList()
    writeOutput(outputPathname, outputText)
  }
  // Generate a web app for the model.
  if (opts.genhtml) {
    linkCSourceFiles(modelDirname, buildDirname)
    if (generateWASM(buildDirname, webDirname) === 0) {
      makeModelConfig()
      await makeChartData()
      copyTemplate(buildDirname)
      customizeApp(modelDirname, webDirname)
      packApp(webDirname)
    }
  }
}
let generateWASM = (buildDirname, webDirname) => {
  // Generate WASM from C source files in the build directory.
  let args = filesExcept(`${buildDirname}/*.c`, name => name.endsWith('main.c'))
  // Include the build directory as a place to look for header files.
  args.push(`-I${buildDirname}`)
  // Set the output pathname for the JavaScript wrapper to the web directory.
  // The WASM file will be written to the same directory and basename.
  args.push('-o')
  args.push(path.join(webDirname, 'model_sde.js'))
  // Set flags for WASM compilation and optimization.
  // Use -O0 optimization in development to get readable model_sde.js wrapper source.
  // Use -Oz optimization for productions runs.
  args.push('-Wall -Oz')
  // Turn on safe heap to debug "application has corrupted its heap memory area" exceptions.
  // Also turn on the clamp when using safe heap. Ref: https://github.com/WebAssembly/binaryen/issues/1110
  // args.push('-s SAFE_HEAP=1')
  // args.push('-s "BINARYEN_TRAP_MODE=\'clamp\'"')
  // Prevent the WASM code from exiting after it runs the model.
  args.push('-s NO_EXIT_RUNTIME=1')
  // Export the function that runs the model.
  args.push('-s EXPORTED_FUNCTIONS="[\'_run_model\']"')
  // Export the Module.cwrap method used to wrap arguments.
  args.push('-s "EXTRA_EXPORTED_RUNTIME_METHODS=[\'cwrap\']"')
  // Use a simpler malloc to reduce code size.
  args.push('-s MALLOC=emmalloc')
  // Run the Closure compiler to minimize JS glue code.
  args.push('--closure 1')
  // Run the emcc command to generate WASM code.
  let cmd = `emcc ${args.join(' ')}`
  // console.log(cmd)
  let exitCode = execCmd(cmd)
  if (exitCode) {
    console.error('The Emscripten SDK must be installed in your path.')
  }
  return exitCode
}
let copyTemplate = buildDirname => {
  // Copy template files from the src/web directory.
  let templateDirname = path.join(new URL('.', import.meta.url).pathname, 'web')
  sh.cp('-Rf', templateDirname, buildDirname)
}
let customizeApp = (modelDirname, webDirname) => {
  try {
    // Read the newly generated model config to customize app files.
    let app = readModelConfig().app
    if (app && app.logo) {
      let logoPathname = `${modelDirname}/${app.logo}`
      sh.cp('-f', logoPathname, webDirname)
    }
    // Copy the custom.css file if it exists in the model directory.
    let cssPathname = path.join(modelDirname, 'config', CUSTOM_CSS)
    if (fs.existsSync(cssPathname)) {
      sh.cp('-f', cssPathname, webDirname)
    } else {
      // Create a blank file if it is not provided to avoid a 404 on the CSS link in the HTML.
      let outputPathname = path.join(webDirname, CUSTOM_CSS)
      B.write('', outputPathname)
    }
  } catch (e) {
    console.error(e.message)
  }
}
let packApp = webDirname => {
  // Concatenate JS source files for the browser.
  let sourcePathname = path.join(webDirname, 'index.js')
  let minPathname = path.join(webDirname, 'index.min.js')
  // Resolve module imports against the SDEverywhere node_modules.
  let nodePath = path.join(new URL('..', import.meta.url).pathname, 'node_modules')
  // Browserify is an optional install that we only import when generating HTML.
  import('browserify').then(browserify => {
    let b = browserify.default(sourcePathname, { paths: nodePath })
    let writable = fs.createWriteStream(minPathname)
    b.bundle()
      .pipe(writable)
      .on('finish', error => {
        // Remove JavaScript source files.
        if (!RETAIN_GENERATED_SOURCE_FILES) {
          let sourceFiles = filesExcept(`${webDirname}/*.js`, name => name.endsWith('index.min.js') || name.endsWith('model_sde.js'))
          sh.rm(sourceFiles)
        }
      })
  })
}
let parseModel = input => {
  // Read the model text and return a parse tree.
  let chars = new antlr4.InputStream(input)
  let lexer = new ModelLexer(chars)
  let tokens = new antlr4.CommonTokenStream(lexer)
  let parser = new ModelParser(tokens)
  parser.buildParseTrees = true
  return parser.model()
}
let parseSpec = specFilename => {
  let spec = parseJsonFile(specFilename)
  // Translate dimension families in the spec to canonical form.
  if (spec.dimensionFamilies) {
    let f = {}
    for (let dimName in spec.dimensionFamilies) {
      let family = spec.dimensionFamilies[dimName]
      f[canonicalName(dimName)] = canonicalName(family)
    }
    spec.dimensionFamilies = f
  }
  return spec
}
let parseJsonFile = filename => {
  // Parse the JSON file if it exists.
  let result = {}
  try {
    let json = B.read(filename)
    result = JSON.parse(json)
    // console.error(`loaded ${filename}`);
  } catch (ex) {
    // If the file doesn't exist, return an empty object without complaining.
  }
  return result
}
let writeOutput = (outputPathname, outputText) => {
  try {
    B.write(outputText, outputPathname)
  } catch (e) {
    console.log(outputPathname)
    console.log(e.message)
  }
}
export default {
  command,
  describe,
  builder,
  handler,
  generate
}
