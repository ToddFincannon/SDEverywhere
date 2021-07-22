import fs from 'fs-extra'
import path from 'path'
import util from 'util'
import R from 'ramda'
import sh from 'shelljs'
import split from 'split-string'
import byline from 'byline'
import XLSX from 'xlsx'
import parseCsv from 'csv-parse/lib/sync.js'
import B from 'bufx'

// Set true to print a stack trace in vlog
export const PRINT_VLOG_TRACE = false

// next sequence number for generated temporary variable names
let nextTmpVarSeq = 1
// next sequence number for generated lookup variable names
let nextLookupVarSeq = 1
// next sequence number for generated level variable names
let nextLevelVarSeq = 1
// next sequence number for generated aux variable names
let nextAuxVarSeq = 1
// string table for web apps
export let strings = []

export let canonicalName = name => {
  // Format a model variable name into a valid C identifier.
  return (
    '_' +
    name
      .trim()
      .replace(/"/g, '_')
      .replace(/\s+!$/g, '!')
      .replace(/\s/g, '_')
      .replace(/,/g, '_')
      .replace(/-/g, '_')
      .replace(/\./g, '_')
      .replace(/\$/g, '_')
      .replace(/'/g, '_')
      .replace(/&/g, '_')
      .replace(/%/g, '_')
      .replace(/\//g, '_')
      .replace(/\|/g, '_')
      .toLowerCase()
  )
}
export let decanonicalize = name => {
  // Decanonicalize the var name.
  try {
    name = name.replace(/^_/, '').replace(/_/g, ' ')
    // Vensim variable names need to be surrounded by quotes if they:
    // do not start with a letter
    // do not contain only letters, spaces, numbers, single quotes, and dollar signs.
    if (!name.match(/^[A-Za-z]/) || name.match(/[^A-Za-z0-9\s'$]/)) {
      name = `"${name}"`
    }
  } catch (e) {
    debugger
    throw e
  }
  return name
}
export let cFunctionName = name => {
  return canonicalName(name).toUpperCase()
}
export let isSeparatedVar = v => {
  return v.separationDims.length > 0
}
export let newTmpVarName = () => {
  // Return a unique temporary variable name
  return `__t${nextTmpVarSeq++}`
}
export let newLookupVarName = () => {
  // Return a unique lookup arg variable name
  return `_lookup${nextLookupVarSeq++}`
}
export let newLevelVarName = (basename = null, levelNumber = 0) => {
  // Return a unique level variable name.
  let levelName = basename || nextLevelVarSeq++
  if (levelNumber) {
    levelName += `_${levelNumber}`
  }
  return `_level${levelName}`
}
export let newAuxVarName = (basename = null, auxNumber = 0) => {
  // Return a unique aux variable name.
  let auxName = basename || nextAuxVarSeq++
  if (auxNumber) {
    auxName += `_${auxNumber}`
  }
  return `_aux${auxName}`
}
export let isSmoothFunction = fn => {
  // Return true if fn is a Vensim smooth function.
  return fn === '_SMOOTH' || fn === '_SMOOTHI' || fn === '_SMOOTH3' || fn === '_SMOOTH3I'
}
export let isTrendFunction = fn => {
  // Return true if fn is a Vensim trend function.
  return fn === '_TREND'
}
export let isDelayFunction = fn => {
  // Return true if fn is a Vensim delay function.
  return fn === '_DELAY1' || fn === '_DELAY1I' || fn === '_DELAY3' || fn === '_DELAY3I'
}
export let isArrayFunction = fn => {
  // Return true if fn is a Vensim array function.
  return fn === '_SUM' || fn === '_VECTOR_SELECT' || fn === '_VMAX' || fn === '_VMIN'
}
export let listConcat = (a, x, addSpaces = false) => {
  // Append a string x to string a with comma delimiters
  let s = addSpaces ? ' ' : ''
  if (R.isEmpty(x)) {
    return a
  } else {
    return a + (R.isEmpty(a) ? '' : `,${s}`) + x
  }
}
export let cdbl = x => {
  // Convert a number into a C double constant.
  let s = x.toString()
  if (!s.includes('.') && !s.includes('e')) {
    s += '.0'
  }
  return s
}
export let strToConst = c => {
  let str = matchRegex(c, /'(.*)'/)
  if (str) {
    // Convert a Vensim string constant into a C string literal.
    return `"${str}"`
  } else {
    // Parse the string into a float.
    let d = parseFloat(c)
    return cdbl(d)
  }
}
export let first = a => R.head(a)
export let rest = a => R.tail(a)
export let extractMatch = (fn, list) => {
  // Return the first element of a list that matches the predicate and remove it from the list,
  // or return undefined if no element matches.
  let i = R.findIndex(fn, list)
  if (i >= 0) {
    return list.splice(i, 1)[0]
  } else {
    return undefined
  }
}
export let replaceInArray = (oldStr, newStr, a) => {
  // Replace the first occurrence of oldStr with newStr in an array of strings a.
  // A new array is constructed. The original array remains unchanged.
  let i = R.indexOf(oldStr, a)
  if (i >= 0) {
    let b = a.slice(0)
    b.splice(i, 1, newStr)
    return b
  } else {
    return a
  }
}
export let mapObjProps = (f, obj) => {
  // Map the key and value for each of the object's properties through function f.
  let result = {}
  R.forEach(k => (result[f(k)] = f(obj[k])), Object.keys(obj))
  return result
}
export let isIterable = obj => {
  // Return true of the object is iterable.
  if (obj == null) {
    return false
  }
  return typeof obj[Symbol.iterator] === 'function'
}
export let stringToId = str => {
  // Look up a string id. Create the id from the string if it is not found.
  let stringIndex = R.indexOf(str, strings)
  if (stringIndex < 0) {
    stringIndex = strings.length
    strings.push(str)
  }
  return `id${stringIndex}`
}
// Command helpers
export let outputDir = (outfile, modelDirname) => {
  if (outfile) {
    outfile = path.dirname(outfile)
  }
  return ensureDir(outfile, 'output', modelDirname)
}
export let buildDir = (build, modelDirname) => {
  // Ensure the given build directory or {modelDir}/build exists.
  return ensureDir(build, 'build', modelDirname)
}
export let webDir = buildDirname => {
  // Ensure a web directory exists under the build directory.
  return ensureDir(null, 'web', buildDirname)
}
export let ensureDir = (dir, defaultDir, modelDirname) => {
  // Ensure the directory exists as given or under the model directory.
  let dirName = dir || path.join(modelDirname, defaultDir)
  fs.ensureDirSync(dirName)
  return dirName
}
export let fileExists = pathname => {
  let exists = fs.existsSync(pathname)
  if (!exists) {
    console.error(`${pathname} not found`)
  }
  return exists
}
export let linkCSourceFiles = (modelDirname, buildDirname) => {
  let cDirname = path.join(new URL('.', import.meta.url).pathname, 'c')
  sh.ls(cDirname).forEach(filename => {
    // If a C source file is present in the model directory, link to it instead
    // as an override.
    let srcPathname = path.join(modelDirname, filename)
    if (!fs.existsSync(srcPathname)) {
      srcPathname = path.join(cDirname, filename)
    }
    let dstPathname = path.join(buildDirname, filename)
    fs.ensureSymlinkSync(srcPathname, dstPathname)
  })
}
export let filesExcept = (glob, exceptionFn) => {
  return R.reject(exceptionFn, sh.ls(glob))
}
export let modelPathProps = model => {
  // Normalize a model pathname that may or may not include the .mdl extension.
  // If there is not a path in the model argument, default to the current working directory.
  // Return an object with properties that look like this:
  // modelDirname: '/Users/todd/src/models/arrays'
  // modelName: 'arrays'
  // modelPathname: '/Users/todd/src/models/arrays/arrays.mdl'
  let p = R.merge({ ext: '.mdl' }, R.pick(['dir', 'name'], path.parse(model)))
  if (R.isEmpty(p.dir)) {
    p.dir = process.cwd()
  }
  return {
    modelDirname: p.dir,
    modelName: p.name,
    modelPathname: path.format(p)
  }
}
export let execCmd = cmd => {
  // Run a command line silently in the "sh" shell. Print error output on error.
  let exitCode = 0
  let result = sh.exec(cmd, { silent: true })
  if (sh.error()) {
    console.log(result.stderr)
    exitCode = 1
  }
  return exitCode
}
export let readDat = async (pathname, prefix = '') => {
  // Read a Vensim DAT file into a Map.
  // Key: variable name in canonical format
  // Value: Map from numeric time value to numeric variable value
  // The optional prefix string is prepended to var names when present.
  let log = new Map()
  let varName = ''
  let varValues = new Map()
  let lineNum = 1
  let splitDatLine = line => {
    const f = line.split('\t').map(s => s.trim())
    if (f.length < 2 || !R.isEmpty(f[1])) {
      return f
    } else {
      return [f[0]]
    }
  }
  let addValues = () => {
    if (varName !== '' && varValues.size > 0) {
      log.set(prefix + varName, varValues)
    }
  }
  // console.log(pathname)
  return new Promise(resolve => {
    let stream = byline(fs.createReadStream(pathname, 'utf8'))
    stream.on('data', line => {
      let values = splitDatLine(line)
      if (values.length === 1) {
        // Lines with a single value are variable names that start a data section.
        // Save the values for the current var if we are not on the first one.
        addValues()
        // Start a new map for this var.
        // Convert the var name to canonical form so it is the same in both logs.
        varName = canonicalVensimName(values[0])
        varValues = new Map()
      } else if (values.length > 1) {
        // Data lines in Vensim DAT format have {time}\t{value} format with optional comments afterward.
        let t = B.num(values[0])
        let value = B.num(values[1])
        // Save the value at time t in the varValues map.
        if (Number.isNaN(t)) {
          console.error(`DAT file ${pathname}:${lineNum} time value is NaN`)
        } else if (Number.isNaN(value)) {
          console.error(
            `DAT file ${pathname}:${lineNum} var "${varName}" value is NaN at time=${t}`
          )
        } else {
          varValues.set(t, value)
        }
      }
      lineNum++
      // if (lineNum % 1e5 === 0) console.log(num(lineNum).format('0,0'))
    })
    stream.on('end', () => {
      addValues()
      resolve(log)
    })
  })
}
export let readXlsx = pathname => {
  return XLSX.readFile(pathname, { cellDates: true })
}
export let readCsv = (pathname, delimiter = ',') => {
  // Read the CSV file at the pathname and parse it with the given delimiter.
  // Return an array of rows that are each an array of columns.
  // If there is a header row, it is returned as the first row.
  let result = null
  const CSV_PARSE_OPTS = {
    delimiter,
    columns: false,
    trim: true,
    skip_empty_lines: true,
    skip_lines_with_empty_values: true
  }
  try {
    let data = B.read(pathname)
    result = parseCsv(data, CSV_PARSE_OPTS)
  } catch (error) {
    console.error(`ERROR: CSV file ${pathname} not found`)
  }
  return result
}
// Convert the var name and subscript names to canonical form separately.
export let canonicalVensimName = vname => {
  let result = vname
  let m = vname.match(/([^\[]+)(?:\[([^\]]+)\])?/)
  if (m) {
    result = canonicalName(m[1])
    if (m[2]) {
      let subscripts = m[2].split(',').map(x => canonicalName(x))
      result += `[${subscripts.join(',')}]`
    }
  }
  return result
}
// Split a model string into an array of equations without the "|" terminator.
// Allow "|" to occur in quoted variable names across line breaks.
// Retain the backslash character.
export let splitEquations = mdl => {
  return split(mdl, { separator: '|', quotes: ['"'], keep: () => true })
}
// Function to map over lists's value and index
export let mapIndexed = R.addIndex(R.map)
// Function to sort an array of strings
export let asort = R.sort((a, b) => (a > b ? 1 : a < b ? -1 : 0))
// Function to alpha sort an array of variables on the model LHS
export let vsort = R.sort((a, b) =>
  a.modelLHS > b.modelLHS ? 1 : a.modelLHS < b.modelLHS ? -1 : 0
)
// Function to list an array to stderr
export let printArray = R.forEach(x => console.error(x))
// Function to expand an array of strings into a comma-delimited list of strings
export let strlist = a => {
  return a.join(', ')
}
// Function to join an array with newlines
export let lines = R.join('\n')
// Match a string against a regular expression and return the first match.
// If a capturing group was present, return the first group, otherwise
// return the entire match. If the string did not match, return the empty string.
export let matchRegex = (str, regex) => {
  let m = str.match(regex)
  if (!m) {
    return ''
  } else if (m.length > 1) {
    return m[1]
  } else if (m.length > 0) {
    return m[0]
  }
}
// Match a string against a regular expression with capture groups.
// Return an array of matches for each capture group.
// If the string did not match, return the empty string.
export let matchRegexCaptures = (str, regex) => {
  let m = str.match(regex)
  if (m && m.length > 0) {
    return m.splice(1)
  } else {
    return []
  }
}
// Match delimiters recursively. Replace delimited strings globally.
export let replaceDelimitedStrings = (str, open, close, newStr) => {
  // str is the string to operate on.
  // open and close are the opening and closing delimiter characters.
  // newStr is the string to replace delimited substrings with.
  let result = ''
  let start = 0
  let depth = 0
  let n = str.length
  for (let i = 0; i < n; i++) {
    if (str.charAt(i) === open) {
      if (depth === 0) {
        result += str.substring(start, i)
      }
      depth++
    } else if (str.charAt(i) === close && depth > 0) {
      depth--
      if (depth === 0) {
        result += newStr
        start = i + 1
      }
    }
  }
  if (start < n) {
    result += str.substring(start)
  }
  return result
}

/**
 * Return the cartesian product of the given array of arrays.
 *
 * For example, if we have an array that lists out two dimensions:
 *   [ ['a1','a2'], ['b1','b2','b3'] ]
 * this function will return all the combinations, e.g.:
 *   [ ['a1', 'b1'], ['a1', 'b2'], ['a1', 'b3'], ['a2', 'b1'], ... ]
 *
 * This can be used in place of nested for loops and has the benefit of working
 * for multi-dimensional inputs.
 */
export const cartesianProductOf = arr => {
  // Implementation based on: https://stackoverflow.com/a/36234242
  return arr.reduce(
    (a, b) => {
      return a.map(x => b.map(y => x.concat([y]))).reduce((v, w) => v.concat(w), [])
    },
    [[]]
  )
}

/**
 * Return all possible permutations of the given array elements.
 *
 * For example, if we have an array of numbers:
 *   [1,2,3]
 * this function will return all the permutations, e.g.:
 *   [ [1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1] ]
 */
export const permutationsOf = (elems, subperms = [[]]) => {
  // Implementation based on: https://gist.github.com/CrossEye/f7c2f77f7db7a94af209
  return R.isEmpty(elems)
    ? subperms
    : R.addIndex(R.chain)(
        (elem, idx) => permutationsOf(R.remove(idx, 1, elems), R.map(R.append(elem), subperms)),
        elems
      )
}

//
// Debugging helpers
//
export let vlog = (title, value, depth = 1) => {
  if (value) {
    console.error(title, ':', util.inspect(value, { depth: depth, colors: false }))
  } else {
    console.error(title)
  }
  if (PRINT_VLOG_TRACE) {
    console.trace()
  }
}
export let abend = error => {
  console.error(error)
  process.exit(1)
}
